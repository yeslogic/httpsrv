:- module httpsrv.

% Copyright (C) 2014 YesLogic Pty. Ltd.
% All rights reserved.

:- interface.

:- import_module assoc_list.
:- import_module io.
:- import_module list.
:- import_module maybe.
:- import_module pair.
:- import_module time.

:- import_module headers.

:- include_module httpsrv.status.
:- import_module httpsrv.status.

%-----------------------------------------------------------------------------%

% Starting the server

:- type daemon.

:- type server_setting
    --->    bind_address(string)
    ;       port(int)
    ;       back_log(int).

:- type request_handler == pred(request, io, io).
:- inst request_handler == (pred(in, di, uo) is cc_multi).

:- pred setup(request_handler::in(request_handler), list(server_setting)::in,
    maybe_error(daemon)::out, io::di, io::uo) is det.

:- pred run(daemon::in, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%

% Getting information about the request

:- type request.

:- type method
    --->    delete
    ;       get
    ;       head
    ;       post
    ;       put
    ;       other(string).

:- type url
    --->    url(
                schema      :: maybe(string),
                host        :: maybe(string),
                port        :: maybe(string),
                path_raw    :: maybe(string),
                query_raw   :: maybe(string),
                fragment    :: maybe(string)
            ).

:- type content
    --->    none
    ;       string(string)

            % For application/x-www-form-urlencoded
            % Keys are in RECEIVED order and duplicate keys are possible.
    ;       form_urlencoded(assoc_list(string, string))

            % For multipart/form-data
            % Keys are in RECEIVED order and duplicate keys are possible.
    ;       multipart_formdata(assoc_list(string, formdata)).

:- func get_method(request) = method.

:- func get_url_raw(request) = string.

:- func get_url(request) = url.

:- func get_path_decoded(request) = maybe(string).

:- func get_query_parameters(request) = assoc_list(string).

:- func get_headers(request) = headers.

:- func get_cookies(request) = assoc_list(string).

:- func get_body(request) = content.

:- pred get_client_address_ipv4(request::in, maybe(string)::out,
    io::di, io::uo) is det.

%-----------------------------------------------------------------------------%

% Form-data

:- type formdata
    --->    formdata(
                disposition                 :: string,
                filename                    :: maybe(string),
                media_type                  :: string,
                content_transfer_encoding   :: maybe(string),
                content                     :: formdata_content
            ).

:- type formdata_content.

:- func formdata_content_length(formdata_content) = int.

:- pred formdata_content_to_string(formdata_content::in, string::out)
    is semidet.

%-----------------------------------------------------------------------------%

% Setting the response

    % NOTE: for now, the user is responsible for any necessary escaping!
    %
:- type response_header
    --->    cache_control_max_age(int)
    ;       content_type(string)
    ;       content_type_charset_utf8(string)
    ;       content_disposition(string)
    ;       set_cookie(pair(string), list(cookie_attribute))
    ;       x_content_type_options_nosniff
    ;       custom(pair(string), assoc_list(string)).

:- type cookie_attribute
    --->    expires(time_t)
    ;       max_age(int) % strictly positive
    ;       domain(string)
    ;       path(string)
    ;       secure
    ;       httponly.

:- type response_content
    --->    strings(list(string))
    ;       file(static_file).

:- pred set_response(request::in, status_code::in, list(response_header)::in,
    response_content::in, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%

% Serving static files

:- type static_file.

:- pred open_static_file(string::in, maybe_error(static_file)::out,
    io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module bool.
:- import_module time.

:- import_module buffer.

:- include_module httpsrv.formdata_accum.
:- include_module httpsrv.parse_url.
:- include_module httpsrv.request.
:- include_module httpsrv.response.
:- include_module httpsrv.response_header.
:- include_module httpsrv.static_file.

:- use_module httpsrv.response.
:- use_module httpsrv.static_file.

%-----------------------------------------------------------------------------%

:- type client.

:- pragma foreign_type("C", daemon, "daemon_t *").
:- pragma foreign_type("C", client, "client_t *").

:- pragma foreign_decl("C", "
    typedef struct daemon daemon_t;
    typedef struct client client_t;
    typedef struct buffer buffer_t;
").

:- pragma foreign_decl("C", local, "
    #include ""httpsrv1.h""
").

:- pragma foreign_code("C", "
    #include ""httpsrv1.c""
").

:- type request
    --->    request(
                client      :: client,
                method      :: method,
                url_raw     :: string, % mainly for debugging
                url         :: url,
                path_decoded:: maybe(string), % percent decoded
                query_params:: assoc_list(string), % percent decoded
                headers     :: headers,
                cookies     :: assoc_list(string),
                body        :: content
            ).

:- type formdata_content == list(buffer(buffer_ro)).

:- type static_file
    --->    static_file(
                fd          :: int,
                file_size   :: int
            ).

%-----------------------------------------------------------------------------%

:- func url_init = url.

url_init = url(no, no, no, no, no, no).

:- pred method(string, method).
:- mode method(in, out) is semidet.
:- mode method(out, in) is semidet.

method("DELETE", delete).
method("GET", get).
method("HEAD", head).
method("POST", post).
method("PUT", put).

%-----------------------------------------------------------------------------%

setup(RequestHandler, Settings, Res, !IO) :-
    FindStr = find(Settings),
    FindInt = find(Settings),
    BindAddress = FindStr(pred(bind_address(X)::in, X::out) is semidet, "127.0.0.1"),
    Port = FindInt(pred(port(X)::in, X::out) is semidet, 80),
    BackLog = FindInt(pred(back_log(X)::in, X::out) is semidet, 1),

    setup_2(RequestHandler, BindAddress, Port, BackLog, Daemon, Ok, !IO),
    (
        Ok = yes,
        Res = ok(Daemon)
    ;
        Ok = no,
        % XXX better error message
        Res = error("(no error details yet)")
    ).

:- pred setup_2(request_handler::in(request_handler),
    string::in, int::in, int::in, daemon::out, bool::out, io::di, io::uo)
    is det.

:- pragma foreign_proc("C",
    setup_2(RequestHandler::in(request_handler),
        BindAddress::in, Port::in, BackLog::in,
        Daemon::out, Ok::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io,
        may_not_duplicate],
"
    Daemon = daemon_setup(RequestHandler, BindAddress, Port, BackLog);
    Ok = (Daemon) ? MR_YES : MR_NO;
").

:- func find(list(T)::in, pred(T, X)::in(pred(in, out) is semidet), X::in)
    = (X::out) is det.

find([], _P, Default) = Default.
find([T | Ts], P, Default) =
    ( P(T, X) ->
        X
    ;
        find(Ts, P, Default)
    ).

%-----------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    run(Daemon::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io,
        may_not_duplicate],
"
    uv_run(Daemon->loop, UV_RUN_DEFAULT);
    daemon_cleanup(Daemon);
").

%-----------------------------------------------------------------------------%

get_method(Request) = Request ^ method.

get_url_raw(Request) = Request ^ url_raw.

get_url(Request) = Request ^ url.

get_path_decoded(Request) = Request ^ path_decoded.

get_query_parameters(Request) = Request ^ query_params.

get_headers(Request) = Request ^ headers.

get_cookies(Request) = Request ^ cookies.

get_body(Request) = Request ^ body.

get_client_address_ipv4(Request, Res, !IO) :-
    client_address_ipv4(Request ^ client, Address, !IO),
    ( Address = "" ->
        Res = no
    ;
        Res = yes(Address)
    ).

:- pred client_address_ipv4(client::in, string::out, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    client_address_ipv4(Client::in, String::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io,
        may_not_duplicate],
"
    String = client_address_ipv4(Client, MR_ALLOC_ID);
").

%-----------------------------------------------------------------------------%

formdata_content_length(Content) = total_length(Content).

formdata_content_to_string(Bufs, String) :-
    buffers_to_string_utf8(Bufs, String).

%-----------------------------------------------------------------------------%

set_response(Request, Status, AdditionalHeaders, Content, !IO) :-
    response.set_response(Request, Status, AdditionalHeaders, Content, !IO).

%-----------------------------------------------------------------------------%

open_static_file(Path, Result, !IO) :-
    static_file.open_static_file(Path, Result, !IO).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
