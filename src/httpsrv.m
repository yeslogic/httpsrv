:- module httpsrv.

% Copyright (C) 2014 YesLogic Pty. Ltd.
% All rights reserved.

:- interface.

:- import_module assoc_list.
:- import_module io.
:- import_module list.
:- import_module maybe.

%-----------------------------------------------------------------------------%

:- type daemon.
:- type client.

:- type server_setting
    --->    bind_address(string)
    ;       port(int)
    ;       back_log(int).

:- type request_handler == pred(client, request, io, io).
:- inst request_handler == (pred(in, in, di, uo) is cc_multi).

:- type request
    --->    request(
                method  :: method,
                url     :: string,
                headers :: assoc_list(string, string),
                body    :: content
            ).

:- type method
    --->    delete
    ;       get
    ;       head
    ;       post
    ;       put
    ;       other(string).

:- type content
    --->    none
    ;       string(string)

            % For application/x-www-form-urlencoded
            % Keys are in RECEIVED order and duplicate keys are possible.
    ;       form_urlencoded(assoc_list(string, string))

            % For multipart/form-data
            % Keys are in RECEIVED order and duplicate keys are possible.
    ;       multipart_formdata(assoc_list(string, formdata)).

:- type formdata
    --->    formdata(
                disposition                 :: string,
                filename                    :: maybe(string),
                media_type                  :: string,
                content_transfer_encoding   :: maybe(string),
                content                     :: formdata_content
            ).

:- type formdata_content.

:- pred setup(request_handler::in(request_handler), list(server_setting)::in,
    maybe_error(daemon)::out, io::di, io::uo) is det.

:- pred run(daemon::in, io::di, io::uo) is det.

:- pred set_response(client::in, list(string)::in, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module bool.
:- import_module int.
:- import_module pair.
:- import_module string.
:- import_module time.

:- import_module buffer.
:- import_module form_urlencoded.
:- import_module http_date.
:- import_module mime_headers.
:- import_module multipart_parser.

:- include_module httpsrv.formdata_accum.
:- import_module httpsrv.formdata_accum.

%-----------------------------------------------------------------------------%

:- pragma foreign_type("C", daemon, "daemon_t *").
:- pragma foreign_type("C", client, "client_t *").

:- pragma foreign_decl("C", "
    typedef struct daemon daemon_t;
    typedef struct client client_t;
    typedef struct buffer buffer_t;
").

:- pragma foreign_decl("C", local, "
    #include ""uv.h""
    #include ""http_parser.h""

    #include ""httpsrv1.h""
").

:- pragma foreign_code("C", "
    #include ""httpsrv1.c""
").

:- type formdata_content == list(buffer).

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
").

%-----------------------------------------------------------------------------%

    % XXX support other types of responses
set_response(Client, Content, !IO) :-
    time(Time, !IO),
    HttpDate = timestamp_to_http_date(Time),
    KeepAlive = client_should_keep_alive(Client),
    (
        KeepAlive = yes,
        MaybeConnectionClose = ""
    ;
        KeepAlive = no,
        MaybeConnectionClose = "Connection: close\r\n"
    ),
    ResponseList = [
        "HTTP/1.1 200 OK\r\n",
        "Date: ", HttpDate, "\r\n",
        % "Content-Type: text/plain\r\n",
        "Content-Length: ", from_int(sum_length(Content)), "\r\n",
        MaybeConnectionClose,
        "\r\n"
        | Content
    ],
    set_response_2(Client, ResponseList, length(ResponseList), !IO).

:- pred set_response_2(client::in, list(string)::in, int::in, io::di, io::uo)
    is det.

:- pragma foreign_proc("C",
    set_response_2(Client::in, ResponseList::in, ResponseListLength::in,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io,
        may_not_duplicate],
"
    set_response_bufs(Client, ResponseList, ResponseListLength);
    send_async(Client);
").

:- func sum_length(list(string)) = int.

sum_length(Xs) = foldl(plus, map(length, Xs), 0).

%-----------------------------------------------------------------------------%

% Mercury to C

:- func request_init = request.

:- pragma foreign_export("C", request_init = out, "request_init").

request_init = request(other(""), "", [], none).

:- func request_set_method(request, string) = request.

:- pragma foreign_export("C", request_set_method(in, in) = out,
    "request_set_method").

request_set_method(Req0, MethodString) = Req :-
    ( method(MethodString, Method) ->
        Req = Req0 ^ method := Method
    ;
        Req = Req0 ^ method := other(MethodString)
    ).

:- pred method(string, method).
:- mode method(in, out) is semidet.
:- mode method(out, in) is semidet.

method("DELETE", delete).
method("GET", get).
method("HEAD", head).
method("POST", post).
method("PUT", put).

:- func request_set_url(request, string) = request.

:- pragma foreign_export("C", request_set_url(in, in) = out,
    "request_set_url").

request_set_url(Req, Url) = Req ^ url := Url.

:- func request_add_header(request, string, string) = request.

:- pragma foreign_export("C", request_add_header(in, in, in) = out,
    "request_add_header").

request_add_header(Req0, Field, Value) = Req :-
    Req0 ^ headers = Headers0,
    Headers = [Field - Value | Headers0],
    Req = Req0 ^ headers := Headers.

:- func request_get_expect_header(request) = int.

:- pragma foreign_export("C", request_get_expect_header(in) = out,
    "request_get_expect_header").

request_get_expect_header(Req) = Result :-
    get_expect_header(Req ^ headers, 0, Result).

:- pred get_expect_header(assoc_list(string, string)::in, int::in, int::out)
    is det.

get_expect_header([], Acc, Acc).
get_expect_header([Field - Value | Tail], Acc0, Acc) :-
    ( string_equal_ci(Field, "Expect") ->
        ( string_equal_ci(Value, "100-continue") ->
            get_expect_header(Tail, 1, Acc)
        ;
            Acc = -1
        )
    ;
        get_expect_header(Tail, Acc0, Acc)
    ).

:- func request_set_body_stringish(request, string) = request.

:- pragma foreign_export("C", request_set_body_stringish(in, in) = out,
    "request_set_body_stringish").

request_set_body_stringish(Req0, String) = Req :-
    Headers = wrap(Req0 ^ headers),
    (
        get_content_type(Headers, MediaType, _Params),
        media_type_equals(MediaType, application_x_www_form_urlencoded),
        parse_form_urlencoded(String, Form)
    ->
        Body = form_urlencoded(Form)
    ;
        Body = string(String)
    ),
    Req = Req0 ^ body := Body.

:- func application_x_www_form_urlencoded = string.

application_x_www_form_urlencoded = "application/x-www-form-urlencoded".

:- pred request_search_multipart_formdata_boundary(request::in, string::out)
    is semidet.

:- pragma foreign_export("C",
    request_search_multipart_formdata_boundary(in, out),
    "request_search_multipart_formdata_boundary").

request_search_multipart_formdata_boundary(Req, Boundary) :-
    Headers = wrap(Req ^ headers),
    search_multipart_formdata_boundary(Headers, Boundary).

:- func create_formdata_parser(string) = multipart_parser(formdata_accum).

:- pragma foreign_export("C", create_formdata_parser(in) = out,
    "create_formdata_parser").

create_formdata_parser(Boundary) =
    multipart_parser.init(Boundary, formdata_accum.init).

:- pred parse_formdata(buffer::in, int::in, int::out,
    multipart_parser(formdata_accum)::in, multipart_parser(formdata_accum)::out,
    bool::out, string::out, io::di, io::uo) is det.

:- pragma foreign_export("C",
    parse_formdata(in, in, out, in, out, out, out, di, uo),
    "parse_formdata").

parse_formdata(Buf, !BufPos, !PS, IsError, ErrorString, !IO) :-
    multipart_parser.execute(Buf, !BufPos, !PS, !IO),
    multipart_parser.get_error(!.PS, MaybeError),
    (
        MaybeError = ok,
        IsError = no,
        ErrorString = ""
    ;
        MaybeError = error(ErrorString),
        IsError = yes
    ).

:- func request_set_body_formdata(request, multipart_parser(formdata_accum))
    = request.

:- pragma foreign_export("C", request_set_body_formdata(in, in) = out,
    "request_set_body_formdata").

request_set_body_formdata(Req0, PS) = Req :-
    Parts = get_parts(get_userdata(PS)),
    Req = Req0 ^ body := multipart_formdata(Parts).

:- pred call_request_handler_pred(request_handler::in(request_handler),
    client::in, request::in, io::di, io::uo) is cc_multi.

:- pragma foreign_export("C",
    call_request_handler_pred(in(request_handler), in, in, di, uo),
    "call_request_handler_pred").

call_request_handler_pred(Pred, Client, Request, !IO) :-
    call(Pred, Client, Request, !IO).

%-----------------------------------------------------------------------------%

% C to Mercury

:- func client_should_keep_alive(client) = bool.

:- pragma foreign_proc("C",
    client_should_keep_alive(Client::in) = (KeepAlive::out),
    [will_not_call_mercury, promise_pure, thread_safe, may_not_duplicate],
"
    KeepAlive = (Client->should_keep_alive) ? MR_YES : MR_NO;
").

%-----------------------------------------------------------------------------%

% Utilities

:- pred string_equal_ci(string::in, string::in) is semidet.

string_equal_ci(A, B) :-
    string.to_lower(A, Lower),
    string.to_lower(B, Lower).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
