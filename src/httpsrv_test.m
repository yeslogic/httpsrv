:- module httpsrv_test.

% Copyright (C) 2014 YesLogic Pty. Ltd.
% All rights reserved.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module assoc_list.
:- import_module dir.
:- import_module list.
:- import_module maybe.
:- import_module pair.
:- import_module string.
:- import_module thread.

:- import_module httpsrv.
:- import_module httpsrv.status.
:- import_module httpsrv.signal.

%-----------------------------------------------------------------------------%

main(!IO) :-
    ignore_sigpipe(!IO),
    Settings = [
        bind_address("0.0.0.0" @ BindAddress),
        port(8000 @ Port),
        back_log(8),
        max_body(1000000)
    ],
    setup(request_handler, Settings, Res, !IO),
    (
        Res = ok(Daemon),
        add_periodic(Daemon, 5000, periodic_handler("A"), !IO),
        add_periodic(Daemon, 10000, periodic_handler("B"), !IO),
        io.format("Started server on %s port %d\n",
            [s(BindAddress), i(Port)], !IO),
        run(Daemon, !IO),
        io.write_string("Server stopped.\n", !IO)
    ;
        Res = error(Error),
        io.format(io.stderr_stream, "setup failed: %s\n", [s(Error)], !IO),
        io.set_exit_status(1, !IO)
    ).

:- pred request_handler(request::in, io::di, io::uo) is cc_multi.

request_handler(Request, !IO) :-
    % real_handler(Request, !IO).
    thread.spawn(real_handler(Request), !IO).

:- pred real_handler(request::in, io::di, io::uo) is cc_multi.

real_handler(Request, !IO) :-
    usleep(100000, !IO),
    MaybePathDecoded = get_path_decoded(Request),
    (
        MaybePathDecoded = yes(PathDecoded),
        Resource = words_separator(unify('/'), PathDecoded)
    ;
        MaybePathDecoded = no,
        Resource = []
    ),
    ( static_path(Resource, FilePath) ->
        static_path_handler(Request, FilePath, Response, !IO)
    ; upload_path(Resource) ->
        upload_handler(Request, Response, !IO)
    ;
        echo_handler(Request, Response, !IO)
    ),
    set_response(Request, Response, !IO),
    cc_multi_equal(!IO).

%-----------------------------------------------------------------------------%

:- pred echo_handler(request::in, response::out, io::di, io::uo) is det.

echo_handler(Request, response(Status, AdditionalHeaders, Content), !IO) :-
    get_client_address_ipv4(Request, MaybeClientAddress, !IO),
    Status = ok_200,
    AdditionalHeaders = [
        content_type_charset_utf8("text/plain"),
        set_cookie("my-cookie" - "my-cookie-value", [])
    ],
    Method = get_method(Request),
    Url = get_url(Request),
    MaybePathDecoded = get_path_decoded(Request),
    QueryParams = get_query_parameters(Request),
    Headers = get_headers(Request),
    Cookies = get_cookies(Request),
    Body = get_body(Request),
    Content = strings([
        "Client address: ", string(MaybeClientAddress), "\n",
        "Method: ", string(Method), "\n",
        "URL: ", string(Url), "\n",
        "Path: ", string(MaybePathDecoded), "\n",
        "Query parameters: ", string(QueryParams), "\n",
        "Headers: ", string(Headers), "\n",
        "Cookies: ", string(Cookies), "\n",
        "Body: ", string(Body), "\n"
    ] ++ present(Body)).

:- func present(content) = list(string).

present(string(String)) = ["«", String, "»\n"].
present(form_urlencoded(Pairs)) =
    condense(map(present_pairs, Pairs)).
present(multipart_formdata(FormData)) =
    condense(map(present_formdata, FormData)).

:- func present_pairs(pair(string)) = list(string).

present_pairs(Name - Value) = [Name, "=«", Value, "»\n"].

:- func present_formdata(pair(string, formdata)) = list(string).

present_formdata(Name - FormData) = L :-
    FileName = FormData ^ filename,
    MediaType = FormData ^ media_type,
    CTE = FormData ^ content_transfer_encoding,
    Content = FormData ^ content,
    ContentLength = formdata_content_length(Content),
    ( formdata_content_to_string(Content, String) ->
        PresentString = ["«", String, "»\n"]
    ;
        PresentString = ["(not a UTF-8 string)\n"]
    ),
    L = [
        "--------\n",
        "name=", Name, "\n",
        "filename=", string(FileName), "\n",
        "mediatype=", string(MediaType), "\n",
        "content-transfer-encoding=", string(CTE), "\n",
        "length=", from_int(ContentLength), "\n"
        | PresentString
    ].

%-----------------------------------------------------------------------------%

:- pred static_path(list(string)::in, string::out) is semidet.

static_path(Resource, StaticPath) :-
    Resource = ["static" | _],
    StaticPath = string.join_list("/", Resource).

:- pred static_path_handler(request::in, string::in, response::out,
    io::di, io::uo) is det.

static_path_handler(Request, FilePath,
        response(Status, AdditionalHeaders, Content), !IO) :-
    Method = get_method(Request),
    (
        ( Method = get
        ; Method = head
        ),
        open_static_file(FilePath, OpenResult, !IO),
        (
            OpenResult = ok(StaticFile),
            Status = ok_200,
            AdditionalHeaders = [guess_content_type(FilePath)],
            Content = file(StaticFile)
        ;
            OpenResult = error(Error),
            Status = not_found_404,
            AdditionalHeaders = [],
            Content = strings([Error])
        )
    ;
        Method = post,
        Status = forbidden_403,
        AdditionalHeaders = [],
        Content = strings([])
    ;
        ( Method = put
        ; Method = delete
        ; Method = other(_)
        ),
        Status = not_implemented_501,
        AdditionalHeaders = [],
        Content = strings([])
    ).

:- func guess_content_type(string) = response_header.

guess_content_type(FileName) = Header :-
    ( string.suffix(FileName, ".html") ->
        Header = content_type_charset_utf8("text/html")
    ;
        Header = content_type("application/octet-stream")
    ).

%-----------------------------------------------------------------------------%

:- pred upload_path(list(string)::in) is semidet.

upload_path(["upload"]).

:- pred upload_handler(request::in, response::out, io::di, io::uo) is det.

upload_handler(Request, Response, !IO) :-
    Method = get_method(Request),
    (
        Method = post,
        RequestBody = get_body(Request),
        (
            RequestBody = multipart_formdata(Form),
            assoc_list.search(Form, "upload", Upload),
            Upload ^ filename = yes(UploadFileName0),
            Upload ^ content = UploadContent,
            UploadFileName = dir.basename(UploadFileName0),
            UploadFileName \= ""
        ->
            FilePath = "static" / UploadFileName,
            upload_handler_2(Request, FilePath, UploadContent, Response, !IO)
        ;
            Response = response(bad_request_400, [], strings([]))
        )
    ;
        ( Method = get
        ; Method = head
        ),
        Response = response(forbidden_403, [], strings([]))
    ;
        ( Method = put
        ; Method = delete
        ; Method = other(_)
        ),
        Response = response(not_implemented_501, [], strings([]))
    ).

:- pred upload_handler_2(request::in, string::in, formdata_content::in,
    response::out, io::di, io::uo) is det.

upload_handler_2(Request, FilePath, UploadContent, Response, !IO) :-
    write_formdata_content_to_file(FilePath, UploadContent, WriteRes, !IO),
    (
        WriteRes = ok,
        Status = created_201,
        OrigUrl = get_url(Request),
        AbsoluteUrl = make_location(OrigUrl, FilePath),
        AdditionalHeaders = [location(AbsoluteUrl)],
        Content = strings([])
    ;
        WriteRes = error(Error),
        % If insuffient disk could be 507 Insufficient Storage.
        Status = internal_server_error_500,
        AdditionalHeaders = [],
        Content = strings([Error, "\n"])
    ),
    Response = response(Status, AdditionalHeaders, Content).

:- func make_location(url, string) = string.

make_location(OrigUrl, FilePath) = AbsUrl :-
    OrigScheme = OrigUrl ^ scheme,
    OrigHost = OrigUrl ^ host,
    OrigPort = OrigUrl ^ port,
    (
        OrigScheme = yes(Scheme)
    ;
        OrigScheme = no,
        Scheme = "http"
    ),
    (
        OrigHost = yes(Host)
    ;
        OrigHost = no,
        Host = "localhost" % not really
    ),
    (
        OrigPort = yes(Port),
        HostPort = Host ++ ":" ++ Port
    ;
        OrigPort = no,
        HostPort = Host
    ),
    AbsUrl = Scheme ++ "://" ++ HostPort ++ "/" ++ FilePath.

%-----------------------------------------------------------------------------%

:- pred periodic_handler(string::in, io::di, io::uo) is det.

periodic_handler(Id, !IO) :-
    io.format("periodic handler %s - begin\n", [s(Id)], !IO),
    usleep(100000, !IO),
    io.format("periodic handler %s - end\n", [s(Id)], !IO).

%-----------------------------------------------------------------------------%

:- pred usleep(int::in, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    usleep(N::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    usleep(N);
").

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
