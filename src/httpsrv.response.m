:- module httpsrv.response.

% Copyright (C) 2014 YesLogic Pty. Ltd.
% All rights reserved.

:- interface.

:- pred set_response(request::in, status_code::in, list(response_header)::in,
    response_content::in, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module cord.
:- import_module int.
:- import_module string.

:- import_module http_date.
:- import_module httpsrv.response_header.

:- pragma foreign_decl("C", local, "
    #include ""httpsrv1.h""
").

%-----------------------------------------------------------------------------%

set_response(Request, Status, AdditionalHeaders, Content, !IO) :-
    Client = Request ^ client,
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
    (
        Content = strings(ContentStrings),
        ContentLength = sum_length(ContentStrings),
        ( skip_body(Request) ->
            Body = cord.init
        ;
            Body = cord.from_list(ContentStrings)
        ),
        FileFd = -1,
        FileSize = 0
    ;
        Content = file(StaticFile),
        ContentLength = StaticFile ^ file_size,
        Body = cord.init,
        ( skip_body(Request) ->
            static_file.close_static_file(StaticFile, !IO),
            FileFd = -1,
            FileSize = 0
        ;
            StaticFile = static_file(FileFd, FileSize)
        )
    ),

    HeaderPre = cord.from_list([
        "HTTP/1.1 ", text(Status), "\r\n",
        "Date: ", HttpDate, "\r\n"
    ]),
    HeaderMid = list.map(render_response_header, AdditionalHeaders),
    HeaderPost = cord.from_list([
        "Content-Length: ", from_int(ContentLength), "\r\n",
        MaybeConnectionClose,
        "\r\n"
    ]),
    ResponseCord = HeaderPre ++ cord_list_to_cord(HeaderMid) ++ HeaderPost ++
        Body,
    ResponseList = cord.list(ResponseCord),
    set_response_2(Client, ResponseList, length(ResponseList),
        FileFd, FileSize, !IO).

:- pred set_response_2(client::in, list(string)::in, int::in,
    int::in, int::in, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    set_response_2(Client::in, ResponseList::in, ResponseListLength::in,
        FileFd::in, FileLength::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io,
        may_not_duplicate],
"
    _httpsrv_set_response_bufs(Client, ResponseList, ResponseListLength,
        FileFd, FileLength);
    _httpsrv_send_async(Client);
").

:- pred skip_body(request::in) is semidet.

skip_body(Request) :-
    Request ^ method = head.

:- func sum_length(list(string)) = int.

sum_length(Xs) = foldl(plus, map(length, Xs), 0).

:- func client_should_keep_alive(client) = bool.

:- pragma foreign_proc("C",
    client_should_keep_alive(Client::in) = (KeepAlive::out),
    [will_not_call_mercury, promise_pure, thread_safe, may_not_duplicate],
"
    KeepAlive = (Client->should_keep_alive) ? MR_YES : MR_NO;
").

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
