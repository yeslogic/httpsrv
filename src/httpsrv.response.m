:- module httpsrv.response.

% Copyright (C) 2014, 2018 YesLogic Pty. Ltd.
% All rights reserved.

:- interface.

:- pred set_response(request::in, response::in, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module cord.
:- import_module int.
:- import_module string.

:- import_module http_date.
:- import_module httpsrv.response_header.

:- pragma foreign_decl("C", local, include_file("httpsrv1.h")).

:- type uv_buf_array.

:- pragma foreign_type("C", uv_buf_array, "uv_buf_t *").

%-----------------------------------------------------------------------------%

set_response(Request, response(Status, AdditionalHeaders, Content), !IO) :-
    Client = Request ^ client,
    time(Time, !IO),
    HttpDate = timestamp_to_http_date(Time),
    client_should_keep_alive(Client, KeepAlive, !IO),
    (
        KeepAlive = yes,
        MaybeConnectionClose = ""
    ;
        KeepAlive = no,
        MaybeConnectionClose = "Connection: close\r\n"
    ),
    (
        (
            Content = strings(ContentStrings),
            ContentSlices = list.map(string_slice, ContentStrings)
        ;
            Content = slices(ContentSlices)
        ),
        ContentLength = sum_length(ContentSlices),
        MaybeLastModified = "",
        ( skip_body(Request) ->
            BodySlices = []
        ;
            BodySlices = ContentSlices
        ),
        SetFileFd = -1,
        SetFileSize = 0
    ;
        Content = file(StaticFile),
        ContentLength = StaticFile ^ file_size,
        Mtime = StaticFile ^ file_mtime,
        MaybeLastModified =
            "Last-Modified: " ++ timestamp_to_http_date(Mtime) ++ "\r\n",
        BodySlices = [],
        ( skip_body(Request) ->
            static_file.close_static_file(StaticFile, !IO),
            SetFileFd = -1,
            SetFileSize = 0
        ;
            StaticFile = static_file(SetFileFd, SetFileSize, _Mtime)
        )
    ),

    Header = append_list([
        "HTTP/1.1 ", text(Status), "\r\n",
        "Date: ", HttpDate, "\r\n" |
        Header1
    ]),
    Header1 = map(render_response_header, AdditionalHeaders) ++ Header2,
    Header2 = [
        "Content-Length: ", from_int(ContentLength), "\r\n",
        MaybeLastModified,
        MaybeConnectionClose,
        "\r\n"
    ],
    HeaderSlice = string_slice(Header),

    some [!BufArray] (
        BufArrayLength = 1 + length(BodySlices),
        alloc_uv_buf_array(BufArrayLength, !:BufArray),
        set_uv_buf_array_element(0, HeaderSlice, !BufArray),
        set_uv_buf_array_elements(1, BodySlices, !BufArray),
        do_set_response(Client, !.BufArray, BufArrayLength,
            SetFileFd, SetFileSize, !IO)
    ).

:- pred client_should_keep_alive(client::in, bool::out, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    client_should_keep_alive(Client::in, KeepAlive::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io,
        may_not_duplicate],
"
    KeepAlive = _httpsrv_client_should_keep_alive(Client) ? MR_YES : MR_NO;
").

:- pred do_set_response(client::in, uv_buf_array::di, int::in,
    int::in, int::in, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    do_set_response(Client::in, BufArray::di, BufArrayLength::in,
        FileFd::in, FileLength::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io,
        may_not_duplicate],
"
    _httpsrv_set_response_bufs(Client, BufArray, BufArrayLength,
        FileFd, FileLength);
").

:- pred skip_body(request::in) is semidet.

skip_body(Request) :-
    Request ^ method = head.

:- func sum_length(list(slice)) = int.

sum_length(Xs) = foldl(plus, map(slice_length, Xs), 0).

:- func slice_length(slice) = int.

slice_length(slice(_, Length)) = Length.

:- func string_slice(string) = slice.

string_slice(S) = slice(string_c_pointer(S), string.length(S)).

:- func string_c_pointer(string) = c_pointer.

:- pragma foreign_proc("C",
    string_c_pointer(S::in) = (Ptr::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Ptr = (MR_Word) S;
").

:- pred alloc_uv_buf_array(int::in, uv_buf_array::uo) is det.

:- pragma foreign_proc("C",
    alloc_uv_buf_array(Length::in, BufArray::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    BufArray = MR_GC_NEW_ARRAY(uv_buf_t, Length);
").

:- pred set_uv_buf_array_elements(int::in, list(slice)::in,
    uv_buf_array::di, uv_buf_array::uo) is det.

set_uv_buf_array_elements(_Index, [], !BufArray).
set_uv_buf_array_elements(Index, [X | Xs], !BufArray) :-
    set_uv_buf_array_element(Index, X, !BufArray),
    set_uv_buf_array_elements(Index + 1, Xs, !BufArray).

:- pred set_uv_buf_array_element(int::in, slice::in,
    uv_buf_array::di, uv_buf_array::uo) is det.

set_uv_buf_array_element(Index, slice(Base, Length), !BufArray) :-
    set_uv_buf_array_element_2(Index, Base, Length, !BufArray).

:- pred set_uv_buf_array_element_2(int::in, c_pointer::in, int::in,
    uv_buf_array::di, uv_buf_array::uo) is det.

:- pragma foreign_proc("C",
    set_uv_buf_array_element_2(Index::in, Base::in, Length::in,
        BufArray0::di, BufArray::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    BufArray = BufArray0;
    BufArray[Index] = uv_buf_init((char *) Base, Length);
").

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
