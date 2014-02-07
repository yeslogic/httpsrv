:- module multipart_parser.

% Copyright (C) 2014 YesLogic Pty. Ltd.
% All rights reserved.

:- interface.

:- import_module io.
:- import_module maybe.

:- import_module buffer.
:- import_module mime_headers.

%-----------------------------------------------------------------------------%

:- type multipart_parser(T).

:- func init(string, T) = multipart_parser(T).

:- func get_userdata(multipart_parser(T)) = T.

:- pred get_error(multipart_parser(T)::in, maybe_error::out) is det.

:- pred execute(buffer::in, int::in, int::out,
    multipart_parser(T)::in, multipart_parser(T)::out, io::di, io::uo) is det
    <= callbacks(T).

:- typeclass callbacks(T) where [
    pred on_headers_complete(headers::in, maybe_error::out, T::in, T::out)
            is det,
    pred on_body_chunk(c_pointer::in, int::in, T::in, T::out, io::di, io::uo)
            is det,
    pred on_part_end(T::in, T::out) is det
].

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module bool.
:- import_module char.
:- import_module int.
:- import_module require.
:- import_module string.

:- pragma foreign_decl("C", local, "
    #include <string.h> /* memmem is GNU extension */
").

%-----------------------------------------------------------------------------%

:- type multipart_parser(T)
    --->    multipart_parser(
                state           :: mpstate,
                open_boundary   :: string,  % --boundary\r\n
                close_boundary  :: string,  % \r\n--boundary
                boundary_length :: int,     % same for open/close_boundary
                userdata :: T
            ).

:- type mpstate
    --->    before_first_part
    ;       in_headers
    ;       in_body
    ;       just_after_part
    ;       after_final_part
    ;       error(string).

%-----------------------------------------------------------------------------%

init(Boundary, UserData) = PS :-
    OpenBoundary = "--" ++ Boundary ++ "\r\n",
    CloseBoundary = "\r\n--" ++ Boundary,
    string.count_code_units(OpenBoundary, BoundaryLength),
    string.count_code_units(CloseBoundary, CloseBoundaryLength),
    expect(unify(BoundaryLength, CloseBoundaryLength), $module, $pred,
        "open_boundary and close_boundary should have same length"),

    PS ^ state = before_first_part,
    PS ^ open_boundary = OpenBoundary,
    PS ^ close_boundary = CloseBoundary,
    PS ^ boundary_length = BoundaryLength,
    PS ^ userdata = UserData.

get_userdata(PS) = PS ^ userdata.

get_error(PS, Res) :-
    State = PS ^ state,
    (
        ( State = before_first_part
        ; State = in_headers
        ; State = in_body
        ; State = just_after_part
        ; State = after_final_part
        ),
        Res = ok
    ;
        State = error(Error),
        Res = error(Error)
    ).

%-----------------------------------------------------------------------------%

    % XXX major oversight in parser
    % [RFC 2046] The boundary may be followed by zero or more characters of
    % linear whitespace. It is then terminated by either another CRLF and
    % the header fields for the next part, or by two CRLFs, in which case
    % there are no header fields for the next part.

execute(Buf, !BufPos, !PS, !IO) :-
    !.PS ^ state = before_first_part,
    !.PS ^ open_boundary = OpenBoundary,
    !.PS ^ boundary_length = BoundaryLength,
    find(Buf, OpenBoundary, BoundaryLength, BoundaryPos, !BufPos, !IO),
    ( BoundaryPos >= 0 ->
        !PS ^ state := in_headers,
        execute(Buf, !BufPos, !PS, !IO)
    ;
        % XXX advance Buf to save memory if it grows too big
        true
    ).

execute(Buf, !BufPos, !PS, !IO) :-
    !.PS ^ state = in_headers,
    HeadersStartPos = !.BufPos,
    find_crlf_crlf(Buf, CrlfCrlfPos, !BufPos, !IO),
    ( CrlfCrlfPos >= 0 ->
        % The first CRLF is part of the last header field.
        HeadersEndPos = CrlfCrlfPos + 2,
        have_header_block(Buf, HeadersStartPos, HeadersEndPos, !PS, !IO),
        execute(Buf, !BufPos, !PS, !IO)
    ;
        % Should we abort if we find the open/close boundary first?
        true
    ).

execute(Buf, BufPos0, BufPos, !PS, !IO) :-
    !.PS ^ state = in_body,
    !.PS ^ close_boundary = CloseBoundary,
    !.PS ^ boundary_length = BoundaryLength,
    find(Buf, CloseBoundary, BoundaryLength, BodyEndPos, BufPos0, BufPos1,
        !IO),
    ( BodyEndPos >= 0 ->
        % Found the closing boundary.
        call_on_body_chunk(Buf, BufPos0, BodyEndPos, !PS, !IO),
        call_on_part_end(!PS),
        !PS ^ state := just_after_part,
        execute(Buf, BufPos1, BufPos, !PS, !IO)
    ;
        % Not yet found the closing boundary.
        % If necessary, give up smaller chunks of the body to prevent building
        % a large contiguous buffer, which is bad for GC.
        buffer.length(Buf, BufLen, !IO),
        Avail = BufLen - BoundaryLength - BufPos0,
        ( Avail >= body_chunk_size_threshold ->
            BufPos = BufPos0 + round_down_to_power_of_two(Avail),
            call_on_body_chunk(Buf, BufPos0, BufPos, !PS, !IO)
        ;
            BufPos = BufPos0
        )
    ).

execute(Buf, !BufPos, !PS, !IO) :-
    !.PS ^ state = just_after_part,
    % We have just seen the closing "\r\n--BOUNDARY".
    % The next two bytes determines what follows:
    %  CR/LF indicates there is another part,
    %  two hyphens indicates that was the final part.
    next_two_bytes(Buf, HaveBytes, ByteA, ByteB, !BufPos, !IO),
    (
        HaveBytes = no
        % Not enough data.
    ;
        HaveBytes = yes,
        (
            ByteA = char.to_int('\r'),
            ByteB = char.to_int('\n')
        ->
            % Continue with next part.
            !PS ^ state := in_headers,
            execute(Buf, !BufPos, !PS, !IO)
        ;
            ByteA = char.to_int('-'),
            ByteB = char.to_int('-')
        ->
            % We saw the final part.
            !PS ^ state := after_final_part,
            execute(Buf, !BufPos, !PS, !IO)
        ;
            !PS ^ state := error("improper closing boundary")
        )
    ).

execute(Buf, _BufPos0, BufPos, !PS, !IO) :-
    !.PS ^ state = after_final_part,
    % Just consume the rest.
    buffer.length(Buf, BufLen, !IO),
    BufPos = BufLen.

execute(_Buf, !BufPos, !PS, !IO) :-
    !.PS ^ state = error(_).

%-----------------------------------------------------------------------------%

:- pred have_header_block(buffer::in, int::in, int::in,
    multipart_parser(T)::in, multipart_parser(T)::out, io::di, io::uo) is det
    <= callbacks(T).

have_header_block(Buf, StartPos, EndPos, !PS, !IO) :-
    make_string_utf8(Buf, StartPos, EndPos, Ok, String, !IO),
    (
        Ok = yes,
        ( parse_headers(String, Headers) ->
            have_headers(Headers, !PS)
        ;
            !PS ^ state := error("error parsing headers")
        )
    ;
        Ok = no,
        !PS ^ state := error("headers not UTF-8")
    ).

:- pred have_headers(headers::in,
    multipart_parser(T)::in, multipart_parser(T)::out) is det <= callbacks(T).

have_headers(Headers, !PS) :-
    get_content_type(Headers, MediaType, _Params),
    ( media_type_equals(MediaType, multipart_mixed) ->
        % The encoding algorithm in the HTML5 spec avoids content type
        % multipart/mixed so supporting it is probably not a high priority.
        !PS ^ state := error("multipart/mixed not supported")
    ;
        call_on_headers_complete(Headers, Continue, !PS),
        (
            Continue = ok,
            !PS ^ state := in_body
        ;
            Continue = error(Error),
            !PS ^ state := error(Error)
        )
    ).

:- pred call_on_headers_complete(headers::in, maybe_error::out,
    multipart_parser(T)::in, multipart_parser(T)::out) is det <= callbacks(T).

call_on_headers_complete(Headers, Continue, PS0, PS) :-
    PS0 ^ userdata = UserData0,
    on_headers_complete(Headers, Continue, UserData0, UserData),
    PS = PS0 ^ userdata := UserData.

:- func multipart_mixed = string.

multipart_mixed = "multipart/mixed".

%-----------------------------------------------------------------------------%

:- pred call_on_body_chunk(buffer::in, int::in, int::in,
    multipart_parser(T)::in, multipart_parser(T)::out, io::di, io::uo) is det
    <= callbacks(T).

call_on_body_chunk(Buf, StartPos, EndPos, PS0, PS, !IO) :-
    Len = EndPos - StartPos,
    ( Len > 0 ->
        PS0 ^ userdata = UserData0,
        offset_pointer(Buf, StartPos, Ptr, !IO),
        on_body_chunk(Ptr, Len, UserData0, UserData, !IO),
        PS = PS0 ^ userdata := UserData
    ;
        PS = PS0
    ).

:- pred offset_pointer(buffer::in, int::in, c_pointer::out, io::di, io::uo)
    is det.

:- pragma foreign_proc("C",
    offset_pointer(Buf::in, Offset::in, Ptr::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Ptr = (MR_intptr_t) (Buf->data + Offset);
").

:- func body_chunk_size_threshold = int.

body_chunk_size_threshold = 2048.

:- func round_down_to_power_of_two(int) = int.

round_down_to_power_of_two(X) = POT :-
    int.log2(X + 1, Log + 1),  % int.log2 rounds up
    int.pow(2, Log, POT).

%-----------------------------------------------------------------------------%

:- pred call_on_part_end(multipart_parser(T)::in, multipart_parser(T)::out)
    is det <= callbacks(T).

call_on_part_end(PS0, PS) :-
    PS0 ^ userdata = UserData0,
    on_part_end(UserData0, UserData),
    PS = PS0 ^ userdata := UserData.

%-----------------------------------------------------------------------------%

    % Returns -1 if not found.
    %
:- pred find(buffer::in, string::in, int::in, int::out, int::in, int::out,
    io::di, io::uo) is det.

:- pragma foreign_proc("C",
    find(Buf::in, Needle::in, NeedleLen::in, NeedlePos::out,
        Pos0::in, Pos::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, may_not_duplicate],
"
    const void *haystack;
    size_t haystacklen;
    const void *p;

    haystack = Buf->data + Pos0;
    haystacklen = Buf->len - Pos0;
    p = memmem(haystack, haystacklen, Needle, NeedleLen);
    if (p != NULL) {
        NeedlePos = (const char *)p - Buf->data;
        Pos = NeedlePos + NeedleLen;
    } else {
        NeedlePos = -1;
        Pos = Pos0;
    }
").

:- pred find_crlf_crlf(buffer::in, int::out, int::in, int::out, io::di, io::uo)
    is det.

find_crlf_crlf(Buf, FoundPos, !BufPos, !IO) :-
    find(Buf, "\r\n\r\n", 4, FoundPos, !BufPos, !IO).

:- pred next_two_bytes(buffer::in, bool::out, int::out, int::out,
    int::in, int::out, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    next_two_bytes(Buf::in, HaveBytes::out, ByteA::out, ByteB::out,
        BufPos0::in, BufPos::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    if (BufPos0 + 2 <= Buf->len) {
        HaveBytes = MR_YES;
        ByteA = (unsigned char) Buf->data[BufPos0];
        ByteB = (unsigned char) Buf->data[BufPos0 + 1];
        BufPos = BufPos0 + 2;
    } else {
        HaveBytes = MR_NO;
        ByteA = -1;
        ByteB = -1;
        BufPos = BufPos0;
    }
").

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
