:- module multipart_parser.

% Copyright (C) 2014 YesLogic Pty. Ltd.
% All rights reserved.

:- interface.

:- import_module io.
:- import_module maybe.

:- import_module buffer.
:- import_module case_insensitive.

%-----------------------------------------------------------------------------%

:- type multipart_parser(T).

:- func init(string, T) = multipart_parser(T).

:- func get_userdata(multipart_parser(T)) = T.

:- pred get_error(multipart_parser(T)::in, maybe_error::out) is det.

:- pred valid_final_state(multipart_parser(T)::in) is semidet.

:- pred execute(buffer::in, int::in, int::out,
    multipart_parser(T)::in, multipart_parser(T)::out, io::di, io::uo) is det
    <= callbacks(T).

:- typeclass callbacks(T) where [
    pred on_headers_complete(on_headers_complete_info::in, maybe_error::out,
            T::in, T::out) is det,
    pred on_body_chunk(c_pointer::in, int::in, T::in, T::out, io::di, io::uo)
            is det,
    pred on_part_end(T::in, T::out) is det
].

:- type on_headers_complete_info
    --->    on_headers_complete_info(
                content_disposition_type :: maybe(case_insensitive),
                content_disposition_name :: maybe(string),
                content_disposition_filename :: maybe(string),
                content_type_media_type :: case_insensitive,
                content_transfer_encoding_mechanism :: maybe(case_insensitive)
            ).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module list.
:- import_module pair.
:- import_module string.

:- import_module case_insensitive.
:- import_module headers.
:- use_module rfc2045.
:- use_module rfc2183.
:- use_module rfc2822.

%-----------------------------------------------------------------------------%

:- type multipart_parser(T)
    --->    multipart_parser(
                state                   :: mpstate,
                dash_boundary           :: string,  % --boundary
                dash_boundary_length    :: int,
                delimiter               :: string,  % \r\n--boundary
                delimiter_length        :: int,
                userdata                :: T
            ).

:- type mpstate
    --->    preamble                    % expecting: dash-boundary
    ;       seen_dash_boundary          % expecting: transport-padding CRLF
    ;       in_part_headers             % looking for CRLF CRLF
    ;       in_part_body                % looking for delimiter
    ;       just_seen_delimiter         % expecting: transport-padding CRLF or "--"
    ;       seen_close_delimiter        % expecting: transport-padding
                                        %            [CRLF epilogue]
    ;       epilogue
    ;       error(string).

/*
     multipart-body := [preamble CRLF]
                       dash-boundary transport-padding CRLF
                       body-part *encapsulation
                       close-delimiter transport-padding
                       [CRLF epilogue]

     dash-boundary := "--" boundary

     transport-padding := *LWSP-char

     body-part := MIME-part-headers [CRLF *OCTET]

     encapsulation := delimiter transport-padding CRLF body-part

     delimiter := CRLF dash-boundary

     close-delimiter := delimiter "--"
*/

%-----------------------------------------------------------------------------%

:- pragma foreign_decl("C", local, "
#include <string.h> /* memmem is GNU extension */

static void *
local_memmem(const void *haystack, size_t haystacklen,
    const void *needle, size_t needlelen);
").

:- pragma foreign_code("C",
"
static void *
local_memmem(const void *haystack, size_t haystacklen,
             const void *needle, size_t needlelen)
{
#ifdef _GNU_SOURCE
    return memmem(haystack, haystacklen, needle, needlelen);
#else
    const unsigned char *hayst = haystack;
    const unsigned char *needl = needle;
    const unsigned char *p = haystack;

    if (needlelen == 0)
        return (void *) haystack;

    while (needlelen <= haystacklen - (p - hayst)) {
        p = memchr(p, needl[0], haystacklen - (p - hayst));
        if (p == NULL)
            break;
        if (0 == memcmp(p, needle, needlelen))
            return (void *) p;
        p++;
    }

    return NULL;
#endif
}
").

%-----------------------------------------------------------------------------%

init(Boundary, UserData) = PS :-
    DashBoundary = "--" ++ Boundary,
    Delimiter = "\r\n" ++ DashBoundary,

    PS ^ state = preamble,
    PS ^ dash_boundary = DashBoundary,
    PS ^ dash_boundary_length = count_code_units(DashBoundary),
    PS ^ delimiter = Delimiter,
    PS ^ delimiter_length = count_code_units(Delimiter),
    PS ^ userdata = UserData.

get_userdata(PS) = PS ^ userdata.

get_error(PS, Res) :-
    State = PS ^ state,
    (
        ( State = preamble
        ; State = seen_dash_boundary
        ; State = in_part_headers
        ; State = in_part_body
        ; State = just_seen_delimiter
        ; State = seen_close_delimiter
        ; State = epilogue
        ),
        Res = ok
    ;
        State = error(Error),
        Res = error(Error)
    ).

valid_final_state(PS) :-
    State = PS ^ state,
    require_complete_switch [State]
    (
        State = seen_close_delimiter
    ;
        State = epilogue
    ;
        ( State = preamble
        ; State = seen_dash_boundary
        ; State = in_part_headers
        ; State = in_part_body
        ; State = just_seen_delimiter
        ; State = error(_)
        ),
        fail
    ).

%-----------------------------------------------------------------------------%

execute(Buf, !BufPos, !PS, !IO) :-
    !.PS ^ state = preamble,
    !.PS ^ dash_boundary = DashBoundary,
    !.PS ^ dash_boundary_length = DashBoundaryLength,
    find(Buf, DashBoundary, DashBoundaryLength, BoundaryPos, !BufPos, !IO),
    ( BoundaryPos >= 0 ->
        !PS ^ state := seen_dash_boundary,
        execute(Buf, !BufPos, !PS, !IO)
    ;
        % Don't wait too long to find the boundary.
        buffer.length(Buf, BufLen, !IO),
        PreambleSize = BufLen - !.BufPos,
        ( PreambleSize > max_preamble_size ->
            !PS ^ state := error("maximum preamble size exceeded")
        ;
            true
        )
    ).

execute(Buf, !BufPos, !PS, !IO) :-
    !.PS ^ state = seen_dash_boundary,
    skip_transport_padding_CRLF(Buf, !BufPos, SkipResult, !IO),
    ( SkipResult = 0 ->
        true
    ; SkipResult = 1 ->
        !PS ^ state := in_part_headers,
        execute(Buf, !BufPos, !PS, !IO)
    ;
        !PS ^ state := error("invalid opening boundary line")
    ).

execute(Buf, !BufPos, !PS, !IO) :-
    !.PS ^ state = in_part_headers,
    HeadersStartPos = !.BufPos,
    find_crlf_crlf(Buf, CrlfCrlfPos, !BufPos, !IO),
    ( CrlfCrlfPos >= 0 ->
        % The first CRLF is part of the last header field.
        HeadersEndPos = CrlfCrlfPos + 2,
        have_header_block(Buf, HeadersStartPos, HeadersEndPos, !PS, !IO),
        execute(Buf, !BufPos, !PS, !IO)
    ;
        % Should we abort if we find the boundary / delimiter first?
        true
    ).

execute(Buf, BufPos0, BufPos, !PS, !IO) :-
    !.PS ^ state = in_part_body,
    !.PS ^ delimiter = Delimiter,
    !.PS ^ delimiter_length = DelimiterLength,
    find(Buf, Delimiter, DelimiterLength, BodyEndPos, BufPos0, BufPos1, !IO),
    ( BodyEndPos >= 0 ->
        % Found the delimiter or close-delimiter.
        call_on_body_chunk(Buf, BufPos0, BodyEndPos, !PS, !IO),
        call_on_part_end(!PS),
        !PS ^ state := just_seen_delimiter,
        execute(Buf, BufPos1, BufPos, !PS, !IO)
    ;
        % Not yet found the delimiter.
        % If necessary, give up smaller chunks of the body to prevent building
        % a large contiguous buffer, which is bad for GC.
        buffer.length(Buf, BufLen, !IO),
        Avail = BufLen - DelimiterLength - BufPos0,
        ( Avail >= body_chunk_size_threshold ->
            BufPos = BufPos0 + round_down_to_power_of_two(Avail),
            call_on_body_chunk(Buf, BufPos0, BufPos, !PS, !IO)
        ;
            BufPos = BufPos0
        )
    ).

execute(Buf, !BufPos, !PS, !IO) :-
    !.PS ^ state = just_seen_delimiter,
    % We have just seen the delimiter "\r\n--boundary".
    after_delimiter(Buf, !BufPos, Result, !IO),
    ( Result = 0 ->
        % Not enough data.
        true
    ; Result = 1 ->
        % After the delimiter was two hyphens.
        !PS ^ state := seen_close_delimiter,
        execute(Buf, !BufPos, !PS, !IO)
    ;
        % After the delimiter was NOT two hyphens.
        !PS ^ state := seen_dash_boundary,
        execute(Buf, !BufPos, !PS, !IO)
    ).

execute(Buf, !BufPos, !PS, !IO) :-
    !.PS ^ state = seen_close_delimiter,
    skip_transport_padding_CRLF(Buf, !BufPos, SkipResult, !IO),
    ( SkipResult = 0 ->
        % Not found yet.
        true
    ; SkipResult = 1 ->
        % Found CRLF.
        !PS ^ state := epilogue,
        execute(Buf, !BufPos, !PS, !IO)
    ;
        !PS ^ state := error("invalid closing boundary line")
    ).

execute(Buf, _BufPos0, BufPos, !PS, !IO) :-
    !.PS ^ state = epilogue,
    % Just consume the rest.  However, note that:
    % [RFC 2616] Unlike in RFC 2046, the epilogue of any multipart message MUST
    % be empty; HTTP applications MUST NOT transmit the epilogue (even if the
    % original multipart contains an epilogue).
    buffer.length(Buf, BufLen, !IO),
    BufPos = BufLen.

execute(_Buf, !BufPos, !PS, !IO) :-
    !.PS ^ state = error(_).

%-----------------------------------------------------------------------------%

:- func max_preamble_size = int.

max_preamble_size = 128.

%-----------------------------------------------------------------------------%

:- pred have_header_block(buffer::in, int::in, int::in,
    multipart_parser(T)::in, multipart_parser(T)::out, io::di, io::uo) is det
    <= callbacks(T).

have_header_block(Buf, StartPos, EndPos, !PS, !IO) :-
    % MIME headers should be restricted to US-ASCII but browsers do send
    % unencoded octets, and those octets can be UTF-8 if we ask. That's nice.
    make_string_utf8(Buf, StartPos, EndPos, MaybeString, !IO),
    (
        MaybeString = yes(String),
        ( rfc2822.parse_fields(String, Fields) ->
            ( from_assoc_list(reject_duplicate_header, Fields, Headers) ->
                have_headers(Headers, !PS)
            ;
                !PS ^ state := error("duplicate header field")
            )
        ;
            !PS ^ state := error("error parsing headers")
        )
    ;
        MaybeString = no,
        !PS ^ state := error("headers not UTF-8")
    ).

:- pred have_headers(headers::in,
    multipart_parser(T)::in, multipart_parser(T)::out) is det <= callbacks(T).

have_headers(Headers, !PS) :-
    get_content_type_or_default(Headers, MediaType, _Params),
    ( MediaType = multipart_mixed ->
        % The encoding algorithm in the HTML5 spec avoids content type
        % multipart/mixed so supporting it is probably not a high priority.
        !PS ^ state := error("multipart/mixed not supported")
    ;
        make_on_headers_complete_info(Headers, MediaType, Info),
        call_on_headers_complete(Info, Continue, !PS),
        (
            Continue = ok,
            !PS ^ state := in_part_body
        ;
            Continue = error(Error),
            !PS ^ state := error(Error)
        )
    ).

:- pred get_content_type_or_default(headers::in, case_insensitive::out,
    parameters::out) is det.

get_content_type_or_default(Headers, MediaType, Params) :-
    (
        search_field(Headers, content_type, [Body]),
        rfc2822.parse_structured_field_body(Body, rfc2045.content_type_body,
            Output)
    ->
        Output = MediaType - ParamsMap,
        Params = init_parameters_from_map(ParamsMap)
    ;
        rfc2045.content_type_defaults(MediaType, ParamsMap),
        Params = init_parameters_from_map(ParamsMap)
    ).

:- pred make_on_headers_complete_info(headers::in, case_insensitive::in,
    on_headers_complete_info::out) is det.

make_on_headers_complete_info(Headers, MediaType, Info) :-
    ( search_content_disposition(Headers, Disposition, Params) ->
        MaybeDispositionType = yes(Disposition),
        ( search_parameter(Params, name, Name) ->
            MaybeName = yes(Name)
        ;
            MaybeName = no
        ),
        ( search_parameter(Params, filename, FileName) ->
            MaybeFileName = yes(FileName)
        ;
            MaybeFileName = no
        )
    ;
        MaybeDispositionType = no,
        MaybeName = no,
        MaybeFileName = no
    ),
    ( search_content_transfer_encoding(Headers, CTE) ->
        MaybeCTE = yes(CTE)
    ;
        MaybeCTE = no
    ),
    Info ^ content_disposition_type = MaybeDispositionType,
    Info ^ content_disposition_name = MaybeName,
    Info ^ content_disposition_filename = MaybeFileName,
    Info ^ content_type_media_type = MediaType,
    Info ^ content_transfer_encoding_mechanism = MaybeCTE.

:- pred call_on_headers_complete(on_headers_complete_info::in,
    maybe_error::out, multipart_parser(T)::in, multipart_parser(T)::out)
    is det <= callbacks(T).

call_on_headers_complete(Info, Continue, PS0, PS) :-
    PS0 ^ userdata = UserData0,
    on_headers_complete(Info, Continue, UserData0, UserData),
    PS = PS0 ^ userdata := UserData.

:- func content_type = case_insensitive.

content_type = case_insensitive("content-type").

:- func multipart_mixed = case_insensitive.

multipart_mixed = case_insensitive("multipart/mixed").

:- func name = case_insensitive.

name = case_insensitive("name").

:- func filename = case_insensitive.

filename = case_insensitive("filename").

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
    p = local_memmem(haystack, haystacklen, Needle, NeedleLen);
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

:- pred skip_transport_padding_CRLF(buffer::in, int::in, int::out, int::out,
    io::di, io::uo) is det.

:- pragma foreign_proc("C",
    skip_transport_padding_CRLF(Buf::in, BufPos0::in, BufPos::out,
        SkipResult::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io,
        may_not_duplicate],
"
    /* Skip LWSP-chars. */
    while (BufPos0 < Buf->len) {
        unsigned char c = Buf->data[BufPos0];
        if (c != ' ' && c != '\t')
            break;
        BufPos0++;
    }
    BufPos = BufPos0;

    if (Buf->len < BufPos + 2) {
        SkipResult = 0; /* not found yet */
    } else if (Buf->data[BufPos] == '\\r' && Buf->data[BufPos + 1] == '\\n') {
        BufPos += 2;
        SkipResult = 1; /* found CRLF */
    } else {
        SkipResult = -1; /* error */
    }
").

:- pred after_delimiter(buffer::in, int::in, int::out, int::out,
    io::di, io::uo) is det.

:- pragma foreign_proc("C",
    after_delimiter(Buf::in, BufPos0::in, BufPos::out, Result::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io,
        may_not_duplicate],
"
    BufPos = BufPos0;

    if (Buf->len < BufPos + 2) {
        Result = 0; /* not enough data */
    } else if (Buf->data[BufPos] == '-' && Buf->data[BufPos + 1] == '-') {
        BufPos += 2;
        Result = 1; /* found two hyphens */
    } else {
        Result = 2; /* something else */
    }
").

%-----------------------------------------------------------------------------%

:- pred search_content_transfer_encoding(headers::in, case_insensitive::out)
    is semidet.

search_content_transfer_encoding(Headers, Mechanism) :-
    search_field(Headers, content_transfer_encoding, [Body]),
    rfc2822.parse_structured_field_body(Body,
        rfc2045.content_transfer_encoding_body, Mechanism).

:- func content_transfer_encoding = case_insensitive.

content_transfer_encoding = case_insensitive("content-transfer-encoding").

%-----------------------------------------------------------------------------%

:- pred search_content_disposition(headers::in, case_insensitive::out,
    parameters::out) is semidet.

search_content_disposition(Headers, DispositionType, Params) :-
    search_field(Headers, content_disposition_header, [Body]),
    rfc2822.parse_structured_field_body(Body, rfc2183.content_disposition_body,
        DispositionType - ParamsMap),
    Params = init_parameters_from_map(ParamsMap).

:- func content_disposition_header = case_insensitive.

content_disposition_header = case_insensitive("content-disposition").

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
