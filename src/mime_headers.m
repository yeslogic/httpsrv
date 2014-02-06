:- module mime_headers.

% Copyright (C) 2014 YesLogic Pty. Ltd.
% All rights reserved.

:- interface.

:- import_module assoc_list.

:- type headers.

:- type parameters.

:- type media_type
    --->    media_type(string). % case-insensitive

:- func wrap(assoc_list(string, string)) = headers.

:- pred parse_headers(string::in, headers::out) is semidet.

:- pred search_header(headers::in, string::in, string::out) is semidet.

:- pred parse_header_value(string::in, string::out, parameters::out) is det.

:- pred search_parameter(parameters::in, string::in, string::out) is semidet.

:- pred search_content_disposition(headers::in, string::out, parameters::out)
    is semidet.

:- pred get_content_type(headers::in, media_type::out, parameters::out) is det.

:- pred media_type_equals(media_type::in, string::in) is semidet.

:- pred search_multipart_formdata_boundary(headers::in, string::out)
    is semidet.

:- pred search_content_transfer_encoding(headers::in, string::out)
    is semidet.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module list.
:- import_module pair.
:- import_module require.
:- import_module string.

:- type headers == assoc_list(string, string).

:- type parameters == list(string). % stripped

%-----------------------------------------------------------------------------%

wrap(Headers) = Headers.

%-----------------------------------------------------------------------------%

    % XXX MIME headers can be folded, but in HTTP?
    %
parse_headers(HeaderBlock, Headers) :-
    Lines = string.split_at_string(crlf, HeaderBlock),
    list.map(parse_header_pair, Lines, Headers).

:- pred parse_header_pair(string::in, pair(string, string)::out) is semidet.

parse_header_pair(Line, Field - Value) :-
    string.sub_string_search(Line, ":", ColonPos),
    string.unsafe_between(Line, 0, ColonPos, Field0),
    string.unsafe_between(Line, ColonPos + 1, count_code_units(Line), Value0),
    Field = string.strip(Field0),
    Value = string.strip(Value0).

:- func crlf = string.

crlf = "\r\n".

%-----------------------------------------------------------------------------%

search_header(Headers, SearchField, Value) :-
    list.find_first_match(
        (pred((F - _)::in) is semidet :- string_equal_ci(F, SearchField)),
        Headers, _ - Value).

%-----------------------------------------------------------------------------%

parse_header_value(Value, FirstWord, Params) :-
    Words = map(strip, split_at_char(';', Value)),
    (
        Words = [],
        unexpected($module, $pred, "empty list")
    ;
        Words = [FirstWord | Params]
    ).

%-----------------------------------------------------------------------------%

    % Parameter names are case-insensitive.
    %
search_parameter([Param | Params], SearchName, Value) :-
    (
        string.sub_string_search(Param, "=", EqualsPos),
        string.unsafe_between(Param, 0, EqualsPos, Name),
        % XXX linear whitespace is possible before the equals.
        string_equal_ci(Name, SearchName)
    ->
        string.unsafe_between(Param, EqualsPos + 1, count_code_units(Param),
            Value0),
        % Linear whitespace is possible after the equals.
        Value1 = lstrip(Value0),
        strip_quotes(Value1, Value)
    ;
        search_parameter(Params, SearchName, Value)
    ).

    % XXX parse properly handling escapes, e.g. \"
    %
:- pred strip_quotes(string::in, string::out) is det.

strip_quotes(S0, S) :-
    End = count_code_units(S0),
    (
        string.unsafe_index_next(S0, 0, Pos1, '"'),
        string.unsafe_prev_index(S0, End, Pos2, '"'),
        Pos1 =< Pos2
    ->
        string.unsafe_between(S0, Pos1, Pos2, S)
    ;
        S = S0
    ).

%-----------------------------------------------------------------------------%

search_content_disposition(Headers, ContentDisposition, Params) :-
    search_header(Headers, content_disposition_header, Value),
    parse_header_value(Value, ContentDisposition, Params).

:- func content_disposition_header = string.

content_disposition_header = "Content-Disposition".

%-----------------------------------------------------------------------------%

get_content_type(Headers, MediaType, Params) :-
    ( search_header(Headers, content_type_header, Value) ->
        parse_header_value(Value, MediaTypeString, Params),
        MediaType = media_type(MediaTypeString)
    ;
        MediaType = text_plain, % default
        Params = []
    ).

:- func content_type_header = string.

content_type_header = "Content-Type".

:- func text_plain = media_type.

text_plain = media_type("text/plain").

%-----------------------------------------------------------------------------%

media_type_equals(media_type(A), B) :-
    string_equal_ci(A, B).

:- pred string_equal_ci(string::in, string::in) is semidet.

string_equal_ci(A, B) :-
    string.to_lower(A, Lower),
    string.to_lower(B, Lower).

%-----------------------------------------------------------------------------%

search_multipart_formdata_boundary(Headers, Boundary) :-
    get_content_type(Headers, MediaType, Params),
    media_type_equals(MediaType, multipart_formdata),
    search_parameter(Params, boundary, Boundary).

:- func multipart_formdata = string.

multipart_formdata = "multipart/form-data".

:- func boundary = string.

boundary = "boundary".

%-----------------------------------------------------------------------------%

search_content_transfer_encoding(Headers, CTE) :-
    search_header(Headers, content_transfer_encoding, CTE).

:- func content_transfer_encoding = string.

content_transfer_encoding = "Content-Transfer-Encoding".

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
