:- module mime_headers.

% Copyright (C) 2014 YesLogic Pty. Ltd.
% All rights reserved.

:- interface.

:- type headers.

:- type parameters.

:- type media_type
    --->    media_type(string). % case-insensitive, stored in lowercase

:- type maybe_multipart
    --->    not_multipart
    ;       multipart(media_type, string) % second argument is boundary
    ;       error(string).

:- func init_headers = headers.

:- pred add_header(string::in, string::in, headers::in, headers::out) is det.

:- pred parse_headers(string::in, headers::out) is semidet.

:- pred search_field(headers::in, string::in, string::out) is semidet.

:- pred search_parameter(parameters::in, string::in, string::out) is semidet.

:- pred get_content_type(headers::in, media_type::out, parameters::out) is det.

:- pred media_type_equals(media_type::in, string::in) is semidet.

:- pred is_multipart_content_type(headers::in, maybe_multipart::out) is det.

:- pred search_content_transfer_encoding(headers::in, string::out)
    is semidet.

:- pred search_content_disposition(headers::in, string::out, parameters::out)
    is semidet.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module assoc_list.
:- import_module list.
:- import_module map.
:- import_module pair.
:- import_module string.

:- import_module rfc2045.
:- import_module rfc2046.
:- import_module rfc2183.
:- import_module rfc2822.

/*
** [RFC 822] Standard for the Format of APRA Internet Text Messages
** [RFC 2822] Internet Message Format
** [RFC 5322] Internet Message Format
**
** [RFC 2045] MIME Part One: Format of Internet Message Bodies
** [RFC 2046] MIME Part Two: Media Types
** [RFC 2183] The Content-Disposition Header Field
** [RFC 2388] Returning Values from Forms: multipart/form-data
**
** NOT [RFC 2616] This relates to HTTP and not the payload.
** NOT [RFC 6266] This relates to HTTP and not the payload.
*/

    % XXX how to handle duplicate fields?
:- type headers == list(rfc2822.field).

:- type parameters == rfc2045.parameters.

%-----------------------------------------------------------------------------%

init_headers = [].

add_header(Name, Body, Headers0, Headers) :-
    string.to_lower(Name, NameLower),
    Headers = [NameLower - Body | Headers0].

parse_headers(Input, Headers) :-
    rfc2822.parse_fields(Input, Headers).

search_field(Headers, SearchName, Body) :-
    string.to_lower(SearchName, SearchNameLower),
    assoc_list.search(Headers, SearchNameLower, Body).

search_parameter(Params, SearchName, Value) :-
    string.to_lower(SearchName, SearchNameLower),
    map.search(Params, SearchNameLower, Value).

%-----------------------------------------------------------------------------%

get_content_type(Headers, media_type(MediaType), Params) :-
    (
        search_field(Headers, content_type, Body),
        parse_structured_field_body(Body, content_type_body, Output)
    ->
        Output = MediaType - Params
    ;
        content_type_defaults(MediaType, Params)
    ).

:- func content_type = string.

content_type = "Content-Type".

media_type_equals(media_type(A), B) :-
    A = string.to_lower(B).

%-----------------------------------------------------------------------------%

is_multipart_content_type(Headers, Res) :-
    (
        get_content_type(Headers, MediaType, Params),
        is_multipart_type(MediaType)
    ->
        (
            search_parameter(Params, boundary, BoundaryPrime),
            is_valid_boundary(BoundaryPrime)
        ->
            Boundary = BoundaryPrime,
            ( search_content_transfer_encoding(Headers, CTE) ->
                ( is_valid_content_transfer_encoding_for_multipart(CTE) ->
                    Res = multipart(MediaType, Boundary)
                ;
                    Res = error("bad content-transfer-encoding for multipart")
                )
            ;
                % Missing Content-Transfer-Encoding is okay.
                % XXX what about malformed?
                Res = multipart(MediaType, Boundary)
            )
        ;
            Res = error("missing multipart boundary, or syntax error")
        )
    ;
        Res = not_multipart
    ).

:- func boundary = string.

boundary = "boundary".

:- pred is_multipart_type(media_type::in) is semidet.

is_multipart_type(media_type(MediaType)) :-
    string.prefix(MediaType, "multipart/").

%-----------------------------------------------------------------------------%

search_content_transfer_encoding(Headers, Mechanism) :-
    search_field(Headers, content_transfer_encoding, Body),
    parse_structured_field_body(Body, content_transfer_encoding_body,
        Mechanism).

:- func content_transfer_encoding = string.

content_transfer_encoding = "Content-Transfer-Encoding".

%-----------------------------------------------------------------------------%

search_content_disposition(Headers, DispositionType, Params) :-
    search_field(Headers, content_disposition_header, Body),
    parse_structured_field_body(Body, content_disposition_body,
        DispositionType - Params).

:- func content_disposition_header = string.

content_disposition_header = "Content-Disposition".

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
