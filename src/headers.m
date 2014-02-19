:- module headers.

% Copyright (C) 2014 YesLogic Pty. Ltd.
% All rights reserved.

:- interface.

:- import_module assoc_list.
:- import_module list.
:- import_module map.

:- import_module case_insensitive.

    % RFC 822-style headers (with provision for duplicate headers).
    % Field names are case-insensitive.
    % Field values may be case-sensitive or not.
    %
:- type headers.

    % RFC 822-style parameters.
    % Parameter names are case-insensitive.
    % Duplicate parameter names are not allowed.
    %
:- type parameters.

:- func init_headers = headers.

:- type duplicate_header
    --->    reject_duplicate_header
    ;       append_duplicate_header.

:- pred add_header(duplicate_header::in, case_insensitive::in, string::in,
    headers::in, headers::out) is semidet.

:- pred from_assoc_list(duplicate_header::in,
    assoc_list(case_insensitive, string)::in, headers::out) is semidet.

    % Return the bodies of all fields which have the given name.
    %
:- pred search_field(headers::in, case_insensitive::in, list(string)::out)
    is semidet.

:- func init_parameters_from_map(map(case_insensitive, string)) = parameters.

:- pred search_parameter(parameters::in, case_insensitive::in, string::out)
    is semidet.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module cord.
:- import_module maybe.
:- import_module pair.

    % Map from header field names to a cord of header field values,
    % to account for duplicate header fields.
    %
:- type headers == map(case_insensitive, cord(string)).

    % (same as rfc2045.m)
    %
:- type parameters == map(case_insensitive, string).

%-----------------------------------------------------------------------------%

init_headers = map.init.

add_header(Dup, Name, Body, !Headers) :-
    map.search_insert(Name, singleton(Body), MaybeExisting, !Headers),
    (
        MaybeExisting = yes(OldBodies),
        (
            Dup = append_duplicate_header,
            map.det_update(Name, snoc(OldBodies, Body), !Headers)
        ;
            Dup = reject_duplicate_header,
            fail
        )
    ;
        MaybeExisting = no
    ).

:- pred add_header(duplicate_header::in, pair(case_insensitive, string)::in,
    headers::in, headers::out) is semidet.

add_header(Dup, Name - Body, !Headers) :-
    add_header(Dup, Name, Body, !Headers).

from_assoc_list(Dup, Fields, Map) :-
    list.foldl(add_header(Dup), Fields, init_headers, Map).

search_field(Headers, SearchName, list(Bodies)) :-
    map.search(Headers, SearchName, Bodies).

%-----------------------------------------------------------------------------%

init_parameters_from_map(Map) = Map.

search_parameter(Params, SearchName, Value) :-
    map.search(Params, SearchName, Value).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
