:- module headers.

% Copyright (C) 2014 YesLogic Pty. Ltd.
% All rights reserved.

:- interface.

:- import_module assoc_list.
:- import_module list.
:- import_module map.

    % RFC 822-style headers.
    % Field names are case-insensitive.
    % Field values may be case-sensitive or not.
    % XXX how to handle duplicate fields?
    %
:- type headers.

    % RFC 822-style parameters.
    % Parameter names are case-insensitive.
    % Duplicate parameter names are not allowed.
    %
:- type parameters.

:- func init_headers = headers.

    % The field names must be in lowercase.
    %
:- func init_headers_from_assoc_list(assoc_list(string, string)) = headers.

:- pred add_header(string::in, string::in, headers::in, headers::out) is det.

    % Return the body of the first field with the given name.
    %
:- pred search_field(headers::in, string::in, string::out) is semidet.

    % Return the bodies of all fields which have the given name.
    %
:- pred search_field_multi(headers::in, string::in, list(string)::out) is det.

    % The parameter names must be in lowercase.
    %
:- func init_parameters_from_map(map(string, string)) = parameters.

:- pred search_parameter(parameters::in, string::in, string::out) is semidet.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module pair.
:- import_module string.

    % Header field names are case-insensitive and stored in lowercase.
    % (same as rfc2822.m)
    %
:- type headers == assoc_list(string, string).

    % Parameter names are case-insensitive and stored in lowercase.
    % (same as rfc2045.m)
    %
:- type parameters == map(string, string).
:- type parameter == pair(string, string).

%-----------------------------------------------------------------------------%

init_headers = [].

init_headers_from_assoc_list(Fields) = Fields.

add_header(Name, Body, Headers0, Headers) :-
    string.to_lower(Name, NameLower),
    % XXX this is reversed
    Headers = [NameLower - Body | Headers0].

search_field(Headers, SearchName, Body) :-
    string.to_lower(SearchName, SearchNameLower),
    assoc_list.search(Headers, SearchNameLower, Body).

search_field_multi(Headers, SearchName, Bodies) :-
    string.to_lower(SearchName, SearchNameLower),
    list.filter_map(match_field(SearchNameLower), Headers, Bodies).

:- pred match_field(string::in, pair(string)::in, string::out) is semidet.

match_field(SearchNameLower, SearchNameLower - Body, Body).

%-----------------------------------------------------------------------------%

init_parameters_from_map(Map) = Map.

search_parameter(Params, SearchName, Value) :-
    string.to_lower(SearchName, SearchNameLower),
    map.search(Params, SearchNameLower, Value).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
