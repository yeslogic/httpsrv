:- module headers.

% Copyright (C) 2014 YesLogic Pty. Ltd.
% All rights reserved.

:- interface.

:- import_module assoc_list.
:- import_module list.
:- import_module map.

:- import_module case_insensitive.

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

:- func init_headers_from_assoc_list(assoc_list(case_insensitive, string))
    = headers.

:- pred add_header(case_insensitive::in, string::in, headers::in, headers::out)
    is det.

    % Return the body of the first field with the given name.
    %
:- pred search_field(headers::in, case_insensitive::in, string::out)
    is semidet.

    % Return the bodies of all fields which have the given name.
    %
:- pred search_field_multi(headers::in, case_insensitive::in,
    list(string)::out) is det.

:- func init_parameters_from_map(map(case_insensitive, string)) = parameters.

:- pred search_parameter(parameters::in, case_insensitive::in, string::out)
    is semidet.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module pair.

    % (same as rfc2822.m)
    %
:- type headers == assoc_list(case_insensitive, string).

    % (same as rfc2045.m)
    %
:- type parameters == map(case_insensitive, string).

%-----------------------------------------------------------------------------%

init_headers = [].

init_headers_from_assoc_list(Fields) = Fields.

add_header(Name, Body, Headers0, Headers) :-
    % XXX this is reversed
    Headers = [Name - Body | Headers0].

search_field(Headers, SearchName, Body) :-
    assoc_list.search(Headers, SearchName, Body).

search_field_multi(Headers, SearchName, Bodies) :-
    list.filter_map(match_field(SearchName), Headers, Bodies).

:- pred match_field(case_insensitive::in, pair(case_insensitive, string)::in,
    string::out) is semidet.

match_field(SearchName, SearchName - Body, Body).

%-----------------------------------------------------------------------------%

init_parameters_from_map(Map) = Map.

search_parameter(Params, SearchName, Value) :-
    map.search(Params, SearchName, Value).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
