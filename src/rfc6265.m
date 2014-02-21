:- module rfc6265.

% Copyright (C) 2014 YesLogic Pty. Ltd.
% All rights reserved.

:- interface.

:- import_module list.
:- import_module pair.

    % Cookie names are case-sensitive.
    %
:- type cookie == pair(string, string).

    % Parse the Cookie: header field value.
    %
:- pred parse_cookie_header_value(string::in, list(cookie)::out) is semidet.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module char.
:- import_module int.
:- import_module parsing_utils.
:- import_module unit.

:- import_module rfc_parsing_utils.
:- use_module rfc2616.

%-----------------------------------------------------------------------------%

parse_cookie_header_value(Input, Cookies) :-
    promise_equivalent_solutions [ParseResult] (
        parsing_utils.parse(Input, skip_OWS, cookie_header_value,
            ParseResult)
    ),
    ParseResult = ok(Cookies).

:- pred skip_OWS(src::in, unit::out, ps::in, ps::out) is semidet.

skip_OWS(Src, unit, !PS) :-
    ( 'OWS'(Src, !PS) ->
        skip_OWS(Src, _, !PS)
    ;
        true
    ).

:- pred 'OWS'(src::in, ps::in, ps::out) is semidet.

'OWS'(Src, !PS) :-
    % CRLF folds should already have been removed by the HTTP header parser.
    next_char_no_progress(Src, Char, !PS),
    (
        Char = (' ')  % WSP
    ;
        Char = ('\t') % WSP
    ).

:- pred cookie_header_value(src::in, list(cookie)::out, ps::in, ps::out)
    is semidet.

cookie_header_value(Src, Cookies, !PS) :-
    cookie_string(Src, Cookies, !PS),
    eof(Src, _, !PS).

:- pred cookie_string(src::in, list(cookie)::out, ps::in, ps::out) is semidet.

cookie_string(Src, [Cookie | Cookies], !PS) :-
    cookie_pair(Src, Cookie, !PS),
    zero_or_more(delim_cookie_pair, Src, Cookies, !PS).

:- pred cookie_pair(src::in, cookie::out, ps::in, ps::out) is semidet.

cookie_pair(Src, Name - Value, !PS) :-
    cookie_name(Src, Name, !PS),
    punct("=", Src, _, !PS),
    cookie_value(Src, Value, !PS).

:- pred delim_cookie_pair(src::in, cookie::out, ps::in, ps::out) is semidet.

delim_cookie_pair(Src, Cookie, !PS) :-
    punct(";", Src, _, !PS),
    cookie_pair(Src, Cookie, !PS).

:- pred cookie_name(src::in, string::out, ps::in, ps::out) is semidet.

cookie_name(Src, Name, !PS) :-
    rfc2616.token(Src, Name, !PS).

:- pred cookie_value(src::in, string::out, ps::in, ps::out) is semidet.

cookie_value(Src, Value, !PS) :-
    % Section 5.4 says: "Despite its name, the cookie-string is actually a
    % sequence of octets, not a sequence of characters."
    %
    % However, if we had generated the cookie following the grammar in
    % section 4.1.1 then the cookie value would be limited to a subset of
    % US-ASCII chars.
    ( one_or_more_chars_to_string(cookie_octet, Src, String, !PS) ->
        Value = String
    ;
        next_char(Src, '"', !PS),
        zero_or_more_chars_to_string(cookie_octet, Src, Value, !PS),
        next_char(Src, '"', !PS)
    ),
    call_skip_pred(Src, !PS).

:- pred cookie_octet(char::in) is semidet.

cookie_octet(C) :-
    char.to_int(C, I),
    0x20 < I, I < 0x7f, % US-ASCII excluding CTLs, whitespace, DEL
    C \= ('"'),
    C \= (','),
    C \= (';'),
    C \= ('\\').

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
