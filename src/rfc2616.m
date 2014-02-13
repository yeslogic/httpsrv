:- module rfc2616.

% Copyright (C) 2014 YesLogic Pty. Ltd.
% All rights reserved.

:- interface.

:- import_module maybe.
:- import_module parsing_utils.

:- pred token(src::in, string::out, ps::in, ps::out) is semidet.

:- pred parse_host_header_value(string::in, string::out, maybe(string)::out)
    is semidet.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module char.
:- import_module int.
:- import_module pair.
:- import_module string.
:- import_module unit.

:- import_module rfc_parsing_utils.

%-----------------------------------------------------------------------------%

token(Src, Token, !PS) :-
    one_or_more_chars_to_string(token_char, Src, Token, !PS),
    call_skip_pred(Src, !PS).

:- pred token_char(char::in) is semidet.

token_char(Char) :-
    char.to_int(Char, Int),
    % Any CHAR except CTLs or separators.
    0x20 =< Int, Int < 0x7f,
    not separator(Char).

:- pred separator(char::in) is semidet.

separator('('). separator(')'). separator('<'). separator('>'). separator('@').
separator(','). separator(';'). separator(':'). separator('\\'). separator('"').
separator('/'). separator('['). separator(']'). separator('?'). separator('=').
separator('{'). separator('}'). separator(' '). separator('\t').

%-----------------------------------------------------------------------------%

:- pred skip_LWS_chars(src::in, unit::out, ps::in, ps::out) is semidet.

skip_LWS_chars(Src, unit, !PS) :-
    (
        next_char_no_progress(Src, Char, !PS),
        'LWS'(Char)
    ->
        skip_LWS_chars(Src, _, !PS)
    ;
        true
    ).

:- pred 'LWS'(char::in) is semidet.

'LWS'(' ').
'LWS'('\t').

%-----------------------------------------------------------------------------%

% 14.23 Host

parse_host_header_value(Input, Host, MaybePort) :-
    promise_equivalent_solutions [ParseResult] (
        parsing_utils.parse(Input, skip_LWS_chars, host_and_optional_port,
            ParseResult)
    ),
    ParseResult = ok(Host - MaybePort).

:- pred host_and_optional_port(src::in, pair(string, maybe(string))::out,
    ps::in, ps::out) is semidet.

host_and_optional_port(Src, Host - MaybePort, !PS) :-
    host(Src, Host, !PS),
    optional(colon_port, Src, MaybePort, !PS),
    eof(Src, _, !PS).

:- pred host(src::in, string::out, ps::in, ps::out) is semidet.

host(Src, Host, !PS) :-
    % [RFC 2732] Format for Literal IPv6 Addresses in URL's
    ( next_char_no_progress(Src, ('['), !PS) ->
        one_or_more_chars_to_string(ipv6_host_char, Src, Host, !PS),
        next_char_no_progress(Src, ']', !PS)
    ;
        one_or_more_chars_to_string(host_char, Src, Host, !PS)
    ),
    call_skip_pred(Src, !PS).

:- pred host_char(char::in) is semidet.

host_char(C) :-
    ( char.is_alnum(C)
    ; C = ('.')
    ; C = ('-')
    ).

:- pred ipv6_host_char(char::in) is semidet.

ipv6_host_char(C) :-
    ( char.is_hex_digit(C)
    ; C = (':')
    ).

:- pred colon_port(src::in, string::out, ps::in, ps::out) is semidet.

colon_port(Src, Port, !PS) :-
    punct(":", Src, _, !PS),
    port(Src, Port, !PS).

:- pred port(src::in, string::out, ps::in, ps::out) is semidet.

port(Src, Port, !PS) :-
    one_or_more_chars_to_string(char.is_digit, Src, Port0, !PS),
    % Canonicalise port.
    port_to_int(Port0, Int),
    Port = string.from_int(Int).

:- pred port_to_int(string::in, int::out) is semidet.

port_to_int(String, Int) :-
    string.to_int(String, Int),
    Int > 0.

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
