:- module rfc2616.

% Copyright (C) 2014 YesLogic Pty. Ltd.
% All rights reserved.

:- interface.

:- import_module parsing_utils.

:- pred token(src::in, string::out, ps::in, ps::out) is semidet.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module char.
:- import_module int.

:- import_module rfc_parsing_utils.

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
% vim: ft=mercury ts=4 sts=4 sw=4 et
