:- module rfc2046.

% Copyright (C) 2014 YesLogic Pty. Ltd.
% All rights reserved.

:- interface.

:- pred is_valid_boundary(string::in) is semidet.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module char.
:- import_module int.
:- import_module string.

%-----------------------------------------------------------------------------%

% 5.1.1. Common Syntax

is_valid_boundary(String) :-
    all_bchars(String, 0, Length),
    1 =< Length, Length =< 70,
    % Last char cannot not be space.
    string.unsafe_prev_index(String, Length, _LastPos, LastChar),
    LastChar \= (' ').

:- pred all_bchars(string::in, int::in, int::out) is semidet.

all_bchars(String, !Pos) :-
    ( string.unsafe_index_next(String, !Pos, Char) ->
        bchars(Char),
        all_bchars(String, !Pos)
    ;
        true % end of string
    ).

:- pred bchars(char::in) is semidet.

bchars(C) :-
    ( char.is_alnum(C)
    ; C = ('\'')
    ; C = ('(')
    ; C = (')')
    ; C = ('+')
    ; C = ('_')
    ; C = (',')
    ; C = ('-')
    ; C = ('.')
    ; C = ('/')
    ; C = (':')
    ; C = ('=')
    ; C = ('?')
    ; C = (' ')
    ).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
