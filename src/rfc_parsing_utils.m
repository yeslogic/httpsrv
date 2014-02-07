:- module rfc_parsing_utils.

% Copyright (C) 2014 YesLogic Pty. Ltd.
% All rights reserved.

:- interface.

:- import_module char.
:- import_module list.
:- import_module parsing_utils.

%-----------------------------------------------------------------------------%

:- pred call_skip_pred(src::in, ps::in, ps::out) is semidet.

:- pred run_of(pred(src, T, ps, ps)::in(pred(in, out, in, out) is semidet),
    src::in, list(T)::out, ps::in, ps::out) is det.

:- pred run_of_chars(pred(char)::in(pred(in) is semidet),
    src::in, list(char)::out, ps::in, ps::out) is det.

:- pred zero_or_more_chars_to_string(pred(char)::in(pred(in) is semidet),
    src::in, string::out, ps::in, ps::out) is det.

:- pred one_or_more_chars_to_string(pred(char)::in(pred(in) is semidet),
    src::in, string::out, ps::in, ps::out) is semidet.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module string.

call_skip_pred(Src, !PS) :-
    get_skip_whitespace_pred(Src, Skip),
    Skip(Src, _, !PS).

run_of(P, Src, Xs, !PS) :-
    ( P(Src, X, !PS) ->
        run_of(P, Src, Xs1, !PS),
        Xs = [X | Xs1]
    ;
        Xs = []
    ).

run_of_chars(P, Src, Chars, !PS) :-
    (
        next_char_no_progress(Src, Char, !PS),
        P(Char)
    ->
        run_of_chars(P, Src, Chars1, !PS),
        Chars = [Char | Chars1]
    ;
        Chars = []
    ).

zero_or_more_chars_to_string(P, Src, String, !PS) :-
    run_of_chars(P, Src, Chars, !PS),
    string.from_char_list(Chars, String).

one_or_more_chars_to_string(P, Src, String, !PS) :-
    run_of_chars(P, Src, Chars, !PS),
    Chars = [_ | _],
    string.from_char_list(Chars, String).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
