:- module rfc_parsing_utils.

% Copyright (C) 2014 YesLogic Pty. Ltd.
% All rights reserved.

:- interface.

:- import_module char.
:- import_module parsing_utils.

%-----------------------------------------------------------------------------%

:- pred call_skip_pred(src::in, ps::in, ps::out) is semidet.

:- pred zero_or_more_chars_to_string(pred(char)::in(pred(in) is semidet),
    src::in, string::out, ps::in, ps::out) is det.

:- pred one_or_more_chars_to_string(pred(char)::in(pred(in) is semidet),
    src::in, string::out, ps::in, ps::out) is semidet.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module require.

call_skip_pred(Src, !PS) :-
    get_skip_whitespace_pred(Src, Skip),
    Skip(Src, _, !PS).

zero_or_more_chars_to_string(P, Src, String, !PS) :-
    current_offset(Src, Start, !PS),
    while(P, Src, !PS),
    current_offset(Src, End, !PS),
    ( input_substring(Src, Start, End, StringPrime) ->
        String = StringPrime
    ;
        unexpected($module, $pred, "input_substring failed")
    ).

one_or_more_chars_to_string(P, Src, String, !PS) :-
    current_offset(Src, Start, !PS),
    while(P, Src, !PS),
    current_offset(Src, End, !PS),
    End > Start,
    ( input_substring(Src, Start, End, StringPrime) ->
        String = StringPrime
    ;
        unexpected($module, $pred, "input_substring failed")
    ).

:- pred while(pred(char)::in(pred(in) is semidet), src::in, ps::in, ps::out)
    is det.

while(P, Src, !PS) :-
    (
        next_char_no_progress(Src, Char, !PS),
        P(Char)
    ->
        while(P, Src, !PS)
    ;
        true
    ).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
