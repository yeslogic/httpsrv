:- module percent_decoding.

% Copyright (C) 2014 YesLogic Pty. Ltd.
% All rights reserved.

:- interface.

    % This does NOT decode + to space.
    %
    % "+" is used in application/x-www-form-urldecoded,
    % which is included in the query component of a URI.
    %
:- pred percent_decode(string::in, string::out) is semidet.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module char.
:- import_module int.
:- import_module list.
:- import_module string.

%-----------------------------------------------------------------------------%

percent_decode(S0, S) :-
    ( requires_decoding(S0) ->
        percent_decode_2(S0, 0, _EndPos, [], RevOctets),
        list.reverse(RevOctets, Octets),
        string.from_code_unit_list(Octets, S)
    ;
        S = S0
    ).

:- pred percent_decode_2(string::in, int::in, int::out,
    list(int)::in, list(int)::out) is semidet.  

percent_decode_2(S, !Pos, !RevOctets) :-
    ( string.unsafe_index_next(S, !Pos, Char) ->
        ( Char = ('%') ->
            string.unsafe_index_next(S, !Pos, Hi),
            string.unsafe_index_next(S, !Pos, Lo),
            char.is_hex_digit(Hi, Int_Hi),
            char.is_hex_digit(Lo, Int_Lo),
            Octet = (Int_Hi << 4) \/ Int_Lo
        ;
            char.to_int(Char, Octet),
            Octet =< 0x7f
        ),
        list.cons(Octet, !RevOctets),
        percent_decode_2(S, !Pos, !RevOctets)
    ;
        true
    ).

:- pred requires_decoding(string::in) is semidet.

requires_decoding(S) :-
    string.sub_string_search(S, "%", _).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
