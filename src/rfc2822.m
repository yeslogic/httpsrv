:- module rfc2822.

% Copyright (C) 2014 YesLogic Pty. Ltd.
% All rights reserved.

:- interface.

:- import_module list.
:- import_module pair.
:- import_module parsing_utils.

:- import_module case_insensitive.

:- type field == pair(case_insensitive, string).

:- pred parse_fields(string::in, list(field)::out) is semidet.

:- pred parse_structured_field_body(string::in, parser(T)::in(parser), T::out)
    is semidet.

:- pred quoted_string(src::in, string::out, ps::in, ps::out) is semidet.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module char.
:- import_module int.
:- import_module string.
:- import_module unit.

:- import_module rfc_parsing_utils.

%-----------------------------------------------------------------------------%

% 2.2. Header Fields

parse_fields(Input, Fields) :-
    promise_equivalent_solutions [ParseResult] (
        parsing_utils.parse(Input, no_skip_whitespace, zero_or_more(field),
            ParseResult)
    ),
    ParseResult = ok(Fields).

:- pred no_skip_whitespace(src::in, unit::out, ps::in, ps::out) is semidet.

no_skip_whitespace(_Src, unit, !PS) :-
    semidet_true.

:- pred field(src::in, pair(case_insensitive, string)::out, ps::in, ps::out)
    is semidet.

field(Src, FieldName - FieldBody, !PS) :-
    field_name(Src, FieldName, !PS),
    skip_WSP_chars(Src, !PS), % obsolete syntax
    punct(":", Src, _, !PS),
    skip_WSP_chars(Src, !PS),
    field_body(Src, FieldBody, !PS),
    crlf(Src, !PS).

:- pred field_name(src::in, case_insensitive::out, ps::in, ps::out) is semidet.

field_name(Src, FieldName, !PS) :-
    one_or_more_chars_to_string(field_name_char, Src, Word, !PS),
    FieldName = from_string(Word).

:- pred field_name_char(char::in) is semidet.

field_name_char(C) :-
    char.to_int(C, I),
    33 =< I, I =< 126,
    C \= (':').

:- pred field_body(src::in, string::out, ps::in, ps::out) is det.

field_body(Src, FieldBody, !PS) :-
    run_of(field_body_char, Src, Chars, !PS),
    string.from_char_list(Chars, FieldBody).

:- pred field_body_char(src::in, char::out, ps::in, ps::out) is semidet.

field_body_char(Src, Char, !PS) :-
    next_char_no_progress(Src, C, !PS),
    ( C = ('\r') ->
        % Unfolding: remove CRLF before WSP.
        next_char_no_progress(Src, ('\n'), !PS),
        next_char_no_progress(Src, Char, !PS),
        'WSP'(Char)
    ;
        char.to_int(C, I),
        % We do NOT allow NUL.
        I > 0,
        % As an extension, we allow any Unicode code points.
        % I =< 127,
        Char = C
    ).

:- pred skip_WSP_chars(src::in, ps::in, ps::out) is semidet.

skip_WSP_chars(Src, !PS) :-
    (
        next_char_no_progress(Src, Char, !PS),
        'WSP'(Char)
    ->
        skip_WSP_chars(Src, !PS)
    ;
        semidet_true
    ).

:- pred 'WSP'(char::in) is semidet.

'WSP'(' ').
'WSP'('\t').

:- pred crlf(src::in, ps::in, ps::out) is semidet.

crlf(Src, !PS) :-
    next_char_no_progress(Src, '\r', !PS),
    next_char_no_progress(Src, '\n', !PS).

%-----------------------------------------------------------------------------%

parse_structured_field_body(Input, Parser, Output) :-
    promise_equivalent_solutions [ParseResult] (
        parsing_utils.parse(Input, skip_CFWS, Parser, ParseResult)
    ),
    ParseResult = ok(Output).

%-----------------------------------------------------------------------------%

% 3.2.1. Primitive Tokens

:- pred 'NO-WS-CTL'(char::in) is semidet.

'NO-WS-CTL'(C) :-
    char.to_int(C, I),
    ( 1 =< I, I =< 8
    ; I = 11
    ; I = 12
    ; 14 =< I, I =< 31
    ; I = 127
    ).

:- pred text(char::in) is semidet.

text(C) :-
    char.to_int(C, I),
    ( 1 =< I, I =< 9
    ; I = 11
    ; I = 12
    ; 14 =< I,I =< 127
    % ; obs-text adds NUL and bare CR and bare LF
    ).

:- pred text(src::in, char::out, ps::in, ps::out) is semidet.

text(Src, Char, !PS) :-
    next_char_no_progress(Src, Char, !PS),
    text(Char).

%-----------------------------------------------------------------------------%

% 3.2.3. Folding white space and comments

:- pred skip_FWS(src::in, ps::in, ps::out) is semidet.

skip_FWS(Src, !PS) :-
    % We assume that CRLFs for folding have been removed already.
    skip_WSP_chars(Src, !PS).

:- pred skip_CFWS(src::in, unit::out, ps::in, ps::out) is semidet.

skip_CFWS(Src, unit, !PS) :-
    % We assume that CRLFs for folding have been removed already.
    skip_FWS(Src, !PS),
    ( comment(Src, _, !PS) ->
        skip_CFWS(Src, _, !PS)
    ;
        true
    ).

    % This implementation could be a lot simpler.
    %
:- pred comment(src::in, unit::out, ps::in, ps::out) is semidet.

comment(Src, Unit, !PS) :-
    next_char_no_progress(Src, '(', !PS),
    comment_tail(Src, Unit, !PS).

:- pred comment_tail(src::in, unit::out, ps::in, ps::out) is semidet.

comment_tail(Src, unit, !PS) :-
    comment_loop(Src, _, !PS),
    skip_FWS(Src, !PS),
    next_char_no_progress(Src, ')', !PS).

:- pred comment_loop(src::in, unit::out, ps::in, ps::out) is semidet.

comment_loop(Src, unit, !PS) :-
    skip_FWS(Src, !PS),
    ( ccontent(Src, _, !PS) ->
        comment_loop(Src, _, !PS)
    ;
        true
    ).

:- pred ccontent(src::in, unit::out, ps::in, ps::out) is semidet.

ccontent(Src, unit, !PS) :-
    next_char_no_progress(Src, C, !PS),
    ( C = (')') ->
        fail
    ; C = ('(') ->
        comment_tail(Src, _, !PS) % nested comment
    ; C = ('\\') ->
        quoted_pair_tail(Src, _, !PS)
    ;
        'NO-WS-CTL'(C)
    ;
        char.to_int(C, I),
        33 =< I, I =< 126
    ).

%-----------------------------------------------------------------------------%

% 3.2.5. Quoted strings

:- pred 'DQUOTE'(src::in, ps::in, ps::out) is semidet.

'DQUOTE'(Src, !PS) :-
    next_char_no_progress(Src, '"', !PS).

quoted_string(Src, String, !PS) :-
    % Not sure why RFC 2822 has explicit optional CFWS here.
    % call_skip_pred(Src, _, !PS),
    'DQUOTE'(Src, !PS),
    run_of(qcontent, Src, CharList, !PS),
    'DQUOTE'(Src, !PS),
    string.from_char_list(CharList, String),
    call_skip_pred(Src, !PS). % [CFWS] in RFC 2822

:- pred qcontent(src::in, char::out, ps::in, ps::out) is semidet.

qcontent(Src, Char, !PS) :-
    next_char_no_progress(Src, Char0, !PS),
    ( Char0 = ('"') ->
        fail
    ; Char0 = ('\\') ->
        quoted_pair_tail(Src, Char, !PS)
    ; Char0 = ('\r') ->
        % We assume that CRLFs for folding have been removed already.
        fail
    ; Char0 = ('\n') ->
        % We assume that CRLFs for folding have been removed already.
        fail
    ;
        % This includes NO-WS-CTL / WSP / rest of US-ASCII.
        % As an extension, we allow any Unicode code points.
        Char = Char0,
        char.to_int(Char, Int),
        Int > 0,
        % Int =< 127
        true
    ).

:- pred quoted_pair_tail(src::in, char::out, ps::in, ps::out) is semidet.

quoted_pair_tail(Src, Char, !PS) :-
    % quoted-pair (but not obs-qp)
    text(Src, Char, !PS).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
