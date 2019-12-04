:- module rfc2183.

% Copyright (C) 2014 YesLogic Pty. Ltd.
% All rights reserved.

:- interface.

:- import_module pair.
:- import_module parsing_utils.

:- import_module case_insensitive.
:- use_module rfc2045.

:- pred content_disposition_body(src::in,
    pair(case_insensitive, rfc2045.parameters)::out, ps::in, ps::out)
    is semidet.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module char.
:- import_module int.
:- import_module io.
:- import_module list.
:- import_module map.
:- import_module string.
:- import_module unit.

:- import_module rfc_parsing_utils.

%-----------------------------------------------------------------------------%

content_disposition_body(Src, DispositionType - Params, !PS) :-
    trace [io(!IO), compile_time(flag("content_disposition_body"))] (
        input_string(Src, Input, _InputLength),
        io.write_string("content_disposition_body: «", !IO),
        io.write_string(Input, !IO),
        io.write_string("»\n", !IO)
    ),

    disposition_type(Src, DispositionType, !PS),

    % The parameter list of Content-Disposition fields is broken in practice.
    %
    % Firefox does NOT escape backslash.
    % Firefox escapes dquote.
    %
    % Chrome does NOT escape backslash.
    % Chrome replaces dquote with %22.
    %
    % IE8 does NOT escape backslash.
    % IE8 does NOT escape dquote.
    % However we will not try to compensate for IE because Windows does not
    % allow backslash and dquote in filenames so they should only occur if you
    % deliberately use those characters in an HTML form control name.

    (
        semidet_fail,
        rfc2045.zero_or_more_parameters_list(Src, ParamsPrime, !PS),
        eof(Src, _, !PS)
    ->
        Params = ParamsPrime
    ;
        promise_equivalent_solutions [Params, !:PS] (
            broken_parameters_list(Src, map.init, Params, !PS),
            eof(Src, _, !PS)
        )
    ).

:- pred disposition_type(src::in, case_insensitive::out, ps::in, ps::out)
    is semidet.

disposition_type(Src, DispositionType, !PS) :-
    rfc2045.token(Src, Token, !PS),
    DispositionType = from_string(Token).

%-----------------------------------------------------------------------------%

:- pred broken_parameters_list(src::in,
    rfc2045.parameters::in, rfc2045.parameters::out, ps::in, ps::out)
    is nondet.

broken_parameters_list(Src, !Params, !PS) :-
    ( semicolon_then_broken_parameter(Src, unit, !Params, !PS) ->
        broken_parameters_list(Src, !Params, !PS)
    ;
        true
    ).

:- pred semicolon_then_broken_parameter(src::in, unit::out,
    rfc2045.parameters::in, rfc2045.parameters::out, ps::in, ps::out)
    is nondet.

semicolon_then_broken_parameter(Src, unit, !Params, !PS) :-
    punct(";", Src, _, !PS),
    rfc2045.parameter_attribute(Src, Attrib, !PS),
    punct("=", Src, _, !PS),
    broken_parameter_value(Src, Value, !PS),
    % Do not allow duplicate parameter names.
    map.insert(Attrib, Value, !Params).

:- pred broken_parameter_value(src::in, string::out, ps::in, ps::out)
    is nondet.

broken_parameter_value(Src, Value, !PS) :-
    ( rfc2045.token(Src, Token, !PS) ->
        Value = Token
    ;
        broken_quoted_string(Src, Value, !PS)
    ).

:- pred broken_quoted_string(src::in, string::out, ps::in, ps::out)
    is nondet.

broken_quoted_string(Src, String, !PS) :-
    'DQUOTE'(Src, !PS),
    amb_run_of(broken_qcontent, Src, CharList, !PS),
    'DQUOTE'(Src, !PS),
    string.from_char_list(CharList, String),
    call_skip_pred(Src, !PS). % [CFWS] in RFC 2822

:- pred amb_run_of(pred(src, T, ps, ps), src, list(T), ps, ps).
:- mode amb_run_of(in(pred(in, out, in, out) is nondet), in, out, in, out)
    is multi.

amb_run_of(P, Src, Xs, !PS) :-
    (
        P(Src, X, !PS),
        amb_run_of(P, Src, Xs1, !PS),
        Xs = [X | Xs1]
    ;
        Xs = []
    ).

:- pred broken_qcontent(src::in, char::out, ps::in, ps::out) is nondet.

broken_qcontent(Src, Char, !PS) :-
    next_char_no_progress(Src, Char0, !PS),
    ( Char0 = ('"') ->
        % For IE, could be literal-dquote though.
        fail
    ; Char0 = ('\\') ->
        (
            % For Firefox, \" can be escaped-dquote but can also be
            % literal-backslash quoted-string-terminator.
            next_char_no_progress(Src, ('"'), !PS),
            Char = ('"')
        ;
            Char = Char0
        )
    ;
        % We assume that CRLFs for folding have been removed already.
        Char0 \= ('\r'),
        Char0 \= ('\n'),
        % This includes NO-WS-CTL / WSP / rest of US-ASCII.
        % As an extension, we allow any Unicode code points.
        Char = Char0,
        char.to_int(Char, Int),
        Int > 0,
        % Int =< 127
        true
    ).

:- pred 'DQUOTE'(src::in, ps::in, ps::out) is semidet.

'DQUOTE'(Src, !PS) :-
    next_char_no_progress(Src, '"', !PS).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
