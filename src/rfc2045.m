:- module rfc2045.

% Copyright (C) 2014 YesLogic Pty. Ltd.
% All rights reserved.

:- interface.

:- import_module map.
:- import_module pair.
:- import_module parsing_utils.

    % Parameter names are case-insensitive and stored in lowercase.
    %
:- type parameters == map(string, string).
:- type parameter == pair(string, string).

:- pred content_type_body(src::in, pair(string, parameters)::out,
    ps::in, ps::out) is semidet.

:- pred zero_or_more_parameters_list(src::in, parameters::out, ps::in, ps::out)
    is semidet.

:- pred token(src::in, string::out, ps::in, ps::out) is semidet.

:- pred content_type_defaults(string::out, parameters::out) is det.

:- pred content_transfer_encoding_body(src::in, string::out,
    ps::in, ps::out) is semidet.

:- pred is_valid_content_transfer_encoding_for_multipart(string::in)
    is semidet.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module char.
:- import_module int.
:- import_module string.
:- import_module unit.

:- import_module rfc2822.
:- import_module rfc_parsing_utils.

%-----------------------------------------------------------------------------%

% 5.1. Syntax of the Content-Type Header Field

content_type_body(Src, MediaType - Params, !PS) :-
    media_type(Src, MediaType, !PS),
    zero_or_more_parameters_list(Src, Params, !PS),
    eof(Src, _, !PS).

:- pred media_type(src::in, string::out, ps::in, ps::out) is semidet.

media_type(Src, MediaType, !PS) :-
    % The RFC is more restrictive.
    token(Src, Type, !PS),
    punct("/", Src, _, !PS),
    token(Src, SubType, !PS),
    string.to_lower(Type ++ "/" ++ SubType, MediaType). % case-insensitive

zero_or_more_parameters_list(Src, Params, !PS) :-
    zero_or_more(semicolon_then_parameter, Src, _, map.init, Params, !PS).

:- pred semicolon_then_parameter(src::in, unit::out,
    parameters::in, parameters::out, ps::in, ps::out) is semidet.

semicolon_then_parameter(Src, unit, !Params, !PS) :-
    punct(";", Src, _, !PS),
    parameter_attribute(Src, Attrib, !PS),
    punct("=", Src, _, !PS),
    parameter_value(Src, Value, !PS),
    % Do not allow duplicate parameter names.
    map.insert(Attrib, Value, !Params).

:- pred parameter_attribute(src::in, string::out, ps::in, ps::out) is semidet.

parameter_attribute(Src, Attrib, !PS) :-
    token(Src, Token, !PS),
    string.to_lower(Token, Attrib). % case-insensitive

:- pred parameter_value(src::in, string::out, ps::in, ps::out) is semidet.

parameter_value(Src, Value, !PS) :-
    ( token(Src, Token, !PS) ->
        Value = Token
    ;
        rfc2822.quoted_string(Src, Value, !PS)
    ).

token(Src, Token, !PS) :-
    one_or_more_chars_to_string(token_char, Src, Token, !PS),
    call_skip_pred(Src, !PS).

:- pred token_char(char::in) is semidet.

token_char(Char) :-
    char.to_int(Char, Int),
    % Exclude SPACE, CTL (0-31 and DEL), and non-ASCII.
    0x20 < Int, Int < 0x7f,
    not tspecial(Char).

:- pred tspecial(char::in) is semidet.

tspecial('('). tspecial(')'). tspecial('<'). tspecial('>'). tspecial('@').
tspecial(','). tspecial(';'). tspecial(':'). tspecial('\\'). tspecial('"').
tspecial('/'). tspecial('['). tspecial(']'). tspecial('?'). tspecial('=').

%-----------------------------------------------------------------------------%

% 5.2. Content-Type Defaults

content_type_defaults(MediaType, Params) :-
    MediaType = "text/plain",
    Params = map.singleton("charset", "us-ascii").

%-----------------------------------------------------------------------------%

% 6. Content-Transfer-Encoding Header Field

content_transfer_encoding_body(Src, Mechanism, !PS) :-
    token(Src, Token, !PS),
    string.to_lower(Token, Mechanism). % case-insensitive

is_valid_content_transfer_encoding_for_multipart("7bit").
is_valid_content_transfer_encoding_for_multipart("8bit").
is_valid_content_transfer_encoding_for_multipart("binary").

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
