:- module rfc2183.

% Copyright (C) 2014 YesLogic Pty. Ltd.
% All rights reserved.

:- interface.

:- import_module pair.
:- import_module parsing_utils.

:- import_module case_insensitive.
:- import_module rfc2045.

:- pred content_disposition_body(src::in,
    pair(case_insensitive, parameters)::out, ps::in, ps::out) is semidet.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

content_disposition_body(Src, DispositionType - Params, !PS) :-
    disposition_type(Src, DispositionType, !PS),

    % Mozilla and WebKit both don't escape backslashes such that an HTML form
    % with a control named "foo\" will result in a Content-Disposition line
    % which we cannot parse - the backslash consumes the closing double quote
    % character.
    %
    % Mozilla escapes double quotes as \"
    % WebKit encodes double quotes as %022
    %
    % Mozilla and WebKit both pass UTF-8 code points through unencoded.

    zero_or_more_parameters_list(Src, Params, !PS),
    eof(Src, _, !PS).

:- pred disposition_type(src::in, case_insensitive::out, ps::in, ps::out)
    is semidet.

disposition_type(Src, DispositionType, !PS) :-
    token(Src, Token, !PS),
    DispositionType = from_string(Token).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
