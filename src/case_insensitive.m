:- module case_insensitive.

% Copyright (C) 2014 YesLogic Pty. Ltd.
% All rights reserved.

:- interface.

    % Represents a string that is case-insensitive in the ASCII range ONLY.
    % The representation must only use lowercase ASCII letters [a-z]
    % instead of uppercase ASCII letters [A-Z].
    %
:- type case_insensitive
    --->    case_insensitive(string).

:- func from_string(string) = case_insensitive.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module string.

from_string(S) = case_insensitive(string.to_lower(S)).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
