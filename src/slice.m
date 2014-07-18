:- module slice.

% Copyright (C) 2014 YesLogic Pty. Ltd.
% All rights reserved.

:- interface.

:- type slice
    --->    slice(c_pointer, int).  % base, length

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
