:- module httpsrv.url.

% Copyright (C) 2014 YesLogic Pty. Ltd.
% All rights reserved.

:- interface.

:- func init = url.

:- pred is_absolute_url(url::in) is semidet.

:- pred resolve_relative(url::in, url::in, url::out) is det.

:- func to_string(url) = string.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module list.
:- import_module string.

init = url(no, no, no, no, no, no).

is_absolute_url(Url) :-
    Url ^ scheme = yes(_).

% [RFC 2396] 5.2. Resolving Relative References to Absolute Form

resolve_relative(BaseUrl, RelUrl, Url) :-
    BaseUrl = url(BaseScheme, BaseHost, BasePort, _BasePath, _BaseQuery,
        _BaseFragment),
    RelUrl = url(RelScheme, RelHost, RelPort, RelPath, RelQuery,
        RelFragment),
    (
        RelScheme = yes(_),
        % RelUrl is absolute.
        Url = RelUrl
    ;
        RelScheme = no,
        Scheme = BaseScheme,
        (
            RelHost = yes(_),
            Host = RelHost,
            Port = RelPort
        ;
            RelHost = no,
            Host = BaseHost,
            Port = BasePort
        ),
        % XXX step 6: resolve a relative-path to the base path.
        Path = RelPath,
        Query = RelQuery,
        Fragment = RelFragment,
        Url = url(Scheme, Host, Port, Path, Query, Fragment)
    ).

to_string(Url) = String :-
    Url = url(MaybeScheme, MaybeHost, MaybePort, MaybePath, MaybeQuery,
        MaybeFragment),
    some [!Acc] (
        !:Acc = [],
        (
            MaybeFragment = yes(Fragment),
            !:Acc = ["#", Fragment | !.Acc]
        ;
            MaybeFragment = no
        ),
        (
            MaybeQuery = yes(Query),
            !:Acc = ["?", Query | !.Acc]
        ;
            MaybeQuery = no
        ),
        (
            MaybePath = yes(Path),
            !:Acc = [Path | !.Acc]
        ;
            MaybePath = no
        ),
        (
            MaybeHost = yes(Host),
            MaybePort = yes(Port),
            !:Acc = ["//", Host, ":", Port | !.Acc]
        ;
            MaybeHost = yes(Host),
            MaybePort = no,
            !:Acc = ["//", Host | !.Acc]
        ;
            MaybeHost = no
        ),
        (
            MaybeScheme = yes(Scheme),
            !:Acc = [Scheme, ":" | !.Acc]
        ;
            MaybeScheme = no
        ),
        String = string.append_list(!.Acc)
    ).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
