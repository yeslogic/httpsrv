:- module httpsrv.response_header.

% Copyright (C) 2014 YesLogic Pty. Ltd.
% All rights reserved.

:- interface.

:- import_module cord.

:- func render_response_header(response_header) = cord(string).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module list.
:- import_module string.

:- import_module http_date.

%-----------------------------------------------------------------------------%

render_response_header(Header) =
    from_list(render(Header)) ++ singleton("\r\n").

:- func render(response_header) = list(string).

render(H) = L :-
    (
        H = cache_control_max_age(DeltaSeconds),
        L = ["Cache-Control: max-age=", from_int(DeltaSeconds)]
    ;
        H = content_type(MediaType),
        L = ["Content-Type: ", MediaType]
    ;
        H = content_type_charset_utf8(MediaType),
        L = ["Content-Type: ", MediaType, "; charset=utf-8"]
    ;
        H = content_disposition(DispositionType),
        L = ["Content-Disposition: ", DispositionType]
    ;
        H = location(URI),
        L = ["Location: ", URI]
    ;
        H = set_cookie(Name - Value, Attrs),
        L = ["Set-Cookie: ", Name, "=", Value | render_cookie_attrs(Attrs)]
    ;
        H = x_content_type_options_nosniff,
        L = ["X-Content-Type-Options: nosniff"]
    ;
        H = custom(Name - Value, Params),
        L = [Name, ": ", Value | render_params(Params)]
    ).

:- func render_cookie_attrs(list(cookie_attribute)) = list(string).

render_cookie_attrs(Attrs) = condense(map(render_cookie_attr, Attrs)).

:- func render_cookie_attr(cookie_attribute) = list(string).

render_cookie_attr(Attr) = L :-
    (
        Attr = expires(Time),
        L = ["; Expires=", timestamp_to_http_date(Time)]
    ;
        Attr = max_age(MaxAge),
        % By RFC 6265, this should be strictly positive (not zero).
        L = ["; MaxAge=", from_int(MaxAge)]
    ;
        Attr = domain(Domain),
        L = ["; Domain=", Domain]
    ;
        Attr = path(Path),
        L = ["; Path=", Path]
    ;
        Attr = secure,
        L = ["; Secure"]
    ;
        Attr = httponly,
        L = ["; HttpOnly"]
    ).

:- func render_params(list(pair(string))) = list(string).

render_params(Pairs) = condense(map(render_param, Pairs)).

:- func render_param(pair(string)) = list(string).

render_param(Name - Value) = ["; ", Name, "=", Value].

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
