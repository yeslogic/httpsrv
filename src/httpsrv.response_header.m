:- module httpsrv.response_header.

% Copyright (C) 2014 YesLogic Pty. Ltd.
% All rights reserved.

:- interface.

:- func render_response_header(response_header) = string.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module list.
:- import_module string.

:- import_module http_date.

%-----------------------------------------------------------------------------%

render_response_header(H) = S :-
    (
        H = cache_control_max_age(DeltaSeconds),
        S = format("Cache-Control: max-age=%d\r\n", [i(DeltaSeconds)])
    ;
        H = content_type(MediaType),
        S = "Content-Type: " ++ MediaType ++ "\r\n"
    ;
        H = content_type_charset_utf8(MediaType),
        S = "Content-Type: " ++ MediaType ++ "; charset=utf-8\r\n"
    ;
        H = content_disposition(DispositionType),
        S = "Content-Disposition: " ++ DispositionType ++ "\r\n"
    ;
        H = location(URI),
        S = "Location: " ++ URI ++ "\r\n"
    ;
        H = set_cookie(Name - Value, Attrs),
        S = format("Set-Cookie: %s=%s%s\r\n",
                [s(Name), s(Value), s(render_cookie_attrs(Attrs))])
    ;
        H = x_content_type_options_nosniff,
        S = "X-Content-Type-Options: nosniff\r\n"
    ;
        H = custom(Name - Value, Params),
        S = format("%s: %s%s\r\n",
                [s(Name), s(Value), s(render_params(Params))])
    ).

:- func render_cookie_attrs(list(cookie_attribute)) = string.

render_cookie_attrs(Attrs) =
    append_list(condense(map(render_cookie_attr, Attrs))).

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

:- func render_params(list(pair(string))) = string.

render_params(Pairs) =
    append_list(condense(map(render_param, Pairs))).

:- func render_param(pair(string)) = list(string).

render_param(Name - Value) = ["; ", Name, "=", Value].

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
