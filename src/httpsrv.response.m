:- module httpsrv.response.

% Copyright (C) 2014 YesLogic Pty. Ltd.
% All rights reserved.

:- interface.

:- func render_response_header(response_header) = cord(string).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module list.

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
        H = set_cookie(Name - Value, Params),
        L = ["Set-Cookie: ", Name, "=", Value | render_params(Params)]
    ;
        H = x_content_type_options_nosniff,
        L = ["X-Content-Type-Options: nosniff"]
    ;
        H = custom(Name - Value, Params),
        L = [Name, ": ", Value | render_params(Params)]
    ).

:- func render_params(list(pair(string))) = list(string).

render_params(Pairs) = condense(map(render_param, Pairs)).

:- func render_param(pair(string)) = list(string).

render_param(Name - Value) = ["; ", Name, "=", Value].

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
