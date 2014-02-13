:- module httpsrv.request.

% Copyright (C) 2014 YesLogic Pty. Ltd.
% All rights reserved.

:- interface.

:- type dummy_export
    --->    dummy_export.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module string.

:- import_module mime_headers.
:- import_module multipart_parser.
:- import_module percent_decoding.
:- import_module urlencoding.
:- use_module rfc6265.

:- import_module httpsrv.formdata_accum.
:- import_module httpsrv.parse_url.

:- pragma foreign_decl("C", "
    typedef struct client client_t;
    typedef struct buffer buffer_t;
").

%-----------------------------------------------------------------------------%

:- func request_init(client) = request.

:- pragma foreign_export("C", request_init(in) = out, "request_init").

request_init(Client) =
    request(Client, other(""), "", url_init, no, [], init_headers, [], none).

%-----------------------------------------------------------------------------%

:- func request_add_header(request, string, string) = request.

:- pragma foreign_export("C", request_add_header(in, in, in) = out,
    "request_add_header").

request_add_header(Req0, Name, Body) = Req :-
    Req0 ^ headers = Headers0,
    add_header(Name, Body, Headers0, Headers),
    Req = Req0 ^ headers := Headers.

%-----------------------------------------------------------------------------%

:- func request_get_expect_header(request) = int.

:- pragma foreign_export("C", request_get_expect_header(in) = out,
    "request_get_expect_header").

request_get_expect_header(Req) = Result :-
    Headers = Req ^ headers,
    ( search_field(Headers, "Expect", Body) ->
        % Strictly speaking the expectation value can be a comma separated list
        % and "100-continue" is one of the possible elements of that list.
        % But at least HTTP field values may NOT have comments unless
        % specifically stated (unlike RFC 822).
        ( string_equal_ci(Body, "100-continue") ->
            Result = 1
        ;
            Result = -1
        )
    ;
        Result = 0
    ).

:- pred string_equal_ci(string::in, string::in) is semidet.

string_equal_ci(A, B) :-
    string.to_lower(A, Lower),
    string.to_lower(B, Lower).

%-----------------------------------------------------------------------------%

:- pred request_prepare(string::in, string::in, request::in, request::out)
    is semidet.

:- pragma foreign_export("C", request_prepare(in, in, in, out),
    "request_prepare").

request_prepare(MethodString, UrlString, !Req) :-
    request_set_method(MethodString, !Req),
    request_set_url(UrlString, !Req),
    request_set_cookies(!Req).

:- pred request_set_method(string::in, request::in, request::out) is det.

request_set_method(MethodString, !Req) :-
    ( method(MethodString, Method) ->
        !Req ^ method := Method
    ;
        !Req ^ method := other(MethodString)
    ).

:- pred request_set_url(string::in, request::in, request::out) is semidet.

request_set_url(UrlString, !Req) :-
    !Req ^ url_raw := UrlString,
    ( UrlString = "*" ->
        % Request applies to the server and not a resource.
        % Maybe we could add an option for this.
        true
    ;
        parse_url_and_host_header(!.Req ^ headers, UrlString, Url),
        require_det (
            decode_path(Url, MaybePathDecoded),
            decode_query_parameters(Url, QueryParams),
            !Req ^ url := Url,
            !Req ^ path_decoded := MaybePathDecoded,
            !Req ^ query_params := QueryParams
        )
    ).

:- pred decode_path(url::in, maybe(string)::out) is det.

decode_path(Url, MaybePathDecoded) :-
    MaybePathRaw = Url ^ path_raw,
    (
        MaybePathRaw = yes(PathRaw),
        percent_decode(PathRaw, PathDecoded)
    ->
        MaybePathDecoded = yes(PathDecoded)
    ;
        MaybePathDecoded = no
    ).

:- pred decode_query_parameters(url::in, assoc_list(string)::out) is det.

decode_query_parameters(Url, Params) :-
    MaybeQuery = Url ^ query_raw,
    (
        MaybeQuery = yes(Query),
        parse_query_parameters(Query, ParamsPrime)
    ->
        Params = ParamsPrime
    ;
        Params = []
    ).

:- pred request_set_cookies(request::in, request::out) is det.

request_set_cookies(!Req) :-
    % Parse all Cookie: header values, dropping anything we can't recognise.
    search_field_multi(!.Req ^ headers, "Cookie", CookieHeaderValues),
    list.filter_map(rfc6265.parse_cookie_header_value, CookieHeaderValues,
        Cookiess),
    list.condense(Cookiess, Cookies),
    !Req ^ cookies := Cookies.

%-----------------------------------------------------------------------------%

:- func request_set_body_stringish(request, string) = request.

:- pragma foreign_export("C", request_set_body_stringish(in, in) = out,
    "request_set_body_stringish").

request_set_body_stringish(Req0, String) = Req :-
    Headers = Req0 ^ headers,
    (
        get_content_type(Headers, MediaType, _Params),
        media_type_equals(MediaType, application_x_www_form_urlencoded),
        parse_form_urlencoded(String, Form)
    ->
        Body = form_urlencoded(Form)
    ;
        Body = string(String)
    ),
    Req = Req0 ^ body := Body.

:- func application_x_www_form_urlencoded = string.

application_x_www_form_urlencoded = "application/x-www-form-urlencoded".

%-----------------------------------------------------------------------------%

:- pred request_search_multipart_formdata_boundary(request::in, string::out)
    is semidet.

:- pragma foreign_export("C",
    request_search_multipart_formdata_boundary(in, out),
    "request_search_multipart_formdata_boundary").

request_search_multipart_formdata_boundary(Req, Boundary) :-
    Headers = Req ^ headers,
    % XXX report errors, reject other multipart types
    is_multipart_content_type(Headers, MaybeMultiPart),
    MaybeMultiPart = multipart(MediaType, Boundary),
    media_type_equals(MediaType, multipart_formdata).

:- func multipart_formdata = string.

multipart_formdata = "multipart/form-data".

%-----------------------------------------------------------------------------%

:- func create_formdata_parser(string) = multipart_parser(formdata_accum).

:- pragma foreign_export("C", create_formdata_parser(in) = out,
    "create_formdata_parser").

create_formdata_parser(Boundary) =
    multipart_parser.init(Boundary, formdata_accum.init).

:- pred parse_formdata(buffer::in, int::in, int::out,
    multipart_parser(formdata_accum)::in, multipart_parser(formdata_accum)::out,
    bool::out, string::out, io::di, io::uo) is det.

:- pragma foreign_export("C",
    parse_formdata(in, in, out, in, out, out, out, di, uo),
    "parse_formdata").

parse_formdata(Buf, !BufPos, !PS, IsError, ErrorString, !IO) :-
    multipart_parser.execute(Buf, !BufPos, !PS, !IO),
    multipart_parser.get_error(!.PS, MaybeError),
    (
        MaybeError = ok,
        IsError = no,
        ErrorString = ""
    ;
        MaybeError = error(ErrorString),
        IsError = yes
    ).

:- func request_set_body_formdata(request, multipart_parser(formdata_accum))
    = request.

:- pragma foreign_export("C", request_set_body_formdata(in, in) = out,
    "request_set_body_formdata").

request_set_body_formdata(Req0, PS) = Req :-
    Parts = get_parts(get_userdata(PS)),
    Req = Req0 ^ body := multipart_formdata(Parts).

%-----------------------------------------------------------------------------%

:- pred call_request_handler_pred(request_handler::in(request_handler),
    request::in, io::di, io::uo) is cc_multi.

:- pragma foreign_export("C",
    call_request_handler_pred(in(request_handler), in, di, uo),
    "call_request_handler_pred").

call_request_handler_pred(Pred, Request, !IO) :-
    call(Pred, Request, !IO).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
