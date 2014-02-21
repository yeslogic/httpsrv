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

:- import_module multipart_parser.
:- import_module percent_decoding.
:- import_module urlencoding.
:- use_module rfc2045.
:- use_module rfc2046.
:- use_module rfc2616.
:- use_module rfc2822.
:- use_module rfc6265.

:- import_module httpsrv.formdata_accum.
:- import_module httpsrv.parse_url.
:- import_module httpsrv.url.

:- pragma foreign_decl("C", "
    typedef struct client client_t;
    typedef struct buffer buffer_t;
").

%-----------------------------------------------------------------------------%

:- func request_init(client) = request.

:- pragma foreign_export("C", request_init(in) = out, "request_init").

request_init(Client) =
    request(Client, other(""), "", url.init, no, [], init_headers, [],
        string("")).

%-----------------------------------------------------------------------------%

:- func request_add_header(request, string, string) = request.

:- pragma foreign_export("C", request_add_header(in, in, in) = out,
    "request_add_header").

request_add_header(Req0, Name, Body) = Req :-
    % HTTP allows some (though not all) duplicate header fields.
    Req0 ^ headers = Headers0,
    (
        add_header(append_duplicate_header, from_string(Name), Body,
            Headers0, Headers)
    ->
        Req = Req0 ^ headers := Headers
    ;
        % Shouldn't happen.
        Req = Req0
    ).

%-----------------------------------------------------------------------------%

:- func request_get_expect_header(request) = int.

:- pragma foreign_export("C", request_get_expect_header(in) = out,
    "request_get_expect_header").

request_get_expect_header(Req) = Result :-
    Headers = Req ^ headers,
    ( search_field(Headers, expect, [Body]) ->
        % Strictly speaking the expectation value can be a comma separated list
        % and "100-continue" is one of the possible elements of that list.
        % But at least HTTP field values may NOT have comments unless
        % specifically stated (unlike RFC 822).
        ( string.to_lower(Body, "100-continue") ->
            Result = 1
        ;
            Result = -1
        )
    ;
        Result = 0
    ).

:- func expect = case_insensitive.

expect = case_insensitive("expect").

%-----------------------------------------------------------------------------%

:- pred request_prepare(string::in, string::in, bool::in,
    request::in, request::out) is semidet.

:- pragma foreign_export("C", request_prepare(in, in, in, in, out),
    "request_prepare").

request_prepare(MethodString, RequestUri, EnforceHostHeader, !Req) :-
    Headers = !.Req ^ headers,
    request_set_method(MethodString, !Req),
    ( search_field(Headers, host, [HostFieldValuePrime]) ->
        HostFieldValue = HostFieldValuePrime
    ;
        EnforceHostHeader = no,
        HostFieldValue = ""
    ),
    request_set_request_uri(RequestUri, HostFieldValue, !Req),
    request_set_cookies(!Req).

:- func host = case_insensitive.

host = case_insensitive("host").

:- pred request_set_method(string::in, request::in, request::out) is det.

request_set_method(MethodString, !Req) :-
    ( method(MethodString, Method) ->
        !Req ^ method := Method
    ;
        !Req ^ method := other(MethodString)
    ).

:- pred request_set_request_uri(string::in, string::in,
    request::in, request::out) is semidet.

request_set_request_uri(RequestUri, HostFieldValue, !Req) :-
    !Req ^ request_uri := RequestUri,
    ( RequestUri = "*" ->
        % Request applies to the server and not a resource.
        % Maybe we could add an option for this.
        true
    ;
        make_url(RequestUri, HostFieldValue, Url),
        require_det (
            decode_path(Url, MaybePathDecoded),
            decode_query_parameters(Url, QueryParams),
            !Req ^ url := Url,
            !Req ^ path_decoded := MaybePathDecoded,
            !Req ^ query_params := QueryParams
        )
    ).

    % 5.2. The Resource Identified by a Request
    %
:- pred make_url(string::in, string::in, url::out) is semidet.

make_url(RequestUri, HostFieldValue, Url) :-
    ( RequestUri = "*" ->
        Url = url.init
    ;
        parse_url.parse_url(RequestUri, Url0),
        ( is_absolute_url(Url0) ->
            % Ignore the Host header field.
            Url = Url0
        ; HostFieldValue = "" ->
            % "If the requested URI does not include an Internet host name for
            % the service being requested, then the Host header field MUST be
            % given with an empty value."
            Url = Url0
        ;
            rfc2616.parse_host_header_value(HostFieldValue, Host, MaybePort),
            Url1 = Url0 ^ host := yes(Host),
            Url = Url1 ^ port := MaybePort
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
    ( search_field(!.Req ^ headers, cookie, CookieHeaderValues) ->
        list.filter_map(rfc6265.parse_cookie_header_value, CookieHeaderValues,
            Cookiess),
        list.condense(Cookiess, Cookies),
        !Req ^ cookies := Cookies
    ;
        true
    ).

:- func cookie = case_insensitive.

cookie = case_insensitive("cookie").

%-----------------------------------------------------------------------------%

:- func request_set_body_stringish(request, string) = request.

:- pragma foreign_export("C", request_set_body_stringish(in, in) = out,
    "request_set_body_stringish").

request_set_body_stringish(Req0, String) = Req :-
    Headers = Req0 ^ headers,
    (
        search_field(Headers, content_type, [ContentTypeBody]),
        % Hack: this is a HTTP header not a MIME header.
        % One known difference is that [RFC 2616] 3.7 Media Types:
        % "Linear white space (LWS) MUST NOT be used [...] between
        % an attribute and its value."
        rfc2822.parse_structured_field_body(ContentTypeBody,
            rfc2045.content_type_body, MediaType - _Params),
        MediaType = application_x_www_form_urlencoded,
        parse_form_urlencoded(String, Form)
    ->
        Body = form_urlencoded(Form)
    ;
        Body = string(String)
    ),
    Req = Req0 ^ body := Body.

:- func content_type = case_insensitive.

content_type = case_insensitive("content-type").

:- func application_x_www_form_urlencoded = case_insensitive.

application_x_www_form_urlencoded =
    case_insensitive("application/x-www-form-urlencoded").

%-----------------------------------------------------------------------------%

:- pred request_search_multipart_formdata_boundary(request::in, string::out)
    is semidet.

:- pragma foreign_export("C",
    request_search_multipart_formdata_boundary(in, out),
    "request_search_multipart_formdata_boundary").

request_search_multipart_formdata_boundary(Req, Boundary) :-
    Headers = Req ^ headers,
    search_field(Headers, content_type, [ContentTypeBody]),

    % Hack: this is a HTTP header not a MIME header.
    % See also another instance above.
    rfc2822.parse_structured_field_body(ContentTypeBody,
        rfc2045.content_type_body, MediaType - ParamsMap),
    % XXX report errors, reject other multipart types
    MediaType = multipart_formdata,

    Params = init_parameters_from_map(ParamsMap),
    search_parameter(Params, boundary, Boundary),
    rfc2046.is_valid_boundary(Boundary).
    % [RFC 2616] HTTP, unlike MIME, does not use Content-Transfer-Encoding.
    % So we don't have to check that.

:- func multipart_formdata = case_insensitive.

multipart_formdata = case_insensitive("multipart/form-data").

:- func boundary = case_insensitive.

boundary = case_insensitive("boundary").

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

:- pred request_set_body_formdata(multipart_parser(formdata_accum)::in,
    request::in, request::out, bool::out) is det.

:- pragma foreign_export("C", request_set_body_formdata(in, in, out, out),
    "request_set_body_formdata").

request_set_body_formdata(PS, !Req, Valid) :-
    ( valid_final_state(PS) ->
        get_parts(get_userdata(PS), MaybeParts),
        (
            MaybeParts = ok(Parts),
            !Req ^ body := multipart_formdata(Parts),
            Valid = yes
        ;
            MaybeParts = error(_),
            Valid = no
        )
    ;
        Valid = no
    ).

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
