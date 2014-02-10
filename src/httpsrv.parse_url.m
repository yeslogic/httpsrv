:- module httpsrv.parse_url.

% Copyright (C) 2014 YesLogic Pty. Ltd.
% All rights reserved.

:- interface.

:- pred parse_url_and_host_header(headers::in, string::in, url::out)
    is semidet.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module char.
:- import_module parsing_utils.
:- import_module unit.

:- import_module rfc_parsing_utils.

:- pragma foreign_decl("C", local, "
    #include ""http_parser.h""
").

:- type parse_url_result.

:- pragma foreign_type("C", parse_url_result, "struct http_parser_url *").

:- type parse_url_field
    --->    'UF_SCHEMA'
    ;       'UF_HOST'
    ;       'UF_PORT'
    ;       'UF_PATH'
    ;       'UF_QUERY'
    ;       'UF_FRAGMENT'.

:- pragma foreign_enum("C", parse_url_field/0,
    [
        'UF_SCHEMA' - "UF_SCHEMA",
        'UF_HOST' - "UF_HOST",
        'UF_PORT' - "UF_PORT",
        'UF_PATH' - "UF_PATH",
        'UF_QUERY' - "UF_QUERY",
        'UF_FRAGMENT' - "UF_FRAGMENT"
    ]).

%-----------------------------------------------------------------------------%

% 5.2. The Resource Identified by a Request

parse_url_and_host_header(Headers, UrlString, Url) :-
    ( UrlString = "*" ->
        Url = url_init
    ;
        parse_url(UrlString, Url0),
        search_field(Headers, "Host", HostFieldValue),
        ( is_absolute_url(Url0) ->
            % Ignore the Host header field.
            % Strictly speaking we are supposed to enforce its existence.
            Url = Url0
        ;
            parse_host_and_maybe_port(HostFieldValue, Host, MaybePort),
            Url1 = Url0 ^ host := yes(Host),
            Url = Url1 ^ port := canonicalise_maybe_port(MaybePort)
        )
    ).

:- pred is_absolute_url(url::in) is semidet.

is_absolute_url(Url) :-
    Url ^ schema = yes(_).

:- func canonicalise_maybe_port(maybe(string)) = maybe(string).

canonicalise_maybe_port(no) = no.
canonicalise_maybe_port(yes(Port0)) = yes(Port) :-
    ( port_to_int(Port0, Int) ->
        Port = string.from_int(Int)
    ;
        % Should not be reachable.
        Port = Port0
    ).

%-----------------------------------------------------------------------------%

:- pred parse_url(string::in, url::out) is semidet.

parse_url(Input, Url) :-
    parse_url_2(Input, ParseResult),
    require_det (
        MaybeField = maybe_field(Input, ParseResult),
        MaybeField('UF_SCHEMA', MaybeSchema),
        MaybeField('UF_HOST', MaybeHost),
        MaybeField('UF_PORT', MaybePort),
        MaybeField('UF_PATH', MaybePath),
        MaybeField('UF_QUERY', MaybeQuery),
        MaybeField('UF_FRAGMENT', MaybeFragment),
        Url = url(MaybeSchema, MaybeHost, MaybePort, MaybePath, MaybeQuery,
            MaybeFragment)
    ).

:- pred parse_url_2(string::in, parse_url_result::out) is semidet.

:- pragma foreign_proc("C",
    parse_url_2(Input::in, ParseResult::out),
    [will_not_call_mercury, promise_pure, thread_safe, may_not_duplicate],
"
    ParseResult = MR_GC_NEW(struct http_parser_url);
    memset(ParseResult, 0, sizeof(struct http_parser_url));
    SUCCESS_INDICATOR = (0 == http_parser_parse_url(Input, strlen(Input), 0,
        ParseResult));
").

:- pred maybe_field(string::in, parse_url_result::in, parse_url_field::in,
    maybe(string)::out) is det.

maybe_field(Input, ParseResult, Field, MaybeString) :-
    ( get_field(Input, ParseResult, Field, String) ->
        MaybeString = yes(String)
    ;
        MaybeString = no
    ).

:- pred get_field(string::in, parse_url_result::in, parse_url_field::in,
    string::out) is semidet.

:- pragma foreign_proc("C",
    get_field(Input::in, ParseResult::in, Field::in, Str::out),
    [will_not_call_mercury, promise_pure, thread_safe, may_not_duplicate],
"
    SUCCESS_INDICATOR = (ParseResult->field_set & (1 << Field));
    if (SUCCESS_INDICATOR) {
        uint16_t off = ParseResult->field_data[Field].off;
        uint16_t len = ParseResult->field_data[Field].len;
        Str = make_string(Input, off, len);
    } else {
        Str = MR_make_string_const("""");
    }
").

%-----------------------------------------------------------------------------%

% [RFC 2616] 14.23 Host

:- pred parse_host_and_maybe_port(string::in, string::out, maybe(string)::out)
    is semidet.

parse_host_and_maybe_port(Input, Host, MaybePort) :-
    promise_equivalent_solutions [ParseResult] (
        parsing_utils.parse(Input, skip_LWS_chars, host_and_maybe_port,
            ParseResult)
    ),
    ParseResult = ok(Host - MaybePort).

:- pred skip_LWS_chars(src::in, unit::out, ps::in, ps::out) is semidet.

skip_LWS_chars(Src, unit, !PS) :-
    (
        next_char_no_progress(Src, Char, !PS),
        'LWS'(Char)
    ->
        skip_LWS_chars(Src, _, !PS)
    ;
        true
    ).

:- pred 'LWS'(char::in) is semidet.

'LWS'(' ').
'LWS'('\t').

:- pred host_and_maybe_port(src::in, pair(string, maybe(string))::out,
    ps::in, ps::out) is semidet.

host_and_maybe_port(Src, Host - MaybePort, !PS) :-
    host(Src, Host, !PS),
    optional(colon_port, Src, MaybePort, !PS),
    eof(Src, _, !PS).

:- pred host(src::in, string::out, ps::in, ps::out) is semidet.

host(Src, Host, !PS) :-
    % [RFC 2732]
    ( next_char_no_progress(Src, ('['), !PS) ->
        one_or_more_chars_to_string(ipv6_host_char, Src, Host, !PS),
        next_char_no_progress(Src, ']', !PS)
    ;
        one_or_more_chars_to_string(host_char, Src, Host, !PS)
    ),
    call_skip_pred(Src, !PS).

:- pred host_char(char::in) is semidet.

host_char(C) :-
    ( char.is_alnum(C)
    ; C = ('.')
    ; C = ('-')
    ).

:- pred ipv6_host_char(char::in) is semidet.

ipv6_host_char(C) :-
    ( char.is_hex_digit(C)
    ; C = (':')
    ).

:- pred colon_port(src::in, string::out, ps::in, ps::out) is semidet.

colon_port(Src, Port, !PS) :-
    punct(":", Src, _, !PS),
    port(Src, Port, !PS).

:- pred port(src::in, string::out, ps::in, ps::out) is semidet.

port(Src, Port, !PS) :-
    one_or_more_chars_to_string(char.is_digit, Src, Port0, !PS),
    % Canonicalise port.
    port_to_int(Port0, Int),
    Port = string.from_int(Int).

:- pred port_to_int(string::in, int::out) is semidet.

port_to_int(String, Int) :-
    string.to_int(String, Int),
    Int > 0.

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
