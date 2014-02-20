:- module httpsrv.parse_url.

% Copyright (C) 2014 YesLogic Pty. Ltd.
% All rights reserved.

:- interface.

:- pred parse_url(string::in, url::out) is semidet.

:- pred is_absolute_url(url::in) is semidet.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module string.

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

parse_url(Input, Url) :-
    % XXX struct http_parser_url uses uint16_t for offset and length
    % so will produce incorrect results on very long strings.
    ( string.count_code_units(Input) =< 0xffff ->
        parse_url_2(Input, ParseResult)
    ;
        fail
    ),
    require_det (
        MaybeField = maybe_field(Input, ParseResult),
        MaybeField('UF_SCHEMA', MaybeScheme),
        MaybeField('UF_HOST', MaybeHost),
        MaybeField('UF_PORT', MaybePort),
        MaybeField('UF_PATH', MaybePathRaw),
        MaybeField('UF_QUERY', MaybeQuery),
        MaybeField('UF_FRAGMENT', MaybeFragment),
        Url = url(MaybeScheme, MaybeHost, MaybePort, MaybePathRaw, MaybeQuery,
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
    if (ParseResult->field_set & (1 << Field)) {
        uint16_t off = ParseResult->field_data[Field].off;
        uint16_t len = ParseResult->field_data[Field].len;
        MR_bool valid;

        Str = make_string_utf8(Input, off, len, &valid);
        SUCCESS_INDICATOR = valid;
    } else {
        Str = MR_make_string_const("""");
        SUCCESS_INDICATOR = MR_FALSE;
    }
").

%-----------------------------------------------------------------------------%

is_absolute_url(Url) :-
    Url ^ scheme = yes(_).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
