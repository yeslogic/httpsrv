:- module rfc2183_test.

% Copyright (C) 2014 YesLogic Pty. Ltd.
% All rights reserved.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module list.
:- import_module map.
:- import_module pair.
:- import_module string.

:- import_module case_insensitive.
:- import_module rfc2183.
:- import_module rfc2822.

:- type test_case
    --->    test_case(
                input :: string,
                expected :: string
            ).

%-----------------------------------------------------------------------------%

main(!IO) :-
    io.command_line_arguments(Args, !IO),
    (
        Args = [Arg | _],
        string.to_int(Arg, Drop),
        list.drop(Drop, test_cases, TestCasesPrime)
    ->
        TestCases = TestCasesPrime
    ;
        TestCases = test_cases
    ),
    list.foldl(test, TestCases, !IO).

:- pred test(test_case::in, io::di, io::uo) is det.

test(test_case(Input, Expected), !IO) :-
    io.write_string("--------------------\n", !IO),
    io.write_string("Input = «", !IO),
    io.write_string(Input, !IO),
    io.write_string("»\n", !IO),
    (
        rfc2822.parse_structured_field_body(Input,
            rfc2183.content_disposition_body, _ - Params)
    ->
        ( map.search(Params, case_insensitive("name"), Name) ->
            ( Name = Expected ->
                io.write_string("OK - ", !IO)
            ;
                io.write_string("FAIL - ", !IO),
                io.set_exit_status(1, !IO)
            ),
            io.write_string("name=«", !IO),
            io.write_string(Name, !IO),
            io.write_string("»\n", !IO)
        ;
            io.write_string("FAIL - missing name attribute\n", !IO),
            io.set_exit_status(1, !IO)
        )
    ;
        io.write_string("FAIL - parse failure\n", !IO),
        io.set_exit_status(1, !IO)
    ).

:- func test_cases = list(test_case).

test_cases = [
    % Normal - do not break this!
    test_case(
        replace("form-data; name=[QUOT]a[QUOT]"),
        replace("a")
    ),

    % Normal - do not break this!
    test_case(
        replace("form-data; name=[QUOT]a[QUOT]; filename=[QUOT]b[QUOT]"),
        replace("a")
    ),

    % Chrome / Firefox / IE8
    test_case(
        replace("form-data; name=[QUOT]a[BKSL]b[QUOT]"),
        % replace("ab")
        % the user actually intended:
        replace("a[BKSL]b")
    ),

    % Chrome / Firefox / IE8
    test_case(
        replace("form-data; name=[QUOT]a[BKSL]b[BKSL][QUOT]"),
        replace("a[BKSL]b[BKSL]")
    ),

    % Firefox
    % «form-data; name="a\b\\\""»
    test_case(
        replace("form-data; name=[QUOT]a[BKSL]b[BKSL][BKSL][BKSL][QUOT][QUOT]"),
        replace("a[BKSL]b[BKSL][BKSL][QUOT]")
    )

    % IE8
    /*
    test_case(
        replace("form-data; name=[QUOT]a[QUOT]b[QUOT]"),
        replace("a[QUOT]b")
    )
    */
].

:- func replace(string) = string.

replace(S0) = S :-
    string.replace_all(S0, "[QUOT]", "\"", S1),
    string.replace_all(S1, "[BKSL]", "\\", S).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
