:- module httpsrv_test.

% Copyright (C) 2014 YesLogic Pty. Ltd.
% All rights reserved.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module list.
:- import_module maybe.
:- import_module pair.
:- import_module string.
:- import_module thread.

:- import_module httpsrv.
:- import_module httpsrv.status.

%-----------------------------------------------------------------------------%

main(!IO) :-
    ignore_sigpipe(!IO),
    Settings = [
        bind_address("0.0.0.0" @ BindAddress),
        port(8000 @ Port),
        back_log(8)
    ],
    setup(request_handler, Settings, Res, !IO),
    (
        Res = ok(Daemon),
        io.format("Started server on %s port %d\n",
            [s(BindAddress), i(Port)], !IO),
        run(Daemon, !IO),
        io.write_string("Server stopped.\n", !IO)
    ;
        Res = error(Error),
        io.format(io.stderr_stream, "setup failed: %s\n", [s(Error)], !IO),
        io.set_exit_status(1, !IO)
    ).

:- pred request_handler(client::in, request::in, io::di, io::uo) is cc_multi.

request_handler(Client, Request, !IO) :-
    % real_handler(Client, Request, !IO).
    thread.spawn(real_handler(Client, Request), !IO).

:- pred real_handler(client::in, request::in, io::di, io::uo) is cc_multi.

real_handler(Client, Request, !IO) :-
    usleep(100000, !IO),
    Method = Request ^ method,
    Url = Request ^ url,
    MaybePathDecoded = Request ^ path_decoded,
    QueryParams = Request ^ query_params,
    Cookies = Request ^ cookies,
    (
        MaybePathDecoded = yes(PathDecoded),
        static_path(PathDecoded, FilePath)
    ->
        open_static_file(FilePath, OpenResult, !IO),
        (
            OpenResult = ok(StaticFile),
            Status = ok_200,
            AdditionalHeaders = [content_type("application/octet-stream")],
            Content = file(StaticFile)
        ;
            OpenResult = error(Error),
            Status = not_found_404,
            AdditionalHeaders = [],
            Content = strings([Error])
        )
    ;
        get_client_address_ipv4(Client, MaybeClientAddress, !IO),
        Status = ok_200,
        AdditionalHeaders = [set_cookie("my-cookie" - "my-cookie-value", [])],
        Headers = Request ^ headers,
        Body = Request ^ body,
        Content = strings([
            "Client address: ", string(MaybeClientAddress), "\n",
            "Method: ", string(Method), "\n",
            "URL: ", string(Url), "\n",
            "Path: ", string(MaybePathDecoded), "\n",
            "Query parameters: ", string(QueryParams), "\n",
            "Headers: ", string(Headers), "\n",
            "Cookies: ", string(Cookies), "\n",
            "Body: ", string(Body), "\n"
        ])
    ),
    set_response(Client, Request, Status, AdditionalHeaders, Content, !IO),
    cc_multi_equal(!IO).

:- pred static_path(string::in, string::out) is semidet.

static_path(PathDecoded, StaticPath) :-
    Resource = words_separator(unify('/'), PathDecoded),
    Resource = ["static" | _],
    StaticPath = string.join_list("/", Resource).

:- pred usleep(int::in, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    usleep(N::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    usleep(N);
").

%-----------------------------------------------------------------------------%

:- pragma foreign_decl("C", local, "
#include ""mercury_signal.h""

static void     handle_sigpipe(int signum);
").

:- pred ignore_sigpipe(io::di, io::uo) is det.

:- pragma foreign_proc("C",
    ignore_sigpipe(_IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io,
        may_not_duplicate],
"
    MR_setup_signal(SIGPIPE, handle_sigpipe, MR_FALSE,
        ""cannot install signal handler (SIGPIPE)"");
").

:- pragma foreign_code("C", "
static void handle_sigpipe(int signum)
{
    fprintf(stderr, ""Ignoring signal %d (SIGPIPE).\\n"", signum);
}
").

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
