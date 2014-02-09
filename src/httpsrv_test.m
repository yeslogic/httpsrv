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
:- import_module string.
:- import_module thread.

:- import_module httpsrv.

%-----------------------------------------------------------------------------%

main(!IO) :-
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
        run(Daemon, !IO)
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
    Headers = Request ^ headers,
    Body = Request ^ body,
    Content = [
        "Method: ", string(Method), "\n",
        "URL: ", Url, "\n",
        "Headers: ", string(Headers), "\n",
        "Body: ", string(Body), "\n"
    ],
    set_response(Client, Request, Content, !IO),
    cc_multi_equal(!IO).

:- pred usleep(int::in, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    usleep(N::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    usleep(N);
").

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
