:- module httpsrv.signal.

% Copyright (C) 2014 YesLogic Pty. Ltd.
% All rights reserved.

:- interface.

:- import_module io.

:- pred ignore_sigpipe(io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- pragma foreign_decl("C", local, "
    #include ""mercury_signal.h""

    static void handle_sigpipe(int signum);
").

:- pragma foreign_proc("C",
    ignore_sigpipe(_IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io,
        may_not_duplicate],
"
#ifdef SIGPIPE
    MR_setup_signal(SIGPIPE, handle_sigpipe, MR_FALSE,
        ""cannot install signal handler (SIGPIPE)"");
#endif
").

:- pragma foreign_code("C", "
static void handle_sigpipe(int signum)
{
    fprintf(stderr, ""Ignoring signal %d (SIGPIPE).\\n"", signum);
}
").

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
