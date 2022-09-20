:- module httpsrv.static_file.

% Copyright (C) 2014 YesLogic Pty. Ltd.
% All rights reserved.

:- interface.

:- pred open_static_file(string::in, maybe_error(static_file)::out,
    io::di, io::uo) is det.

:- pred close_static_file(static_file::in, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module int.

:- pragma foreign_decl("C", local, "
    #include <fcntl.h>
").

open_static_file(Path, Result, !IO) :-
    open_static_file_2(Path, Fd, Size, Mtime, Error, !IO),
    ( Fd < 0 ->
        Result = error(Error)
    ;
        Result = ok(static_file(Fd, Size, Mtime))
    ).

:- pred open_static_file_2(string::in, int::out, int::out, time_t::out,
    string::out, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    open_static_file_2(Path::in, Fd::out, Size::out, Mtime::out, Error::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io,
        may_not_duplicate],
"
    struct stat st;

    Fd = -1;
    Size = 0;
    Mtime = 0;

    if (stat(Path, &st) < 0) {
        Error = MR_make_string_const(""stat failed"");
    } else if (! S_ISREG(st.st_mode)) {
        Error = MR_make_string_const(""not regular file"");
    } else {
        Fd = open(Path, O_RDONLY);
        if (Fd < 0) {
            Error = MR_make_string_const(""open failed"");
        } else {
            Size = st.st_size;
            Mtime = st.st_mtime;
        }
    }
").

close_static_file(static_file(Fd, _Size, _Mtime), !IO) :-
    close_static_file_2(Fd, !IO).

:- pred close_static_file_2(int::in, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    close_static_file_2(Fd::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io,
        may_not_duplicate],
"
    close(Fd);
").

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
