:- module buffer.disk.

% Copyright (C) 2014 YesLogic Pty. Ltd.
% All rights reserved.

:- interface.

:- pred write_to_file(string::in, list(buffer(T))::in, maybe_error::out,
    io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module bool.

:- type file.

:- pragma foreign_type("C", file, "FILE *").

%-----------------------------------------------------------------------------%

write_to_file(Path, Bufs, Res, !IO) :-
    fopen(Path, File, OpenOk, !IO),
    (
        OpenOk = yes,
        fwrite_bufs(File, Bufs, WriteOk, !IO),
        (
            WriteOk = yes,
            fclose(File, CloseOk, !IO),
            (
                CloseOk = yes,
                Res = ok
            ;
                CloseOk = no,
                io.remove_file(Path, _, !IO),
                Res = error("error closing file")
            )
        ;
            WriteOk = no,
            io.remove_file(Path, _, !IO),
            Res = error("error writing file")
        )
    ;
        OpenOk = no,
        Res = error("error opening file")
    ).

:- pred fopen(string::in, file::out, bool::out, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    fopen(Path::in, Fp::out, Ok::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io,
        may_not_duplicate],
"
    Fp = fopen(Path, ""wb"");
    Ok = (Fp != NULL) ? MR_TRUE : MR_FALSE;
").

:- pred fclose(file::in, bool::out, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    fclose(Fp::in, Ok::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io,
        may_not_duplicate],
"
    Ok = (fclose(Fp) == 0) ? MR_TRUE : MR_FALSE;
").

:- pred fwrite_bufs(file::in, list(buffer(T))::in, bool::out, io::di, io::uo)
    is det.

fwrite_bufs(_File, [], yes, !IO).
fwrite_bufs(File, [Buf | Bufs], Res, !IO) :-
    fwrite_buf(File, Buf, Res0, !IO),
    (
        Res0 = yes,
        fwrite_bufs(File, Bufs, Res, !IO)
    ;
        Res0 = no,
        Res = no
    ).

:- pred fwrite_buf(file::in, buffer(T)::in, bool::out, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    fwrite_buf(File::in, Buf::in, Res::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io,
        may_not_duplicate],
"
    size_t nwritten = fwrite(Buf->data, 1, Buf->len, File);
    Res = (nwritten == Buf->len);
").

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
