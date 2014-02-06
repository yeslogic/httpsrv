:- module buffer.

% Copyright (C) 2014 YesLogic Pty. Ltd.
% All rights reserved.

:- interface.

:- import_module bool.
:- import_module io.

:- type buffer.

:- pred make_buffer(c_pointer::in, int::in, buffer::out, io::di, io::uo)
    is det.

:- pred make_string_utf8(buffer::in, int::in, int::in, bool::out, string::out,
    io::di, io::uo) is det.

:- pred length(buffer::in, int::out, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- pragma foreign_type("C", buffer, "buffer_t *").

:- pragma foreign_decl("C", "
    #include ""buffer1.h""
").

:- pragma foreign_code("C", "
    #include ""buffer1.c""
").

:- pragma foreign_proc("C",
    make_buffer(Ptr::in, Size::in, Buf::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Buf = buffer_new();
    buffer_append(Buf, (const char *) Ptr, Size);
").

:- pragma foreign_proc("C",
    make_string_utf8(Buf::in, StartPos::in, EndPos::in, Ok::out, Str::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Str = make_string(Buf->data, StartPos, EndPos - StartPos);
    if (MR_utf8_verify(Str)) {
        Ok = MR_TRUE;
    } else {
        Ok = MR_FALSE;
        Str = MR_make_string_const("""");
    }
").

:- pragma foreign_proc("C",
    length(Buf::in, Len::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Len = Buf->len;
").

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
