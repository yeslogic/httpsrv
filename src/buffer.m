:- module buffer.

% Copyright (C) 2014 YesLogic Pty. Ltd.
% All rights reserved.

:- interface.

:- import_module io.
:- import_module list.
:- import_module maybe.

%-----------------------------------------------------------------------------%

:- type buffer(T).

:- type buffer == buffer(buffer_rw).

:- type buffer_rw.
:- type buffer_ro.

:- pred make_buffer(c_pointer::in, int::in, buffer(T)::out, io::di, io::uo)
    is det.

:- pred length(buffer(T)::in, int::out, io::di, io::uo) is det.

:- func total_length(list(buffer(buffer_ro))) = int.

:- pred make_string_utf8(buffer(T)::in, int::in, int::in, maybe(string)::out,
    io::di, io::uo) is det.

:- pred buffers_to_string_utf8(list(buffer(buffer_ro))::in, string::out)
    is semidet.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module int.

:- pragma foreign_type("C", buffer(T), "buffer_t *").

:- pragma foreign_decl("C", "
    #include ""buffer1.h""
").

:- pragma foreign_code("C", "
    #include ""buffer1.c""
").

:- type buffer_rw ---> buffer_rw.
:- type buffer_ro ---> buffer_ro.

%-----------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    make_buffer(Ptr::in, Size::in, Buf::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Buf = buffer_new();
    buffer_append(Buf, (const char *) Ptr, Size);
").

%-----------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    length(Buf::in, Len::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Len = Buf->len;
").

%-----------------------------------------------------------------------------%

total_length(Bufs) = total_length(Bufs, 0).

:- func total_length(list(buffer(buffer_ro)), int) = int.

total_length([], Len) = Len.
total_length([Buf | Bufs], Len) = total_length(Bufs, Len + length_ro(Buf)).

:- func length_ro(buffer(buffer_ro)) = int.

:- pragma foreign_proc("C",
    length_ro(Buf::in) = (Len::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Len = Buf->len;
").

%-----------------------------------------------------------------------------%

make_string_utf8(Buf, StartPos, EndPos, MaybeString, !IO) :-
    % This is valid because we have the io state.
    promise_pure (
        ( semipure make_string_utf8(Buf, StartPos, EndPos, String) ->
            MaybeString = yes(String)
        ;
            MaybeString = no
        )
    ).

:- semipure pred make_string_utf8(buffer(T)::in, int::in, int::in, string::out)
    is semidet.

:- pragma foreign_proc("C",
    make_string_utf8(Buf::in, StartPos::in, EndPos::in, Str::out),
    [will_not_call_mercury, promise_semipure, thread_safe],
"
    MR_bool valid;
    Str = make_string_utf8(Buf->data, StartPos, EndPos - StartPos, &valid);
    SUCCESS_INDICATOR = valid;
").

%-----------------------------------------------------------------------------%

buffers_to_string_utf8(Bufs, String) :-
    buffers_to_string_utf8(Bufs, total_length(Bufs), String).

:- pred buffers_to_string_utf8(list(buffer(buffer_ro))::in, int::in,
    string::out) is semidet.

:- pragma foreign_proc("C",
    buffers_to_string_utf8(Bufs::in, TotalLen::in, Str::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    MR_bool valid;
    Str = buffers_to_string_utf8(Bufs, TotalLen, &valid);
    SUCCESS_INDICATOR = valid;
").

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
