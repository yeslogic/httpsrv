:- module buffer.

% Copyright (C) 2014, 2018 YesLogic Pty. Ltd.
% All rights reserved.

:- interface.

:- include_module buffer.disk.

:- import_module io.
:- import_module list.
:- import_module maybe.

:- import_module slice.

%-----------------------------------------------------------------------------%

:- type buffer(T).

:- type buffer == buffer(buffer_rw).

:- type buffer_rw.
:- type buffer_ro.

:- pred make_buffer(c_pointer::in, int::in, buffer(T)::out, io::di, io::uo)
    is det.

:- pred length(buffer(T)::in, int::out, io::di, io::uo) is det.

:- func total_length(list(buffer(buffer_ro))) = int.

:- pred buffers_to_slice(list(buffer(buffer_ro))::in, slice::out) is det.

:- pred make_string_utf8(buffer(T)::in, int::in, int::in, maybe(string)::out,
    io::di, io::uo) is det.

:- pred buffers_to_string_utf8(list(buffer(buffer_ro))::in, string::out)
    is semidet.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module int.

:- pragma foreign_type("C", buffer(T), "buffer_t *").

:- pragma foreign_decl("C", include_file("buffer1.h")).
:- pragma foreign_code("C", include_file("buffer1.c")).

:- type buffer_rw ---> buffer_rw.
:- type buffer_ro ---> buffer_ro.

%-----------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    make_buffer(Ptr::in, Size::in, Buf::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Buf = buffer_new(MR_ALLOC_ID);
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

buffers_to_slice(Bufs, Slice) :-
    (
        Bufs = [],
        Slice = slice(null, 0)
    ;
        Bufs = [Buf],
        Slice = slice(base_ro(Buf), length_ro(Buf))
    ;
        Bufs = [_, _ | _],
        Length = total_length(Bufs),
        buffers_join(Bufs, Length, Base),
        Slice = slice(Base, Length)
    ).

:- pred buffers_join(list(buffer(buffer_ro))::in, int::in, c_pointer::out)
    is det.

:- pragma foreign_proc("C",
    buffers_join(Bufs::in, TotalLen::in, Base::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Base = (MR_Word) buffers_join(Bufs, TotalLen, MR_ALLOC_ID);
").

:- func base_ro(buffer(buffer_ro)) = c_pointer.

:- pragma foreign_proc("C",
    base_ro(Buf::in) = (Base::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Base = (MR_Word) Buf->data;
").

:- func null = c_pointer.

:- pragma foreign_proc("C",
    null = (Ptr::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Ptr = 0;
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
    Str = make_string_utf8(Buf->data, StartPos, EndPos - StartPos, &valid,
        MR_ALLOC_ID);
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
    Str = buffers_to_string_utf8(Bufs, TotalLen, &valid, MR_ALLOC_ID);
    SUCCESS_INDICATOR = valid;
").

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
