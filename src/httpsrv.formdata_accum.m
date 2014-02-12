:- module httpsrv.formdata_accum.

% Copyright (C) 2014 YesLogic Pty. Ltd.
% All rights reserved.

:- interface.

:- type formdata_accum.

:- func init = formdata_accum.

:- func get_parts(formdata_accum) = assoc_list(string, formdata).

:- instance multipart_parser.callbacks(formdata_accum).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module require.

:- import_module buffer.

:- type formdata_accum
    --->    formdata_accum(
                % Completed form-data parts in reverse order.
                rev_parts       :: assoc_list(string, formdata),

                % Accumulate the current form-data.
                cur_name        :: string,
                cur_part        :: formdata
            ).

%-----------------------------------------------------------------------------%

init = formdata_accum([], "", formdata_init).

:- func formdata_init = formdata.

formdata_init = formdata("", no, "", no, []).

%-----------------------------------------------------------------------------%

get_parts(Acc) = Parts :-
    Acc = formdata_accum(RevParts, CurName, CurPart),
    list.reverse(RevParts, Parts),
    % Sanity check.
    expect(CurName `unify` "", $module, $pred, "incomplete part"),
    expect(CurPart `unify` formdata_init, $module, $pred, "incomplete part").

%-----------------------------------------------------------------------------%

:- instance multipart_parser.callbacks(formdata_accum) where [
    pred(on_headers_complete/4) is formdata_accum.on_headers_complete,
    pred(on_body_chunk/6)       is formdata_accum.on_body_chunk,
    pred(on_part_end/2)         is formdata_accum.on_part_end
].

:- pred on_headers_complete(on_headers_complete_info::in, maybe_error::out,
    formdata_accum::in, formdata_accum::out) is det.

on_headers_complete(Info, Res, !Acc) :-
    Info ^ content_disposition_type = MaybeDispositionType,
    Info ^ content_disposition_name = MaybeName,
    Info ^ content_disposition_filename = MaybeFileName,
    Info ^ content_type_media_type = MediaType,
    Info ^ content_transfer_encoding_mechanism = MaybeCTE,

    (
        MaybeDispositionType = yes(DispositionType),
        DispositionType = form_data
    ->
        !Acc ^ cur_part ^ disposition := DispositionType,
        (
            MaybeName = yes(Name),
            !Acc ^ cur_name := Name
        ;
            MaybeName = no
        ),
        (
            MaybeFileName = yes(FileName),
            !Acc ^ cur_part ^ filename := yes(FileName)
        ;
            MaybeFileName = no
        ),
        !Acc ^ cur_part ^ media_type := MediaType,
        (
            MaybeCTE = yes(CTE),
            !Acc ^ cur_part ^ content_transfer_encoding := yes(CTE)
        ;
            MaybeCTE = no
        ),
        Res = ok
    ;
        Res = error("content disposition is not form-data or failed to parse")
    ).

:- pred on_body_chunk(c_pointer::in, int::in,
    formdata_accum::in, formdata_accum::out, io::di, io::uo) is det.

on_body_chunk(Ptr, Size, !Acc, !IO) :-
    !.Acc ^ cur_part ^ content = RevContent0,
    make_buffer(Ptr, Size, Buf, !IO),
    RevContent = [Buf | RevContent0],
    !Acc ^ cur_part ^ content := RevContent.

:- pred on_part_end(formdata_accum::in, formdata_accum::out) is det.

on_part_end(Acc0, Acc) :-
    Acc0 = formdata_accum(RevParts0, Name, Part0),

    % The content was accumulated in reverse order.
    Part0 ^ content = RevContents,
    list.reverse(RevContents, Contents),
    Part = Part0 ^ content := Contents,

    RevParts = [Name - Part | RevParts0],
    Acc = formdata_accum(RevParts, "", formdata_init).

%-----------------------------------------------------------------------------%

:- func form_data = string.

form_data = "form-data".

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
