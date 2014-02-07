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

:- pred on_headers_complete(headers::in, maybe_error::out,
    formdata_accum::in, formdata_accum::out) is det.

on_headers_complete(Headers, Res, !Acc) :-
    (
        search_content_disposition(Headers, Disposition, Params),
        Disposition = form_data
    ->
        !Acc ^ cur_part ^ disposition := Disposition,
        ( search_parameter(Params, name, Name) ->
            !Acc ^ cur_name := Name
        ;
            true
        ),
        ( search_parameter(Params, filename, FileName) ->
            !Acc ^ cur_part ^ filename := yes(FileName)
        ;
            true
        ),

        get_content_type(Headers, media_type(MediaType), _ContentTypeParams),
        !Acc ^ cur_part ^ media_type := MediaType,

        ( search_content_transfer_encoding(Headers, CTE) ->
            !Acc ^ cur_part ^ content_transfer_encoding := yes(CTE)
        ;
            true
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

:- func name = string.

name = "name".

:- func filename = string.

filename = "filename".

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
