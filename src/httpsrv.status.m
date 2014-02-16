:- module httpsrv.status.

% Copyright (C) 2014 YesLogic Pty. Ltd.
% All rights reserved.

:- interface.

:- type status_code
    --->    ok_200
    ;       created_201
    ;       accepted_202
    ;       nonauthoritative_information_203
    ;       no_content_204          % response must not include message-body
    ;       reset_content_205       % response must not include an entity
    ;       partial_content_206
    ;       multiple_choices_300
    ;       moved_permanently_301
    ;       found_302
    ;       see_other_303
    ;       not_modified_304
    ;       temporary_redirect_307
    ;       bad_request_400
    ;       unauthorized_401
    ;       forbidden_403
    ;       not_found_404
    ;       method_not_allowed_405
    ;       not_acceptable_406
    ;       request_timeout_408
    ;       conflict_409
    ;       gone_410
    ;       length_required_411
    ;       precondition_failed_412
    ;       request_entity_too_large_413
    ;       request_uri_too_long_414
    ;       unsupported_media_type_415
    ;       requested_range_not_satisfiable_416
    ;       expectation_failed_417
    ;       unprocessable_entity_422
    ;       internal_server_error_500
    ;       not_implemented_501
    ;       bad_gateway_502
    ;       service_unavailable_503
    ;       gateway_timeout_504
    ;       http_version_not_supported_505
    ;       insufficient_storage_507.

:- func text(status_code) = string.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

text(ok_200) = "200 OK".
text(created_201) = "201 Created".
text(accepted_202) = "202 Accepted".
text(nonauthoritative_information_203) = "203 Non-Authoritative Information".
text(no_content_204) = "204 No Content".
text(reset_content_205) = "205 Reset Content".
text(partial_content_206) = "206 Partial Content".
text(multiple_choices_300) = "300 Multiple Choices".
text(moved_permanently_301) = "301 Moved Permanently".
text(found_302) = "302 Found".
text(see_other_303) = "303 See Other".
text(not_modified_304) = "304 Not Modified".
text(temporary_redirect_307) = "307 Temporary Redirect".
text(bad_request_400) = "400 Bad Request".
text(unauthorized_401) = "401 Unauthorized".
text(forbidden_403) = "403 Forbidden".
text(not_found_404) = "404 Not Found".
text(method_not_allowed_405) = "405 Method Not Allowed".
text(not_acceptable_406) = "406 Not Acceptable".
text(request_timeout_408) = "408 Request Timeout".
text(conflict_409) = "409 Conflict".
text(gone_410) = "410 Gone".
text(length_required_411) = "411 Length Required".
text(precondition_failed_412) = "412 Precondition Failed".
text(request_entity_too_large_413) = "413 Request Entity Too Large".
text(request_uri_too_long_414) = "414 Request-URI Too Long".
text(unsupported_media_type_415) = "415 Unsupported Media Type".
text(requested_range_not_satisfiable_416) = "416 Requested Range Not Satisfiable".
text(expectation_failed_417) = "417 Expectation Failed".
text(unprocessable_entity_422) = "422 Unprocessable Entity".
text(internal_server_error_500) = "500 Internal Server Error".
text(not_implemented_501) = "501 Not Implemented".
text(bad_gateway_502) = "502 Bad Gateway".
text(service_unavailable_503) = "503 Service Unavailable".
text(gateway_timeout_504) = "504 Gateway Timeout".
text(http_version_not_supported_505) = "505 HTTP Version Not Supported".
text(insufficient_storage_507) = "507 Insufficient Storage".

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
