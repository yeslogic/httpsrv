/*
** Copyright (C) 2014 YesLogic Pty. Ltd.
** All rights reserved.
*/

#ifndef __included_httpsrv1_h
#define __included_httpsrv1_h

#include <stdbool.h>

#include "uv.h"
#include "http_parser.h"

enum daemon_state {
    DAEMON_STARTING = 1,
    DAEMON_RUNNING,
    DAEMON_STOPPING
};

struct daemon {
    unsigned magic;             /* DAEMON_MAGIC */
    enum daemon_state state;
    uv_loop_t *loop;
    uv_signal_t signal1;
    uv_signal_t signal2;
    uv_tcp_t server;
    http_parser_settings parser_settings;
    MR_Word request_handler;
    struct periodic *periodics;
    client_t *clients;
    unsigned next_client_id;    /* for debugging */
};

struct periodic {
    unsigned magic;             /* PERIODIC_MAGIC */
    uv_timer_t timer;
    MR_Word handler;
    struct periodic *next;
};

enum last_header_cb {
    NONE,
    WAS_HEADER_FIELD,
    WAS_HEADER_VALUE
};

enum client_state {
    IDLE = 1,                   /* not yet reading a request */
    READING_REQUEST_HEADER,     /* in request header */
    WRITING_CONTINUE_STATUS_LINE, /* writing 100 Continue (if needed) */
    READING_REQUEST_BODY,       /* in request body */
    PREPARING_RESPONSE,         /* waiting for user program response */
    WRITING_RESPONSE,           /* writing out user program response */
    CLOSING                     /* closing */
};

enum error_status {
    NO_ERROR_YET,
    BAD_REQUEST_400 = 400,
    EXPECTATION_FAILED_417 = 417
};

struct client {
    unsigned magic;             /* CLIENT_MAGIC */
    unsigned id;                /* for debugging */
    unsigned request_count;     /* for debugging */
    daemon_t *daemon;
    client_t *next;

    uv_tcp_t tcp;
    uv_async_t async;
    uv_timer_t timer;
    http_parser parser;

    enum client_state state;
    enum error_status error_detected;
    MR_Word request;

    buffer_t read_buf;
    struct {
        buffer_t request_uri_buf;
        buffer_t header_field_buf;
        buffer_t header_value_buf;
        enum last_header_cb last_header_cb;
        buffer_t body_buf;
        MR_Word multipart_parser; /* 0 or pointer */
    } request_acc;

    uv_buf_t *response_bufs; /* NULL or array */
    size_t response_bufs_length;
    uv_file response_file; /* -1 for none */
    size_t response_file_size;
    uv_fs_t response_file_req;
    buffer_t response_file_buf;
    uv_write_t write_req;
};

static daemon_t *
daemon_setup(MR_Word request_handler,
    MR_String bind_address,
    MR_Integer port,
    MR_Integer back_log);

static void
daemon_cleanup(daemon_t *daemon);

static void
daemon_add_periodic(daemon_t *daemon, MR_Integer milliseconds,
    MR_Word periodic_handler);

static void
daemon_cleanup_periodics(daemon_t *daemon);

static void
daemon_on_signal(uv_signal_t *signal, int status);

static void
daemon_on_periodic_timer(uv_timer_t *timer, int status);

static void
server_on_connect(uv_stream_t *server_handle, int status);

static void
client_resume_read(client_t *client, uv_timer_cb timeout_cb, int64_t timeout);

static void
client_pause_read(client_t *client);

static uv_buf_t
client_on_alloc(uv_handle_t *client, size_t suggested_size);

static void
client_on_read(uv_stream_t *tcp, ssize_t nread, uv_buf_t buf);

static void
client_on_read_timeout(uv_timer_t *timer, int status);

static int
client_on_message_begin(http_parser *parser);

static int
client_on_url(http_parser *parser, const char *at, size_t length);

static int
client_on_header_field(http_parser *parser, const char *at, size_t length);

static int
client_on_header_value(http_parser *parser, const char *at, size_t length);

static int
client_on_headers_complete(http_parser *parser);

static void
client_maybe_start_formdata_parser(client_t *client);

static int
client_on_body(http_parser *parser, const char *at, size_t length);

static int
client_on_message_complete(http_parser *parser);

static void
client_maybe_write_continue_status_line(client_t *client, bool really);

static void
client_after_write_continue_status_line(uv_write_t *req, int status);

static void
client_write_400_bad_request(client_t *client);

static void
client_write_417_expectation_failed(client_t *client);

static void
client_write_error_response(client_t *client, const char *response,
    size_t responselen);

static bool
client_set_request_body(client_t *client);

static void
client_on_async(uv_async_t *async, int status);

static void
client_on_write_timeout(uv_timer_t *timer, int status);

static void
client_after_write_response_bufs(uv_write_t *req, int status);

static void
client_start_response_file(client_t *client);

static void
client_on_read_response_file_buf(uv_fs_t *req);

static void
client_after_write_response_file_buf(uv_write_t *req, int status);

static void
client_after_response_file(client_t *client, int status);

static void
client_close_response_file(client_t *client);

static void
client_after_full_response(client_t *client, int status);

static void
client_on_keepalive_timeout(uv_timer_t *timer, int status);

static void
client_close(client_t *client, int line);

static void
client_on_close_1(uv_handle_t *handle);

static void
client_on_close_2(uv_handle_t *handle);

static void
client_on_close_3(uv_handle_t *handle);

static void
client_unlink(client_t *client);

static MR_String
client_address_ipv4(client_t *client, MR_AllocSiteInfoPtr alloc_id);

void
_httpsrv_set_response_bufs(client_t *client,
    MR_Word response_list, MR_Integer response_list_length,
    MR_Integer response_file_fd, MR_Integer response_file_size);

void
_httpsrv_send_async(client_t *client);

bool
_httpsrv_client_should_keep_alive(client_t *client);

#endif

/* vim: set sts=4 sw=4 et: */
