/*
** Copyright (C) 2014 YesLogic Pty. Ltd.
** All rights reserved.
*/

#ifndef __included_httpsrv1_h
#define __included_httpsrv1_h

#include <stdbool.h>

struct daemon {
    unsigned magic;             /* DAEMON_MAGIC */
    uv_loop_t *loop;
    uv_tcp_t server;
    http_parser_settings parser_settings;
    MR_Word request_handler;
    unsigned next_client_id;    /* for debugging */
};

enum last_header_cb {
    NONE,
    WAS_HEADER_FIELD,
    WAS_HEADER_VALUE
};

enum response_state {
    IDLE,
    PREPARING_RESPONSE,
    WRITING_RESPONSE
};

struct client {
    unsigned magic;             /* CLIENT_MAGIC */
    unsigned id;                /* for debugging */
    unsigned request_count;     /* for debugging */
    daemon_t *daemon;

    uv_tcp_t tcp;
    uv_async_t async;
    uv_timer_t timer;
    http_parser parser;

    buffer_t read_buf;
    struct {
        buffer_t url_buf;
        buffer_t header_field_buf;
        buffer_t header_value_buf;
        enum last_header_cb last_header_cb;
    } request_acc;
    MR_Word request;
    bool should_keep_alive;

    enum response_state response_state;
    uv_buf_t *response_bufs;    /* NULL or array */
    unsigned int response_bufs_length;
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
server_on_connect(uv_stream_t *server_handle, int status);

static void
client_enable_read(client_t *client, uv_timer_cb timeout_cb, int64_t timeout);

static void
client_disable_read_and_stop_timer(client_t *client);

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

static int
client_on_body(http_parser *parser, const char *at, size_t length);

static int
client_on_message_complete(http_parser *parser);

static void
client_on_async(uv_async_t *async, int status);

static void
client_after_write(uv_write_t *req, int status);

static void
client_on_keepalive_timeout(uv_timer_t *timer, int status);

static void
client_close(client_t *client);

static void
client_on_close_1(uv_handle_t *handle);

static void
client_on_close_2(uv_handle_t *handle);

static void
set_response_bufs(client_t *client,
    MR_Word response_list, MR_Integer response_list_length);

static void
send_async(client_t *client);

#endif

/* vim: set sts=4 sw=4 et: */
