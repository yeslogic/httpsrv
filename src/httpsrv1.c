/*
** Copyright (C) 2014 YesLogic Pty. Ltd.
** All rights reserved.
*/

#include "httpsrv1.h"

/*
** Constants
*/

#define ID(a, b, c, d) \
    ((d) | ((c)<<8) | ((b)<<16) | ((a)<<24))

enum {
    DAEMON_MAGIC = ID('S','C','R','Y'), /* oOoOoo... */
    CLIENT_MAGIC = ID('C','L','N','T')
};

enum {
    /* XXX revisit these */
    READ_START_TIMEOUT  = 5000, /* milliseconds */
    READ_CONT_TIMEOUT   = 5000, /* milliseconds */
    KEEPALIVE_TIMEOUT   = 5000  /* milliseconds */
};

/*
** Debugging
*/

#define DISABLE if(0)
#define LOG logger

static void
logger(const char *fmt, ...)
{
    va_list ap;
    va_start(ap, fmt);
    vfprintf(stderr, fmt, ap);
    va_end(ap);
}

/*
** Casting functions
*/

static uv_handle_t *
handle_from_stream(uv_stream_t *x)
{
    return (uv_handle_t *) x;
}

static uv_stream_t *
stream_from_tcp(uv_tcp_t *x)
{
    return (uv_stream_t *) x;
}

static uv_handle_t *
handle_from_tcp(uv_tcp_t *x)
{
    return handle_from_stream(stream_from_tcp(x));
}

static uv_handle_t *
handle_from_async(uv_async_t *x)
{
    return (uv_handle_t *) x;
}

static uv_tcp_t *
tcp_from_handle_checked(uv_handle_t *handle)
{
    assert(handle->type == UV_TCP);
    return (uv_tcp_t *) handle;
}

static uv_async_t *
async_from_handle_checked(uv_handle_t *handle)
{
    assert(handle->type == UV_ASYNC);
    return (uv_async_t *) handle;
}

static daemon_t *
daemon_from_handle_data(uv_handle_t *x)
{
    daemon_t *daemon = x->data;
    assert(daemon && daemon->magic == DAEMON_MAGIC);
    return daemon;
}

static daemon_t *
daemon_from_stream_data(uv_stream_t *x)
{
    daemon_t *daemon = x->data;
    assert(daemon && daemon->magic == DAEMON_MAGIC);
    return daemon;
}

static uv_stream_t *
client_tcp_stream(client_t *x)
{
    return stream_from_tcp(&x->tcp);
}

static client_t *
client_from_handle_data(uv_handle_t *x)
{
    client_t *client = x->data;
    assert(client && client->magic == CLIENT_MAGIC);
    return client;
}

static client_t *
client_from_stream_data(uv_stream_t *x)
{
    client_t *client = x->data;
    assert(client && client->magic == CLIENT_MAGIC);
    return client;
}

static client_t *
client_from_parser_data(http_parser *x)
{
    client_t *client = x->data;
    assert(client && client->magic == CLIENT_MAGIC);
    return client;
}

static client_t *
client_from_async_data(uv_async_t *x)
{
    client_t *client = x->data;
    assert(client && client->magic == CLIENT_MAGIC);
    return client;
}

static client_t *
client_from_timer_data(uv_timer_t *x)
{
    client_t *client = x->data;
    assert(client && client->magic == CLIENT_MAGIC);
    return client;
}

/*
** Utilities
*/

static bool
http11_or_greater(http_parser *parser)
{
    return (parser->http_major == 1 && parser->http_minor == 1)
        || (parser->http_major > 1);
}

/*
** Daemon
*/

static daemon_t *
daemon_setup(MR_Word request_handler,
    MR_String bind_address,
    MR_Integer port,
    MR_Integer back_log)
{
    daemon_t *daemon;
    int r;

    daemon = MR_GC_NEW(daemon_t);
    memset(daemon, 0, sizeof(daemon_t));

    daemon->magic = DAEMON_MAGIC;
    daemon->loop = uv_loop_new();

    r = uv_tcp_init(daemon->loop, &daemon->server);
    daemon->server.data = daemon; /* for daemon_cleanup */
    if (r != 0) {
        LOG("[srv] tcp init error=%d\n", r);
        daemon_cleanup(daemon);
        return NULL;
    }

    r = uv_tcp_bind(&daemon->server, uv_ip4_addr(bind_address, port));
    if (r != 0) {
        LOG("[srv] tcp bind error=%d\n", r);
        daemon_cleanup(daemon);
        return NULL;
    }

    daemon->parser_settings.on_message_begin = client_on_message_begin;
    daemon->parser_settings.on_url = client_on_url;
    daemon->parser_settings.on_header_field = client_on_header_field;
    daemon->parser_settings.on_header_value = client_on_header_value;
    daemon->parser_settings.on_headers_complete = client_on_headers_complete;
    daemon->parser_settings.on_body = client_on_body;
    daemon->parser_settings.on_message_complete = client_on_message_complete;
    daemon->request_handler = request_handler;
    daemon->next_client_id = 1;

    r = uv_listen(stream_from_tcp(&daemon->server), back_log,
        server_on_connect);
    if (r != 0) {
        LOG("[srv] listen error=%d\n", r);
        daemon_cleanup(daemon);
        return NULL;
    }

    return daemon;
}

static void
daemon_cleanup(daemon_t *daemon)
{
    LOG("[srv] cleanup\n");

    uv_close(handle_from_tcp(&daemon->server), NULL);
    uv_run(daemon->loop, UV_RUN_DEFAULT);
    uv_loop_delete(daemon->loop);

    MR_GC_free(daemon);
}

/*
** Callbacks
*/

static void
server_on_connect(uv_stream_t *server_handle, int status)
{
    daemon_t *daemon = daemon_from_stream_data(server_handle);
    client_t *client;
    int r;

    LOG("[srv] server_on_connect: status=%d\n", status);
    if (status != 0)
        return;

    /*
    ** Client structures must be allocated uncollectable as they will be become
    ** unreachable from Mercury code near the end of their lifetimes.
    */
    client = MR_GC_NEW_UNCOLLECTABLE(client_t);
    memset(client, 0, sizeof(client_t));

    client->magic = CLIENT_MAGIC;
    client->id = daemon->next_client_id++;
    client->request_count = 0;
    client->daemon = daemon;

    uv_tcp_init(server_handle->loop, &client->tcp);
    uv_async_init(server_handle->loop, &client->async, client_on_async); 
    uv_timer_init(server_handle->loop, &client->timer);
    http_parser_init(&client->parser, HTTP_REQUEST);

    client->tcp.data = client;
    client->async.data = client;
    client->timer.data = client;
    client->parser.data = client;

    /* See also client_on_message_begin */
    buffer_init(&client->read_buf);
    buffer_init(&client->request_acc.url_buf);
    buffer_init(&client->request_acc.header_field_buf);
    buffer_init(&client->request_acc.header_value_buf);
    client->request_acc.last_header_cb = NONE;
    buffer_init(&client->request_acc.body_buf);
    client->request = request_init();
    client->should_keep_alive = false;
    client->deferred_on_message_complete = false;

    client->response_state = IDLE;
    client->response_bufs = NULL;
    client->response_bufs_length = 0;

    r = uv_accept(server_handle, client_tcp_stream(client));
    if (r != 0) {
        LOG("[%d:%d] accept failed\n", client->id, client->request_count);
        client_close(client);
        return;
    }

    LOG("[%d:%d] new client\n", client->id, client->request_count);

    client_enable_read(client, client_on_read_timeout, READ_START_TIMEOUT);
}

static void
client_enable_read(client_t *client, uv_timer_cb timeout_cb, int64_t timeout)
{
    uv_loop_t *loop = client->timer.loop;

    uv_update_time(loop);
    uv_timer_start(&client->timer, timeout_cb, timeout, 0);

    LOG("[%d:%d] read start: now=%ld, timeout=%ld\n",
        client->id, client->request_count, uv_now(loop), timeout);

    uv_read_start(client_tcp_stream(client), client_on_alloc, client_on_read);
}

static void
client_disable_read_and_stop_timer(client_t *client)
{
    LOG("[%d:%d] read stop\n", client->id, client->request_count);

    uv_read_stop(client_tcp_stream(client));

    /* Disable the read timeout as well. */
    uv_timer_stop(&client->timer);
}

static uv_buf_t
client_on_alloc(uv_handle_t *handle, size_t suggested_size)
{
    uv_stream_t *stream = stream_from_tcp(tcp_from_handle_checked(handle));
    client_t *client = client_from_stream_data(stream);
    char *ptr;

#if 1 /* pathological */
    suggested_size = 1;
#endif

    DISABLE LOG("[%d:%d] alloc: %ld bytes\n",
        client->id, client->request_count, suggested_size);

    ptr = buffer_reserve(&client->read_buf, suggested_size);

    return uv_buf_init(ptr, suggested_size);
}

static void
client_on_read(uv_stream_t *tcp, ssize_t nread, uv_buf_t buf)
{
    client_t *client = client_from_stream_data(tcp);

    DISABLE LOG("[%d:%d] on_read: nread=%d\n",
        client->id, client->request_count, nread);

    if (nread > 0) {
        buffer_advance(&client->read_buf, nread);

        /* XXX still susceptible to attack */
        uv_update_time(client->timer.loop);
        uv_timer_start(&client->timer, client_on_read_timeout,
            READ_CONT_TIMEOUT, 0);

        DISABLE LOG("[%d:%d] on_read: deferred read timeout, now=%d\n",
            client->id, client->request_count,
            uv_now(client->timer.loop));
    }

    if (nread < 0) {
        uv_err_t err = uv_last_error(client->daemon->loop);
        LOG("[%d:%d] read error: %s\n",
            client->id, client->request_count,
            uv_strerror(err));
        client_close(client);
        return;
    }

    if (client->read_buf.len > 0) {
        size_t parsed;
        enum http_errno errnum;

        DISABLE LOG("[%d:%d] parsing: nread=%d, read_buf.len=%d\n",
            client->id, client->request_count,
            nread, client->read_buf.len);

        parsed = http_parser_execute(&client->parser,
            &client->daemon->parser_settings,
            client->read_buf.data, (size_t)client->read_buf.len);

        errnum = HTTP_PARSER_ERRNO(&client->parser);
        if (errnum != HPE_OK) {
            LOG("[%d:%d] parse error: %s %s\n",
                client->id, client->request_count,
                http_errno_name(errnum), http_errno_description(errnum));
            client_close(client);
            return;
        }

        buffer_shift(&client->read_buf, parsed);
    }
}

static void
client_on_read_timeout(uv_timer_t *timer, int status)
{
    client_t *client = client_from_timer_data(timer);
    uv_loop_t *loop = timer->loop;

    LOG("[%d:%d] on_read_timeout: now=%ld, status=%d\n",
        client->id, client->request_count,
        uv_now(loop), status);

    client_close(client);
}

static int
client_on_message_begin(http_parser *parser)
{
    client_t *client = client_from_parser_data(parser);

    client->request_count++;
    LOG("[%d:%d] on_message_begin\n", client->id, client->request_count);

    /* Do not clear read_buf; would break pipelining. */
    buffer_clear(&client->request_acc.url_buf);
    buffer_clear(&client->request_acc.header_field_buf);
    buffer_clear(&client->request_acc.header_value_buf);
    client->request_acc.last_header_cb = NONE;
    buffer_clear(&client->request_acc.body_buf);
    client->request = request_init();
    client->should_keep_alive = false;
    client->deferred_on_message_complete = false;

    assert(client->response_state == IDLE);
    assert(client->response_bufs == NULL);
    assert(client->response_bufs_length == 0);

    return 0;
}

static int
client_on_url(http_parser *parser, const char *at, size_t length)
{
    client_t *client = client_from_parser_data(parser);

    DISABLE LOG("[%d:%d] on_url '%s'\n",
        client->id, client->request_count,
        make_string(at, 0, length));

    buffer_append(&client->request_acc.url_buf, at, length);

    return 0;
}

static void
maybe_done_prev_header(client_t *client, bool force_clear)
{
    if (client->request_acc.last_header_cb == WAS_HEADER_VALUE) {
        MR_String field = buffer_to_string(&client->request_acc.header_field_buf);
        MR_String value = buffer_to_string(&client->request_acc.header_value_buf);

        LOG("[%d:%d] header: '%s: %s'\n",
            client->id, client->request_count,
            field, value);

        client->request = request_add_header(client->request, field, value);
        force_clear = true;
    }

    if (force_clear) {
        buffer_clear(&client->request_acc.header_field_buf);
        buffer_clear(&client->request_acc.header_value_buf);
        client->request_acc.last_header_cb = NONE;
    }
}

static int
client_on_header_field(http_parser *parser, const char *at, size_t length)
{
    client_t *client = client_from_parser_data(parser);

    maybe_done_prev_header(client, false);

    DISABLE LOG("[%d:%d] on_header_field\n",
        client->id, client->request_count);

    buffer_append(&client->request_acc.header_field_buf, at, length);
    client->request_acc.last_header_cb = WAS_HEADER_FIELD;

    return 0;
}

static int
client_on_header_value(http_parser *parser, const char *at, size_t length)
{
    client_t *client = client_from_parser_data(parser);

    DISABLE LOG("[%d:%d] on_header_value\n",
        client->id, client->request_count);

    buffer_append(&client->request_acc.header_value_buf, at, length);
    client->request_acc.last_header_cb = WAS_HEADER_VALUE;

    return 0;
}

static int
client_on_headers_complete(http_parser *parser)
{
    client_t *client = client_from_parser_data(parser);
    MR_String method;
    MR_String url;

    /* Pick up the last header, if any */
    maybe_done_prev_header(client, true);

    /* Pick up the method. */
    MR_make_aligned_string(method, http_method_str(client->parser.method));
    client->request = request_set_method(client->request, method);

    /* Pick up the URL. */
    url = buffer_to_string(&client->request_acc.url_buf);
    buffer_clear(&client->request_acc.url_buf);
    client->request = request_set_url(client->request, url);

    client->should_keep_alive = http_should_keep_alive(parser);
    client->deferred_on_message_complete = false;

    LOG("[%d:%d] on_headers_complete: method='%s', url='%s', should_keep_alive=%d\n",
        client->id, client->request_count,
        method, url, client->should_keep_alive);

    if (http11_or_greater(&client->parser)) {
        int expect = request_get_expect_header(client->request);
        if (expect == 0) {
            /* No Expect: header. */
        } else if (expect > 0) {
            /* Expect: 100-continue */
            client_write_100_continue(client);
        } else {
            /* Cannot satisfy expectation. */
            client_write_417_expectation_failed(client);
        }
    }

    return 0;
}

static int
client_on_body(http_parser *parser, const char *at, size_t length)
{
    client_t *client = client_from_parser_data(parser);

    LOG("[%d:%d] on_body: %d bytes\n",
        client->id, client->request_count, length);

    /* XXX limit on body length */
    buffer_append(&client->request_acc.body_buf, at, length);

    return 0;
}

static int
client_on_message_complete(http_parser *parser)
{
    client_t *client = client_from_parser_data(parser);
    MR_String method;

    if (client->response_state != IDLE) {
        /*
        ** This callback may be called after we have started sending the
        ** 100-continue status line but before client_after_write_100_continue.
        ** Set a flag to run this callback later.
        **
        ** This callback may also be called after we have sent the
        ** 417 Expectation Failed code.  Do not continue.
        */
        LOG("[%d:%d] on_message_complete: deferred\n",
            client->id, client->request_count);
        client->deferred_on_message_complete = true;
        return 0;
    }

    LOG("[%d:%d] on_message_complete\n",
        client->id, client->request_count);

    /* Don't read any more while the request handle works. */
    client_disable_read_and_stop_timer(client);

    /* Pick up the body. */
    client_set_request_body(client);
    buffer_init(&client->request_acc.body_buf);

    /* Call the request handler. */
    LOG("[%d:%d] on_message_complete: call request handler\n",
        client->id, client->request_count);

    client->response_state = PREPARING_RESPONSE;
    call_request_handler_pred(client->daemon->request_handler,
        client, client->request);

    return 0;
}

static void
client_write_100_continue(client_t *client)
{
    static char text[] = "HTTP/1.1 100 Continue\r\n\r\n";
    static uv_buf_t bufs[1];
    bufs[0] = uv_buf_init(text, sizeof(text));

    LOG("[%d:%d] write 100-continue\n",
        client->id, client->request_count);

    client_disable_read_and_stop_timer(client);

    /* XXX set write timeout */

    client->response_state = WRITING_100_CONTINUE;
    uv_write(&client->write_req, client_tcp_stream(client),
        bufs, 1, client_after_write_100_continue);
}

static void
client_after_write_100_continue(uv_write_t *req, int status)
{
    client_t *client = client_from_stream_data(req->handle);

    LOG("[%d:%d] after write 100-continue: status=%d\n",
        client->id, client->request_count, status);

    assert(client->response_state == WRITING_100_CONTINUE);
    client->response_state = IDLE;

    if (status != 0) {
        client_close(client);
        return;
    }

    if (client->deferred_on_message_complete) {
        LOG("[%d:%d] calling deferred on_message_complete\n",
            client->id, client->request_count, status);
        client->deferred_on_message_complete = false;
        client_on_message_complete(&client->parser);
    } else {
        client_enable_read(client, client_on_read_timeout, READ_CONT_TIMEOUT);
    }
}

static void
client_write_417_expectation_failed(client_t *client)
{
    static char text[] = "HTTP/1.1 417 Expectation Failed\r\n\r\n";
    static uv_buf_t bufs[1];
    bufs[0] = uv_buf_init(text, sizeof(text));

    LOG("[%d:%d] write 417 Expectation Failed\n",
        client->id, client->request_count);

    client_disable_read_and_stop_timer(client);

    client->should_keep_alive = false;
    client->response_state = WRITING_RESPONSE;

    /* XXX set write timeout */

    uv_write(&client->write_req, client_tcp_stream(client),
        bufs, 1, client_after_write);
}

static void
client_set_request_body(client_t *client)
{
    if (client->request_acc.body_buf.len == 0) {
        /* none */
    } else {
        /* XXX verify UTF-8 */
        client->request = request_set_body_string(client->request,
            buffer_to_string(&client->request_acc.body_buf));
    }
}

static void
client_on_async(uv_async_t *async, int status)
{
    client_t *client = client_from_async_data(async);

    LOG("[%d:%d] on_async: status=%d\n",
        client->id, client->request_count, status);

    /*
    ** uv_async_send causes the callback to be invoked one or more times;
    ** ignore the second and subsequent invocations.
    */
    if (client->response_state != PREPARING_RESPONSE) {
        LOG("[%d:%d] on_async: ignored due to response_state\n",
            client->id, client->request_count);
        return;
    }

    assert(client->response_bufs);

    client->response_state = WRITING_RESPONSE;

    /* XXX set write timeout */

    uv_write(&client->write_req, client_tcp_stream(client),
        client->response_bufs, client->response_bufs_length,
        client_after_write);
}

static void
client_after_write(uv_write_t *req, int status)
{
    client_t *client = client_from_stream_data(req->handle);

    LOG("[%d:%d] after_write: status=%d\n",
        client->id, client->request_count, status);

    assert(client->response_state == WRITING_RESPONSE);
    client->response_state = IDLE;
    client->response_bufs = NULL;
    client->response_bufs_length = 0;

    if (status != 0 || !client->should_keep_alive) {
        client_close(client);
        return;
    }

    LOG("[%d:%d] persisting connection\n",
        client->id, client->request_count);

    client_enable_read(client, client_on_keepalive_timeout, KEEPALIVE_TIMEOUT);
}

static void
client_on_keepalive_timeout(uv_timer_t *timer, int status)
{
    client_t *client = client_from_timer_data(timer);
    uv_loop_t *loop = timer->loop;

    LOG("[%d:%d] on_keepalive_timeout: now=%ld, status=%d\n",
        client->id, client->request_count, uv_now(loop), status);

    client_close(client);
}

static void
client_close(client_t *client)
{
    LOG("[%d:%d] closing\n", client->id, client->request_count);

    client_disable_read_and_stop_timer(client);

    uv_close(handle_from_async(&client->async), client_on_close_1);
}

static void
client_on_close_1(uv_handle_t *handle)
{
    uv_async_t *async = async_from_handle_checked(handle);
    client_t *client = client_from_async_data(async);

    LOG("[%d:%d] on_close_1\n", client->id, client->request_count);

    uv_close(handle_from_stream(client_tcp_stream(client)), client_on_close_2);
}

static void
client_on_close_2(uv_handle_t *handle)
{
    client_t *client = client_from_handle_data(handle);

    LOG("[%d:%d] on_close_2\n", client->id, client->request_count);

    assert(!uv_is_active(handle_from_async(&client->async)));

    MR_GC_free(client); /* allocated uncollectable */
}

/*
** Set response
*/

static void
set_response_bufs(client_t *client,
    MR_Word response_list, MR_Integer response_list_length)
{
    uv_buf_t *response_bufs;
    MR_Integer i;
    MR_String s;

    LOG("[%d:%d] set_response: response_list_length=%d\n",
        client->id, client->request_count,
        response_list_length);

    assert(client->response_bufs == NULL);
    assert(client->response_bufs_length == 0);

    response_bufs = MR_GC_NEW_ARRAY(uv_buf_t, response_list_length);

    for (i = 0; i < response_list_length; i++) {
        assert(! MR_list_is_empty(response_list));
        s = (const MR_String) MR_list_head(response_list);
        response_bufs[i].base = s;
        response_bufs[i].len = strlen(s);
        response_list = MR_list_tail(response_list);
    }

    client->response_bufs = response_bufs;
    client->response_bufs_length = response_list_length;
}

static void
send_async(client_t *client)
{
    /*
    ** We can't call libuv functions off the main thread.
    ** Send an async signal which triggers a callback on the main thread.
    */

    assert(client->response_state == PREPARING_RESPONSE);
    assert(client->response_bufs != NULL);

    uv_async_send(&client->async);
}

/* vim: set sts=4 sw=4 et: */
