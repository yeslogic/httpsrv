/*
** Copyright (C) 2014 YesLogic Pty. Ltd.
** All rights reserved.
*/

#include "httpsrv1.h"

#include "httpsrv.request.mh"

/*
** Constants
*/

#define ID(a, b, c, d) \
    ((d) | ((c)<<8) | ((b)<<16) | ((a)<<24))

enum {
    DAEMON_MAGIC = ID('S','C','R','Y'), /* oOoOoo... */
    CLIENT_MAGIC = ID('C','L','N','T'),
    PERIODIC_MAGIC = ID('P','R','D','C')
};

enum {
    /* XXX revisit these */
    /*
    ** We set a relatively large WRITE_TIMEOUT because the browser may prompt
    ** the user to choose a path to save to before reading any more data.
    */
    READ_START_TIMEOUT  = 5000, /* milliseconds */
    READ_CONT_TIMEOUT   = 5000, /* milliseconds */
    WRITE_TIMEOUT       = 60000, /* milliseconds */
    KEEPALIVE_TIMEOUT   = 5000, /* milliseconds */
    RESPONSE_FILE_BUFSIZE = 65536 /* bytes */
};

/*
** Debugging
*/

#define LOG_LEVEL   (6)
#define LOG_ERROR   if (LOG_LEVEL >= 3) logger
#define LOG_WARN    if (LOG_LEVEL >= 4) logger
#define LOG_NOTICE  if (LOG_LEVEL >= 5) logger
#define LOG_INFO    if (LOG_LEVEL >= 6) logger
#define LOG_DEBUG   if (LOG_LEVEL >= 7) logger

static void
logger(const char *fmt, ...)
{
    va_list ap;
    va_start(ap, fmt);
    fprintf(stderr, "httpsrv ");
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

static uv_handle_t *
handle_from_timer(uv_timer_t *x)
{
    return (uv_handle_t *) x;
}

static uv_handle_t *
handle_from_signal(uv_signal_t *x)
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

static uv_timer_t *
timer_from_handle_checked(uv_handle_t *handle)
{
    assert(handle->type == UV_TIMER);
    return (uv_timer_t *) handle;
}

static daemon_t *
daemon_from_handle_data(uv_handle_t *x)
{
    daemon_t *dmn = x->data;
    assert(dmn && dmn->magic == DAEMON_MAGIC);
    return dmn;
}

static daemon_t *
daemon_from_stream_data(uv_stream_t *x)
{
    daemon_t *dmn = x->data;
    assert(dmn && dmn->magic == DAEMON_MAGIC);
    return dmn;
}

static daemon_t *
daemon_from_signal_data(uv_signal_t *x)
{
    daemon_t *dmn = x->data;
    assert(dmn && dmn->magic == DAEMON_MAGIC);
    return dmn;
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

static client_t *
client_from_fs(uv_fs_t *x)
{
    client_t *client = x->data;
    assert(client && client->magic == CLIENT_MAGIC);
    return client;
}

static struct periodic *
periodic_from_handle_data(uv_handle_t *x)
{
    struct periodic *periodic = x->data;
    assert(periodic && periodic->magic == PERIODIC_MAGIC);
    return periodic;
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
    MR_Integer back_log,
    MR_Integer max_body,
    MR_String *error_message)
{
    daemon_t *dmn;
    int r;

    dmn = MR_GC_NEW(daemon_t);
    memset(dmn, 0, sizeof(daemon_t));

    dmn->magic = DAEMON_MAGIC;
    dmn->state = DAEMON_STARTING;
    dmn->loop = uv_loop_new();

    dmn->signal1.data = dmn; /* for daemon_cleanup */
    dmn->signal2.data = dmn; /* for daemon_cleanup */
    dmn->server.data = dmn; /* for daemon_cleanup */

    uv_signal_init(dmn->loop, &dmn->signal1);
    uv_signal_init(dmn->loop, &dmn->signal2);
    uv_signal_start(&dmn->signal1, daemon_on_signal, SIGINT);
    uv_signal_start(&dmn->signal2, daemon_on_signal, SIGTERM);

    r = uv_tcp_init(dmn->loop, &dmn->server);
    if (r != 0) {
        *error_message = MR_make_string(MR_ALLOC_SITE_NONE,
            "%s", uv_strerror(uv_last_error(dmn->loop)));
        LOG_ERROR("[srv] tcp init error=%d (%s)\n", r, *error_message);
        daemon_cleanup(dmn);
        return NULL;
    }

    r = uv_tcp_bind(&dmn->server, uv_ip4_addr(bind_address, port));
    if (r != 0) {
        *error_message = MR_make_string(MR_ALLOC_SITE_NONE,
            "%s", uv_strerror(uv_last_error(dmn->loop)));
        LOG_ERROR("[srv] tcp bind error=%d (%s)\n", r, *error_message);
        daemon_cleanup(dmn);
        return NULL;
    }

    dmn->parser_settings.on_message_begin = client_on_message_begin;
    dmn->parser_settings.on_url = client_on_url;
    dmn->parser_settings.on_header_field = client_on_header_field;
    dmn->parser_settings.on_header_value = client_on_header_value;
    dmn->parser_settings.on_headers_complete = client_on_headers_complete;
    dmn->parser_settings.on_body = client_on_body;
    dmn->parser_settings.on_message_complete = client_on_message_complete;
    dmn->max_body = max_body;
    dmn->request_handler = request_handler;
    dmn->periodics = NULL;
    dmn->clients = NULL;
    dmn->next_client_id = 1;

    r = uv_listen(stream_from_tcp(&dmn->server), back_log,
        server_on_connect);
    if (r != 0) {
        *error_message = MR_make_string(MR_ALLOC_SITE_NONE,
            "%s", uv_strerror(uv_last_error(dmn->loop)));
        LOG_ERROR("[srv] listen error=%d (%s)\n", r, *error_message);
        daemon_cleanup(dmn);
        return NULL;
    }

    dmn->state = DAEMON_RUNNING;

    *error_message = MR_make_string_const("");
    return dmn;
}

static void
daemon_cleanup(daemon_t *dmn)
{
    LOG_INFO("[srv] cleanup\n");

    uv_close(handle_from_signal(&dmn->signal1), NULL);
    uv_close(handle_from_signal(&dmn->signal2), NULL);
    if (dmn->state != DAEMON_STOPPING) {
        uv_close(handle_from_tcp(&dmn->server), NULL);
    }

    daemon_cleanup_periodics(dmn);

    uv_run(dmn->loop, UV_RUN_DEFAULT);

    assert(dmn->clients == NULL);

    /* uv_print_all_handles(dmn->loop); */

    uv_loop_delete(dmn->loop);

    MR_GC_free(dmn);
}

static void
daemon_add_periodic(daemon_t *dmn, MR_Integer milliseconds,
    MR_Word periodic_handler)
{
    struct periodic *periodic;

    periodic = MR_GC_NEW(struct periodic);
    periodic->magic = PERIODIC_MAGIC;
    uv_timer_init(dmn->loop, &periodic->timer);
    periodic->timer.data = periodic;
    periodic->handler = periodic_handler;
    periodic->next = dmn->periodics;
    dmn->periodics = periodic;

    uv_update_time(dmn->loop);
    uv_timer_start(&periodic->timer, daemon_on_periodic_timer,
        milliseconds, milliseconds);

    LOG_INFO("[srv] add periodic: now=%ld, milliseconds=%ld\n",
        uv_now(dmn->loop), milliseconds);
}

static void
daemon_cleanup_periodics(daemon_t *dmn)
{
    struct periodic *periodic;

    while ((periodic = dmn->periodics)) {
        uv_close(handle_from_timer(&periodic->timer), NULL);
        dmn->periodics = periodic->next;
    }

    assert(dmn->periodics == NULL);
}

/*
** Client
*/

static client_t *
make_client(daemon_t *dmn)
{
    uv_loop_t *loop = dmn->loop;
    client_t *client;

    /*
    ** Client structures must be allocated uncollectable as they will be become
    ** unreachable from Mercury code near the end of their lifetimes.
    */
    client = MR_GC_NEW_UNCOLLECTABLE(client_t);
    memset(client, 0, sizeof(client_t));

    client->magic = CLIENT_MAGIC;
    client->id = dmn->next_client_id++;
    client->request_count = 0;
    client->daemon = dmn;
    /* link */
    client->next = dmn->clients;
    dmn->clients = client;

    client->state = IDLE;
    client->deferred_error = NO_ERROR_YET;
    client->request = request_init(client);

    uv_tcp_init(loop, &client->tcp);
    uv_async_init(loop, &client->async, client_on_async);
    uv_timer_init(loop, &client->timer);
    http_parser_init(&client->parser, HTTP_REQUEST);

    client->tcp.data = client;
    client->async.data = client;
    client->timer.data = client;
    client->parser.data = client;

    /* See also client_on_message_begin */
    buffer_init(&client->read_buf);
    buffer_init(&client->request_acc.request_uri_buf);
    buffer_init(&client->request_acc.header_field_buf);
    buffer_init(&client->request_acc.header_value_buf);
    client->request_acc.last_header_cb = NONE;
    buffer_init(&client->request_acc.body_buf);
    client->request_acc.body_total = 0;
    client->request_acc.multipart_parser = 0;

    client->response_bufs = NULL;
    client->response_bufs_length = 0;
    client->response_file = -1;
    client->response_file_size = 0;
    buffer_init(&client->response_file_buf);

    return client;
}

static const char *state_to_string(enum client_state state)
{
    switch (state) {
        case IDLE:
            return "IDLE";
        case READING_REQUEST_HEADER:
            return "READING_REQUEST_HEADER";
        case WRITING_CONTINUE_STATUS_LINE:
            return "WRITING_CONTINUE_STATUS_LINE";
        case READING_REQUEST_BODY:
            return "READING_REQUEST_BODY";
        case PREPARING_RESPONSE:
            return "PREPARING_RESPONSE";
        case WRITING_RESPONSE:
            return "WRITING_RESPONSE";
        case WRITING_FATAL_RESPONSE:
            return "WRITING_FATAL_RESPONSE";
        case CLOSING:
            return "CLOSING";
        default:
            return "(unknown)";
    }
}

static void
client_set_state(client_t *client, enum client_state new_state)
{
    switch (client->state) {
        case IDLE:
            assert(new_state == READING_REQUEST_HEADER
                || new_state == CLOSING);
            break;
        case READING_REQUEST_HEADER:
            assert(new_state == WRITING_CONTINUE_STATUS_LINE
                || new_state == WRITING_FATAL_RESPONSE
                || new_state == CLOSING);
            break;
        case WRITING_CONTINUE_STATUS_LINE:
            assert(new_state == READING_REQUEST_BODY
                || new_state == CLOSING);
            break;
        case READING_REQUEST_BODY:
            assert(new_state == PREPARING_RESPONSE
                || new_state == WRITING_RESPONSE
                || new_state == WRITING_FATAL_RESPONSE
                || new_state == CLOSING);
            break;
        case PREPARING_RESPONSE:
            assert(new_state == WRITING_RESPONSE);
            /* not CLOSING */
            break;
        case WRITING_RESPONSE:
            assert(new_state == IDLE
                || new_state == CLOSING);
            break;
        case WRITING_FATAL_RESPONSE:
            assert(new_state == CLOSING);
            break;
        case CLOSING:
            assert(0 && "CLOSING is the last state");
            break;
    }

    LOG_DEBUG("[%d:%d] client set state=%s\n",
        client->id, client->request_count, state_to_string(new_state));

    client->state = new_state;
}

/*
** Callbacks
*/

static void
daemon_on_signal(uv_signal_t *sig, int status)
{
    daemon_t *dmn = daemon_from_signal_data(sig);
    struct periodic *periodic;
    client_t *client;

    LOG_NOTICE("[srv] received signal %d\n", sig->signum);

    if (dmn->state != DAEMON_RUNNING) {
        return;
    }

    dmn->state = DAEMON_STOPPING;
    uv_close(handle_from_tcp(&dmn->server), NULL);

    /* Unref signal handles so they do not prevent the loop finishing. */
    uv_unref(handle_from_signal(&dmn->signal1));
    uv_unref(handle_from_signal(&dmn->signal2));

    /* Stop periodic timers immediately. */
    for (periodic = dmn->periodics; periodic; periodic = periodic->next) {
        uv_timer_stop(&periodic->timer);
    }

    /* Close idle clients immediately. */
    for (client = dmn->clients; client; client = client->next) {
        if (client->state == IDLE) {
            client_close(client, __LINE__);
        }
    }
}

static void
daemon_on_periodic_timer(uv_timer_t *timer, int status)
{
    struct periodic *periodic = periodic_from_handle_data(
        handle_from_timer(timer));

    LOG_INFO("[srv] periodic timer\n");

    call_periodic_handler_pred(periodic->handler);
}

static void
server_on_connect(uv_stream_t *server_handle, int status)
{
    daemon_t *dmn = daemon_from_stream_data(server_handle);
    client_t *client;
    int r;

    if (status != 0) {
        LOG_WARN("[srv] server_on_connect: status=%d\n", status);
        return;
    }

    LOG_DEBUG("[srv] server_on_connect\n");

    client = make_client(dmn);

    r = uv_accept(server_handle, client_tcp_stream(client));
    if (r != 0) {
        LOG_WARN("[%d:%d] accept failed\n", client->id, client->request_count);
        client_close(client, __LINE__);
        return;
    }

    LOG_INFO("[%d:%d] new client\n", client->id, client->request_count);

    client_resume_read(client, client_on_read_timeout, READ_START_TIMEOUT);
}

static void
client_resume_read(client_t *client, uv_timer_cb timeout_cb, int64_t timeout)
{
    uv_loop_t *loop = client->timer.loop;

    assert(client->state == IDLE
        || client->state == READING_REQUEST_HEADER
        || client->state == READING_REQUEST_BODY);

    LOG_DEBUG("[%d:%d] resume read: now=%ld, timeout=%ld\n",
        client->id, client->request_count, uv_now(loop), timeout);

    if (HTTP_PARSER_ERRNO(&client->parser) == HPE_PAUSED) {
        http_parser_pause(&client->parser, 0);
    }

    if (client->read_buf.len > 0) {
        client_on_read(client_tcp_stream(client), 0, uv_buf_init(0, 0));
    }

    uv_update_time(loop);
    uv_timer_start(&client->timer, timeout_cb, timeout, 0);

    if (client->state == IDLE
        || client->state == READING_REQUEST_HEADER
        || client->state == READING_REQUEST_BODY)
    {
        uv_read_start(client_tcp_stream(client), client_on_alloc, client_on_read);
    }
}

static void
client_pause_read(client_t *client)
{
    LOG_DEBUG("[%d:%d] pause read: timer=%p\n",
        client->id, client->request_count, &client->timer);

    if (HTTP_PARSER_ERRNO(&client->parser) == HPE_OK) {
        http_parser_pause(&client->parser, 1);
    }

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

#if 0 /* pathological */
    suggested_size = 1;
#endif

    LOG_DEBUG("[%d:%d] alloc: %ld bytes\n",
        client->id, client->request_count, suggested_size);

    ptr = buffer_reserve(&client->read_buf, suggested_size);

    return uv_buf_init(ptr, suggested_size);
}

static void
client_on_read(uv_stream_t *tcp, ssize_t nread, uv_buf_t buf)
{
    client_t *client = client_from_stream_data(tcp);

    LOG_DEBUG("[%d:%d] on_read: nread=%d\n",
        client->id, client->request_count, nread);

    if (nread > 0) {
        buffer_advance(&client->read_buf, nread);

        /* XXX still susceptible to attack */
        uv_update_time(client->timer.loop);
        uv_timer_start(&client->timer, client_on_read_timeout,
            READ_CONT_TIMEOUT, 0);

        LOG_DEBUG("[%d:%d] on_read: deferred read timeout, now=%d\n",
            client->id, client->request_count,
            uv_now(client->timer.loop));
    }

    if (nread < 0) {
        uv_err_t err = uv_last_error(client->daemon->loop);
        if (err.code == UV_EOF) {
            LOG_INFO("[%d:%d] read eof\n",
                client->id, client->request_count);
        } else {
            LOG_NOTICE("[%d:%d] read error: %s\n",
                client->id, client->request_count,
                uv_strerror(err));
        }
        client_close(client, __LINE__);
        return;
    }

    assert(client->state == IDLE
        || client->state == READING_REQUEST_HEADER
        || client->state == READING_REQUEST_BODY);

    if (client->read_buf.len > 0) {
        size_t parsed;
        enum http_errno errnum;

        LOG_DEBUG("[%d:%d] parser_execute: nread=%d, read_buf.len=%d\n",
            client->id, client->request_count,
            nread, client->read_buf.len);

        parsed = http_parser_execute(&client->parser,
            &client->daemon->parser_settings,
            client->read_buf.data, (size_t)client->read_buf.len);

        LOG_DEBUG("[%d:%d] parser_execute returned: parsed=%d\n",
            client->id, client->request_count, parsed);

        buffer_shift(&client->read_buf, parsed);

        errnum = HTTP_PARSER_ERRNO(&client->parser);
        if (errnum == HPE_PAUSED) {
            LOG_DEBUG("[%d:%d] parser_execute result: parser was paused\n",
                client->id, client->request_count);
            return;
        }
        if (errnum == HPE_HEADER_OVERFLOW) {
            client_pause_read(client);
            client_write_fatal_431_request_header_fields_too_large(client);
            return;
        }
        if (errnum != HPE_OK) {
            LOG_INFO("[%d:%d] parse error: %s %s\n",
                client->id, client->request_count,
                http_errno_name(errnum), http_errno_description(errnum));
            client_close(client, __LINE__);
            return;
        }
    }
}

static void
client_on_read_timeout(uv_timer_t *timer, int status)
{
    client_t *client = client_from_timer_data(timer);
    uv_loop_t *loop = timer->loop;

    LOG_INFO("[%d:%d] on_read_timeout: now=%ld, state=%s\n",
        client->id, client->request_count,
        uv_now(loop), state_to_string(client->state));

    /* XXX why does this callback fire even after we called uv_timer_stop? */
    /* This is why we must check the state. */
    if (client->state == IDLE
        || client->state == READING_REQUEST_HEADER
        || client->state == READING_REQUEST_BODY)
    {
        client_close(client, __LINE__);
    }
}

static int
client_on_message_begin(http_parser *parser)
{
    client_t *client = client_from_parser_data(parser);

    client->request_count++;
    LOG_DEBUG("[%d:%d] on_message_begin: state=%s\n",
        client->id, client->request_count,
        state_to_string(client->state));

    assert(client->state == IDLE);
    client_set_state(client, READING_REQUEST_HEADER);
    client->deferred_error = NO_ERROR_YET;
    client->request = request_init(client);

    /* Do not clear read_buf; would break pipelining. */
    buffer_clear(&client->request_acc.request_uri_buf);
    buffer_clear(&client->request_acc.header_field_buf);
    buffer_clear(&client->request_acc.header_value_buf);
    client->request_acc.last_header_cb = NONE;
    buffer_clear(&client->request_acc.body_buf);
    client->request_acc.body_total = 0;
    client->request_acc.multipart_parser = 0;

    assert(client->response_bufs == NULL);
    assert(client->response_bufs_length == 0);
    assert(client->response_file == -1);
    assert(client->response_file_size == 0);
    buffer_init(&client->response_file_buf);

    return 0;
}

static int
client_on_url(http_parser *parser, const char *at, size_t length)
{
    client_t *client = client_from_parser_data(parser);

    assert(client->state == READING_REQUEST_HEADER);

    LOG_DEBUG("[%d:%d] on_url\n",
        client->id, client->request_count);

    buffer_append(&client->request_acc.request_uri_buf, at, length);

    return 0;
}

static bool
maybe_done_prev_header(client_t *client, bool force_clear)
{
    bool ret = true;

    if (client->request_acc.last_header_cb == WAS_HEADER_VALUE) {
        MR_String field;
        MR_String value;
        MR_bool valid_field;
        MR_bool valid_value;

        /*
        ** HTTP header names are US-ASCII only.
        ** HTTP header values may contain octets to be interpreted as
        ** ISO-8859-1 in some places (will be deprecated).
        */
        field = buffer_to_string_utf8(
            &client->request_acc.header_field_buf, &valid_field);
        value = buffer_to_string_iso_8859_1(
            &client->request_acc.header_value_buf, &valid_value);

        if (!valid_field || !valid_value) {
            LOG_INFO("[%d:%d] invalid header\n",
                client->id, client->request_count);
            client->deferred_error = BAD_REQUEST_400;
            ret = false;
        } else {
            /* Probably lower this to LOG_DEBUG later. */
            LOG_INFO("[%d:%d] header: '%s: %s'\n",
                client->id, client->request_count,
                field, value);
            client->request = request_add_header(client->request, field, value);
        }

        force_clear = true;
    }

    if (force_clear) {
        buffer_clear(&client->request_acc.header_field_buf);
        buffer_clear(&client->request_acc.header_value_buf);
        client->request_acc.last_header_cb = NONE;
    }

    return ret;
}

static int
client_on_header_field(http_parser *parser, const char *at, size_t length)
{
    client_t *client = client_from_parser_data(parser);

    assert(client->state == READING_REQUEST_HEADER);

    if (!maybe_done_prev_header(client, false)) {
        return -1;
    }

    LOG_DEBUG("[%d:%d] on_header_field\n",
        client->id, client->request_count);

    buffer_append(&client->request_acc.header_field_buf, at, length);
    client->request_acc.last_header_cb = WAS_HEADER_FIELD;

    return 0;
}

static int
client_on_header_value(http_parser *parser, const char *at, size_t length)
{
    client_t *client = client_from_parser_data(parser);

    assert(client->state == READING_REQUEST_HEADER);

    LOG_DEBUG("[%d:%d] on_header_value\n",
        client->id, client->request_count);

    buffer_append(&client->request_acc.header_value_buf, at, length);
    client->request_acc.last_header_cb = WAS_HEADER_VALUE;

    return 0;
}

static int
client_on_headers_complete(http_parser *parser)
{
    client_t *client = client_from_parser_data(parser);
    daemon_t *dmn = client->daemon;
    MR_String method;
    MR_String request_uri;
    MR_bool enforce_host_header;
    MR_bool valid;
    bool really_write_continue;

    assert(client->state == READING_REQUEST_HEADER);

    client_pause_read(client);

    /* Pick up the last header, if any */
    if (!maybe_done_prev_header(client, true)) {
        return -1;
    }

    /* Pick up the method. */
    MR_make_aligned_string(method, http_method_str(client->parser.method));

    /* Pick up the Request-URI. Should be limited to US-ASCII. */
    request_uri = buffer_to_string_utf8(&client->request_acc.request_uri_buf,
        &valid);
    buffer_clear(&client->request_acc.request_uri_buf);
    if (!valid) {
        return -1;
    }

    LOG_INFO("[%d:%d] on_headers_complete: method='%s', request-uri='%s'\n",
        client->id, client->request_count,
        method, request_uri);

    if (!(client->parser.flags & F_CHUNKED) &&
        client->parser.content_length != ULLONG_MAX &&
        client->parser.content_length > dmn->max_body)
    {
        LOG_INFO("[%d:%d] content_length=%ld too big\n",
            client->id, client->request_count,
            client->parser.content_length);
        client_write_fatal_413_request_entity_too_large(client);
        return 0;
    }

    enforce_host_header = http11_or_greater(&client->parser);
    if (request_prepare(method, request_uri, enforce_host_header,
            client->request, &client->request) == MR_FALSE)
    {
        LOG_NOTICE("[%d:%d] request_prepare failed\n",
            client->id, client->request_count);
        client->deferred_error = BAD_REQUEST_400;
    }

    if (!http11_or_greater(&client->parser)
        || client->deferred_error != NO_ERROR_YET)
    {
        really_write_continue = false;
    } else {
        int expect = request_get_expect_header(client->request);
        if (expect == 0) {
            /* No Expect: header. */
            really_write_continue = false;
        } else if (expect > 0) {
            /* Expect: 100-continue */
            really_write_continue = true;
        } else {
            /* Cannot satisfy expectation. */
            client->deferred_error = EXPECTATION_FAILED_417;
            really_write_continue = false;
        }
    }

    client_maybe_write_continue_status_line(client, really_write_continue);

    return 0;
}

static void
client_maybe_write_continue_status_line(client_t *client, bool really)
{
    static char text[] = "HTTP/1.1 100 Continue\r\n\r\n";
    const int nbufs = 1;
    uv_buf_t bufs[1];

    client_set_state(client, WRITING_CONTINUE_STATUS_LINE);

    if (really) {
        LOG_DEBUG("[%d:%d] writing 100 Continue\n",
            client->id, client->request_count);
        bufs[0] = uv_buf_init(text, sizeof(text) - 1);
    } else {
        bufs[0] = uv_buf_init(text, 0); /* dummy */
    }

    uv_write(&client->write_req, client_tcp_stream(client),
        bufs, nbufs, client_after_write_continue_status_line);
}

static void
client_after_write_continue_status_line(uv_write_t *req, int status)
{
    client_t *client = client_from_stream_data(req->handle);

    LOG_DEBUG("[%d:%d] after write_continue_status_line: status=%d\n",
        client->id, client->request_count, status);

    assert(client->state == WRITING_CONTINUE_STATUS_LINE);

    if (status != 0) {
        client_close(client, __LINE__);
        return;
    }

    client_set_state(client, READING_REQUEST_BODY);
    client_maybe_start_formdata_parser(client);
    client_resume_read(client, client_on_read_timeout, READ_CONT_TIMEOUT);
}

static void
client_maybe_start_formdata_parser(client_t *client)
{
    MR_bool succeeded;
    MR_String boundary;

    succeeded = request_search_multipart_formdata_boundary(client->request,
        &boundary);
    if (succeeded) {
        LOG_DEBUG("[%d:%d] multipart/form-data parser: boundary='%s'\n",
            client->id, client->request_count, boundary);
        client->request_acc.multipart_parser =
            create_formdata_parser(boundary);
    }
}

static int
client_on_body(http_parser *parser, const char *at, size_t length)
{
    client_t *client = client_from_parser_data(parser);
    daemon_t *dmn = client->daemon;

    if (client->request_acc.body_total == 0) {
        LOG_DEBUG("[%d:%d] on_body first: %d bytes\n",
            client->id, client->request_count, length);
    } else {
        LOG_DEBUG("[%d:%d] on_body subseq: %d bytes\n",
            client->id, client->request_count, length);
    }

    assert(client->state == READING_REQUEST_BODY);

    if (client->deferred_error != NO_ERROR_YET) {
        LOG_INFO("[%d:%d] on_body deferred_error=%d\n",
            client->id, client->request_count, client->deferred_error);
        return 0;
    }

    /*
    ** We checked the content-length header earlier, but this checks
    ** chunked requests which do not provide the length up front.
    ** The multipart parser regularly drains body_buf so
    ** we need a separate counter for total length.
    */
    if (client->request_acc.body_total + length > dmn->max_body) {
        client_pause_read(client);
        client_write_fatal_413_request_entity_too_large(client);
        return 0;
    }

    buffer_append(&client->request_acc.body_buf, at, length);
    client->request_acc.body_total += length;

    /* Run the form-data parser if in effect. */
    if (client->request_acc.multipart_parser != 0) {
        MR_Integer parsed;
        MR_Bool is_error;
        MR_String error_string;

        parse_formdata(&client->request_acc.body_buf, 0, &parsed,
            client->request_acc.multipart_parser,
            &client->request_acc.multipart_parser,
            &is_error, &error_string);

        buffer_shift(&client->request_acc.body_buf, parsed);

        LOG_DEBUG("[%d:%d] parse_formdata: parsed=%ld\n",
            client->id, client->request_count, parsed);

        if (is_error) {
            LOG_NOTICE("[%d:%d] parse_formdata error: %s\n",
                client->id, client->request_count, error_string);
            client_pause_read(client);
            client_write_fatal_400_bad_request(client);
            return 0;
        }
    }

    return 0;
}

static int
client_on_message_complete(http_parser *parser)
{
    client_t *client = client_from_parser_data(parser);
    MR_String method;
    bool valid;

    assert(client->state == READING_REQUEST_BODY);

    client_pause_read(client);

    LOG_DEBUG("[%d:%d] on_message_complete: state=%s, deferred_error=%d\n",
        client->id, client->request_count,
        state_to_string(client->state), client->deferred_error);

    switch (client->deferred_error) {
        case BAD_REQUEST_400:
            client_write_400_bad_request(client);
            return 0;
        case EXPECTATION_FAILED_417:
            client_write_417_expectation_failed(client);
            return 0;
        case NO_ERROR_YET:
            break;
        default:
            assert(client->deferred_error == NO_ERROR_YET);
            break;
    }

    /* Pick up the body. */
    valid = client_set_request_body(client);
    buffer_init(&client->request_acc.body_buf);
    client->request_acc.multipart_parser = 0;
    if (!valid) {
        client_write_400_bad_request(client); /* probably */
        return 0;
    }

    /* Call the request handler. */
    LOG_DEBUG("[%d:%d] on_message_complete: call request handler\n",
        client->id, client->request_count);

    client_set_state(client, PREPARING_RESPONSE);
    call_request_handler_pred(client->daemon->request_handler,
        client->request);

    return 0;
}

static void
client_write_400_bad_request(client_t *client)
{
    static char text[] =
        "HTTP/1.1 400 Bad Request\r\n"
        "Content-Length: 0\r\n"
        "\r\n";

    client_write_error_response(client, 400, WRITING_RESPONSE,
        text, sizeof(text) - 1);
}

static void
client_write_417_expectation_failed(client_t *client)
{
    static char text[] =
        "HTTP/1.1 417 Expectation Failed\r\n"
        "Content-Length: 0\r\n"
        "\r\n";

    client_write_error_response(client, 417, WRITING_RESPONSE,
        text, sizeof(text) - 1);
}

static void
client_write_fatal_400_bad_request(client_t *client)
{
    static char text[] =
        "HTTP/1.1 400 Bad Request\r\n"
        "Content-Length: 0\r\n"
        "\r\n";

    /*
    ** This error is fatal as we don't want to read any of the body
    ** after sending the error response.
    */
    client_write_error_response(client, 400, WRITING_FATAL_RESPONSE,
        text, sizeof(text) - 1);
}

static void
client_write_fatal_413_request_entity_too_large(client_t *client)
{
    static char text[] =
        "HTTP/1.1 413 Request Entity Too Large\r\n"
        "Content-Length: 0\r\n"
        "\r\n";

    /*
    ** This error is fatal as we don't want to read any of the body
    ** after sending the error response.
    */
    client_write_error_response(client, 413, WRITING_FATAL_RESPONSE,
        text, sizeof(text) - 1);
}

static void
client_write_fatal_431_request_header_fields_too_large(client_t *client)
{
    static char text[] =
        "HTTP/1.1 431 Request Header Fields Too Large\r\n"
        "Content-Length: 0\r\n"
        "\r\n";

    /*
    ** This error is fatal as we don't want to read any of the header or body
    ** after sending the error response.
    */
    client_write_error_response(client, 431, WRITING_FATAL_RESPONSE,
        text, sizeof(text) - 1);
}

static void
client_write_error_response(client_t *client, int status_code,
    enum client_state new_state, const char *response, size_t responselen)
{
    const int nbufs = 1;
    uv_buf_t bufs[nbufs];

    LOG_INFO("[%d:%d] write error response: status_code=%d, new_state=%s\n",
        client->id, client->request_count, status_code,
        state_to_string(new_state));

    client_set_state(client, new_state);

    /* XXX set write timeout? */

    bufs[0] = uv_buf_init((char *) response, responselen);
    uv_write(&client->write_req, client_tcp_stream(client),
        bufs, nbufs, client_after_write_response_bufs);
}

static bool
client_set_request_body(client_t *client)
{
    MR_bool valid;

    LOG_DEBUG("[%d:%d] set_request_body\n",
        client->id, client->request_count);

    if (client->request_acc.multipart_parser != 0) {
        request_set_body_formdata(client->request_acc.multipart_parser,
            client->request, &client->request, &valid);
        if (!valid) {
            return false;
        }
    } else {
        MR_String body;

        /* Perhaps we need to support other charsets here. */
        body = buffer_to_string_utf8(&client->request_acc.body_buf, &valid);
        if (!valid) {
            return false;
        }
        client->request = request_set_body_stringish(client->request, body);
    }

    return true;
}

static void
client_on_async(uv_async_t *async, int status)
{
    client_t *client = client_from_async_data(async);
    uv_loop_t *loop = client->daemon->loop;

    LOG_DEBUG("[%d:%d] on_async: status=%d, state=%s\n",
        client->id, client->request_count, status,
        state_to_string(client->state));

    /*
    ** uv_async_send causes the callback to be invoked one or more times;
    ** ignore the second and subsequent invocations.
    */
    if (client->state != PREPARING_RESPONSE) {
        LOG_INFO("[%d:%d] on_async: ignored\n",
            client->id, client->request_count);
        return;
    }

    /* We hold onto response_bufs while writing to prevent collection. */
    assert(client->response_bufs != NULL);

    client_set_state(client, WRITING_RESPONSE);

    /* Set write timeout. */
    uv_update_time(loop);
    uv_timer_start(&client->timer, client_on_write_timeout,
        WRITE_TIMEOUT, WRITE_TIMEOUT);

    /* Begin writing. */
    uv_write(&client->write_req, client_tcp_stream(client),
        client->response_bufs, client->response_bufs_length,
        client_after_write_response_bufs);
}

static void
client_on_write_timeout(uv_timer_t *timer, int status)
{
    client_t *client = client_from_timer_data(timer);

    LOG_NOTICE("[%d:%d] on_write_timeout: state=%s\n",
        client->id, client->request_count,
        state_to_string(client->state));

    /*
    ** There is no way to cancel a write request (yet?) so we just close.
    ** This will cause write callbacks to be called with an error status.
    */
    if (client->state == WRITING_RESPONSE) {
        client_close_response_file(client);
        client_close(client, __LINE__);
    }
}

static void
client_after_write_response_bufs(uv_write_t *req, int status)
{
    client_t *client = client_from_stream_data(req->handle);

    uv_timer_stop(&client->timer);

    LOG_DEBUG("[%d:%d] after_write_response_bufs: status=%d\n",
        client->id, client->request_count, status);

    if (client->state == CLOSING) {
        /* Write timed out. */
        return;
    }

    assert(client->state == WRITING_RESPONSE
        || client->state == WRITING_FATAL_RESPONSE);

    client->response_bufs = NULL;
    client->response_bufs_length = 0;

    if (status != 0) {
        client_close_response_file(client);
    }

    if (client->response_file != -1) {
        client_start_response_file(client);
    } else {
        client_after_full_response(client, status);
    }
}

static void
client_start_response_file(client_t *client)
{
    uv_loop_t *loop = client->daemon->loop;
    int r;

    LOG_DEBUG("[%d:%d] start_response_file: fd=%d\n",
        client->id, client->request_count, client->response_file);

    assert(client->state == WRITING_RESPONSE);
    assert(client->response_file != -1);

    buffer_reserve(&client->response_file_buf, RESPONSE_FILE_BUFSIZE);
    client->response_file_req.data = client;

    /* Set write timeout. */
    uv_update_time(loop);
    uv_timer_start(&client->timer, client_on_write_timeout,
        WRITE_TIMEOUT, WRITE_TIMEOUT);

    /* Start reading. */
    r = uv_fs_read(loop,
        &client->response_file_req, client->response_file,
        client->response_file_buf.data, client->response_file_buf.cap, -1,
        client_on_read_response_file_buf);
    if (r != 0) {
        client_after_response_file(client, -1);
    }
}

static void
client_on_read_response_file_buf(uv_fs_t *req)
{
    client_t *client = client_from_fs(req);
    uv_loop_t *loop = client->daemon->loop;
    ssize_t nread;
    const int nbufs = 1;
    uv_buf_t bufs[nbufs];

    nread = req->result;
    uv_fs_req_cleanup(req);

    LOG_DEBUG("[%d:%d] on_read_response_file_buf: nread=%d, state=%s\n",
        client->id, client->request_count, nread,
        state_to_string(client->state));

    if (client->state == CLOSING) {
        /* Write timeout. */
        return;
    }

    assert(client->state == WRITING_RESPONSE);

    if (nread < 0) {
        client_after_response_file(client, -1);
        return;
    }

    if (nread == 0) { /* EOF */
        client_after_response_file(client, 0);
        return;
    }

    LOG_DEBUG("[%d:%d] write response file buffer\n",
        client->id, client->request_count, client->response_file);

    /* Renew timeout. */
    uv_timer_again(&client->timer);

    /* Begin writing. */
    bufs[0] = uv_buf_init(client->response_file_buf.data, nread);
    uv_write(&client->write_req, client_tcp_stream(client),
        bufs, nbufs, client_after_write_response_file_buf);
}

static void
client_after_write_response_file_buf(uv_write_t *req, int status)
{
    client_t *client = client_from_stream_data(req->handle);
    int r;

    LOG_DEBUG("[%d:%d] after_write_response_file_buf: status=%d, state=%s\n",
        client->id, client->request_count, status,
        state_to_string(client->state));

    if (client->state == CLOSING) {
        /* Was closed earlier. */
        return;
    }

    assert(client->state == WRITING_RESPONSE);

    if (status != 0) {
        client_after_response_file(client, status);
        assert(client->state == CLOSING);
        return;
    }

    /* Continue reading. */

    assert(client->response_file_buf.cap > 0);

    r = uv_fs_read(client->daemon->loop,
        &client->response_file_req, client->response_file,
        client->response_file_buf.data, client->response_file_buf.cap, -1,
        client_on_read_response_file_buf);
    if (r != 0) {
        client_after_response_file(client, -1);
        assert(client->state == CLOSING);
    }
}

static void
client_after_response_file(client_t *client, int status)
{
    LOG_DEBUG("[%d:%d] after_response_file: status=%d, state=%s\n",
        client->id, client->request_count, status,
        state_to_string(client->state));

    uv_timer_stop(&client->timer);

    client_close_response_file(client);

    client_after_full_response(client, status);
}

static void
client_close_response_file(client_t *client)
{
    uv_fs_t close_req;

    if (client->response_file < 0)
        return;

    LOG_DEBUG("[%d:%d] close_response_file: fd=%d\n",
        client->id, client->request_count, client->response_file);

    /* Synchronous close (because of null callback). */
    uv_fs_close(client->daemon->loop, &close_req, client->response_file, NULL);
    client->response_file = -1;
    client->response_file_size = 0;
    buffer_init(&client->response_file_buf);
}

static void
client_after_full_response(client_t *client, int status)
{
    LOG_DEBUG("[%d:%d] after_full_response: status=%d, state=%s\n",
        client->id, client->request_count, status,
        state_to_string(client->state));

    if (client->state == CLOSING) {
        /* Write timed out. */
        return;
    }

    if (client->state == WRITING_FATAL_RESPONSE) {
        client_close(client, __LINE__);
        return;
    }

    assert(client->state == WRITING_RESPONSE);
    client_set_state(client, IDLE);

    if (status != 0 || !http_should_keep_alive(&client->parser)) {
        client_close(client, __LINE__);
        return;
    }

    LOG_INFO("[%d:%d] persisting connection\n",
        client->id, client->request_count);

    if (client->read_buf.len == 0) {
        client_resume_read(client, client_on_keepalive_timeout,
            KEEPALIVE_TIMEOUT);
    } else {
        /* Already had part of the next request in the read_buf. */
        client_resume_read(client, client_on_read_timeout, READ_START_TIMEOUT);
    }
}

static void
client_on_keepalive_timeout(uv_timer_t *timer, int status)
{
    client_t *client = client_from_timer_data(timer);
    uv_loop_t *loop = timer->loop;

    LOG_INFO("[%d:%d] on_keepalive_timeout: now=%ld, state=%s\n",
        client->id, client->request_count, uv_now(loop),
        state_to_string(client->state));

    if (client->state == IDLE) {
        client_close(client, __LINE__);
    }
}

static void
client_close(client_t *client, int line)
{
    LOG_INFO("[%d:%d] close by %s:%d: state=%s\n",
        client->id, client->request_count,
        __FILE__, line, state_to_string(client->state));

    assert(client->response_file == -1);

    client_set_state(client, CLOSING);

    client_pause_read(client);

    uv_close(handle_from_timer(&client->timer), client_on_close_1);
}

static void
client_on_close_1(uv_handle_t *handle)
{
    client_t *client = client_from_handle_data(handle);
    uv_timer_t *timer = timer_from_handle_checked(handle);

    LOG_DEBUG("[%d:%d] on_close_1\n",
        client->id, client->request_count);

    /*
    ** For some reason we need to stop it again after closing otherwise it may
    ** still fire?
    */
    uv_timer_stop(timer);

    uv_close(handle_from_async(&client->async), client_on_close_2);
}

static void
client_on_close_2(uv_handle_t *handle)
{
    client_t *client = client_from_handle_data(handle);

    LOG_DEBUG("[%d:%d] on_close_2\n", client->id, client->request_count);

    uv_close(handle_from_stream(client_tcp_stream(client)), client_on_close_3);
}

static void
client_on_close_3(uv_handle_t *handle)
{
    client_t *client = client_from_handle_data(handle);
    uv_loop_t *loop = client->daemon->loop;

    LOG_DEBUG("[%d:%d] on_close_3\n", client->id, client->request_count);

    assert(!uv_is_active(handle_from_async(&client->async)));

    client_unlink(client);

    MR_GC_free(client); /* allocated uncollectable */

    /* uv_print_all_handles(loop); */
}

static void
client_unlink(client_t *client)
{
    daemon_t *dmn = client->daemon;
    client_t *prev;
    client_t *cur;

    prev = NULL;
    cur = dmn->clients;
    while (cur != NULL) {
        if (cur == client) {
            if (prev == NULL) {
                dmn->clients = cur->next;
            } else {
                prev->next = cur->next;
            }
            cur->next = NULL;
            return;
        }
        prev = cur;
        cur = cur->next;
    }

    assert(0 && "client not in linked list");
}

/*
** Get client address
*/

static MR_String
client_address_ipv4(client_t *client, MR_AllocSiteInfoPtr alloc_id)
{
    struct sockaddr_in name;
    int namelen;
    char tmp[sizeof "255.255.255.255"];
    MR_String s;
    int r;

    namelen = sizeof(name);
    r = uv_tcp_getpeername(&client->tcp, (struct sockaddr *) &name, &namelen);
    if (r != 0 || namelen > sizeof(name)) {
        s = MR_make_string_const("");
    } else {
        r = uv_ip4_name(&name, tmp, sizeof(tmp));
        if (r == 0) {
            MR_make_aligned_string_copy_msg(s, tmp, alloc_id);
        } else {
            s = MR_make_string_const("");
        }
    }
    return s;
}

/*
** Set response
*/

void
_httpsrv_set_response_bufs(client_t *client,
    MR_Word response_list, MR_Integer response_list_length,
    MR_Integer response_file, MR_Integer response_file_size)
{
    uv_buf_t *response_bufs;
    MR_Integer i;
    MR_String s;

    LOG_DEBUG("[%d:%d] set_response: response_list_length=%d\n",
        client->id, client->request_count,
        response_list_length);

    assert(client->response_bufs == NULL);
    assert(client->response_file == -1);

    response_bufs = MR_GC_NEW_ARRAY(uv_buf_t, response_list_length);

    for (i = 0; i < response_list_length; i++) {
        assert(! MR_list_is_empty(response_list));
        s = (const MR_String) MR_list_head(response_list);
        response_bufs[i] = uv_buf_init(s, strlen(s));
        response_list = MR_list_tail(response_list);
    }

    client->response_bufs = response_bufs;
    client->response_bufs_length = response_list_length;

    client->response_file = response_file;
    client->response_file_size = response_file_size;
}

void
_httpsrv_send_async(client_t *client)
{
    /*
    ** We can't call libuv functions off the main thread.
    ** Send an async signal which triggers a callback on the main thread.
    */

    assert(client->state == PREPARING_RESPONSE);
    assert(client->response_bufs != NULL);

    uv_async_send(&client->async);
}

bool
_httpsrv_client_should_keep_alive(client_t *client)
{
    return http_should_keep_alive(&client->parser);
}

/* vim: set sts=4 sw=4 et: */
