/*
** Copyright (C) 2014 YesLogic Pty. Ltd.
** All rights reserved.
*/

static ssize_t
pot(ssize_t x)
{
    ssize_t y = 1;
    while (y < x && y > 0) {
	y *= 2;
    }
    assert(y >= x);
    return y;
}

static void *
gc_realloc_atomic(void *oldptr, size_t size)
{
    if (oldptr == NULL) {
        MR_Word ptr;
        MR_incr_hp_atomic(ptr, MR_bytes_to_words(size));
        return (void *) ptr;
    } else {
        return MR_GC_realloc(oldptr, size);
    }
}

void
buffer_init(buffer_t *buf)
{
    buf->data = NULL;
    buf->len = 0;
    buf->cap = 0;
}

buffer_t *
buffer_new(void)
{
    buffer_t *buf = MR_GC_NEW(buffer_t);
    buffer_init(buf);
    return buf;
}

void
buffer_clear(buffer_t *buf)
{
    buf->len = 0;
}

char *
buffer_reserve(buffer_t *buf, ssize_t addlen)
{
    ssize_t reqcap;

    assert(buf->len >= 0);
    assert(addlen >= 0);

    reqcap = buf->len + addlen;
    assert(reqcap >= buf->len);

    if (buf->cap < reqcap) {
	ssize_t newcap = pot(reqcap);
	buf->data = gc_realloc_atomic(buf->data, newcap);
	buf->cap = newcap;
    }

    assert(buf->cap >= reqcap);

    return buf->data + buf->len;
}

void
buffer_append(buffer_t *buf, const char *adddata, ssize_t addlen)
{
    char *next;

    next = buffer_reserve(buf, addlen);
    memcpy(next, adddata, addlen);
    buf->len += addlen;
}

void
buffer_advance(buffer_t *buf, ssize_t n)
{
    ssize_t newlen = buf->len + n;
    assert(newlen >= buf->len);
    assert(newlen <= buf->cap);
    buf->len = newlen;
}

void
buffer_shift(buffer_t *buf, ssize_t n)
{
    ssize_t newlen = buf->len - n;
    assert(newlen >= 0);
    memmove(buf->data, buf->data + n, newlen);
    buf->len = newlen;
}

MR_String
buffer_to_string_utf8(buffer_t *buf, MR_bool *valid)
{
    return make_string_utf8(buf->data, 0, buf->len, valid);
}

MR_String
make_string_utf8(const char *buf, size_t off, size_t len, MR_bool *valid)
{
    MR_String s;

    MR_allocate_aligned_string_msg(s, len, MR_ALLOC_SITE_NONE);
    memcpy(s, buf + off, len);
    s[len] = '\0';

    /* Check for valid UTF-8 encoding and no embedded NULs. */
    if (strlen(s) == len && MR_utf8_verify(s)) {
        *valid = MR_TRUE;
        return s;
    } else {
        *valid = MR_FALSE;
        return MR_make_string_const("");
    }
}

MR_String
buffers_to_string_utf8(MR_Word bufs, size_t total_len, MR_bool *valid)
{
    MR_String s;
    size_t soff;

    MR_allocate_aligned_string_msg(s, total_len, MR_ALLOC_SITE_NONE);
    soff = 0;
    while (! MR_list_is_empty(bufs)) {
        const buffer_t *buf = (const buffer_t *) MR_list_head(bufs);
        memcpy(s + soff, buf->data, buf->len);
        soff += buf->len;
        bufs = MR_list_tail(bufs);
    }
    s[total_len] = '\0';
    assert(soff == total_len);

    /* Check for valid UTF-8 encoding and no embedded NULs. */
    if (strlen(s) == total_len && MR_utf8_verify(s)) {
        *valid = MR_TRUE;
        return s;
    } else {
        *valid = MR_FALSE;
        return MR_make_string_const("");
    }
}

/* vim: set sts=4 sw=4 et: */
