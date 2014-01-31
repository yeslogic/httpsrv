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

static void
buffer_init(buffer_t *buf)
{
    buf->data = NULL;
    buf->len = 0;
    buf->cap = 0;
}

static void
buffer_clear(buffer_t *buf)
{
    buf->len = 0;
}

static void
buffer_append(buffer_t *buf, const char *adddata, ssize_t addlen)
{
    ssize_t reqcap;

    assert(buf->len >= 0);
    assert(addlen >= 0);

    reqcap = buf->len + addlen;
    assert(reqcap >= buf->len);

    if (buf->cap < reqcap) {
	ssize_t newcap = pot(reqcap);
	buf->data = MR_GC_realloc(buf->data, newcap);
	buf->cap = newcap;
    }

    assert(buf->cap >= reqcap);

    memcpy(buf->data + buf->len, adddata, addlen);
    buf->len += addlen;
}

static void
buffer_shift(buffer_t *buf, ssize_t n)
{
    ssize_t newlen = buf->len - n;
    assert(newlen >= 0);
    memmove(buf->data, buf->data + n, newlen);
    buf->len = newlen;
}

static MR_String
make_string(const char *buf, uint16_t off, uint16_t len)
{
    MR_String s;

    MR_allocate_aligned_string_msg(s, len, MR_ALLOC_SITE_NONE);
    memcpy(s, buf + off, len);
    s[len] = '\0';
    return s;
}

static MR_String
buffer_to_string(buffer_t *buf)
{
    return make_string(buf->data, 0, buf->len);
}

/* vim: set sts=4 sw=4 et: */
