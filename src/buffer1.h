/*
** Copyright (C) 2014 YesLogic Pty. Ltd.
** All rights reserved.
*/

#ifndef __included_buffer1_h
#define __included_buffer1_h

typedef struct buffer buffer_t;

struct buffer {
    char *data;
    ssize_t len;    /* bytes */
    ssize_t cap;    /* bytes */
};

void
buffer_init(buffer_t *buf);

buffer_t *
buffer_new(void);

void
buffer_clear(buffer_t *buf);

char *
buffer_reserve(buffer_t *buf, ssize_t addlen);

void
buffer_append(buffer_t *buf, const char *adddata, ssize_t addlen);

void
buffer_advance(buffer_t *buf, ssize_t n);

void
buffer_shift(buffer_t *buf, ssize_t n);

const unsigned char *
buffers_join(MR_Word bufs /* list(buffer(buffer_ro)) */,
    size_t total_len, MR_AllocSiteInfoPtr alloc_id);

MR_String
buffer_to_string_utf8(buffer_t *buf, MR_bool *valid);

MR_String
make_string_utf8(const char *buf, size_t off, size_t len, MR_bool *valid);

MR_String
buffer_to_string_iso_8859_1(buffer_t *buf, MR_bool *valid);

MR_String
make_string_iso_8859_1(const char *buf, size_t off, size_t len,
    MR_bool *valid);

MR_String
buffers_to_string_utf8(MR_Word bufs /* list(buffer(buffer_ro)) */,
    size_t total_len, MR_bool *valid);

#endif

/* vim: set sts=4 sw=4 et: */
