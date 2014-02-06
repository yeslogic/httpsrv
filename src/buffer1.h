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

MR_String
buffer_to_string(buffer_t *buf);

MR_String
make_string(const char *buf, size_t off, size_t len);

#endif

/* vim: set sts=4 sw=4 et: */
