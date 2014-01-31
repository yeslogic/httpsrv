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

#endif

/* vim: set sts=4 sw=4 et: */
