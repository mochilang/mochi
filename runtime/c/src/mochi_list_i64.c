/*
 * MEP-42 Phase 4.3.1 typed-i64 list runtime. See mochi_list_i64.h.
 */
#include "mochi_list_i64.h"

#include <inttypes.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

mochi_list_i64 *mochi_list_i64_new(void) {
    mochi_list_i64 *l = (mochi_list_i64 *)malloc(sizeof(mochi_list_i64));
    if (l == NULL) {
        abort();
    }
    l->data = NULL;
    l->len = 0;
    l->cap = 0;
    return l;
}

int64_t mochi_list_i64_len(const mochi_list_i64 *l) {
    return l->len;
}

void mochi_list_i64_push(mochi_list_i64 *l, int64_t v) {
    if (l->len == l->cap) {
        int64_t newcap = l->cap == 0 ? 4 : l->cap * 2;
        int64_t *nd = (int64_t *)realloc(l->data, (size_t)newcap * sizeof(int64_t));
        if (nd == NULL) {
            abort();
        }
        l->data = nd;
        l->cap = newcap;
    }
    l->data[l->len++] = v;
}

int64_t mochi_list_i64_get(const mochi_list_i64 *l, int64_t i) {
    return l->data[i];
}

void mochi_list_i64_set(mochi_list_i64 *l, int64_t i, int64_t v) {
    l->data[i] = v;
}

const char *mochi_list_i64_to_str(const mochi_list_i64 *l) {
    /* Empty list: return the static literal "[]" so callers do not have
     * to special-case malloc(2). The carrier is `const char*`, so a
     * literal is indistinguishable from a heap buffer downstream. */
    if (l->len == 0) {
        return "[]";
    }
    /* Worst-case per-element width: "-9223372036854775808" is 20 bytes
     * plus the ", " separator (2 bytes); reserve 22. Plus "[" + "]" + NUL. */
    size_t cap = (size_t)l->len * 22 + 3;
    char *buf = (char *)malloc(cap);
    if (buf == NULL) {
        abort();
    }
    size_t off = 0;
    buf[off++] = '[';
    for (int64_t i = 0; i < l->len; i++) {
        if (i > 0) {
            buf[off++] = ',';
            buf[off++] = ' ';
        }
        int n = snprintf(buf + off, cap - off, "%" PRId64, l->data[i]);
        if (n < 0) {
            abort();
        }
        off += (size_t)n;
    }
    buf[off++] = ']';
    buf[off] = '\0';
    return buf;
}

mochi_list_i64 *mochi_list_i64_concat(const mochi_list_i64 *a, const mochi_list_i64 *b) {
    mochi_list_i64 *out = mochi_list_i64_new();
    int64_t total = a->len + b->len;
    if (total > 0) {
        int64_t *nd = (int64_t *)malloc((size_t)total * sizeof(int64_t));
        if (nd == NULL) {
            abort();
        }
        if (a->len > 0) {
            memcpy(nd, a->data, (size_t)a->len * sizeof(int64_t));
        }
        if (b->len > 0) {
            memcpy(nd + a->len, b->data, (size_t)b->len * sizeof(int64_t));
        }
        out->data = nd;
        out->cap = total;
        out->len = total;
    }
    return out;
}
