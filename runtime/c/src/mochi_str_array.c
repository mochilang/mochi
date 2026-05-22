/*
 * MEP-42 Phase 4.2.17 typed-str array runtime. See mochi_str_array.h.
 */
#include "mochi_str_array.h"

#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

mochi_str_array *mochi_str_array_new(void) {
    mochi_str_array *a = (mochi_str_array *)malloc(sizeof(mochi_str_array));
    if (a == NULL) {
        abort();
    }
    a->data = NULL;
    a->len = 0;
    a->cap = 0;
    return a;
}

int64_t mochi_str_array_len(const mochi_str_array *a) {
    return a->len;
}

void mochi_str_array_push(mochi_str_array *a, const char *v) {
    if (a->len == a->cap) {
        int64_t newcap = a->cap == 0 ? 4 : a->cap * 2;
        const char **nd = (const char **)realloc((void *)a->data, (size_t)newcap * sizeof(const char *));
        if (nd == NULL) {
            abort();
        }
        a->data = nd;
        a->cap = newcap;
    }
    a->data[a->len++] = v;
}

const char *mochi_str_array_get(const mochi_str_array *a, int64_t i) {
    return a->data[i];
}

void mochi_str_array_set(mochi_str_array *a, int64_t i, const char *v) {
    a->data[i] = v;
}

/*
 * quoted_len_and_write computes (when out==NULL) or writes (when
 * out!=NULL) the strconv.Quote-equivalent representation of s into
 * out, returning the number of bytes used (excluding NUL). The
 * two-call shape lets mochi_str_array_to_str size the result buffer
 * exactly without rendering twice the work: we measure the length
 * for every element, malloc once, then write into place.
 *
 * Escape rule (matches Go strconv.Quote for the ASCII subset):
 *   "  -> \"
 *   \\ -> \\\\
 *   \n -> \n
 *   \r -> \r
 *   \t -> \t
 *   \b -> \b
 *   \f -> \f
 *   0x00..0x1F (other) -> \u00NN (six bytes)
 *   0x20..0x7E -> verbatim
 *   0x80..0xFF -> verbatim (Mochi strings are UTF-8 byte sequences;
 *     the VM's strconv.Quote leaves multi-byte UTF-8 alone for
 *     printable runes, and the bench/tutorial fixtures use only
 *     well-formed UTF-8)
 *
 * NULL input returns 0 bytes for measuring; writing NULL would be
 * UB, but no caller passes NULL today (every TypeStr value in the
 * IR is at minimum the empty string literal "").
 */
static size_t quoted_len_and_write(const char *s, char *out) {
    size_t n = 0;
    if (out != NULL) out[n] = '"';
    n++;
    for (const unsigned char *p = (const unsigned char *)s; *p; p++) {
        unsigned char c = *p;
        switch (c) {
        case '"':
            if (out != NULL) { out[n] = '\\'; out[n + 1] = '"'; }
            n += 2;
            continue;
        case '\\':
            if (out != NULL) { out[n] = '\\'; out[n + 1] = '\\'; }
            n += 2;
            continue;
        case '\n':
            if (out != NULL) { out[n] = '\\'; out[n + 1] = 'n'; }
            n += 2;
            continue;
        case '\r':
            if (out != NULL) { out[n] = '\\'; out[n + 1] = 'r'; }
            n += 2;
            continue;
        case '\t':
            if (out != NULL) { out[n] = '\\'; out[n + 1] = 't'; }
            n += 2;
            continue;
        case '\b':
            if (out != NULL) { out[n] = '\\'; out[n + 1] = 'b'; }
            n += 2;
            continue;
        case '\f':
            if (out != NULL) { out[n] = '\\'; out[n + 1] = 'f'; }
            n += 2;
            continue;
        }
        if (c < 0x20) {
            /* \u00NN form for the remaining C0 control bytes. */
            if (out != NULL) {
                static const char hex[] = "0123456789abcdef";
                out[n + 0] = '\\';
                out[n + 1] = 'u';
                out[n + 2] = '0';
                out[n + 3] = '0';
                out[n + 4] = hex[(c >> 4) & 0xF];
                out[n + 5] = hex[c & 0xF];
            }
            n += 6;
            continue;
        }
        /* 0x20..0xFF: copy through verbatim. */
        if (out != NULL) out[n] = (char)c;
        n++;
    }
    if (out != NULL) out[n] = '"';
    n++;
    return n;
}

const char *mochi_str_array_to_str(const mochi_str_array *a) {
    if (a->len == 0) {
        return "[]";
    }
    size_t total = 2;                              /* '[' + ']' */
    if (a->len > 1) total += (size_t)(a->len - 1) * 2; /* ", " * (n-1) */
    for (int64_t i = 0; i < a->len; i++) {
        total += quoted_len_and_write(a->data[i], NULL);
    }
    char *buf = (char *)malloc(total + 1);
    if (buf == NULL) abort();
    size_t off = 0;
    buf[off++] = '[';
    for (int64_t i = 0; i < a->len; i++) {
        if (i > 0) {
            buf[off++] = ',';
            buf[off++] = ' ';
        }
        off += quoted_len_and_write(a->data[i], buf + off);
    }
    buf[off++] = ']';
    buf[off] = '\0';
    return buf;
}
