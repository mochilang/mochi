/*
 * MEP-42 Phase 4.2.21 nested integer list runtime. See
 * mochi_list_list.h.
 */
#include "mochi_list_list.h"

#include <stdint.h>
#include <stdlib.h>
#include <string.h>

mochi_list_list *mochi_list_list_new(void) {
    mochi_list_list *m = (mochi_list_list *)malloc(sizeof(mochi_list_list));
    if (m == NULL) abort();
    m->data = NULL;
    m->len = 0;
    m->cap = 0;
    return m;
}

int64_t mochi_list_list_len(const mochi_list_list *m) {
    return m->len;
}

void mochi_list_list_push(mochi_list_list *m, mochi_list_i64 *row) {
    if (m->len == m->cap) {
        int64_t newcap = m->cap == 0 ? 4 : m->cap * 2;
        mochi_list_i64 **nd = (mochi_list_i64 **)realloc(
            (void *)m->data, (size_t)newcap * sizeof(mochi_list_i64 *));
        if (nd == NULL) abort();
        m->data = nd;
        m->cap = newcap;
    }
    m->data[m->len++] = row;
}

mochi_list_i64 *mochi_list_list_get(const mochi_list_list *m, int64_t i) {
    return m->data[i];
}

const char *mochi_list_list_to_str(const mochi_list_list *m) {
    if (m->len == 0) return "[]";
    /* Render each row, measure widths, then assemble. The rows are
     * cached so each mochi_list_i64_to_str pays at most once per row.
     * Memory for the per-row strings is owned by mochi_list_i64_to_str
     * (heap, leaked at process exit), so we only store the pointers
     * here. */
    const char **rows = (const char **)malloc((size_t)m->len * sizeof(const char *));
    if (rows == NULL) abort();
    size_t total = 2; /* '[' + ']' */
    if (m->len > 1) total += (size_t)(m->len - 1) * 2; /* ", " * (n-1) */
    for (int64_t i = 0; i < m->len; i++) {
        rows[i] = mochi_list_i64_to_str(m->data[i]);
        total += strlen(rows[i]);
    }
    char *buf = (char *)malloc(total + 1);
    if (buf == NULL) abort();
    size_t off = 0;
    buf[off++] = '[';
    for (int64_t i = 0; i < m->len; i++) {
        if (i > 0) {
            buf[off++] = ',';
            buf[off++] = ' ';
        }
        size_t n = strlen(rows[i]);
        memcpy(buf + off, rows[i], n);
        off += n;
    }
    buf[off++] = ']';
    buf[off] = '\0';
    free((void *)rows);
    return buf;
}
