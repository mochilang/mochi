/*
 * MEP-42 Phase 4.3.1 typed-i64 list runtime. See mochi_list_i64.h.
 */
#include "mochi_list_i64.h"

#include <stdint.h>
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
