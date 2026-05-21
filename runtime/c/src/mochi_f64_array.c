/*
 * MEP-42 Phase 4.3.3 typed-f64 array runtime. See mochi_f64_array.h.
 */
#include "mochi_f64_array.h"

#include <stdint.h>
#include <stdlib.h>

mochi_f64_array *mochi_f64_array_new(void) {
    mochi_f64_array *a = (mochi_f64_array *)malloc(sizeof(mochi_f64_array));
    if (a == NULL) {
        abort();
    }
    a->data = NULL;
    a->len = 0;
    a->cap = 0;
    return a;
}

int64_t mochi_f64_array_len(const mochi_f64_array *a) {
    return a->len;
}

void mochi_f64_array_push(mochi_f64_array *a, double v) {
    if (a->len == a->cap) {
        int64_t newcap = a->cap == 0 ? 4 : a->cap * 2;
        double *nd = (double *)realloc(a->data, (size_t)newcap * sizeof(double));
        if (nd == NULL) {
            abort();
        }
        a->data = nd;
        a->cap = newcap;
    }
    a->data[a->len++] = v;
}

double mochi_f64_array_get(const mochi_f64_array *a, int64_t i) {
    return a->data[i];
}

void mochi_f64_array_set(mochi_f64_array *a, int64_t i, double v) {
    a->data[i] = v;
}
