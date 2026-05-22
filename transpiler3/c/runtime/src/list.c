/*
 * libmochi: list runtime implementation.
 *
 * MEP-45 Phase 3.1. See website/docs/mep/mep-0045.md and
 * website/docs/implementation/0045/phase-03-records-collections.md
 * sub-phase 3.1.
 *
 * Identity. Portable C; only <stdint.h>, <stdlib.h>, <string.h>
 * required. Each helper is the canonical lowering target for one
 * Mochi list operation (literal, append, index, len) at one
 * concrete element type. Per-T duplication is intentional in 3.1
 * to keep the surface declarative; the monomorphisation pass
 * (Phase 3.4) takes over once map / set arrive.
 *
 * Memory ownership. Each list value owns a freshly-malloced
 * backing buffer; pass-by-value of the struct copies the header
 * (data pointer + len + cap) but not the buffer, so two list
 * values can transiently share the same buffer. The append path
 * always allocates a new buffer, copies, and writes the new
 * element, so subsequent mutations never escape into the input
 * value. Free is deferred to Phase 7 (panic unwinding); 3.1
 * intentionally leaks on program exit, which the OS reclaims.
 */
#include "mochi/list.h"

#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#include "mochi/errors.h"

/* --- int (int64_t) -------------------------------------------------- */

mochi_list_i64 mochi_list_i64_lit(const int64_t *xs, int64_t n) {
    mochi_list_i64 out = {NULL, n, n};
    if (n > 0) {
        out.data = (int64_t *)malloc((size_t)n * sizeof(int64_t));
        if (out.data == NULL) {
            mochi_panic_index();
        }
        memcpy(out.data, xs, (size_t)n * sizeof(int64_t));
    }
    return out;
}

mochi_list_i64 mochi_list_i64_append(mochi_list_i64 xs, int64_t v) {
    mochi_list_i64 out = {NULL, xs.len + 1, xs.len + 1};
    out.data = (int64_t *)malloc((size_t)out.cap * sizeof(int64_t));
    if (out.data == NULL) {
        mochi_panic_index();
    }
    if (xs.len > 0) {
        memcpy(out.data, xs.data, (size_t)xs.len * sizeof(int64_t));
    }
    out.data[xs.len] = v;
    return out;
}

int64_t mochi_list_i64_index(mochi_list_i64 xs, int64_t i) {
    if (i < 0 || i >= xs.len) {
        mochi_panic_index();
    }
    return xs.data[i];
}

int64_t mochi_list_i64_len(mochi_list_i64 xs) { return xs.len; }

/* --- float (double) ------------------------------------------------- */

mochi_list_f64 mochi_list_f64_lit(const double *xs, int64_t n) {
    mochi_list_f64 out = {NULL, n, n};
    if (n > 0) {
        out.data = (double *)malloc((size_t)n * sizeof(double));
        if (out.data == NULL) {
            mochi_panic_index();
        }
        memcpy(out.data, xs, (size_t)n * sizeof(double));
    }
    return out;
}

mochi_list_f64 mochi_list_f64_append(mochi_list_f64 xs, double v) {
    mochi_list_f64 out = {NULL, xs.len + 1, xs.len + 1};
    out.data = (double *)malloc((size_t)out.cap * sizeof(double));
    if (out.data == NULL) {
        mochi_panic_index();
    }
    if (xs.len > 0) {
        memcpy(out.data, xs.data, (size_t)xs.len * sizeof(double));
    }
    out.data[xs.len] = v;
    return out;
}

double mochi_list_f64_index(mochi_list_f64 xs, int64_t i) {
    if (i < 0 || i >= xs.len) {
        mochi_panic_index();
    }
    return xs.data[i];
}

int64_t mochi_list_f64_len(mochi_list_f64 xs) { return xs.len; }

/* --- bool (int 0/1) ------------------------------------------------- */

mochi_list_bool mochi_list_bool_lit(const int *xs, int64_t n) {
    mochi_list_bool out = {NULL, n, n};
    if (n > 0) {
        out.data = (int *)malloc((size_t)n * sizeof(int));
        if (out.data == NULL) {
            mochi_panic_index();
        }
        memcpy(out.data, xs, (size_t)n * sizeof(int));
    }
    return out;
}

mochi_list_bool mochi_list_bool_append(mochi_list_bool xs, int v) {
    mochi_list_bool out = {NULL, xs.len + 1, xs.len + 1};
    out.data = (int *)malloc((size_t)out.cap * sizeof(int));
    if (out.data == NULL) {
        mochi_panic_index();
    }
    if (xs.len > 0) {
        memcpy(out.data, xs.data, (size_t)xs.len * sizeof(int));
    }
    out.data[xs.len] = v ? 1 : 0;
    return out;
}

int mochi_list_bool_index(mochi_list_bool xs, int64_t i) {
    if (i < 0 || i >= xs.len) {
        mochi_panic_index();
    }
    return xs.data[i];
}

int64_t mochi_list_bool_len(mochi_list_bool xs) { return xs.len; }

/* --- string (const char *) ----------------------------------------- */

mochi_list_str mochi_list_str_lit(const char *const *xs, int64_t n) {
    mochi_list_str out = {NULL, n, n};
    if (n > 0) {
        out.data = (const char **)malloc((size_t)n * sizeof(const char *));
        if (out.data == NULL) {
            mochi_panic_index();
        }
        memcpy(out.data, xs, (size_t)n * sizeof(const char *));
    }
    return out;
}

mochi_list_str mochi_list_str_append(mochi_list_str xs, const char *v) {
    mochi_list_str out = {NULL, xs.len + 1, xs.len + 1};
    out.data = (const char **)malloc((size_t)out.cap * sizeof(const char *));
    if (out.data == NULL) {
        mochi_panic_index();
    }
    if (xs.len > 0) {
        memcpy(out.data, xs.data, (size_t)xs.len * sizeof(const char *));
    }
    out.data[xs.len] = v;
    return out;
}

const char *mochi_list_str_index(mochi_list_str xs, int64_t i) {
    if (i < 0 || i >= xs.len) {
        mochi_panic_index();
    }
    return xs.data[i];
}

int64_t mochi_list_str_len(mochi_list_str xs) { return xs.len; }
