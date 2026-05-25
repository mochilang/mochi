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

mochi_list_str mochi_list_str_lit(const char **xs, int64_t n) {
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

/* ---- Phase 8.1: sort_asc and slice --------------------------------- */

#include <stdlib.h>
#include <string.h>

static int mochi_cmp_i64(const void *a, const void *b) {
    int64_t x = *(const int64_t *)a;
    int64_t y = *(const int64_t *)b;
    return (x > y) - (x < y);
}

mochi_list_i64 mochi_list_i64_sort_asc(mochi_list_i64 xs) {
    mochi_list_i64 out = mochi_list_i64_lit(xs.data, xs.len);
    qsort(out.data, (size_t)out.len, sizeof(int64_t), mochi_cmp_i64);
    return out;
}

mochi_list_i64 mochi_list_i64_slice(mochi_list_i64 xs, int64_t start, int64_t end) {
    if (start < 0) start = 0;
    if (end > xs.len) end = xs.len;
    if (start >= end) return mochi_list_i64_lit(NULL, 0);
    return mochi_list_i64_lit(xs.data + start, end - start);
}

static int mochi_cmp_f64(const void *a, const void *b) {
    double x = *(const double *)a;
    double y = *(const double *)b;
    return (x > y) - (x < y);
}

mochi_list_f64 mochi_list_f64_sort_asc(mochi_list_f64 xs) {
    mochi_list_f64 out = mochi_list_f64_lit(xs.data, xs.len);
    qsort(out.data, (size_t)out.len, sizeof(double), mochi_cmp_f64);
    return out;
}

mochi_list_f64 mochi_list_f64_slice(mochi_list_f64 xs, int64_t start, int64_t end) {
    if (start < 0) start = 0;
    if (end > xs.len) end = xs.len;
    if (start >= end) return mochi_list_f64_lit(NULL, 0);
    return mochi_list_f64_lit(xs.data + start, end - start);
}

static int mochi_cmp_bool(const void *a, const void *b) {
    int x = *(const int *)a;
    int y = *(const int *)b;
    return (x > y) - (x < y);
}

mochi_list_bool mochi_list_bool_sort_asc(mochi_list_bool xs) {
    mochi_list_bool out = mochi_list_bool_lit(xs.data, xs.len);
    qsort(out.data, (size_t)out.len, sizeof(int), mochi_cmp_bool);
    return out;
}

mochi_list_bool mochi_list_bool_slice(mochi_list_bool xs, int64_t start, int64_t end) {
    if (start < 0) start = 0;
    if (end > xs.len) end = xs.len;
    if (start >= end) return mochi_list_bool_lit(NULL, 0);
    return mochi_list_bool_lit(xs.data + start, end - start);
}

static int mochi_cmp_str(const void *a, const void *b) {
    return strcmp(*(const char *const *)a, *(const char *const *)b);
}

mochi_list_str mochi_list_str_sort_asc(mochi_list_str xs) {
    mochi_list_str out = mochi_list_str_lit(xs.data, xs.len);
    qsort(out.data, (size_t)out.len, sizeof(const char *), mochi_cmp_str);
    return out;
}

mochi_list_str mochi_list_str_slice(mochi_list_str xs, int64_t start, int64_t end) {
    if (start < 0) start = 0;
    if (end > xs.len) end = xs.len;
    if (start >= end) return mochi_list_str_lit(NULL, 0);
    return mochi_list_str_lit(xs.data + start, end - start);
}

/* ---- Phase 2.5: min / max ------------------------------------------ */

int64_t mochi_list_i64_min(mochi_list_i64 xs) {
    if (xs.len == 0) mochi_panic_index();
    int64_t v = xs.data[0];
    for (int64_t i = 1; i < xs.len; i++) if (xs.data[i] < v) v = xs.data[i];
    return v;
}

int64_t mochi_list_i64_max(mochi_list_i64 xs) {
    if (xs.len == 0) mochi_panic_index();
    int64_t v = xs.data[0];
    for (int64_t i = 1; i < xs.len; i++) if (xs.data[i] > v) v = xs.data[i];
    return v;
}

double mochi_list_f64_min(mochi_list_f64 xs) {
    if (xs.len == 0) mochi_panic_index();
    double v = xs.data[0];
    for (int64_t i = 1; i < xs.len; i++) if (xs.data[i] < v) v = xs.data[i];
    return v;
}

double mochi_list_f64_max(mochi_list_f64 xs) {
    if (xs.len == 0) mochi_panic_index();
    double v = xs.data[0];
    for (int64_t i = 1; i < xs.len; i++) if (xs.data[i] > v) v = xs.data[i];
    return v;
}

const char *mochi_list_str_min(mochi_list_str xs) {
    if (xs.len == 0) mochi_panic_index();
    const char *v = xs.data[0];
    for (int64_t i = 1; i < xs.len; i++) if (strcmp(xs.data[i], v) < 0) v = xs.data[i];
    return v;
}

const char *mochi_list_str_max(mochi_list_str xs) {
    if (xs.len == 0) mochi_panic_index();
    const char *v = xs.data[0];
    for (int64_t i = 1; i < xs.len; i++) if (strcmp(xs.data[i], v) > 0) v = xs.data[i];
    return v;
}
