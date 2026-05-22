/*
 * MEP-42 Phase 4.3.3 typed-f64 array runtime. See mochi_f64_array.h.
 */
#include "mochi_f64_array.h"

#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

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

/*
 * format_f64_decimal writes the shortest decimal representation of v
 * in Go's strconv.FormatFloat(v, 'f', -1, 64) form into buf, then
 * appends a ".0" suffix if the result has no decimal point. Used by
 * mochi_f64_array_to_str so list<float> elements byte-match the VM's
 * valueToString rule (FormatFloat 'f' -1 64 + ".0" for integral).
 *
 * Algorithm: scan p in [0,17] for the smallest precision where
 * "%.*f" round-trips through strtod. That p is exactly the number
 * of decimal places needed; %.*f never emits trailing zeros that
 * could be stripped while still round-tripping (because reducing p
 * by 1 would still round-trip, contradicting "smallest"). When p=0
 * the value is integral, so append ".0".
 *
 * Special cases:
 *   - 0.0 / -0.0 -> "0" + ".0" via p=0
 *   - NaN -> "nan"  (snprintf %f), left unchanged (no list bench
 *     fixture today; matches the same gap mochi_f64_format leaves)
 *   - +/-Inf -> "inf" / "-inf", same rationale
 *
 * Buffer: 32 bytes covers values up to ~1e20 in 'f' form (20 digits
 * + ".0" + sign + NUL). Caller passes >=32-byte buf for the typical
 * range; extreme magnitudes (|v| > 1e25 in fixed form) would need
 * more, but the bench corpus does not exercise those.
 */
static int format_f64_decimal(char *buf, int bufsize, double v) {
    /* Special cases that printf prints as text, not numbers: leave
     * them as-is so we don't append a stray ".0" to "nan"/"inf". */
    if (v != v) { return snprintf(buf, (size_t)bufsize, "nan"); }
    if (v > 0 && v / v != v / v) { return snprintf(buf, (size_t)bufsize, "+inf"); }
    if (v < 0 && v / v != v / v) { return snprintf(buf, (size_t)bufsize, "-inf"); }

    int p;
    int n = 0;
    for (p = 0; p <= 17; p++) {
        n = snprintf(buf, (size_t)bufsize, "%.*f", p, v);
        double back = strtod(buf, NULL);
        if (back == v) {
            break;
        }
    }
    if (p > 17) {
        p = 17;
        n = snprintf(buf, (size_t)bufsize, "%.*f", p, v);
    }
    if (p == 0) {
        /* Integral value: snprintf gave "N" with no decimal point.
         * Append ".0" so the rendering matches the VM's list rule. */
        if (n + 2 < bufsize) {
            buf[n++] = '.';
            buf[n++] = '0';
            buf[n] = '\0';
        }
    }
    return n;
}

mochi_f64_array *mochi_f64_array_concat(const mochi_f64_array *a, const mochi_f64_array *b) {
    mochi_f64_array *out = mochi_f64_array_new();
    int64_t total = a->len + b->len;
    if (total > 0) {
        double *nd = (double *)malloc((size_t)total * sizeof(double));
        if (nd == NULL) {
            abort();
        }
        if (a->len > 0) {
            for (int64_t i = 0; i < a->len; i++) {
                nd[i] = a->data[i];
            }
        }
        if (b->len > 0) {
            for (int64_t i = 0; i < b->len; i++) {
                nd[a->len + i] = b->data[i];
            }
        }
        out->data = nd;
        out->cap = total;
        out->len = total;
    }
    return out;
}

const char *mochi_f64_array_to_str(const mochi_f64_array *a) {
    if (a->len == 0) {
        return "[]";
    }
    /* Format each element into a temporary buffer, then assemble the
     * "[e1, e2, ...]" result with a single malloc. We size the result
     * from the actual rendered lengths so values like 1e20 (22 chars
     * in 'f' form) and 0.001 (5 chars) both fit without overshooting.
     * 64 bytes per element is enough for |v| up to ~1e30 in fixed
     * form; the bench corpus and tutorial fixtures stay well below. */
    char elem[64];
    /* First pass: compute total byte budget so the result malloc
     * matches the rendered length exactly (no realloc, no over-
     * allocation). Track each element's length in a small heap
     * scratch so the second pass can copy without re-rendering. */
    int *lens = (int *)malloc((size_t)a->len * sizeof(int));
    if (lens == NULL) abort();
    size_t total = 2;                              /* '[' + ']' */
    if (a->len > 1) total += (size_t)(a->len - 1) * 2; /* ", " * (n-1) */
    for (int64_t i = 0; i < a->len; i++) {
        lens[i] = format_f64_decimal(elem, (int)sizeof elem, a->data[i]);
        if (lens[i] < 0) abort();
        total += (size_t)lens[i];
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
        int n = format_f64_decimal(elem, (int)sizeof elem, a->data[i]);
        if (n < 0) abort();
        memcpy(buf + off, elem, (size_t)n);
        off += (size_t)n;
    }
    buf[off++] = ']';
    buf[off] = '\0';
    free(lens);
    return buf;
}
