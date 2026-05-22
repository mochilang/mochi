/*
 * MEP-42 Phase 4.2.3 C-target string runtime. See mochi_str.h for
 * the contract.
 */
#include "mochi_str.h"

#include <inttypes.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

const char *mochi_str_concat(const char *a, const char *b) {
    size_t la = strlen(a);
    size_t lb = strlen(b);
    /*
     * The +1 reserves room for the NUL terminator so the result
     * remains a valid C string for downstream strlen / strcmp /
     * fputs calls (the Phase 4.2.0-4.2.2 string ops all read
     * through the NUL-terminated path).
     */
    char *out = (char *)malloc(la + lb + 1);
    memcpy(out, a, la);
    memcpy(out + la, b, lb);
    out[la + lb] = '\0';
    return out;
}

const char *mochi_str_from_i64(long long v) {
    /*
     * INT64_MIN renders as "-9223372036854775808" (20 chars + NUL),
     * so a 24-byte buffer covers every i64 with margin.
     */
    char buf[24];
    int n = snprintf(buf, sizeof buf, "%" PRId64, (int64_t)v);
    char *out = (char *)malloc((size_t)n + 1);
    memcpy(out, buf, (size_t)n + 1);
    return out;
}

const char *mochi_str_from_f64(double v) {
    /*
     * Shortest round-trip: mirror runtime/c/src/print.c mochi_print_f64
     * so str(x) and print(x) produce the same digits for the same
     * double, matching Go's strconv.FormatFloat(v, 'g', -1, 64).
     */
    char buf[64];
    int p;
    int n = 0;
    for (p = 1; p <= 17; p++) {
        n = snprintf(buf, sizeof buf, "%.*g", p, v);
        double back = strtod(buf, NULL);
        if (back == v) {
            break;
        }
    }
    if (p > 17) {
        n = snprintf(buf, sizeof buf, "%.17g", v);
    }
    char *out = (char *)malloc((size_t)n + 1);
    memcpy(out, buf, (size_t)n + 1);
    return out;
}

const char *mochi_str_from_bool(int v) {
    /*
     * Returns one of two static C99 string literals. No allocation:
     * the carrier discipline (everywhere a `const char*` is treated
     * as borrowed) makes the static-literal-vs-heap distinction
     * invisible to the Mochi level.
     */
    return v ? "true" : "false";
}
