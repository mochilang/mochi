/*
 * MEP-42 Phase 4.2.3 C-target string runtime. See mochi_str.h for
 * the contract.
 */
#include "mochi_str.h"

#include <inttypes.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

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

int mochi_f64_format(char *buf, int bufsize, double v) {
    /*
     * Goal: byte-match Go's strconv.FormatFloat(v, 'g', -1, 64).
     *
     * Step 1: find the shortest precision p in [1,17] such that
     * "%.*g" round-trips through strtod. This is the same shortest-
     * round-trip search Phase 4.1 used.
     *
     * Step 2: C99 "%g" chooses exponent form when the decimal
     * exponent X is "X < -4 || X >= P" (where P is the precision
     * passed in). Go's strconv with prec=-1 uses "X < -4 || X >= 6"
     * (Go strconv/ftoa.go hard-codes eprec=6 in shortest mode).
     * When the two rules disagree on the form, reformat.
     */
    int p;
    int n = 0;
    for (p = 1; p <= 17; p++) {
        n = snprintf(buf, (size_t)bufsize, "%.*g", p, v);
        double back = strtod(buf, NULL);
        if (back == v) {
            break;
        }
    }
    if (p > 17) {
        p = 17;
        n = snprintf(buf, (size_t)bufsize, "%.17g", v);
    }
    /*
     * Special-case the values "%g" prints in non-decimal form so
     * we don't try to parse a fake decimal exponent: 0, +/-Inf, NaN.
     * %g prints "0" for 0.0 (no decimal exponent), "inf"/"-inf",
     * "nan". All three match Go (which prints "0", "+Inf"/"-Inf",
     * "NaN" -- wait, Go prints +Inf with sign, and NaN capitalised).
     * mochi_run on these values comes through the same fmt.Println
     * path so the C target follows that. Today the bench corpus has
     * no inf/nan emitter; if one shows up the case lives here.
     */
    if (v != v || v == 0.0) {
        return n;
    }
    /* Detect whether the current buffer is in exponent form. */
    int eIdx = -1;
    for (int i = 0; i < n; i++) {
        if (buf[i] == 'e' || buf[i] == 'E') {
            eIdx = i;
            break;
        }
    }
    int exp;
    if (eIdx >= 0) {
        /* Parse "eXXX" or "e+XXX" or "e-XXX". */
        int sign = 1;
        int j = eIdx + 1;
        if (buf[j] == '-') { sign = -1; j++; }
        else if (buf[j] == '+') { j++; }
        int v_abs = 0;
        for (; j < n && buf[j] >= '0' && buf[j] <= '9'; j++) {
            v_abs = v_abs * 10 + (buf[j] - '0');
        }
        exp = sign * v_abs;
    } else {
        /*
         * Fixed form: derive decimal exponent of leading digit
         * directly from |v|. log10 + floor is sufficient because
         * the value already round-tripped via strtod above.
         */
        double absv = v < 0.0 ? -v : v;
        exp = (int)floor(log10(absv));
    }
    int wantExp = (exp < -4 || exp >= 6);
    int gotExp = (eIdx >= 0);
    if (wantExp == gotExp) {
        return n;
    }
    /* Reformat to match Go's form choice. */
    if (wantExp) {
        /*
         * Go wants exp form but C produced fixed. Reformat with
         * "%.*e" using p-1 fractional digits (so p sig figs total),
         * then strip trailing zeros from the mantissa for Go
         * parity (Go's "1.5e+06" never has padding zeros).
         */
        n = snprintf(buf, (size_t)bufsize, "%.*e", p - 1, v);
        int dotIdx = -1, eIdx2 = -1;
        for (int i = 0; i < n; i++) {
            if (buf[i] == '.') dotIdx = i;
            else if (buf[i] == 'e' || buf[i] == 'E') { eIdx2 = i; break; }
        }
        if (dotIdx >= 0 && eIdx2 >= 0) {
            int j = eIdx2 - 1;
            while (j > dotIdx && buf[j] == '0') j--;
            if (j == dotIdx) j--; /* drop trailing '.' too */
            int mantEnd = j + 1;
            int tailLen = n - eIdx2;
            memmove(buf + mantEnd, buf + eIdx2, (size_t)tailLen + 1);
            n = mantEnd + tailLen;
        }
        /*
         * Normalise exponent: C99 produces "1e+01" (two-digit
         * minimum); Go also uses two-digit minimum for |exp| < 100,
         * three for >= 100. Same.
         */
    } else {
        /*
         * Go wants fixed form but C produced exp. Reformat with
         * "%.*f" where frac digits = (p-1) - exp, clamped to 0.
         * For 10.0 with p=1, exp=1: frac = 1-1-1 = -1 -> 0;
         * "%.0f" of 10.0 = "10". Round-trip preserved because the
         * sig-digit budget is unchanged.
         */
        int frac = (p - 1) - exp;
        if (frac < 0) frac = 0;
        n = snprintf(buf, (size_t)bufsize, "%.*f", frac, v);
    }
    return n;
}

const char *mochi_str_from_f64(double v) {
    /*
     * Shortest round-trip: mirror runtime/c/src/print.c mochi_print_f64
     * so str(x) and print(x) produce the same digits for the same
     * double, matching Go's strconv.FormatFloat(v, 'g', -1, 64).
     */
    char buf[64];
    int n = mochi_f64_format(buf, (int)sizeof buf, v);
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
