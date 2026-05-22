/*
 * libmochi: print runtime implementation.
 *
 * MEP-45 Phase 1 + 2.0. See website/docs/mep/mep-0045.md.
 *
 * Identity: portable C; only <stdio.h>, <inttypes.h> required.
 * The float printer uses %.17g for now (Phase 2.0 fixtures all
 * print values whose shortest round-trip happens to be the
 * default Go format). Phase 2.4 replaces it with a strconv
 * 'g' -1 64 equivalent that matches vm3 byte-for-byte for the
 * full IEEE 754 range including NaN and Inf.
 */
#include "mochi/print.h"

#include <inttypes.h>
#include <stdio.h>

void mochi_print_str(const char *s) {
    if (s != NULL) {
        fputs(s, stdout);
    }
    fputc('\n', stdout);
}

void mochi_print_i64(int64_t x) {
    printf("%" PRId64 "\n", x);
}

void mochi_print_f64(double x) {
    printf("%.17g\n", x);
}

void mochi_print_bool(int x) {
    fputs(x ? "true\n" : "false\n", stdout);
}
