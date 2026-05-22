/*
 * libmochi: print runtime implementation.
 *
 * MEP-45 Phase 1 + 2.0 + 2.4. See website/docs/mep/mep-0045.md.
 *
 * Identity: portable C; only <stdio.h>, <inttypes.h>, <math.h>
 * required. <math.h> is included for the isnan / isinf C99
 * macros; on every tier-1 toolchain these expand to compiler
 * intrinsics without needing -lm at link time.
 *
 * The float printer dispatches NaN / Inf to their Go-equivalent
 * spellings (Phase 2.4) and routes finite values through %.17g
 * (Phase 2.0 carry-over). Shortest-round-trip rendering for
 * arbitrary finite doubles is a later sub-phase.
 */
#include "mochi/print.h"

#include <inttypes.h>
#include <math.h>
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
    if (isnan(x)) {
        fputs("NaN\n", stdout);
        return;
    }
    if (isinf(x)) {
        fputs(x > 0 ? "+Inf\n" : "-Inf\n", stdout);
        return;
    }
    printf("%.17g\n", x);
}

void mochi_print_bool(int x) {
    fputs(x ? "true\n" : "false\n", stdout);
}
