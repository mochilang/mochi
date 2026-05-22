/*
 * MEP-42 Phase 4.1 C-target print runtime. See print.h for the
 * contract. The implementation is intentionally tiny so the
 * compiler3 driver can drop it into OutDir and compile it together
 * with the generated source in one cc invocation.
 */
#include "print.h"

#include <inttypes.h>
#include <stdio.h>
#include <stdlib.h>

void mochi_print_i64(int64_t x) {
    /* PRId64 is the C99-portable spelling for int64_t decimal. */
    printf("%" PRId64 "\n", x);
}

void mochi_print_bool(int x) {
    fputs(x ? "true\n" : "false\n", stdout);
}

void mochi_print_str(const char *s) {
    fputs(s, stdout);
    fputc('\n', stdout);
}

void mochi_print_f64(double x) {
    /*
     * Shortest round-trip: try precision 1..17 and pick the first
     * that strtod() reads back to the exact double. Go's
     * strconv.FormatFloat with prec=-1 uses Ryu under the hood; the
     * digits produced by %.*g with precision-search and Ryu agree
     * for the typical finite case. Edge cases where Go switches
     * fixed vs scientific differently from C %g remain divergent
     * and are tracked for Phase 4.2.
     */
    char buf[64];
    int p;
    for (p = 1; p <= 17; p++) {
        snprintf(buf, sizeof buf, "%.*g", p, x);
        double back = strtod(buf, NULL);
        if (back == x) {
            break;
        }
    }
    if (p > 17) {
        snprintf(buf, sizeof buf, "%.17g", x);
    }
    fputs(buf, stdout);
    fputc('\n', stdout);
}
