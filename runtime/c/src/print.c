/*
 * MEP-42 Phase 4.1 C-target print runtime. See print.h for the
 * contract. The implementation is intentionally tiny so the
 * compiler3 driver can drop it into OutDir and compile it together
 * with the generated source in one cc invocation.
 */
#include "print.h"
#include "mochi_str.h"

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
     * Delegate to mochi_f64_format so print(x) and str(x) produce
     * the same digits for the same double, both matching Go's
     * strconv.FormatFloat(x, 'g', -1, 64). The pre-Phase-4.2.8 body
     * lived here (shortest-round-trip search via %.*g); the divergence
     * with Go on values like 10.0 (C produced "1e+01", Go "10") is
     * fixed in the shared helper.
     */
    char buf[64];
    int n = mochi_f64_format(buf, (int)sizeof buf, x);
    fwrite(buf, 1, (size_t)n, stdout);
    fputc('\n', stdout);
}
