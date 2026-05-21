/*
 * MEP-42 Phase 4.1 C-target print runtime.
 *
 * This header is consumed by the C source compiler3/emit/c produces.
 * Each helper formats one Mochi value to stdout the same way Go's
 * `fmt.Println(value)` does, so a Mochi program compiled with
 * `mochi build --target=c` byte-matches the same source run under
 * `mochi run` (which uses the Go fmt path through runtime/mochi/fmt).
 *
 * Phase 4.1 covers the scalar types the compiler3 MVP frontend can
 * lower to a print() call argument: int64, float64, bool. String
 * literals (and string-returning expressions) require TypeStr
 * support in the frontend; that lands in Phase 4.2 alongside
 * runtime/c/str.{h,c}.
 *
 * Identity: no libc beyond stdio.h/stdint.h/stdbool.h is required.
 * The whole runtime is portable C99.
 */
#ifndef MOCHI_RUNTIME_C_PRINT_H
#define MOCHI_RUNTIME_C_PRINT_H

#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

/* mochi_print_i64 writes the decimal form of x followed by '\n'. */
void mochi_print_i64(int64_t x);

/* mochi_print_bool writes "true\n" or "false\n". */
void mochi_print_bool(int x);

/*
 * mochi_print_f64 writes x using the shortest representation that
 * round-trips through strtod, followed by '\n'. The output matches
 * Go's `fmt.Println(float64)` for normal finite values where Go's
 * default %v format and C's %g with the round-trip search agree.
 * Edge cases (Inf, NaN, large fixed-vs-scientific cross-over) are
 * documented as known divergence in MEP-42 §10.5; Phase 4.2
 * narrows the gap.
 */
void mochi_print_f64(double x);

#ifdef __cplusplus
}
#endif

#endif /* MOCHI_RUNTIME_C_PRINT_H */
