/*
 * libmochi: error model + arithmetic safety helpers.
 *
 * MEP-45 Phase 2.3. See website/docs/mep/mep-0045.md §9.
 *
 * Built-in error codes are the MOCHI_ERR_* macros below. The MEP
 * spec assigns them as negative numerics (-1, -2, ...) inside the
 * Mochi error namespace; here they are the corresponding positive
 * Unix exit codes (abs of the spec value), since Unix exit status
 * is 8-bit unsigned and we want a memorable number rather than the
 * wrap value. The trip helpers (`mochi_panic_*`) call exit() with
 * these values directly.
 *
 * Phase 2.3 ships only the divide-by-zero pair. Later phases add
 * the rest (index OOB, type, fetch, parse, ...). Adding a code
 * means: define MOCHI_ERR_<NAME> here, add a mochi_panic_<name>
 * thunk that calls fputs + exit, and route the relevant emit path
 * through it.
 *
 * ABI stability: every symbol here is part of libmochi's versioned
 * surface. Existing prototypes never change shape.
 */
#ifndef MOCHI_ERRORS_H
#define MOCHI_ERRORS_H

#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

/* Unix exit codes for the built-in error namespace (MEP-45 §9). */
#define MOCHI_ERR_FETCH    1
#define MOCHI_ERR_PARSE    2
#define MOCHI_ERR_TYPE     3
#define MOCHI_ERR_INDEX    4
#define MOCHI_ERR_DIVZERO  5
#define MOCHI_ERR_OVERFLOW 6
#define MOCHI_ERR_FFI      7
#define MOCHI_ERR_LLM      8
#define MOCHI_ERR_ASSERT   9

/*
 * mochi_panic_div_zero writes a fixed diagnostic
 * ("mochi: integer divide by zero\n") to stderr and exits with
 * MOCHI_ERR_DIVZERO. Marked _Noreturn so the optimiser drops the
 * dead block after each call site.
 */
_Noreturn void mochi_panic_div_zero(void);

/*
 * mochi_panic_index writes a fixed diagnostic
 * ("mochi: index out of range\n") to stderr and exits with
 * MOCHI_ERR_INDEX. Used by the list / string index helpers when
 * a bounds check fails.
 */
_Noreturn void mochi_panic_index(void);

/*
 * mochi_panic_parse writes a fixed diagnostic
 * ("mochi: invalid utf-8 encoding\n") to stderr and exits with
 * MOCHI_ERR_PARSE (2). Called by mochi_read_file when the file
 * content is not valid UTF-8 (Phase 6.6).
 */
_Noreturn void mochi_panic_parse(void);

/*
 * mochi_div_i64 returns lhs / rhs after checking rhs against zero.
 * On rhs == 0 it tail-calls mochi_panic_div_zero (which never
 * returns); otherwise it returns the truncated C-semantics quotient.
 * Inlined so the divzero check sits at the arithmetic call site
 * without a function-call hop on the hot path.
 */
static inline int64_t mochi_div_i64(int64_t lhs, int64_t rhs) {
    if (rhs == 0) {
        mochi_panic_div_zero();
    }
    return lhs / rhs;
}

/*
 * mochi_mod_i64: sibling of mochi_div_i64 for the C `%` operator.
 * vm3 raises the same ErrDivByZero for OpDivI64 and OpModI64, so
 * both routes share mochi_panic_div_zero.
 */
static inline int64_t mochi_mod_i64(int64_t lhs, int64_t rhs) {
    if (rhs == 0) {
        mochi_panic_div_zero();
    }
    return lhs % rhs;
}

#ifdef __cplusplus
}
#endif

#endif /* MOCHI_ERRORS_H */
