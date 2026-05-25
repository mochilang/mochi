/*
 * libmochi: error model implementation.
 *
 * MEP-45 Phase 2.3 + Phase 7.2. See website/docs/mep/mep-0045.md §9.
 *
 * Phase 7.2 routes panic thunks through mochi_raise() so that a future
 * try/catch block (Phase 7.1) can intercept them. When no try block is
 * active the behaviour is identical to the pre-7.2 direct exit() calls.
 */
#include "mochi/errors.h"
#include "mochi/except.h"

_Noreturn void mochi_panic_div_zero(void) {
    mochi_raise(MOCHI_ERR_DIVZERO, "mochi: integer divide by zero");
}

_Noreturn void mochi_panic_index(void) {
    mochi_raise(MOCHI_ERR_INDEX, "mochi: index out of range");
}

_Noreturn void mochi_panic_parse(void) {
    mochi_raise(MOCHI_ERR_PARSE, "mochi: invalid utf-8 encoding");
}
