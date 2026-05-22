/*
 * libmochi: error model implementation.
 *
 * MEP-45 Phase 2.3. See website/docs/mep/mep-0045.md §9 and
 * website/docs/implementation/0045/phase-02-primitives-control-flow.md
 * sub-phase 2.3.
 *
 * Identity: portable C; only <stdio.h>, <stdlib.h> required. The
 * per-trip thunks are placed out-of-line so the corresponding
 * inline helpers in errors.h stay one branch + one call wide at
 * the caller's instruction stream.
 */
#include "mochi/errors.h"

#include <stdio.h>
#include <stdlib.h>

_Noreturn void mochi_panic_div_zero(void) {
    fputs("mochi: integer divide by zero\n", stderr);
    exit(MOCHI_ERR_DIVZERO);
}
