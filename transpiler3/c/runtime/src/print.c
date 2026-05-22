/*
 * libmochi: print runtime implementation.
 *
 * MEP-45 Phase 1. See website/docs/mep/mep-0045.md.
 *
 * Identity: portable C; only <stdio.h> required.
 */
#include "mochi/print.h"

#include <stdio.h>

void mochi_print_str(const char *s) {
    if (s != NULL) {
        fputs(s, stdout);
    }
    fputc('\n', stdout);
}
