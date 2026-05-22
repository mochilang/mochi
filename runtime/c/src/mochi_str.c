/*
 * MEP-42 Phase 4.2.3 C-target string runtime. See mochi_str.h for
 * the contract.
 */
#include "mochi_str.h"

#include <stdlib.h>
#include <string.h>

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
