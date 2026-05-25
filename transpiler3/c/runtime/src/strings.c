/*
 * libmochi: string operation implementation.
 *
 * MEP-45 Phase 6.0.
 */
#include "mochi/strings.h"

#include <stddef.h>
#include <stdlib.h>
#include <string.h>

const char *mochi_str_cat(const char *a, const char *b) {
    if (a == NULL) a = "";
    if (b == NULL) b = "";
    size_t la = strlen(a);
    size_t lb = strlen(b);
    char *out = malloc(la + lb + 1);
    if (out == NULL) return "";
    memcpy(out, a, la);
    memcpy(out + la, b, lb);
    out[la + lb] = '\0';
    return out;
}
