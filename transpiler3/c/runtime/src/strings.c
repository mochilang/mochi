/*
 * libmochi: string operation implementation.
 *
 * MEP-45 Phase 6.0: mochi_str_cat.
 * MEP-45 Phase 6.1: mochi_str_index, mochi_str_contains,
 *                   mochi_str_substring, mochi_str_reverse.
 *
 * All functions that return strings return freshly malloc'd buffers.
 * Memory is never freed in Phase 6.x (deferred to Phase 17 GC).
 *
 * Phase 6.1 operates on bytes (ASCII). Full UTF-8 codepoint support
 * via utf8proc is Phase 6.2.
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

const char *mochi_str_index(const char *s, int64_t i) {
    if (s == NULL) return "";
    size_t n = strlen(s);
    if (i < 0 || (size_t)i >= n) return "";
    char *out = malloc(2);
    if (out == NULL) return "";
    out[0] = s[i];
    out[1] = '\0';
    return out;
}

int mochi_str_contains(const char *s, const char *sub) {
    if (s == NULL || sub == NULL) return 0;
    return strstr(s, sub) != NULL ? 1 : 0;
}

const char *mochi_str_substring(const char *s, int64_t start, int64_t end) {
    if (s == NULL) return "";
    int64_t n = (int64_t)strlen(s);
    if (start < 0) start = 0;
    if (end > n) end = n;
    if (start >= end) return "";
    size_t len = (size_t)(end - start);
    char *out = malloc(len + 1);
    if (out == NULL) return "";
    memcpy(out, s + start, len);
    out[len] = '\0';
    return out;
}

const char *mochi_str_reverse(const char *s) {
    if (s == NULL) return "";
    size_t n = strlen(s);
    char *out = malloc(n + 1);
    if (out == NULL) return "";
    for (size_t i = 0; i < n; i++) {
        out[i] = s[n - 1 - i];
    }
    out[n] = '\0';
    return out;
}
