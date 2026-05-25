/*
 * libmochi: string operation implementation.
 *
 * MEP-45 Phase 6.0: mochi_str_cat.
 * MEP-45 Phase 6.1: mochi_str_index, mochi_str_contains,
 *                   mochi_str_substring, mochi_str_reverse.
 * MEP-45 Phase 6.2: mochi_str_from_i64, mochi_str_from_f64,
 *                   mochi_str_from_bool.
 * MEP-45 Phase 6.3: mochi_str_upper, mochi_str_lower, mochi_str_split,
 *                   mochi_str_join. ASCII-only; full UTF-8 codepoint support
 *                   via utf8proc is deferred.
 * MEP-45 Phase 6.6: mochi_utf8_valid -- pure-C UTF-8 byte-sequence validator.
 *
 * All functions that return strings return freshly malloc'd buffers.
 * Memory is never freed in Phase 6.x (deferred to Phase 17 GC).
 */
#include "mochi/strings.h"

#include <ctype.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
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

/* ---- Phase 6.2: str(x) type-to-string conversion ------------------- */

const char *mochi_str_from_i64(int64_t v) {
    /* PRId64 on most platforms is "ld" or "lld"; snprintf handles it. */
    char buf[32];
    int n = snprintf(buf, sizeof(buf), "%lld", (long long)v);
    if (n < 0 || (size_t)n >= sizeof(buf)) return "";
    char *out = malloc((size_t)n + 1);
    if (out == NULL) return "";
    memcpy(out, buf, (size_t)n + 1);
    return out;
}

const char *mochi_str_from_f64(double v) {
    /* Use %g to match Go's fmt.Sprint(float64) shortest-decimal style.
     * %g removes trailing zeros and the decimal point when not needed,
     * so str(1.0) == "1", str(3.14) == "3.14", str(0.1) == "0.1". */
    char buf[64];
    int n = snprintf(buf, sizeof(buf), "%g", v);
    if (n < 0 || (size_t)n >= sizeof(buf)) return "";
    char *out = malloc((size_t)n + 1);
    if (out == NULL) return "";
    memcpy(out, buf, (size_t)n + 1);
    return out;
}

const char *mochi_str_from_bool(int v) {
    return v ? "true" : "false";
}

/* ---- Phase 6.3: case-conversion and split/join --------------------- */

const char *mochi_str_upper(const char *s) {
    if (s == NULL) return "";
    size_t n = strlen(s);
    char *out = malloc(n + 1);
    if (out == NULL) return "";
    for (size_t i = 0; i < n; i++) {
        out[i] = (char)toupper((unsigned char)s[i]);
    }
    out[n] = '\0';
    return out;
}

const char *mochi_str_lower(const char *s) {
    if (s == NULL) return "";
    size_t n = strlen(s);
    char *out = malloc(n + 1);
    if (out == NULL) return "";
    for (size_t i = 0; i < n; i++) {
        out[i] = (char)tolower((unsigned char)s[i]);
    }
    out[n] = '\0';
    return out;
}

mochi_list_str mochi_str_split(const char *s, const char *sep) {
    mochi_list_str result = mochi_list_str_lit(NULL, 0);
    if (s == NULL || sep == NULL) return result;
    size_t seplen = strlen(sep);
    if (seplen == 0) {
        /* Empty separator: split into individual characters. */
        size_t n = strlen(s);
        for (size_t i = 0; i < n; i++) {
            char *ch = malloc(2);
            if (ch == NULL) return result;
            ch[0] = s[i];
            ch[1] = '\0';
            result = mochi_list_str_append(result, ch);
        }
        return result;
    }
    const char *p = s;
    while (1) {
        const char *found = strstr(p, sep);
        if (found == NULL) {
            result = mochi_list_str_append(result, p);
            break;
        }
        size_t len = (size_t)(found - p);
        char *tok = malloc(len + 1);
        if (tok == NULL) return result;
        memcpy(tok, p, len);
        tok[len] = '\0';
        result = mochi_list_str_append(result, tok);
        p = found + seplen;
    }
    return result;
}

const char *mochi_str_join(mochi_list_str xs, const char *sep) {
    if (sep == NULL) sep = "";
    int64_t n = mochi_list_str_len(xs);
    if (n == 0) {
        char *out = malloc(1);
        if (out == NULL) return "";
        out[0] = '\0';
        return out;
    }
    size_t seplen = strlen(sep);
    /* First pass: compute total length. */
    size_t total = 0;
    for (int64_t i = 0; i < n; i++) {
        const char *elem = mochi_list_str_index(xs, i);
        if (elem == NULL) elem = "";
        total += strlen(elem);
    }
    total += (size_t)(n - 1) * seplen;
    char *out = malloc(total + 1);
    if (out == NULL) return "";
    char *p = out;
    for (int64_t i = 0; i < n; i++) {
        if (i > 0) {
            memcpy(p, sep, seplen);
            p += seplen;
        }
        const char *elem = mochi_list_str_index(xs, i);
        if (elem == NULL) elem = "";
        size_t elen = strlen(elem);
        memcpy(p, elem, elen);
        p += elen;
    }
    *p = '\0';
    return out;
}

/* Phase 6.6: UTF-8 validation (Bancilhon-style state machine).
 *
 * Accepts code points U+0000-U+10FFFF, rejects:
 *   - Continuation bytes (0x80-0xBF) without a lead byte.
 *   - Overlong 2-byte sequences (lead 0xC0-0xC1).
 *   - Code points above U+10FFFF (lead >= 0xF5).
 *   - Truncated multi-byte sequences (missing continuation bytes).
 *
 * Does NOT reject UTF-16 surrogates (U+D800-U+DFFF); surrogate filtering
 * is deferred to the utf8proc integration in a later phase.
 */
int mochi_utf8_valid(const char *s, size_t n) {
    const unsigned char *u = (const unsigned char *)s;
    size_t i = 0;
    while (i < n) {
        unsigned char c = u[i];
        size_t extra;
        if      (c < 0x80u)                    { extra = 0; }
        else if (c < 0xC2u || c > 0xF4u)       { return 0; }
        else if (c < 0xE0u)                    { extra = 1; }
        else if (c < 0xF0u)                    { extra = 2; }
        else                                   { extra = 3; }
        i++;
        for (size_t j = 0; j < extra; j++, i++) {
            if (i >= n || (u[i] & 0xC0u) != 0x80u) {
                return 0;
            }
        }
    }
    return 1;
}
