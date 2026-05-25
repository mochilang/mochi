/*
 * libmochi: string operation declarations.
 *
 * MEP-45 Phase 6.0: mochi_str_cat concatenates two NUL-terminated C
 * strings and returns a freshly malloc'd result. The caller owns the
 * memory; in Phase 6.0 it is never freed (arena / GC deferred).
 *
 * MEP-45 Phase 6.1: string indexing, contains, substring, reverse.
 * All returned strings are freshly malloc'd; memory deferred to Phase 17 GC.
 */
#pragma once

#include <stdint.h>

/* Phase 6.0 */
const char *mochi_str_cat(const char *a, const char *b);

/* Phase 6.1 */
/* mochi_str_index(s, i) -- one-character string at rune index i, or "" if OOB. */
const char *mochi_str_index(const char *s, int64_t i);
/* mochi_str_contains(s, sub) -- 1 if sub is a substring of s, else 0. */
int mochi_str_contains(const char *s, const char *sub);
/* mochi_str_substring(s, start, end) -- rune-slice [start, end). */
const char *mochi_str_substring(const char *s, int64_t start, int64_t end);
/* mochi_str_reverse(s) -- reversed copy of s. */
const char *mochi_str_reverse(const char *s);

/* Phase 6.2: str(x) type-to-string conversion. */
/* mochi_str_from_i64(v) -- decimal string for a signed 64-bit integer. */
const char *mochi_str_from_i64(int64_t v);
/* mochi_str_from_f64(v) -- shortest decimal representation matching vm3. */
const char *mochi_str_from_f64(double v);
/* mochi_str_from_bool(v) -- "true" or "false". */
const char *mochi_str_from_bool(int v);
