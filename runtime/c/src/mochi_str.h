/*
 * MEP-42 Phase 4.2.3 C-target string runtime.
 *
 * Backs Mochi's `+` operator on TypeStr when the program is
 * compiled with `mochi build --target=c`. Phase 4.2.0-4.2.2 used
 * `const char*` carriers pointing at C99 read-only string-literal
 * storage; that aliasing model worked for literals, print, len,
 * and strcmp because none of them allocated. Concat is the first
 * op whose result has no source-pointed literal to alias, so the
 * runtime allocates a NUL-terminated heap buffer and hands back
 * a pointer the caller treats as `const char*`.
 *
 * Identity: pure C99, no libc beyond stdlib.h/string.h. The
 * runtime is dropped next to gen.c by the build driver and
 * compiled in the same cc invocation; no separate library.
 *
 * Ownership: the returned buffer is heap-resident and currently
 * leaks at process exit. Mochi-on-C MVP programs run to completion
 * in seconds (the bench corpus), so a leak in a tight concat loop
 * is documented as a known limitation; a later 4.2.x sub-phase
 * adds an arena once a long-running fixture surfaces.
 */
#ifndef MOCHI_RUNTIME_C_STR_H
#define MOCHI_RUNTIME_C_STR_H

#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

/*
 * mochi_str_concat returns a freshly allocated NUL-terminated byte
 * sequence containing the bytes of a followed by the bytes of b.
 * The result is owned by the runtime (currently leaked); callers
 * must not free() it. Both inputs are NUL-terminated `const char*`
 * carriers from the Phase 4.2.0 lowering of string literals.
 */
const char *mochi_str_concat(const char *a, const char *b);

/*
 * mochi_str_from_i64 / mochi_str_from_f64 return freshly allocated
 * NUL-terminated decimal representations (the int form is %lld via
 * PRId64; the float form is a shortest round-trip representation
 * matching the Go target's strconv.FormatFloat('g', -1, 64)). The
 * result is owned by the runtime (currently leaked); callers must
 * not free() it.
 *
 * mochi_str_from_bool returns one of two static C99 string literals,
 * "true" or "false". The result is not heap-allocated, but callers
 * treat all three the same way (as `const char*` carriers) because
 * no Mochi operation distinguishes a literal from an allocation.
 */
const char *mochi_str_from_i64(long long v);
const char *mochi_str_from_f64(double v);
const char *mochi_str_from_bool(int v);

/*
 * mochi_str_char_at returns a freshly allocated NUL-terminated
 * single-rune string holding the i-th rune of s. UTF-8 decoded: a
 * byte 0xxxxxxx is 1 byte, 110xxxxx + 1 continuation is 2 bytes,
 * 1110xxxx + 2 continuations is 3 bytes, 11110xxx + 3 continuations
 * is 4 bytes. This matches the Mochi VM's `string([]rune(s)[i])`
 * lowering so the C target byte-matches `mochi run` for non-ASCII
 * inputs too.
 *
 * Bounds are not checked: `i` past the end of the string walks to
 * the NUL terminator and returns a copy of the empty string. The
 * caller is responsible for staying in range (matches the existing
 * mochi_list_i64_get convention on the C target).
 *
 * The returned buffer is owned by the runtime (leaked at process
 * exit; see the ownership note above).
 */
const char *mochi_str_char_at(const char *s, int64_t i);

/*
 * mochi_str_rune_len returns the number of UTF-8 runes in s (not
 * the byte length; see strlen for that). Walks the byte sequence
 * counting leader bytes; continuation bytes (10xxxxxx) and malformed
 * leaders are skipped so a malformed sequence advances byte-by-byte
 * (matches the leader-width fallback in mochi_str_char_at). Used as
 * the loop bound for `for ch in s` (Phase 4.2.29). Returns an
 * int64_t to align with the IR's TypeI64 result; the value is
 * non-negative.
 */
int64_t mochi_str_rune_len(const char *s);

/*
 * mochi_f64_format writes the shortest decimal representation of v
 * into buf (NUL-terminated) using the same rule as Go's
 * strconv.FormatFloat(v, 'g', -1, 64): exponent form when the
 * decimal exponent of the leading digit is < -4 or >= 6, fixed form
 * otherwise. Returns the number of bytes written (excluding NUL).
 * buf must have room for at least 32 bytes; the longest output a
 * finite double can produce here is 24 bytes (the leak-budget
 * comment in mochi_str.c quantifies the worst case).
 *
 * Shared by both mochi_print_f64 (Phase 4.1) and mochi_str_from_f64
 * (Phase 4.2.4); centralising avoids drift between print() and
 * str() output for the same double (MEP-42 Phase 4.2.8).
 */
int mochi_f64_format(char *buf, int bufsize, double v);

#ifdef __cplusplus
}
#endif

#endif /* MOCHI_RUNTIME_C_STR_H */
