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

#ifdef __cplusplus
}
#endif

#endif /* MOCHI_RUNTIME_C_STR_H */
