/*
 * libmochi: print runtime.
 *
 * MEP-45 Phase 1. See website/docs/mep/mep-0045.md.
 *
 * Phase 1 ships one entry point: mochi_print_str, which writes
 * the bytes of a NUL-terminated string followed by a single
 * '\n' to stdout. This matches Go's fmt.Println(string) and so
 * matches the vm3 oracle byte-for-byte on plain-string prints.
 *
 * Later phases extend this header with i64, f64, bool, and
 * owning string variants; this file's first entry must stay
 * ABI-stable so older emitted C compiles against newer
 * libmochi.
 */
#ifndef MOCHI_PRINT_H
#define MOCHI_PRINT_H

#ifdef __cplusplus
extern "C" {
#endif

/*
 * mochi_print_str writes s (NUL-terminated) followed by a
 * single '\n' to stdout. The implementation flushes only via
 * stdio's line-buffering policy; callers that need a stronger
 * guarantee call fflush(stdout) explicitly. Returns no value.
 */
void mochi_print_str(const char *s);

#ifdef __cplusplus
}
#endif

#endif /* MOCHI_PRINT_H */
