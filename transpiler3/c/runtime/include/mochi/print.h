/*
 * libmochi: print runtime.
 *
 * MEP-45 Phase 1 + 2.0. See website/docs/mep/mep-0045.md.
 *
 * Phase 1 shipped mochi_print_str. Phase 2.0 adds the three
 * scalar primitive prints (i64, f64, bool). Each writes the
 * value's text representation followed by a single '\n' to
 * stdout, matching the vm3 oracle byte-for-byte for ordinary
 * (non-NaN) values; Phase 2.4 hardens the NaN/Inf path.
 *
 * ABI stability: every entry here is part of libmochi's
 * versioned surface. Older emitted C must keep linking against
 * newer libmochi, so existing prototypes never change shape.
 */
#ifndef MOCHI_PRINT_H
#define MOCHI_PRINT_H

#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

/*
 * mochi_print_str writes s (NUL-terminated) followed by a
 * single '\n' to stdout. The implementation flushes only via
 * stdio's line-buffering policy; callers that need a stronger
 * guarantee call fflush(stdout) explicitly.
 */
void mochi_print_str(const char *s);

/*
 * mochi_print_i64 writes x in signed decimal followed by '\n'.
 * Matches Go's fmt.Println(int64) and vm3's int-print path
 * byte-for-byte across tier-1 hosts.
 */
void mochi_print_i64(int64_t x);

/*
 * mochi_print_f64 writes x using Go's strconv.FormatFloat
 * 'g' -1 64 convention, followed by '\n'. Phase 2.4 will
 * extend the implementation to cover NaN / +Inf / -Inf with
 * vm3-equivalent spellings; Phase 2.0 emits whatever the host
 * %.17g format produces for finite values, which is sufficient
 * for the fixtures in this sub-phase.
 */
void mochi_print_f64(double x);

/*
 * mochi_print_bool writes "true\n" when x is nonzero and
 * "false\n" otherwise. The int parameter (instead of <stdbool.h>
 * `bool`) keeps the runtime ABI usable from C++ and from emit
 * passes that store booleans as ints.
 */
void mochi_print_bool(int x);

#ifdef __cplusplus
}
#endif

#endif /* MOCHI_PRINT_H */
