/*
 * libmochi: exception model (Phase 7.0).
 *
 * MEP-45 Phase 7.0 adds a per-translation-unit jump-buffer stack so that
 * a future `try { ... } catch e { ... }` statement (Phase 7.1, requires
 * parser) can intercept panics raised by mochi_raise().
 *
 * Until Phase 7.1 lands (try/catch syntax), programs never push a jump
 * buffer. mochi_raise() then follows the "stack empty" path: write the
 * diagnostic to stderr and exit with the error code. This preserves the
 * exact same observable behaviour as the pre-Phase-7.0 direct exit()
 * calls in mochi_panic_div_zero / mochi_panic_index.
 *
 * ABI:
 *   mochi_try_push(buf)  -- push a setjmp buffer onto the stack
 *   mochi_try_pop()      -- pop the top buffer (must match a prior push)
 *   mochi_raise(code, msg) -- longjmp to top buffer, or exit if empty
 *
 * The stack is a translation-unit-local static array; mochi programs
 * are currently single-threaded so no TLS is needed. When Phase 9
 * (streams/agents, multi-threaded) lands this will be upgraded to
 * __thread storage.
 */
#ifndef MOCHI_EXCEPT_H
#define MOCHI_EXCEPT_H

#include <setjmp.h>
#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

/* Maximum nesting depth of try blocks. Enough for deeply recursive programs. */
#define MOCHI_TRY_MAX_DEPTH 64

/*
 * mochi_try_push registers buf as the jump target for the next mochi_raise.
 * Called by the try-block prologue emitted for `try { ... }` (Phase 7.1).
 * Returns 0 on initial entry; the longjmp path sets the non-zero code.
 */
void mochi_try_push(jmp_buf *buf);

/*
 * mochi_try_pop removes the top jump buffer. Called after the try-block body
 * (normal exit path, before the catch block is skipped).
 */
void mochi_try_pop(void);

/*
 * mochi_raise raises an exception with the given error code and message.
 * If a jump buffer is on the stack, longjmps to it with code. Otherwise
 * writes msg to stderr and exits with code.
 */
_Noreturn void mochi_raise(int code, const char *msg);

/*
 * mochi_except_code is the error code delivered by the most recent
 * mochi_raise longjmp. The catch-block prologue reads it to populate
 * the catch variable. Valid only immediately after setjmp returns non-zero
 * in the try-block frame.
 */
extern int mochi_except_code;

/*
 * mochi_except_msg is the error message delivered by the most recent
 * mochi_raise longjmp. Valid only immediately after setjmp returns non-zero.
 */
extern const char *mochi_except_msg;

#ifdef __cplusplus
}
#endif

#endif /* MOCHI_EXCEPT_H */
