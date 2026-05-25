/*
 * libmochi: exception model implementation (Phase 7.0).
 *
 * See include/mochi/except.h for the interface documentation.
 *
 * Phase 12.2: under WASM/WASI setjmp/longjmp is unavailable (requires the
 * Exception handling proposal). The WASM build provides a minimal stub:
 * mochi_raise always exits; mochi_try_push/pop are omitted entirely.
 */
#include "mochi/except.h"

#include <stdio.h>
#include <stdlib.h>

int         mochi_except_code = 0;
const char *mochi_except_msg  = "";

#ifndef __wasm__

/* Translation-unit-local jump-buffer stack. */
static jmp_buf *__mochi_try_stack[MOCHI_TRY_MAX_DEPTH];
static int      __mochi_try_depth = 0;

void mochi_try_push(jmp_buf *buf) {
    if (__mochi_try_depth >= MOCHI_TRY_MAX_DEPTH) {
        fputs("mochi: try nesting exceeded MOCHI_TRY_MAX_DEPTH\n", stderr);
        exit(1);
    }
    __mochi_try_stack[__mochi_try_depth++] = buf;
}

void mochi_try_pop(void) {
    if (__mochi_try_depth > 0) {
        --__mochi_try_depth;
    }
}

_Noreturn void mochi_raise(int code, const char *msg) {
    if (__mochi_try_depth > 0) {
        mochi_except_code = code;
        mochi_except_msg  = msg;
        longjmp(*__mochi_try_stack[--__mochi_try_depth], code);
    }
    /* No try block on the stack: write diagnostic and exit. */
    fputs(msg, stderr);
    fputc('\n', stderr);
    exit(code);
}

#else /* __wasm__: no setjmp/longjmp; raise always exits */

_Noreturn void mochi_raise(int code, const char *msg) {
    fputs(msg, stderr);
    fputc('\n', stderr);
    exit(code);
}

#endif /* __wasm__ */
