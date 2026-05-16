//go:build darwin && arm64

package tracejit

/*
#include <stdint.h>
#include <pthread.h>
#include <libkern/OSCacheControl.h>

typedef void (*tracefn)(int64_t *);

static void call_trace(void *code, int64_t *regs) {
    ((tracefn)code)(regs);
}

static void wp(int enable) {
    pthread_jit_write_protect_np(enable);
}

static void icache(void *p, size_t n) {
    sys_icache_invalidate(p, n);
}
*/
import "C"
import (
	"unsafe"

	"mochi/runtime/jit/tmpljit"
)

func pthread_jit_write_protect_np(enable bool) {
	v := C.int(0)
	if enable {
		v = 1
	}
	C.wp(v)
}

func sys_icache_invalidate(p unsafe.Pointer, n uintptr) {
	C.icache(p, C.size_t(n))
}

// run invokes the compiled trace with the live register file. The
// trace mutates *regs in place; on return, regs contains the
// post-loop register state and the caller should resume the
// interpreter at trace.ExitPC.
func (c *CompiledTrace) run(regs *[tmpljit.NumRegs]int64) {
	C.call_trace(c.entry, (*C.int64_t)(unsafe.Pointer(&regs[0])))
}
