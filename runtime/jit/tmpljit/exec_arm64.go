//go:build darwin && arm64

package tmpljit

/*
#include <stdint.h>
#include <pthread.h>
#include <libkern/OSCacheControl.h>

typedef int64_t (*jitfn)(int64_t);

static int64_t call_jit(void *code, int64_t arg) {
    return ((jitfn)code)(arg);
}

static void wp(int enable) {
    pthread_jit_write_protect_np(enable);
}

static void icache(void *p, size_t n) {
    sys_icache_invalidate(p, n);
}
*/
import "C"
import "unsafe"

// mapJit is darwin's MAP_JIT (0x0800). Required on arm64 for any
// mmap'd executable page since macOS 11.
const mapJit = 0x0800

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

// Call invokes the compiled function with arg.
func (c *CompiledFunc) Call(arg int64) int64 {
	return int64(C.call_jit(c.entry, C.int64_t(arg)))
}
