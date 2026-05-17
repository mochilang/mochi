//go:build darwin && arm64

package trampoline

/*
#include <stdint.h>

// The JIT ABI: the compiled function takes a pointer to the Cell register file
// in x0 and returns the result Cell in x0.
typedef uint64_t (*jitfn)(uint64_t *regs);

static uint64_t call_jit(void *entry, uint64_t *regs) {
    return ((jitfn)entry)(regs);
}
*/
import "C"
import "unsafe"

// Call invokes a JIT'd arm64 function via CGo.
// entry is the mmap'd executable page entry point produced by pageEntry.
// regs points to the vm2 Cell register file.
func Call(entry unsafe.Pointer, regs unsafe.Pointer) uint64 {
	return uint64(C.call_jit(entry, (*C.uint64_t)(regs)))
}
