//go:build darwin && arm64

package vm3jit

/*
#include <stdint.h>
#include <pthread.h>
#include <libkern/OSCacheControl.h>

static void jwp(int enable) { pthread_jit_write_protect_np(enable); }
static void icache(void *p, size_t n) { sys_icache_invalidate(p, n); }
*/
import "C"
import (
	"fmt"
	"syscall"
	"unsafe"
)

const osPageSize = 16384
const mapJIT = 0x0800

func pageRound(n int) int { return (n + osPageSize - 1) &^ (osPageSize - 1) }

func pageAllocOS(nBytes int) ([]byte, error) {
	p, err := syscall.Mmap(-1, 0, pageRound(nBytes),
		syscall.PROT_READ|syscall.PROT_WRITE,
		syscall.MAP_ANON|syscall.MAP_PRIVATE|mapJIT)
	if err != nil {
		return nil, fmt.Errorf("vm3jit/page: mmap: %w", err)
	}
	return p, nil
}

func pageMakeExecOS(page, src []byte) error {
	C.jwp(0)
	copy(page, src)
	C.jwp(1)
	C.icache(unsafe.Pointer(&page[0]), C.size_t(len(src)))
	if err := syscall.Mprotect(page, syscall.PROT_READ|syscall.PROT_EXEC); err != nil {
		return fmt.Errorf("vm3jit/page: mprotect: %w", err)
	}
	return nil
}

func pageFreeOS(page []byte) error { return syscall.Munmap(page) }
