//go:build linux && amd64

package vm3jit

import (
	"fmt"
	"syscall"
)

// osPageSize is Linux's standard 4 KiB page size on AMD64. Pages
// allocated via Mmap with MAP_ANON are zero-filled and rounded up to
// this size by the kernel; we round explicitly so pageAlloc returns a
// slice whose len() is the actual mapped size.
const osPageSize = 4096

func pageRound(n int) int { return (n + osPageSize - 1) &^ (osPageSize - 1) }

// pageAllocOS reserves an anonymous, private, RW page large enough for
// nBytes of native code. The page is RW so pageMakeExecOS can copy
// into it, then mprotect()s to RX.
func pageAllocOS(nBytes int) ([]byte, error) {
	p, err := syscall.Mmap(-1, 0, pageRound(nBytes),
		syscall.PROT_READ|syscall.PROT_WRITE,
		syscall.MAP_ANON|syscall.MAP_PRIVATE)
	if err != nil {
		return nil, fmt.Errorf("vm3jit/page: mmap: %w", err)
	}
	return p, nil
}

// pageMakeExecOS copies src into page, then flips the page to RX via
// mprotect. Linux/AMD64 does not need an explicit icache flush; CPUs
// snoop the data cache. (Compare to darwin/arm64 which needs
// sys_icache_invalidate and the pthread_jit_write_protect_np handshake.)
func pageMakeExecOS(page, src []byte) error {
	copy(page, src)
	if err := syscall.Mprotect(page, syscall.PROT_READ|syscall.PROT_EXEC); err != nil {
		return fmt.Errorf("vm3jit/page: mprotect: %w", err)
	}
	return nil
}

// pageFreeOS releases the mapping.
func pageFreeOS(page []byte) error { return syscall.Munmap(page) }
