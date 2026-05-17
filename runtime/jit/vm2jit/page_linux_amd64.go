//go:build linux && amd64

package vm2jit

import (
	"fmt"
	"syscall"
)

const osPageSize = 4096

func pageRound(n int) int { return (n + osPageSize - 1) &^ (osPageSize - 1) }

func pageAllocOS(nBytes int) ([]byte, error) {
	p, err := syscall.Mmap(-1, 0, pageRound(nBytes),
		syscall.PROT_READ|syscall.PROT_WRITE,
		syscall.MAP_ANON|syscall.MAP_PRIVATE)
	if err != nil {
		return nil, fmt.Errorf("vm2jit/page: mmap: %w", err)
	}
	return p, nil
}

func pageMakeExecOS(page, src []byte) error {
	copy(page, src)
	if err := syscall.Mprotect(page, syscall.PROT_READ|syscall.PROT_EXEC); err != nil {
		return fmt.Errorf("vm2jit/page: mprotect: %w", err)
	}
	return nil
}

func pageFreeOS(page []byte) error {
	return syscall.Munmap(page)
}
