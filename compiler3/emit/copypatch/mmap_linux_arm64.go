//go:build linux && arm64

package copypatch

import (
	"fmt"
	"os"
	"syscall"
	"unsafe"
)

// NewLinuxARM64Mapping creates a dual-mapping (rw, rx) pair of size
// bytes that share the same physical pages, on aarch64 Linux. The
// technique is byte-identical to the linux/amd64 path
// (`NewLinuxAMD64Mapping`), the only difference being the
// `memfd_create` syscall number (319 on x86_64, 279 on aarch64).
//
//  1. memfd_create("mochi-jit", MFD_CLOEXEC) returns an anonymous
//     file descriptor backed by RAM.
//  2. ftruncate(fd, size) sizes the backing file.
//  3. mmap(fd, PROT_READ|PROT_WRITE, MAP_SHARED) produces the RW
//     view.
//  4. mmap(fd, PROT_READ|PROT_EXEC,  MAP_SHARED) produces the RX
//     view.
//  5. close(fd) drops the descriptor; the two mappings keep the
//     backing storage alive until both are munmap'd.
//
// The two views are not at the same virtual address. A stencil's
// internal jumps are patched against the RX entry, the address the
// trampoline jumps to, not the RW view the patcher writes through.
// cache.Install passes the RX-view entry as baseAddr to applyRelocs.
//
// W^X is structural: neither mapping is ever simultaneously writable
// and executable. The RW mapping has `PROT_READ|PROT_WRITE`; the RX
// mapping has `PROT_READ|PROT_EXEC`. No mprotect toggling is needed.
// This is the linux/arm64 spelling of MEP-41 §5's W^X requirement
// (axis 1) and the analog of `NewLinuxAMD64Mapping` on x86_64 Linux.
//
// aarch64 cache coherency: the dual-mapping technique relies on the
// kernel keeping the data cache and instruction cache coherent for
// the same backing pages. Linux's MAP_SHARED + the kernel-side
// `flush_dcache_page` invocation on fault satisfies this; the
// userspace caller does not need to issue `IC IVAU` / `DSB` /
// `ISB` manually for first-fault pages. A future `Cache.Reset` that
// reuses pages will need an explicit cache-flush pass; that lands in
// Phase 1.6 alongside the BG parity gate.
func NewLinuxARM64Mapping(size int) (rw, rx []byte, err error) {
	if size <= 0 {
		return nil, nil, fmt.Errorf("copypatch.NewLinuxARM64Mapping: size must be positive, got %d", size)
	}
	if size%pageSizeARM64() != 0 {
		return nil, nil, fmt.Errorf("copypatch.NewLinuxARM64Mapping: size %d not page-aligned (page=%d)",
			size, pageSizeARM64())
	}
	fd, err := memfdCreateARM64("mochi-jit", mfdCloexecARM64)
	if err != nil {
		return nil, nil, fmt.Errorf("memfd_create: %w", err)
	}
	defer func() {
		// Drop the fd unconditionally; the mappings keep the backing
		// storage alive on their own. Failure to close the fd is
		// non-fatal but should never happen on Linux.
		_ = syscall.Close(fd)
	}()
	if err := syscall.Ftruncate(fd, int64(size)); err != nil {
		return nil, nil, fmt.Errorf("ftruncate: %w", err)
	}
	rw, err = syscall.Mmap(fd, 0, size, syscall.PROT_READ|syscall.PROT_WRITE, syscall.MAP_SHARED)
	if err != nil {
		return nil, nil, fmt.Errorf("mmap rw: %w", err)
	}
	rx, err = syscall.Mmap(fd, 0, size, syscall.PROT_READ|syscall.PROT_EXEC, syscall.MAP_SHARED)
	if err != nil {
		_ = syscall.Munmap(rw)
		return nil, nil, fmt.Errorf("mmap rx: %w", err)
	}
	return rw, rx, nil
}

// ReleaseLinuxARM64Mapping unmaps both views. Errors from the
// individual munmap calls are surfaced via the returned wrapped
// error; callers should log them but cannot meaningfully recover.
func ReleaseLinuxARM64Mapping(rw, rx []byte) error {
	var errRW, errRX error
	if len(rw) > 0 {
		errRW = syscall.Munmap(rw)
	}
	if len(rx) > 0 {
		errRX = syscall.Munmap(rx)
	}
	switch {
	case errRW != nil && errRX != nil:
		return fmt.Errorf("munmap rw=%v, rx=%v", errRW, errRX)
	case errRW != nil:
		return fmt.Errorf("munmap rw: %w", errRW)
	case errRX != nil:
		return fmt.Errorf("munmap rx: %w", errRX)
	}
	return nil
}

// pageSizeARM64 returns the host page size on aarch64 Linux. The
// arm64-specific spelling avoids collisions with the amd64
// `pageSize()` helper defined in mmap_linux_amd64.go; both files
// can be compiled into separate per-GOARCH binaries without
// linker-level conflict.
func pageSizeARM64() int {
	return os.Getpagesize()
}

// memfd_create syscall constants for aarch64 Linux. The syscall
// number is 279 on aarch64 (vs 319 on x86_64). The MFD_CLOEXEC flag
// value is architecture-agnostic. Hard-coded here so the linux/arm64
// build does not pull in golang.org/x/sys (Mochi's pure-Go-no-cgo
// identity rule, MEP-42 §13).
const (
	sysMemfdCreateARM64 = 279 // aarch64 syscall number; man 2 memfd_create
	mfdCloexecARM64     = 0x0001
)

func memfdCreateARM64(name string, flags int) (int, error) {
	// syscall.BytePtrFromString returns a NUL-terminated copy of the
	// name; required by the kernel's strncpy_from_user.
	bp, err := syscall.BytePtrFromString(name)
	if err != nil {
		return 0, err
	}
	r1, _, errno := syscall.Syscall(sysMemfdCreateARM64, uintptr(unsafe.Pointer(bp)), uintptr(flags), 0)
	if errno != 0 {
		return 0, errno
	}
	return int(r1), nil
}
