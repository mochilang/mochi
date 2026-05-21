//go:build linux && amd64

package copypatch

import (
	"fmt"
	"os"
	"syscall"
	"unsafe"
)

// NewLinuxAMD64Mapping creates a dual-mapping (rw, rx) pair of size
// bytes that share the same physical pages. The technique:
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
// The two views are not at the same virtual address, so a stencil's
// internal jumps must be patched against the entry address in the
// RX view (the address the trampoline will jump to), not the RW
// view (the address the patcher writes through). cache.Install
// already does this: it passes the RX-view entry as baseAddr to
// applyRelocs.
//
// W^X is structural: neither mapping is ever simultaneously writable
// and executable. The RW mapping has PROT_READ|PROT_WRITE; the RX
// mapping has PROT_READ|PROT_EXEC. No mprotect toggling is needed,
// which means no per-Install syscall overhead beyond the initial
// mapping setup. This is the linux/amd64 spelling of MEP-41 §5's
// W^X requirement (axis 1).
//
// Phase 1 returns the raw byte slices to the caller, who is expected
// to hand them to NewCache and to munmap them on Cache teardown via
// ReleaseLinuxAMD64Mapping. A higher-level lifecycle wrapper lands
// in Phase 1.6 alongside the BG parity gate.
func NewLinuxAMD64Mapping(size int) (rw, rx []byte, err error) {
	if size <= 0 {
		return nil, nil, fmt.Errorf("copypatch.NewLinuxAMD64Mapping: size must be positive, got %d", size)
	}
	if size%pageSize() != 0 {
		return nil, nil, fmt.Errorf("copypatch.NewLinuxAMD64Mapping: size %d not page-aligned (page=%d)",
			size, pageSize())
	}
	fd, err := memfdCreate("mochi-jit", mfdCloexec)
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

// ReleaseLinuxAMD64Mapping unmaps both views. Errors from the
// individual munmap calls are surfaced via the returned wrapped
// error; callers should log them but cannot meaningfully recover.
func ReleaseLinuxAMD64Mapping(rw, rx []byte) error {
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

// pageSize returns the host page size. Centralized so the size-
// alignment check in NewLinuxAMD64Mapping has one source of truth.
func pageSize() int {
	return os.Getpagesize()
}

// memfd_create syscall constants. Hard-coded here so the linux/amd64
// build does not pull in golang.org/x/sys (Mochi's pure-Go-no-cgo
// identity rule, MEP-42 §13).
const (
	sysMemfdCreate = 319 // x86_64 syscall number; man 2 memfd_create
	mfdCloexec     = 0x0001
)

func memfdCreate(name string, flags int) (int, error) {
	// syscall.BytePtrFromString returns a NUL-terminated copy of the
	// name; required by the kernel's strncpy_from_user.
	bp, err := syscall.BytePtrFromString(name)
	if err != nil {
		return 0, err
	}
	r1, _, errno := syscall.Syscall(sysMemfdCreate, uintptr(unsafe.Pointer(bp)), uintptr(flags), 0)
	if errno != 0 {
		return 0, errno
	}
	return int(r1), nil
}
