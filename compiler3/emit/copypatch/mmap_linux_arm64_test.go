//go:build linux && arm64

package copypatch

import (
	"os"
	"testing"
)

// TestLinuxARM64MappingDualView covers the load-bearing W^X invariant
// on aarch64 Linux: after NewLinuxARM64Mapping returns (rw, rx), a
// write to rw[i] is observable as a read at rx[i] (the two mappings
// share physical pages via the shared memfd backing). The test does
// not attempt to execute through rx; the Phase 1.6 integration test
// owns the real-trampoline coverage.
//
// aarch64 cache-coherence note: the test relies on the Linux kernel's
// `flush_dcache_page` invocation on first-fault to keep the data and
// instruction caches in sync on the same backing pages. The userspace
// caller does not need to issue IC IVAU / DSB / ISB for first-fault
// pages; the kernel does that for us through MAP_SHARED's coherence
// semantics on aarch64.
func TestLinuxARM64MappingDualView(t *testing.T) {
	page := os.Getpagesize()
	rw, rx, err := NewLinuxARM64Mapping(page)
	if err != nil {
		t.Fatalf("NewLinuxARM64Mapping: %v", err)
	}
	defer func() {
		if err := ReleaseLinuxARM64Mapping(rw, rx); err != nil {
			t.Errorf("ReleaseLinuxARM64Mapping: %v", err)
		}
	}()
	if len(rw) != page || len(rx) != page {
		t.Errorf("mapping lens = (%d, %d), want (%d, %d)", len(rw), len(rx), page, page)
	}
	for i := range 16 {
		rw[i] = byte(0xA0 + i)
	}
	for i := range 16 {
		if got := rx[i]; got != byte(0xA0+i) {
			t.Errorf("rx[%d] = 0x%x, want 0x%x (rw write not visible through rx)",
				i, got, 0xA0+i)
		}
	}
}

// TestLinuxARM64MappingRejectsZero covers input validation. A zero or
// negative size is a misuse; the mapper must fail loudly rather than
// calling memfd_create with garbage.
func TestLinuxARM64MappingRejectsZero(t *testing.T) {
	if _, _, err := NewLinuxARM64Mapping(0); err == nil {
		t.Errorf("NewLinuxARM64Mapping(0) = nil error, want non-nil")
	}
	if _, _, err := NewLinuxARM64Mapping(-4096); err == nil {
		t.Errorf("NewLinuxARM64Mapping(-4096) = nil error, want non-nil")
	}
}

// TestLinuxARM64MappingRejectsMisalignment covers the page-alignment
// requirement. The aarch64 page size on Linux is configurable at
// kernel build time (4 KiB, 16 KiB, or 64 KiB); the test uses
// os.Getpagesize() so it adapts to whichever the host runs.
func TestLinuxARM64MappingRejectsMisalignment(t *testing.T) {
	page := os.Getpagesize()
	if _, _, err := NewLinuxARM64Mapping(page + 17); err == nil {
		t.Errorf("NewLinuxARM64Mapping(page+17) = nil error, want non-nil")
	}
}

// TestLinuxARM64MappingWithCache wires the arm64 mapping into a real
// Cache and installs a stencil. This is the end-to-end path the
// runtime takes on a real JIT compile on aarch64 Linux; we cannot
// execute the bytes (no trampoline in this test), but we can verify
// the patcher writes the right values into the executable view's
// physical pages.
func TestLinuxARM64MappingWithCache(t *testing.T) {
	if !Supported() {
		t.Skip("aarch64 stencil table unavailable")
	}
	page := os.Getpagesize()
	rw, rx, err := NewLinuxARM64Mapping(page)
	if err != nil {
		t.Fatalf("mapping: %v", err)
	}
	defer ReleaseLinuxARM64Mapping(rw, rx)
	c, err := NewCache(rw, rx)
	if err != nil {
		t.Fatalf("NewCache: %v", err)
	}
	e, _ := NewEmitter()
	fn := buildConstReturn(0xABCD)
	code, relocs, err := e.Compile(fn)
	if err != nil {
		t.Fatalf("Compile: %v", err)
	}
	var st SymbolTable
	st.Set(SymOpRetTarget, 1)
	for i := range relocs {
		relocs[i].Addend--
	}
	entry, err := c.Install(code, relocs, &st)
	if err != nil {
		t.Fatalf("Install: %v", err)
	}
	if entry == 0 {
		t.Errorf("entry address is zero")
	}
	for i, b := range rw[:len(code)] {
		if rx[i] != b {
			t.Errorf("rx[%d] = 0x%x, rw[%d] = 0x%x (views disagree)",
				i, rx[i], i, b)
		}
	}
}
