//go:build linux && amd64

package copypatch

import (
	"os"
	"testing"
)

// TestLinuxMappingDualView covers the load-bearing W^X invariant:
// after NewLinuxAMD64Mapping returns (rw, rx), a write to rw[i] is
// observable as a read at rx[i] (the two mappings share physical
// pages). The test does not attempt to execute through rx; that is
// the responsibility of the Phase 1.6 integration test that needs a
// real trampoline.
func TestLinuxMappingDualView(t *testing.T) {
	page := os.Getpagesize()
	rw, rx, err := NewLinuxAMD64Mapping(page)
	if err != nil {
		t.Fatalf("NewLinuxAMD64Mapping: %v", err)
	}
	defer func() {
		if err := ReleaseLinuxAMD64Mapping(rw, rx); err != nil {
			t.Errorf("ReleaseLinuxAMD64Mapping: %v", err)
		}
	}()
	if len(rw) != page || len(rx) != page {
		t.Errorf("mapping lens = (%d, %d), want (%d, %d)", len(rw), len(rx), page, page)
	}
	// Write through rw, read through rx.
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

// TestLinuxMappingRejectsZero covers the input validation in
// NewLinuxAMD64Mapping. A zero or negative size is a misuse; the
// mapper must fail loudly rather than calling into mmap with garbage.
func TestLinuxMappingRejectsZero(t *testing.T) {
	if _, _, err := NewLinuxAMD64Mapping(0); err == nil {
		t.Errorf("NewLinuxAMD64Mapping(0) = nil error, want non-nil")
	}
	if _, _, err := NewLinuxAMD64Mapping(-4096); err == nil {
		t.Errorf("NewLinuxAMD64Mapping(-4096) = nil error, want non-nil")
	}
}

// TestLinuxMappingRejectsMisalignment covers the page-alignment
// requirement. mmap rounds up under the hood, but Mochi requires
// page-aligned sizes so the cache's bump allocator can reason about
// capacity exactly.
func TestLinuxMappingRejectsMisalignment(t *testing.T) {
	page := os.Getpagesize()
	if _, _, err := NewLinuxAMD64Mapping(page + 17); err == nil {
		t.Errorf("NewLinuxAMD64Mapping(page+17) = nil error, want non-nil")
	}
}

// TestLinuxMappingWithCache wires the mapping into a real Cache and
// installs a stencil. This is the end-to-end path the runtime takes
// on a real JIT compile; we cannot execute the bytes (no trampoline
// in this test), but we can verify the patcher writes the right
// values into the executable view's physical pages.
func TestLinuxMappingWithCache(t *testing.T) {
	if !Supported() {
		t.Skip("phase 1 ships amd64 only")
	}
	page := os.Getpagesize()
	rw, rx, err := NewLinuxAMD64Mapping(page)
	if err != nil {
		t.Fatalf("mapping: %v", err)
	}
	defer ReleaseLinuxAMD64Mapping(rw, rx)
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
	// Verify the patched bytes are visible through the rx view.
	for i, b := range rw[:len(code)] {
		if rx[i] != b {
			t.Errorf("rx[%d] = 0x%x, rw[%d] = 0x%x (views disagree)",
				i, rx[i], i, b)
		}
	}
}
