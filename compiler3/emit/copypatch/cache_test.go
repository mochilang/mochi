package copypatch

import (
	"encoding/binary"
	"testing"
)

// TestCacheNewMismatch ensures NewCache rejects a mismatched (rw, rx)
// pair. The dual-mapping invariant is load-bearing: an asymmetric
// pair would leave the patcher writing through one address space and
// the runtime jumping into another.
func TestCacheNewMismatch(t *testing.T) {
	rw := make([]byte, 4096)
	rx := make([]byte, 8192)
	if _, err := NewCache(rw, rx); err == nil {
		t.Errorf("NewCache with mismatched sizes did not return error")
	}
	if _, err := NewCache(nil, rx); err == nil {
		t.Errorf("NewCache with nil rw did not return error")
	}
}

// TestCacheInstallSingle covers the happy path: emit a tiny Const+Return
// function and install it into a synthetic (rw, rx) pair (same buffer
// twice, which is not W^X but is a valid in-process test substrate).
// After Install the rw view must hold the patched bytes and the
// returned entry address must equal the rx-base.
func TestCacheInstallSingle(t *testing.T) {
	if !Supported() {
		t.Skip("phase 1 ships amd64 only")
	}
	page := make([]byte, 4096)
	c, err := NewCache(page, page)
	if err != nil {
		t.Fatalf("NewCache: %v", err)
	}
	if got := c.Capacity(); got != 4096 {
		t.Errorf("Capacity = %d, want 4096", got)
	}
	if got := c.HighWater(); got != 0 {
		t.Errorf("HighWater (fresh) = %d, want 0", got)
	}
	e, err := NewEmitter()
	if err != nil {
		t.Fatalf("NewEmitter: %v", err)
	}
	fn := buildConstReturn(0xCAFE)
	code, relocs, err := e.Compile(fn)
	if err != nil {
		t.Fatalf("Compile: %v", err)
	}
	var st SymbolTable
	st.Set(SymOpRetTarget, 0) // imm64 path; literal flows through Addend
	// applyRelocs rejects addr == 0; bind a sentinel and absorb it
	// into the Addend so the final value is just the Const.
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
	if got := c.HighWater(); got != len(code) {
		t.Errorf("HighWater post-install = %d, want %d", got, len(code))
	}
	// The OpConst stencil's imm64 sits at offset 2..10; after patch it
	// must hold (1 + (Const - 1)) = Const = 0xCAFE.
	if got := binary.LittleEndian.Uint64(page[2:10]); got != 0xCAFE {
		t.Errorf("patched imm64 = 0x%x, want 0xCAFE", got)
	}
}

// TestCacheInstallSequence covers two consecutive Install calls. The
// second blob must land at the post-first high-water mark; the first
// blob's bytes must be undisturbed.
func TestCacheInstallSequence(t *testing.T) {
	if !Supported() {
		t.Skip("phase 1 ships amd64 only")
	}
	page := make([]byte, 4096)
	c, _ := NewCache(page, page)
	e, _ := NewEmitter()
	fnA := buildConstReturn(0x1111)
	fnB := buildConstReturn(0x2222)
	codeA, relocsA, _ := e.Compile(fnA)
	codeB, relocsB, _ := e.Compile(fnB)
	var st SymbolTable
	st.Set(SymOpRetTarget, 1)
	for i := range relocsA {
		relocsA[i].Addend--
	}
	for i := range relocsB {
		relocsB[i].Addend--
	}
	entryA, err := c.Install(codeA, relocsA, &st)
	if err != nil {
		t.Fatalf("Install A: %v", err)
	}
	entryB, err := c.Install(codeB, relocsB, &st)
	if err != nil {
		t.Fatalf("Install B: %v", err)
	}
	if entryA == entryB {
		t.Errorf("entryA == entryB; high-water mark did not advance")
	}
	if entryB-entryA != uintptr(len(codeA)) {
		t.Errorf("entryB - entryA = %d, want %d (len(codeA))",
			entryB-entryA, len(codeA))
	}
	// Verify both imm64 patch sites carry the right constants.
	if got := binary.LittleEndian.Uint64(page[2:10]); got != 0x1111 {
		t.Errorf("blob A imm64 = 0x%x, want 0x1111", got)
	}
	if got := binary.LittleEndian.Uint64(page[len(codeA)+2 : len(codeA)+10]); got != 0x2222 {
		t.Errorf("blob B imm64 = 0x%x, want 0x2222", got)
	}
}

// TestCacheInstallFull covers the cache-full path: when the high-water
// mark would exceed capacity, Install returns ErrCacheFull so the JIT
// can fall back to interpretation.
func TestCacheInstallFull(t *testing.T) {
	page := make([]byte, 16)
	c, _ := NewCache(page, page)
	code := make([]byte, 32)
	var st SymbolTable
	if _, err := c.Install(code, nil, &st); err != ErrCacheFull {
		t.Errorf("Install over-cap err = %v, want ErrCacheFull", err)
	}
}

// TestCacheReset covers the high-water-rewind path used by the GC
// integration in Phase 1.6.
func TestCacheReset(t *testing.T) {
	page := make([]byte, 1024)
	c, _ := NewCache(page, page)
	var st SymbolTable
	if _, err := c.Install([]byte{0xC3}, nil, &st); err != nil {
		t.Fatalf("Install: %v", err)
	}
	if c.HighWater() != 1 {
		t.Errorf("HighWater = %d, want 1", c.HighWater())
	}
	c.Reset()
	if c.HighWater() != 0 {
		t.Errorf("HighWater post-Reset = %d, want 0", c.HighWater())
	}
}

// TestCacheNilSafe covers the nil-receiver guards. These are
// belt-and-braces; the runtime path always has a valid Cache, but a
// misuse must not segfault.
func TestCacheNilSafe(t *testing.T) {
	var c *Cache
	if got := c.Capacity(); got != 0 {
		t.Errorf("nil.Capacity = %d, want 0", got)
	}
	if got := c.HighWater(); got != 0 {
		t.Errorf("nil.HighWater = %d, want 0", got)
	}
	c.Reset() // must not panic
	if _, err := c.Install(nil, nil, nil); err == nil {
		t.Errorf("nil.Install = nil error, want non-nil")
	}
}

// TestCacheInstallNilTable covers the SymbolTable-required guard.
func TestCacheInstallNilTable(t *testing.T) {
	page := make([]byte, 4096)
	c, _ := NewCache(page, page)
	_, err := c.Install([]byte{0xC3}, []RelocSite{{Kind: RelocImm32, Symbol: SymArenaBase}}, nil)
	if err == nil {
		t.Errorf("Install with nil SymbolTable did not error")
	}
}
