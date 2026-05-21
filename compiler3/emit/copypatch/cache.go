package copypatch

import (
	"fmt"
	"sync"
	"unsafe"
)

// DefaultCacheBytes is the Phase 1 code-cache size: 64 MiB, matching
// the MEP-42 §11.3 "configurable cap" default. The Phase 1.6 BG
// kernel runs all fit in 64 MiB with two orders of magnitude headroom;
// long-running REPL / server workloads tune via MOCHI_JIT_CACHE_MB.
const DefaultCacheBytes = 64 << 20

// Cache is the bump-allocated, optionally LRU-evicted code cache for
// emitted stencil sequences. One Cache per VM; concurrent access is
// guarded by mu.
//
// Phase 1 ships with bump allocation only: every emit appends to the
// high-water mark and never frees. LRU eviction is the Phase 1.6
// follow-up; until then the cache returns ErrCacheFull when capacity
// is exceeded and the JIT falls back to vm3 interpretation.
//
// Layout: the RW page and the RX page are two mappings of the same
// physical memory, set up by mmap_linux_amd64.go. The bump cursor
// runs along the RW view; jumps target the RX view. Both views share
// base address invariants: page-aligned, contiguous, of equal size.
type Cache struct {
	mu sync.Mutex

	rw []byte // writable mapping, used by the patcher
	rx []byte // executable mapping, used by the runtime jumper
	hw int    // high-water mark: rw[:hw] is committed code
}

// ErrCacheFull is returned by Install when there is not enough room
// for the next stencil run. The caller falls back to interpretation
// and may choose to flush the cache (Reset) on the next GC cycle.
var ErrCacheFull = fmt.Errorf("copypatch.Cache: high-water mark would exceed capacity")

// NewCache wraps an existing (rw, rx) dual-mapping pair into a Cache.
// The caller (typically the platform-specific mmap manager in
// mmap_linux_amd64.go) is responsible for the dual mapping; Cache
// owns nothing it allocates itself, so a Cache can be deterministically
// closed by tearing down the mapping outside.
func NewCache(rw, rx []byte) (*Cache, error) {
	if len(rw) == 0 || len(rx) == 0 {
		return nil, fmt.Errorf("copypatch.NewCache: empty mapping")
	}
	if len(rw) != len(rx) {
		return nil, fmt.Errorf("copypatch.NewCache: rw len %d != rx len %d", len(rw), len(rx))
	}
	return &Cache{rw: rw, rx: rx}, nil
}

// Capacity returns the size of the RW/RX pair in bytes.
func (c *Cache) Capacity() int {
	if c == nil {
		return 0
	}
	return len(c.rw)
}

// HighWater returns the number of bytes currently committed. Useful
// for tests and for the MOCHI_JIT_CACHE_MB advisory metric.
func (c *Cache) HighWater() int {
	if c == nil {
		return 0
	}
	c.mu.Lock()
	defer c.mu.Unlock()
	return c.hw
}

// Install copies the emitted machine code into the RW view at the
// current high-water mark, runs applyRelocs against the dst slot in
// the RX view's coordinate system, and returns the entry address
// (in the RX mapping) that the trampoline should jump to.
//
// The (rw, rx) pair shares the same physical pages, so writing
// through rw is observable through rx after the mandatory memory
// barrier. The barrier is provided by the kernel's mprotect /
// pthread_jit_write_protect_np / clflush dance in
// mmap_linux_amd64.go's pageFlipExec; Install assumes the caller has
// already set up the mapping correctly and does not issue its own
// barrier.
func (c *Cache) Install(code []byte, relocs []RelocSite, syms *SymbolTable) (entry uintptr, err error) {
	if c == nil {
		return 0, fmt.Errorf("copypatch.Cache.Install: nil Cache")
	}
	c.mu.Lock()
	defer c.mu.Unlock()
	if c.hw+len(code) > len(c.rw) {
		return 0, ErrCacheFull
	}
	off := c.hw
	copy(c.rw[off:off+len(code)], code)
	// The entry address the runtime jumps to is in the RX mapping.
	rxBase := bytePtrAddr(c.rx)
	entry = rxBase + uintptr(off)
	// Apply relocs against the RW view; because the two mappings share
	// physical pages, the writes are visible through RX after the
	// kernel-level barrier set up at mapping time.
	if err := applyRelocs(c.rw[off:off+len(code)], entry, relocs, syms); err != nil {
		return 0, fmt.Errorf("copypatch.Cache.Install: %w", err)
	}
	c.hw += len(code)
	return entry, nil
}

// Reset moves the high-water mark back to zero. The RW/RX mapping is
// left in place; subsequent Installs reuse the existing pages. Used
// by the GC integration in Phase 1.6 to recycle the cache when no
// JIT'd frames are live.
func (c *Cache) Reset() {
	if c == nil {
		return
	}
	c.mu.Lock()
	defer c.mu.Unlock()
	c.hw = 0
}

// bytePtrAddr returns the address of the first byte of b. Centralized
// so the unsafe.Pointer narrowing lives in one place for audit. The
// caller must hold a reference to b until they are done with the
// returned uintptr; otherwise the GC may move it.
//
// Phase 1 mappings are mmap'd memory, not Go-heap memory, so the GC
// is not in the picture. The function is structured this way to keep
// the Phase 1.6 transition (when long-lived Cache state may include
// Go-heap pointers) safe.
func bytePtrAddr(b []byte) uintptr {
	if len(b) == 0 {
		return 0
	}
	return uintptr(unsafe.Pointer(&b[0]))
}
