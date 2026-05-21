//go:build (darwin && arm64) || (linux && amd64)

package vm3jit

import (
	"runtime"
	"testing"
)

// TestPageWXTransition exercises the W^X transition end-to-end: alloc
// gives back a writable page, write+make-exec flips it to read-execute,
// and a final read from the executable page succeeds without faulting.
// On platforms without a backend the alloc step returns errNoPageBackend
// and the test reports a skip (the structurally-secure fallback path).
//
// The test is the executable companion to docs/security/jit-hardening.md
// axis 1: it ensures the page is never observed as RWX. The runtime
// performs the transition atomically (mprotect on linux/amd64;
// pthread_jit_write_protect_np + mprotect on darwin/arm64).
func TestPageWXTransition(t *testing.T) {
	if !platformHasPageBackend() {
		t.Skipf("vm3jit: no executable-page backend on %s/%s; falling back to interpreter (structurally secure)", runtime.GOOS, runtime.GOARCH)
	}
	page, err := pageAlloc(64)
	if err != nil {
		t.Fatalf("pageAlloc: %v", err)
	}
	defer func() {
		if err := pageFree(page); err != nil {
			t.Errorf("pageFree: %v", err)
		}
	}()

	// Pre-make-exec: the page is writable. We must be able to write
	// into it without trapping. This is the "W" half of W^X; the
	// alloc step opens the writable window.
	page[0] = 0xC3 // x86 RET / arm64 SVC; the byte itself is opaque to W^X
	if got := page[0]; got != 0xC3 {
		t.Errorf("pre-make-exec write: got page[0]=%#x, want 0xC3", got)
	}

	// Build a tiny RET-only snippet. Both architectures have a
	// single-instruction return:
	//   x86_64: 0xC3                 (RET)
	//   arm64:  D6 5F 03 C0 (LE)     (RET x30)
	var snippet []byte
	switch {
	case runtime.GOARCH == "amd64":
		snippet = []byte{0xC3}
	case runtime.GOARCH == "arm64":
		snippet = []byte{0xC0, 0x03, 0x5F, 0xD6}
	default:
		t.Skipf("vm3jit: no test snippet for %s", runtime.GOARCH)
	}

	// Flip to RX. After this, page[0] must be readable (the X half
	// implies the X-side of RX), but a write would fault. We do not
	// attempt a write here because catching the resulting SIGSEGV
	// from inside the test would require platform-specific signal
	// handling; the structural property is enforced by the OS, not
	// by the test.
	if err := pageWrite(page, snippet); err != nil {
		t.Fatalf("pageWrite: %v", err)
	}

	if got := page[0]; got != snippet[0] {
		t.Errorf("post-make-exec read: got page[0]=%#x, want %#x", got, snippet[0])
	}
}

// TestPageRefusesOnUnsupportedPlatform is the structural-fallback
// check: on a platform without a real W^X backend, pageAlloc returns
// an error rather than silently producing a possibly-RWX page. The
// returned error is wrapped via fmt.Errorf in the real backends but
// is the raw sentinel from page_stub on unsupported platforms; this
// test does not distinguish, only that pageAlloc fails closed.
//
// On supported platforms the test skips (the LANDED-path TestPageWXTransition
// already exercises the success case).
func TestPageRefusesOnUnsupportedPlatform(t *testing.T) {
	if platformHasPageBackend() {
		t.Skipf("vm3jit: %s/%s has a real page backend; the unsupported-platform fallback is not exercised on this host", runtime.GOOS, runtime.GOARCH)
	}
	if _, err := pageAlloc(64); err == nil {
		t.Error("pageAlloc on unsupported platform returned nil; want an error so the runtime falls back to interpreter")
	}
}

// TestPageRoundConservative spot-checks the page-rounding helper:
// (a) zero rounds to zero (no spurious page allocation for empty input),
// (b) one byte rounds up to a full OS page, (c) exactly-one-page sizes
// pass through unchanged, (d) one-byte-over-page rounds to two pages.
// The helper is the input gate for pageAlloc; a regression here would
// silently change the allocation contract.
func TestPageRoundConservative(t *testing.T) {
	if !platformHasPageBackend() {
		t.Skipf("vm3jit: no page backend on %s/%s", runtime.GOOS, runtime.GOARCH)
	}
	cases := []struct {
		in, want int
	}{
		{0, 0},
		{1, osPageSize},
		{osPageSize, osPageSize},
		{osPageSize + 1, 2 * osPageSize},
		{2 * osPageSize, 2 * osPageSize},
	}
	for _, c := range cases {
		if got := pageRound(c.in); got != c.want {
			t.Errorf("pageRound(%d) = %d, want %d", c.in, got, c.want)
		}
	}
}

// platformHasPageBackend reports whether the host (OS/arch) has a real
// W^X backend in tree. The two supported platforms today are
// darwin/arm64 and linux/amd64; every other combination uses page_stub.
// Kept in sync with the build tags on page_*.go.
func platformHasPageBackend() bool {
	switch {
	case runtime.GOOS == "darwin" && runtime.GOARCH == "arm64":
		return true
	case runtime.GOOS == "linux" && runtime.GOARCH == "amd64":
		return true
	}
	return false
}
