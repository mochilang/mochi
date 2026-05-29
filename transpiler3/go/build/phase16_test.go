package build

import (
	"bytes"
	"crypto/sha256"
	"encoding/hex"
	"os"
	"path/filepath"
	"runtime"
	"testing"
)

// TestPhase16ByteEqualRebuild is the MEP-54 Phase 16 gate for
// reproducible builds. It runs the full Driver.Build pipeline
// twice against the hello fixture in two distinct, freshly
// created work directories. The two output binaries must be
// byte-identical.
//
// The flag set responsible for byte-equality lives in
// gobuild.go: `-trimpath` strips absolute filesystem paths,
// `-buildvcs=false` disables git-derived stamping, and
// `-ldflags=-buildid=` strips the BuildID. Phase 16 adds
// `SOURCE_DATE_EPOCH=0` to the build env.
//
// Platform notes:
//   - Linux is the primary gate (ELF) — fully deterministic
//     under the flag set above.
//   - darwin is gated too: empirically, Mach-O LC_UUID under
//     `-trimpath -buildvcs=false -ldflags=-buildid=` is stable
//     across rebuilds on modern toolchains (verified on
//     go1.26.x, ld64 from Xcode 16+). The MEP-54 spec's earlier
//     "darwin LC_UUID skipped" note pre-dates this and is
//     overridden by the empirical result.
//   - Windows is skipped at the Phase 1 POSIX invariant.
func TestPhase16ByteEqualRebuild(t *testing.T) {
	if runtime.GOOS == "windows" {
		t.Skip("Phase 1 ships POSIX `go` invocation only; Windows lands in Phase 16.x")
	}
	root := repoRoot(t)
	src := filepath.Join(root, "tests", "transpiler3", "go", "fixtures", "hello", "hello.mochi")

	build := func(label string) string {
		outBin := filepath.Join(t.TempDir(), "hello-"+label)
		d := &Driver{CacheDir: t.TempDir()}
		if err := d.Build(src, outBin, "", ""); err != nil {
			t.Fatalf("Driver.Build %s: %v", label, err)
		}
		return outBin
	}

	bin1 := build("a")
	bin2 := build("b")

	b1, err := os.ReadFile(bin1)
	if err != nil {
		t.Fatalf("read bin1: %v", err)
	}
	b2, err := os.ReadFile(bin2)
	if err != nil {
		t.Fatalf("read bin2: %v", err)
	}
	if !bytes.Equal(b1, b2) {
		h1 := sha256.Sum256(b1)
		h2 := sha256.Sum256(b2)
		t.Fatalf("binaries differ across rebuilds (Phase 16 gate)\n  bin1 sha256: %s\n  bin2 sha256: %s\n  size1=%d size2=%d",
			hex.EncodeToString(h1[:]), hex.EncodeToString(h2[:]), len(b1), len(b2))
	}
}

// TestPhase16SourceDateEpoch asserts that goBuild propagates
// SOURCE_DATE_EPOCH=0 into the `go build` env by default.
// We can't observe the child env directly, so we verify the
// gobuild.go contract by reading the source file itself — the
// goBuild function must reference SOURCE_DATE_EPOCH explicitly
// (a regression check that the env knob isn't accidentally
// removed without updating the byte-equal gate above).
func TestPhase16SourceDateEpoch(t *testing.T) {
	if runtime.GOOS == "windows" {
		t.Skip("Phase 1 invariant: skipping on Windows")
	}
	root := repoRoot(t)
	srcPath := filepath.Join(root, "transpiler3", "go", "build", "gobuild.go")
	src, err := os.ReadFile(srcPath)
	if err != nil {
		t.Fatalf("read gobuild.go: %v", err)
	}
	if !bytes.Contains(src, []byte("SOURCE_DATE_EPOCH=0")) {
		t.Fatalf("gobuild.go must set SOURCE_DATE_EPOCH=0 in the build env (Phase 16 contract):\n%s", src)
	}
}
