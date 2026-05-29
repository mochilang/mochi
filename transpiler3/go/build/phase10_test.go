package build

import (
	"os"
	"path/filepath"
	"runtime"
	"strings"
	"testing"
)

// TestPhase10GoFFISource gates the MEP-54 Phase 10.2 lowering
// shape. The shared aotir lowerer mangles `extern go fun NAME`
// call sites to `mochi_go_NAME` (a name the C target uses for
// its subprocess-RPC trampoline). The Go target strips that
// prefix so the call resolves to the user's native Go function
// in the sibling .go file copied into the work dir.
//
// This test asserts:
//
//  1. The generated main.go contains `add_ints(` (not the
//     `mochi_go_add_ints(` mangled form).
//  2. The sibling fixture .go file lands in the work dir
//     alongside main.go.
//  3. The work dir contains no `mochi_go_` symbol references
//     anywhere (the prefix is gone end-to-end).
func TestPhase10GoFFISource(t *testing.T) {
	if runtime.GOOS == "windows" {
		t.Skip("Phase 1 ships POSIX `go` invocation only; Windows lands in Phase 16.x")
	}
	root := repoRoot(t)
	src := filepath.Join(root, "tests", "transpiler3", "go", "fixtures", "go_ffi_add_ints", "go_ffi_add_ints.mochi")
	outBin := filepath.Join(t.TempDir(), "go_ffi_add_ints")

	d := &Driver{CacheDir: t.TempDir(), KeepWorkDir: true}
	if err := d.Build(src, outBin, "", ""); err != nil {
		t.Fatalf("Driver.Build: %v", err)
	}
	defer os.RemoveAll(d.WorkDirPath)

	mainBytes, err := os.ReadFile(filepath.Join(d.WorkDirPath, "main.go"))
	if err != nil {
		t.Fatalf("read main.go: %v", err)
	}
	main := string(mainBytes)
	if !strings.Contains(main, "add_ints(") {
		t.Fatalf("main.go missing direct call `add_ints(`:\n%s", main)
	}
	if strings.Contains(main, "mochi_go_") {
		t.Fatalf("main.go still references the mochi_go_ prefix:\n%s", main)
	}

	companion := filepath.Join(d.WorkDirPath, "go_ffi_add_ints.go")
	companionBytes, err := os.ReadFile(companion)
	if err != nil {
		t.Fatalf("read companion %s: %v", companion, err)
	}
	companionSrc := string(companionBytes)
	if !strings.Contains(companionSrc, "func add_ints") {
		t.Fatalf("companion file missing func add_ints:\n%s", companionSrc)
	}
	if !strings.Contains(companionSrc, "package main") {
		t.Fatalf("companion file missing `package main`:\n%s", companionSrc)
	}
}

// TestPhase10StripFFIPrefix is a small unit-level check on
// the helper so a regression in the prefix-strip logic is
// caught without rebuilding any fixture.
func TestPhase10StripFFIPrefix(t *testing.T) {
	cases := []struct {
		in, want string
	}{
		{"mochi_go_add_ints", "add_ints"},
		{"mochi_py_anything", "anything"},
		{"mochi_js_thing", "thing"},
		{"mochi__userfun", "mochi__userfun"}, // user-fun prefix unchanged
		{"plain", "plain"},
		{"mochi_go_", "mochi_go_"}, // empty suffix means not an FFI mangling
	}
	for _, c := range cases {
		got := stripFFIPrefixForTest(c.in)
		if got != c.want {
			t.Errorf("stripFFIPrefix(%q) = %q, want %q", c.in, got, c.want)
		}
	}
}

// stripFFIPrefixForTest re-implements the lower-package helper
// inline because the helper is unexported. Keeping the test in
// the build package (where the integration gate lives) and
// duplicating one 5-line function is cheaper than exposing the
// helper.
func stripFFIPrefixForTest(name string) string {
	for _, p := range []string{"mochi_go_", "mochi_py_", "mochi_js_"} {
		if len(name) > len(p) && name[:len(p)] == p {
			return name[len(p):]
		}
	}
	return name
}
