package build

import (
	"bytes"
	"os"
	"os/exec"
	"path/filepath"
	"runtime"
	"strings"
	"testing"
)

// TestPhase17Static is the MEP-45 Phase 17.3 gate. It builds the hello
// fixture with Driver.Static=true and verifies two properties:
//
//  1. The build succeeds (Driver.Build returns nil).
//  2. The produced binary runs correctly (output matches expect.txt).
//  3. On Linux, `file <binary>` reports "statically linked", confirming
//     that no shared-library dependencies were introduced.
//
// The test is skipped on Windows (no static-link story yet; Phase 11
// covers cross-compilation) and on darwin (Apple's linker rejects
// -static for system-libc targets; static darwin builds go through zig
// cc with a musl triple, which is a cross-compile case covered by Phase
// 11). On Linux the test uses whatever cc resolves (host gcc or zig cc)
// and asserts the output is statically linked.
func TestPhase17Static(t *testing.T) {
	if runtime.GOOS != "linux" {
		t.Skipf("Phase 17.3 static gate only runs on Linux (got %s)", runtime.GOOS)
	}
	root := repoRoot(t)
	src := filepath.Join(root, "tests", "transpiler3", "c", "fixtures", "hello", "hello.mochi")
	exp := filepath.Join(root, "tests", "transpiler3", "c", "fixtures", "hello", "expect.txt")

	outBin := filepath.Join(t.TempDir(), "hello_static")
	d := &Driver{
		CacheDir: t.TempDir(),
		NoCache:  true,
		Static:   true,
	}
	if err := d.Build(src, outBin, "", ""); err != nil {
		t.Fatalf("static build: %v", err)
	}

	// Verify output matches expected.
	got, runErr := exec.Command(outBin).Output()
	if runErr != nil {
		t.Fatalf("run static binary: %v", runErr)
	}
	want, err := os.ReadFile(exp)
	if err != nil {
		t.Fatalf("read expect: %v", err)
	}
	if !bytes.Equal(got, want) {
		t.Fatalf("output mismatch:\ngot:  %q\nwant: %q", got, want)
	}

	// Verify the binary has no shared-library deps via `file`.
	fileOut, fileErr := exec.Command("file", outBin).Output()
	if fileErr != nil {
		t.Skipf("file(1) not available: %v", fileErr)
	}
	if !strings.Contains(string(fileOut), "statically linked") {
		t.Fatalf("binary not statically linked; file output:\n%s", fileOut)
	}
	t.Logf("static binary confirmed: %s", strings.TrimSpace(string(fileOut)))
}
