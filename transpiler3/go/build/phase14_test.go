package build

import (
	"os"
	"path/filepath"
	"runtime"
	"strings"
	"testing"
)

// TestPhase14FetchSource verifies the MEP-54 Phase 14.1 lowering
// emits a mochiHttpGet call and the helper that dispatches between
// `file://` (os.ReadFile) and http(s):// (net/http.Get). The
// fetch_basic fixture exercises the file scheme.
func TestPhase14FetchSource(t *testing.T) {
	if runtime.GOOS == "windows" {
		t.Skip("Phase 1 ships POSIX `go` invocation only; Windows lands in Phase 16")
	}
	root := repoRoot(t)
	src := filepath.Join(root, "tests", "transpiler3", "go", "fixtures", "fetch_basic", "fetch_basic.mochi")
	outBin := filepath.Join(t.TempDir(), "fetch_basic")
	d := &Driver{CacheDir: t.TempDir(), KeepWorkDir: true}
	if err := d.Build(src, outBin, "", ""); err != nil {
		t.Fatalf("Driver.Build: %v", err)
	}
	defer os.RemoveAll(d.WorkDirPath)

	mainPath := filepath.Join(d.WorkDirPath, "main.go")
	srcBytes, err := os.ReadFile(mainPath)
	if err != nil {
		t.Fatalf("read main.go: %v", err)
	}
	got := string(srcBytes)

	for _, marker := range []string{
		`mochiHttpGet("file:///tmp/mochi_go_fetch1.txt")`,
		"func mochiHttpGet(urlStr string) string",
		`strings.HasPrefix(urlStr, "file://")`,
		"http.Get(urlStr)",
		"io.ReadAll(resp.Body)",
	} {
		if !strings.Contains(got, marker) {
			t.Fatalf("main.go missing %q:\n%s", marker, got)
		}
	}
}
