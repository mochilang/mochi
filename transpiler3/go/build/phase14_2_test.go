package build

import (
	"bytes"
	"os"
	"os/exec"
	"path/filepath"
	"runtime"
	"sort"
	"strings"
	"testing"
)

// TestPhase14_2JsonDecode is the MEP-54 Phase 14.2 gate for
// `json_decode(s)`. Each fixture lives flat under
// tests/transpiler3/go/fixtures with a `setup/` sibling
// containing one or more *.json files that must exist at /tmp
// before the binary runs (the fixture .mochi reads them via
// readFile so the JSON content never appears in source — the
// C lowerer's format-string parsing rejects literal `{`).
//
// The test copies every setup/*.json into /tmp/<filename>
// before exec and removes them on cleanup. Phase 1's walker
// already skips any fixture with a `setup/` subdir.
func TestPhase14_2JsonDecode(t *testing.T) {
	if runtime.GOOS == "windows" {
		t.Skip("Phase 1 ships POSIX `go` invocation only; Windows lands in Phase 16")
	}
	root := repoRoot(t)
	base := filepath.Join(root, "tests", "transpiler3", "go", "fixtures")
	entries, err := os.ReadDir(base)
	if err != nil {
		t.Fatalf("read fixtures dir: %v", err)
	}

	var names []string
	for _, e := range entries {
		if !e.IsDir() {
			continue
		}
		if !strings.HasPrefix(e.Name(), "json_decode_") {
			continue
		}
		if _, err := os.Stat(filepath.Join(base, e.Name(), "setup")); err != nil {
			continue
		}
		names = append(names, e.Name())
	}
	sort.Strings(names)
	if len(names) == 0 {
		t.Fatalf("no json_decode fixtures under %s", base)
	}

	for _, name := range names {
		t.Run(name, func(t *testing.T) {
			fixture := filepath.Join(base, name)
			src := filepath.Join(fixture, name+".mochi")
			want, err := os.ReadFile(filepath.Join(fixture, "expect.txt"))
			if err != nil {
				t.Fatalf("read expect.txt: %v", err)
			}

			setupDir := filepath.Join(fixture, "setup")
			setupFiles, err := os.ReadDir(setupDir)
			if err != nil {
				t.Fatalf("read setup dir: %v", err)
			}
			var planted []string
			for _, sf := range setupFiles {
				if sf.IsDir() {
					continue
				}
				srcPath := filepath.Join(setupDir, sf.Name())
				dstPath := filepath.Join("/tmp", sf.Name())
				b, err := os.ReadFile(srcPath)
				if err != nil {
					t.Fatalf("read setup %s: %v", srcPath, err)
				}
				if err := os.WriteFile(dstPath, b, 0o644); err != nil {
					t.Fatalf("write /tmp %s: %v", dstPath, err)
				}
				planted = append(planted, dstPath)
			}
			t.Cleanup(func() {
				for _, p := range planted {
					_ = os.Remove(p)
				}
			})

			outBin := filepath.Join(t.TempDir(), name)
			d := &Driver{CacheDir: t.TempDir()}
			if err := d.Build(src, outBin, "", ""); err != nil {
				t.Fatalf("Driver.Build: %v", err)
			}

			cmd := exec.Command(outBin)
			var stdout bytes.Buffer
			cmd.Stdout = &stdout
			cmd.Stderr = os.Stderr
			if err := cmd.Run(); err != nil {
				t.Fatalf("run %s: %v", outBin, err)
			}
			if got := stdout.String(); got != string(want) {
				t.Fatalf("stdout mismatch:\n--- want ---\n%q\n--- got ---\n%q", string(want), got)
			}
		})
	}
}

// TestPhase14_2JsonDecodeSource gates the lowering shape for
// json_decode_basic without executing the binary. It verifies
// the call routes through mochiJsonDecode and that the helper
// uses encoding/json with a string-coercing fallback for
// non-string scalar values.
func TestPhase14_2JsonDecodeSource(t *testing.T) {
	if runtime.GOOS == "windows" {
		t.Skip("Phase 1 ships POSIX `go` invocation only; Windows lands in Phase 16")
	}
	root := repoRoot(t)
	src := filepath.Join(root, "tests", "transpiler3", "go", "fixtures", "json_decode_basic", "json_decode_basic.mochi")
	outBin := filepath.Join(t.TempDir(), "json_decode_basic")
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
		"mochiJsonDecode(",
		"func mochiJsonDecode(s string) map[string]string",
		`"encoding/json"`,
		"json.Unmarshal([]byte(s), &raw)",
		"map[string]interface{}",
	} {
		if !strings.Contains(got, marker) {
			t.Fatalf("main.go missing %q:\n%s", marker, got)
		}
	}
}
