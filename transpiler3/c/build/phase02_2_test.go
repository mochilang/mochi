package build

import (
	"bytes"
	"os"
	"os/exec"
	"path/filepath"
	"runtime"
	"sort"
	"testing"
)

// TestPhase2Functions is the MEP-45 Phase 2.2 gate for user-defined
// functions. It walks every fixture directory under
// tests/transpiler3/c/fixtures/functions, runs the end-to-end
// pipeline (parse, type-check, lower, emit, cc, link, exec), and
// diffs the binary's stdout against the fixture's expect.txt.
func TestPhase2Functions(t *testing.T) {
	if runtime.GOOS == "windows" {
		t.Skip("Phase 1 ships host-cc discovery only on POSIX; Windows lands in Phase 11")
	}
	runFixtureSuite(t, "functions")
}

// TestPhase2ForRange is the MEP-45 Phase 2.2 gate for `for x in
// start..end` integer half-open ranges. Mirror of
// TestPhase2Functions but rooted at the for-range fixture set so
// the two surfaces are scored independently in the gate report.
func TestPhase2ForRange(t *testing.T) {
	if runtime.GOOS == "windows" {
		t.Skip("Phase 1 ships host-cc discovery only on POSIX; Windows lands in Phase 11")
	}
	runFixtureSuite(t, "for-range")
}

// runFixtureSuite drives the standard fixture walker over the
// directory at tests/transpiler3/c/fixtures/<dir>. Each subdirectory
// containing `<name>.mochi` + `expect.txt` becomes one subtest.
func runFixtureSuite(t *testing.T, dir string) {
	t.Helper()
	root := repoRoot(t)
	base := filepath.Join(root, "tests", "transpiler3", "c", "fixtures", dir)
	entries, err := os.ReadDir(base)
	if err != nil {
		t.Fatalf("read fixtures dir: %v", err)
	}

	var names []string
	for _, e := range entries {
		if e.IsDir() {
			names = append(names, e.Name())
		}
	}
	sort.Strings(names)
	if len(names) == 0 {
		t.Fatalf("no fixtures under %s", base)
	}

	for _, name := range names {
		t.Run(name, func(t *testing.T) {
			fixture := filepath.Join(base, name)
			src := filepath.Join(fixture, name+".mochi")
			expect, err := os.ReadFile(filepath.Join(fixture, "expect.txt"))
			if err != nil {
				t.Fatalf("read expect.txt: %v", err)
			}

			outBin := filepath.Join(t.TempDir(), name)
			d := &Driver{CacheDir: t.TempDir()}
			if err := d.Build(src, outBin, "", ""); err != nil {
				t.Fatalf("Driver.Build %s: %v", src, err)
			}
			if _, err := os.Stat(outBin); err != nil {
				t.Fatalf("stat output binary: %v", err)
			}

			cmd := exec.Command(outBin)
			var stdout bytes.Buffer
			cmd.Stdout = &stdout
			cmd.Stderr = os.Stderr
			if err := cmd.Run(); err != nil {
				t.Fatalf("run %s: %v", outBin, err)
			}
			if got := stdout.String(); got != string(expect) {
				t.Fatalf("stdout mismatch for %s:\n--- want ---\n%q\n--- got ---\n%q",
					name, string(expect), got)
			}
		})
	}
}
