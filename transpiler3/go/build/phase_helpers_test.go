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

// runFixturesMatchingPrefixes runs every fixture directory under the shared
// fixtures base whose name matches at least one of the given prefixes. Each
// matching fixture is run as a sub-test: parse → type-check → aotir lower →
// gotree lower → emit → go build → exec, with stdout diffed against expect.txt.
func runFixturesMatchingPrefixes(t *testing.T, prefixes ...string) {
	t.Helper()
	if runtime.GOOS == "windows" {
		t.Skip("POSIX go invocation only; Windows support lands in Phase 16")
	}
	root := repoRoot(t)
	base := filepath.Join(root, "tests", "transpiler3", "go", "fixtures")
	entries, err := os.ReadDir(base)
	if err != nil {
		t.Fatalf("ReadDir %s: %v", base, err)
	}
	matched := 0
	for _, e := range entries {
		if !e.IsDir() {
			continue
		}
		name := e.Name()
		if !matchesAnyPrefix(name, prefixes) {
			continue
		}
		// Skip fixtures that require ambient setup files (setup/) or a
		// cassette dir (cassette/); their dedicated test functions own them.
		if _, err := os.Stat(filepath.Join(base, name, "setup")); err == nil {
			continue
		}
		if _, err := os.Stat(filepath.Join(base, name, "cassette")); err == nil {
			continue
		}
		matched++
		fixtureName := name
		t.Run(name, func(t *testing.T) {
			t.Parallel()
			name = fixtureName
			runGoFixture(t, filepath.Join(base, name), name)
		})
	}
	if matched == 0 {
		t.Fatalf("no fixtures matched prefixes %v under %s", prefixes, base)
	}
}

func matchesAnyPrefix(name string, prefixes []string) bool {
	for _, p := range prefixes {
		if strings.HasPrefix(name, p) {
			return true
		}
	}
	return false
}

// runGoFixture runs a single named fixture: builds and executes the binary,
// then diffs stdout against expect.txt.
func runGoFixture(t *testing.T, fixture, name string) {
	t.Helper()
	src := filepath.Join(fixture, name+".mochi")
	want, err := os.ReadFile(filepath.Join(fixture, "expect.txt"))
	if err != nil {
		t.Fatalf("read expect.txt: %v", err)
	}
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
}
