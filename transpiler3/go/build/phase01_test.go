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

// TestPhase1Hello is the MEP-54 Phase 1 gate. It walks every
// fixture directory under tests/transpiler3/go/fixtures, runs
// the end-to-end pipeline (parse, type-check, aotir lower,
// gotree lower, emit, go build, exec), and diffs the binary's
// stdout against the fixture's expect.txt.
//
// Per-fixture isolation: each sub-test gets its own CacheDir
// so a stray file in one fixture's work directory cannot bleed
// into the next.
func TestPhase1Hello(t *testing.T) {
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
			want, err := os.ReadFile(filepath.Join(fixture, "expect.txt"))
			if err != nil {
				t.Fatalf("read expect.txt: %v", err)
			}

			outBin := filepath.Join(t.TempDir(), name)
			d := &Driver{CacheDir: t.TempDir()}
			if err := d.Build(src, outBin, "", ""); err != nil {
				t.Fatalf("Driver.Build: %v", err)
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
			if got := stdout.String(); got != string(want) {
				t.Fatalf("stdout mismatch:\n--- want ---\n%q\n--- got ---\n%q", string(want), got)
			}
		})
	}
}

// TestPhase1GoMod verifies the emitted go.mod declares the
// `go 1.26.0` floor and pins `toolchain go1.26.3`.
func TestPhase1GoMod(t *testing.T) {
	mod := goModContent("")
	if !strings.Contains(mod, "go 1.26.0") {
		t.Fatalf("missing `go 1.26.0`:\n%s", mod)
	}
	if !strings.Contains(mod, "toolchain go1.26.3") {
		t.Fatalf("missing `toolchain go1.26.3`:\n%s", mod)
	}
	if !strings.Contains(mod, "module mochi_userprog") {
		t.Fatalf("missing default module path:\n%s", mod)
	}
}

// TestPhase1KeepWorkDir verifies the KeepWorkDir flag leaves a
// usable work directory containing main.go + go.mod, and that
// `go vet` succeeds against the generated project.
func TestPhase1KeepWorkDir(t *testing.T) {
	if runtime.GOOS == "windows" {
		t.Skip("Phase 1 ships POSIX `go` invocation only; Windows lands in Phase 16")
	}
	root := repoRoot(t)
	src := filepath.Join(root, "tests", "transpiler3", "go", "fixtures", "hello", "hello.mochi")
	outBin := filepath.Join(t.TempDir(), "hello")
	d := &Driver{CacheDir: t.TempDir(), KeepWorkDir: true}
	if err := d.Build(src, outBin, "", ""); err != nil {
		t.Fatalf("Driver.Build: %v", err)
	}
	defer os.RemoveAll(d.WorkDirPath)

	for _, name := range []string{"main.go", "go.mod"} {
		if _, err := os.Stat(filepath.Join(d.WorkDirPath, name)); err != nil {
			t.Fatalf("missing %s in work dir: %v", name, err)
		}
	}

	// TestGoVetClean gate: vet must succeed on the generated project.
	cmd := exec.Command("go", "vet", "./...")
	cmd.Dir = d.WorkDirPath
	var stderr bytes.Buffer
	cmd.Stderr = &stderr
	if err := cmd.Run(); err != nil {
		t.Fatalf("go vet on generated project: %v\n%s", err, stderr.String())
	}

	// TestGofmtFixedPoint gate: gofmt must report no files needing formatting.
	cmd = exec.Command("gofmt", "-l", "main.go")
	cmd.Dir = d.WorkDirPath
	var out bytes.Buffer
	cmd.Stdout = &out
	cmd.Stderr = &stderr
	if err := cmd.Run(); err != nil {
		t.Fatalf("gofmt -l: %v\n%s", err, stderr.String())
	}
	if got := strings.TrimSpace(out.String()); got != "" {
		t.Fatalf("gofmt reported files needing formatting: %q", got)
	}
}
