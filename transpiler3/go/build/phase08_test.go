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

// TestPhase8Datalog is the MEP-54 Phase 8.0 gate. It runs each
// dl_* fixture through the full pipeline (parse, type-check, aotir
// lower, gotree lower, emit, go build, exec) and diffs the binary's
// stdout against expect.txt.
//
// Datalog is evaluated at lowering time by lowerDatalogQueryExpr,
// so the generated Go file contains a static `[]string{...}` for the
// query result. The print loop and free-variable layout match the
// C and BEAM backends, keeping per-target output byte-identical.
//
// Fixture set (mirrors the BEAM phase08_datalog suite):
//
//	dl_facts          - grandparent over two parent facts
//	dl_connected      - recursive transitive closure over link
//	dl_sibling        - join over parent with a bound free variable
//	dl_multi_freevar  - path with multiple free vars
//	dl_negation       - negation as failure (dead = person and not alive)
func TestPhase8Datalog(t *testing.T) {
	if runtime.GOOS == "windows" {
		t.Skip("Phase 1 ships POSIX `go` invocation only; Windows lands in Phase 16")
	}
	root := repoRoot(t)
	base := filepath.Join(root, "tests", "transpiler3", "go", "fixtures")
	names := []string{
		"dl_facts",
		"dl_connected",
		"dl_sibling",
		"dl_multi_freevar",
		"dl_negation",
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

// TestPhase8DatalogStaticEval verifies the Go backend evaluates
// Datalog at compile time: the generated main.go for a Datalog
// fixture must contain the resolved `[]string{...}` slice literal
// inline (no runtime engine call) so the binary is independent of
// any datalog runtime package.
func TestPhase8DatalogStaticEval(t *testing.T) {
	if runtime.GOOS == "windows" {
		t.Skip("Phase 1 ships POSIX `go` invocation only; Windows lands in Phase 16")
	}
	root := repoRoot(t)
	src := filepath.Join(root, "tests", "transpiler3", "go", "fixtures", "dl_facts", "dl_facts.mochi")
	d := &Driver{CacheDir: t.TempDir(), KeepWorkDir: true}
	outBin := filepath.Join(t.TempDir(), "dl_facts")
	if err := d.Build(src, outBin, "", ""); err != nil {
		t.Fatalf("Driver.Build: %v", err)
	}
	defer os.RemoveAll(d.WorkDirPath)

	main, err := os.ReadFile(filepath.Join(d.WorkDirPath, "main.go"))
	if err != nil {
		t.Fatalf("read main.go: %v", err)
	}
	got := string(main)
	if !strings.Contains(got, `[]string{"Alice", "Carol"}`) {
		t.Fatalf("expected inline []string{\"Alice\", \"Carol\"} in main.go, got:\n%s", got)
	}
	if strings.Contains(got, `runtime/datalog`) {
		t.Fatalf("main.go must not import runtime/datalog (compile-time eval); got:\n%s", got)
	}
}
