package build

import (
	"os"
	"os/exec"
	"path/filepath"
	"testing"
)

// BenchmarkBeamBuild measures the end-to-end compilation pipeline latency
// (parse → C-lower → BEAM-lower → emit → escript) for a representative
// Phase 1 fixture. Phase 18.3 gate: median <500ms on CI hardware.
func BenchmarkBeamBuild(b *testing.B) {
	root := repoRoot(b)
	mochiPath := filepath.Join(root, "tests", "transpiler3", "beam", "fixtures", "phase01", "100_hello_world", "100_hello_world.mochi")
	if _, err := os.Stat(mochiPath); err != nil {
		b.Skipf("fixture not found: %v", err)
	}

	b.ResetTimer()
	for range b.N {
		out := filepath.Join(b.TempDir(), "bench.escript")
		d := &Driver{CacheDir: b.TempDir()}
		if err := d.Build(mochiPath, out, TargetEscript); err != nil {
			b.Fatalf("Build: %v", err)
		}
	}
}

// BenchmarkBeamQuery measures compilation of a query-DSL fixture (Phase 7.2
// group_by) which exercises the full lowering pipeline including hash join
// and group-by desugaring.
func BenchmarkBeamQuery(b *testing.B) {
	root := repoRoot(b)
	mochiPath := filepath.Join(root, "tests", "transpiler3", "beam", "fixtures",
		"phase07_2", "group_by_basic", "group_by_basic.mochi")
	if _, err := os.Stat(mochiPath); err != nil {
		b.Skipf("fixture not found: %v", err)
	}

	b.ResetTimer()
	for range b.N {
		out := filepath.Join(b.TempDir(), "bench.escript")
		d := &Driver{CacheDir: b.TempDir()}
		if err := d.Build(mochiPath, out, TargetEscript); err != nil {
			b.Fatalf("Build: %v", err)
		}
	}
}

// BenchmarkBeamRun measures wall-clock execution of the resulting escript
// for a Phase 1 hello-world fixture.
func BenchmarkBeamRun(b *testing.B) {
	root := repoRoot(b)
	mochiPath := filepath.Join(root, "tests", "transpiler3", "beam", "fixtures", "phase01", "100_hello_world", "100_hello_world.mochi")
	if _, err := os.Stat(mochiPath); err != nil {
		b.Skipf("fixture not found: %v", err)
	}

	// Build once outside the loop.
	out := filepath.Join(b.TempDir(), "bench.escript")
	d := &Driver{CacheDir: b.TempDir()}
	if err := d.Build(mochiPath, out, TargetEscript); err != nil {
		b.Fatalf("Build: %v", err)
	}

	b.ResetTimer()
	for range b.N {
		cmd := exec.Command(out)
		cmd.Stdout = nil
		cmd.Stderr = os.Stderr
		if err := cmd.Run(); err != nil {
			b.Fatalf("run: %v", err)
		}
	}
}

