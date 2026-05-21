package migrate

import (
	"os"
	"path/filepath"
	"strings"
	"testing"
)

// writeFixture writes a paired .mochi + .out to dir. The .out file is
// the stdout golden the A/B harness compares against; the legacy
// transpiler's IR-format `.mochi.out` is a different artifact.
func writeFixture(t *testing.T, dir, name, out string) {
	t.Helper()
	if err := os.WriteFile(filepath.Join(dir, name+".mochi"), []byte("print(0)\n"), 0o644); err != nil {
		t.Fatalf("write mochi: %v", err)
	}
	if err := os.WriteFile(filepath.Join(dir, name+".out"), []byte(out), 0o644); err != nil {
		t.Fatalf("write golden: %v", err)
	}
}

// TestRunCorpusAllPending asserts a corpus run with the default
// pending runner reports every fixture as pending (and zero mismatch),
// so Phase 9's Phase 6 dependency stays a soft gate.
func TestRunCorpusAllPending(t *testing.T) {
	dir := t.TempDir()
	writeFixture(t, dir, "a", "1\n")
	writeFixture(t, dir, "b", "2\n")
	rep, err := RunCorpus(LoadGoldenLegacy{New: PendingRunner{}}, dir)
	if err != nil {
		t.Fatalf("run: %v", err)
	}
	if rep.Total != 2 || rep.Pending != 2 || rep.Mismatch != 0 {
		t.Errorf("counts: total=%d pending=%d mismatch=%d match=%d",
			rep.Total, rep.Pending, rep.Mismatch, rep.Match)
	}
}

// fakeNewRunner reproduces the legacy golden for one fixture and a
// divergent payload for another, so RunCorpus can be exercised with a
// concrete non-pending new path.
type fakeNewRunner struct{ outputs map[string]string }

func (f fakeNewRunner) RunLegacy(string) Result { return Result{} }
func (f fakeNewRunner) RunNew(fixture string) Result {
	base := filepath.Base(fixture)
	name := strings.TrimSuffix(base, ".mochi")
	if v, ok := f.outputs[name]; ok {
		return Result{Stdout: []byte(v), ExitCode: 0}
	}
	return Result{Err: ErrNewPathPending}
}

// TestRunCorpusMixedOutcomes covers a corpus where one fixture matches,
// one diverges, and one is pending. The report's counts must reflect
// the per-fixture outcome.
func TestRunCorpusMixedOutcomes(t *testing.T) {
	dir := t.TempDir()
	writeFixture(t, dir, "match", "1\n")
	writeFixture(t, dir, "diverge", "2\n")
	writeFixture(t, dir, "pending", "3\n")

	r := struct {
		LoadGoldenLegacy
	}{LoadGoldenLegacy: LoadGoldenLegacy{New: fakeNewRunner{outputs: map[string]string{
		"match":   "1\n",
		"diverge": "X\n",
	}}}}

	rep, err := RunCorpus(r, dir)
	if err != nil {
		t.Fatalf("run: %v", err)
	}
	if rep.Total != 3 {
		t.Fatalf("total: got %d", rep.Total)
	}
	if rep.Match != 1 || rep.Mismatch != 1 || rep.Pending != 1 {
		t.Errorf("counts: match=%d mismatch=%d pending=%d", rep.Match, rep.Mismatch, rep.Pending)
	}
	s := rep.String()
	if !strings.Contains(s, "1 match") || !strings.Contains(s, "1 mismatch") || !strings.Contains(s, "1 pending") {
		t.Errorf("summary missing counts:\n%s", s)
	}
}
