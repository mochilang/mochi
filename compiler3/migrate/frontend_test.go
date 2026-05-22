package migrate

import (
	"errors"
	"os"
	"path/filepath"
	"testing"
)

// TestFrontendRunnerEndToEnd asserts FrontendRunner runs the full
// parse + lower + emit + go-run pipeline on a fixture the MVP supports.
// Pairs with LoadGoldenLegacy so the diff compares against the .out
// stdout golden.
func TestFrontendRunnerEndToEnd(t *testing.T) {
	dir := t.TempDir()
	src := "let a = 10\nlet b: int = 20\nprint(a + b)\n"
	mochiPath := filepath.Join(dir, "demo.mochi")
	outPath := filepath.Join(dir, "demo.out")
	if err := os.WriteFile(mochiPath, []byte(src), 0o644); err != nil {
		t.Fatalf("write mochi: %v", err)
	}
	if err := os.WriteFile(outPath, []byte("30\n"), 0o644); err != nil {
		t.Fatalf("write golden: %v", err)
	}

	runner := LoadGoldenLegacy{New: FrontendRunner{}}
	legacy, new, diff := RunBoth(runner, mochiPath)
	if new.Err != nil && !errors.Is(new.Err, ErrNewPathPending) {
		t.Fatalf("RunNew failed: %v\nstdout=%q", new.Err, new.Stdout)
	}
	if errors.Is(new.Err, ErrNewPathPending) {
		t.Fatal("expected non-pending result for a supported fixture")
	}
	if !diff.StdoutEqual {
		t.Errorf("stdout differs: legacy=%q new=%q", legacy.Stdout, new.Stdout)
	}
}

// TestFrontendRunnerPendingForUnsupportedSurface asserts FrontendRunner
// surfaces MVP-unsupported sources as ErrNewPathPending so the A/B
// harness records a skip rather than a regression. Phase 4.2.0 lifted
// the string-literal restriction, so the durable unsupported surface
// is now the `none` literal.
func TestFrontendRunnerPendingForUnsupportedSurface(t *testing.T) {
	dir := t.TempDir()
	src := `let s = none` + "\n"
	mochiPath := filepath.Join(dir, "demo.mochi")
	if err := os.WriteFile(mochiPath, []byte(src), 0o644); err != nil {
		t.Fatalf("write mochi: %v", err)
	}

	res := FrontendRunner{}.RunNew(mochiPath)
	if !errors.Is(res.Err, ErrNewPathPending) {
		t.Errorf("got err=%v, want ErrNewPathPending", res.Err)
	}
}
