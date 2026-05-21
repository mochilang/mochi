package migrate

import (
	"errors"
	"testing"
)

// fakeRunner is the test injection point.
type fakeRunner struct {
	legacy Result
	new    Result
}

func (r fakeRunner) RunLegacy(string) Result { return r.legacy }
func (r fakeRunner) RunNew(string) Result    { return r.new }

// TestCompareResultsEqual asserts the diff is "match" when both legs
// agree on stdout + exit code.
func TestCompareResultsEqual(t *testing.T) {
	legacy := Result{Stdout: []byte("42\n"), ExitCode: 0}
	new := Result{Stdout: []byte("42\n"), ExitCode: 0}
	d := CompareResults(legacy, new)
	if !d.StdoutEqual || !d.ExitEqual {
		t.Errorf("CompareResults equal mismatch: %+v", d)
	}
}

// TestCompareResultsDiff asserts the diff is "no match" when stdout
// differs.
func TestCompareResultsDiff(t *testing.T) {
	legacy := Result{Stdout: []byte("42\n"), ExitCode: 0}
	new := Result{Stdout: []byte("43\n"), ExitCode: 0}
	d := CompareResults(legacy, new)
	if d.StdoutEqual {
		t.Errorf("CompareResults flagged stdout-equal on differing legs: %+v", d)
	}
}

// TestRunBothPending asserts the harness treats ErrNewPathPending as a
// soft pass: legacy runs, new is pending, the diff reports equal so
// the suite continues to be green.
func TestRunBothPending(t *testing.T) {
	r := fakeRunner{
		legacy: Result{Stdout: []byte("42\n"), ExitCode: 0},
		new:    Result{Err: ErrNewPathPending},
	}
	_, newRes, d := RunBoth(r, "fixture.mochi")
	if !errors.Is(newRes.Err, ErrNewPathPending) {
		t.Errorf("new leg should be pending; got Err=%v", newRes.Err)
	}
	if !d.StdoutEqual || !d.ExitEqual {
		t.Errorf("pending new leg should yield equal diff; got %+v", d)
	}
}

// TestRunBothMatch asserts the harness reports equal when both legs
// produce the same output.
func TestRunBothMatch(t *testing.T) {
	r := fakeRunner{
		legacy: Result{Stdout: []byte("42\n"), ExitCode: 0},
		new:    Result{Stdout: []byte("42\n"), ExitCode: 0},
	}
	_, _, d := RunBoth(r, "fixture.mochi")
	if !d.StdoutEqual || !d.ExitEqual {
		t.Errorf("matching legs should diff equal; got %+v", d)
	}
}

// TestRunBothDiverge asserts the harness reports unequal when legs
// disagree.
func TestRunBothDiverge(t *testing.T) {
	r := fakeRunner{
		legacy: Result{Stdout: []byte("42\n"), ExitCode: 0},
		new:    Result{Stdout: []byte("999\n"), ExitCode: 0},
	}
	_, _, d := RunBoth(r, "fixture.mochi")
	if d.StdoutEqual {
		t.Errorf("diverging legs should diff unequal; got %+v", d)
	}
}

// TestDefaultRunnerIsPending asserts the default Runner returns
// ErrNewPathPending for the new leg.
func TestDefaultRunnerIsPending(t *testing.T) {
	r := Default()
	res := r.RunNew("anything.mochi")
	if !errors.Is(res.Err, ErrNewPathPending) {
		t.Errorf("Default.RunNew should be pending; got Err=%v", res.Err)
	}
}
