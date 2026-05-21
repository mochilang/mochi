package migrate

import (
	"errors"
	"fmt"
	"os"
	"path/filepath"
	"sort"
	"strings"
)

// FixtureResult is the per-fixture outcome reported by RunCorpus.
type FixtureResult struct {
	Path     string
	Diff     Diff
	Legacy   Result
	New      Result
	Pending  bool
	Mismatch bool
}

// CorpusReport summarizes a run over a fixture directory.
type CorpusReport struct {
	Total     int
	Pending   int
	Match     int
	Mismatch  int
	Fixtures  []FixtureResult
}

// LoadGoldenLegacy returns a Runner whose RunLegacy reads the golden
// stdout file next to the fixture (e.g. `let_and_print.mochi` -> `let_and_print.out`)
// as the legacy result. RunNew is delegated. Used by RunCorpus when
// the actual legacy transpiler shouldn't be invoked; the golden output
// is the canonical reference instead.
type LoadGoldenLegacy struct {
	New Runner
}

// RunLegacy reads the golden stdout file matching the fixture path.
// The convention in tests/vm/valid is `<base>.mochi` paired with
// `<base>.out`; the `.mochi.out` sibling is a different artifact (IR
// dump) and is not the stdout golden.
func (g LoadGoldenLegacy) RunLegacy(fixture string) Result {
	base := strings.TrimSuffix(fixture, ".mochi")
	out, err := os.ReadFile(base + ".out")
	if err != nil {
		return Result{Err: fmt.Errorf("golden: %w", err)}
	}
	return Result{Stdout: out, ExitCode: 0}
}

// RunNew delegates to the wrapped runner; nil falls back to pending.
func (g LoadGoldenLegacy) RunNew(fixture string) Result {
	if g.New == nil {
		return PendingRunner{}.RunNew(fixture)
	}
	return g.New.RunNew(fixture)
}

// RunCorpus walks fixtureDir for *.mochi files and runs each through
// RunBoth with the given runner. Fixtures whose new leg is pending are
// counted separately so a report with N pending and 0 mismatch is the
// expected Phase 9 baseline until the frontend lights up.
func RunCorpus(r Runner, fixtureDir string) (CorpusReport, error) {
	var fixtures []string
	err := filepath.Walk(fixtureDir, func(path string, info os.FileInfo, err error) error {
		if err != nil {
			return err
		}
		if info.IsDir() {
			return nil
		}
		if strings.HasSuffix(path, ".mochi") {
			fixtures = append(fixtures, path)
		}
		return nil
	})
	if err != nil {
		return CorpusReport{}, err
	}
	sort.Strings(fixtures)

	rep := CorpusReport{Total: len(fixtures)}
	for _, f := range fixtures {
		legacy, newRes, d := RunBoth(r, f)
		fr := FixtureResult{Path: f, Diff: d, Legacy: legacy, New: newRes}
		if errors.Is(newRes.Err, ErrNewPathPending) {
			fr.Pending = true
			rep.Pending++
		} else if !d.StdoutEqual || !d.ExitEqual {
			fr.Mismatch = true
			rep.Mismatch++
		} else {
			rep.Match++
		}
		rep.Fixtures = append(rep.Fixtures, fr)
	}
	return rep, nil
}

// String renders a one-line corpus summary and a per-fixture suffix
// showing the first few mismatches if any. Pending is reported but not
// treated as failure.
func (r CorpusReport) String() string {
	var b strings.Builder
	fmt.Fprintf(&b, "corpus: %d fixtures, %d match, %d mismatch, %d pending\n",
		r.Total, r.Match, r.Mismatch, r.Pending)
	shown := 0
	for _, f := range r.Fixtures {
		if !f.Mismatch {
			continue
		}
		fmt.Fprintf(&b, "  mismatch: %s\n", f.Path)
		shown++
		if shown >= 10 {
			fmt.Fprintf(&b, "  ... (%d more)\n", r.Mismatch-shown)
			break
		}
	}
	return b.String()
}
