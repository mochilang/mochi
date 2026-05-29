package build

import (
	"os"
	"path/filepath"
	"strings"
	"testing"
)

// TestPhase8Datalog walks the Phase 8 Datalog fixtures and exercises
// each through the full PHP transpiler. Tests skip when PHP is not
// installed; CI uses shivammathur/setup-php@v2 to drive the
// end-to-end gate.
func TestPhase8Datalog(t *testing.T) {
	fixtureDir := filepath.Join(repoRoot(t), "tests", "transpiler3", "php", "fixtures", "phase08-datalog")
	entries, err := os.ReadDir(fixtureDir)
	if err != nil {
		t.Fatalf("ReadDir %s: %v", fixtureDir, err)
	}
	for _, e := range entries {
		if !strings.HasSuffix(e.Name(), ".mochi") {
			continue
		}
		name := strings.TrimSuffix(e.Name(), ".mochi")
		t.Run(name, func(t *testing.T) {
			runPhpFixture(t,
				filepath.Join(fixtureDir, e.Name()),
				filepath.Join(fixtureDir, name+".out"))
		})
	}
}

// TestPhase8EmitFragments asserts that the PHP target evaluates the
// Datalog program at compile time (matching the BEAM backend's
// strategy) and emits a static array literal of result strings.
// Coverage spans: ground-fact query, transitive closure via recursive
// rule, empty result, X != Y inequality body literals, multiple
// queries in one program, and three flavours of stratified negation
// (orphan/indirect/complement).
func TestPhase8EmitFragments(t *testing.T) {
	cases := []struct {
		fixture string
		wants   []string
	}{
		{
			fixture: "dl_parent_basic.mochi",
			wants: []string{
				// Static array literal of free-var matches; the engine
				// is evaluated at compile time so no PHP runtime
				// helpers are needed.
				`$xs = ["bob", "carol"];`,
			},
		},
		{
			fixture: "dl_ancestor.mochi",
			wants: []string{
				// Transitive closure: ancestor(tom, Y) reaches all
				// downstream nodes through fixpoint iteration.
				`$xs = ["bob", "ann", "pat"];`,
			},
		},
		{
			fixture: "dl_reachability.mochi",
			wants: []string{
				`$xs = ["b", "c", "d"];`,
			},
		},
		{
			fixture: "dl_empty_result.mochi",
			wants: []string{
				// No ground-fact match returns an empty array literal.
				`$xs = [];`,
			},
		},
		{
			fixture: "dl_siblings.mochi",
			wants: []string{
				// X != Y body literal filters self-sibling matches.
				`$xs = ["carol", "dave"];`,
			},
		},
		{
			fixture: "dl_multi_query.mochi",
			wants: []string{
				// Each `let q = query ...` runs its own evaluator pass
				// over the same DatalogProgram.
				`$reds = ["red"];`,
				`$blues = ["blue"];`,
			},
		},
		{
			fixture: "neg_orphan.mochi",
			wants: []string{
				// `not has_child(X)` keeps only people whose parent
				// tuples don't appear; carol has no parent-tuple LHS.
				`$results = ["carol"];`,
			},
		},
		{
			fixture: "neg_indirect.mochi",
			wants: []string{
				// `not has_blocked_friend(X)` over a stratified rule.
				`$results = ["bob", "carol"];`,
			},
		},
		{
			fixture: "neg_complement.mochi",
			wants: []string{
				// Stratified negation over a transitive closure: a is
				// not reachable from itself in this dataset, and d has
				// no incoming edges, so both are isolated.
				`$results = ["a", "d"];`,
			},
		},
		{
			fixture: "dl_chain.mochi",
			wants: []string{
				// Length-4 transitive closure exercises the
				// semi-naive evaluator's iterate-until-fixpoint loop
				// (lower/datalog.go:dlDeriveRule). The result order
				// is the derivation order, so a regression that
				// re-ordered tuples in dlTupleIn would surface here.
				`$xs = ["n2", "n3", "n4", "n5"];`,
			},
		},
		{
			fixture: "dl_filter_const.mochi",
			wants: []string{
				// Query with a constant LHS argument (`like("alice", Y)`)
				// locks the binding/unification path where one query
				// arg is a literal and the other is the binder.
				`$xs = ["cats", "birds"];`,
			},
		},
	}

	for _, c := range cases {
		t.Run(strings.TrimSuffix(c.fixture, ".mochi"), func(t *testing.T) {
			mochiPath := filepath.Join(repoRoot(t), "tests", "transpiler3", "php", "fixtures", "phase08-datalog", c.fixture)
			if _, err := os.Stat(mochiPath); err != nil {
				t.Skipf("fixture missing: %v", err)
			}
			outDir := t.TempDir()
			d := &Driver{CacheDir: t.TempDir(), NoCache: true}
			p, err := d.Build(mochiPath, outDir, TargetPhpSource)
			if err != nil {
				t.Fatalf("Build(%s): %v", c.fixture, err)
			}
			data, err := os.ReadFile(p)
			if err != nil {
				t.Fatalf("read %s: %v", p, err)
			}
			src := string(data)
			for _, want := range c.wants {
				if !strings.Contains(src, want) {
					t.Errorf("%s: emitted source missing %q\n---\n%s", c.fixture, want, src)
				}
			}
		})
	}
}
