package build

import (
	"os"
	"path/filepath"
	"strings"
	"testing"
)

// TestPhase8Datalog is the Phase 8 primary gate. Every fixture under
// tests/transpiler3/typescript/fixtures/phase08-datalog must lower via
// the TypeScript transpiler and produce byte-equal stdout on Node 22,
// Deno 2, and Bun 1.1 against the vm3-recorded .out.
//
// Phase 8 lands Mochi's Datalog sub-language on TypeScript by routing
// `query Rel(args)` through a compile-time semi-naive bottom-up
// evaluator that runs against the aotir DatalogProgram snapshot at
// build time. The evaluator computes the fixed-point of (facts, rules)
// and emits the matching free-variable values as a static `string[]`
// literal. The C transpiler ships the equivalent fixed-point loop as a
// per-query RawCStmt; the TS transpiler skips the RawCStmt because the
// result list is already baked into the emit.
//
// This strategy has three consequences worth recording:
//
//   - The `@mochi/runtime/datalog` runtime budget the original spec
//     allocated (8 KB gzipped) drops to zero on TypeScript. There is
//     no engine to ship.
//   - The emit is fully reproducible: byte-equal stdout between
//     runtimes is trivial because the result list is computed once at
//     build time and rendered into the source.
//   - Mochi's Datalog admits no runtime fact ingestion (no `assert`
//     statement, no fact-injection FFI in Phase 8), so the
//     compile-time fixed-point is observationally identical to a
//     runtime engine. If a future phase adds runtime facts, the
//     emitter switches to a runtime engine without disturbing the
//     test contract here.
//
// The implementation mirrors the Rust transpiler's compile-time
// evaluator at transpiler3/rust/lower/lower.go:1681 (the same
// algorithm, the same fact/rule unification, the same
// stratification-via-iteration-order rules). 20 fixtures cover:
//
//   - Single fact projection (dl_parent_basic, dl_two_facts)
//   - Bound constants in the query head (dl_const_query, dl_filter_const)
//   - Recursive rules with transitive closure (dl_ancestor,
//     dl_chain, dl_reachability, ms_*)
//   - Inequality literals (dl_siblings, ms_sibling)
//   - Stratified negation-as-failure (neg_orphan, neg_complement,
//     neg_indirect)
//   - Empty result and no-match edge cases (dl_empty_result,
//     dl_no_match, dl_rule_const)
//   - Multi-query programs with shared fact base (dl_multi_query)
//
// Phase 8 ships 20 fixtures; the floor is 20.
func TestPhase8DatalogNode(t *testing.T) { runPhase8FixturesOn(t, "node") }
func TestPhase8DatalogDeno(t *testing.T) { runPhase8FixturesOn(t, "deno") }
func TestPhase8DatalogBun(t *testing.T)  { runPhase8FixturesOn(t, "bun") }

func runPhase8FixturesOn(t *testing.T, runtime string) {
	t.Helper()
	fixtureDir := filepath.Join(repoRoot(t), "tests", "transpiler3", "typescript", "fixtures", "phase08-datalog")
	entries, err := os.ReadDir(fixtureDir)
	if err != nil {
		t.Fatalf("ReadDir %s: %v", fixtureDir, err)
	}
	mochiCount := 0
	for _, e := range entries {
		if !strings.HasSuffix(e.Name(), ".mochi") {
			continue
		}
		mochiCount++
		name := strings.TrimSuffix(e.Name(), ".mochi")
		t.Run(name, func(t *testing.T) {
			runTsFixture(t, runtime,
				filepath.Join(fixtureDir, e.Name()),
				filepath.Join(fixtureDir, name+".out"))
		})
	}
	if mochiCount < 20 {
		t.Fatalf("Phase 8 fixture corpus has %d .mochi files, expected at least 20", mochiCount)
	}
}

// TestPhase8EmitShape asserts the load-bearing tokens of the Datalog
// lowering are present in the emit. The invariants encode the chosen
// strategy (compile-time eval + static list literal) and rule out two
// alternatives that would produce the same stdout but break later
// phases:
//
//   - Runtime engine. Would surface as `mochi_datalog_eval(`,
//     `new FactDB(`, or a `rules: Rule[]` array literal. Breaks the
//     Phase 16 byte-equal reproducibility gate (engine version skew)
//     and Phase 15 size budget (8 KB+ engine in every package).
//   - Per-query inline imperative loop. Would surface as a
//     `for (const t of relation)` over a `Map<string, string[][]>`.
//     Breaks Phase 16 because the loop body is sensitive to the
//     aotir fact/rule order, and rewrites between aotir versions
//     would shift the emit even when behaviour was identical.
//
// What is asserted instead: the emit is a plain `: string[] =
// [...]` literal with the expected string contents, and the C-only
// pre-rendered fixed-point setup block (RawCStmt) is dropped. This is
// the tightest invariant that distinguishes compile-time eval from
// every runtime-engine variant.
func TestPhase8EmitShape(t *testing.T) {
	fixtureDir := filepath.Join(repoRoot(t), "tests", "transpiler3", "typescript", "fixtures", "phase08-datalog")
	cases := []struct {
		fixture string
		wants   []string
	}{
		{
			fixture: "dl_parent_basic.mochi",
			wants: []string{
				`: string[] = ["bob", "carol"]`,
			},
		},
		{
			fixture: "dl_ancestor.mochi",
			wants: []string{
				`: string[] = ["bob", "ann", "pat"]`,
			},
		},
		{
			fixture: "dl_empty_result.mochi",
			wants: []string{
				`: string[] = []`,
			},
		},
		{
			fixture: "dl_siblings.mochi",
			wants: []string{
				`: string[] = ["carol", "dave"]`,
			},
		},
		{
			fixture: "neg_orphan.mochi",
			wants: []string{
				`: string[] = ["carol"]`,
			},
		},
	}
	for _, tc := range cases {
		t.Run(strings.TrimSuffix(tc.fixture, ".mochi"), func(t *testing.T) {
			outDir := t.TempDir()
			d := &Driver{CacheDir: t.TempDir(), NoCache: true}
			p, err := d.Build(filepath.Join(fixtureDir, tc.fixture), outDir, TargetTypeScriptSource)
			if err != nil {
				t.Fatalf("Build %s: %v", tc.fixture, err)
			}
			src := readTrim(t, p)
			for _, want := range tc.wants {
				if !strings.Contains(src, want) {
					t.Errorf("%s emit missing %q\n---\n%s", tc.fixture, want, src)
				}
			}
		})
	}
}

// TestPhase8NoRuntimeEngine asserts that no Datalog runtime engine
// tokens leak into the TS emit. The compile-time evaluator must fully
// consume the (facts, rules, query) shape; nothing of the C
// transpiler's per-query fixed-point setup (RawCStmt body) and nothing
// of a hypothetical runtime engine (`@mochi/runtime/datalog/...`,
// `FactDB`, `evaluateDatalog`) should appear.
//
// If this test fails the most likely cause is that lowerRawCStmt
// regressed to passing the C code through, or that an engine import
// was added inadvertently.
func TestPhase8NoRuntimeEngine(t *testing.T) {
	fixtureDir := filepath.Join(repoRoot(t), "tests", "transpiler3", "typescript", "fixtures", "phase08-datalog")
	entries, err := os.ReadDir(fixtureDir)
	if err != nil {
		t.Fatalf("ReadDir: %v", err)
	}
	forbidden := []string{
		"mochi_datalog_",
		"FactDB",
		"evaluateDatalog",
		"@mochi/runtime/datalog",
		"strcmp(",
		"mochi_list_str_",
	}
	for _, e := range entries {
		if !strings.HasSuffix(e.Name(), ".mochi") {
			continue
		}
		t.Run(strings.TrimSuffix(e.Name(), ".mochi"), func(t *testing.T) {
			outDir := t.TempDir()
			d := &Driver{CacheDir: t.TempDir(), NoCache: true}
			p, err := d.Build(filepath.Join(fixtureDir, e.Name()), outDir, TargetTypeScriptSource)
			if err != nil {
				t.Fatalf("Build: %v", err)
			}
			src := readTrim(t, p)
			for _, f := range forbidden {
				if strings.Contains(src, f) {
					t.Errorf("%s emit leaked engine token %q\n---\n%s", e.Name(), f, src)
				}
			}
		})
	}
}
