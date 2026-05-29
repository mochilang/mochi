package build

import (
	"os"
	"path/filepath"
	"strings"
	"testing"
)

// TestPhase7Query is the Phase 7 primary gate. Every fixture under
// tests/transpiler3/typescript/fixtures/phase07-query must lower via
// the TS transpiler and produce byte-equal stdout on Node 22, Deno 2,
// and Bun 1.1 against the vm3-recorded .out.
//
// Phase 7 lands Mochi's query DSL surface:
//
//   from x in xs
//   [from y in ys ...]
//   [join y in ys on x.k == y.k]
//   [left join y in ys on x.k == y.k]
//   [where pred]
//   [order by key]
//   [skip N]
//   [take N]
//   select expr
//
// The aotir lowering pass desugars every query into a pre-allocated
// mutable result list plus a ForEachStmt + AppendExpr inner loop,
// wrapped in a QueryScopeStmt for the C transpiler's arena-allocation
// pass. The TypeScript emitter treats QueryScopeStmt as a transparent
// wrapper (lexical scope + GC handle the lifetimes that the C arena
// is for) and lets the rest of the pipeline route through Phase 2/3
// lowerers: ForEachStmt to `for (const x of xs) {...}`, AppendExpr to
// `[...xs, v]` via SpreadAppendExpr, IfStmt to TS `if`, AssignStmt
// to `__queryN = ...`, ListSortAscExpr to `mochi_list_sort_asc(...)`,
// and ListSliceExpr to `mochi_list_slice(...)`.
//
// Hash-join. When the aotir `extractHashJoinKeys` analysis identifies
// an equality join condition (e.g. `x.id == y.uid`) where the inner
// key depends only on the inner row and the outer key depends only on
// outer rows, the lowerer replaces the O(n*m) nested loop with an
// O(n+m) hash index over the inner side. The index is `Map<K, T[]>`
// because multiple inner rows can share a key. TS Phase 7 widens
// tsTypeForMapSlot to render `Map<K, T[]>` (Phase 3.2 supported only
// scalar values). MapLit, MapHasExpr, MapGetExpr, MapPutStmt all
// thread the `ListValueElemType` field from aotir through the type
// renderer; no new runtime helper is needed (the existing
// `mochi_map_get` returns `V` which is `T[]` here, and `.set` /
// `.has` work uniformly).
//
// Left join. The aotir lowerer expands `left join` into a sentinel-
// flag pattern: `let __anyN = false; for y in ys { if (cond) { ...
// matched } } if (!__anyN) { ... fallback }`. Both the matched and
// fallback bodies append to the result list. Every shape the lowerer
// emits (BoolLit, UnaryExpr UnNotBool, mutable LetStmt + AssignStmt,
// IfStmt) routes through Phase 2 untouched.
//
// Order by + skip + take. The aotir lowerer post-processes the result
// list with optional ListSortAscExpr and ListSliceExpr. Both already
// have TS lowerers from Phase 3.1. The aotir `order by k` form sorts
// by the same expression that select returns; sort-by-arbitrary-key
// is an aotir limitation (the Phase 7 fixture corpus uses `order by
// x` only, never `order by f(x)` with a different projection).
//
// Phase 7 ships 25 fixtures; the floor is 20.
func TestPhase7QueryNode(t *testing.T) { runPhase7FixturesOn(t, "node") }
func TestPhase7QueryDeno(t *testing.T) { runPhase7FixturesOn(t, "deno") }
func TestPhase7QueryBun(t *testing.T)  { runPhase7FixturesOn(t, "bun") }

func runPhase7FixturesOn(t *testing.T, runtime string) {
	t.Helper()
	fixtureDir := filepath.Join(repoRoot(t), "tests", "transpiler3", "typescript", "fixtures", "phase07-query")
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
		t.Fatalf("Phase 7 fixture corpus has %d .mochi files, expected at least 20", mochiCount)
	}
}

// TestPhase7EmitShape asserts that the load-bearing tokens of the
// query lowering are present in the emit. The invariants checked
// here distinguish the shipped lowering (transparent QueryScopeStmt
// wrapper + reuse of Phase 2/3 surface) from an alternative based on
// `Array.prototype.filter/.map/.flatMap` chains. The chain form
// would print the same numbers but break two later phases:
//
//   - Phase 10 (streams): the chain form requires an async-iterator
//     fork on every `.map`; the for-of loop form is sync by default
//     and async by trivial substitution to `for await`.
//   - Phase 16 (reproducible): the chain form's per-fixture token
//     count varies with the optimisation pass, breaking byte-equal
//     reproducibility against a baseline emit.
//
// Tokens checked:
//
//   - `let __query` — the result var is declared mutable (not const)
//     because the inner loop appends. Reproducibility hashes this
//     mutability bit.
//   - `for (const ` — the inner loop is a for-of, not a forEach
//     callback. The for-of form composes with Phase 11 async coloring.
//   - `[...__query` — append uses spread; the AppendExpr functional
//     contract is preserved.
//   - `Map<number, number[]>` — hash-join indexes type as
//     Map<K, T[]>, exercising the Phase 7 widening of
//     tsTypeForMapSlot.
//   - `mochi_list_sort_asc(` — order by routes through the existing
//     Phase 3.1 runtime helper.
//   - `mochi_list_slice(` — skip/take routes through the existing
//     Phase 3.1 runtime helper.
func TestPhase7EmitShape(t *testing.T) {
	fixtureDir := filepath.Join(repoRoot(t), "tests", "transpiler3", "typescript", "fixtures", "phase07-query")
	cases := []struct {
		fixture string
		wants   []string
	}{
		{
			fixture: "req_filter_int.mochi",
			wants: []string{
				"let __query",
				"for (const n of nums)",
				"[...__query",
			},
		},
		{
			fixture: "req_order_asc_int.mochi",
			wants: []string{
				"mochi_list_sort_asc(",
			},
		},
		{
			fixture: "req_skip_take.mochi",
			wants: []string{
				"mochi_list_sort_asc(",
				"mochi_list_slice(",
			},
		},
		{
			fixture: "req_inner_join_int.mochi",
			wants: []string{
				"Map<number, number[]>",
				"__hidx_",
				"new Map<number, number[]>()",
			},
		},
		{
			fixture: "req_left_join.mochi",
			wants: []string{
				"let __any",
				"if (!(__any",
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

// TestPhase7QueryScopeIsTransparent asserts that the QueryScopeStmt
// arena wrapper is a no-op at the TS surface: nothing the C emitter
// uses (mochi_arena_init, __qaN, mochi_arena_free, _arena append
// variants) leaks into the TS source. The JS GC + array growth
// handle the lifetime and amortised-O(1) push that the C arena is
// there for.
func TestPhase7QueryScopeIsTransparent(t *testing.T) {
	fixtureDir := filepath.Join(repoRoot(t), "tests", "transpiler3", "typescript", "fixtures", "phase07-query")
	entries, err := os.ReadDir(fixtureDir)
	if err != nil {
		t.Fatalf("ReadDir: %v", err)
	}
	forbidden := []string{
		"mochi_arena_",
		"__qa",
		"_append_arena",
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
					t.Errorf("%s emit leaked arena token %q\n---\n%s", e.Name(), f, src)
				}
			}
		})
	}
}
