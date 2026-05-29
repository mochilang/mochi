package build

import (
	"os"
	"path/filepath"
	"strings"
	"testing"
)

// TestPhase4_2RecordEquality is the Phase 4.2 primary gate. Every
// fixture under tests/transpiler3/typescript/fixtures/phase04.2-record-equality
// must lower via the TS transpiler and produce byte-equal stdout on
// Node 22, Deno 2, and Bun 1.1 against the vm3-recorded .out.
//
// Phase 4.2 ships whole-record `==` / `!=`. The lowering contract is:
//
//   - aotir BinEqRec / BinNeRec lower to a generated per-record
//     helper `mochi_eq_<R>(a, b): boolean` that ANDs scalar-field
//     comparisons. `!=` wraps the call in a unary `!` rather than
//     emitting a parallel `mochi_ne_<R>` helper.
//   - The helper is emitted exactly once per record name in the
//     program, in `prog.Records` source order, regardless of how
//     many `==` sites reference it. Records the program never
//     compares for equality don't pay for an unused helper.
//   - String fields compare with TS `===` (byte-wise on JS strings
//     per the spec), int and float fields with `===` (IEEE on
//     floats matches vm3's semantics for NaN-free fixtures; the
//     bigint specialisation lands with the same sub-phase that
//     ships bigint int).
//
// Why this gate matters. Phase 7's query DSL `where` clauses run
// `==` between two record values constantly (the join predicate);
// without 4.2 the TS transpiler errors out at `unsupported binary
// op 26 (Phase 2)` for any program that compares records.
//
// The fixture corpus covers nine surface areas:
//
//   - Whole-record equality in an `if` condition.
//   - Whole-record equality used as the argument of `print()` so
//     the boolean output is visible.
//   - String / int / float / bool field types.
//   - Multi-field records (3+ fields).
//   - Two and three record types in the same program (the helper
//     emit walks prog.Records and filters; the order has to stay
//     byte-stable across runs).
//   - `==` and `!=` against self (always true / always false).
//   - `==` combined with `&&` and `||` chains.
//   - `==` in a `for ... in` loop counting matches.
//   - `==` in a `while` loop with an indexed binding.
//   - `==` as the return value of a user function.
//   - `==` assigned to a `var bool` then reused.
//
// Phase 4.2 ships 28 fixtures; the floor is 20.
func TestPhase4_2RecordEqualityNode(t *testing.T) { runPhase4_2FixturesOn(t, "node") }
func TestPhase4_2RecordEqualityDeno(t *testing.T) { runPhase4_2FixturesOn(t, "deno") }
func TestPhase4_2RecordEqualityBun(t *testing.T)  { runPhase4_2FixturesOn(t, "bun") }

func runPhase4_2FixturesOn(t *testing.T, runtime string) {
	t.Helper()
	fixtureDir := filepath.Join(repoRoot(t), "tests", "transpiler3", "typescript", "fixtures", "phase04.2-record-equality")
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
		t.Fatalf("Phase 4.2 fixture corpus has %d .mochi files, expected at least 20", mochiCount)
	}
}

// TestPhase4_2EmitWithoutRuntime confirms the Phase 4.2 record
// equality surface lowers to syntactically plausible TS without
// any JS runtime present. It picks one representative fixture per
// surface area and asserts the emit includes the load-bearing
// tokens that distinguish the spec's lowering from a wrong but
// byte-equal alternative.
//
// The most important invariants here:
//
//   - A `mochi_eq_<R>` helper is emitted with the right signature
//     (`function mochi_eq_<R>(a: <R>, b: <R>): boolean`).
//   - The helper body reads `return (a.f1 === b.f1) && (a.f2 === b.f2);`
//     (parenthesised per-field, AND-joined).
//   - `==` sites lower to `mochi_eq_<R>(a, b)` (not `===` between
//     class instances, which would be reference equality and
//     silently wrong).
//   - `!=` sites lower to `!mochi_eq_<R>(a, b)` (no `mochi_ne_<R>`
//     helper).
//   - A program with two record types where only one is compared
//     emits only one helper.
func TestPhase4_2EmitWithoutRuntime(t *testing.T) {
	fixtureDir := filepath.Join(repoRoot(t), "tests", "transpiler3", "typescript", "fixtures", "phase04.2-record-equality")
	cases := []struct {
		fixture string
		wants   []string
	}{
		{
			fixture: "req_basic_eq.mochi",
			wants: []string{
				"function mochi_eq_Pt(a: Pt, b: Pt): boolean",
				"return (a.x === b.x) && (a.y === b.y);",
				"mochi_eq_Pt(p1, p2)",
			},
		},
		{
			fixture: "req_string_field_eq.mochi",
			wants: []string{
				"function mochi_eq_User(a: User, b: User): boolean",
				"return (a.id === b.id) && (a.name === b.name);",
				"mochi_eq_User(u1, u2)",
			},
		},
		{
			fixture: "req_print_ne_true.mochi",
			wants: []string{
				"function mochi_eq_Pt(a: Pt, b: Pt): boolean",
				"!(mochi_eq_Pt(a, b))",
			},
		},
		{
			fixture: "req_multi_field_all_same.mochi",
			wants: []string{
				"function mochi_eq_Quad(a: Quad, b: Quad): boolean",
				"return (a.a === b.a) && (a.b === b.b) && (a.c === b.c) && (a.d === b.d);",
			},
		},
		{
			fixture: "req_two_records.mochi",
			wants: []string{
				"function mochi_eq_A(a: A, b: A): boolean",
				"function mochi_eq_B(a: B, b: B): boolean",
				"return (a.v === b.v);",
				"return (a.s === b.s);",
			},
		},
		{
			fixture: "req_eq_unused_record.mochi",
			wants: []string{
				"function mochi_eq_A(a: A, b: A): boolean",
				"mochi_eq_A(a1, a2)",
			},
		},
		{
			fixture: "req_eq_fn_return.mochi",
			wants: []string{
				"function mochi__same(a: Pt, b: Pt): boolean",
				"return mochi_eq_Pt(a, b);",
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

// TestPhase4_2UnusedRecordNoHelper asserts the helper-emission rule
// stays opt-in. A program that declares a record but never compares
// two values of it with `==` or `!=` must NOT emit a
// `mochi_eq_<R>` helper. The bundle size cost of a dead helper is
// the immediate price; the deeper risk is that future minifiers
// keep the helper alive because the class is alive, defeating the
// "pay for what you use" property.
func TestPhase4_2UnusedRecordNoHelper(t *testing.T) {
	fixtureDir := filepath.Join(repoRoot(t), "tests", "transpiler3", "typescript", "fixtures", "phase04.2-record-equality")
	outDir := t.TempDir()
	d := &Driver{CacheDir: t.TempDir(), NoCache: true}
	p, err := d.Build(filepath.Join(fixtureDir, "req_eq_unused_record.mochi"), outDir, TargetTypeScriptSource)
	if err != nil {
		t.Fatalf("Build: %v", err)
	}
	src := readTrim(t, p)
	if strings.Contains(src, "function mochi_eq_B(") {
		t.Errorf("emit contains mochi_eq_B helper for an unused record; B is never compared for equality\n---\n%s", src)
	}
	if !strings.Contains(src, "function mochi_eq_A(") {
		t.Errorf("emit missing mochi_eq_A helper for the record actually compared\n---\n%s", src)
	}
}

// TestPhase4_2HelperEmittedOnce asserts the helper for a given
// record name appears exactly once in the emit, even when the
// program contains many `==` and `!=` sites referencing it. A
// regression to per-call-site emission would inflate the bundle
// linearly with comparison count and break Phase 16 byte-equal
// reproducibility (two builds may visit call sites in a different
// order under future scheduler changes).
func TestPhase4_2HelperEmittedOnce(t *testing.T) {
	fixtureDir := filepath.Join(repoRoot(t), "tests", "transpiler3", "typescript", "fixtures", "phase04.2-record-equality")
	cases := []string{
		"req_multi_field_one_diff.mochi", // four == / != sites against the same record
		"req_count_eq_loop.mochi",        // loop with one == site
		"req_eq_combined.mochi",          // == inside an iteration
	}
	for _, fx := range cases {
		t.Run(strings.TrimSuffix(fx, ".mochi"), func(t *testing.T) {
			outDir := t.TempDir()
			d := &Driver{CacheDir: t.TempDir(), NoCache: true}
			p, err := d.Build(filepath.Join(fixtureDir, fx), outDir, TargetTypeScriptSource)
			if err != nil {
				t.Fatalf("Build: %v", err)
			}
			src := readTrim(t, p)
			// Per fixture, count occurrences of `function mochi_eq_*(` (the
			// declaration). We expect exactly one per record name compared,
			// regardless of how many `==` / `!=` call sites reference it.
			helpers := 0
			for _, line := range strings.Split(src, "\n") {
				if strings.HasPrefix(strings.TrimSpace(line), "function mochi_eq_") {
					helpers++
				}
			}
			if helpers != 1 {
				t.Errorf("%s: expected exactly one mochi_eq_* helper declaration, found %d\n---\n%s", fx, helpers, src)
			}
		})
	}
}

// TestPhase4_2HelperOrder asserts the helpers are emitted in
// prog.Records source order, not call-site order. Byte-stable
// helper order is the Phase 16 reproducibility contract: two
// builds that visit `==` sites in different orders (e.g. under a
// future parallel lowerer) must still produce the same file.
func TestPhase4_2HelperOrder(t *testing.T) {
	fixtureDir := filepath.Join(repoRoot(t), "tests", "transpiler3", "typescript", "fixtures", "phase04.2-record-equality")
	outDir := t.TempDir()
	d := &Driver{CacheDir: t.TempDir(), NoCache: true}
	p, err := d.Build(filepath.Join(fixtureDir, "req_three_records.mochi"), outDir, TargetTypeScriptSource)
	if err != nil {
		t.Fatalf("Build: %v", err)
	}
	src := readTrim(t, p)
	idxA := strings.Index(src, "function mochi_eq_A(")
	idxB := strings.Index(src, "function mochi_eq_B(")
	idxC := strings.Index(src, "function mochi_eq_C(")
	if idxA < 0 || idxB < 0 || idxC < 0 {
		t.Fatalf("missing one of mochi_eq_{A,B,C}: A=%d B=%d C=%d\n---\n%s", idxA, idxB, idxC, src)
	}
	if !(idxA < idxB && idxB < idxC) {
		t.Errorf("helper order wrong: want A before B before C; got A@%d B@%d C@%d\n---\n%s",
			idxA, idxB, idxC, src)
	}
}
