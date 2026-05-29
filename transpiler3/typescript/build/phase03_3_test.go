package build

import (
	"os"
	"path/filepath"
	"strings"
	"testing"
)

// TestPhase3_3Sets is the Phase 3.3 primary gate. Every fixture
// under tests/transpiler3/typescript/fixtures/phase03.3-sets must
// lower via the TS transpiler and produce byte-equal stdout on
// Node 22, Deno 2, and Bun 1.1 against the hand-validated .out.
//
// The fixture corpus covers seven surface areas: literals across
// four scalar element types (int / float / bool / string),
// duplicates (literal-time dedupe), membership (`has(s, x)` and
// the `x in s` infix), len (`len(s)`), mutation (`s = add(s, x)`),
// iteration (`for x in s` lowers via SetToListExpr through a
// sorted-by-String helper), and round-trips through user functions
// (set parameter, set return).
//
// Iteration order: the emitter routes for-in through
// `mochi_set_to_list_sorted` which sorts by `String(x)` ascending.
// vm3's set support is broken (`has` returns "nil" and `len` 0), so
// the byte-equal target is the hand-validated .out, not a vm3 oracle.
// The sort decision keeps stdout deterministic across Node 22,
// Deno 2, and Bun 1.1 even though JS Set iteration is
// insertion-ordered (the three runtimes agree on that, but the
// sort is the Mochi-level contract: iteration order is unspecified).
//
// Phase 3.3 ships 33 fixtures; MEP-52 §Phase 3.3 sets a
// 25-fixture floor.
func TestPhase3_3SetsNode(t *testing.T) { runPhase3_3FixturesOn(t, "node") }
func TestPhase3_3SetsDeno(t *testing.T) { runPhase3_3FixturesOn(t, "deno") }
func TestPhase3_3SetsBun(t *testing.T)  { runPhase3_3FixturesOn(t, "bun") }

func runPhase3_3FixturesOn(t *testing.T, runtime string) {
	t.Helper()
	fixtureDir := filepath.Join(repoRoot(t), "tests", "transpiler3", "typescript", "fixtures", "phase03.3-sets")
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
	if mochiCount < 25 {
		t.Fatalf("Phase 3.3 fixture corpus has %d .mochi files, expected at least 25 per MEP-52 §Phase 3.3", mochiCount)
	}
}

// TestPhase3_3EmitWithoutRuntime confirms the Phase 3.3 set
// surface lowers to syntactically plausible TS without any JS
// runtime present. It picks one representative fixture per surface
// area and asserts the emit includes a load-bearing token.
//
// The emit-shape tests guard against regressions in lowering
// decisions that would still produce byte-equal stdout (a
// shape-blind silent change) but would drift away from the spec's
// emit contract.
func TestPhase3_3EmitWithoutRuntime(t *testing.T) {
	fixtureDir := filepath.Join(repoRoot(t), "tests", "transpiler3", "typescript", "fixtures", "phase03.3-sets")
	cases := []struct {
		fixture string
		wants   []string
	}{
		{
			fixture: "set_basic.mochi",
			wants: []string{
				`const s: Set<number> = new Set<number>([1, 2, 3]);`,
				"s.has(1)",
				"s.has(4)",
			},
		},
		{
			fixture: "set_string_literal.mochi",
			wants: []string{
				`const s: Set<string> = new Set<string>(["hello", "world", "mochi"]);`,
			},
		},
		{
			fixture: "set_bool_literal.mochi",
			wants: []string{
				`const s: Set<boolean> = new Set<boolean>([true, false]);`,
			},
		},
		{
			fixture: "set_float_literal.mochi",
			wants: []string{
				`const s: Set<number> = new Set<number>([1.5, 2.5, 3.5]);`,
			},
		},
		{
			fixture: "set_len_int.mochi",
			wants: []string{
				"s.size",
			},
		},
		{
			fixture: "set_add_int.mochi",
			wants: []string{
				`let s: Set<number> = new Set<number>([1, 2]);`,
				"new Set<number>([...s, 3])",
			},
		},
		{
			fixture: "set_for_in_int.mochi",
			wants: []string{
				"function mochi_set_to_list_sorted<T>(s: ReadonlySet<T>): T[]",
				"for (const x of mochi_set_to_list_sorted(s)) {",
				`return sa < sb ? -1 : sa > sb ? 1 : 0;`,
			},
		},
		{
			fixture: "set_for_in_string.mochi",
			wants: []string{
				"for (const x of mochi_set_to_list_sorted(s)) {",
			},
		},
		{
			fixture: "set_in_operator.mochi",
			wants: []string{
				"s.has(2)",
				"s.has(3)",
			},
		},
		{
			fixture: "set_grow_loop.mochi",
			wants: []string{
				`let s: Set<number> = new Set<number>([0]);`,
				"new Set<number>([...s, i])",
			},
		},
		{
			fixture: "set_dedupe_list.mochi",
			wants: []string{
				"new Set<number>([...s, x])",
				"for (const x of xs) {",
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

// TestPhase3_3SetIterIsSorted asserts every emit that iterates a
// set routes through `mochi_set_to_list_sorted` rather than `for
// (const x of s)` directly. JS Set iteration preserves insertion
// order; Mochi's set iteration order is unspecified, so emitting
// the sort helper is the agreed contract. A raw for-of over the
// set value would still match insertion order on every runtime
// today, but the contract is sort-by-String to stay stable if a
// future runtime changes iteration order.
func TestPhase3_3SetIterIsSorted(t *testing.T) {
	fixtureDir := filepath.Join(repoRoot(t), "tests", "transpiler3", "typescript", "fixtures", "phase03.3-sets")
	entries, err := os.ReadDir(fixtureDir)
	if err != nil {
		t.Fatalf("ReadDir: %v", err)
	}
	for _, e := range entries {
		if !strings.HasSuffix(e.Name(), ".mochi") {
			continue
		}
		name := strings.TrimSuffix(e.Name(), ".mochi")
		t.Run(name, func(t *testing.T) {
			outDir := t.TempDir()
			d := &Driver{CacheDir: t.TempDir(), NoCache: true}
			p, err := d.Build(filepath.Join(fixtureDir, e.Name()), outDir, TargetTypeScriptSource)
			if err != nil {
				t.Fatalf("Build: %v", err)
			}
			src := readTrim(t, p)
			lines := strings.Split(src, "\n")
			inHelper := false
			braceDepth := 0
			for i, line := range lines {
				trimmed := strings.TrimSpace(line)
				if strings.HasPrefix(trimmed, "//") {
					continue
				}
				if strings.Contains(line, "function mochi_") {
					inHelper = true
					braceDepth = strings.Count(line, "{") - strings.Count(line, "}")
					continue
				}
				if inHelper {
					braceDepth += strings.Count(line, "{") - strings.Count(line, "}")
					if braceDepth <= 0 {
						inHelper = false
					}
					continue
				}
				// User code must not iterate Set directly.
				// for-of on a bare `Set<T>` (e.g. `for (const x of s)`)
				// would yield insertion order, not the sort-by-String
				// contract. Iteration must route through the helper.
				if strings.Contains(line, "for (const ") && strings.Contains(line, " of s)") {
					t.Errorf("%s line %d emits raw `for of s` (should use mochi_set_to_list_sorted): %s", e.Name(), i+1, line)
				}
			}
		})
	}
}

// TestPhase3_3SetAddIsFunctional asserts every fixture's `add(s, x)`
// lowers to a fresh `new Set<T>([...s, x])` rather than a bare
// `s.add(x)`. Mochi `add` is functional (the aotir verifier rejects
// in-place mutation of the receiver); the TS emit must mirror that
// so re-assignments stay coherent and shared references aren't
// aliased.
func TestPhase3_3SetAddIsFunctional(t *testing.T) {
	fixtureDir := filepath.Join(repoRoot(t), "tests", "transpiler3", "typescript", "fixtures", "phase03.3-sets")
	entries, err := os.ReadDir(fixtureDir)
	if err != nil {
		t.Fatalf("ReadDir: %v", err)
	}
	for _, e := range entries {
		if !strings.HasSuffix(e.Name(), ".mochi") {
			continue
		}
		name := strings.TrimSuffix(e.Name(), ".mochi")
		t.Run(name, func(t *testing.T) {
			outDir := t.TempDir()
			d := &Driver{CacheDir: t.TempDir(), NoCache: true}
			p, err := d.Build(filepath.Join(fixtureDir, e.Name()), outDir, TargetTypeScriptSource)
			if err != nil {
				t.Fatalf("Build: %v", err)
			}
			src := readTrim(t, p)
			lines := strings.Split(src, "\n")
			inHelper := false
			braceDepth := 0
			for i, line := range lines {
				trimmed := strings.TrimSpace(line)
				if strings.HasPrefix(trimmed, "//") {
					continue
				}
				if strings.Contains(line, "function mochi_") {
					inHelper = true
					braceDepth = strings.Count(line, "{") - strings.Count(line, "}")
					continue
				}
				if inHelper {
					braceDepth += strings.Count(line, "{") - strings.Count(line, "}")
					if braceDepth <= 0 {
						inHelper = false
					}
					continue
				}
				// `Set.prototype.add` mutates and returns the set;
				// Mochi `add` is functional, so user code must not
				// call `.add(` directly. `.has(` and `.size` stay fine.
				if strings.Contains(line, ".add(") {
					t.Errorf("%s line %d emits bare Set.add (should use new Set([...s, x])): %s", e.Name(), i+1, line)
				}
			}
		})
	}
}
