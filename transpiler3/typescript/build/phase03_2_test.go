package build

import (
	"os"
	"path/filepath"
	"strings"
	"testing"
)

// TestPhase3_2Maps is the Phase 3.2 primary gate. Every fixture
// under tests/transpiler3/typescript/fixtures/phase03.2-maps must
// lower via the TS transpiler and produce byte-equal stdout on
// Node 22, Deno 2, and Bun 1.1 against the vm3-recorded .out.
//
// The fixture corpus covers eight surface areas: literals across
// the four scalar value types (int / float / bool / string) and
// two key types (string / int), keyed reads (`m[k]`), membership
// (`k in m`), len (`len(m)`), mutation (`var m; m[k] = v`),
// iteration (`for k in m` lowers via MapKeysExpr; `for v in
// values(m)` lowers via MapValuesExpr), aggregation (sum, count,
// max via values()), and round-trips through user functions
// (map parameter, map return).
//
// Iteration order: the emitter sorts keys by stringified
// comparison so `for k in m` matches vm3's lex-sorted iteration
// order. The TS helpers mochi_map_keys_sorted /
// mochi_map_values_sorted enforce this on the JS side even though
// JS Map preserves insertion order natively. The choice is
// deliberate: byte-equal stdout across vm3 and three JS runtimes
// is the Phase 3.2 gate, not native Map iteration.
//
// Phase 3.2 ships 29 fixtures; MEP-52 §Phase 3.2 sets a
// 25-fixture floor.
func TestPhase3_2MapsNode(t *testing.T) { runPhase3_2FixturesOn(t, "node") }
func TestPhase3_2MapsDeno(t *testing.T) { runPhase3_2FixturesOn(t, "deno") }
func TestPhase3_2MapsBun(t *testing.T)  { runPhase3_2FixturesOn(t, "bun") }

func runPhase3_2FixturesOn(t *testing.T, runtime string) {
	t.Helper()
	fixtureDir := filepath.Join(repoRoot(t), "tests", "transpiler3", "typescript", "fixtures", "phase03.2-maps")
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
		t.Fatalf("Phase 3.2 fixture corpus has %d .mochi files, expected at least 25 per MEP-52 §Phase 3.2", mochiCount)
	}
}

// TestPhase3_2EmitWithoutRuntime confirms the Phase 3.2 map
// surface lowers to syntactically plausible TS without any JS
// runtime present. It picks one representative fixture per surface
// area and asserts the emit includes a load-bearing token.
//
// The emit-shape tests guard against regressions in lowering
// decisions that would still produce byte-equal stdout (a
// shape-blind silent change) but would drift away from the spec's
// emit contract.
func TestPhase3_2EmitWithoutRuntime(t *testing.T) {
	fixtureDir := filepath.Join(repoRoot(t), "tests", "transpiler3", "typescript", "fixtures", "phase03.2-maps")
	cases := []struct {
		fixture string
		wants   []string
	}{
		{
			fixture: "map_int_literal.mochi",
			wants: []string{
				`const m: Map<string, number> = new Map<string, number>([["a", 1], ["b", 2], ["c", 3]]);`,
				"function mochi_map_get<K, V>(m: ReadonlyMap<K, V>, k: K): V",
				`throw new RangeError("mochi_map_get: missing key " + String(k));`,
				`mochi_map_get(m, "a")`,
				"m.size",
			},
		},
		{
			fixture: "map_string_literal.mochi",
			wants: []string{
				`const m: Map<string, string> = new Map<string, string>([["red", "rouge"], ["blue", "azure"]]);`,
			},
		},
		{
			fixture: "map_bool_literal.mochi",
			wants: []string{
				`const m: Map<string, boolean> = new Map<string, boolean>([["on", true], ["off", false]]);`,
			},
		},
		{
			fixture: "map_int_keys.mochi",
			wants: []string{
				`const m: Map<number, string> = new Map<number, string>([[1, "one"], [2, "two"], [3, "three"]]);`,
			},
		},
		{
			fixture: "map_empty.mochi",
			wants: []string{
				"const m: Map<string, number> = new Map<string, number>()",
			},
		},
		{
			fixture: "map_has_present.mochi",
			wants: []string{
				`m.has("a")`,
			},
		},
		{
			fixture: "map_for_keys_string.mochi",
			wants: []string{
				"function mochi_map_keys_sorted<K, V>(m: ReadonlyMap<K, V>): K[]",
				"for (const k of mochi_map_keys_sorted(m)) {",
				`return sa < sb ? -1 : sa > sb ? 1 : 0;`,
			},
		},
		{
			fixture: "map_for_keys_sum.mochi",
			wants: []string{
				"function mochi_map_values_sorted<K, V>(m: ReadonlyMap<K, V>): V[]",
				"for (const v of mochi_map_values_sorted(m)) {",
			},
		},
		{
			fixture: "map_put_new.mochi",
			wants: []string{
				`let m: Map<string, number> = new Map<string, number>([["a", 1]]);`,
				`m.set("b", 2);`,
				`m.set("c", 3);`,
			},
		},
		{
			fixture: "map_put_int_keys.mochi",
			wants: []string{
				`let m: Map<number, number> = new Map<number, number>([[1, 10]]);`,
				`m.set(2, 20);`,
				`m.set(3, 30);`,
			},
		},
		{
			fixture: "map_passed_to_fn.mochi",
			wants: []string{
				"function mochi__total(m: Map<string, number>): number",
			},
		},
		{
			fixture: "map_returned_from_fn.mochi",
			wants: []string{
				"function mochi__pair(a: string, b: string): Map<string, number>",
			},
		},
		{
			fixture: "map_len_int.mochi",
			wants: []string{
				"m.size",
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

// TestPhase3_2MapGetAlwaysGuarded asserts every fixture that
// reads a value from a map routes through `mochi_map_get` rather
// than emitting a bare `m.get(k)` call. The runtime helper raises
// on miss; bare `m.get(k)` under `--strict` types as `V | undefined`
// and would force every caller to narrow. The same regression
// guard exists for list reads (mochi_list_at) in
// TestPhase3_1ListAtAlwaysGuarded.
func TestPhase3_2MapGetAlwaysGuarded(t *testing.T) {
	fixtureDir := filepath.Join(repoRoot(t), "tests", "transpiler3", "typescript", "fixtures", "phase03.2-maps")
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
				// User code must not call `.get(` directly on a Map.
				// `m.has(` and `m.set(` are fine; `m.size` is a property.
				if strings.Contains(line, ".get(") {
					t.Errorf("%s line %d emits bare Map.get (should route through mochi_map_get): %s", e.Name(), i+1, line)
				}
			}
		})
	}
}

// TestPhase3_2KeyIterIsSorted asserts every emit that iterates
// keys or values uses the sorted-by-String helper rather than
// raw `m.keys()` / `m.values()`. JS Maps preserve insertion order,
// which would diverge from vm3's lex-sorted iteration for the
// for-each fixtures and break the byte-equal gate.
func TestPhase3_2KeyIterIsSorted(t *testing.T) {
	fixtureDir := filepath.Join(repoRoot(t), "tests", "transpiler3", "typescript", "fixtures", "phase03.2-maps")
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
				if strings.Contains(line, "m.keys()") || strings.Contains(line, "m.values()") {
					t.Errorf("%s line %d emits raw m.keys()/m.values() (should use mochi_map_keys_sorted/values_sorted): %s", e.Name(), i+1, line)
				}
			}
		})
	}
}
