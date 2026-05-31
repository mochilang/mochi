package build

import (
	"os"
	"path/filepath"
	"strings"
	"testing"
)

// TestPhase3_1Lists is the Phase 3.1 primary gate. Every fixture
// under tests/transpiler3/typescript/fixtures/phase03.1-lists must
// lower via the TS transpiler and produce byte-equal stdout on
// Node 22, Deno 2, and Bun 1.1 against the vm3-recorded .out.
//
// The fixture corpus covers seven surface areas: literals across
// the four scalar element types (int / float / bool / string),
// indexed reads (`xs[i]`), len (`len(xs)`), functional append
// (`append(xs, v)`), iteration (`for x in xs`), reductions
// (`sum`, `min`, `max`, `in`), and round-trips through user
// functions (list parameter, list return).
//
// Phase 3.1 ships 25 fixtures; MEP-52 §Phase 3 splits the
// collection surface into four sub-phases and tags this one with
// a 25-fixture target.
func TestPhase3_1ListsNode(t *testing.T) { runPhase3_1FixturesOn(t, "node") }
func TestPhase3_1ListsDeno(t *testing.T) { runPhase3_1FixturesOn(t, "deno") }
func TestPhase3_1ListsBun(t *testing.T)  { runPhase3_1FixturesOn(t, "bun") }

func runPhase3_1FixturesOn(t *testing.T, runtime string) {
	t.Helper()
	fixtureDir := filepath.Join(repoRoot(t), "tests", "transpiler3", "typescript", "fixtures", "phase03.1-lists")
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
		t.Fatalf("Phase 3.1 fixture corpus has %d .mochi files, expected at least 25 per MEP-52 §Phase 3.1", mochiCount)
	}
}

// TestPhase3_1EmitWithoutRuntime confirms the Phase 3.1 list
// surface lowers to syntactically plausible TS without any JS
// runtime present. It picks one representative fixture per surface
// area and asserts the emit includes a load-bearing token.
func TestPhase3_1EmitWithoutRuntime(t *testing.T) {
	fixtureDir := filepath.Join(repoRoot(t), "tests", "transpiler3", "typescript", "fixtures", "phase03.1-lists")
	cases := []struct {
		fixture string
		wants   []string
	}{
		{
			fixture: "list_int_literal.mochi",
			wants: []string{
				"const xs: number[] = [1, 2, 3];",
				"function mochi_list_at<T>(xs: readonly T[], i: number): T",
				"mochi_list_at(xs, 0)",
				"xs.length",
			},
		},
		{
			fixture: "list_string_literal.mochi",
			wants: []string{
				`const xs: string[] = ["alpha", "beta", "gamma"];`,
			},
		},
		{
			fixture: "list_bool_literal.mochi",
			wants: []string{
				"const xs: boolean[] = [true, false, true];",
			},
		},
		{
			fixture: "list_int_append.mochi",
			wants: []string{
				"const ys: number[] = [...xs, 4];",
			},
		},
		{
			fixture: "list_for_each_int.mochi",
			wants: []string{
				"for (const x of xs) {",
			},
		},
		{
			fixture: "list_sum_int.mochi",
			wants: []string{
				"function mochi_list_sum(xs: readonly number[]): number",
				"mochi_list_sum(xs)",
			},
		},
		{
			fixture: "list_min_max.mochi",
			wants: []string{
				"function mochi_list_min(xs: readonly number[]): number",
				"function mochi_list_max(xs: readonly number[]): number",
			},
		},
		{
			fixture: "list_contains_int.mochi",
			wants: []string{
				"xs.includes(3)",
			},
		},
		{
			fixture: "list_passed_to_fn.mochi",
			wants: []string{
				"function mochi__head(xs: number[]): number",
			},
		},
		{
			fixture: "list_returned_from_fn.mochi",
			wants: []string{
				"function mochi__pair(a: number, b: number): number[]",
			},
		},
		{
			fixture: "list_index_assign.mochi",
			wants: []string{
				"let xs: number[] = [1, 2, 3];",
				"xs[1] = 42;",
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

// TestPhase3_1ListAtAlwaysGuarded asserts every fixture that reads
// an indexed list value routes through `mochi_list_at` rather than
// emitting a bare bracket access. The runtime helper bounds-checks
// and raises; bare `xs[i]` under `--noUncheckedIndexedAccess` types
// as `T | undefined` and would force every caller to narrow.
func TestPhase3_1ListAtAlwaysGuarded(t *testing.T) {
	fixtureDir := filepath.Join(repoRoot(t), "tests", "transpiler3", "typescript", "fixtures", "phase03.1-lists")
	entries, err := os.ReadDir(fixtureDir)
	if err != nil {
		t.Fatalf("ReadDir: %v", err)
	}
	for _, e := range entries {
		if !strings.HasSuffix(e.Name(), ".mochi") {
			continue
		}
		// list_index_assign uses xs[i] as the assignment LHS, which
		// is fine; bare brackets on the read side is the regression
		// this test guards. Skip the assignment fixture: the
		// LHS-form bracket is intentional and is wrapped by the
		// emitter as IndexAssignStmt, not as a read.
		name := strings.TrimSuffix(e.Name(), ".mochi")
		t.Run(name, func(t *testing.T) {
			outDir := t.TempDir()
			d := &Driver{CacheDir: t.TempDir(), NoCache: true}
			p, err := d.Build(filepath.Join(fixtureDir, e.Name()), outDir, TargetTypeScriptSource)
			if err != nil {
				t.Fatalf("Build: %v", err)
			}
			src := readTrim(t, p)
			// Strip the helper definition body so we don't flag the
			// helper's internal `xs[i] as T` return.
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
				// In user code, find any `IDENT[` that isn't a
				// known LHS (the LHS bracket is fine; reading
				// through it is the regression).
				// Heuristic: a bare bracket access in an
				// expression position appears with a leading
				// identifier and a trailing `]` not followed by
				// `=` on the same line outside string literals.
				if idx := strings.Index(line, "["); idx > 0 {
					if !isIdentChar(line[idx-1]) {
						continue
					}
					rest := line[idx+1:]
					closeIdx := strings.Index(rest, "]")
					if closeIdx < 0 {
						continue
					}
					afterClose := strings.TrimSpace(rest[closeIdx+1:])
					if strings.HasPrefix(afterClose, "=") && !strings.HasPrefix(afterClose, "===") {
						// LHS assignment, fine.
						continue
					}
					// Reads are not allowed unless this is
					// `xs[i] as T` inside the mochi_list_at
					// helper, already filtered above.
					t.Errorf("%s line %d emits bare bracket read (should route through mochi_list_at): %s", e.Name(), i+1, line)
				}
			}
		})
	}
}

func isIdentChar(c byte) bool {
	return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') || c == '_'
}
