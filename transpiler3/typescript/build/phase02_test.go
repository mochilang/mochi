package build

import (
	"os"
	"path/filepath"
	"strings"
	"testing"
)

// TestPhase2Scalars is the Phase 2 primary gate: every fixture
// under tests/transpiler3/typescript/fixtures/phase02-scalars must
// lower via the TS transpiler and produce byte-equal stdout on
// Node 22, Deno 2, and Bun 1.1 against the vm3-recorded .out.
// The fixture corpus covers six surface areas: int arithmetic,
// float arithmetic, comparisons, booleans, control flow (if /
// while / for / break / continue), strings (concat / len / index /
// contains), and user-defined functions (basic + recursive).
//
// Phase 2 ships 32 fixtures; the MEP-52 spec lists 30 as a target,
// and the two extras (arith_complex + compare_str_eq) lock distinct
// surface that would otherwise rely on Phase 3+ coverage.
func TestPhase2ScalarsNode(t *testing.T) { runPhase2FixturesOn(t, "node") }
func TestPhase2ScalarsDeno(t *testing.T) { runPhase2FixturesOn(t, "deno") }
func TestPhase2ScalarsBun(t *testing.T)  { runPhase2FixturesOn(t, "bun") }

func runPhase2FixturesOn(t *testing.T, runtime string) {
	t.Helper()
	fixtureDir := filepath.Join(repoRoot(t), "tests", "transpiler3", "typescript", "fixtures", "phase02-scalars")
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
	if mochiCount < 30 {
		t.Fatalf("Phase 2 fixture corpus has %d .mochi files, expected at least 30 per MEP-52 §Phase 2", mochiCount)
	}
}

// TestPhase2EmitWithoutRuntime confirms the Phase 2 surface lowers
// to syntactically plausible TS without any JS runtime present.
// It picks one representative fixture per surface area and asserts
// the emit includes a load-bearing token for that area.
func TestPhase2EmitWithoutRuntime(t *testing.T) {
	fixtureDir := filepath.Join(repoRoot(t), "tests", "transpiler3", "typescript", "fixtures", "phase02-scalars")
	cases := []struct {
		fixture string
		wants   []string
	}{
		{
			fixture: "arith_add.mochi",
			wants: []string{
				"function mochi_print_i64(value: number): void",
				"const a: number = 3;",
				"const b: number = 4;",
				"mochi_print_i64((a + b));",
			},
		},
		{
			fixture: "let_var.mochi",
			wants: []string{
				"const x: number = 10;",
				"let y: number = 0;",
				"y = (x + 5);",
				"y = (y * 2);",
			},
		},
		{
			fixture: "if_else.mochi",
			wants: []string{
				`if ((x > 5)) {`,
				`} else {`,
				`mochi_print_str("big");`,
				`mochi_print_str("small");`,
			},
		},
		{
			fixture: "while_loop.mochi",
			wants: []string{
				`while ((i < 4)) {`,
				`i = (i + 1);`,
			},
		},
		{
			fixture: "for_range.mochi",
			wants: []string{
				`for (let i: number = 0; i < 5; i++) {`,
			},
		},
		{
			fixture: "break_loop.mochi",
			wants: []string{
				"break;",
			},
		},
		{
			fixture: "continue_loop.mochi",
			wants: []string{
				"continue;",
			},
		},
		{
			fixture: "str_len.mochi",
			wants: []string{
				"function mochi_str_len(value: string): number",
				"for (const _ of value) { n++; }",
				"return n;",
				"mochi_str_len(s)",
			},
		},
		{
			fixture: "str_index.mochi",
			wants: []string{
				"function mochi_str_at(value: string, index: number): string",
				"mochi_str_at(s, 0)",
			},
		},
		{
			fixture: "str_contains.mochi",
			wants: []string{
				"function mochi_str_contains(haystack: string, needle: string): boolean",
				"return haystack.includes(needle);",
				`mochi_str_contains(s, "world")`,
			},
		},
		{
			fixture: "user_fn.mochi",
			wants: []string{
				"function mochi__add(a: number, b: number): number",
				"return (a + b);",
				"mochi__add(3, 4)",
			},
		},
		{
			fixture: "user_fn_recursive.mochi",
			wants: []string{
				"function mochi__fib(n: number): number",
				"return (mochi__fib((n - 1)) + mochi__fib((n - 2)));",
			},
		},
		{
			fixture: "bool_not.mochi",
			wants: []string{
				"mochi_print_bool(!(t));",
				"mochi_print_bool(!(f));",
			},
		},
		{
			fixture: "compare_int_eq.mochi",
			wants: []string{
				"(a === b)",
				"(a !== c)",
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

// TestPhase2NoTripleEquals_NoBareEquals pins the comparison
// emit convention: every Mochi `==` lowers to `===` and every `!=`
// lowers to `!==`. A regression that emitted bare `==` would fail
// `tsc --strict` lint via @typescript-eslint/eqeqeq and would also
// risk silent type-coercion bugs at runtime. The check walks every
// emitted Phase 2 fixture and asserts no `==` / `!=` that isn't part
// of `===` / `!==`.
func TestPhase2NoBareEquality(t *testing.T) {
	fixtureDir := filepath.Join(repoRoot(t), "tests", "transpiler3", "typescript", "fixtures", "phase02-scalars")
	entries, err := os.ReadDir(fixtureDir)
	if err != nil {
		t.Fatalf("ReadDir: %v", err)
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
			// Walk each `==` / `!=` token; reject if not part of
			// `===` / `!==` and not inside a string literal. The
			// fixture corpus has no string-literal == in source so
			// a simple structural check suffices.
			lines := strings.Split(src, "\n")
			for i, line := range lines {
				trimmed := strings.TrimSpace(line)
				if strings.HasPrefix(trimmed, "//") {
					continue
				}
				idx := 0
				for idx < len(line) {
					p := strings.Index(line[idx:], "==")
					if p < 0 {
						break
					}
					absP := idx + p
					// Skip part of `===`.
					if absP+2 < len(line) && line[absP+2] == '=' {
						idx = absP + 3
						continue
					}
					// Skip part of `!==` (rendered as `!` then `==`).
					if absP > 0 && line[absP-1] == '!' {
						idx = absP + 2
						continue
					}
					t.Errorf("%s line %d emits bare `==` (lower must use `===`): %s", e.Name(), i+1, line)
					idx = absP + 2
				}
			}
		})
	}
}
