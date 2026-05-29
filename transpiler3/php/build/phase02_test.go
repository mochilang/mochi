package build

import (
	"os"
	"path/filepath"
	"strings"
	"testing"
)

// TestPhase2Scalars walks every .mochi file under the Phase 2 fixture
// directory, lowers it to PHP, then runs the emitted main.php and diffs
// stdout against the matching .out file. Tests skip cleanly when PHP is
// not on PATH; CI uses shivammathur/setup-php@v2 to exercise the full
// pipeline.
func TestPhase2Scalars(t *testing.T) {
	fixtureDir := filepath.Join(repoRoot(t), "tests", "transpiler3", "php", "fixtures", "phase02-scalars")
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

// TestPhase2EmitFragments asserts the lowerer produces the expected PHP
// shape for one representative fixture per Phase 2 feature: this catches
// regressions in node-level lowering without needing PHP installed.
func TestPhase2EmitFragments(t *testing.T) {
	cases := []struct {
		fixture string
		wants   []string
	}{
		{
			fixture: "let_var.mochi",
			wants: []string{
				`$x = 42;`,
				`$y = 0;`,
				`$y = ($x + 1);`,
				`mochi_print_i64($y);`,
			},
		},
		{
			fixture: "arith_div.mochi",
			wants: []string{
				`intdiv($a, $b)`,
			},
		},
		{
			fixture: "arith_float.mochi",
			wants: []string{
				`function mochi_print_f64(float $value): void`,
				`mochi_print_f64(($x + $y));`,
			},
		},
		{
			fixture: "bool_ops.mochi",
			wants: []string{
				`(true && false)`,
				`(true || false)`,
				`(!true)`,
			},
		},
		{
			fixture: "break_continue.mochi",
			wants: []string{
				`while (($i < 10)) {`,
				`continue;`,
				`break;`,
			},
		},
		{
			fixture: "compare_int.mochi",
			wants: []string{
				`($a > $b)`,
				`($a === $b)`,
			},
		},
		{
			fixture: "compare_str.mochi",
			wants: []string{
				`($s === $t)`,
			},
		},
		{
			// Float equality must lower to PHP's strict `===` (same as
			// int/string), not the loose `==` that performs numeric
			// coercion on string/null/bool operands. The only other
			// float-touching fragment cases (arith_float, float_nan_inf,
			// float_neg_inf) exercise arithmetic and printing; without
			// this entry, a regression that swapped `===` for `==` in
			// the float-compare path would only fail under end-to-end
			// PHP runs and silently pass on hosts without `php`.
			fixture: "compare_float.mochi",
			wants: []string{
				`($a < $b)`,
				`($a === $b)`,
				`($b > $a)`,
			},
		},
		{
			fixture: "float_nan_inf.mochi",
			wants: []string{
				`fdiv(1, 0)`,
				`fdiv(0, 0)`,
				`function mochi_print_f64(float $value): void`,
			},
		},
		{
			fixture: "float_neg_inf.mochi",
			wants: []string{
				// `-1.0 / 0.0` lowers to fdiv with a parenthesised
				// negative numerator. The helper's is_infinite branch
				// picks the sign based on the value, so the printed
				// token is "-Inf" rather than "+Inf".
				`fdiv((-1), 0)`,
				`if (is_infinite($value)) { echo $value < 0 ? "-Inf\n" : "+Inf\n"; return; }`,
			},
		},
		{
			fixture: "for_range.mochi",
			wants: []string{
				`for ($i = 0; $i < 5; $i++) {`,
			},
		},
		{
			fixture: "if_else.mochi",
			wants: []string{
				`if (($x > 5)) {`,
				`} else {`,
				`mochi_print_str("big");`,
				`mochi_print_str("small");`,
			},
		},
		{
			fixture: "int_cast.mochi",
			wants: []string{
				`(int) $x`,
			},
		},
		{
			fixture: "str_cat.mochi",
			wants: []string{
				`($a . $b)`,
			},
		},
		{
			fixture: "str_contains.mochi",
			wants: []string{
				`function mochi_str_contains(string $haystack, string $needle): bool`,
				`mochi_str_contains($s, "world")`,
			},
		},
		{
			fixture: "str_contains_empty.mochi",
			wants: []string{
				// Empty-needle short-circuit lives in the helper:
				// `$needle === "" || str_contains(...)` returns true
				// without consulting PHP's str_contains. Pinning this
				// shape catches a regression that dropped the OR.
				`return $needle === "" || str_contains($haystack, $needle);`,
				`mochi_str_contains($s, "")`,
				`mochi_str_contains($empty, "")`,
			},
		},
		{
			fixture: "str_index.mochi",
			wants: []string{
				`substr($s, 1, 1)`,
			},
		},
		{
			fixture: "str_len.mochi",
			wants: []string{
				`strlen($s)`,
			},
		},
		{
			fixture: "user_fn.mochi",
			wants: []string{
				// aotir mangles user function names with a `mochi__`
				// prefix so they cannot collide with the inline
				// runtime helpers in the same global namespace.
				`function mochi__add(int $a, int $b): int`,
				`return ($a + $b);`,
				`mochi_print_i64(mochi__add(3, 4));`,
			},
		},
		{
			fixture: "user_fn_recursive.mochi",
			wants: []string{
				`function mochi__fib(int $n): int`,
				`(mochi__fib(($n - 1)) + mochi__fib(($n - 2)))`,
			},
		},
		{
			fixture: "while_loop.mochi",
			wants: []string{
				`while (($i < 3)) {`,
				`$i = ($i + 1);`,
			},
		},
	}

	for _, c := range cases {
		t.Run(strings.TrimSuffix(c.fixture, ".mochi"), func(t *testing.T) {
			mochiPath := filepath.Join(repoRoot(t), "tests", "transpiler3", "php", "fixtures", "phase02-scalars", c.fixture)
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
