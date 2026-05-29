package build

import (
	"os"
	"path/filepath"
	"strings"
	"testing"
)

// TestPhase6Closures walks the Phase 6 closure/HOF fixtures and
// exercises each through the full PHP transpiler. Tests skip when
// PHP is not installed; CI uses shivammathur/setup-php@v2 to drive
// the end-to-end gate.
func TestPhase6Closures(t *testing.T) {
	fixtureDir := filepath.Join(repoRoot(t), "tests", "transpiler3", "php", "fixtures", "phase06-closures")
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

// TestPhase6EmitFragments asserts the lowerer emits the expected PHP
// shape for the four Phase 6 feature axes: bare lambda binding,
// lambda-as-argument, capturing closure (env threaded as leading
// param), and the three list HOFs (map / filter / reduce) backed by
// PHP's array_map / array_values+array_filter / array_reduce.
func TestPhase6EmitFragments(t *testing.T) {
	cases := []struct {
		fixture string
		wants   []string
	}{
		{
			fixture: "lambda_basic.mochi",
			wants: []string{
				`function __anon_1(int $x): int`,
				`return ($x * $x);`,
				// FunLit lowers to an arrow function that forwards
				// its typed args to the lifted callee.
				`$square = fn(int $__p0): int => __anon_1($__p0);`,
				`$square(6)`,
			},
		},
		{
			fixture: "lambda_as_arg.mochi",
			wants: []string{
				// Function-typed parameter renders as `Closure` (the
				// global PHP class) so PHP 8.4 accepts only true
				// callables built via fn(...) / function(...) {...}.
				`function mochi__apply(Closure $f, int $x): int`,
				`return $f($x);`,
				`$dbl = fn(int $__p0): int => __anon_1($__p0);`,
				`mochi__apply($dbl, 6)`,
			},
		},
		{
			fixture: "closure_capture.mochi",
			wants: []string{
				// Capturing lifted function gets the capture prepended
				// as its leading typed parameter.
				`function __anon_1(int $n, int $x): int`,
				`return ($x + $n);`,
				// makeAdder returns a Closure that auto-captures $n
				// from its outer scope (PHP arrow-function semantics)
				// and threads it into the lifted callee as the first
				// arg.
				`function mochi__makeAdder(int $n): Closure`,
				`return fn(int $__p0): int => __anon_1($n, $__p0);`,
				`$add10 = mochi__makeAdder(10);`,
				`$add10(5)`,
			},
		},
		{
			fixture: "hof_map.mochi",
			wants: []string{
				// PHP's array_map argument order is (callable, array)
				// which is the reverse of Mochi's map(xs, fn).
				`array_map(fn(int $__p0): int => __anon_1($__p0), $nums)`,
			},
		},
		{
			fixture: "hof_filter.mochi",
			wants: []string{
				// array_filter preserves the original keys, so we
				// wrap with array_values to re-pack 0..k-1.
				`array_values(array_filter($nums, fn(int $__p0): bool => __anon_1($__p0)))`,
			},
		},
		{
			fixture: "hof_reduce.mochi",
			wants: []string{
				// array_reduce(xs, fn, init): the standard library
				// signature already matches reduce(xs, fn(acc, x), init).
				`array_reduce($nums, fn(int $__p0, int $__p1): int => __anon_1($__p0, $__p1), 0)`,
			},
		},
	}

	for _, c := range cases {
		t.Run(strings.TrimSuffix(c.fixture, ".mochi"), func(t *testing.T) {
			mochiPath := filepath.Join(repoRoot(t), "tests", "transpiler3", "php", "fixtures", "phase06-closures", c.fixture)
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
