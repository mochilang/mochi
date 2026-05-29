package build

import (
	"os"
	"path/filepath"
	"strings"
	"testing"
)

// TestPhase11Async walks the Phase 11 async fixtures and exercises
// each through the full PHP transpiler. Tests skip when PHP is not
// installed; CI uses shivammathur/setup-php@v2 to drive the end-to-end
// gate.
func TestPhase11Async(t *testing.T) {
	fixtureDir := filepath.Join(repoRoot(t), "tests", "transpiler3", "php", "fixtures", "phase11-async")
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

// TestPhase11EmitFragments asserts that the PHP lowerer emits the
// expected MochiFuture shape and helper calls for each async feature:
// basic async/await, bool/string element types, await chaining,
// multi-future programs, and await_all fan-in.
//
// Async/await lowers to one inline runtime class plus three helper
// functions; user code only ever sees `mochi_future_make`,
// `mochi_future_await`, and `mochi_future_await_all`. All Phase 11
// fixtures observe deterministic results from sequential computations,
// so the body of an `async EXPR` is evaluated eagerly and wrapped.
func TestPhase11EmitFragments(t *testing.T) {
	cases := []struct {
		fixture string
		wants   []string
	}{
		{
			fixture: "async_basic.mochi",
			wants: []string{
				// Inline runtime class ships with every async-using
				// program; it's idempotent across fixtures.
				`final class MochiFuture`,
				`public function __construct(public mixed $value) {}`,
				// Helpers are three named functions; the runtime never
				// branches on element type at PHP-source level.
				`function mochi_future_make($v): MochiFuture`,
				`function mochi_future_await(MochiFuture $f): mixed`,
				`function mochi_future_await_all(array $fs): array`,
				// User-level lowering: bare builtin calls. The
				// `compute()` call is evaluated eagerly inside
				// mochi_future_make, then await unwraps the value.
				`$fut = mochi_future_make(mochi__compute());`,
				`$result = mochi_future_await($fut);`,
			},
		},
		{
			fixture: "async_bool.mochi",
			wants: []string{
				// Element types don't affect the helper signatures; the
				// PHP type `mixed` on the value slot covers all of
				// int/float/bool/string uniformly.
				`$fut = mochi_future_make(mochi__is_even(4));`,
				`$result = mochi_future_await($fut);`,
				`mochi_print_bool($result);`,
			},
		},
		{
			fixture: "async_string.mochi",
			wants: []string{
				`$fut = mochi_future_make(mochi__greet("world"));`,
				`$msg = mochi_future_await($fut);`,
				`mochi_print_str($msg);`,
			},
		},
		{
			fixture: "async_negate.mochi",
			wants: []string{
				// Negative-valued return: MochiFuture stores the int as
				// `mixed`, mochi_future_await pulls it back out
				// unchanged.
				`$fut = mochi_future_make(mochi__negate(99));`,
				`$r = mochi_future_await($fut);`,
			},
		},
		{
			fixture: "async_chain.mochi",
			wants: []string{
				// Two-stage pipeline: the awaited value of the first
				// future flows into the body of the second async.
				`$f1 = mochi_future_make(mochi__add_one(10));`,
				`$v1 = mochi_future_await($f1);`,
				`$f2 = mochi_future_make(mochi__double($v1));`,
				`$v2 = mochi_future_await($f2);`,
			},
		},
		{
			fixture: "async_compose.mochi",
			wants: []string{
				// Same chained pattern with different function names;
				// confirms each `async f()` keeps its own callee.
				`$f1 = mochi_future_make(mochi__add_one(5));`,
				`$f2 = mochi_future_make(mochi__double_it($v1));`,
			},
		},
		{
			fixture: "async_two.mochi",
			wants: []string{
				// Two independent futures in one program: each holds
				// its own MochiFuture instance; awaits are sequential
				// reads of stored values.
				`$f1 = mochi_future_make(mochi__double(1));`,
				`$f2 = mochi_future_make(mochi__double(2));`,
				`$result1 = mochi_future_await($f1);`,
				`$result2 = mochi_future_await($f2);`,
			},
		},
		{
			fixture: "async_sum.mochi",
			wants: []string{
				// Awaited values flow into a plain arithmetic
				// expression at the call site.
				`$f1 = mochi_future_make(mochi__square(3));`,
				`$f2 = mochi_future_make(mochi__square(4));`,
				`$a = mochi_future_await($f1);`,
				`$b = mochi_future_await($f2);`,
			},
		},
		{
			fixture: "async_all.mochi",
			wants: []string{
				// await_all over a list literal of futures: each future
				// is constructed inline, then mochi_future_await_all
				// array-maps the values back out.
				`$f1 = mochi_future_make(mochi__double(1));`,
				`$f2 = mochi_future_make(mochi__double(2));`,
				`$f3 = mochi_future_make(mochi__double(3));`,
				`$results = mochi_future_await_all([$f1, $f2, $f3]);`,
				`foreach ($results as $r)`,
			},
		},
		{
			fixture: "async_parallel_all.mochi",
			wants: []string{
				// Four-way fan-in over square(n); same await_all
				// helper unwraps each MochiFuture in list order.
				`$f1 = mochi_future_make(mochi__square(2));`,
				`$f2 = mochi_future_make(mochi__square(3));`,
				`$f3 = mochi_future_make(mochi__square(4));`,
				`$f4 = mochi_future_make(mochi__square(5));`,
				`$rs = mochi_future_await_all([$f1, $f2, $f3, $f4]);`,
			},
		},
		{
			fixture: "async_counter.mochi",
			wants: []string{
				`$fut = mochi_future_make(mochi__inc(41));`,
				`$r = mochi_future_await($fut);`,
			},
		},
		{
			fixture: "async_triple.mochi",
			wants: []string{
				// Two awaits in sequence on different futures; the
				// awaited values reach two separate print calls.
				`$f1 = mochi_future_make(mochi__triple(4));`,
				`$f2 = mochi_future_make(mochi__triple(7));`,
				`$r1 = mochi_future_await($f1);`,
				`$r2 = mochi_future_await($f2);`,
			},
		},
	}

	for _, c := range cases {
		t.Run(strings.TrimSuffix(c.fixture, ".mochi"), func(t *testing.T) {
			mochiPath := filepath.Join(repoRoot(t), "tests", "transpiler3", "php", "fixtures", "phase11-async", c.fixture)
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
