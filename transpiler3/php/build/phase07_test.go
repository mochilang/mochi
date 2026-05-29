package build

import (
	"os"
	"path/filepath"
	"strings"
	"testing"
)

// TestPhase7Query walks the Phase 7 query-DSL fixtures and exercises
// each through the full PHP transpiler. Tests skip when PHP is not
// installed; CI uses shivammathur/setup-php@v2 to drive the
// end-to-end gate.
func TestPhase7Query(t *testing.T) {
	fixtureDir := filepath.Join(repoRoot(t), "tests", "transpiler3", "php", "fixtures", "phase07-query")
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

// TestPhase7EmitFragments asserts the lowerer produces the expected
// PHP shape for each query-pipeline feature: bare where, where+map,
// nested queries, sort ascending, skip / take / skip+take slicing,
// string predicates, and queries inside user-defined functions.
//
// Queries are desugared by the shared aotir lowerer into LetStmt +
// ForEachStmt + (optional ListSortAscExpr / ListSliceExpr); the PHP
// target unwraps the C-only QueryScopeStmt arena wrapper and emits
// the plain imperative form.
func TestPhase7EmitFragments(t *testing.T) {
	cases := []struct {
		fixture string
		wants   []string
	}{
		{
			fixture: "query_filter.mochi",
			wants: []string{
				`$__query1 = [];`,
				`foreach ($nums as $n) {`,
				`if ((($n % 2) === 0)) {`,
				`$__query1 = [...$__query1, $n];`,
				`$evens = $__query1;`,
			},
		},
		{
			fixture: "query_filter_map.mochi",
			wants: []string{
				`if (($n > 3)) {`,
				`$__query1 = [...$__query1, ($n * 10)];`,
			},
		},
		{
			fixture: "query_map.mochi",
			wants: []string{
				// Select-only query: no `if` guard, just push the
				// projected element each iteration.
				`foreach ($nums as $n) {`,
				`$__query1 = [...$__query1, ($n * 2)];`,
			},
		},
		{
			fixture: "query_nested.mochi",
			wants: []string{
				// Two queries in one function share the lowerer's
				// per-function temp counter, so the second one gets
				// __query2.
				`$small = $__query1;`,
				`$doubled_small = $__query2;`,
				`foreach ($small as $n) {`,
			},
		},
		{
			fixture: "query_order_by.mochi",
			wants: []string{
				`function mochi_list_sort_asc(array $xs): array`,
				`usort($xs, fn($a, $b) => $a <=> $b);`,
				// Sort runs after the gather loop so the source order
				// is preserved into the temp before reordering.
				`$__query1 = mochi_list_sort_asc($__query1);`,
				`$sorted = $__query1;`,
			},
		},
		{
			fixture: "query_skip.mochi",
			wants: []string{
				// Skip with no take uses INT_MAX (4611686018427387903)
				// as the upper bound; array_slice clamps to the actual
				// array length so the over-large length is harmless.
				`$__query1 = array_slice($__query1, 2, (4611686018427387903 - 2));`,
			},
		},
		{
			fixture: "query_take.mochi",
			wants: []string{
				// Take-only: start at 0, length = take count.
				`$__query1 = array_slice($__query1, 0, ((0 + 3) - 0));`,
			},
		},
		{
			fixture: "query_skip_take.mochi",
			wants: []string{
				// Skip 1 take 3: array_slice($xs, 1, 3).
				`$__query1 = array_slice($__query1, 1, ((1 + 3) - 1));`,
			},
		},
		{
			fixture: "query_string_filter.mochi",
			wants: []string{
				// String predicates lower to the Phase 2 mochi_str_contains
				// helper (no PHP str_contains short-circuit on empty).
				`mochi_str_contains($w, "a")`,
				`$__query1 = [...$__query1, $w];`,
			},
		},
		{
			fixture: "query_in_function.mochi",
			wants: []string{
				// Queries inside user functions emit an independent
				// __query1 per function body (counters are local).
				`function mochi__filter_evens(array $nums): array`,
				`function mochi__double_all(array $nums): array`,
				`return $__query1;`,
			},
		},
		{
			fixture: "query_order_skip_take.mochi",
			wants: []string{
				// Pipeline: gather → sort → slice → bind.
				`$__query1 = mochi_list_sort_asc($__query1);`,
				`$__query1 = array_slice($__query1, 2, ((2 + 4) - 2));`,
			},
		},
		{
			fixture: "query_bool_filter.mochi",
			wants: []string{
				// Bare bool predicate `where f` lowers to a raw
				// truthiness check, not `=== true`.
				`foreach ($flags as $f) {`,
				`if ($f) {`,
				`$__query1 = [...$__query1, $f];`,
			},
		},
		{
			fixture: "query_order_by_strings.mochi",
			wants: []string{
				// `order by w` over strings goes through the same
				// spaceship-backed helper as ints/floats. Locks the
				// type-agnostic sort path.
				`$words = ["banana", "apple", "cherry", "date"];`,
				`foreach ($words as $w) {`,
				`$__query1 = [...$__query1, $w];`,
				`$__query1 = mochi_list_sort_asc($__query1);`,
				`$sorted = $__query1;`,
			},
		},
		{
			fixture: "query_order_filter.mochi",
			wants: []string{
				// `where n % 2 == 0 order by n` composes a filtered
				// gather and a sort, in that order.
				`foreach ($nums as $n) {`,
				`if ((($n % 2) === 0)) {`,
				`$__query1 = [...$__query1, $n];`,
				`$__query1 = mochi_list_sort_asc($__query1);`,
				`$evens_sorted = $__query1;`,
			},
		},
		{
			fixture: "query_order_take.mochi",
			wants: []string{
				// `order by n take 3` produces gather → sort → slice
				// (with the zero-skip canonical form).
				`$__query1 = mochi_list_sort_asc($__query1);`,
				`$__query1 = array_slice($__query1, 0, ((0 + 3) - 0));`,
				`$top3 = $__query1;`,
			},
		},
		{
			fixture: "query_string_map.mochi",
			wants: []string{
				// `select len(w)` invokes the StrLenExpr lowering
				// inside the per-row push expression.
				`foreach ($words as $w) {`,
				`$__query1 = [...$__query1, strlen($w)];`,
				`$lengths = $__query1;`,
			},
		},
		{
			fixture: "query_order_empty.mochi",
			wants: []string{
				// `order by n` over an empty source list still emits
				// the gather loop, the sort call, and the bind. The
				// sort helper must accept `[]` without erroring; the
				// for-each below it iterates zero times. Other
				// order-by fixtures only exercise multi-element
				// non-empty inputs, so this pins the boundary.
				`$nums = [];`,
				`foreach ($nums as $n) {`,
				`$__query1 = mochi_list_sort_asc($__query1);`,
				`$sorted = $__query1;`,
			},
		},
	}

	for _, c := range cases {
		t.Run(strings.TrimSuffix(c.fixture, ".mochi"), func(t *testing.T) {
			mochiPath := filepath.Join(repoRoot(t), "tests", "transpiler3", "php", "fixtures", "phase07-query", c.fixture)
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
