package build

import (
	"path/filepath"
	"testing"
)

// TestPhase31Integration exercises cross-cutting scenarios that touch
// multiple phases at once. Per-feature phase tests intentionally isolate
// one mechanism; this file catches regressions where two features
// interact (closures over query results, sums inside maps, try/catch
// around I/O, etc.). All subtests run on CRuby 3.2+.
func TestPhase31Integration(t *testing.T) {
	tc, err := resolveToolchain()
	if err != nil {
		t.Skipf("ruby toolchain not available: %v", err)
	}
	repoRoot := repoRootForTest(t)
	runtimeLib := filepath.Join(repoRoot, "mochi-runtime", "lib")

	t.Run("closure_over_query_result", func(t *testing.T) {
		// Closure (phase 7) consumes a query result (phase 8) and
		// applies map(.) (phase 15) to it. Confirms the lifted lambda
		// trampoline + Enumerable plumbing interoperate.
		src := "let xs: list<int> = [1, 2, 3, 4]\n" +
			"let evens = from x in xs where x % 2 == 0 select x\n" +
			"let dbl = fun(n: int): int => n * 2\n" +
			"let out = map(evens, dbl)\n" +
			"for n in out {\n" +
			"  print(n)\n" +
			"}\n"
		runRubyFixture(t, tc, runtimeLib, "closure_over_query_result", src, "4\n8\n")
	})

	t.Run("nested_hof_filter_then_map_then_reduce", func(t *testing.T) {
		// Chained HOFs (phase 15) over a list (phase 3) with three
		// distinct closures (phase 7). Confirms the .call trampoline
		// composes without intermediate-state corruption.
		src := "fun add(a: int, b: int): int { return a + b }\n" +
			"let xs: list<int> = [1, 2, 3, 4, 5, 6, 7, 8]\n" +
			"let isEven = fun(n: int): bool => n % 2 == 0\n" +
			"let triple = fun(n: int): int => n * 3\n" +
			"let evens = filter(xs, isEven)\n" +
			"let tripled = map(evens, triple)\n" +
			"let total = reduce(tripled, add, 0)\n" +
			"print(total)\n"
		runRubyFixture(t, tc, runtimeLib,
			"nested_hof_filter_then_map_then_reduce", src, "60\n")
	})

	t.Run("try_catch_around_record_field_access", func(t *testing.T) {
		// try/catch (phase 19) around code that constructs and reads
		// a Data.define record (phase 4). Confirms records work
		// transparently inside begin/rescue blocks.
		src := "type Pair = { a: int, b: int }\n" +
			"fun double(p: Pair): int {\n" +
			"  return p.a + p.b\n" +
			"}\n" +
			"var got: int = 0\n" +
			"try {\n" +
			"  let p: Pair = Pair { a: 3, b: 4 }\n" +
			"  got = double(p)\n" +
			"} catch e {\n" +
			"  got = -1\n" +
			"}\n" +
			"print(got)\n"
		runRubyFixture(t, tc, runtimeLib,
			"try_catch_around_record_field_access", src, "7\n")
	})

	t.Run("list_of_records_query", func(t *testing.T) {
		// list<record> (phase 3 + 4) walked by a query (phase 8) that
		// projects a record field. Confirms Data.define field access
		// inside an Enumerable select chain.
		src := "type User = { name: string, age: int }\n" +
			"let users: list<User> = [\n" +
			"  User { name: \"a\", age: 30 },\n" +
			"  User { name: \"b\", age: 17 },\n" +
			"  User { name: \"c\", age: 45 },\n" +
			"]\n" +
			"let adults = from u in users where u.age >= 18 select u.name\n" +
			"for n in adults {\n" +
			"  print(n)\n" +
			"}\n"
		runRubyFixture(t, tc, runtimeLib, "list_of_records_query",
			src, "a\nc\n")
	})

	t.Run("match_arm_returns_distinct_per_variant", func(t *testing.T) {
		// Multi-variant match (phase 6) inside a fun (phase 5) whose
		// callers exercise every arm. Confirms case/in dispatch
		// produces variant-specific return values consistently.
		src := "type Op = Inc | Dbl\n" +
			"fun apply(o: Op, n: int): int {\n" +
			"  return match o {\n" +
			"    Inc => n + 1\n" +
			"    Dbl => n * 2\n" +
			"  }\n" +
			"}\n" +
			"print(apply(Inc, 5))\n" +
			"print(apply(Dbl, 5))\n"
		runRubyFixture(t, tc, runtimeLib,
			"match_arm_returns_distinct_per_variant", src, "6\n10\n")
	})
}
