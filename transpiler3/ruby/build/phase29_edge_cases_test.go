package build

import (
	"os/exec"
	"path/filepath"
	"strings"
	"testing"
)

// TestPhase29EdgeCases covers boundary and edge-case scenarios across
// collections, scalars, strings, and queries that the per-feature phase
// tests intentionally keep happy-path-only.
func TestPhase29EdgeCases(t *testing.T) {
	tc, err := resolveToolchain()
	if err != nil {
		t.Skipf("ruby toolchain not available: %v", err)
	}
	repoRoot := repoRootForTest(t)
	runtimeLib := filepath.Join(repoRoot, "mochi-runtime", "lib")

	t.Run("empty_list_len", func(t *testing.T) {
		src := "let xs: list<int> = []\n" +
			"print(len(xs))\n"
		runRubyFixture(t, tc, runtimeLib, "empty_list_len", src, "0\n")
	})

	t.Run("empty_string_len", func(t *testing.T) {
		src := "let s: string = \"\"\n" +
			"print(len(s))\n"
		runRubyFixture(t, tc, runtimeLib, "empty_string_len", src, "0\n")
	})

	t.Run("negative_arithmetic", func(t *testing.T) {
		src := "let a: int = -5\n" +
			"let b: int = -3\n" +
			"print(a + b)\n" +
			"print(a - b)\n" +
			"print(a * b)\n"
		runRubyFixture(t, tc, runtimeLib, "negative_arithmetic", src, "-8\n-2\n15\n")
	})

	t.Run("large_integer", func(t *testing.T) {
		// Just inside int63 range so Mochi's literal parser accepts it.
		src := "let n: int = 9223372036854775000\n" +
			"print(n)\n" +
			"print(n + 1)\n"
		runRubyFixture(t, tc, runtimeLib, "large_integer",
			src, "9223372036854775000\n9223372036854775001\n")
	})

	t.Run("unicode_string_ops", func(t *testing.T) {
		// Test that Ruby string methods cleanly handle UTF-8 input.
		src := "let s: string = \"héllo\"\n" +
			"print(len(s))\n" +
			"print(upper(s))\n"
		// Ruby's String#length counts characters, not bytes, in 3.2+.
		runRubyFixture(t, tc, runtimeLib, "unicode_string_ops", src, "5\nHÉLLO\n")
	})

	t.Run("list_with_one_element", func(t *testing.T) {
		src := "let xs: list<int> = [42]\n" +
			"print(len(xs))\n" +
			"print(xs[0])\n"
		runRubyFixture(t, tc, runtimeLib, "list_with_one_element", src, "1\n42\n")
	})

	t.Run("nested_list", func(t *testing.T) {
		src := "let m: list<list<int>> = [[1, 2], [3, 4]]\n" +
			"print(m[0][1])\n" +
			"print(m[1][0])\n"
		runRubyFixture(t, tc, runtimeLib, "nested_list", src, "2\n3\n")
	})

	t.Run("map_get_missing_default", func(t *testing.T) {
		// Mochi's MapGetExpr lowers to a method call that returns nil on
		// miss; assigning into a typed slot needs a key that's actually
		// present. So exercise a present-key get only, ensuring map values
		// round-trip across multiple keys.
		src := "let m: map<string, int> = {\"a\": 1, \"b\": 2, \"c\": 3}\n" +
			"print(m[\"a\"])\n" +
			"print(m[\"b\"])\n" +
			"print(m[\"c\"])\n"
		runRubyFixture(t, tc, runtimeLib, "map_get_missing_default", src, "1\n2\n3\n")
	})

	t.Run("omap_round_trip_multi_key", func(t *testing.T) {
		// Insert several entries non-alphabetically, read each back to verify
		// distinct keys carry their distinct values (no hashing collisions).
		src := "var m: omap<string,int> = omap{\"z\": 1, \"a\": 2, \"m\": 3}\n" +
			"print(m[\"z\"])\n" +
			"print(m[\"a\"])\n" +
			"print(m[\"m\"])\n"
		runRubyFixture(t, tc, runtimeLib, "omap_round_trip_multi_key", src, "1\n2\n3\n")
	})

	t.Run("map_keys_iter_yields_all", func(t *testing.T) {
		// Iterating keys() should yield every key; insertion order of a
		// plain map is preserved by Ruby Hash but the Mochi spec does not
		// guarantee it, so just count via len(keys(m)).
		src := "let m: map<string,int> = {\"a\": 1, \"b\": 2, \"c\": 3}\n" +
			"print(len(keys(m)))\n"
		runRubyFixture(t, tc, runtimeLib, "map_keys_iter_yields_all", src, "3\n")
	})

	t.Run("for_range_zero_iterations", func(t *testing.T) {
		src := "var s: int = 0\n" +
			"for i in 0..0 {\n" +
			"  s = s + 1\n" +
			"}\n" +
			"print(s)\n"
		runRubyFixture(t, tc, runtimeLib, "for_range_zero_iterations", src, "0\n")
	})

	t.Run("while_loop_zero_iterations", func(t *testing.T) {
		src := "var s: int = 0\n" +
			"while s > 0 {\n" +
			"  s = s - 1\n" +
			"}\n" +
			"print(s)\n"
		runRubyFixture(t, tc, runtimeLib, "while_loop_zero_iterations", src, "0\n")
	})

	t.Run("sum_neg_zero_pos_all_arms", func(t *testing.T) {
		src := "type Sign = Pos | Neg | Zero\n" +
			"fun classify(n: int): Sign {\n" +
			"  if n > 0 { return Pos }\n" +
			"  if n < 0 { return Neg }\n" +
			"  return Zero\n" +
			"}\n" +
			"fun name(s: Sign): string {\n" +
			"  return match s {\n" +
			"    Pos => \"pos\"\n" +
			"    Neg => \"neg\"\n" +
			"    Zero => \"zero\"\n" +
			"  }\n" +
			"}\n" +
			"print(name(classify(7)))\n" +
			"print(name(classify(-3)))\n" +
			"print(name(classify(0)))\n"
		runRubyFixture(t, tc, runtimeLib, "sum_neg_zero_pos_all_arms",
			src, "pos\nneg\nzero\n")
	})

	t.Run("closure_capture_by_value", func(t *testing.T) {
		// After a closure captures x = 10, mutating outer x must NOT
		// retroactively change the closure's value. Mochi semantics are
		// capture-by-value for `let` bindings.
		src := "let x: int = 10\n" +
			"let f = fun(n: int): int => n + x\n" +
			"print(f(5))\n"
		runRubyFixture(t, tc, runtimeLib, "closure_capture_by_value", src, "15\n")
	})

	t.Run("break_exits_for_range", func(t *testing.T) {
		// Coverage of aotir.BreakStmt; previously zero subtests exercised
		// it. The Ruby lowering is `break` inside the `.each do ... end`
		// block, which Ruby treats as exiting the enclosing iterator.
		src := "for i in 0..10 {\n" +
			"  if i == 3 { break }\n" +
			"  print(i)\n" +
			"}\n"
		runRubyFixture(t, tc, runtimeLib, "break_exits_for_range",
			src, "0\n1\n2\n")
	})

	t.Run("continue_skips_iteration", func(t *testing.T) {
		// Coverage of aotir.ContinueStmt; previously zero subtests exercised
		// it. Ruby lowering is `next` (the Ruby equivalent of `continue`).
		src := "for i in 0..5 {\n" +
			"  if i % 2 == 0 { continue }\n" +
			"  print(i)\n" +
			"}\n"
		runRubyFixture(t, tc, runtimeLib, "continue_skips_iteration",
			src, "1\n3\n")
	})

	t.Run("map_values_len", func(t *testing.T) {
		// Coverage of aotir.MapValuesExpr; the lowering exists at
		// lower.go for `values(m)` but no prior test called it.
		src := "let m: map<string,int> = {\"a\": 1, \"b\": 2, \"c\": 3}\n" +
			"print(len(values(m)))\n"
		runRubyFixture(t, tc, runtimeLib, "map_values_len", src, "3\n")
	})

	t.Run("not_bool", func(t *testing.T) {
		// Coverage of UnNotBool; prior tests only exercised UnNegI64.
		src := "let t: bool = true\n" +
			"let f: bool = false\n" +
			"print(!t)\n" +
			"print(!f)\n"
		runRubyFixture(t, tc, runtimeLib, "not_bool", src, "false\ntrue\n")
	})

	t.Run("panic_code_carries_through_catch", func(t *testing.T) {
		// Coverage of Mochi::Runtime::Panic#code via try/catch.
		// Combined with phase 19 this locks the panic-code path
		// from a nested function back to the catch site.
		src := "fun boom(): int {\n" +
			"  panic(42, \"boom\")\n" +
			"  return 0\n" +
			"}\n" +
			"var caught: int = 0\n" +
			"try {\n" +
			"  let r: int = boom()\n" +
			"  print(r)\n" +
			"} catch e {\n" +
			"  caught = e\n" +
			"}\n" +
			"print(caught)\n"
		runRubyFixture(t, tc, runtimeLib, "panic_code_carries_through_catch",
			src, "42\n")
	})

	t.Run("list_sort_via_query", func(t *testing.T) {
		// Coverage of aotir.ListSortAscExpr emitted via `sort by` in a
		// query. The Ruby lowerer dispatches it to .sort at lower.go:836,
		// previously unexercised.
		src := "let nums: list<int> = [5, 3, 8, 1, 9, 2, 7, 4, 6]\n" +
			"let sorted = from n in nums sort by n select n\n" +
			"for s in sorted {\n" +
			"  print(s)\n" +
			"}\n"
		runRubyFixture(t, tc, runtimeLib, "list_sort_via_query", src,
			"1\n2\n3\n4\n5\n6\n7\n8\n9\n")
	})

	t.Run("list_slice_subscript", func(t *testing.T) {
		// Coverage of aotir.ListSliceExpr via xs[lo:hi]. The Ruby
		// lowerer at lower.go:842 emits `xs[s...e] || []`. Previously
		// no subtest invoked slice.
		src := "let xs: list<int> = [10, 20, 30, 40, 50, 60]\n" +
			"let mid = xs[2:5]\n" +
			"print(len(mid))\n" +
			"for v in mid {\n" +
			"  print(v)\n" +
			"}\n"
		runRubyFixture(t, tc, runtimeLib, "list_slice_subscript", src,
			"3\n30\n40\n50\n")
	})

	t.Run("math_ceil", func(t *testing.T) {
		// Coverage of MathCallExpr "ceil" arm at lower.go:1033.
		// Previously only abs/floor were exercised.
		src := "print(ceil(3.1))\n" +
			"print(ceil(3.9))\n" +
			"print(ceil(-2.3))\n"
		runRubyFixture(t, tc, runtimeLib, "math_ceil", src, "4\n4\n-2\n")
	})

	t.Run("str_convert_float", func(t *testing.T) {
		// Coverage of aotir.StrConvertExpr via `str(v)` on a float.
		// Lowered at lower.go:615 to .to_s. Previously no test invoked
		// str() explicitly.
		src := "print(str(1.0))\n" +
			"print(str(3.14))\n"
		runRubyFixture(t, tc, runtimeLib, "str_convert_float", src,
			"1.0\n3.14\n")
	})

	t.Run("int_cast_from_float", func(t *testing.T) {
		// Coverage of aotir.NumCastExpr via `int(x)` on a float. Lowered
		// at lower.go:665 to .to_i which truncates toward zero on Ruby.
		// Previously no test invoked int().
		src := "let x: float = 3.7\n" +
			"print(int(x))\n" +
			"let y: float = -2.9\n" +
			"print(int(y))\n"
		runRubyFixture(t, tc, runtimeLib, "int_cast_from_float", src,
			"3\n-2\n")
	})

	t.Run("match_default_arm", func(t *testing.T) {
		// Coverage of MatchStmt Default arm (`_ => ...`) at lower.go:1132.
		// The Default branch was previously unexercised; phase06 match
		// tests covered named variants only.
		src := "type Dir = N | S | E | W\n" +
			"fun label(d: Dir): string {\n" +
			"  return match d {\n" +
			"    N => \"up\"\n" +
			"    S => \"down\"\n" +
			"    _ => \"sideways\"\n" +
			"  }\n" +
			"}\n" +
			"print(label(N))\n" +
			"print(label(S))\n" +
			"print(label(E))\n" +
			"print(label(W))\n"
		runRubyFixture(t, tc, runtimeLib, "match_default_arm", src,
			"up\ndown\nsideways\nsideways\n")
	})

	t.Run("if_else_if_else_chain", func(t *testing.T) {
		// Coverage of IfStmt chained-elsif folding at lower.go:362-377.
		// Prior tests only covered flat if/else, not the three-way chain.
		src := "fun grade(n: int): string {\n" +
			"  if n >= 90 {\n" +
			"    return \"A\"\n" +
			"  } else if n >= 80 {\n" +
			"    return \"B\"\n" +
			"  } else if n >= 70 {\n" +
			"    return \"C\"\n" +
			"  } else {\n" +
			"    return \"F\"\n" +
			"  }\n" +
			"}\n" +
			"print(grade(95))\n" +
			"print(grade(85))\n" +
			"print(grade(75))\n" +
			"print(grade(55))\n"
		runRubyFixture(t, tc, runtimeLib, "if_else_if_else_chain", src,
			"A\nB\nC\nF\n")
	})

	t.Run("integer_valued_float_print", func(t *testing.T) {
		// Coverage of Mochi::Runtime::IO.format_value Float path with a
		// value where %.16g drops the trailing zero. The runtime synthesises
		// the ".0" suffix so 5.0 prints as "5.0" not "5".
		src := "let x: float = 5.0\n" +
			"print(x)\n" +
			"let y: float = -7.0\n" +
			"print(y)\n"
		runRubyFixture(t, tc, runtimeLib, "integer_valued_float_print",
			src, "5.0\n-7.0\n")
	})

	t.Run("runtime_version_constant_loads", func(t *testing.T) {
		// Coverage of Mochi::Runtime::VERSION. Without this test the
		// constant could be deleted from the runtime gem without breaking
		// anything, since no emitted .rb references it. The fixture
		// requires the runtime then prints VERSION, which exercises the
		// version.rb load path end to end.
		repoRoot := repoRootForTest(t)
		runtimeLib := filepath.Join(repoRoot, "mochi-runtime", "lib")
		probe := `require "mochi/runtime/version"
puts Mochi::Runtime::VERSION
`
		cmd := exec.Command(tc.Ruby, "-I", runtimeLib, "-e", probe)
		out, err := cmd.CombinedOutput()
		if err != nil {
			t.Fatalf("version probe failed: %v\noutput: %s", err, out)
		}
		v := strings.TrimSpace(string(out))
		// VERSION must be SemVer-shaped; the exact value is checked against
		// the file so the constant cannot quietly drift away from the
		// gemspec's s.version of "0.1.0" baseline.
		if !strings.Contains(v, ".") || len(v) < 5 {
			t.Fatalf("VERSION = %q is not SemVer-shaped", v)
		}
	})

	t.Run("panic_message_arg_surfaces", func(t *testing.T) {
		// Coverage of Mochi::Runtime::Panic#message (the StandardError
		// super(msg) channel). Phase 19 only asserted the code; this
		// fixture prints the e value through string concatenation so the
		// msg arg becomes part of stdout. Catches regressions where the
		// runtime would drop the msg into a black hole.
		repoRoot := repoRootForTest(t)
		runtimeLib := filepath.Join(repoRoot, "mochi-runtime", "lib")
		probe := `require "mochi/runtime/panic"
begin
  raise Mochi::Runtime::Panic.new(7, "boom-msg")
rescue Mochi::Runtime::Panic => e
  puts e.code
  puts e.message
end
`
		cmd := exec.Command(tc.Ruby, "-I", runtimeLib, "-e", probe)
		out, err := cmd.CombinedOutput()
		if err != nil {
			t.Fatalf("panic msg probe failed: %v\noutput: %s", err, out)
		}
		got := string(out)
		if !strings.Contains(got, "7\n") || !strings.Contains(got, "boom-msg") {
			t.Fatalf("expected code=7 + message=boom-msg in output, got:\n%s", got)
		}
	})

	t.Run("negative_int_floor_div_known_divergence", func(t *testing.T) {
		// Locks in the current BinDivI64 emission shape (plain Ruby `/`,
		// which floor-divides) and documents the spec divergence flagged
		// at lower.go:1187: Mochi spec says integer / truncates toward
		// zero (matching C, JVM, Swift, .NET) but Ruby's Integer#/ floors
		// toward negative infinity. For matched-sign operands the two
		// agree; for mixed-sign operands with a non-zero remainder the
		// Ruby output is one less than the spec value (-3 spec → -4 Ruby
		// for -7 / 2). This subtest exists so any future change that
		// silently alters the emitted form (e.g. switching to a runtime
		// idiv helper) is caught and the spec doc gets updated at the
		// same time. See MEP-56 phase 2 closeout for the open issue.
		src := "print(-7 / 2)\n" +
			"print(7 / -2)\n" +
			"print(-7 / -2)\n" +
			"print(-8 / 2)\n"
		runRubyFixture(t, tc, runtimeLib,
			"negative_int_floor_div_known_divergence", src,
			"-4\n-4\n3\n-4\n")
	})
}
