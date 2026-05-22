package lower

import (
	"strings"
	"testing"

	"mochi/parser"
	"mochi/types"
)

// TestLowerRejectsPhase31Plus pins the Phase 3.1 surface boundary:
// shapes that belong in Phase 3.2 onward (maps, sets, generics,
// casts, none/Option, fun-expressions) must produce a clear,
// phase-named or otherwise-explicit diagnostic rather than silently
// being miscompiled. Records (3.0), lists (3.1), list<R>
// (Phase 3.4a), and list<list<scalar>> (Phase 3.4b) are now
// accepted.
func TestLowerRejectsPhase31Plus(t *testing.T) {
	cases := []struct {
		name    string
		program string
		want    string
	}{
		{
			name:    "print_list",
			program: "let xs = [1, 2, 3]\nprint(xs)\n",
			want:    "Phase 3.1",
		},
		{
			name:    "nested_fun",
			program: "fun outer(): int {\n  fun inner(): int { return 1 }\n  return 1\n}\nprint(outer())\n",
			want:    "nested",
		},
		{
			name:    "fun_missing_return_type",
			program: "fun foo(x: int) { print(x) }\nfoo(1)\n",
			want:    "explicit `: T` return type",
		},
		{
			name:    "fun_missing_param_type",
			program: "fun foo(x): int { return x }\nprint(foo(1))\n",
			want:    "explicit `: T` type",
		},
		{
			name:    "empty_list_literal",
			program: "let xs = []\n",
			want:    "empty list literal",
		},
		{
			name:    "append_type_mismatch",
			program: "let xs = [1, 2]\nlet ys = append(xs, \"oops\")\n",
			want:    "list element type",
		},
		{
			name:    "call_undefined",
			program: "print(bogus(1))\n",
			want:    "unresolved callee",
		},
		{
			name:    "mixed_int_float_arith",
			program: "print(1 + 2.0)\n",
			want:    "operator",
		},
		{
			name:    "none_literal",
			program: "print(none)\n",
			want:    "Option",
		},
		{
			name:    "break_outside_loop",
			program: "break\n",
			want:    "break outside",
		},
		{
			name:    "continue_outside_loop",
			program: "continue\n",
			want:    "continue outside",
		},
		{
			name:    "assign_to_let",
			program: "let x = 1\nx = 2\n",
			want:    "immutable",
		},
		{
			name:    "assign_to_for_var",
			program: "for i in 0..3 {\n  i = i + 1\n}\n",
			want:    "immutable",
		},
		{
			name:    "assign_to_undeclared",
			program: "x = 1\n",
			want:    "undeclared",
		},
		{
			name:    "if_cond_not_bool",
			program: "if 1 { print(1) }\n",
			want:    "if cond must be bool",
		},
		{
			name:    "while_cond_not_bool",
			program: "while 1 { print(1) }\n",
			want:    "while cond must be bool",
		},
		{
			name:    "for_range_start_not_int",
			program: "for x in 1.0..3.0 { print(x) }\n",
			want:    "must be int",
		},
		{
			name:    "value_return_from_main",
			program: "return 1\n",
			want:    "bare `return` only",
		},
		{
			name:    "missing_value_return",
			program: "fun foo(): int {\n  return\n}\nprint(foo())\n",
			want:    "return without a value",
		},
		{
			name:    "return_wrong_type",
			program: "fun foo(): int {\n  return 1.5\n}\nprint(foo())\n",
			want:    "function returns",
		},
		{
			name:    "print_in_expr_position",
			program: "let x = print(1)\n",
			want:    "unit",
		},
	}
	for _, c := range cases {
		t.Run(c.name, func(t *testing.T) {
			prog, err := parser.ParseString(c.program)
			if err != nil {
				// Parser refusal is also an acceptable rejection
				// (the surface is closed earlier).
				return
			}
			_ = types.Check(prog, types.NewEnv(nil))
			if _, err := Lower(prog); err == nil {
				t.Fatalf("expected Lower to reject %q, got nil error", c.program)
			} else if !strings.Contains(err.Error(), c.want) {
				t.Fatalf("Lower error %q did not contain %q", err.Error(), c.want)
			}
		})
	}
}
