package lower

import (
	"strings"
	"testing"

	"mochi/parser"
	"mochi/types"
)

// TestLowerRejectsPhase22Plus pins the Phase 2.1 surface boundary:
// shapes that belong in 2.2 (for-in, user functions) or later
// (records, casts, none/Option, mixed-type arithmetic) must produce a
// clear, phase-named diagnostic rather than silently being
// miscompiled.
func TestLowerRejectsPhase22Plus(t *testing.T) {
	cases := []struct {
		name    string
		program string
		want    string
	}{
		{
			name:    "user_function_decl",
			program: "fun foo() { print(1) }\nfoo()\n",
			want:    "Phase 2.2",
		},
		{
			name:    "for_loop",
			program: "for i in 0..3 { print(i) }\n",
			want:    "Phase 2.2",
		},
		{
			name:    "value_return",
			program: "return 1\n",
			want:    "Phase 2.2",
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
