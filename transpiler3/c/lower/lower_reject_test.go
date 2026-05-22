package lower

import (
	"strings"
	"testing"

	"mochi/parser"
	"mochi/types"
)

// TestLowerRejectsPhase21Plus pins the Phase 2.0 surface boundary:
// shapes that belong in 2.1 (let/var, if, while, for, return) or
// later must produce a clear "unsupported" diagnostic rather than
// silently being miscompiled.
func TestLowerRejectsPhase21Plus(t *testing.T) {
	cases := []struct {
		name    string
		program string
		want    string
	}{
		{
			name:    "let_binding",
			program: "let x = 1\nprint(x)\n",
			want:    "Phase 2.0",
		},
		{
			name:    "if_expression",
			program: "if true { print(1) } else { print(2) }\n",
			want:    "Phase 2.0",
		},
		{
			name:    "user_function_call",
			program: "fun foo() { print(1) }\nfoo()\n",
			want:    "Phase 2.0",
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
