package vm

import (
	"mochi/parser"
	"mochi/types"
	"testing"
)

func TestCoalesceMoves(t *testing.T) {
	src := `var x = 1
var y = x
print(y)`
	prog, err := parser.ParseString(src)
	if err != nil {
		t.Fatalf("parse: %v", err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		t.Fatalf("type error: %v", errs[0])
	}
	p, err := Compile(prog, env)
	if err != nil {
		t.Fatalf("compile: %v", err)
	}
	fn := &p.Funcs[0]
	moves := 0
	for _, ins := range fn.Code {
		if ins.Op == OpMove && ins.A != ins.B {
			moves++
		}
	}
	if moves != 1 {
		t.Fatalf("expected 1 Move after coalescing, got %d", moves)
	}
}
