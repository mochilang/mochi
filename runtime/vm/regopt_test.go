package vm

import (
	"mochi/parser"
	"mochi/types"
	"testing"
)

func TestRegisterCoalescing(t *testing.T) {
	src := `var a = 1
var b = a
print(b)`
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
	fn := p.Funcs[0]
	for _, ins := range fn.Code {
		if ins.Op == OpMove && ins.A == 2 && ins.B == 1 {
			t.Fatalf("redundant move not eliminated")
		}
	}
	if fn.NumRegs != 2 {
		t.Fatalf("expected 2 regs, got %d", fn.NumRegs)
	}
}
