//go:build slow

package vm

import (
	"mochi/parser"
	"mochi/types"
	"testing"
)

func TestInfer_TagPropagation(t *testing.T) {
	src := `var x = 1
if x > 0 {
  x = x + 1
} else {
  x = x - 1
}
print(x)`
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
	res := Infer(fn)

	pc := -1
	for i, ins := range fn.Code {
		if ins.Op == OpPrint {
			pc = i
			break
		}
	}
	if pc == -1 {
		t.Fatalf("print not found")
	}
	if res.In[pc][1] != TagInt {
		t.Fatalf("register r1 expected int, got %v", res.In[pc][1])
	}
	if res.Dead[pc] {
		t.Fatalf("print instruction marked dead")
	}
}
