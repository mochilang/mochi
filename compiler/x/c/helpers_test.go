//go:build slow

package ccode

import (
	"mochi/parser"
	"mochi/types"
	"testing"
)

func TestListElemTypeLiteral(t *testing.T) {
	prog, err := parser.ParseString("let xs = [1, 2, 3]\n")
	if err != nil {
		t.Fatal(err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		t.Fatalf("type error: %v", errs[0])
	}
	expr := prog.Statements[0].Let.Value
	typ := listElemType(expr, env)
	if !isInt(typ) {
		t.Fatalf("expected int element type, got %v", typ)
	}
}

func TestSelectorTypeStructField(t *testing.T) {
	src := "type Person { name: string }\nlet p = Person{name: \"bob\"}\nlet x = p.name\n"
	prog, err := parser.ParseString(src)
	if err != nil {
		t.Fatal(err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		t.Fatalf("type error: %v", errs[0])
	}
	sel := prog.Statements[2].Let.Value.Binary.Left.Value.Target.Selector
	typ := New(env).selectorType(sel)
	if _, ok := typ.(types.StringType); !ok {
		t.Fatalf("expected string type, got %v", typ)
	}
}
