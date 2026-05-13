package parser_test

import (
	"reflect"
	"testing"

	"mochi/parser"
)

func TestParse_FunStmtEffectAnnotation(t *testing.T) {
	cases := []struct {
		name string
		src  string
		want []string
	}{
		{"no annotation", "fun add(x: int, y: int): int { return x + y }", nil},
		{"single label", "fun greet(name: string) ! io { print(name) }", []string{"io"}},
		{"multi label", "fun snapshot(): int ! io, time { print(1); return now() }", []string{"io", "time"}},
		{"all labels", "fun any() ! io, fs, net, time, meta { }", []string{"io", "fs", "net", "time", "meta"}},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			prog, err := parser.ParseString(tc.src)
			if err != nil {
				t.Fatalf("parse: %v", err)
			}
			if len(prog.Statements) == 0 || prog.Statements[0].Fun == nil {
				t.Fatalf("expected a FunStmt at top, got %+v", prog.Statements)
			}
			got := prog.Statements[0].Fun.Effects
			if tc.want == nil {
				if len(got) != 0 {
					t.Fatalf("expected no effects, got %v", got)
				}
				return
			}
			if !reflect.DeepEqual(got, tc.want) {
				t.Fatalf("Effects=%v want %v", got, tc.want)
			}
		})
	}
}

func TestParse_FunExprEffectAnnotation(t *testing.T) {
	src := "let f = fun(x: int): int ! io => x"
	prog, err := parser.ParseString(src)
	if err != nil {
		t.Fatalf("parse: %v", err)
	}
	let := prog.Statements[0].Let
	if let == nil || let.Value == nil {
		t.Fatalf("expected let, got %+v", prog.Statements[0])
	}
	p := let.Value.Binary.Left.Value.Target
	if p == nil || p.FunExpr == nil {
		t.Fatalf("expected FunExpr, got %+v", p)
	}
	if got, want := p.FunExpr.Effects, []string{"io"}; !reflect.DeepEqual(got, want) {
		t.Fatalf("FunExpr.Effects=%v want %v", got, want)
	}
}
