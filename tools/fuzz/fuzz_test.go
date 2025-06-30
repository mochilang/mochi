//go:build slow

package fuzz

import (
	"io"
	"testing"

	"github.com/alecthomas/participle/v2"
	"github.com/alecthomas/participle/v2/lexer"
	"mochi/interpreter"
	"mochi/parser"
	"mochi/runtime/data"
	vm "mochi/runtime/vm"
	"mochi/types"
)

var queryParser = participle.MustBuild[parser.QueryExpr](
	participle.Lexer(lexer.MustSimple([]lexer.SimpleRule{
		{Name: "Comment", Pattern: `//[^\n]*|/\*([^*]|\*+[^*/])*\*/`},
		{Name: "Bool", Pattern: `\b(true|false)\b`},
		{Name: "Keyword", Pattern: `\b(test|expect|agent|intent|on|stream|emit|type|fun|extern|import|return|break|continue|let|var|if|else|for|while|in|generate|match|fetch|load|save|package|export|fact|rule|all|null)\b`},
		{Name: "Ident", Pattern: `[\p{L}\p{So}_][\p{L}\p{So}\p{N}_]*`},
		{Name: "Float", Pattern: `\d+\.\d+`},
		{Name: "Int", Pattern: `\d+`},
		{Name: "String", Pattern: `"(?:\\.|[^"])*"`},
		{Name: "Punct", Pattern: `==|!=|<=|>=|&&|\|\||=>|:-|\.\.|[-+*/%=<>!|{}\[\](),.:]`},
		{Name: "Whitespace", Pattern: `[ \t\n\r]+`},
	})),
	participle.Elide("Whitespace", "Comment"),
	participle.Unquote("String"),
	participle.UseLookahead(999),
)

// FuzzVM compiles and executes arbitrary Mochi programs. Invalid
// programs are skipped. Any panic or crash inside the VM will be
// surfaced by the Go fuzzing engine.
func FuzzVM(f *testing.F) {
	gen := NewGenerator()
	for {
		src, ok := gen.Next()
		if !ok {
			break
		}
		f.Add(src)
	}

	f.Fuzz(func(t *testing.T, src string) {
		prog, err := parser.ParseString(src)
		if err != nil {
			t.Skip()
		}
		env := types.NewEnv(nil)
		if errs := types.Check(prog, env); len(errs) > 0 {
			t.Skip()
		}
		p, err := vm.Compile(prog, env)
		if err != nil {
			t.Skip()
		}
		m := vm.New(p, io.Discard)
		_ = m.Run()
	})
}

// FuzzQueries executes randomly generated dataset queries using the interpreter
// and in-memory engine. Invalid queries are skipped.
func FuzzQueries(f *testing.F) {
	gen := NewQueryGenerator()
	for {
		q, ok := gen.Next()
		if !ok {
			break
		}
		f.Add(q)
	}

	f.Fuzz(func(t *testing.T, q string) {
		query, err := queryParser.ParseString("", q)
		if err != nil {
			t.Skip()
		}
		env := newDatasetEnv()
		eval := func(e *types.Env, ex *parser.Expr) (any, error) {
			interp := interpreter.New(&parser.Program{}, e, "")
			interp.SetDataPlan("memory")
			return interp.EvalExpr(ex)
		}
		_, err = data.RunQuery(query, env, data.MemoryEngine{}, eval)
		if err != nil {
			t.Skip()
		}
	})
}
