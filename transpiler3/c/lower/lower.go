package lower

import (
	"fmt"

	"mochi/parser"
	"mochi/transpiler3/c/aotir"
)

// Lower turns a type-checked parser.Program into an aotir.Program.
//
// Phase 1 surface: a script whose top-level body is a single
// `print("...")` call with one string-literal argument. Anything
// else is rejected with "unsupported in Phase 1: <shape>" so the
// gate test catches accidental scope creep. Later phases widen
// the accepted shape per their gate.
//
// The pass synthesises one function named "main" with a single
// CallStmt to the runtime builtin mochi_print_str. The caller is
// expected to have run types.Check on prog first.
func Lower(prog *parser.Program) (*aotir.Program, error) {
	if prog == nil {
		return nil, fmt.Errorf("transpiler3/c/lower: nil program")
	}
	body := &aotir.Block{}
	for i, st := range prog.Statements {
		if st == nil {
			return nil, fmt.Errorf("transpiler3/c/lower: statement %d is nil", i)
		}
		if st.Expr == nil {
			return nil, fmt.Errorf("transpiler3/c/lower: unsupported statement at index %d (Phase 1 accepts a single top-level print() call)", i)
		}
		call, lit, err := matchPrintStringLit(st.Expr.Expr)
		if err != nil {
			return nil, fmt.Errorf("transpiler3/c/lower: statement %d: %w", i, err)
		}
		_ = call
		body.Statements = append(body.Statements, &aotir.CallStmt{
			Func: "mochi_print_str",
			Args: []aotir.Expr{&aotir.StringLit{Value: lit}},
		})
	}
	fn := &aotir.Function{
		Name:       "main",
		ReturnType: aotir.TypeUnit,
		Body:       body,
	}
	p := &aotir.Program{
		Functions: []*aotir.Function{fn},
		Main:      0,
	}
	if err := aotir.Verify(p); err != nil {
		return nil, fmt.Errorf("transpiler3/c/lower: verify: %w", err)
	}
	return p, nil
}

// matchPrintStringLit returns the matched CallExpr and the
// extracted string literal payload when expr is the shape
// `print("literal")`. Any other shape is rejected so the
// Phase 1 gate test fails loudly if upstream changes silently
// expand the accepted surface.
func matchPrintStringLit(expr *parser.Expr) (*parser.CallExpr, string, error) {
	if expr == nil {
		return nil, "", fmt.Errorf("nil expression")
	}
	bin := expr.Binary
	if bin == nil || bin.Left == nil || len(bin.Right) != 0 {
		return nil, "", fmt.Errorf("expected a bare call, got compound binary expression")
	}
	unary := bin.Left
	if len(unary.Ops) != 0 {
		return nil, "", fmt.Errorf("unary operators not supported in Phase 1")
	}
	post := unary.Value
	if post == nil || len(post.Ops) != 0 || post.Target == nil {
		return nil, "", fmt.Errorf("postfix operators not supported in Phase 1")
	}
	call := post.Target.Call
	if call == nil {
		return nil, "", fmt.Errorf("expected a function call, got a different primary")
	}
	if call.Func != "print" {
		return nil, "", fmt.Errorf("expected print(), got %s()", call.Func)
	}
	if len(call.Args) != 1 {
		return nil, "", fmt.Errorf("print() in Phase 1 takes exactly one argument, got %d", len(call.Args))
	}
	arg := call.Args[0]
	if arg == nil || arg.Binary == nil || arg.Binary.Left == nil ||
		len(arg.Binary.Right) != 0 || len(arg.Binary.Left.Ops) != 0 ||
		arg.Binary.Left.Value == nil || len(arg.Binary.Left.Value.Ops) != 0 ||
		arg.Binary.Left.Value.Target == nil {
		return nil, "", fmt.Errorf("print() argument is not a bare literal in Phase 1")
	}
	lit := arg.Binary.Left.Value.Target.Lit
	if lit == nil || lit.Str == nil {
		return nil, "", fmt.Errorf("print() argument is not a string literal in Phase 1")
	}
	return call, *lit.Str, nil
}
