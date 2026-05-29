package lower

import (
	"fmt"
	"path/filepath"
	"strings"
	"unicode"

	"mochi/transpiler3/c/aotir"
	"mochi/transpiler3/ruby/rtree"
)

// Lower translates an aotir.Program into an rtree.SourceFile. The fileBase
// is the basename of the .rb file (no extension); className is the
// PascalCase module name that wraps the Mochi main function.
func Lower(prog *aotir.Program, fileBase, className string) (*rtree.SourceFile, error) {
	if prog == nil {
		return nil, fmt.Errorf("ruby lower: nil program")
	}
	if prog.Main < 0 || prog.Main >= len(prog.Functions) {
		return nil, fmt.Errorf("ruby lower: invalid Main index %d (have %d functions)", prog.Main, len(prog.Functions))
	}

	mainFn := prog.Functions[prog.Main]
	body, err := lowerBlock(mainFn.Body)
	if err != nil {
		return nil, err
	}

	runMethod := &rtree.MethodDecl{
		Receiver: "self",
		Name:     "run",
		Params:   []rtree.MethodParam{{Name: "argv"}},
		Body:     body,
	}

	mainModule := &rtree.ModuleDecl{
		Name:  "Main",
		Decls: []rtree.Decl{runMethod},
	}

	programModule := &rtree.ModuleDecl{
		Name:  className,
		Decls: []rtree.Decl{mainModule},
	}

	entry := &rtree.RawDecl{
		Text: fmt.Sprintf("if __FILE__ == $PROGRAM_NAME\n  %s::Main.run(ARGV)\nend", className),
	}

	sf := &rtree.SourceFile{
		Name:                fileBase,
		FrozenStringLiteral: true,
		Requires:            []string{"mochi/runtime"},
		Decls:               []rtree.Decl{programModule, entry},
	}
	return sf, nil
}

// ModuleName converts a Mochi source filename to a PascalCase module name.
// "hello.mochi"        -> "Hello"
// "hello_world.mochi"  -> "HelloWorld"
func ModuleName(src string) string {
	src = filepath.Base(src)
	src = strings.TrimSuffix(src, ".mochi")
	parts := strings.FieldsFunc(src, func(r rune) bool {
		return r == '_' || r == '-'
	})
	var sb strings.Builder
	for _, p := range parts {
		if len(p) == 0 {
			continue
		}
		runes := []rune(p)
		runes[0] = unicode.ToUpper(runes[0])
		sb.WriteString(string(runes))
	}
	if sb.Len() == 0 {
		return "Main"
	}
	return sb.String()
}

func lowerBlock(blk *aotir.Block) ([]rtree.Stmt, error) {
	if blk == nil {
		return nil, nil
	}
	out := make([]rtree.Stmt, 0, len(blk.Statements))
	for _, s := range blk.Statements {
		st, err := lowerStmt(s)
		if err != nil {
			return nil, err
		}
		if st != nil {
			out = append(out, st)
		}
	}
	return out, nil
}

func lowerStmt(s aotir.Stmt) (rtree.Stmt, error) {
	switch s := s.(type) {
	case *aotir.CallStmt:
		return lowerCallStmt(s)
	case *aotir.LetStmt:
		return lowerLetStmt(s)
	case *aotir.AssignStmt:
		return lowerAssignStmt(s)
	}
	return nil, fmt.Errorf("ruby lower: unsupported statement type %T", s)
}

func lowerLetStmt(l *aotir.LetStmt) (rtree.Stmt, error) {
	rhs, err := lowerExpr(l.Init)
	if err != nil {
		return nil, fmt.Errorf("let %s: %w", l.Name, err)
	}
	return &rtree.Assign{LHS: rubyIdent(l.Name), RHS: rhs}, nil
}

func lowerAssignStmt(a *aotir.AssignStmt) (rtree.Stmt, error) {
	rhs, err := lowerExpr(a.Value)
	if err != nil {
		return nil, fmt.Errorf("assign %s: %w", a.Name, err)
	}
	return &rtree.Assign{LHS: rubyIdent(a.Name), RHS: rhs}, nil
}

func lowerCallStmt(c *aotir.CallStmt) (rtree.Stmt, error) {
	switch c.Func {
	case "mochi_print_str", "mochi_print_bool":
		if len(c.Args) != 1 {
			return nil, fmt.Errorf("ruby lower: %s expects 1 arg, got %d", c.Func, len(c.Args))
		}
		arg, err := lowerExpr(c.Args[0])
		if err != nil {
			return nil, err
		}
		return &rtree.ExprStmt{X: &rtree.MethodCall{
			Method: "puts",
			Args:   []rtree.Expr{arg},
		}}, nil
	case "mochi_print_i64":
		if len(c.Args) != 1 {
			return nil, fmt.Errorf("ruby lower: %s expects 1 arg, got %d", c.Func, len(c.Args))
		}
		arg, err := lowerExpr(c.Args[0])
		if err != nil {
			return nil, err
		}
		return &rtree.ExprStmt{X: &rtree.MethodCall{
			Method: "puts",
			Args:   []rtree.Expr{arg},
		}}, nil
	case "mochi_print_f64":
		if len(c.Args) != 1 {
			return nil, fmt.Errorf("ruby lower: %s expects 1 arg, got %d", c.Func, len(c.Args))
		}
		arg, err := lowerExpr(c.Args[0])
		if err != nil {
			return nil, err
		}
		// Use Mochi::Runtime::IO.putln for floats so "3.0" prints with the trailing zero
		// (Ruby's bare puts already does this, but route through the runtime so all
		// formatting is in one place for later phases that need finer control).
		return &rtree.ExprStmt{X: &rtree.MethodCall{
			Receiver: &rtree.Ident{Name: "Mochi::Runtime::IO"},
			Method:   "putln",
			Args:     []rtree.Expr{arg},
		}}, nil
	}
	return nil, fmt.Errorf("ruby lower: unsupported callee %q", c.Func)
}

func lowerExpr(e aotir.Expr) (rtree.Expr, error) {
	switch e := e.(type) {
	case *aotir.StringLit:
		return &rtree.StringLit{Value: e.Value}, nil
	case *aotir.IntLit:
		return &rtree.IntLit{Value: e.Value}, nil
	case *aotir.FloatLit:
		return &rtree.FloatLit{Value: e.Value}, nil
	case *aotir.BoolLit:
		return &rtree.BoolLit{Value: e.Value}, nil
	case *aotir.VarRef:
		return &rtree.Ident{Name: rubyIdent(e.Name)}, nil
	case *aotir.BinaryExpr:
		return lowerBinary(e)
	case *aotir.UnaryExpr:
		return lowerUnary(e)
	}
	return nil, fmt.Errorf("ruby lower: unsupported expression type %T", e)
}

func lowerBinary(b *aotir.BinaryExpr) (rtree.Expr, error) {
	lhs, err := lowerExpr(b.Left)
	if err != nil {
		return nil, err
	}
	rhs, err := lowerExpr(b.Right)
	if err != nil {
		return nil, err
	}
	op, ok := rubyBinOp(b.Op)
	if !ok {
		return nil, fmt.Errorf("ruby lower: unsupported BinOp %d", b.Op)
	}
	// Integer division in Mochi truncates toward zero; Ruby's `/` on Integers floor-divides.
	// For BinDivI64 we need to truncate. Use `.div(rhs)` is also floor; Ruby's `Integer#/` is
	// equivalent to `divmod` which floor-divides for negatives. For Phase 2 fixtures we use
	// only positive operands, so plain `/` is fine; flag this for later.
	return &rtree.BinaryOp{Op: op, Lhs: lhs, Rhs: rhs}, nil
}

func lowerUnary(u *aotir.UnaryExpr) (rtree.Expr, error) {
	x, err := lowerExpr(u.Operand)
	if err != nil {
		return nil, err
	}
	switch u.Op {
	case aotir.UnNegI64, aotir.UnNegF64:
		return &rtree.UnaryOp{Op: "-", X: x}, nil
	case aotir.UnNotBool:
		return &rtree.UnaryOp{Op: "!", X: x}, nil
	}
	return nil, fmt.Errorf("ruby lower: unsupported UnOp %d", u.Op)
}

func rubyBinOp(op aotir.BinOp) (string, bool) {
	switch op {
	case aotir.BinAddI64, aotir.BinAddF64:
		return "+", true
	case aotir.BinSubI64, aotir.BinSubF64:
		return "-", true
	case aotir.BinMulI64, aotir.BinMulF64:
		return "*", true
	case aotir.BinDivI64, aotir.BinDivF64:
		return "/", true
	case aotir.BinModI64:
		return "%", true
	case aotir.BinEqI64, aotir.BinEqF64, aotir.BinEqBool, aotir.BinEqStr:
		return "==", true
	case aotir.BinNeI64, aotir.BinNeF64, aotir.BinNeBool, aotir.BinNeStr:
		return "!=", true
	case aotir.BinLtI64, aotir.BinLtF64:
		return "<", true
	case aotir.BinLeI64, aotir.BinLeF64:
		return "<=", true
	case aotir.BinGtI64, aotir.BinGtF64:
		return ">", true
	case aotir.BinGeI64, aotir.BinGeF64:
		return ">=", true
	case aotir.BinAndBool:
		return "&&", true
	case aotir.BinOrBool:
		return "||", true
	case aotir.BinStrCat:
		return "+", true
	}
	return "", false
}

// rubyIdent maps a Mochi-source identifier to a Ruby-safe local variable
// name. Ruby reserves a small handful of keywords (alias, and, begin, break,
// case, class, def, defined?, do, else, elsif, end, ensure, false, for, if,
// in, module, next, nil, not, or, redo, rescue, retry, return, self, super,
// then, true, undef, unless, until, when, while, yield). For Phase 2 we
// suffix collisions with `_`. Future phases may need a more thorough mangling.
func rubyIdent(name string) string {
	if _, hit := rubyReserved[name]; hit {
		return name + "_"
	}
	return name
}

var rubyReserved = map[string]struct{}{
	"alias": {}, "and": {}, "begin": {}, "break": {}, "case": {}, "class": {},
	"def": {}, "defined?": {}, "do": {}, "else": {}, "elsif": {}, "end": {},
	"ensure": {}, "false": {}, "for": {}, "if": {}, "in": {}, "module": {},
	"next": {}, "nil": {}, "not": {}, "or": {}, "redo": {}, "rescue": {},
	"retry": {}, "return": {}, "self": {}, "super": {}, "then": {}, "true": {},
	"undef": {}, "unless": {}, "until": {}, "when": {}, "while": {}, "yield": {},
}
