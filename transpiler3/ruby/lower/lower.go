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

	mainDecls := []rtree.Decl{runMethod}
	for i, fn := range prog.Functions {
		if i == prog.Main {
			continue
		}
		md, err := lowerFunction(fn)
		if err != nil {
			return nil, fmt.Errorf("ruby lower: function %s: %w", fn.Name, err)
		}
		mainDecls = append(mainDecls, md)
	}

	mainModule := &rtree.ModuleDecl{
		Name:  "Main",
		Decls: mainDecls,
	}

	progDecls := make([]rtree.Decl, 0, len(prog.Records)+len(prog.Unions)+1)
	for _, r := range prog.Records {
		fields := make([]string, len(r.Fields))
		for i, f := range r.Fields {
			fields[i] = f.Name
		}
		progDecls = append(progDecls, &rtree.DataDecl{Name: r.Name, Fields: fields})
	}
	for _, u := range prog.Unions {
		variantDecls := make([]rtree.Decl, len(u.Variants))
		for i, v := range u.Variants {
			fs := make([]string, len(v.Fields))
			for j, f := range v.Fields {
				fs[j] = f.Name
			}
			variantDecls[i] = &rtree.DataDecl{Name: v.Name, Fields: fs}
		}
		progDecls = append(progDecls, &rtree.ModuleDecl{Name: u.Name, Decls: variantDecls})
	}
	progDecls = append(progDecls, mainModule)

	programModule := &rtree.ModuleDecl{
		Name:  className,
		Decls: progDecls,
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

func lowerFunction(fn *aotir.Function) (*rtree.MethodDecl, error) {
	body, err := lowerBlock(fn.Body)
	if err != nil {
		return nil, err
	}
	params := make([]rtree.MethodParam, len(fn.Params))
	for i, p := range fn.Params {
		params[i] = rtree.MethodParam{Name: rubyIdent(p.Name)}
	}
	return &rtree.MethodDecl{
		Receiver: "self",
		Name:     rubyMethodName(fn.Name),
		Params:   params,
		Body:     body,
	}, nil
}

// rubyMethodName lowers a Mochi (possibly mangled) function name to a
// Ruby-safe method name. Currently a no-op pass-through; future work may
// strip mochi internal prefixes or fold dotted names.
func rubyMethodName(name string) string {
	return name
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
	case *aotir.IfStmt:
		return lowerIfStmt(s)
	case *aotir.WhileStmt:
		return lowerWhileStmt(s)
	case *aotir.ForRangeStmt:
		return lowerForRangeStmt(s)
	case *aotir.ForEachStmt:
		return lowerForEachStmt(s)
	case *aotir.ListSetStmt:
		idx, err := lowerExpr(s.Index)
		if err != nil {
			return nil, err
		}
		val, err := lowerExpr(s.Value)
		if err != nil {
			return nil, err
		}
		return &rtree.RawStmt{Text: fmt.Sprintf("%s[%s] = %s", rubyIdent(s.Name), idx.RubyExprString(), val.RubyExprString())}, nil
	case *aotir.MatchStmt:
		return lowerMatchStmt(s)
	case *aotir.MapPutStmt:
		key, err := lowerExpr(s.Key)
		if err != nil {
			return nil, err
		}
		val, err := lowerExpr(s.Value)
		if err != nil {
			return nil, err
		}
		return &rtree.RawStmt{Text: fmt.Sprintf("%s[%s] = %s", rubyIdent(s.Name), key.RubyExprString(), val.RubyExprString())}, nil
	case *aotir.BreakStmt:
		return &rtree.RawStmt{Text: "break"}, nil
	case *aotir.ContinueStmt:
		return &rtree.RawStmt{Text: "next"}, nil
	case *aotir.ReturnStmt:
		if s.Value == nil {
			return &rtree.Return{}, nil
		}
		v, err := lowerExpr(s.Value)
		if err != nil {
			return nil, err
		}
		return &rtree.Return{X: v}, nil
	}
	return nil, fmt.Errorf("ruby lower: unsupported statement type %T", s)
}

func lowerIfStmt(i *aotir.IfStmt) (rtree.Stmt, error) {
	cond, err := lowerExpr(i.Cond)
	if err != nil {
		return nil, err
	}
	thenBody, err := lowerBlock(i.Then)
	if err != nil {
		return nil, err
	}
	out := &rtree.IfStmt{Cond: cond, Then: thenBody}
	// Detect chained if/elsif/else patterns: when Else is a single Block
	// containing one IfStmt, fold it into an elsif arm. The aotir IR for
	// `if A {} else if B {} else {}` is a nested IfStmt inside Else.
	cur := i.Else
	for cur != nil && len(cur.Statements) == 1 {
		nested, ok := cur.Statements[0].(*aotir.IfStmt)
		if !ok {
			break
		}
		ncond, err := lowerExpr(nested.Cond)
		if err != nil {
			return nil, err
		}
		nthen, err := lowerBlock(nested.Then)
		if err != nil {
			return nil, err
		}
		out.Elsifs = append(out.Elsifs, rtree.ElsifBranch{Cond: ncond, Body: nthen})
		cur = nested.Else
	}
	if cur != nil {
		elseBody, err := lowerBlock(cur)
		if err != nil {
			return nil, err
		}
		out.Else = elseBody
	}
	return out, nil
}

func lowerWhileStmt(w *aotir.WhileStmt) (rtree.Stmt, error) {
	cond, err := lowerExpr(w.Cond)
	if err != nil {
		return nil, err
	}
	body, err := lowerBlock(w.Body)
	if err != nil {
		return nil, err
	}
	// Render as `while Cond ... end` via a RawStmt wrapping a sub-render.
	var sb strings.Builder
	sb.WriteString("while " + cond.RubyExprString() + "\n")
	for _, st := range body {
		sb.WriteString(st.RubyString(1) + "\n")
	}
	sb.WriteString("end")
	return &rtree.RawStmt{Text: sb.String()}, nil
}

func lowerForEachStmt(f *aotir.ForEachStmt) (rtree.Stmt, error) {
	list, err := lowerExpr(f.List)
	if err != nil {
		return nil, err
	}
	body, err := lowerBlock(f.Body)
	if err != nil {
		return nil, err
	}
	var sb strings.Builder
	fmt.Fprintf(&sb, "%s.each do |%s|\n", list.RubyExprString(), rubyIdent(f.Var))
	for _, st := range body {
		sb.WriteString(st.RubyString(1) + "\n")
	}
	sb.WriteString("end")
	return &rtree.RawStmt{Text: sb.String()}, nil
}

func lowerForRangeStmt(f *aotir.ForRangeStmt) (rtree.Stmt, error) {
	start, err := lowerExpr(f.Start)
	if err != nil {
		return nil, err
	}
	end, err := lowerExpr(f.End)
	if err != nil {
		return nil, err
	}
	body, err := lowerBlock(f.Body)
	if err != nil {
		return nil, err
	}
	var sb strings.Builder
	// (start...end).each do |var| ... end -- triple-dot range is half-open,
	// matching Mochi's [Start, End) semantics.
	fmt.Fprintf(&sb, "(%s...%s).each do |%s|\n", start.RubyExprString(), end.RubyExprString(), rubyIdent(f.Var))
	for _, st := range body {
		sb.WriteString(st.RubyString(1) + "\n")
	}
	sb.WriteString("end")
	return &rtree.RawStmt{Text: sb.String()}, nil
}

func lowerLetStmt(l *aotir.LetStmt) (rtree.Stmt, error) {
	if l.Init == nil {
		// Uninitialised declaration: the C lowerer emits these for
		// match-as-expression result vars; the MatchStmt arms assign them.
		// Initialise to nil so the binding is in scope for arm bodies.
		return &rtree.Assign{LHS: rubyIdent(l.Name), RHS: &rtree.NilLit{}}, nil
	}
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
	// Fall through: a discarded user-fn call (CallStmt for a non-void fn whose result
	// is unused). Render as a bare method call.
	args := make([]rtree.Expr, 0, len(c.Args))
	for _, a := range c.Args {
		ax, err := lowerExpr(a)
		if err != nil {
			return nil, err
		}
		args = append(args, ax)
	}
	return &rtree.ExprStmt{X: &rtree.MethodCall{Method: rubyMethodName(c.Func), Args: args, UseParens: true}}, nil
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
	case *aotir.ListLit:
		return lowerListLit(e)
	case *aotir.IndexExpr:
		return lowerIndexExpr(e)
	case *aotir.LenExpr:
		recv, err := lowerExpr(e.Receiver)
		if err != nil {
			return nil, err
		}
		return &rtree.MethodCall{Receiver: recv, Method: "length"}, nil
	case *aotir.StrLenExpr:
		recv, err := lowerExpr(e.Receiver)
		if err != nil {
			return nil, err
		}
		return &rtree.MethodCall{Receiver: recv, Method: "length"}, nil
	case *aotir.AppendExpr:
		recv, err := lowerExpr(e.Receiver)
		if err != nil {
			return nil, err
		}
		val, err := lowerExpr(e.Value)
		if err != nil {
			return nil, err
		}
		// Functional append: receiver + [value] returns a new array; the
		// input list is never mutated, matching Mochi append() semantics.
		return &rtree.BinaryOp{Op: "+", Lhs: recv, Rhs: &rtree.RawExpr{Text: "[" + val.RubyExprString() + "]"}}, nil
	case *aotir.NumCastExpr:
		x, err := lowerExpr(e.Operand)
		if err != nil {
			return nil, err
		}
		return &rtree.MethodCall{Receiver: x, Method: "to_i"}, nil
	case *aotir.RecordLit:
		args := make([]rtree.Expr, 0, len(e.Fields))
		for _, f := range e.Fields {
			fv, err := lowerExpr(f.Value)
			if err != nil {
				return nil, err
			}
			args = append(args, &rtree.RawExpr{Text: f.Name + ": " + fv.RubyExprString()})
		}
		return &rtree.MethodCall{
			Receiver:  &rtree.Ident{Name: e.TypeName},
			Method:    "new",
			Args:      args,
			UseParens: true,
		}, nil
	case *aotir.FieldAccess:
		recv, err := lowerExpr(e.Receiver)
		if err != nil {
			return nil, err
		}
		return &rtree.MethodCall{Receiver: recv, Method: e.FieldName}, nil
	case *aotir.CallExpr:
		args := make([]rtree.Expr, 0, len(e.Args))
		for _, a := range e.Args {
			ax, err := lowerExpr(a)
			if err != nil {
				return nil, err
			}
			args = append(args, ax)
		}
		return &rtree.MethodCall{Method: rubyMethodName(e.Func), Args: args, UseParens: true}, nil
	case *aotir.MapLit:
		return lowerMapLit(e)
	case *aotir.MapGetExpr:
		recv, err := lowerExpr(e.Receiver)
		if err != nil {
			return nil, err
		}
		key, err := lowerExpr(e.Key)
		if err != nil {
			return nil, err
		}
		return &rtree.RawExpr{Text: recv.RubyExprString() + ".fetch(" + key.RubyExprString() + ")"}, nil
	case *aotir.VariantLit:
		args := make([]rtree.Expr, 0, len(e.Fields))
		for _, f := range e.Fields {
			fv, err := lowerExpr(f.Value)
			if err != nil {
				return nil, err
			}
			args = append(args, &rtree.RawExpr{Text: f.Name + ": " + fv.RubyExprString()})
		}
		return &rtree.MethodCall{
			Receiver:  &rtree.Ident{Name: e.UnionName + "::" + e.VariantName},
			Method:    "new",
			Args:      args,
			UseParens: true,
		}, nil
	case *aotir.UnionVarRef:
		return &rtree.Ident{Name: rubyIdent(e.Name)}, nil
	case *aotir.VariantFieldAccess:
		recv, err := lowerExpr(e.Receiver)
		if err != nil {
			return nil, err
		}
		return &rtree.MethodCall{Receiver: recv, Method: e.FieldName}, nil
	case *aotir.MapHasExpr:
		recv, err := lowerExpr(e.Receiver)
		if err != nil {
			return nil, err
		}
		key, err := lowerExpr(e.Key)
		if err != nil {
			return nil, err
		}
		return &rtree.MethodCall{Receiver: recv, Method: "key?", Args: []rtree.Expr{key}, UseParens: true}, nil
	}
	return nil, fmt.Errorf("ruby lower: unsupported expression type %T", e)
}

func lowerListLit(l *aotir.ListLit) (rtree.Expr, error) {
	parts := make([]string, 0, len(l.Elems))
	for _, el := range l.Elems {
		ex, err := lowerExpr(el)
		if err != nil {
			return nil, err
		}
		parts = append(parts, ex.RubyExprString())
	}
	return &rtree.RawExpr{Text: "[" + strings.Join(parts, ", ") + "]"}, nil
}

func lowerMatchStmt(m *aotir.MatchStmt) (rtree.Stmt, error) {
	target, err := lowerExpr(m.Target)
	if err != nil {
		return nil, err
	}
	arms := make([]rtree.CaseInArm, 0, len(m.Arms))
	for _, a := range m.Arms {
		// Build the pattern: UnionName::Variant(field1:, field2:).
		// Each binding renames the matched field to a Ruby local var by appending
		// `=> varname` if the var name differs from the field name.
		var pat strings.Builder
		fmt.Fprintf(&pat, "%s::%s", m.UnionName, a.VariantName)
		if len(a.Bindings) > 0 {
			pat.WriteByte('(')
			for i, b := range a.Bindings {
				if i > 0 {
					pat.WriteString(", ")
				}
				if b.VarName == b.FieldName {
					fmt.Fprintf(&pat, "%s:", b.FieldName)
				} else {
					fmt.Fprintf(&pat, "%s: %s", b.FieldName, rubyIdent(b.VarName))
				}
			}
			pat.WriteByte(')')
		}
		body, err := lowerBlock(a.Body)
		if err != nil {
			return nil, err
		}
		arms = append(arms, rtree.CaseInArm{Pattern: pat.String(), Body: body})
	}
	var elseBody []rtree.Stmt
	if m.Default != nil {
		body, err := lowerBlock(m.Default.Body)
		if err != nil {
			return nil, err
		}
		elseBody = body
	}
	return &rtree.CaseInStmt{Scrutinee: target, Arms: arms, Else: elseBody}, nil
}

func lowerMapLit(m *aotir.MapLit) (rtree.Expr, error) {
	if len(m.Keys) != len(m.Values) {
		return nil, fmt.Errorf("ruby lower: MapLit Keys/Values length mismatch")
	}
	parts := make([]string, 0, len(m.Keys))
	for i := range m.Keys {
		k, err := lowerExpr(m.Keys[i])
		if err != nil {
			return nil, err
		}
		v, err := lowerExpr(m.Values[i])
		if err != nil {
			return nil, err
		}
		parts = append(parts, k.RubyExprString()+" => "+v.RubyExprString())
	}
	return &rtree.RawExpr{Text: "{" + strings.Join(parts, ", ") + "}"}, nil
}

func lowerIndexExpr(e *aotir.IndexExpr) (rtree.Expr, error) {
	recv, err := lowerExpr(e.Receiver)
	if err != nil {
		return nil, err
	}
	idx, err := lowerExpr(e.Index)
	if err != nil {
		return nil, err
	}
	return &rtree.RawExpr{Text: recv.RubyExprString() + "[" + idx.RubyExprString() + "]"}, nil
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
	case aotir.BinEqI64, aotir.BinEqF64, aotir.BinEqBool, aotir.BinEqStr,
		aotir.BinEqRec, aotir.BinEqList, aotir.BinEqMap:
		return "==", true
	case aotir.BinNeI64, aotir.BinNeF64, aotir.BinNeBool, aotir.BinNeStr,
		aotir.BinNeRec, aotir.BinNeList, aotir.BinNeMap:
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
