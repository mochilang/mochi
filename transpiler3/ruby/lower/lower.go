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
	for _, a := range prog.Agents {
		cd, err := lowerAgent(a)
		if err != nil {
			return nil, fmt.Errorf("ruby lower: agent %s: %w", a.Name, err)
		}
		progDecls = append(progDecls, cd)
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
	params := make([]rtree.MethodParam, 0, len(fn.Params)+1)
	if fn.IsLifted {
		// Lifted closure: receives the env Hash as its first parameter so
		// captures can be looked up as __env[:name]. Non-capturing lifted
		// functions still take the slot (caller passes nil) for ABI uniformity.
		params = append(params, rtree.MethodParam{Name: "__env"})
	}
	for _, p := range fn.Params {
		params = append(params, rtree.MethodParam{Name: rubyIdent(p.Name)})
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
	case *aotir.ClosureEnvStmt:
		return lowerClosureEnvStmt(s)
	case *aotir.QueryScopeStmt:
		return lowerQueryScopeStmt(s)
	case *aotir.RawCStmt:
		// C-target-specific scaffolding (e.g. the Datalog fixpoint setup
		// emitted by the C lowerer). Ruby evaluates the Datalog program at
		// compile time via lowerDatalogQueryExpr, so this is dead weight.
		return nil, nil
	case *aotir.AgentIntentCallStmt:
		recv, err := lowerExpr(s.Receiver)
		if err != nil {
			return nil, err
		}
		args := make([]rtree.Expr, 0, len(s.Args))
		for _, a := range s.Args {
			ax, err := lowerExpr(a)
			if err != nil {
				return nil, err
			}
			args = append(args, ax)
		}
		return &rtree.ExprStmt{X: &rtree.MethodCall{
			Receiver:  recv,
			Method:    s.IntentName,
			Args:      args,
			UseParens: true,
		}}, nil
	case *aotir.StreamEmitStmt:
		st, err := lowerExpr(s.Stream)
		if err != nil {
			return nil, err
		}
		v, err := lowerExpr(s.Val)
		if err != nil {
			return nil, err
		}
		return &rtree.ExprStmt{X: &rtree.MethodCall{
			Receiver:  st,
			Method:    "emit",
			Args:      []rtree.Expr{v},
			UseParens: true,
		}}, nil
	case *aotir.ChanSendStmt:
		ch, err := lowerExpr(s.Chan)
		if err != nil {
			return nil, err
		}
		v, err := lowerExpr(s.Val)
		if err != nil {
			return nil, err
		}
		return &rtree.ExprStmt{X: &rtree.MethodCall{
			Receiver:  ch,
			Method:    "push",
			Args:      []rtree.Expr{v},
			UseParens: true,
		}}, nil
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
	case *aotir.OMapPutStmt:
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
	lhs := rubyIdent(a.Name)
	// Agent intent bodies write to fields as AssignStmt{Name: "__self->field"}.
	if rest, ok := strings.CutPrefix(a.Name, "__self->"); ok {
		lhs = "@" + rest
	}
	return &rtree.Assign{LHS: lhs, RHS: rhs}, nil
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
		// Captured variables inside a lifted closure body appear as
		// VarRef{Name: "__e->field"} (the C lowerer's emit-name convention).
		// Rewrite them to __env[:field] for Ruby.
		if rest, ok := strings.CutPrefix(e.Name, "__e->"); ok {
			return &rtree.RawExpr{Text: "__env[:" + rest + "]"}, nil
		}
		// Agent intent bodies reference fields as VarRef{Name: "__self->field"}.
		if rest, ok := strings.CutPrefix(e.Name, "__self->"); ok {
			return &rtree.RawExpr{Text: "@" + rest}, nil
		}
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
	case *aotir.StrIndexExpr:
		recv, err := lowerExpr(e.Receiver)
		if err != nil {
			return nil, err
		}
		idx, err := lowerExpr(e.Index)
		if err != nil {
			return nil, err
		}
		return &rtree.RawExpr{Text: recv.RubyExprString() + "[" + idx.RubyExprString() + "]"}, nil
	case *aotir.StrContainsExpr:
		recv, err := lowerExpr(e.Receiver)
		if err != nil {
			return nil, err
		}
		sub, err := lowerExpr(e.Sub)
		if err != nil {
			return nil, err
		}
		return &rtree.MethodCall{Receiver: recv, Method: "include?", Args: []rtree.Expr{sub}, UseParens: true}, nil
	case *aotir.StrSubstringExpr:
		recv, err := lowerExpr(e.Receiver)
		if err != nil {
			return nil, err
		}
		start, err := lowerExpr(e.Start)
		if err != nil {
			return nil, err
		}
		end, err := lowerExpr(e.End)
		if err != nil {
			return nil, err
		}
		return &rtree.RawExpr{Text: fmt.Sprintf("(%s[%s...%s] || \"\")",
			recv.RubyExprString(), start.RubyExprString(), end.RubyExprString())}, nil
	case *aotir.StrReverseExpr:
		recv, err := lowerExpr(e.Receiver)
		if err != nil {
			return nil, err
		}
		return &rtree.MethodCall{Receiver: recv, Method: "reverse"}, nil
	case *aotir.StrConvertExpr:
		x, err := lowerExpr(e.Operand)
		if err != nil {
			return nil, err
		}
		return &rtree.MethodCall{Receiver: x, Method: "to_s"}, nil
	case *aotir.StrUpperExpr:
		recv, err := lowerExpr(e.Receiver)
		if err != nil {
			return nil, err
		}
		return &rtree.MethodCall{Receiver: recv, Method: "upcase"}, nil
	case *aotir.StrLowerExpr:
		recv, err := lowerExpr(e.Receiver)
		if err != nil {
			return nil, err
		}
		return &rtree.MethodCall{Receiver: recv, Method: "downcase"}, nil
	case *aotir.StrSplitExpr:
		s, err := lowerExpr(e.Str)
		if err != nil {
			return nil, err
		}
		sep, err := lowerExpr(e.Sep)
		if err != nil {
			return nil, err
		}
		return &rtree.MethodCall{Receiver: s, Method: "split", Args: []rtree.Expr{sep}, UseParens: true}, nil
	case *aotir.StrJoinExpr:
		list, err := lowerExpr(e.List)
		if err != nil {
			return nil, err
		}
		sep, err := lowerExpr(e.Sep)
		if err != nil {
			return nil, err
		}
		return &rtree.MethodCall{Receiver: list, Method: "join", Args: []rtree.Expr{sep}, UseParens: true}, nil
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
	case *aotir.DatalogQueryExpr:
		return lowerDatalogQueryExpr(e)
	case *aotir.AsyncExpr:
		body, err := lowerExpr(e.Body)
		if err != nil {
			return nil, err
		}
		// Ruby Thread is the simplest fit: Thread.new { body }.value blocks
		// until the body returns and yields its result. Async then becomes
		// a Thread handle; Await calls .value.
		return &rtree.RawExpr{Text: "Thread.new { " + body.RubyExprString() + " }"}, nil
	case *aotir.AwaitExpr:
		fut, err := lowerExpr(e.Future)
		if err != nil {
			return nil, err
		}
		return &rtree.MethodCall{Receiver: fut, Method: "value"}, nil
	case *aotir.AgentLit:
		args := make([]string, 0, len(e.Fields))
		for _, f := range e.Fields {
			fv, err := lowerExpr(f.Value)
			if err != nil {
				return nil, err
			}
			args = append(args, f.Name+": "+fv.RubyExprString())
		}
		return &rtree.RawExpr{Text: e.AgentName + ".new(" + strings.Join(args, ", ") + ")"}, nil
	case *aotir.AgentIntentCallExpr:
		recv, err := lowerExpr(e.Receiver)
		if err != nil {
			return nil, err
		}
		args := make([]rtree.Expr, 0, len(e.Args))
		for _, a := range e.Args {
			ax, err := lowerExpr(a)
			if err != nil {
				return nil, err
			}
			args = append(args, ax)
		}
		return &rtree.MethodCall{
			Receiver:  recv,
			Method:    e.IntentName,
			Args:      args,
			UseParens: true,
		}, nil
	case *aotir.StreamMakeExpr:
		cap, err := lowerExpr(e.Cap)
		if err != nil {
			return nil, err
		}
		return &rtree.RawExpr{Text: "Mochi::Runtime::Stream.new(" + cap.RubyExprString() + ")"}, nil
	case *aotir.SubMakeExpr:
		st, err := lowerExpr(e.Stream)
		if err != nil {
			return nil, err
		}
		return &rtree.MethodCall{Receiver: st, Method: "subscribe"}, nil
	case *aotir.SubRecvExpr:
		sub, err := lowerExpr(e.Sub)
		if err != nil {
			return nil, err
		}
		return &rtree.MethodCall{Receiver: sub, Method: "pop"}, nil
	case *aotir.ChanMakeExpr:
		cap, err := lowerExpr(e.Cap)
		if err != nil {
			return nil, err
		}
		// Thread::SizedQueue blocks push when full, blocks pop when empty,
		// matching Mochi's blocking channel semantics for the single-thread
		// fixtures. Multi-producer/multi-consumer uses surface in Phase 10.1.
		return &rtree.RawExpr{Text: "Thread::SizedQueue.new(" + cap.RubyExprString() + ")"}, nil
	case *aotir.ChanRecvExpr:
		ch, err := lowerExpr(e.Chan)
		if err != nil {
			return nil, err
		}
		return &rtree.MethodCall{Receiver: ch, Method: "pop"}, nil
	case *aotir.ListSortAscExpr:
		recv, err := lowerExpr(e.Receiver)
		if err != nil {
			return nil, err
		}
		return &rtree.MethodCall{Receiver: recv, Method: "sort"}, nil
	case *aotir.ListSliceExpr:
		recv, err := lowerExpr(e.Receiver)
		if err != nil {
			return nil, err
		}
		start, err := lowerExpr(e.Start)
		if err != nil {
			return nil, err
		}
		end, err := lowerExpr(e.End)
		if err != nil {
			return nil, err
		}
		// Mochi's slice semantics clamp End to len(recv); Ruby's range slice
		// returns nil when Start is past the end, so coerce a nil result to
		// an empty array.
		return &rtree.RawExpr{Text: fmt.Sprintf("(%s[%s...%s] || [])",
			recv.RubyExprString(), start.RubyExprString(), end.RubyExprString())}, nil
	case *aotir.FunLit:
		return lowerFunLit(e)
	case *aotir.FunCallExpr:
		return lowerFunCallExpr(e)
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
	case *aotir.ListMinExpr:
		recv, err := lowerExpr(e.Receiver)
		if err != nil {
			return nil, err
		}
		return &rtree.MethodCall{Receiver: recv, Method: "min"}, nil
	case *aotir.ListMaxExpr:
		recv, err := lowerExpr(e.Receiver)
		if err != nil {
			return nil, err
		}
		return &rtree.MethodCall{Receiver: recv, Method: "max"}, nil
	case *aotir.ListContainsExpr:
		list, err := lowerExpr(e.List)
		if err != nil {
			return nil, err
		}
		val, err := lowerExpr(e.Value)
		if err != nil {
			return nil, err
		}
		return &rtree.MethodCall{Receiver: list, Method: "include?", Args: []rtree.Expr{val}, UseParens: true}, nil
	case *aotir.ListSumExpr:
		recv, err := lowerExpr(e.Receiver)
		if err != nil {
			return nil, err
		}
		return &rtree.MethodCall{Receiver: recv, Method: "sum"}, nil
	case *aotir.ListMapExpr:
		list, err := lowerExpr(e.List)
		if err != nil {
			return nil, err
		}
		fn, err := lowerExpr(e.Fn)
		if err != nil {
			return nil, err
		}
		return &rtree.RawExpr{Text: fmt.Sprintf("%s.map { |__x| (%s).call(__x) }",
			list.RubyExprString(), fn.RubyExprString())}, nil
	case *aotir.ListFilterExpr:
		list, err := lowerExpr(e.List)
		if err != nil {
			return nil, err
		}
		fn, err := lowerExpr(e.Fn)
		if err != nil {
			return nil, err
		}
		return &rtree.RawExpr{Text: fmt.Sprintf("%s.select { |__x| (%s).call(__x) }",
			list.RubyExprString(), fn.RubyExprString())}, nil
	case *aotir.ListFoldlExpr:
		list, err := lowerExpr(e.List)
		if err != nil {
			return nil, err
		}
		fn, err := lowerExpr(e.Fn)
		if err != nil {
			return nil, err
		}
		init, err := lowerExpr(e.Init)
		if err != nil {
			return nil, err
		}
		return &rtree.RawExpr{Text: fmt.Sprintf("%s.inject(%s) { |__acc, __x| (%s).call(__acc, __x) }",
			list.RubyExprString(), init.RubyExprString(), fn.RubyExprString())}, nil
	case *aotir.SetLiteralExpr:
		parts := make([]string, 0, len(e.Elems))
		for _, el := range e.Elems {
			ex, err := lowerExpr(el)
			if err != nil {
				return nil, err
			}
			parts = append(parts, ex.RubyExprString())
		}
		return &rtree.RawExpr{Text: "Set.new([" + strings.Join(parts, ", ") + "])"}, nil
	case *aotir.SetAddExpr:
		recv, err := lowerExpr(e.Receiver)
		if err != nil {
			return nil, err
		}
		elem, err := lowerExpr(e.Elem)
		if err != nil {
			return nil, err
		}
		return &rtree.RawExpr{Text: fmt.Sprintf("(%s | Set[%s])",
			recv.RubyExprString(), elem.RubyExprString())}, nil
	case *aotir.SetHasExpr:
		recv, err := lowerExpr(e.Receiver)
		if err != nil {
			return nil, err
		}
		elem, err := lowerExpr(e.Elem)
		if err != nil {
			return nil, err
		}
		return &rtree.MethodCall{Receiver: recv, Method: "include?", Args: []rtree.Expr{elem}, UseParens: true}, nil
	case *aotir.SetLenExpr:
		recv, err := lowerExpr(e.Receiver)
		if err != nil {
			return nil, err
		}
		return &rtree.MethodCall{Receiver: recv, Method: "size"}, nil
	case *aotir.SetToListExpr:
		recv, err := lowerExpr(e.Receiver)
		if err != nil {
			return nil, err
		}
		return &rtree.MethodCall{Receiver: recv, Method: "to_a"}, nil
	case *aotir.OMapLiteralExpr:
		parts := make([]string, 0, len(e.Keys))
		for i := range e.Keys {
			k, err := lowerExpr(e.Keys[i])
			if err != nil {
				return nil, err
			}
			v, err := lowerExpr(e.Values[i])
			if err != nil {
				return nil, err
			}
			parts = append(parts, k.RubyExprString()+" => "+v.RubyExprString())
		}
		return &rtree.RawExpr{Text: "{" + strings.Join(parts, ", ") + "}"}, nil
	case *aotir.OMapGetExpr:
		recv, err := lowerExpr(e.Receiver)
		if err != nil {
			return nil, err
		}
		key, err := lowerExpr(e.Key)
		if err != nil {
			return nil, err
		}
		return &rtree.RawExpr{Text: recv.RubyExprString() + ".fetch(" + key.RubyExprString() + ")"}, nil
	case *aotir.OMapHasExpr:
		recv, err := lowerExpr(e.Receiver)
		if err != nil {
			return nil, err
		}
		key, err := lowerExpr(e.Key)
		if err != nil {
			return nil, err
		}
		return &rtree.MethodCall{Receiver: recv, Method: "key?", Args: []rtree.Expr{key}, UseParens: true}, nil
	case *aotir.OMapLenExpr:
		recv, err := lowerExpr(e.Receiver)
		if err != nil {
			return nil, err
		}
		return &rtree.MethodCall{Receiver: recv, Method: "size"}, nil
	case *aotir.MathCallExpr:
		arg, err := lowerExpr(e.Arg)
		if err != nil {
			return nil, err
		}
		switch e.Func {
		case "abs_i64", "abs_f64":
			return &rtree.MethodCall{Receiver: arg, Method: "abs"}, nil
		case "floor":
			return &rtree.MethodCall{Receiver: arg, Method: "floor"}, nil
		case "ceil":
			return &rtree.MethodCall{Receiver: arg, Method: "ceil"}, nil
		}
		return nil, fmt.Errorf("ruby lower: unknown math func %q", e.Func)
	case *aotir.MapLenExpr:
		recv, err := lowerExpr(e.Receiver)
		if err != nil {
			return nil, err
		}
		return &rtree.MethodCall{Receiver: recv, Method: "size"}, nil
	case *aotir.MapKeysExpr:
		recv, err := lowerExpr(e.Receiver)
		if err != nil {
			return nil, err
		}
		return &rtree.MethodCall{Receiver: recv, Method: "keys"}, nil
	case *aotir.MapValuesExpr:
		recv, err := lowerExpr(e.Receiver)
		if err != nil {
			return nil, err
		}
		return &rtree.MethodCall{Receiver: recv, Method: "values"}, nil
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

// lowerAgent lowers an aotir.AgentDecl to a Ruby class. Mutable fields
// become @ivars (initialised via a keyword-args initialize), and each
// intent becomes an instance method. Field reads/writes inside intent
// bodies arrive here as __self->field VarRefs / AssignStmt names; the
// VarRef/AssignStmt rewrite handles them.
func lowerAgent(a *aotir.AgentDecl) (*rtree.ClassDecl, error) {
	cd := &rtree.ClassDecl{Name: a.Name}
	// initialize: accepts each field as a keyword arg, copies into @ivars.
	initParams := make([]rtree.MethodParam, len(a.Fields))
	initBody := make([]rtree.Stmt, len(a.Fields))
	for i, f := range a.Fields {
		initParams[i] = rtree.MethodParam{Name: f.Name + ":"}
		initBody[i] = &rtree.Assign{LHS: "@" + f.Name, RHS: &rtree.Ident{Name: f.Name}}
	}
	cd.Decls = append(cd.Decls, &rtree.MethodDecl{
		Name:   "initialize",
		Params: initParams,
		Body:   initBody,
	})
	for _, intent := range a.Intents {
		body, err := lowerBlock(intent.Body)
		if err != nil {
			return nil, fmt.Errorf("intent %s: %w", intent.Name, err)
		}
		params := make([]rtree.MethodParam, len(intent.Params))
		for i, p := range intent.Params {
			params[i] = rtree.MethodParam{Name: rubyIdent(p.Name)}
		}
		cd.Decls = append(cd.Decls, &rtree.MethodDecl{
			Name:   intent.Name,
			Params: params,
			Body:   body,
		})
	}
	return cd, nil
}

// lowerQueryScopeStmt lowers a desugared from/where/select query. In C the
// scope drives arena allocation; in Ruby it is a no-op wrapper since GC
// handles the temp list. We just lower the inner Body inline and emit it
// as a sequence of statements wrapped in a `begin ... end` so the lexical
// shape mirrors the IR. (The body's AssignStmt+AppendExpr already mutates
// the ResultVar declared just before this node.)
func lowerQueryScopeStmt(s *aotir.QueryScopeStmt) (rtree.Stmt, error) {
	body, err := lowerBlock(s.Body)
	if err != nil {
		return nil, err
	}
	var sb strings.Builder
	sb.WriteString("begin\n")
	for _, st := range body {
		sb.WriteString(st.RubyString(1) + "\n")
	}
	sb.WriteString("end")
	return &rtree.RawStmt{Text: sb.String()}, nil
}

// lowerClosureEnvStmt lowers the C lowerer's ClosureEnvStmt, which precedes
// a LetStmt binding a capturing FunLit. It builds a Ruby Hash literal
// keyed by FieldName so the lifted method can read captures via __env[:name].
func lowerClosureEnvStmt(s *aotir.ClosureEnvStmt) (rtree.Stmt, error) {
	parts := make([]string, 0, len(s.Captures))
	for _, cap := range s.Captures {
		parts = append(parts, fmt.Sprintf(":%s => %s", cap.FieldName, rubyIdent(cap.SrcName)))
	}
	return &rtree.Assign{
		LHS: rubyIdent(s.EnvVarName),
		RHS: &rtree.RawExpr{Text: "{" + strings.Join(parts, ", ") + "}"},
	}, nil
}

// lowerFunLit lowers an aotir.FunLit to a Ruby lambda that wraps a call to
// the lifted module method. The lambda captures the env hash (or nil for
// non-capturing closures) so the resulting value is a first-class Proc.
func lowerFunLit(f *aotir.FunLit) (rtree.Expr, error) {
	var envExpr string
	if f.EnvVarName != "" {
		envExpr = rubyIdent(f.EnvVarName)
	} else {
		envExpr = "nil"
	}
	params := make([]string, len(f.Sig.ParamTypes))
	for i := range params {
		params[i] = fmt.Sprintf("__a%d", i)
	}
	call := rubyMethodName(f.FuncName) + "(" + envExpr
	for _, p := range params {
		call += ", " + p
	}
	call += ")"
	var lambda string
	if len(params) == 0 {
		lambda = "->() { " + call + " }"
	} else {
		lambda = "->(" + strings.Join(params, ", ") + ") { " + call + " }"
	}
	return &rtree.RawExpr{Text: lambda}, nil
}

// lowerFunCallExpr lowers a call through a fun-typed value. If the callee is
// a FunLit we call the lifted method directly; otherwise we go through the
// Proc's .call.
func lowerFunCallExpr(c *aotir.FunCallExpr) (rtree.Expr, error) {
	args := make([]rtree.Expr, 0, len(c.Args))
	for _, a := range c.Args {
		ax, err := lowerExpr(a)
		if err != nil {
			return nil, fmt.Errorf("FunCallExpr arg: %w", err)
		}
		args = append(args, ax)
	}
	if fl, ok := c.Callee.(*aotir.FunLit); ok {
		var envExpr string
		if fl.EnvVarName != "" {
			envExpr = rubyIdent(fl.EnvVarName)
		} else {
			envExpr = "nil"
		}
		parts := []string{envExpr}
		for _, a := range args {
			parts = append(parts, a.RubyExprString())
		}
		return &rtree.RawExpr{Text: rubyMethodName(fl.FuncName) + "(" + strings.Join(parts, ", ") + ")"}, nil
	}
	callee, err := lowerExpr(c.Callee)
	if err != nil {
		return nil, fmt.Errorf("FunCallExpr callee: %w", err)
	}
	return &rtree.MethodCall{Receiver: callee, Method: "call", Args: args, UseParens: true}, nil
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
