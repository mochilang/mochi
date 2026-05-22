package lower

import (
	"fmt"
	"math"
	"strings"

	"mochi/parser"
	"mochi/transpiler3/c/aotir"
)

// Lower turns a type-checked parser.Program into an aotir.Program.
//
// Accepted shape (Phase 2.2):
//
//   - Top-level: a mix of `fun NAME(<params>): T { ... }` declarations
//     and the same script statements as Phase 2.1. The fun decls are
//     hoisted to their own aotir.Function entries; the remaining
//     statements lower into main().
//
//   - Statements inside main and inside any user fun:
//   - `print(<expr>)` (statement form)
//   - `<fn>(<args>)` (discard-result call statement)
//   - `let NAME = <expr>` / `let NAME: T = <expr>`
//   - `var NAME = <expr>` / `var NAME: T = <expr>`
//   - `NAME = <expr>` (assign to an existing var binding)
//   - `if <cond> { ... } [else if ... | else { ... }]`
//   - `while <cond> { ... }`
//   - `for x in <start>..<end> { ... }` (int half-open range)
//   - `break`, `continue`
//   - `return <expr>?`
//
//   - Expressions extend Phase 2.1 with calls to user functions; the
//     callee must resolve against a top-level fun decl.
//
// Anything outside this set is rejected with an explicit
// phase-named diagnostic so the gate fails loudly if upstream
// broadens the surface without us noticing.
func Lower(prog *parser.Program) (*aotir.Program, error) {
	if prog == nil {
		return nil, fmt.Errorf("transpiler3/c/lower: nil program")
	}

	// Pass 1: collect every user-defined fun decl and record its
	// signature so the body lowering can resolve forward and
	// mutual references.
	funcs := map[string]*funcSig{}
	var funDecls []*parser.FunStmt
	for i, st := range prog.Statements {
		if st == nil || st.Fun == nil {
			continue
		}
		fn := st.Fun
		if fn.Name == "" {
			return nil, fmt.Errorf("transpiler3/c/lower: statement %d: fun with empty name", i)
		}
		if fn.Name == "main" {
			return nil, fmt.Errorf("transpiler3/c/lower: statement %d: user fun cannot be named main", i)
		}
		if _, dup := funcs[fn.Name]; dup {
			return nil, fmt.Errorf("transpiler3/c/lower: statement %d: redeclaration of fun %q", i, fn.Name)
		}
		sig, err := buildFuncSig(fn)
		if err != nil {
			return nil, fmt.Errorf("transpiler3/c/lower: fun %q: %w", fn.Name, err)
		}
		funcs[fn.Name] = sig
		funDecls = append(funDecls, fn)
	}

	// Pass 2a: lower each fun body using the shared funcs table.
	out := &aotir.Program{}
	for _, fn := range funDecls {
		sig := funcs[fn.Name]
		l := &lowerer{
			funcs:           funcs,
			scope:           newLScope(nil),
			currentFnReturn: sig.returnType,
		}
		// Seed parameters into the function scope as immutable.
		for _, p := range sig.params {
			l.scope.vars[p.Name] = lbinding{t: p.Type, mutable: false}
		}
		body := &aotir.Block{}
		for i, st := range fn.Body {
			if st == nil {
				return nil, fmt.Errorf("transpiler3/c/lower: fun %q stmt %d is nil", fn.Name, i)
			}
			if err := l.lowerStatement(body, st); err != nil {
				return nil, fmt.Errorf("transpiler3/c/lower: fun %q stmt %d: %w", fn.Name, i, err)
			}
		}
		out.Functions = append(out.Functions, &aotir.Function{
			Name:       fn.Name,
			Params:     sig.params,
			ReturnType: sig.returnType,
			Body:       body,
		})
	}

	// Pass 2b: lower the top-level script (everything that is not
	// a fun decl) into main.
	mainBody := &aotir.Block{}
	mainL := &lowerer{
		funcs:           funcs,
		scope:           newLScope(nil),
		currentFnReturn: aotir.TypeUnit,
	}
	for i, st := range prog.Statements {
		if st == nil {
			return nil, fmt.Errorf("transpiler3/c/lower: statement %d is nil", i)
		}
		if st.Fun != nil {
			continue
		}
		if err := mainL.lowerStatement(mainBody, st); err != nil {
			return nil, fmt.Errorf("transpiler3/c/lower: statement %d: %w", i, err)
		}
	}
	mainFn := &aotir.Function{
		Name:       "main",
		ReturnType: aotir.TypeUnit,
		Body:       mainBody,
	}
	out.Functions = append(out.Functions, mainFn)
	out.Main = len(out.Functions) - 1

	if err := aotir.Verify(out); err != nil {
		return nil, fmt.Errorf("transpiler3/c/lower: verify: %w", err)
	}
	return out, nil
}

// funcSig is the lower-time projection of an aotir.Function signature
// (no body); the lowerer needs it to resolve user-fn calls during
// expression lowering.
type funcSig struct {
	params     []aotir.Param
	returnType aotir.Type
}

// buildFuncSig turns a parser.FunStmt into its lower-time signature.
// Both parameter types and return type are required; Mochi accepts
// `fun f(x) { ... }` as inferring from caller context, but Phase 2.2
// requires explicit annotations so the C-AOT monomorpher does not
// have to do inference.
func buildFuncSig(fn *parser.FunStmt) (*funcSig, error) {
	if fn.Return == nil {
		return nil, fmt.Errorf("fun %q requires an explicit `: T` return type in Phase 2.2", fn.Name)
	}
	ret, err := typeFromRef(fn.Return)
	if err != nil {
		return nil, fmt.Errorf("fun %q return: %w", fn.Name, err)
	}
	if len(fn.TypeParams) != 0 {
		return nil, fmt.Errorf("fun %q is generic; type parameters land with Phase 3", fn.Name)
	}
	if len(fn.Effects) != 0 {
		return nil, fmt.Errorf("fun %q has effects; effect annotations land in a later phase", fn.Name)
	}
	params := make([]aotir.Param, 0, len(fn.Params))
	seen := map[string]bool{}
	for i, p := range fn.Params {
		if p.Name == "" {
			return nil, fmt.Errorf("fun %q param %d has empty name", fn.Name, i)
		}
		if seen[p.Name] {
			return nil, fmt.Errorf("fun %q duplicate parameter %q", fn.Name, p.Name)
		}
		seen[p.Name] = true
		if p.Type == nil {
			return nil, fmt.Errorf("fun %q param %q requires an explicit `: T` type in Phase 2.2", fn.Name, p.Name)
		}
		t, err := typeFromRef(p.Type)
		if err != nil {
			return nil, fmt.Errorf("fun %q param %q: %w", fn.Name, p.Name, err)
		}
		params = append(params, aotir.Param{Name: p.Name, Type: t})
	}
	return &funcSig{params: params, returnType: ret}, nil
}

// lowerer carries the per-function scope stack, loop-depth counter,
// and the enclosing function's return type. Mirrors the verifier's
// verifyCtx so the same scoping / typing rules apply at lower time.
type lowerer struct {
	funcs           map[string]*funcSig
	scope           *lscope
	loopDepth       int
	currentFnReturn aotir.Type
}

// lscope mirrors aotir's scope: lexical frame with parent chain.
type lscope struct {
	parent *lscope
	vars   map[string]lbinding
}

type lbinding struct {
	t       aotir.Type
	mutable bool
}

func newLScope(parent *lscope) *lscope {
	return &lscope{parent: parent, vars: map[string]lbinding{}}
}

func (s *lscope) lookup(name string) (lbinding, bool) {
	for s != nil {
		if b, ok := s.vars[name]; ok {
			return b, true
		}
		s = s.parent
	}
	return lbinding{}, false
}

// lowerStatement dispatches on the parser Statement variant.
func (l *lowerer) lowerStatement(out *aotir.Block, st *parser.Statement) error {
	switch {
	case st.Expr != nil:
		return l.lowerExprStmt(out, st.Expr)
	case st.Let != nil:
		return l.lowerLet(out, st.Let)
	case st.Var != nil:
		return l.lowerVar(out, st.Var)
	case st.Assign != nil:
		return l.lowerAssign(out, st.Assign)
	case st.If != nil:
		return l.lowerIf(out, st.If)
	case st.While != nil:
		return l.lowerWhile(out, st.While)
	case st.For != nil:
		return l.lowerFor(out, st.For)
	case st.Break != nil:
		if l.loopDepth == 0 {
			return fmt.Errorf("break outside a loop")
		}
		out.Statements = append(out.Statements, &aotir.BreakStmt{})
		return nil
	case st.Continue != nil:
		if l.loopDepth == 0 {
			return fmt.Errorf("continue outside a loop")
		}
		out.Statements = append(out.Statements, &aotir.ContinueStmt{})
		return nil
	case st.Return != nil:
		return l.lowerReturn(out, st.Return)
	case st.Fun != nil:
		return fmt.Errorf("nested `fun` declarations are not supported in Phase 2.2")
	case st.Type != nil:
		return fmt.Errorf("`type` declarations land in Phase 3")
	}
	return fmt.Errorf("unsupported statement in Phase 2.2")
}

// lowerExprStmt handles a top-level expression statement. Phase 2.2
// accepts `print(<expr>)` and a discard-result call to a user fn.
// Anything else (a bare arithmetic expression, a bare variable
// reference) is rejected -- the result has nowhere to go.
func (l *lowerer) lowerExprStmt(out *aotir.Block, es *parser.ExprStmt) error {
	call, err := matchBareCall(es.Expr)
	if err != nil {
		return err
	}
	if call.Func == "print" {
		return l.lowerPrintCall(out, call)
	}
	sig, ok := l.funcs[call.Func]
	if !ok {
		return fmt.Errorf("unresolved callee %q at statement position", call.Func)
	}
	args, err := l.lowerCallArgs(call, sig)
	if err != nil {
		return err
	}
	_ = sig.returnType // discarded
	out.Statements = append(out.Statements, &aotir.CallStmt{
		Func: call.Func,
		Args: args,
	})
	return nil
}

// lowerPrintCall handles `print(<expr>)`. The single-arg restriction
// is Phase 2 -- Phase 3 widens print() to mirror vm3's variadic form.
func (l *lowerer) lowerPrintCall(out *aotir.Block, call *parser.CallExpr) error {
	if len(call.Args) != 1 {
		return fmt.Errorf("print() takes exactly one argument, got %d", len(call.Args))
	}
	arg, err := l.lowerExpr(call.Args[0])
	if err != nil {
		return err
	}
	callee, err := printCalleeFor(arg.Type())
	if err != nil {
		return err
	}
	out.Statements = append(out.Statements, &aotir.CallStmt{
		Func: callee,
		Args: []aotir.Expr{arg},
	})
	return nil
}

// lowerCallArgs lowers each argument expression and cross-checks
// the argument type against the resolved callee parameter list.
func (l *lowerer) lowerCallArgs(call *parser.CallExpr, sig *funcSig) ([]aotir.Expr, error) {
	if len(call.Args) != len(sig.params) {
		return nil, fmt.Errorf("call %q expects %d args, got %d", call.Func, len(sig.params), len(call.Args))
	}
	out := make([]aotir.Expr, 0, len(call.Args))
	for i, a := range call.Args {
		expr, err := l.lowerExpr(a)
		if err != nil {
			return nil, fmt.Errorf("call %q arg %d: %w", call.Func, i, err)
		}
		if expr.Type() != sig.params[i].Type {
			return nil, fmt.Errorf("call %q arg %d: expected %s, got %s",
				call.Func, i, sig.params[i].Type, expr.Type())
		}
		out = append(out, expr)
	}
	return out, nil
}

// lowerLet lowers an immutable binding. If the declared type is
// omitted, it is inferred from the init expression.
func (l *lowerer) lowerLet(out *aotir.Block, ls *parser.LetStmt) error {
	return l.lowerBinding(out, ls.Name, ls.Type, ls.Value, false)
}

// lowerVar lowers a mutable binding.
func (l *lowerer) lowerVar(out *aotir.Block, vs *parser.VarStmt) error {
	return l.lowerBinding(out, vs.Name, vs.Type, vs.Value, true)
}

// lowerBinding is the shared path for let/var: typecheck the init
// against an optional type annotation, register the binding in the
// current scope, and emit a LetStmt.
func (l *lowerer) lowerBinding(out *aotir.Block, name string, declared *parser.TypeRef, init *parser.Expr, mutable bool) error {
	if name == "" {
		return fmt.Errorf("binding with empty name")
	}
	if init == nil {
		return fmt.Errorf("binding %q requires an initializer in Phase 2.1", name)
	}
	if _, dup := l.scope.vars[name]; dup {
		return fmt.Errorf("redeclaration of %q in same scope", name)
	}
	value, err := l.lowerExpr(init)
	if err != nil {
		return fmt.Errorf("binding %q init: %w", name, err)
	}
	declType := value.Type()
	if declared != nil {
		t, err := typeFromRef(declared)
		if err != nil {
			return fmt.Errorf("binding %q type: %w", name, err)
		}
		if t != declType {
			return fmt.Errorf("binding %q: declared %s, init produces %s", name, t, declType)
		}
		declType = t
	}
	l.scope.vars[name] = lbinding{t: declType, mutable: mutable}
	out.Statements = append(out.Statements, &aotir.LetStmt{
		Name:    name,
		VarType: declType,
		Init:    value,
		Mutable: mutable,
	})
	return nil
}

// lowerAssign handles `NAME = expr`. Field/index targets are
// rejected for Phase 2.1 (records and lists land in Phase 3).
func (l *lowerer) lowerAssign(out *aotir.Block, as *parser.AssignStmt) error {
	if len(as.Index) != 0 || len(as.Field) != 0 {
		return fmt.Errorf("assignment to a[i] or a.f targets land with records/lists in Phase 3")
	}
	b, ok := l.scope.lookup(as.Name)
	if !ok {
		return fmt.Errorf("assignment to undeclared %q", as.Name)
	}
	if !b.mutable {
		return fmt.Errorf("assignment to immutable %q (declared with let)", as.Name)
	}
	value, err := l.lowerExpr(as.Value)
	if err != nil {
		return fmt.Errorf("assign %q: %w", as.Name, err)
	}
	if value.Type() != b.t {
		return fmt.Errorf("assign %q: binding is %s, value is %s", as.Name, b.t, value.Type())
	}
	out.Statements = append(out.Statements, &aotir.AssignStmt{
		Name:  as.Name,
		Value: value,
	})
	return nil
}

// lowerIf lowers an if/else-if/else chain. else-if is preserved as a
// nested IfStmt inside the Else block of its parent: the verifier
// allows it and the emit pass keeps the source structure for the
// debugger line table (Phase 16).
func (l *lowerer) lowerIf(out *aotir.Block, is *parser.IfStmt) error {
	cond, err := l.lowerExpr(is.Cond)
	if err != nil {
		return fmt.Errorf("if cond: %w", err)
	}
	if cond.Type() != aotir.TypeBool {
		return fmt.Errorf("if cond must be bool, got %s", cond.Type())
	}
	thenBlock, err := l.lowerNestedBlock(is.Then)
	if err != nil {
		return fmt.Errorf("if then: %w", err)
	}
	var elseBlock *aotir.Block
	switch {
	case is.ElseIf != nil:
		// Wrap the chained `else if` in its own block whose only
		// statement is the nested IfStmt. The verifier walks into
		// the wrapper, so any binding the chained branch declares
		// stays scoped to that branch.
		inner := &aotir.Block{}
		nested := newLScope(l.scope)
		prev := l.scope
		l.scope = nested
		if err := l.lowerIf(inner, is.ElseIf); err != nil {
			l.scope = prev
			return err
		}
		l.scope = prev
		elseBlock = inner
	case len(is.Else) > 0:
		elseBlock, err = l.lowerNestedBlock(is.Else)
		if err != nil {
			return fmt.Errorf("if else: %w", err)
		}
	}
	out.Statements = append(out.Statements, &aotir.IfStmt{
		Cond: cond,
		Then: thenBlock,
		Else: elseBlock,
	})
	return nil
}

// lowerFor lowers `for x in start..end { body }` into a ForRangeStmt.
// Phase 2.2 only covers the int-range form; list iteration (Source
// without a RangeEnd) lands in Phase 3 alongside lists.
//
// The induction variable is registered as immutable in a fresh nested
// scope so an inner `x = ...` is rejected (matches Mochi semantics).
// Loop depth is incremented for the body so BreakStmt / ContinueStmt
// inside the loop are valid.
func (l *lowerer) lowerFor(out *aotir.Block, fs *parser.ForStmt) error {
	if fs.Name == "" {
		return fmt.Errorf("for loop induction variable is empty")
	}
	if fs.RangeEnd == nil {
		return fmt.Errorf("for-in over a list lands with Phase 3; only `for x in start..end` is supported in Phase 2.2")
	}
	start, err := l.lowerExpr(fs.Source)
	if err != nil {
		return fmt.Errorf("for %s start: %w", fs.Name, err)
	}
	if start.Type() != aotir.TypeInt {
		return fmt.Errorf("for %s start must be int, got %s", fs.Name, start.Type())
	}
	end, err := l.lowerExpr(fs.RangeEnd)
	if err != nil {
		return fmt.Errorf("for %s end: %w", fs.Name, err)
	}
	if end.Type() != aotir.TypeInt {
		return fmt.Errorf("for %s end must be int, got %s", fs.Name, end.Type())
	}

	prev := l.scope
	l.scope = newLScope(prev)
	l.scope.vars[fs.Name] = lbinding{t: aotir.TypeInt, mutable: false}
	l.loopDepth++
	body := &aotir.Block{}
	for i, st := range fs.Body {
		if st == nil {
			l.loopDepth--
			l.scope = prev
			return fmt.Errorf("for %s body stmt %d is nil", fs.Name, i)
		}
		if err := l.lowerStatement(body, st); err != nil {
			l.loopDepth--
			l.scope = prev
			return fmt.Errorf("for %s body stmt %d: %w", fs.Name, i, err)
		}
	}
	l.loopDepth--
	l.scope = prev

	out.Statements = append(out.Statements, &aotir.ForRangeStmt{
		Var:   fs.Name,
		Start: start,
		End:   end,
		Body:  body,
	})
	return nil
}

// lowerWhile lowers a `while cond { body }`. Increments loopDepth
// for the body so nested BreakStmt / ContinueStmt resolve correctly.
func (l *lowerer) lowerWhile(out *aotir.Block, ws *parser.WhileStmt) error {
	cond, err := l.lowerExpr(ws.Cond)
	if err != nil {
		return fmt.Errorf("while cond: %w", err)
	}
	if cond.Type() != aotir.TypeBool {
		return fmt.Errorf("while cond must be bool, got %s", cond.Type())
	}
	l.loopDepth++
	body, err := l.lowerNestedBlock(ws.Body)
	l.loopDepth--
	if err != nil {
		return fmt.Errorf("while body: %w", err)
	}
	out.Statements = append(out.Statements, &aotir.WhileStmt{
		Cond: cond,
		Body: body,
	})
	return nil
}

// lowerReturn lowers a `return` statement. From main (unit return)
// only a bare `return` is legal; from a user fn with non-unit return
// the value expression is required and type-checked against the
// enclosing function's return type.
func (l *lowerer) lowerReturn(out *aotir.Block, rs *parser.ReturnStmt) error {
	if l.currentFnReturn == aotir.TypeUnit {
		if rs.Value != nil {
			return fmt.Errorf("bare `return` only: enclosing function returns unit")
		}
		out.Statements = append(out.Statements, &aotir.ReturnStmt{Value: nil})
		return nil
	}
	if rs.Value == nil {
		return fmt.Errorf("return without a value: enclosing function returns %s", l.currentFnReturn)
	}
	value, err := l.lowerExpr(rs.Value)
	if err != nil {
		return fmt.Errorf("return: %w", err)
	}
	if value.Type() != l.currentFnReturn {
		return fmt.Errorf("return: function returns %s, value is %s", l.currentFnReturn, value.Type())
	}
	out.Statements = append(out.Statements, &aotir.ReturnStmt{Value: value})
	return nil
}

// lowerNestedBlock pushes a fresh scope, lowers each statement into a
// new Block, and pops the scope on exit. Mirrors the verifier's
// per-Block scope discipline.
func (l *lowerer) lowerNestedBlock(stmts []*parser.Statement) (*aotir.Block, error) {
	prev := l.scope
	l.scope = newLScope(prev)
	defer func() { l.scope = prev }()
	b := &aotir.Block{}
	for i, st := range stmts {
		if st == nil {
			return nil, fmt.Errorf("block statement %d is nil", i)
		}
		if err := l.lowerStatement(b, st); err != nil {
			return nil, fmt.Errorf("block stmt %d: %w", i, err)
		}
	}
	return b, nil
}

// typeFromRef maps a parser.TypeRef to an aotir.Type. Only the
// primitive identifiers `int`, `float`, `bool`, `string` are
// accepted; everything else is deferred to later phases.
func typeFromRef(ref *parser.TypeRef) (aotir.Type, error) {
	if ref == nil {
		return aotir.TypeInvalid, fmt.Errorf("nil type ref")
	}
	if ref.Optional {
		return aotir.TypeInvalid, fmt.Errorf("optional types land with Option in Phase 3")
	}
	if ref.Simple == nil {
		return aotir.TypeInvalid, fmt.Errorf("composite type annotations land in later phases")
	}
	switch *ref.Simple {
	case "int":
		return aotir.TypeInt, nil
	case "float":
		return aotir.TypeFloat, nil
	case "bool":
		return aotir.TypeBool, nil
	case "string":
		return aotir.TypeString, nil
	}
	return aotir.TypeInvalid, fmt.Errorf("type %q not supported in Phase 2.1", *ref.Simple)
}

// printCalleeFor picks the runtime print entry for an argument
// type. The verifier already mirrors this mapping; keeping the
// switch in one place avoids the two drifting apart.
func printCalleeFor(t aotir.Type) (string, error) {
	switch t {
	case aotir.TypeString:
		return "mochi_print_str", nil
	case aotir.TypeInt:
		return "mochi_print_i64", nil
	case aotir.TypeFloat:
		return "mochi_print_f64", nil
	case aotir.TypeBool:
		return "mochi_print_bool", nil
	}
	return "", fmt.Errorf("print() does not accept %s in Phase 2.1", t)
}

// matchBareCall walks an Expr that is expected to be a single
// top-level call (either `print(...)` or a discarded user-fn call)
// and returns the embedded CallExpr. Anything else (compound binary,
// leading unary, postfix chain, non-call primary) is rejected so
// stray side-effecting subexpressions cannot smuggle past the
// statement-position type check.
func matchBareCall(expr *parser.Expr) (*parser.CallExpr, error) {
	if expr == nil {
		return nil, fmt.Errorf("nil expression")
	}
	bin := expr.Binary
	if bin == nil || bin.Left == nil || len(bin.Right) != 0 {
		return nil, fmt.Errorf("expected a bare call, got compound binary expression")
	}
	unary := bin.Left
	if len(unary.Ops) != 0 {
		return nil, fmt.Errorf("unary operators not supported around a bare call")
	}
	post := unary.Value
	if post == nil || len(post.Ops) != 0 || post.Target == nil {
		return nil, fmt.Errorf("expected a bare call (no postfix operators)")
	}
	call := post.Target.Call
	if call == nil {
		return nil, fmt.Errorf("expected a call, got a different primary")
	}
	return call, nil
}

// lowerExpr lowers a parser.Expr into an aotir.Expr.
func (l *lowerer) lowerExpr(e *parser.Expr) (aotir.Expr, error) {
	if e == nil || e.Binary == nil {
		return nil, fmt.Errorf("nil or non-binary expression")
	}
	return l.lowerBinary(e.Binary)
}

// lowerBinary folds the parser's left-associative chain into an
// aotir.BinaryExpr tree, monomorphising each operator against the
// operand types via opForTypes.
func (l *lowerer) lowerBinary(bin *parser.BinaryExpr) (aotir.Expr, error) {
	if bin == nil || bin.Left == nil {
		return nil, fmt.Errorf("nil binary")
	}
	left, err := l.lowerUnary(bin.Left)
	if err != nil {
		return nil, err
	}
	for _, op := range bin.Right {
		if op == nil || op.Right == nil {
			return nil, fmt.Errorf("nil binary operator")
		}
		right, err := l.lowerUnary(op.Right)
		if err != nil {
			return nil, err
		}
		bop, res, err := opForTypes(op.Op, left.Type(), right.Type())
		if err != nil {
			return nil, err
		}
		left = &aotir.BinaryExpr{
			Op:     bop,
			Left:   left,
			Right:  right,
			Result: res,
		}
	}
	return left, nil
}

// opForTypes maps a source operator + operand types to the typed
// aotir.BinOp plus the result type. Mixed int/float operands are
// rejected: Mochi requires an explicit cast and Phase 2.x does not
// lower casts yet.
func opForTypes(opStr string, lhs, rhs aotir.Type) (aotir.BinOp, aotir.Type, error) {
	switch opStr {
	case "+", "-", "*", "/", "%":
		if lhs == aotir.TypeInt && rhs == aotir.TypeInt {
			switch opStr {
			case "+":
				return aotir.BinAddI64, aotir.TypeInt, nil
			case "-":
				return aotir.BinSubI64, aotir.TypeInt, nil
			case "*":
				return aotir.BinMulI64, aotir.TypeInt, nil
			case "/":
				return aotir.BinDivI64, aotir.TypeInt, nil
			case "%":
				return aotir.BinModI64, aotir.TypeInt, nil
			}
		}
		if lhs == aotir.TypeFloat && rhs == aotir.TypeFloat {
			switch opStr {
			case "+":
				return aotir.BinAddF64, aotir.TypeFloat, nil
			case "-":
				return aotir.BinSubF64, aotir.TypeFloat, nil
			case "*":
				return aotir.BinMulF64, aotir.TypeFloat, nil
			case "/":
				return aotir.BinDivF64, aotir.TypeFloat, nil
			case "%":
				return aotir.BinInvalid, aotir.TypeInvalid,
					fmt.Errorf("operator %q on float operands not supported", opStr)
			}
		}
		return aotir.BinInvalid, aotir.TypeInvalid,
			fmt.Errorf("operator %q wants both int or both float, got %s and %s", opStr, lhs, rhs)
	case "==", "!=", "<", "<=", ">", ">=":
		if lhs == aotir.TypeInt && rhs == aotir.TypeInt {
			return cmpIntOp(opStr), aotir.TypeBool, nil
		}
		if lhs == aotir.TypeFloat && rhs == aotir.TypeFloat {
			return cmpFloatOp(opStr), aotir.TypeBool, nil
		}
		if lhs == aotir.TypeBool && rhs == aotir.TypeBool {
			switch opStr {
			case "==":
				return aotir.BinEqBool, aotir.TypeBool, nil
			case "!=":
				return aotir.BinNeBool, aotir.TypeBool, nil
			}
			return aotir.BinInvalid, aotir.TypeInvalid,
				fmt.Errorf("operator %q on bool operands not supported (only == / !=)", opStr)
		}
		return aotir.BinInvalid, aotir.TypeInvalid,
			fmt.Errorf("comparison %q wants matching int, float, or bool operands, got %s and %s", opStr, lhs, rhs)
	case "&&", "||":
		if lhs != aotir.TypeBool || rhs != aotir.TypeBool {
			return aotir.BinInvalid, aotir.TypeInvalid,
				fmt.Errorf("operator %q requires bool operands, got %s and %s", opStr, lhs, rhs)
		}
		if opStr == "&&" {
			return aotir.BinAndBool, aotir.TypeBool, nil
		}
		return aotir.BinOrBool, aotir.TypeBool, nil
	}
	return aotir.BinInvalid, aotir.TypeInvalid,
		fmt.Errorf("operator %q not supported in Phase 2.1", opStr)
}

func cmpIntOp(op string) aotir.BinOp {
	switch op {
	case "==":
		return aotir.BinEqI64
	case "!=":
		return aotir.BinNeI64
	case "<":
		return aotir.BinLtI64
	case "<=":
		return aotir.BinLeI64
	case ">":
		return aotir.BinGtI64
	case ">=":
		return aotir.BinGeI64
	}
	return aotir.BinInvalid
}

func cmpFloatOp(op string) aotir.BinOp {
	switch op {
	case "==":
		return aotir.BinEqF64
	case "!=":
		return aotir.BinNeF64
	case "<":
		return aotir.BinLtF64
	case "<=":
		return aotir.BinLeF64
	case ">":
		return aotir.BinGtF64
	case ">=":
		return aotir.BinGeF64
	}
	return aotir.BinInvalid
}

// lowerUnary handles a parser.Unary node: the optional leading `-`
// and `!` operators followed by a Primary expression.
func (l *lowerer) lowerUnary(u *parser.Unary) (aotir.Expr, error) {
	if u == nil {
		return nil, fmt.Errorf("nil unary")
	}
	inner, err := l.lowerPostfix(u.Value)
	if err != nil {
		return nil, err
	}
	for i := len(u.Ops) - 1; i >= 0; i-- {
		op := u.Ops[i]
		switch op {
		case "-":
			switch inner.Type() {
			case aotir.TypeInt:
				inner = &aotir.UnaryExpr{Op: aotir.UnNegI64, Operand: inner, Result: aotir.TypeInt}
			case aotir.TypeFloat:
				inner = &aotir.UnaryExpr{Op: aotir.UnNegF64, Operand: inner, Result: aotir.TypeFloat}
			default:
				return nil, fmt.Errorf("unary '-' requires int or float, got %s", inner.Type())
			}
		case "!":
			if inner.Type() != aotir.TypeBool {
				return nil, fmt.Errorf("unary '!' requires bool, got %s", inner.Type())
			}
			inner = &aotir.UnaryExpr{Op: aotir.UnNotBool, Operand: inner, Result: aotir.TypeBool}
		default:
			return nil, fmt.Errorf("unary operator %q not supported in Phase 2.1", op)
		}
	}
	return inner, nil
}

// lowerPostfix handles a PostfixExpr whose only legal Phase 2.1
// shape is a bare Primary (no `.`, `[]`, `()`, or `as` postfixes).
func (l *lowerer) lowerPostfix(p *parser.PostfixExpr) (aotir.Expr, error) {
	if p == nil || p.Target == nil {
		return nil, fmt.Errorf("nil postfix")
	}
	if len(p.Ops) != 0 {
		return nil, fmt.Errorf("postfix operators not supported in Phase 2.1 (calls/indexes/casts land in later phases)")
	}
	return l.lowerPrimary(p.Target)
}

// lowerPrimary lowers a Primary into either a literal, a parenthesised
// expression, or a variable reference. Phase 2.1 accepts a Selector
// only when its Tail is empty (bare ident); records land in Phase 3.
func (l *lowerer) lowerPrimary(pr *parser.Primary) (aotir.Expr, error) {
	if pr == nil {
		return nil, fmt.Errorf("nil primary")
	}
	if pr.Lit != nil {
		return lowerLiteral(pr.Lit)
	}
	if pr.Group != nil {
		return l.lowerExpr(pr.Group)
	}
	if pr.Selector != nil {
		if len(pr.Selector.Tail) != 0 {
			return nil, fmt.Errorf("field access %s.%s lands with records in Phase 3",
				pr.Selector.Root, strings.Join(pr.Selector.Tail, "."))
		}
		b, ok := l.scope.lookup(pr.Selector.Root)
		if !ok {
			return nil, fmt.Errorf("undeclared variable %q", pr.Selector.Root)
		}
		return &aotir.VarRef{Name: pr.Selector.Root, VarType: b.t}, nil
	}
	if pr.Call != nil {
		return l.lowerUserCallExpr(pr.Call)
	}
	return nil, fmt.Errorf("primary %s not supported in Phase 2.2", trimPrimary(pr))
}

// lowerUserCallExpr lowers a value-producing user-fn call. The print
// builtins are unit-return and so cannot appear in expression
// position; the lowerer rejects them explicitly.
func (l *lowerer) lowerUserCallExpr(call *parser.CallExpr) (aotir.Expr, error) {
	if call.Func == "print" {
		return nil, fmt.Errorf("print() returns unit and cannot appear in an expression")
	}
	sig, ok := l.funcs[call.Func]
	if !ok {
		return nil, fmt.Errorf("unresolved callee %q", call.Func)
	}
	if sig.returnType == aotir.TypeUnit {
		return nil, fmt.Errorf("call to %q returns unit and cannot appear in an expression", call.Func)
	}
	args, err := l.lowerCallArgs(call, sig)
	if err != nil {
		return nil, err
	}
	return &aotir.CallExpr{
		Func:   call.Func,
		Args:   args,
		Result: sig.returnType,
	}, nil
}

func lowerLiteral(lit *parser.Literal) (aotir.Expr, error) {
	switch {
	case lit.Int != nil:
		return &aotir.IntLit{Value: int64(*lit.Int)}, nil
	case lit.Float != nil:
		v := *lit.Float
		if math.IsNaN(v) || math.IsInf(v, 0) {
			return nil, fmt.Errorf("NaN/Inf float literals deferred to Phase 2.4")
		}
		return &aotir.FloatLit{Value: v}, nil
	case lit.Bool != nil:
		return &aotir.BoolLit{Value: bool(*lit.Bool)}, nil
	case lit.Str != nil:
		return &aotir.StringLit{Value: *lit.Str}, nil
	case lit.None:
		return nil, fmt.Errorf("none literal lands with Option in Phase 3")
	}
	return nil, fmt.Errorf("empty literal node")
}

// trimPrimary returns a short string describing pr for diagnostics;
// avoids dumping the entire participle tree.
func trimPrimary(pr *parser.Primary) string {
	var b strings.Builder
	switch {
	case pr.Selector != nil:
		fmt.Fprintf(&b, "selector(%s)", pr.Selector.Root)
	case pr.Call != nil:
		fmt.Fprintf(&b, "call(%s)", pr.Call.Func)
	case pr.List != nil:
		b.WriteString("list literal")
	case pr.Map != nil:
		b.WriteString("map literal")
	case pr.FunExpr != nil:
		b.WriteString("fun expression")
	default:
		b.WriteString("unknown primary")
	}
	return b.String()
}
