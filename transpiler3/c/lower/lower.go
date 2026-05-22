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

	// Pass 0: collect every `type T { ... }` declaration. Record
	// names are registered before sig-building so a fun signature
	// or a record-field type can reference any record without
	// regard to source order. Field types are resolved in this
	// same pass; the records map is set membership only at the
	// start, and decls are stamped onto the output program in
	// source order.
	records := map[string]*aotir.RecordDecl{}
	var typeDecls []*parser.TypeDecl
	for i, st := range prog.Statements {
		if st == nil || st.Type == nil {
			continue
		}
		td := st.Type
		if td.Name == "" {
			return nil, fmt.Errorf("transpiler3/c/lower: statement %d: type decl with empty name", i)
		}
		if _, dup := records[td.Name]; dup {
			return nil, fmt.Errorf("transpiler3/c/lower: statement %d: redeclaration of type %q", i, td.Name)
		}
		// Reserve the name so later passes can resolve it.
		records[td.Name] = nil
		typeDecls = append(typeDecls, td)
	}
	out := &aotir.Program{}
	for _, td := range typeDecls {
		rd, err := buildRecordDecl(records, td)
		if err != nil {
			return nil, fmt.Errorf("transpiler3/c/lower: type %q: %w", td.Name, err)
		}
		records[td.Name] = rd
		out.Records = append(out.Records, rd)
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
		sig, err := buildFuncSig(records, fn)
		if err != nil {
			return nil, fmt.Errorf("transpiler3/c/lower: fun %q: %w", fn.Name, err)
		}
		funcs[fn.Name] = sig
		funDecls = append(funDecls, fn)
	}

	// Pass 2a: lower each fun body using the shared funcs table.
	for _, fn := range funDecls {
		sig := funcs[fn.Name]
		l := &lowerer{
			funcs:                 funcs,
			records:               records,
			scope:                 newLScope(nil),
			currentFnReturn:       sig.returnType,
			currentFnReturnRecord: sig.returnRecordName,
		}
		// Seed parameters into the function scope as immutable.
		for _, p := range sig.params {
			l.scope.vars[p.Name] = lbinding{t: p.Type, mutable: false, record: p.RecordName}
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
			Name:             fn.Name,
			Params:           sig.params,
			ReturnType:       sig.returnType,
			ReturnRecordName: sig.returnRecordName,
			Body:             body,
		})
	}

	// Pass 2b: lower the top-level script (everything that is not
	// a fun or type decl) into main.
	mainBody := &aotir.Block{}
	mainL := &lowerer{
		funcs:           funcs,
		records:         records,
		scope:           newLScope(nil),
		currentFnReturn: aotir.TypeUnit,
	}
	for i, st := range prog.Statements {
		if st == nil {
			return nil, fmt.Errorf("transpiler3/c/lower: statement %d is nil", i)
		}
		if st.Fun != nil || st.Type != nil {
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

// buildRecordDecl turns a parser.TypeDecl into an aotir.RecordDecl.
// Phase 3.0 accepts only the `type T { field: Type, ... }` shape with
// scalar field types; methods, variants, aliases, and nested-record
// fields are rejected with phase-named diagnostics.
func buildRecordDecl(records map[string]*aotir.RecordDecl, td *parser.TypeDecl) (*aotir.RecordDecl, error) {
	if len(td.Variants) > 0 || td.SingleVariant != nil {
		return nil, fmt.Errorf("sum-type variants land with Phase 4")
	}
	if td.Alias != nil {
		return nil, fmt.Errorf("type aliases land in a later phase")
	}
	if len(td.Members) == 0 {
		return nil, fmt.Errorf("record type must declare at least one field")
	}
	rd := &aotir.RecordDecl{Name: td.Name}
	seen := map[string]bool{}
	for j, m := range td.Members {
		if m == nil {
			return nil, fmt.Errorf("field %d is nil", j)
		}
		if m.Method != nil {
			return nil, fmt.Errorf("methods land in a later phase")
		}
		if m.Field == nil {
			return nil, fmt.Errorf("field %d has no Field or Method", j)
		}
		f := m.Field
		if f.Name == "" {
			return nil, fmt.Errorf("field %d: empty name", j)
		}
		if seen[f.Name] {
			return nil, fmt.Errorf("duplicate field %q", f.Name)
		}
		seen[f.Name] = true
		t, rec, err := typeFromRef(records, f.Type)
		if err != nil {
			return nil, fmt.Errorf("field %q: %w", f.Name, err)
		}
		if t == aotir.TypeRecord {
			return nil, fmt.Errorf("field %q: nested record fields are not supported in Phase 3.0", f.Name)
		}
		rd.Fields = append(rd.Fields, aotir.RecordField{Name: f.Name, Type: t, RecordName: rec})
	}
	return rd, nil
}

// funcSig is the lower-time projection of an aotir.Function signature
// (no body); the lowerer needs it to resolve user-fn calls during
// expression lowering.
type funcSig struct {
	params           []aotir.Param
	returnType       aotir.Type
	returnRecordName string
}

// buildFuncSig turns a parser.FunStmt into its lower-time signature.
// Both parameter types and return type are required; Mochi accepts
// `fun f(x) { ... }` as inferring from caller context, but Phase 2.2
// requires explicit annotations so the C-AOT monomorpher does not
// have to do inference. Phase 3.0 widens param/return type lookup to
// the records table so user fns can accept and return records.
func buildFuncSig(records map[string]*aotir.RecordDecl, fn *parser.FunStmt) (*funcSig, error) {
	if fn.Return == nil {
		return nil, fmt.Errorf("fun %q requires an explicit `: T` return type in Phase 2.2", fn.Name)
	}
	ret, retRec, err := typeFromRef(records, fn.Return)
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
		t, rec, err := typeFromRef(records, p.Type)
		if err != nil {
			return nil, fmt.Errorf("fun %q param %q: %w", fn.Name, p.Name, err)
		}
		params = append(params, aotir.Param{Name: p.Name, Type: t, RecordName: rec})
	}
	return &funcSig{params: params, returnType: ret, returnRecordName: retRec}, nil
}

// lowerer carries the per-function scope stack, loop-depth counter,
// and the enclosing function's return type. Mirrors the verifier's
// verifyCtx so the same scoping / typing rules apply at lower time.
type lowerer struct {
	funcs                 map[string]*funcSig
	records               map[string]*aotir.RecordDecl
	scope                 *lscope
	loopDepth             int
	currentFnReturn       aotir.Type
	currentFnReturnRecord string
}

// lscope mirrors aotir's scope: lexical frame with parent chain.
type lscope struct {
	parent *lscope
	vars   map[string]lbinding
}

type lbinding struct {
	t       aotir.Type
	mutable bool
	record  string // record name when t==TypeRecord
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
		return fmt.Errorf("`type` declarations are only allowed at the top level")
	}
	return fmt.Errorf("unsupported statement in Phase 3.0")
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
		if sig.params[i].Type == aotir.TypeRecord {
			if argRec := exprRecordName(expr); argRec != sig.params[i].RecordName {
				return nil, fmt.Errorf("call %q arg %d: expected record %q, got %q",
					call.Func, i, sig.params[i].RecordName, argRec)
			}
		}
		out = append(out, expr)
	}
	return out, nil
}

// exprRecordName extracts the record-name identity of a record-typed
// aotir expression. Mirrors the verifier's exprRecordName but lives
// in lower so the lowerer can stamp the right name onto carrier
// fields without round-tripping through Verify.
func exprRecordName(e aotir.Expr) string {
	switch v := e.(type) {
	case *aotir.VarRef:
		return v.RecordName
	case *aotir.RecordLit:
		return v.TypeName
	case *aotir.FieldAccess:
		return v.ResultRecordName
	case *aotir.CallExpr:
		return v.ResultRecordName
	}
	return ""
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
	declRec := exprRecordName(value)
	if declared != nil {
		t, rec, err := typeFromRef(l.records, declared)
		if err != nil {
			return fmt.Errorf("binding %q type: %w", name, err)
		}
		if t != declType {
			return fmt.Errorf("binding %q: declared %s, init produces %s", name, t, declType)
		}
		if t == aotir.TypeRecord && rec != declRec {
			return fmt.Errorf("binding %q: declared record %q, init produces record %q", name, rec, declRec)
		}
		declType = t
		declRec = rec
	}
	l.scope.vars[name] = lbinding{t: declType, mutable: mutable, record: declRec}
	out.Statements = append(out.Statements, &aotir.LetStmt{
		Name:       name,
		VarType:    declType,
		RecordName: declRec,
		Init:       value,
		Mutable:    mutable,
	})
	return nil
}

// lowerAssign handles `NAME = expr`. Field/index targets remain
// rejected in Phase 3.0: records are value-semantics so updating
// `p.f` would semantically reassign `p` as a whole, which the
// surface syntax does not express.
func (l *lowerer) lowerAssign(out *aotir.Block, as *parser.AssignStmt) error {
	if len(as.Index) != 0 {
		return fmt.Errorf("assignment to a[i] targets land with lists in Phase 3.1")
	}
	if len(as.Field) != 0 {
		return fmt.Errorf("assignment to a.f targets is not supported in Phase 3.0 (records are value-semantics; reassign the whole binding)")
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
	if b.t == aotir.TypeRecord {
		if vrec := exprRecordName(value); vrec != b.record {
			return fmt.Errorf("assign %q: binding holds record %q, value produces record %q", as.Name, b.record, vrec)
		}
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
	if l.currentFnReturn == aotir.TypeRecord {
		if vrec := exprRecordName(value); vrec != l.currentFnReturnRecord {
			return fmt.Errorf("return: function returns record %q, value produces record %q",
				l.currentFnReturnRecord, vrec)
		}
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

// typeFromRef maps a parser.TypeRef to an aotir.Type plus an optional
// record name (set when the type is a user record). Phase 3.0 accepts
// the four primitives plus any user-declared record name.
func typeFromRef(records map[string]*aotir.RecordDecl, ref *parser.TypeRef) (aotir.Type, string, error) {
	if ref == nil {
		return aotir.TypeInvalid, "", fmt.Errorf("nil type ref")
	}
	if ref.Optional {
		return aotir.TypeInvalid, "", fmt.Errorf("optional types land with Option in Phase 3")
	}
	if ref.Simple == nil {
		return aotir.TypeInvalid, "", fmt.Errorf("composite type annotations land in later phases")
	}
	switch *ref.Simple {
	case "int":
		return aotir.TypeInt, "", nil
	case "float":
		return aotir.TypeFloat, "", nil
	case "bool":
		return aotir.TypeBool, "", nil
	case "string":
		return aotir.TypeString, "", nil
	}
	if _, ok := records[*ref.Simple]; ok {
		return aotir.TypeRecord, *ref.Simple, nil
	}
	return aotir.TypeInvalid, "", fmt.Errorf("type %q not supported in Phase 3.0", *ref.Simple)
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
	if t == aotir.TypeRecord {
		return "", fmt.Errorf("print() does not accept a record value in Phase 3.0 (access scalar fields instead)")
	}
	return "", fmt.Errorf("print() does not accept %s in Phase 3.0", t)
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
		if lhs == aotir.TypeString && rhs == aotir.TypeString {
			switch opStr {
			case "==":
				return aotir.BinEqStr, aotir.TypeBool, nil
			case "!=":
				return aotir.BinNeStr, aotir.TypeBool, nil
			}
			return aotir.BinInvalid, aotir.TypeInvalid,
				fmt.Errorf("operator %q on string operands not supported (only == / != in Phase 3.0)", opStr)
		}
		if lhs == aotir.TypeRecord && rhs == aotir.TypeRecord {
			switch opStr {
			case "==":
				return aotir.BinEqRec, aotir.TypeBool, nil
			case "!=":
				return aotir.BinNeRec, aotir.TypeBool, nil
			}
			return aotir.BinInvalid, aotir.TypeInvalid,
				fmt.Errorf("operator %q on record operands not supported (only == / !=)", opStr)
		}
		return aotir.BinInvalid, aotir.TypeInvalid,
			fmt.Errorf("comparison %q wants matching int, float, bool, string, or record operands, got %s and %s", opStr, lhs, rhs)
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

// lowerPostfix handles a PostfixExpr. Phase 3.0 accepts the `.Field`
// postfix on record-typed receivers (so a call like `make_point().x`
// works without going through a let-binding). All other postfix shapes
// (Call/Index/Cast/SafeField/SafeIndex) are deferred to later phases.
func (l *lowerer) lowerPostfix(p *parser.PostfixExpr) (aotir.Expr, error) {
	if p == nil || p.Target == nil {
		return nil, fmt.Errorf("nil postfix")
	}
	expr, err := l.lowerPrimary(p.Target)
	if err != nil {
		return nil, err
	}
	for _, op := range p.Ops {
		if op == nil {
			return nil, fmt.Errorf("nil postfix op")
		}
		switch {
		case op.Field != nil:
			expr, err = l.lowerFieldOp(expr, op.Field.Name)
			if err != nil {
				return nil, err
			}
		case op.Call != nil:
			return nil, fmt.Errorf("postfix call on an expression is not supported in Phase 3.0 (use a bare callee name)")
		case op.Index != nil, op.SafeIndex != nil:
			return nil, fmt.Errorf("postfix index lands with lists in Phase 3.1")
		case op.SafeField != nil:
			return nil, fmt.Errorf("safe field access `?.` lands with Option in a later phase")
		case op.Cast != nil:
			return nil, fmt.Errorf("`as` casts land in a later phase")
		default:
			return nil, fmt.Errorf("unsupported postfix operator")
		}
	}
	return expr, nil
}

// lowerFieldOp resolves a `.field` against a record-typed receiver and
// returns a FieldAccess node typed by the field's declared type. The
// receiver expression must already be typed as a record; the lowerer
// then looks up the field on the record's declaration to stamp Result
// (and ResultRecordName if the field is itself record-typed -- not
// reachable in Phase 3.0 since nested records are rejected).
func (l *lowerer) lowerFieldOp(receiver aotir.Expr, fieldName string) (aotir.Expr, error) {
	if receiver.Type() != aotir.TypeRecord {
		return nil, fmt.Errorf("field access .%s: receiver is %s, expected a record", fieldName, receiver.Type())
	}
	recName := exprRecordName(receiver)
	if recName == "" {
		return nil, fmt.Errorf("field access .%s: receiver has no record name", fieldName)
	}
	decl, ok := l.records[recName]
	if !ok {
		return nil, fmt.Errorf("field access .%s: record %q is not declared", fieldName, recName)
	}
	for _, f := range decl.Fields {
		if f.Name == fieldName {
			return &aotir.FieldAccess{
				Receiver:         receiver,
				RecordName:       recName,
				FieldName:        fieldName,
				Result:           f.Type,
				ResultRecordName: f.RecordName,
			}, nil
		}
	}
	return nil, fmt.Errorf("field access .%s: record %q has no field %q", fieldName, recName, fieldName)
}

// lowerPrimary lowers a Primary into either a literal, a parenthesised
// expression, a variable reference, a record literal, a selector
// chain (variable + zero or more `.field` reads), or a call to a user
// function.
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
	if pr.Struct != nil {
		return l.lowerStructLit(pr.Struct)
	}
	if pr.Selector != nil {
		b, ok := l.scope.lookup(pr.Selector.Root)
		if !ok {
			return nil, fmt.Errorf("undeclared variable %q", pr.Selector.Root)
		}
		var expr aotir.Expr = &aotir.VarRef{
			Name:       pr.Selector.Root,
			VarType:    b.t,
			RecordName: b.record,
		}
		for _, field := range pr.Selector.Tail {
			var err error
			expr, err = l.lowerFieldOp(expr, field)
			if err != nil {
				return nil, err
			}
		}
		return expr, nil
	}
	if pr.Call != nil {
		return l.lowerUserCallExpr(pr.Call)
	}
	return nil, fmt.Errorf("primary %s not supported in Phase 3.0%s", trimPrimary(pr), primaryPhaseHint(pr))
}

// lowerStructLit lowers a `R { f1: v1, ... }` literal into a typed
// RecordLit. The lowerer enforces full field coverage, no extras, no
// duplicates, and type-checks each field value against its declared
// type; it also reorders fields from source-literal order to record-
// declaration order so the emit pass can render the C99 designated
// init in struct-field order.
func (l *lowerer) lowerStructLit(sl *parser.StructLiteral) (aotir.Expr, error) {
	if sl.Name == "" {
		return nil, fmt.Errorf("record literal with empty type name")
	}
	decl, ok := l.records[sl.Name]
	if !ok {
		return nil, fmt.Errorf("record literal %q: record is not declared", sl.Name)
	}
	provided := make(map[string]aotir.Expr, len(sl.Fields))
	for _, lf := range sl.Fields {
		if lf == nil || lf.Name == "" {
			return nil, fmt.Errorf("record literal %q: field with empty name", sl.Name)
		}
		if _, dup := provided[lf.Name]; dup {
			return nil, fmt.Errorf("record literal %q: duplicate field %q", sl.Name, lf.Name)
		}
		value, err := l.lowerExpr(lf.Value)
		if err != nil {
			return nil, fmt.Errorf("record literal %q field %q: %w", sl.Name, lf.Name, err)
		}
		provided[lf.Name] = value
	}
	declared := make(map[string]bool, len(decl.Fields))
	for _, df := range decl.Fields {
		declared[df.Name] = true
	}
	for name := range provided {
		if !declared[name] {
			return nil, fmt.Errorf("record literal %q: unknown field %q", sl.Name, name)
		}
	}
	args := make([]aotir.RecordLitArg, 0, len(decl.Fields))
	for _, df := range decl.Fields {
		v, ok := provided[df.Name]
		if !ok {
			return nil, fmt.Errorf("record literal %q: missing field %q", sl.Name, df.Name)
		}
		if v.Type() != df.Type {
			return nil, fmt.Errorf("record literal %q field %q: declared %s, value is %s",
				sl.Name, df.Name, df.Type, v.Type())
		}
		args = append(args, aotir.RecordLitArg{Name: df.Name, Value: v})
	}
	return &aotir.RecordLit{TypeName: sl.Name, Fields: args}, nil
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
		Func:             call.Func,
		Args:             args,
		Result:           sig.returnType,
		ResultRecordName: sig.returnRecordName,
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

// primaryPhaseHint names the phase that adds support for pr, when one
// is known. Phase 3.1 adds list literals, 3.2 adds maps, 4.x adds
// fun-expressions. The hint is appended to the rejection diagnostic
// so users see both the current floor and the future ceiling.
func primaryPhaseHint(pr *parser.Primary) string {
	switch {
	case pr.List != nil:
		return " (list literals land with Phase 3.1)"
	case pr.Map != nil:
		return " (map literals land with Phase 3.2)"
	case pr.FunExpr != nil:
		return " (fun expressions land with Phase 4)"
	}
	return ""
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
