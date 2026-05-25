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
			funcs:                      funcs,
			records:                    records,
			scope:                      newLScope(nil),
			currentFnReturn:            sig.returnType,
			currentFnReturnRecord:      sig.returnRecordName,
			currentFnReturnElem:        sig.returnElemType,
			currentFnReturnElemRec:     sig.returnElemRecord,
			currentFnReturnInnerElem:   sig.returnInnerElem,
			currentFnReturnKey:         sig.returnKeyType,
			currentFnReturnValue:       sig.returnValueType,
			currentFnReturnListValElem: sig.returnListValElem,
		}
		// Seed parameters into the function scope as immutable.
		for _, p := range sig.params {
			l.scope.vars[p.Name] = lbinding{
				t:           p.Type,
				mutable:     false,
				record:      p.RecordName,
				elem:        p.ElemType,
				elemRec:     p.ElemRecordName,
				innerElem:   p.InnerElemType,
				key:         p.KeyType,
				value:       p.ValueType,
				listValElem: p.ListValueElemType,
			}
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
			Name:                    fn.Name,
			Params:                  sig.params,
			ReturnType:              sig.returnType,
			ReturnRecordName:        sig.returnRecordName,
			ReturnElemType:          sig.returnElemType,
			ReturnElemRecordName:    sig.returnElemRecord,
			ReturnInnerElemType:     sig.returnInnerElem,
			ReturnKeyType:           sig.returnKeyType,
			ReturnValueType:         sig.returnValueType,
			ReturnListValueElemType: sig.returnListValElem,
			Body:                    body,
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
		tr, err := typeFromRef(records, f.Type)
		if err != nil {
			return nil, fmt.Errorf("field %q: %w", f.Name, err)
		}
		if tr.t == aotir.TypeRecord {
			return nil, fmt.Errorf("field %q: nested record fields are not supported in Phase 3.0", f.Name)
		}
		if tr.t == aotir.TypeList {
			return nil, fmt.Errorf("field %q: list-typed record fields are not supported in Phase 3.1", f.Name)
		}
		if tr.t == aotir.TypeMap {
			return nil, fmt.Errorf("field %q: map-typed record fields are not supported in Phase 3.2", f.Name)
		}
		rd.Fields = append(rd.Fields, aotir.RecordField{Name: f.Name, Type: tr.t, RecordName: tr.rec})
	}
	return rd, nil
}

// funcSig is the lower-time projection of an aotir.Function signature
// (no body); the lowerer needs it to resolve user-fn calls during
// expression lowering.
type funcSig struct {
	params              []aotir.Param
	returnType          aotir.Type
	returnRecordName    string
	returnElemType      aotir.Type
	returnElemRecord    string     // record name when returnElemType==TypeRecord
	returnInnerElem     aotir.Type // inner elem type when returnElemType==TypeList (Phase 3.4b)
	returnKeyType       aotir.Type
	returnValueType     aotir.Type
	returnListValElem   aotir.Type // inner list elem when returnValueType==TypeList (Phase 3.4e)
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
	retTR, err := typeFromRef(records, fn.Return)
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
		pTR, err := typeFromRef(records, p.Type)
		if err != nil {
			return nil, fmt.Errorf("fun %q param %q: %w", fn.Name, p.Name, err)
		}
		params = append(params, aotir.Param{
			Name:              p.Name,
			Type:              pTR.t,
			RecordName:        pTR.rec,
			ElemType:          pTR.elem,
			ElemRecordName:    pTR.elemRec,
			InnerElemType:     pTR.innerElem,
			KeyType:           pTR.key,
			ValueType:         pTR.value,
			ListValueElemType: pTR.listValElem,
		})
	}
	return &funcSig{
		params:            params,
		returnType:        retTR.t,
		returnRecordName:  retTR.rec,
		returnElemType:    retTR.elem,
		returnElemRecord:  retTR.elemRec,
		returnInnerElem:   retTR.innerElem,
		returnKeyType:       retTR.key,
		returnValueType:     retTR.value,
		returnListValElem:   retTR.listValElem,
	}, nil
}

// lowerer carries the per-function scope stack, loop-depth counter,
// and the enclosing function's return type. Mirrors the verifier's
// verifyCtx so the same scoping / typing rules apply at lower time.
type lowerer struct {
	funcs                       map[string]*funcSig
	records                     map[string]*aotir.RecordDecl
	scope                       *lscope
	loopDepth                   int
	currentFnReturn             aotir.Type
	currentFnReturnRecord       string
	currentFnReturnElem         aotir.Type
	currentFnReturnElemRec      string     // record name when currentFnReturnElem==TypeRecord
	currentFnReturnInnerElem    aotir.Type // inner elem when currentFnReturnElem==TypeList (Phase 3.4b)
	currentFnReturnKey          aotir.Type
	currentFnReturnValue        aotir.Type
	currentFnReturnListValElem  aotir.Type // inner list elem when returnValue==TypeList (Phase 3.4e)
}

// lscope mirrors aotir's scope: lexical frame with parent chain.
type lscope struct {
	parent *lscope
	vars   map[string]lbinding
}

type lbinding struct {
	t            aotir.Type
	mutable      bool
	record       string     // record name when t==TypeRecord
	elem         aotir.Type // element type when t==TypeList
	elemRec      string     // element record name when t==TypeList && elem==TypeRecord
	innerElem    aotir.Type // inner element type when t==TypeList && elem==TypeList (Phase 3.4b)
	key          aotir.Type // key type when t==TypeMap
	value        aotir.Type // value type when t==TypeMap
	listValElem  aotir.Type // inner list elem when t==TypeMap && value==TypeList (Phase 3.4e)
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
	return fmt.Errorf("unsupported statement in Phase 3.1")
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
		if sig.params[i].Type == aotir.TypeList {
			if argElem := exprElemType(expr); argElem != sig.params[i].ElemType {
				return nil, fmt.Errorf("call %q arg %d: expected list<%s>, got list<%s>",
					call.Func, i, sig.params[i].ElemType, argElem)
			}
			if sig.params[i].ElemType == aotir.TypeRecord {
				if argElemRec := exprElemRecordName(expr); argElemRec != sig.params[i].ElemRecordName {
					return nil, fmt.Errorf("call %q arg %d: expected list<%s>, got list<%s>",
						call.Func, i, sig.params[i].ElemRecordName, argElemRec)
				}
			}
			if sig.params[i].ElemType == aotir.TypeList {
				if argInner := exprInnerElemType(expr); argInner != sig.params[i].InnerElemType {
					return nil, fmt.Errorf("call %q arg %d: expected list<list<%s>>, got list<list<%s>>",
						call.Func, i, sig.params[i].InnerElemType, argInner)
				}
			}
		}
		if sig.params[i].Type == aotir.TypeMap {
			if argKey := exprKeyType(expr); argKey != sig.params[i].KeyType {
				return nil, fmt.Errorf("call %q arg %d: expected map<%s,_>, got map<%s,_>",
					call.Func, i, sig.params[i].KeyType, argKey)
			}
			if argVal := exprValueType(expr); argVal != sig.params[i].ValueType {
				return nil, fmt.Errorf("call %q arg %d: expected map<_,%s>, got map<_,%s>",
					call.Func, i, sig.params[i].ValueType, argVal)
			}
			if sig.params[i].ValueType == aotir.TypeList {
				if argLV := exprListValueElemType(expr); argLV != sig.params[i].ListValueElemType {
					return nil, fmt.Errorf("call %q arg %d: expected map<_,list<%s>>, got map<_,list<%s>>",
						call.Func, i, sig.params[i].ListValueElemType, argLV)
				}
			}
		}
		out = append(out, expr)
	}
	return out, nil
}

// isEmptyListLit reports whether e is a bare `[]` with no elements.
// Used by lowerBinding to detect the typed-empty-list pattern
// (`let xs: list<int> = []`) before entering lowerExpr, so that
// lowerListLit never sees a zero-element slice.
func isEmptyListLit(e *parser.Expr) bool {
	if e == nil || e.Binary == nil || len(e.Binary.Right) != 0 {
		return false
	}
	u := e.Binary.Left
	if u == nil || len(u.Ops) != 0 || u.Value == nil || len(u.Value.Ops) != 0 {
		return false
	}
	ll := u.Value.Target.List
	return ll != nil && len(ll.Elems) == 0
}

// isEmptyMapLit reports whether e is a bare `{}` with no entries.
// Used by lowerBinding to detect the typed-empty-map pattern
// (`let m: map<K,V> = {}`) before entering lowerExpr.
func isEmptyMapLit(e *parser.Expr) bool {
	if e == nil || e.Binary == nil || len(e.Binary.Right) != 0 {
		return false
	}
	u := e.Binary.Left
	if u == nil || len(u.Ops) != 0 || u.Value == nil || len(u.Value.Ops) != 0 {
		return false
	}
	ml := u.Value.Target.Map
	return ml != nil && len(ml.Items) == 0
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
	case *aotir.IndexExpr:
		// Phase 3.4a: list<R> indexing returns a record-typed value;
		// the record name rides along on ElemRecordName.
		return v.ElemRecordName
	}
	return ""
}

// exprElemRecordName extracts the element-record-name identity of a
// list-of-record expression. Phase 3.4 callers use this to thread the
// record name through LetStmt/AssignStmt/return checks the same way
// exprRecordName threads bare records.
func exprElemRecordName(e aotir.Expr) string {
	switch v := e.(type) {
	case *aotir.VarRef:
		return v.ElemRecordName
	case *aotir.ListLit:
		return v.ElemRecordName
	case *aotir.CallExpr:
		return v.ResultElemRecordName
	case *aotir.AppendExpr:
		return v.ElemRecordName
	case *aotir.IndexExpr:
		// IndexExpr returns a scalar/record, not a list; included for
		// completeness but always returns "" here.
		return v.ElemRecordName
	}
	return ""
}

// exprElemType extracts the element type of a list-typed aotir
// expression. Mirrors the verifier helper of the same name; Phase
// 3.2 widens the coverage to include MapKeysExpr and MapValuesExpr
// (both produce list-typed values). Phase 3.4b widens to IndexExpr
// when the index produces a list value (receiver was list<list<T>>):
// the IndexExpr's own element type is T, recorded on InnerElemType.
func exprElemType(e aotir.Expr) aotir.Type {
	switch v := e.(type) {
	case *aotir.VarRef:
		return v.ElemType
	case *aotir.ListLit:
		return v.ElemType
	case *aotir.CallExpr:
		return v.ResultElemType
	case *aotir.AppendExpr:
		return v.ElemType
	case *aotir.IndexExpr:
		if v.ElemType == aotir.TypeList {
			return v.InnerElemType
		}
		return aotir.TypeInvalid
	case *aotir.MapKeysExpr:
		return v.KeyType
	case *aotir.MapValuesExpr:
		return v.ValueType
	}
	return aotir.TypeInvalid
}

// exprInnerElemType extracts the inner element type of a
// list<list<T>>-typed aotir expression, mirroring the verifier's
// helper of the same name. Phase 3.4b node coverage: VarRef,
// ListLit, CallExpr, AppendExpr, IndexExpr. Phase 3.4e adds
// MapValuesExpr: values(m) on map<K,list<V>> produces list<list<V>>;
// the inner V lives on MapValuesExpr.ListValueElemType.
func exprInnerElemType(e aotir.Expr) aotir.Type {
	switch v := e.(type) {
	case *aotir.VarRef:
		return v.InnerElemType
	case *aotir.ListLit:
		return v.InnerElemType
	case *aotir.CallExpr:
		return v.ResultInnerElemType
	case *aotir.AppendExpr:
		return v.InnerElemType
	case *aotir.IndexExpr:
		return v.InnerElemType
	case *aotir.MapValuesExpr:
		// values(m) on map<K,list<V>> produces list<list<V>>.
		return v.ListValueElemType
	}
	return aotir.TypeInvalid
}

// exprKeyType extracts the key type of a map-typed aotir expression.
// Mirrors the verifier helper of the same name.
func exprKeyType(e aotir.Expr) aotir.Type {
	switch v := e.(type) {
	case *aotir.VarRef:
		return v.KeyType
	case *aotir.MapLit:
		return v.KeyType
	case *aotir.CallExpr:
		return v.ResultKeyType
	}
	return aotir.TypeInvalid
}

// exprValueType extracts the value type of a map-typed aotir
// expression. Mirrors the verifier helper of the same name.
func exprValueType(e aotir.Expr) aotir.Type {
	switch v := e.(type) {
	case *aotir.VarRef:
		return v.ValueType
	case *aotir.MapLit:
		return v.ValueType
	case *aotir.CallExpr:
		return v.ResultValueType
	}
	return aotir.TypeInvalid
}

// exprListValueElemType extracts the inner scalar element type of the
// list value in a map<K,list<V>>-typed expression, or TypeInvalid
// otherwise. Only meaningful on expressions whose Type() is TypeMap.
// MapValuesExpr is intentionally excluded: values(m) produces a list,
// not a map, so the result binding carries InnerElemType not
// ListValueElemType.
func exprListValueElemType(e aotir.Expr) aotir.Type {
	switch v := e.(type) {
	case *aotir.VarRef:
		return v.ListValueElemType
	case *aotir.MapLit:
		return v.ListValueElemType
	case *aotir.CallExpr:
		return v.ResultListValueElemType
	}
	return aotir.TypeInvalid
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

	// Phase 3.4c: typed-empty-literal fast path.
	// `let xs: list<int> = []` and `let m: map<K,V> = {}` must bypass
	// lowerExpr so that lowerListLit / lowerMapLit never see len==0.
	// The declared annotation supplies the type; the IR node is built
	// directly from typeFromRef and registered without entering lowerExpr.
	if declared != nil {
		if isEmptyListLit(init) {
			tr, err := typeFromRef(l.records, declared)
			if err != nil {
				return fmt.Errorf("binding %q type: %w", name, err)
			}
			if tr.t != aotir.TypeList {
				return fmt.Errorf("binding %q: declared type is %s but init is an empty list literal", name, tr.t)
			}
			lit := &aotir.ListLit{ElemType: tr.elem, ElemRecordName: tr.elemRec, InnerElemType: tr.innerElem}
			l.scope.vars[name] = lbinding{t: aotir.TypeList, mutable: mutable, elem: tr.elem, elemRec: tr.elemRec, innerElem: tr.innerElem}
			out.Statements = append(out.Statements, &aotir.LetStmt{
				Name: name, VarType: aotir.TypeList, ElemType: tr.elem,
				ElemRecordName: tr.elemRec, InnerElemType: tr.innerElem,
				Init: lit, Mutable: mutable,
			})
			return nil
		}
		if isEmptyMapLit(init) {
			tr, err := typeFromRef(l.records, declared)
			if err != nil {
				return fmt.Errorf("binding %q type: %w", name, err)
			}
			if tr.t != aotir.TypeMap {
				return fmt.Errorf("binding %q: declared type is %s but init is an empty map literal", name, tr.t)
			}
			lit := &aotir.MapLit{KeyType: tr.key, ValueType: tr.value, ListValueElemType: tr.listValElem}
			l.scope.vars[name] = lbinding{t: aotir.TypeMap, mutable: mutable, key: tr.key, value: tr.value, listValElem: tr.listValElem}
			out.Statements = append(out.Statements, &aotir.LetStmt{
				Name: name, VarType: aotir.TypeMap, KeyType: tr.key, ValueType: tr.value,
				ListValueElemType: tr.listValElem,
				Init: lit, Mutable: mutable,
			})
			return nil
		}
	}

	value, err := l.lowerExpr(init)
	if err != nil {
		return fmt.Errorf("binding %q init: %w", name, err)
	}
	declType := value.Type()
	declRec := exprRecordName(value)
	declElem := exprElemType(value)
	declElemRec := exprElemRecordName(value)
	declInnerElem := exprInnerElemType(value)
	declKey := exprKeyType(value)
	declValue := exprValueType(value)
	declListValElem := exprListValueElemType(value)
	if declared != nil {
		tr, err := typeFromRef(l.records, declared)
		if err != nil {
			return fmt.Errorf("binding %q type: %w", name, err)
		}
		if tr.t != declType {
			return fmt.Errorf("binding %q: declared %s, init produces %s", name, tr.t, declType)
		}
		if tr.t == aotir.TypeRecord && tr.rec != declRec {
			return fmt.Errorf("binding %q: declared record %q, init produces record %q", name, tr.rec, declRec)
		}
		if tr.t == aotir.TypeList && tr.elem != declElem {
			return fmt.Errorf("binding %q: declared list<%s>, init produces list<%s>", name, tr.elem, declElem)
		}
		if tr.t == aotir.TypeList && tr.elem == aotir.TypeRecord && tr.elemRec != declElemRec {
			return fmt.Errorf("binding %q: declared list<%s>, init produces list<%s>", name, tr.elemRec, declElemRec)
		}
		if tr.t == aotir.TypeList && tr.elem == aotir.TypeList && tr.innerElem != declInnerElem {
			return fmt.Errorf("binding %q: declared list<list<%s>>, init produces list<list<%s>>", name, tr.innerElem, declInnerElem)
		}
		if tr.t == aotir.TypeMap {
			if tr.key != declKey {
				return fmt.Errorf("binding %q: declared map<%s,_>, init produces map<%s,_>", name, tr.key, declKey)
			}
			if tr.value != declValue {
				return fmt.Errorf("binding %q: declared map<_,%s>, init produces map<_,%s>", name, tr.value, declValue)
			}
			if tr.value == aotir.TypeList && tr.listValElem != declListValElem {
				return fmt.Errorf("binding %q: declared map<_,list<%s>>, init produces map<_,list<%s>>", name, tr.listValElem, declListValElem)
			}
		}
		declType = tr.t
		declRec = tr.rec
		declElem = tr.elem
		declElemRec = tr.elemRec
		declInnerElem = tr.innerElem
		declKey = tr.key
		declValue = tr.value
		declListValElem = tr.listValElem
	}
	l.scope.vars[name] = lbinding{
		t:            declType,
		mutable:      mutable,
		record:       declRec,
		elem:         declElem,
		elemRec:      declElemRec,
		innerElem:    declInnerElem,
		key:          declKey,
		value:        declValue,
		listValElem:  declListValElem,
	}
	out.Statements = append(out.Statements, &aotir.LetStmt{
		Name:              name,
		VarType:           declType,
		RecordName:        declRec,
		ElemType:          declElem,
		ElemRecordName:    declElemRec,
		InnerElemType:     declInnerElem,
		KeyType:           declKey,
		ValueType:         declValue,
		ListValueElemType: declListValElem,
		Init:              value,
		Mutable:           mutable,
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
	if b.t == aotir.TypeList {
		if velem := exprElemType(value); velem != b.elem {
			return fmt.Errorf("assign %q: binding holds list<%s>, value produces list<%s>", as.Name, b.elem, velem)
		}
		if b.elem == aotir.TypeRecord {
			if velemRec := exprElemRecordName(value); velemRec != b.elemRec {
				return fmt.Errorf("assign %q: binding holds list<%s>, value produces list<%s>", as.Name, b.elemRec, velemRec)
			}
		}
		if b.elem == aotir.TypeList {
			if vinner := exprInnerElemType(value); vinner != b.innerElem {
				return fmt.Errorf("assign %q: binding holds list<list<%s>>, value produces list<list<%s>>", as.Name, b.innerElem, vinner)
			}
		}
	}
	if b.t == aotir.TypeMap {
		if vkey := exprKeyType(value); vkey != b.key {
			return fmt.Errorf("assign %q: binding holds map<%s,_>, value produces map<%s,_>", as.Name, b.key, vkey)
		}
		if vval := exprValueType(value); vval != b.value {
			return fmt.Errorf("assign %q: binding holds map<_,%s>, value produces map<_,%s>", as.Name, b.value, vval)
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
		return l.lowerForEach(out, fs)
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

// lowerForEach lowers `for x in xs { body }` where xs is a
// list-typed expression. The induction variable is registered as
// immutable inside the body scope with the list's element type.
// Phase 3.2 widens xs to map<K,V> as well: when the source is a map,
// the loop iterates over keys() (sorted by key, matching the vm) and
// the induction variable's type is K. This is a sugar lowering that
// re-uses ForEachStmt over a synthesised MapKeysExpr; no new IR node
// is required because for-iter over a map is exactly equivalent to
// for-iter over keys(m).
func (l *lowerer) lowerForEach(out *aotir.Block, fs *parser.ForStmt) error {
	source, err := l.lowerExpr(fs.Source)
	if err != nil {
		return fmt.Errorf("for %s in: %w", fs.Name, err)
	}
	var listExpr aotir.Expr
	var elem aotir.Type
	var elemRec string
	var innerElem aotir.Type
	switch source.Type() {
	case aotir.TypeList:
		listExpr = source
		elem = exprElemType(source)
		elemRec = exprElemRecordName(source)
		if elem == aotir.TypeList {
			innerElem = exprInnerElemType(source)
		}
	case aotir.TypeMap:
		key := exprKeyType(source)
		val := exprValueType(source)
		listExpr = &aotir.MapKeysExpr{
			Receiver:          source,
			KeyType:           key,
			ValueType:         val,
			ListValueElemType: exprListValueElemType(source),
		}
		elem = key
	default:
		return fmt.Errorf("for %s in: source must be a list or a map, got %s", fs.Name, source.Type())
	}
	prev := l.scope
	l.scope = newLScope(prev)
	// When iterating list<list<T>>, the induction variable is itself a
	// list<T>; its element type (T) lives in lbinding.elem so further
	// indexing/append/len resolves correctly.
	bindElem := elem
	bindElemRec := elemRec
	bindInnerElem := aotir.TypeInvalid
	if elem == aotir.TypeList {
		bindElem = innerElem
		bindElemRec = ""
	}
	l.scope.vars[fs.Name] = lbinding{t: elem, mutable: false, record: elemRec, elem: bindElem, elemRec: bindElemRec, innerElem: bindInnerElem}
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

	out.Statements = append(out.Statements, &aotir.ForEachStmt{
		Var:            fs.Name,
		List:           listExpr,
		ElemType:       elem,
		ElemRecordName: elemRec,
		InnerElemType:  innerElem,
		Body:           body,
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
	if l.currentFnReturn == aotir.TypeList {
		if velem := exprElemType(value); velem != l.currentFnReturnElem {
			return fmt.Errorf("return: function returns list<%s>, value produces list<%s>",
				l.currentFnReturnElem, velem)
		}
		if l.currentFnReturnElem == aotir.TypeRecord {
			if velemRec := exprElemRecordName(value); velemRec != l.currentFnReturnElemRec {
				return fmt.Errorf("return: function returns list<%s>, value produces list<%s>",
					l.currentFnReturnElemRec, velemRec)
			}
		}
		if l.currentFnReturnElem == aotir.TypeList {
			if vinner := exprInnerElemType(value); vinner != l.currentFnReturnInnerElem {
				return fmt.Errorf("return: function returns list<list<%s>>, value produces list<list<%s>>",
					l.currentFnReturnInnerElem, vinner)
			}
		}
	}
	if l.currentFnReturn == aotir.TypeMap {
		if vkey := exprKeyType(value); vkey != l.currentFnReturnKey {
			return fmt.Errorf("return: function returns map<%s,_>, value produces map<%s,_>",
				l.currentFnReturnKey, vkey)
		}
		if vval := exprValueType(value); vval != l.currentFnReturnValue {
			return fmt.Errorf("return: function returns map<_,%s>, value produces map<_,%s>",
				l.currentFnReturnValue, vval)
		}
		if l.currentFnReturnValue == aotir.TypeList {
			if vlv := exprListValueElemType(value); vlv != l.currentFnReturnListValElem {
				return fmt.Errorf("return: function returns map<_,list<%s>>, value produces map<_,list<%s>>",
					l.currentFnReturnListValElem, vlv)
			}
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

// typeResolution is the parsed-and-resolved view of a parser.TypeRef.
// It bundles the aotir.Type plus the parallel-field identities that
// ride alongside composite types: record name (when t==TypeRecord),
// element type (when t==TypeList), and key/value types (when
// t==TypeMap). Phase 3.2 returns this struct from typeFromRef to
// keep callsite arity manageable as more parallel fields land.
// Phase 3.4e adds listValElem, valid when t==TypeMap && value==TypeList.
type typeResolution struct {
	t            aotir.Type
	rec          string
	elem         aotir.Type
	elemRec      string     // valid when elem==TypeRecord (Phase 3.4a)
	innerElem    aotir.Type // valid when elem==TypeList (Phase 3.4b)
	key          aotir.Type
	value        aotir.Type
	listValElem  aotir.Type // valid when t==TypeMap && value==TypeList (Phase 3.4e)
}

// typeFromRef maps a parser.TypeRef to a typeResolution. Phase 3.2
// accepts:
//
//   - the four scalar primitives,
//   - any user-declared record name,
//   - `[T]` or `list<T>` where T is one of the four scalar primitives,
//   - `map<K,V>` where K is int or string and V is one of the four
//     scalar primitives.
func typeFromRef(records map[string]*aotir.RecordDecl, ref *parser.TypeRef) (typeResolution, error) {
	if ref == nil {
		return typeResolution{}, fmt.Errorf("nil type ref")
	}
	if ref.Optional {
		return typeResolution{}, fmt.Errorf("optional types land with Option in a later phase")
	}
	if ref.ListElem != nil {
		elem, elemRec, innerElem, err := listElemFromRef(records, ref.ListElem)
		if err != nil {
			return typeResolution{}, err
		}
		return typeResolution{t: aotir.TypeList, elem: elem, elemRec: elemRec, innerElem: innerElem}, nil
	}
	if ref.Generic != nil {
		switch ref.Generic.Name {
		case "list":
			if len(ref.Generic.Args) != 1 {
				return typeResolution{}, fmt.Errorf("list<T> takes exactly one type argument, got %d", len(ref.Generic.Args))
			}
			elem, elemRec, innerElem, err := listElemFromRef(records, ref.Generic.Args[0])
			if err != nil {
				return typeResolution{}, err
			}
			return typeResolution{t: aotir.TypeList, elem: elem, elemRec: elemRec, innerElem: innerElem}, nil
		case "map":
			if len(ref.Generic.Args) != 2 {
				return typeResolution{}, fmt.Errorf("map<K,V> takes exactly two type arguments, got %d", len(ref.Generic.Args))
			}
			key, err := mapKeyFromRef(records, ref.Generic.Args[0])
			if err != nil {
				return typeResolution{}, err
			}
			value, listValElem, err := mapValueFromRef(records, ref.Generic.Args[1])
			if err != nil {
				return typeResolution{}, err
			}
			return typeResolution{t: aotir.TypeMap, key: key, value: value, listValElem: listValElem}, nil
		}
		return typeResolution{}, fmt.Errorf("generic type %q not supported in Phase 3.2", ref.Generic.Name)
	}
	if ref.Simple == nil {
		return typeResolution{}, fmt.Errorf("composite type annotations land in later phases")
	}
	switch *ref.Simple {
	case "int":
		return typeResolution{t: aotir.TypeInt}, nil
	case "float":
		return typeResolution{t: aotir.TypeFloat}, nil
	case "bool":
		return typeResolution{t: aotir.TypeBool}, nil
	case "string":
		return typeResolution{t: aotir.TypeString}, nil
	}
	if _, ok := records[*ref.Simple]; ok {
		return typeResolution{t: aotir.TypeRecord, rec: *ref.Simple}, nil
	}
	return typeResolution{}, fmt.Errorf("type %q not supported in Phase 3.2", *ref.Simple)
}

// listElemFromRef resolves a list's element TypeRef. Phase 3.1
// accepts the four scalar primitives. Phase 3.4a widens this to
// accept TypeRecord (user-declared records). Phase 3.4b widens it
// once more to accept TypeList where the inner element is a scalar
// primitive, returning the inner element type in the third result so
// callers can stamp InnerElemType onto the IR carrier. Map elements
// remain rejected pending later sub-phases. Three-level nesting
// (list<list<list<T>>>) is still rejected here.
func listElemFromRef(records map[string]*aotir.RecordDecl, ref *parser.TypeRef) (aotir.Type, string, aotir.Type, error) {
	tr, err := typeFromRef(records, ref)
	if err != nil {
		return aotir.TypeInvalid, "", aotir.TypeInvalid, fmt.Errorf("list element: %w", err)
	}
	switch tr.t {
	case aotir.TypeInt, aotir.TypeFloat, aotir.TypeBool, aotir.TypeString:
		return tr.t, "", aotir.TypeInvalid, nil
	case aotir.TypeRecord:
		return aotir.TypeRecord, tr.rec, aotir.TypeInvalid, nil
	case aotir.TypeList:
		// Inner must be a scalar primitive in Phase 3.4b.
		switch tr.elem {
		case aotir.TypeInt, aotir.TypeFloat, aotir.TypeBool, aotir.TypeString:
			return aotir.TypeList, "", tr.elem, nil
		case aotir.TypeRecord:
			return aotir.TypeInvalid, "", aotir.TypeInvalid, fmt.Errorf("list<list<record>> is not supported in Phase 3.4b (lands with a later sub-phase)")
		case aotir.TypeList:
			return aotir.TypeInvalid, "", aotir.TypeInvalid, fmt.Errorf("3-level nested lists (list<list<list<T>>>) are not supported in Phase 3.4b")
		}
		return aotir.TypeInvalid, "", aotir.TypeInvalid, fmt.Errorf("list<list<%s>> not supported in Phase 3.4b", tr.elem)
	case aotir.TypeMap:
		return aotir.TypeInvalid, "", aotir.TypeInvalid, fmt.Errorf("list of map is not supported in Phase 3.4b (lands with a later sub-phase)")
	}
	return aotir.TypeInvalid, "", aotir.TypeInvalid, fmt.Errorf("list element type %s not supported in Phase 3.4b", tr.t)
}

// mapKeyFromRef resolves a map's key TypeRef. Phase 3.2 accepts only
// int and string keys (the two key types the runtime ships helpers
// for); other element types fail with a phase-named diagnostic.
func mapKeyFromRef(records map[string]*aotir.RecordDecl, ref *parser.TypeRef) (aotir.Type, error) {
	tr, err := typeFromRef(records, ref)
	if err != nil {
		return aotir.TypeInvalid, fmt.Errorf("map key: %w", err)
	}
	switch tr.t {
	case aotir.TypeInt, aotir.TypeString:
		return tr.t, nil
	}
	return aotir.TypeInvalid, fmt.Errorf("map key type %s not supported in Phase 3.2 (int or string only)", tr.t)
}

// mapValueFromRef resolves a map's value TypeRef. Phase 3.2 accepts
// the four scalar primitives; Phase 3.4e widens to list<V> where V
// is a scalar primitive. Record / nested-map values land in later
// sub-phases. Returns (valueType, listElemType, error).
func mapValueFromRef(records map[string]*aotir.RecordDecl, ref *parser.TypeRef) (aotir.Type, aotir.Type, error) {
	tr, err := typeFromRef(records, ref)
	if err != nil {
		return aotir.TypeInvalid, aotir.TypeInvalid, fmt.Errorf("map value: %w", err)
	}
	switch tr.t {
	case aotir.TypeInt, aotir.TypeFloat, aotir.TypeBool, aotir.TypeString:
		return tr.t, aotir.TypeInvalid, nil
	case aotir.TypeList:
		// Phase 3.4e: map<K, list<V>> where V is a scalar primitive.
		switch tr.elem {
		case aotir.TypeInt, aotir.TypeFloat, aotir.TypeBool, aotir.TypeString:
			return aotir.TypeList, tr.elem, nil
		}
		return aotir.TypeInvalid, aotir.TypeInvalid, fmt.Errorf("map value list<%s> not supported in Phase 3.4e (scalar inner only)", tr.elem)
	}
	return aotir.TypeInvalid, aotir.TypeInvalid, fmt.Errorf("map value type %s not supported in Phase 3.4e (scalar or list<scalar> only)", tr.t)
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
		return "", fmt.Errorf("print() does not accept a record value in Phase 3.1 (access scalar fields instead)")
	}
	if t == aotir.TypeList {
		return "", fmt.Errorf("print() does not accept a list value in Phase 3.1 (iterate and print elements instead)")
	}
	return "", fmt.Errorf("print() does not accept %s in Phase 3.1", t)
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
		if op.Op == "in" && right.Type() == aotir.TypeMap {
			recvKey := exprKeyType(right)
			if left.Type() != recvKey {
				return nil, fmt.Errorf("`in` map: key type is %s, got %s", recvKey, left.Type())
			}
			left = &aotir.MapHasExpr{
				Receiver:          right,
				Key:               left,
				KeyType:           recvKey,
				ValueType:         exprValueType(right),
				ListValueElemType: exprListValueElemType(right),
			}
			continue
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
		if lhs == aotir.TypeList && rhs == aotir.TypeList {
			switch opStr {
			case "==":
				return aotir.BinEqList, aotir.TypeBool, nil
			case "!=":
				return aotir.BinNeList, aotir.TypeBool, nil
			}
			return aotir.BinInvalid, aotir.TypeInvalid,
				fmt.Errorf("operator %q on list operands not supported (only == / !=)", opStr)
		}
		if lhs == aotir.TypeMap && rhs == aotir.TypeMap {
			switch opStr {
			case "==":
				return aotir.BinEqMap, aotir.TypeBool, nil
			case "!=":
				return aotir.BinNeMap, aotir.TypeBool, nil
			}
			return aotir.BinInvalid, aotir.TypeInvalid,
				fmt.Errorf("operator %q on map operands not supported (only == / !=)", opStr)
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
			return nil, fmt.Errorf("postfix call on an expression is not supported in Phase 3.1 (use a bare callee name)")
		case op.Index != nil:
			expr, err = l.lowerIndexOp(expr, op.Index)
			if err != nil {
				return nil, err
			}
		case op.SafeIndex != nil:
			return nil, fmt.Errorf("safe index `?[k]` lands with Option in a later phase")
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

// lowerIndexOp resolves an `[i]` postfix. Phase 3.2 dispatches on
// the receiver's runtime type: list receivers lower to IndexExpr
// with an int index; map receivers lower to MapGetExpr with a
// KeyType-typed key. Slice/step postfixes remain rejected (deferred
// to a later phase that adds list slicing).
func (l *lowerer) lowerIndexOp(receiver aotir.Expr, idx *parser.IndexOp) (aotir.Expr, error) {
	if idx.Colon != nil || idx.Colon2 != nil || idx.End != nil || idx.Step != nil {
		return nil, fmt.Errorf("slice / step indexing lands in a later phase")
	}
	if idx.Start == nil {
		return nil, fmt.Errorf("index access [k]: missing index expression")
	}
	switch receiver.Type() {
	case aotir.TypeList:
		index, err := l.lowerExpr(idx.Start)
		if err != nil {
			return nil, fmt.Errorf("index expression: %w", err)
		}
		if index.Type() != aotir.TypeInt {
			return nil, fmt.Errorf("list index must be int, got %s", index.Type())
		}
		// For list<list<T>>: receiver's ElemType is TypeList and its
		// InnerElemType is T. The produced IndexExpr is itself a
		// list<T> value; its own InnerElemType is therefore T. For
		// scalar-element lists (Phase 3.1), or for the inner index
		// of a list<list<T>> chain that produces a scalar, the
		// produced value has no inner element, so InnerElemType is
		// left TypeInvalid.
		producedElem := exprElemType(receiver)
		var producedInner aotir.Type
		if producedElem == aotir.TypeList {
			producedInner = exprInnerElemType(receiver)
		}
		return &aotir.IndexExpr{
			Receiver:       receiver,
			Index:          index,
			ElemType:       producedElem,
			ElemRecordName: exprElemRecordName(receiver),
			InnerElemType:  producedInner,
		}, nil
	case aotir.TypeMap:
		key, err := l.lowerExpr(idx.Start)
		if err != nil {
			return nil, fmt.Errorf("index key: %w", err)
		}
		recvKey := exprKeyType(receiver)
		recvVal := exprValueType(receiver)
		if key.Type() != recvKey {
			return nil, fmt.Errorf("map key must be %s, got %s", recvKey, key.Type())
		}
		return &aotir.MapGetExpr{
			Receiver:          receiver,
			Key:               key,
			KeyType:           recvKey,
			ValueType:         recvVal,
			ListValueElemType: exprListValueElemType(receiver),
		}, nil
	}
	return nil, fmt.Errorf("index access [k]: receiver is %s, expected a list or map", receiver.Type())
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
			Name:              pr.Selector.Root,
			VarType:           b.t,
			RecordName:        b.record,
			ElemType:          b.elem,
			ElemRecordName:    b.elemRec,
			InnerElemType:     b.innerElem,
			KeyType:           b.key,
			ValueType:         b.value,
			ListValueElemType: b.listValElem,
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
	if pr.List != nil {
		return l.lowerListLit(pr.List)
	}
	if pr.Map != nil {
		return l.lowerMapLit(pr.Map)
	}
	return nil, fmt.Errorf("primary %s not supported in Phase 3.2%s", trimPrimary(pr), primaryPhaseHint(pr))
}

// lowerListLit lowers a `[e1, e2, ...]` literal. Every element must
// lower to the same type; the resulting ListLit's ElemType is taken
// from the first element. Phase 3.1 accepted the four scalar
// primitives; Phase 3.4a widens to TypeRecord with all elements
// agreeing on record identity (ElemRecordName). Empty list literals
// are rejected here; the `let xs: list<int> = []` typed-empty form
// is handled upstream in lowerBinding (Phase 3.4c) before lowerExpr
// is called, so this function never sees a zero-element slice from
// an annotated binding.
func (l *lowerer) lowerListLit(ll *parser.ListLiteral) (aotir.Expr, error) {
	if len(ll.Elems) == 0 {
		return nil, fmt.Errorf("empty list literal: Phase 3.1 requires at least one element so the element type can be inferred")
	}
	elems := make([]aotir.Expr, 0, len(ll.Elems))
	var elemType aotir.Type
	var elemRec string
	var innerElem aotir.Type
	for i, e := range ll.Elems {
		v, err := l.lowerExpr(e)
		if err != nil {
			return nil, fmt.Errorf("list literal element %d: %w", i, err)
		}
		if i == 0 {
			elemType = v.Type()
			switch elemType {
			case aotir.TypeInt, aotir.TypeFloat, aotir.TypeBool, aotir.TypeString:
				// ok
			case aotir.TypeRecord:
				elemRec = exprRecordName(v)
				if elemRec == "" {
					return nil, fmt.Errorf("list literal element %d: record element has no record name", i)
				}
			case aotir.TypeList:
				// Phase 3.4b: list<list<T>> where T is a scalar
				// primitive. The element's inner type (T) is captured
				// on InnerElemType so downstream operations on the
				// nested list can resolve helpers.
				innerElem = exprElemType(v)
				switch innerElem {
				case aotir.TypeInt, aotir.TypeFloat, aotir.TypeBool, aotir.TypeString:
					// ok
				case aotir.TypeRecord:
					return nil, fmt.Errorf("list literal element %d: list<list<record>> is not supported in Phase 3.4b (record nesting lands in a later sub-phase)", i)
				case aotir.TypeList:
					return nil, fmt.Errorf("list literal element %d: 3-level nested lists are not supported in Phase 3.4b", i)
				default:
					return nil, fmt.Errorf("list literal element %d: list<list<%s>> is not supported", i, innerElem)
				}
			default:
				return nil, fmt.Errorf("list literal element %d: unsupported type %s", i, elemType)
			}
		} else {
			if v.Type() != elemType {
				return nil, fmt.Errorf("list literal element %d: first element is %s, this is %s", i, elemType, v.Type())
			}
			if elemType == aotir.TypeRecord {
				if rec := exprRecordName(v); rec != elemRec {
					return nil, fmt.Errorf("list literal element %d: first element is record %q, this is record %q", i, elemRec, rec)
				}
			}
			if elemType == aotir.TypeList {
				if inner := exprElemType(v); inner != innerElem {
					return nil, fmt.Errorf("list literal element %d: first element is list<%s>, this is list<%s>", i, innerElem, inner)
				}
			}
		}
		elems = append(elems, v)
	}
	return &aotir.ListLit{ElemType: elemType, ElemRecordName: elemRec, InnerElemType: innerElem, Elems: elems}, nil
}

// lowerMapLit lowers a `{ k1: v1, k2: v2, ... }` literal into a typed
// MapLit. The key type is taken from the first key, the value type
// from the first value; subsequent entries must match. Empty map
// literals are rejected here; the `let m: map<K,V> = {}` typed-empty
// form is handled upstream in lowerBinding (Phase 3.4c) before
// lowerExpr is called. Phase 3.2 also rejects struct-literal-shaped
// maps (the shorthand `{ name: x }` the parser accepts as a struct
// literal); fixtures must use the `{ "name": x }` form. Phase 3.4e
// widens the value type to list<V> where V is a scalar primitive.
func (l *lowerer) lowerMapLit(ml *parser.MapLiteral) (aotir.Expr, error) {
	if len(ml.Items) == 0 {
		return nil, fmt.Errorf("empty map literal: Phase 3.2 requires at least one entry so the key + value types can be inferred")
	}
	keys := make([]aotir.Expr, 0, len(ml.Items))
	values := make([]aotir.Expr, 0, len(ml.Items))
	var keyType, valueType aotir.Type
	var listValueElemType aotir.Type
	for i, e := range ml.Items {
		if e == nil || e.Key == nil || e.Value == nil {
			return nil, fmt.Errorf("map literal entry %d: nil key or value", i)
		}
		k, err := l.lowerExpr(e.Key)
		if err != nil {
			return nil, fmt.Errorf("map literal key %d: %w", i, err)
		}
		v, err := l.lowerExpr(e.Value)
		if err != nil {
			return nil, fmt.Errorf("map literal value %d: %w", i, err)
		}
		if i == 0 {
			keyType = k.Type()
			switch keyType {
			case aotir.TypeInt, aotir.TypeString:
				// ok
			default:
				return nil, fmt.Errorf("map literal key %d: unsupported key type %s (Phase 3.2 supports int or string keys only)", i, keyType)
			}
			valueType = v.Type()
			switch valueType {
			case aotir.TypeInt, aotir.TypeFloat, aotir.TypeBool, aotir.TypeString:
				// ok
			case aotir.TypeList:
				// Phase 3.4e: list<V> where V is a scalar primitive.
				listValueElemType = exprElemType(v)
				switch listValueElemType {
				case aotir.TypeInt, aotir.TypeFloat, aotir.TypeBool, aotir.TypeString:
					// ok
				default:
					return nil, fmt.Errorf("map literal value %d: map<_,list<%s>> inner type not supported (Phase 3.4e requires scalar inner)", i, listValueElemType)
				}
			default:
				return nil, fmt.Errorf("map literal value %d: unsupported value type %s (Phase 3.4e supports scalar or list<scalar> values)", i, valueType)
			}
		} else {
			if k.Type() != keyType {
				return nil, fmt.Errorf("map literal key %d: first key is %s, this is %s", i, keyType, k.Type())
			}
			if v.Type() != valueType {
				return nil, fmt.Errorf("map literal value %d: first value is %s, this is %s", i, valueType, v.Type())
			}
			if valueType == aotir.TypeList {
				if inner := exprElemType(v); inner != listValueElemType {
					return nil, fmt.Errorf("map literal value %d: first value is list<%s>, this is list<%s>", i, listValueElemType, inner)
				}
			}
		}
		keys = append(keys, k)
		values = append(values, v)
	}
	return &aotir.MapLit{
		KeyType:           keyType,
		ValueType:         valueType,
		ListValueElemType: listValueElemType,
		Keys:              keys,
		Values:            values,
	}, nil
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
// position; the lowerer rejects them explicitly. Phase 3.1 routes
// the list builtins `len` and `append` here as well; Phase 3.2 adds
// the map builtins `keys`, `values`, and `has`. The builtins are
// recognised by name and lowered to their dedicated IR nodes.
func (l *lowerer) lowerUserCallExpr(call *parser.CallExpr) (aotir.Expr, error) {
	if call.Func == "print" {
		return nil, fmt.Errorf("print() returns unit and cannot appear in an expression")
	}
	if call.Func == "len" {
		return l.lowerLenCall(call)
	}
	if call.Func == "append" {
		return l.lowerAppendCall(call)
	}
	if call.Func == "keys" {
		return l.lowerKeysCall(call)
	}
	if call.Func == "values" {
		return l.lowerValuesCall(call)
	}
	if call.Func == "has" {
		return l.lowerHasCall(call)
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
		Func:                    call.Func,
		Args:                    args,
		Result:                  sig.returnType,
		ResultRecordName:        sig.returnRecordName,
		ResultElemType:          sig.returnElemType,
		ResultElemRecordName:    sig.returnElemRecord,
		ResultInnerElemType:     sig.returnInnerElem,
		ResultKeyType:           sig.returnKeyType,
		ResultValueType:         sig.returnValueType,
		ResultListValueElemType: sig.returnListValElem,
	}, nil
}

// lowerLenCall lowers the `len(xs)` builtin. Phase 3.1 covered list
// receivers (LenExpr); Phase 3.2 widens to map receivers (MapLenExpr).
// String `len` lands with Phase 3.5.
func (l *lowerer) lowerLenCall(call *parser.CallExpr) (aotir.Expr, error) {
	if len(call.Args) != 1 {
		return nil, fmt.Errorf("len() takes exactly one argument, got %d", len(call.Args))
	}
	receiver, err := l.lowerExpr(call.Args[0])
	if err != nil {
		return nil, fmt.Errorf("len argument: %w", err)
	}
	switch receiver.Type() {
	case aotir.TypeList:
		elem := exprElemType(receiver)
		var inner aotir.Type
		if elem == aotir.TypeList {
			inner = exprInnerElemType(receiver)
		}
		return &aotir.LenExpr{
			Receiver:       receiver,
			ElemType:       elem,
			ElemRecordName: exprElemRecordName(receiver),
			InnerElemType:  inner,
		}, nil
	case aotir.TypeMap:
		return &aotir.MapLenExpr{
			Receiver:          receiver,
			KeyType:           exprKeyType(receiver),
			ValueType:         exprValueType(receiver),
			ListValueElemType: exprListValueElemType(receiver),
		}, nil
	}
	return nil, fmt.Errorf("len() argument must be a list or map in Phase 3.2, got %s", receiver.Type())
}

// lowerKeysCall lowers the `keys(m)` builtin to a MapKeysExpr. The
// receiver must be map-typed; the result is a list of the map's K
// in sorted order (the runtime helper sorts on snapshot).
func (l *lowerer) lowerKeysCall(call *parser.CallExpr) (aotir.Expr, error) {
	if len(call.Args) != 1 {
		return nil, fmt.Errorf("keys() takes exactly one argument, got %d", len(call.Args))
	}
	receiver, err := l.lowerExpr(call.Args[0])
	if err != nil {
		return nil, fmt.Errorf("keys argument: %w", err)
	}
	if receiver.Type() != aotir.TypeMap {
		return nil, fmt.Errorf("keys() argument must be a map, got %s", receiver.Type())
	}
	return &aotir.MapKeysExpr{
		Receiver:          receiver,
		KeyType:           exprKeyType(receiver),
		ValueType:         exprValueType(receiver),
		ListValueElemType: exprListValueElemType(receiver),
	}, nil
}

// lowerValuesCall lowers `values(m)` to a MapValuesExpr. Result is
// list<V> in the same sorted-by-key order as keys(m). For
// map<K,list<V>>, the result is list<list<V>> and ListValueElemType
// carries the inner V (Phase 3.4e).
func (l *lowerer) lowerValuesCall(call *parser.CallExpr) (aotir.Expr, error) {
	if len(call.Args) != 1 {
		return nil, fmt.Errorf("values() takes exactly one argument, got %d", len(call.Args))
	}
	receiver, err := l.lowerExpr(call.Args[0])
	if err != nil {
		return nil, fmt.Errorf("values argument: %w", err)
	}
	if receiver.Type() != aotir.TypeMap {
		return nil, fmt.Errorf("values() argument must be a map, got %s", receiver.Type())
	}
	return &aotir.MapValuesExpr{
		Receiver:          receiver,
		KeyType:           exprKeyType(receiver),
		ValueType:         exprValueType(receiver),
		ListValueElemType: exprListValueElemType(receiver),
	}, nil
}

// lowerHasCall lowers `has(m, k)` to a MapHasExpr. Result is bool.
// Phase 3.2 reuses Mochi's `in` operator with arguments-flipped (m
// holds k); a real `in` operator lands in a later phase, this is the
// minimum surface to let fixtures probe a key before unwrapping a
// MapGetExpr.
func (l *lowerer) lowerHasCall(call *parser.CallExpr) (aotir.Expr, error) {
	if len(call.Args) != 2 {
		return nil, fmt.Errorf("has() takes exactly two arguments (map, key), got %d", len(call.Args))
	}
	receiver, err := l.lowerExpr(call.Args[0])
	if err != nil {
		return nil, fmt.Errorf("has map: %w", err)
	}
	if receiver.Type() != aotir.TypeMap {
		return nil, fmt.Errorf("has() first argument must be a map, got %s", receiver.Type())
	}
	key, err := l.lowerExpr(call.Args[1])
	if err != nil {
		return nil, fmt.Errorf("has key: %w", err)
	}
	recvKey := exprKeyType(receiver)
	if key.Type() != recvKey {
		return nil, fmt.Errorf("has() key must be %s, got %s", recvKey, key.Type())
	}
	return &aotir.MapHasExpr{
		Receiver:          receiver,
		Key:               key,
		KeyType:           recvKey,
		ValueType:         exprValueType(receiver),
		ListValueElemType: exprListValueElemType(receiver),
	}, nil
}

// lowerAppendCall lowers the `append(xs, v)` builtin to an
// AppendExpr. The value's type must match the list's element type;
// the lowerer rejects a mismatch with a phase-named diagnostic.
func (l *lowerer) lowerAppendCall(call *parser.CallExpr) (aotir.Expr, error) {
	if len(call.Args) != 2 {
		return nil, fmt.Errorf("append() takes exactly two arguments (list, value), got %d", len(call.Args))
	}
	receiver, err := l.lowerExpr(call.Args[0])
	if err != nil {
		return nil, fmt.Errorf("append list: %w", err)
	}
	if receiver.Type() != aotir.TypeList {
		return nil, fmt.Errorf("append() first argument must be a list, got %s", receiver.Type())
	}
	elem := exprElemType(receiver)
	elemRec := exprElemRecordName(receiver)
	var innerElem aotir.Type
	if elem == aotir.TypeList {
		innerElem = exprInnerElemType(receiver)
	}
	value, err := l.lowerExpr(call.Args[1])
	if err != nil {
		return nil, fmt.Errorf("append value: %w", err)
	}
	if value.Type() != elem {
		return nil, fmt.Errorf("append: list element type is %s, value is %s", elem, value.Type())
	}
	if elem == aotir.TypeRecord {
		if vrec := exprRecordName(value); vrec != elemRec {
			return nil, fmt.Errorf("append: list element is record %q, value is record %q", elemRec, vrec)
		}
	}
	if elem == aotir.TypeList {
		if vinner := exprElemType(value); vinner != innerElem {
			return nil, fmt.Errorf("append: list element is list<%s>, value is list<%s>", innerElem, vinner)
		}
	}
	return &aotir.AppendExpr{
		Receiver:       receiver,
		Value:          value,
		ElemType:       elem,
		ElemRecordName: elemRec,
		InnerElemType:  innerElem,
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
// is known. Phase 3.2 added maps; 4.x adds fun-expressions. The hint
// is appended to the rejection diagnostic so users see both the
// current floor and the future ceiling.
func primaryPhaseHint(pr *parser.Primary) string {
	switch {
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
