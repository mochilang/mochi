package lower

import (
	"fmt"
	"math"
	"sort"
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

	// Pass 0: collect every `type T { ... }` declaration. Record and union
	// names are registered before sig-building so a fun signature
	// or a record/variant-field type can reference any declared type without
	// regard to source order. Field types are resolved in this
	// same pass; the records/unions maps are set membership only at the
	// start, and decls are stamped onto the output program in
	// source order.
	records := map[string]*aotir.RecordDecl{}
	unions := map[string]*aotir.UnionDecl{}
	variantToUnion := map[string]*aotir.UnionDecl{} // populated after union decls are built
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
		if _, dup := unions[td.Name]; dup {
			return nil, fmt.Errorf("transpiler3/c/lower: statement %d: redeclaration of type %q", i, td.Name)
		}
		// Reserve the name so later passes can resolve it.
		if len(td.Variants) > 0 || td.SingleVariant != nil {
			unions[td.Name] = nil
		} else {
			records[td.Name] = nil
		}
		typeDecls = append(typeDecls, td)
	}
	out := &aotir.Program{}
	for _, td := range typeDecls {
		if len(td.Variants) > 0 || td.SingleVariant != nil {
			// Phase 4.0: sum type (union).
			ud, err := buildUnionDecl(records, td)
			if err != nil {
				return nil, fmt.Errorf("transpiler3/c/lower: type %q: %w", td.Name, err)
			}
			unions[td.Name] = ud
			out.Unions = append(out.Unions, ud)
			// Build variant -> union mapping.
			for i := range ud.Variants {
				vd := &ud.Variants[i]
				variantToUnion[vd.Name] = ud
			}
		} else {
			rd, err := buildRecordDecl(records, td)
			if err != nil {
				return nil, fmt.Errorf("transpiler3/c/lower: type %q: %w", td.Name, err)
			}
			records[td.Name] = rd
			out.Records = append(out.Records, rd)
		}
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
		sig, err := buildFuncSig(records, unions, fn)
		if err != nil {
			return nil, fmt.Errorf("transpiler3/c/lower: fun %q: %w", fn.Name, err)
		}
		funcs[fn.Name] = sig
		funDecls = append(funDecls, fn)
	}

	// Phase 5.0: shared anonymous function counter and lifted-function accumulator.
	// Both Pass 2a and Pass 2b lowerers write into the same counter/slice so that
	// __anon_N names are globally unique across the whole translation unit.
	anonCounter := 0
	var liftedFuncs []*aotir.Function
	// Phase 5.2: shared shim-function dedup map so each __shim_<name> is
	// emitted exactly once across the whole translation unit.
	shimFuncs := map[string]bool{}

	// Pass 2a: lower each fun body using the shared funcs table.
	for _, fn := range funDecls {
		sig := funcs[fn.Name]
		l := &lowerer{
			funcs:                      funcs,
			records:                    records,
			unions:                     unions,
			variantToUnion:             variantToUnion,
			scope:                      newLScope(nil),
			currentFnReturn:            sig.returnType,
			currentFnReturnRecord:      sig.returnRecordName,
			currentFnReturnUnion:       sig.returnUnionName,
			currentFnReturnElem:        sig.returnElemType,
			currentFnReturnElemRec:     sig.returnElemRecord,
			currentFnReturnInnerElem:   sig.returnInnerElem,
			currentFnReturnKey:         sig.returnKeyType,
			currentFnReturnValue:       sig.returnValueType,
			currentFnReturnListValElem: sig.returnListValElem,
			anonCounter:                &anonCounter,
			liftedFuncs:                &liftedFuncs,
			shimFuncs:                  &shimFuncs,
		}
		// Seed parameters into the function scope as immutable.
		for _, p := range sig.params {
			l.scope.vars[p.Name] = lbinding{
				t:            p.Type,
				mutable:      false,
				record:       p.RecordName,
				union:        p.UnionName,
				elem:         p.ElemType,
				elemRec:      p.ElemRecordName,
				innerElem:    p.InnerElemType,
				mapElemKey:   p.MapElemKeyType,
				mapElemValue: p.MapElemValueType,
				key:          p.KeyType,
				value:        p.ValueType,
				listValElem:  p.ListValueElemType,
				funSig:       p.FunSig,
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
			ReturnUnionName:         sig.returnUnionName,
			ReturnElemType:          sig.returnElemType,
			ReturnElemRecordName:    sig.returnElemRecord,
			ReturnInnerElemType:     sig.returnInnerElem,
			ReturnMapElemKeyType:    sig.returnMapElemKey,
			ReturnMapElemValueType:  sig.returnMapElemValue,
			ReturnKeyType:           sig.returnKeyType,
			ReturnValueType:         sig.returnValueType,
			ReturnListValueElemType: sig.returnListValElem,
			ReturnFunSig:            sig.returnFunSig,
			Body:                    body,
		})
	}

	// Pass 2b: lower the top-level script (everything that is not
	// a fun or type decl) into main.
	mainBody := &aotir.Block{}
	mainL := &lowerer{
		funcs:           funcs,
		records:         records,
		unions:          unions,
		variantToUnion:  variantToUnion,
		scope:           newLScope(nil),
		currentFnReturn: aotir.TypeUnit,
		anonCounter:     &anonCounter,
		liftedFuncs:     &liftedFuncs,
		shimFuncs:       &shimFuncs,
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

	// Prepend lifted anonymous functions so they appear before any named function
	// that references them. The sort in emit.go will reorder them alphabetically
	// but forward declarations ensure the C compiler accepts any order.
	if len(liftedFuncs) > 0 {
		combined := make([]*aotir.Function, 0, len(liftedFuncs)+len(out.Functions))
		combined = append(combined, liftedFuncs...)
		combined = append(combined, out.Functions...)
		// Re-find main index.
		mainIdx := 0
		for i, fn := range combined {
			if fn.Name == "main" {
				mainIdx = i
				break
			}
		}
		out.Functions = combined
		out.Main = mainIdx
	}

	if err := aotir.Verify(out); err != nil {
		return nil, fmt.Errorf("transpiler3/c/lower: verify: %w", err)
	}
	return out, nil
}

// buildUnionDecl turns a parser.TypeDecl with Variants into an aotir.UnionDecl.
// Phase 4.0 restricts variant fields to scalar primitives (int, float, bool,
// string); nested records and collections land in later sub-phases.
func buildUnionDecl(records map[string]*aotir.RecordDecl, td *parser.TypeDecl) (*aotir.UnionDecl, error) {
	if len(td.Variants) == 0 {
		return nil, fmt.Errorf("buildUnionDecl called on non-union type")
	}
	u := &aotir.UnionDecl{Name: td.Name}
	for tag, v := range td.Variants {
		vd := aotir.VariantDecl{Name: v.Name, Tag: uint8(tag)}
		for _, f := range v.Fields {
			ft, err := scalarVariantFieldType(f.Type)
			if err != nil {
				return nil, fmt.Errorf("variant %q field %q: %w", v.Name, f.Name, err)
			}
			vd.Fields = append(vd.Fields, aotir.VariantField{Name: f.Name, FieldType: ft})
		}
		u.Variants = append(u.Variants, vd)
	}
	return u, nil
}

// scalarVariantFieldType resolves a variant field's TypeRef. Phase 4.0 accepts
// only the four scalar primitives; nested records and collections land later.
func scalarVariantFieldType(ref *parser.TypeRef) (aotir.Type, error) {
	if ref == nil {
		return aotir.TypeInvalid, fmt.Errorf("nil type ref")
	}
	if ref.Simple == nil {
		return aotir.TypeInvalid, fmt.Errorf("variant field type must be a scalar primitive in Phase 4.0 (int, float, bool, string)")
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
	return aotir.TypeInvalid, fmt.Errorf("variant field type %q not supported in Phase 4.0 (scalar primitives only)", *ref.Simple)
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
		tr, err := typeFromRef(records, nil, f.Type)
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
	params               []aotir.Param
	returnType           aotir.Type
	returnRecordName     string
	returnUnionName      string     // union name when returnType==TypeUnion (Phase 4)
	returnElemType       aotir.Type
	returnElemRecord     string     // record name when returnElemType==TypeRecord
	returnInnerElem      aotir.Type // inner elem type when returnElemType==TypeList (Phase 3.4b)
	returnMapElemKey     aotir.Type // map key type when returnElemType==TypeMap (Phase 3.4f)
	returnMapElemValue   aotir.Type // map value type when returnElemType==TypeMap (Phase 3.4f)
	returnKeyType        aotir.Type
	returnValueType      aotir.Type
	returnListValElem    aotir.Type    // inner list elem when returnValueType==TypeList (Phase 3.4e)
	returnFunSig         *aotir.FunSig // function signature when returnType==TypeFun (Phase 5.0/5.1)
}

// buildFuncSig turns a parser.FunStmt into its lower-time signature.
// Both parameter types and return type are required; Mochi accepts
// `fun f(x) { ... }` as inferring from caller context, but Phase 2.2
// requires explicit annotations so the C-AOT monomorpher does not
// have to do inference. Phase 3.0 widens param/return type lookup to
// the records table so user fns can accept and return records. Phase 4.0
// further widens to unions.
func buildFuncSig(records map[string]*aotir.RecordDecl, unions map[string]*aotir.UnionDecl, fn *parser.FunStmt) (*funcSig, error) {
	if fn.Return == nil {
		return nil, fmt.Errorf("fun %q requires an explicit `: T` return type in Phase 2.2", fn.Name)
	}
	retTR, err := typeFromRef(records, unions, fn.Return)
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
		pTR, err := typeFromRef(records, unions, p.Type)
		if err != nil {
			return nil, fmt.Errorf("fun %q param %q: %w", fn.Name, p.Name, err)
		}
		params = append(params, aotir.Param{
			Name:              p.Name,
			Type:              pTR.t,
			RecordName:        pTR.rec,
			UnionName:         pTR.union,
			ElemType:          pTR.elem,
			ElemRecordName:    pTR.elemRec,
			InnerElemType:     pTR.innerElem,
			MapElemKeyType:    pTR.mapElemKey,
			MapElemValueType:  pTR.mapElemValue,
			KeyType:           pTR.key,
			ValueType:         pTR.value,
			ListValueElemType: pTR.listValElem,
			FunSig:            pTR.funSig,
		})
	}
	return &funcSig{
		params:              params,
		returnType:          retTR.t,
		returnRecordName:    retTR.rec,
		returnUnionName:     retTR.union,
		returnElemType:      retTR.elem,
		returnElemRecord:    retTR.elemRec,
		returnInnerElem:     retTR.innerElem,
		returnMapElemKey:    retTR.mapElemKey,
		returnMapElemValue:  retTR.mapElemValue,
		returnKeyType:       retTR.key,
		returnValueType:     retTR.value,
		returnListValElem:   retTR.listValElem,
		returnFunSig:        retTR.funSig,
	}, nil
}

// lowerer carries the per-function scope stack, loop-depth counter,
// and the enclosing function's return type. Mirrors the verifier's
// verifyCtx so the same scoping / typing rules apply at lower time.
type lowerer struct {
	funcs                       map[string]*funcSig
	records                     map[string]*aotir.RecordDecl
	unions                      map[string]*aotir.UnionDecl   // Phase 4: union name -> decl
	variantToUnion              map[string]*aotir.UnionDecl   // Phase 4: variant name -> enclosing union
	scope                       *lscope
	loopDepth                   int
	tempCounter                 int          // for fresh temp variable names in match lowering
	currentBlock                *aotir.Block // block currently being built; used by lowerMatchExpr
	currentFnReturn             aotir.Type
	currentFnReturnRecord       string
	currentFnReturnUnion        string     // union name when currentFnReturn==TypeUnion (Phase 4)
	currentFnReturnElem         aotir.Type
	currentFnReturnElemRec      string     // record name when currentFnReturnElem==TypeRecord
	currentFnReturnInnerElem    aotir.Type // inner elem when currentFnReturnElem==TypeList (Phase 3.4b)
	currentFnReturnKey          aotir.Type
	currentFnReturnValue        aotir.Type
	currentFnReturnListValElem  aotir.Type // inner list elem when returnValue==TypeList (Phase 3.4e)
	// Phase 5.0: anonymous function lifting.
	// anonCounter counts the anonymous functions lifted from this lowerer's
	// context; combined with an outer-level counter it gives globally unique
	// __anon_N names. liftedFuncs accumulates lifted aotir.Functions that
	// are appended to the Program after the parent function is lowered.
	anonCounter *int                  // pointer to shared counter across nested lowerers
	liftedFuncs *[]*aotir.Function    // pointer to shared slice across nested lowerers
	// Phase 5.2: tracks which named-function shims have already been emitted
	// (map key: shim name, e.g. "__shim_double"). Shared across nested lowerers
	// so that each shim is emitted exactly once per translation unit.
	shimFuncs *map[string]bool
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
	union        string     // union name when t==TypeUnion (Phase 4)
	elem         aotir.Type // element type when t==TypeList
	elemRec      string     // element record name when t==TypeList && elem==TypeRecord
	innerElem    aotir.Type // inner element type when t==TypeList && elem==TypeList (Phase 3.4b)
	mapElemKey   aotir.Type // map key type when t==TypeList && elem==TypeMap (Phase 3.4f)
	mapElemValue aotir.Type // map value type when t==TypeList && elem==TypeMap (Phase 3.4f)
	key          aotir.Type    // key type when t==TypeMap
	value        aotir.Type    // value type when t==TypeMap
	listValElem  aotir.Type    // inner list elem when t==TypeMap && value==TypeList (Phase 3.4e)
	funSig       *aotir.FunSig // function signature when t==TypeFun (Phase 5.0)
	// emitName overrides the C identifier emitted for this variable when
	// non-empty. Used by Phase 5.1 capturing closures to make captured
	// variables emit as `__e->fieldname` instead of the original name.
	emitName string
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
	// Track the current output block so lowerMatchExpr (called from expression
	// lowering) can emit LetStmt/MatchStmt into the enclosing block.
	prevBlock := l.currentBlock
	l.currentBlock = out
	defer func() { l.currentBlock = prevBlock }()
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
// Phase 4.0 adds match-as-statement (match with unit arms). Anything
// else (a bare arithmetic expression, a bare variable reference) is
// rejected -- the result has nowhere to go.
func (l *lowerer) lowerExprStmt(out *aotir.Block, es *parser.ExprStmt) error {
	// Phase 4.0: match-as-statement. The match expr lives inside the
	// ExprStmt when the parser surfaces it as a Primary in the ExprStmt.
	if m := exprStmtMatch(es.Expr); m != nil {
		return l.lowerMatch(out, m, "", aotir.TypeInvalid)
	}
	call, err := matchBareCall(es.Expr)
	if err != nil {
		return err
	}
	if call.Func == "print" {
		return l.lowerPrintCall(out, call)
	}
	// Phase 5.0: check if this is a call to a fun-typed variable in scope.
	if b, ok2 := l.scope.lookup(call.Func); ok2 && b.t == aotir.TypeFun {
		if b.funSig == nil {
			return fmt.Errorf("fun-typed variable %q has nil FunSig in scope", call.Func)
		}
		funCallExpr, err := l.lowerFunVarCall(call, b.funSig)
		if err != nil {
			return err
		}
		// Wrap in a CallStmt-equivalent; since FunCallExpr is an Expr not a Stmt,
		// we use a synthetic ReturnStmt... actually we need a way to discard the
		// result. Use a LetStmt with a fresh temp if result is non-unit, or
		// simply emit nothing if unit. For now, reject unit-return fun calls at
		// statement position since we just returned an error above in lowerFunVarCall
		// when returnType==TypeUnit. Non-unit results are discarded via AssignStmt to _
		// but aotir has no discard. We'll add a LetStmt with a mutable temp var.
		// Actually: use a mutable temp binding marked as discard.
		_ = funCallExpr
		return fmt.Errorf("calling a fun-typed variable at statement position (discarding result) is not yet supported in Phase 5.0; call it in expression position (e.g. `let _ = f(x)`)")
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

// exprStmtMatch checks if an ExprStmt wraps a bare match expression.
// The parser surfaces `match x { ... }` as a Primary.Match inside the
// expression tree.
func exprStmtMatch(expr *parser.Expr) *parser.MatchExpr {
	if expr == nil || expr.Binary == nil || len(expr.Binary.Right) != 0 {
		return nil
	}
	u := expr.Binary.Left
	if u == nil || len(u.Ops) != 0 || u.Value == nil || len(u.Value.Ops) != 0 {
		return nil
	}
	return u.Value.Target.Match
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
			if sig.params[i].ElemType == aotir.TypeMap {
				if argMK := exprMapElemKeyType(expr); argMK != sig.params[i].MapElemKeyType {
					return nil, fmt.Errorf("call %q arg %d: expected list<map<%s,_>>, got list<map<%s,_>>",
						call.Func, i, sig.params[i].MapElemKeyType, argMK)
				}
				if argMV := exprMapElemValueType(expr); argMV != sig.params[i].MapElemValueType {
					return nil, fmt.Errorf("call %q arg %d: expected list<map<_,%s>>, got list<map<_,%s>>",
						call.Func, i, sig.params[i].MapElemValueType, argMV)
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
		// Phase 5.0: TypeFun parameter check.
		if sig.params[i].Type == aotir.TypeFun {
			// expr must be TypeFun (a FunLit or a VarRef{TypeFun}).
			// The type equality check above (expr.Type() != sig.params[i].Type)
			// already ensures expr.Type()==TypeFun. Additional sig compatibility
			// is deferred; Phase 5.0 uses structural typing for FunSig.
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

// exprUnionName extracts the union-name identity of a union-typed
// aotir expression. Used to propagate the union identity through
// let/assign/return type-checks and LetStmt.UnionName stamping.
func exprUnionName(e aotir.Expr) string {
	switch v := e.(type) {
	case *aotir.UnionVarRef:
		return v.UnionName
	case *aotir.VariantLit:
		return v.UnionName
	case *aotir.CallExpr:
		return v.ResultUnionName
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
	case *aotir.ListSortAscExpr:
		return v.ElemRecordName
	case *aotir.ListSliceExpr:
		return v.ElemRecordName
	case *aotir.IndexExpr:
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
	case *aotir.ListSortAscExpr:
		return v.ElemType
	case *aotir.ListSliceExpr:
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
	case *aotir.StrSplitExpr:
		return aotir.TypeString // split() always returns list<string>
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
	case *aotir.ListSortAscExpr:
		return v.InnerElemType
	case *aotir.ListSliceExpr:
		return v.InnerElemType
	case *aotir.IndexExpr:
		return v.InnerElemType
	case *aotir.MapValuesExpr:
		return v.ListValueElemType
	}
	return aotir.TypeInvalid
}

// exprKeyType extracts the key type of a map-typed aotir expression.
// Mirrors the verifier helper of the same name. Phase 3.4f adds
// IndexExpr: indexing a list<map<K,V>> produces a map whose key type
// is on MapElemKeyType.
func exprKeyType(e aotir.Expr) aotir.Type {
	switch v := e.(type) {
	case *aotir.VarRef:
		return v.KeyType
	case *aotir.MapLit:
		return v.KeyType
	case *aotir.CallExpr:
		return v.ResultKeyType
	case *aotir.IndexExpr:
		if v.ElemType == aotir.TypeMap {
			return v.MapElemKeyType
		}
	}
	return aotir.TypeInvalid
}

// exprValueType extracts the value type of a map-typed aotir
// expression. Mirrors the verifier helper of the same name.
// Phase 3.4f adds IndexExpr.
func exprValueType(e aotir.Expr) aotir.Type {
	switch v := e.(type) {
	case *aotir.VarRef:
		return v.ValueType
	case *aotir.MapLit:
		return v.ValueType
	case *aotir.CallExpr:
		return v.ResultValueType
	case *aotir.IndexExpr:
		if v.ElemType == aotir.TypeMap {
			return v.MapElemValueType
		}
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

// exprMapElemKeyType extracts the key type of a map element from a
// list<map<K,V>>-typed expression. Only meaningful when the expression's
// Type() is TypeList and ElemType==TypeMap (Phase 3.4f).
func exprMapElemKeyType(e aotir.Expr) aotir.Type {
	switch v := e.(type) {
	case *aotir.VarRef:
		return v.MapElemKeyType
	case *aotir.ListLit:
		return v.MapElemKeyType
	case *aotir.CallExpr:
		return v.ResultMapElemKeyType
	case *aotir.AppendExpr:
		return v.MapElemKeyType
	case *aotir.ListSortAscExpr:
		return v.MapElemKeyType
	case *aotir.ListSliceExpr:
		return v.MapElemKeyType
	}
	return aotir.TypeInvalid
}

// exprMapElemValueType extracts the value type of a map element from a
// list<map<K,V>>-typed expression. Only meaningful when the expression's
// Type() is TypeList and ElemType==TypeMap (Phase 3.4f).
func exprMapElemValueType(e aotir.Expr) aotir.Type {
	switch v := e.(type) {
	case *aotir.VarRef:
		return v.MapElemValueType
	case *aotir.ListLit:
		return v.MapElemValueType
	case *aotir.CallExpr:
		return v.ResultMapElemValueType
	case *aotir.AppendExpr:
		return v.MapElemValueType
	case *aotir.ListSortAscExpr:
		return v.MapElemValueType
	case *aotir.ListSliceExpr:
		return v.MapElemValueType
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
			tr, err := typeFromRef(l.records, l.unions, declared)
			if err != nil {
				return fmt.Errorf("binding %q type: %w", name, err)
			}
			if tr.t != aotir.TypeList {
				return fmt.Errorf("binding %q: declared type is %s but init is an empty list literal", name, tr.t)
			}
			lit := &aotir.ListLit{ElemType: tr.elem, ElemRecordName: tr.elemRec, InnerElemType: tr.innerElem, MapElemKeyType: tr.mapElemKey, MapElemValueType: tr.mapElemValue}
			l.scope.vars[name] = lbinding{t: aotir.TypeList, mutable: mutable, elem: tr.elem, elemRec: tr.elemRec, innerElem: tr.innerElem, mapElemKey: tr.mapElemKey, mapElemValue: tr.mapElemValue}
			out.Statements = append(out.Statements, &aotir.LetStmt{
				Name: name, VarType: aotir.TypeList, ElemType: tr.elem,
				ElemRecordName: tr.elemRec, InnerElemType: tr.innerElem,
				MapElemKeyType: tr.mapElemKey, MapElemValueType: tr.mapElemValue,
				Init: lit, Mutable: mutable,
			})
			return nil
		}
		if isEmptyMapLit(init) {
			tr, err := typeFromRef(l.records, l.unions, declared)
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
	// Phase 5.1: if the init is a capturing FunLit, emit the env allocation
	// statement immediately before the LetStmt that binds the closure value.
	if fl, ok := value.(*aotir.FunLit); ok && len(fl.Captures) > 0 {
		out.Statements = append(out.Statements, &aotir.ClosureEnvStmt{
			EnvTypeName: fl.EnvTypeName,
			EnvVarName:  fl.EnvVarName,
			Captures:    fl.Captures,
		})
	}
	declType := value.Type()
	declRec := exprRecordName(value)
	declElem := exprElemType(value)
	declElemRec := exprElemRecordName(value)
	declInnerElem := exprInnerElemType(value)
	declMapElemKey := exprMapElemKeyType(value)
	declMapElemValue := exprMapElemValueType(value)
	declKey := exprKeyType(value)
	declValue := exprValueType(value)
	declListValElem := exprListValueElemType(value)
	// declUnion carries the union name when declType==TypeUnion.
	declUnion := exprUnionName(value)
	// Phase 5.0: declFunSig carries the fun signature when declType==TypeFun.
	declFunSig := exprFunSig(value)
	if declared != nil {
		tr, err := typeFromRef(l.records, l.unions, declared)
		if err != nil {
			return fmt.Errorf("binding %q type: %w", name, err)
		}
		if tr.t != declType {
			return fmt.Errorf("binding %q: declared %s, init produces %s", name, tr.t, declType)
		}
		if tr.t == aotir.TypeRecord && tr.rec != declRec {
			return fmt.Errorf("binding %q: declared record %q, init produces record %q", name, tr.rec, declRec)
		}
		if tr.t == aotir.TypeUnion && tr.union != declUnion {
			return fmt.Errorf("binding %q: declared union %q, init produces union %q", name, tr.union, declUnion)
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
		if tr.t == aotir.TypeList && tr.elem == aotir.TypeMap {
			if tr.mapElemKey != declMapElemKey {
				return fmt.Errorf("binding %q: declared list<map<%s,_>>, init produces list<map<%s,_>>", name, tr.mapElemKey, declMapElemKey)
			}
			if tr.mapElemValue != declMapElemValue {
				return fmt.Errorf("binding %q: declared list<map<_,%s>>, init produces list<map<_,%s>>", name, tr.mapElemValue, declMapElemValue)
			}
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
		declUnion = tr.union
		declElem = tr.elem
		declElemRec = tr.elemRec
		declInnerElem = tr.innerElem
		declMapElemKey = tr.mapElemKey
		declMapElemValue = tr.mapElemValue
		declKey = tr.key
		declValue = tr.value
		declListValElem = tr.listValElem
		if tr.funSig != nil {
			declFunSig = tr.funSig
		}
	}
	l.scope.vars[name] = lbinding{
		t:            declType,
		mutable:      mutable,
		record:       declRec,
		union:        declUnion,
		elem:         declElem,
		elemRec:      declElemRec,
		innerElem:    declInnerElem,
		mapElemKey:   declMapElemKey,
		mapElemValue: declMapElemValue,
		key:          declKey,
		value:        declValue,
		listValElem:  declListValElem,
		funSig:       declFunSig,
	}
	out.Statements = append(out.Statements, &aotir.LetStmt{
		Name:              name,
		VarType:           declType,
		RecordName:        declRec,
		UnionName:         declUnion,
		ElemType:          declElem,
		ElemRecordName:    declElemRec,
		InnerElemType:     declInnerElem,
		MapElemKeyType:    declMapElemKey,
		MapElemValueType:  declMapElemValue,
		KeyType:           declKey,
		ValueType:         declValue,
		ListValueElemType: declListValElem,
		FunSig:            declFunSig,
		Init:              value,
		Mutable:           mutable,
	})
	return nil
}

// lowerAssign handles `NAME = expr` and `NAME[i] = expr`.
// Field targets remain unsupported (records are value-semantics).
func (l *lowerer) lowerAssign(out *aotir.Block, as *parser.AssignStmt) error {
	if len(as.Index) != 0 {
		return l.lowerIndexAssign(out, as)
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
		if b.elem == aotir.TypeMap {
			if vmk := exprMapElemKeyType(value); vmk != b.mapElemKey {
				return fmt.Errorf("assign %q: binding holds list<map<%s,_>>, value produces list<map<%s,_>>", as.Name, b.mapElemKey, vmk)
			}
			if vmv := exprMapElemValueType(value); vmv != b.mapElemValue {
				return fmt.Errorf("assign %q: binding holds list<map<_,%s>>, value produces list<map<_,%s>>", as.Name, b.mapElemValue, vmv)
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

// lowerIndexAssign handles `NAME[i] = expr` for list and map receivers.
func (l *lowerer) lowerIndexAssign(out *aotir.Block, as *parser.AssignStmt) error {
	if len(as.Index) != 1 {
		return fmt.Errorf("chained index assignment (xs[i][j] = v) not supported")
	}
	idx := as.Index[0]
	if idx.Colon != nil {
		return fmt.Errorf("slice assignment (xs[a:b] = ...) not supported")
	}
	if idx.Start == nil {
		return fmt.Errorf("index assignment requires an index expression")
	}
	b, ok := l.scope.lookup(as.Name)
	if !ok {
		return fmt.Errorf("assignment to undeclared %q", as.Name)
	}
	if !b.mutable {
		return fmt.Errorf("assignment to immutable %q", as.Name)
	}
	switch b.t {
	case aotir.TypeList:
		idxExpr, err := l.lowerExpr(idx.Start)
		if err != nil {
			return fmt.Errorf("list-set %q index: %w", as.Name, err)
		}
		if idxExpr.Type() != aotir.TypeInt {
			return fmt.Errorf("list index must be int, got %s", idxExpr.Type())
		}
		valExpr, err := l.lowerExpr(as.Value)
		if err != nil {
			return fmt.Errorf("list-set %q value: %w", as.Name, err)
		}
		if valExpr.Type() != b.elem {
			return fmt.Errorf("list-set %q: binding elem %s, value %s", as.Name, b.elem, valExpr.Type())
		}
		out.Statements = append(out.Statements, &aotir.ListSetStmt{
			Name:             as.Name,
			Index:            idxExpr,
			Value:            valExpr,
			ElemType:         b.elem,
			ElemRecordName:   b.elemRec,
			InnerElemType:    b.innerElem,
			MapElemKeyType:   b.mapElemKey,
			MapElemValueType: b.mapElemValue,
		})
		return nil
	case aotir.TypeMap:
		keyExpr, err := l.lowerExpr(idx.Start)
		if err != nil {
			return fmt.Errorf("map-put %q key: %w", as.Name, err)
		}
		if keyExpr.Type() != b.key {
			return fmt.Errorf("map-put %q: binding key %s, got %s", as.Name, b.key, keyExpr.Type())
		}
		valExpr, err := l.lowerExpr(as.Value)
		if err != nil {
			return fmt.Errorf("map-put %q value: %w", as.Name, err)
		}
		if valExpr.Type() != b.value {
			return fmt.Errorf("map-put %q: binding value %s, got %s", as.Name, b.value, valExpr.Type())
		}
		out.Statements = append(out.Statements, &aotir.MapPutStmt{
			Name:      as.Name,
			Key:       keyExpr,
			Value:     valExpr,
			KeyType:   b.key,
			ValueType: b.value,
		})
		return nil
	default:
		return fmt.Errorf("index assignment to %s %q not supported", b.t, as.Name)
	}
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
	var mapElemKey, mapElemValue aotir.Type
	switch source.Type() {
	case aotir.TypeList:
		listExpr = source
		elem = exprElemType(source)
		elemRec = exprElemRecordName(source)
		if elem == aotir.TypeList {
			innerElem = exprInnerElemType(source)
		}
		if elem == aotir.TypeMap {
			mapElemKey = exprMapElemKeyType(source)
			mapElemValue = exprMapElemValueType(source)
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
	// When iterating list<map<K,V>>, the induction variable is a map<K,V>;
	// the binding's key and value carry K and V.
	bindElem := elem
	bindElemRec := elemRec
	bindInnerElem := aotir.TypeInvalid
	var bindKey, bindValue aotir.Type
	if elem == aotir.TypeList {
		bindElem = innerElem
		bindElemRec = ""
	}
	if elem == aotir.TypeMap {
		bindKey = mapElemKey
		bindValue = mapElemValue
	}
	l.scope.vars[fs.Name] = lbinding{t: elem, mutable: false, record: elemRec, elem: bindElem, elemRec: bindElemRec, innerElem: bindInnerElem, key: bindKey, value: bindValue}
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
		Var:              fs.Name,
		List:             listExpr,
		ElemType:         elem,
		ElemRecordName:   elemRec,
		InnerElemType:    innerElem,
		MapElemKeyType:   mapElemKey,
		MapElemValueType: mapElemValue,
		Body:             body,
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
	if l.currentFnReturn == aotir.TypeUnion {
		if vunion := exprUnionName(value); vunion != l.currentFnReturnUnion {
			return fmt.Errorf("return: function returns union %q, value produces union %q",
				l.currentFnReturnUnion, vunion)
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
// Phase 3.4f adds mapElemKey/mapElemValue, valid when t==TypeList && elem==TypeMap.
type typeResolution struct {
	t            aotir.Type
	rec          string
	union        string        // valid when t==TypeUnion (Phase 4)
	elem         aotir.Type
	elemRec      string        // valid when elem==TypeRecord (Phase 3.4a)
	innerElem    aotir.Type    // valid when elem==TypeList (Phase 3.4b)
	mapElemKey   aotir.Type    // valid when t==TypeList && elem==TypeMap (Phase 3.4f)
	mapElemValue aotir.Type    // valid when t==TypeList && elem==TypeMap (Phase 3.4f)
	key          aotir.Type
	value        aotir.Type
	listValElem  aotir.Type    // valid when t==TypeMap && value==TypeList (Phase 3.4e)
	funSig       *aotir.FunSig // valid when t==TypeFun (Phase 5.0)
}

// typeFromRef maps a parser.TypeRef to a typeResolution. Phase 3.2
// accepts:
//
//   - the four scalar primitives,
//   - any user-declared record name,
//   - `[T]` or `list<T>` where T is one of the four scalar primitives,
//   - `map<K,V>` where K is int or string and V is one of the four
//     scalar primitives.
//
// Phase 4.0 additionally accepts any user-declared union name (a sum type
// declared with variants). The unions map may be nil when called from
// contexts that predate Phase 4 (e.g. buildRecordDecl field types).
func typeFromRef(records map[string]*aotir.RecordDecl, unions map[string]*aotir.UnionDecl, ref *parser.TypeRef) (typeResolution, error) {
	if ref == nil {
		return typeResolution{}, fmt.Errorf("nil type ref")
	}
	if ref.Optional {
		return typeResolution{}, fmt.Errorf("optional types land with Option in a later phase")
	}
	// Phase 5.0: fun(T1, T2, ...): R type annotation.
	if ref.Fun != nil {
		sig := &aotir.FunSig{}
		for i, pt := range ref.Fun.Params {
			tr, err := typeFromRef(records, unions, pt)
			if err != nil {
				return typeResolution{}, fmt.Errorf("fun param %d: %w", i, err)
			}
			switch tr.t {
			case aotir.TypeInt, aotir.TypeFloat, aotir.TypeBool, aotir.TypeString:
				sig.ParamTypes = append(sig.ParamTypes, tr.t)
			default:
				return typeResolution{}, fmt.Errorf("fun param type %s not supported in Phase 5.0 (scalar primitives only: int, float, bool, string)", tr.t)
			}
		}
		if ref.Fun.Return != nil {
			rtr, err := typeFromRef(records, unions, ref.Fun.Return)
			if err != nil {
				return typeResolution{}, fmt.Errorf("fun return: %w", err)
			}
			switch rtr.t {
			case aotir.TypeInt, aotir.TypeFloat, aotir.TypeBool, aotir.TypeString, aotir.TypeUnit:
				sig.ReturnType = rtr.t
			default:
				return typeResolution{}, fmt.Errorf("fun return type %s not supported in Phase 5.0 (scalar primitives or unit only)", rtr.t)
			}
		} else {
			sig.ReturnType = aotir.TypeUnit
		}
		return typeResolution{t: aotir.TypeFun, funSig: sig}, nil
	}
	if ref.ListElem != nil {
		elem, elemRec, innerElem, mapKey, mapVal, err := listElemFromRef(records, unions, ref.ListElem)
		if err != nil {
			return typeResolution{}, err
		}
		return typeResolution{t: aotir.TypeList, elem: elem, elemRec: elemRec, innerElem: innerElem, mapElemKey: mapKey, mapElemValue: mapVal}, nil
	}
	if ref.Generic != nil {
		switch ref.Generic.Name {
		case "list":
			if len(ref.Generic.Args) != 1 {
				return typeResolution{}, fmt.Errorf("list<T> takes exactly one type argument, got %d", len(ref.Generic.Args))
			}
			elem, elemRec, innerElem, mapKey, mapVal, err := listElemFromRef(records, unions, ref.Generic.Args[0])
			if err != nil {
				return typeResolution{}, err
			}
			return typeResolution{t: aotir.TypeList, elem: elem, elemRec: elemRec, innerElem: innerElem, mapElemKey: mapKey, mapElemValue: mapVal}, nil
		case "map":
			if len(ref.Generic.Args) != 2 {
				return typeResolution{}, fmt.Errorf("map<K,V> takes exactly two type arguments, got %d", len(ref.Generic.Args))
			}
			key, err := mapKeyFromRef(records, unions, ref.Generic.Args[0])
			if err != nil {
				return typeResolution{}, err
			}
			value, listValElem, err := mapValueFromRef(records, unions, ref.Generic.Args[1])
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
	if unions != nil {
		if _, ok := unions[*ref.Simple]; ok {
			return typeResolution{t: aotir.TypeUnion, union: *ref.Simple}, nil
		}
	}
	return typeResolution{}, fmt.Errorf("type %q not supported in Phase 4.0", *ref.Simple)
}

// listElemFromRef resolves a list's element TypeRef. Phase 3.1
// accepts the four scalar primitives. Phase 3.4a widens this to
// accept TypeRecord (user-declared records). Phase 3.4b widens it
// once more to accept TypeList where the inner element is a scalar
// primitive, returning the inner element type in the third result so
// callers can stamp InnerElemType onto the IR carrier. Phase 3.4f
// widens it to accept TypeMap where both key and value are scalars;
// the key and value types are returned in the 4th and 5th result.
// Three-level nesting (list<list<list<T>>>) is still rejected here.
// Returns (elemType, elemRecName, innerElem, mapElemKey, mapElemValue, error).
func listElemFromRef(records map[string]*aotir.RecordDecl, unions map[string]*aotir.UnionDecl, ref *parser.TypeRef) (aotir.Type, string, aotir.Type, aotir.Type, aotir.Type, error) {
	tr, err := typeFromRef(records, unions, ref)
	if err != nil {
		return aotir.TypeInvalid, "", aotir.TypeInvalid, aotir.TypeInvalid, aotir.TypeInvalid, fmt.Errorf("list element: %w", err)
	}
	switch tr.t {
	case aotir.TypeInt, aotir.TypeFloat, aotir.TypeBool, aotir.TypeString:
		return tr.t, "", aotir.TypeInvalid, aotir.TypeInvalid, aotir.TypeInvalid, nil
	case aotir.TypeRecord:
		return aotir.TypeRecord, tr.rec, aotir.TypeInvalid, aotir.TypeInvalid, aotir.TypeInvalid, nil
	case aotir.TypeList:
		// Inner must be a scalar primitive in Phase 3.4b.
		switch tr.elem {
		case aotir.TypeInt, aotir.TypeFloat, aotir.TypeBool, aotir.TypeString:
			return aotir.TypeList, "", tr.elem, aotir.TypeInvalid, aotir.TypeInvalid, nil
		case aotir.TypeRecord:
			return aotir.TypeInvalid, "", aotir.TypeInvalid, aotir.TypeInvalid, aotir.TypeInvalid, fmt.Errorf("list<list<record>> is not supported in Phase 3.4b (lands with a later sub-phase)")
		case aotir.TypeList:
			return aotir.TypeInvalid, "", aotir.TypeInvalid, aotir.TypeInvalid, aotir.TypeInvalid, fmt.Errorf("3-level nested lists (list<list<list<T>>>) are not supported in Phase 3.4b")
		}
		return aotir.TypeInvalid, "", aotir.TypeInvalid, aotir.TypeInvalid, aotir.TypeInvalid, fmt.Errorf("list<list<%s>> not supported in Phase 3.4b", tr.elem)
	case aotir.TypeMap:
		// Phase 3.4f: list<map<K,V>> where K is int/string and V is a scalar.
		switch tr.key {
		case aotir.TypeInt, aotir.TypeString:
			// ok
		default:
			return aotir.TypeInvalid, "", aotir.TypeInvalid, aotir.TypeInvalid, aotir.TypeInvalid, fmt.Errorf("list<map<K,V>> requires int or string key, got %s", tr.key)
		}
		switch tr.value {
		case aotir.TypeInt, aotir.TypeFloat, aotir.TypeBool, aotir.TypeString:
			// ok
		default:
			return aotir.TypeInvalid, "", aotir.TypeInvalid, aotir.TypeInvalid, aotir.TypeInvalid, fmt.Errorf("list<map<K,V>> requires scalar value type, got %s (Phase 3.4f does not support list<map<K,list<V>>>)", tr.value)
		}
		return aotir.TypeMap, "", aotir.TypeInvalid, tr.key, tr.value, nil
	}
	return aotir.TypeInvalid, "", aotir.TypeInvalid, aotir.TypeInvalid, aotir.TypeInvalid, fmt.Errorf("list element type %s not supported in Phase 3.4b", tr.t)
}

// mapKeyFromRef resolves a map's key TypeRef. Phase 3.2 accepts only
// int and string keys (the two key types the runtime ships helpers
// for); other element types fail with a phase-named diagnostic.
func mapKeyFromRef(records map[string]*aotir.RecordDecl, unions map[string]*aotir.UnionDecl, ref *parser.TypeRef) (aotir.Type, error) {
	tr, err := typeFromRef(records, unions, ref)
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
func mapValueFromRef(records map[string]*aotir.RecordDecl, unions map[string]*aotir.UnionDecl, ref *parser.TypeRef) (aotir.Type, aotir.Type, error) {
	tr, err := typeFromRef(records, unions, ref)
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

// exprFunSig extracts the FunSig from a fun-typed aotir expression.
// Phase 5.0 covers FunLit and VarRef{TypeFun}.
// Phase 5.1 adds CallExpr{Result=TypeFun} for functions that return closures.
func exprFunSig(e aotir.Expr) *aotir.FunSig {
	switch v := e.(type) {
	case *aotir.FunLit:
		return v.Sig
	case *aotir.VarRef:
		if v.VarType == aotir.TypeFun {
			return v.FunSig
		}
	case *aotir.CallExpr:
		if v.Result == aotir.TypeFun {
			return v.ResultFunSig
		}
	}
	return nil
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
	if t == aotir.TypeFun {
		return "", fmt.Errorf("print() does not accept a fun value in Phase 5.0")
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
		if op.Op == "in" && right.Type() == aotir.TypeList {
			elem := exprElemType(right)
			switch elem {
			case aotir.TypeInt, aotir.TypeFloat, aotir.TypeBool, aotir.TypeString:
			default:
				return nil, fmt.Errorf("`in` list: element type must be scalar, got %s", elem)
			}
			if left.Type() != elem {
				return nil, fmt.Errorf("`in` list: value type is %s, list element type is %s", left.Type(), elem)
			}
			left = &aotir.ListContainsExpr{
				List:     right,
				Value:    left,
				ElemType: elem,
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
		if lhs == aotir.TypeString && rhs == aotir.TypeString && opStr == "+" {
			return aotir.BinStrCat, aotir.TypeString, nil
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
			// Phase 6.1: complete a string method call (e.g. s.contains("x")).
			sm, ok := expr.(*aotir.StrMethodRef)
			if !ok {
				return nil, fmt.Errorf("postfix call on a non-string-method expression is not supported (Phase 3.1)")
			}
			expr, err = l.lowerStrMethodCallOp(sm, op.Call)
			if err != nil {
				return nil, err
			}
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
	// Phase 3.4g: xs[start:end] slice notation on list receivers.
	if idx.Colon != nil && idx.Step == nil && receiver.Type() == aotir.TypeList {
		return l.lowerListSliceOp(receiver, idx)
	}
	if idx.Colon != nil || idx.Colon2 != nil || idx.End != nil || idx.Step != nil {
		return nil, fmt.Errorf("slice / step indexing on non-list or with step lands in a later phase")
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
		// For list<map<K,V>> (Phase 3.4f): the produced IndexExpr is
		// a map<K,V> value; MapElemKeyType and MapElemValueType carry
		// K and V so subsequent map operations can resolve helpers.
		producedElem := exprElemType(receiver)
		var producedInner aotir.Type
		if producedElem == aotir.TypeList {
			producedInner = exprInnerElemType(receiver)
		}
		var producedMapKey, producedMapValue aotir.Type
		if producedElem == aotir.TypeMap {
			producedMapKey = exprMapElemKeyType(receiver)
			producedMapValue = exprMapElemValueType(receiver)
		}
		return &aotir.IndexExpr{
			Receiver:         receiver,
			Index:            index,
			ElemType:         producedElem,
			ElemRecordName:   exprElemRecordName(receiver),
			InnerElemType:    producedInner,
			MapElemKeyType:   producedMapKey,
			MapElemValueType: producedMapValue,
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
	case aotir.TypeString:
		index, err := l.lowerExpr(idx.Start)
		if err != nil {
			return nil, fmt.Errorf("string index expression: %w", err)
		}
		if index.Type() != aotir.TypeInt {
			return nil, fmt.Errorf("string index must be int, got %s", index.Type())
		}
		return &aotir.StrIndexExpr{Receiver: receiver, Index: index}, nil
	}
	return nil, fmt.Errorf("index access [k]: receiver is %s, expected a list, map, or string", receiver.Type())
}

// lowerListSliceOp lowers `xs[start:end]` to a ListSliceExpr.
// start defaults to 0 when absent; end defaults to a large sentinel
// (INT62) when absent (the runtime clamps to the actual list length).
func (l *lowerer) lowerListSliceOp(receiver aotir.Expr, idx *parser.IndexOp) (aotir.Expr, error) {
	elemType := exprElemType(receiver)
	elemRecord := exprElemRecordName(receiver)
	innerElem := exprInnerElemType(receiver)
	mapKey := exprMapElemKeyType(receiver)
	mapVal := exprMapElemValueType(receiver)

	var startExpr aotir.Expr = &aotir.IntLit{Value: 0}
	if idx.Start != nil {
		s, err := l.lowerExpr(idx.Start)
		if err != nil {
			return nil, fmt.Errorf("slice start: %w", err)
		}
		if s.Type() != aotir.TypeInt {
			return nil, fmt.Errorf("slice start must be int, got %s", s.Type())
		}
		startExpr = s
	}
	var endExpr aotir.Expr = &aotir.IntLit{Value: 1<<62 - 1}
	if idx.End != nil {
		e, err := l.lowerExpr(idx.End)
		if err != nil {
			return nil, fmt.Errorf("slice end: %w", err)
		}
		if e.Type() != aotir.TypeInt {
			return nil, fmt.Errorf("slice end must be int, got %s", e.Type())
		}
		endExpr = e
	}
	return &aotir.ListSliceExpr{
		Receiver:         receiver,
		Start:            startExpr,
		End:              endExpr,
		ElemType:         elemType,
		ElemRecordName:   elemRecord,
		InnerElemType:    innerElem,
		MapElemKeyType:   mapKey,
		MapElemValueType: mapVal,
	}, nil
}

// lowerFieldOp resolves a `.field` against a record-typed receiver and
// returns a FieldAccess node typed by the field's declared type. Phase 6.1
// extends it to TypeString receivers: .contains produces a StrMethodRef
// (resolved to StrContainsExpr by lowerPostfix when the CallOp arrives).
func (l *lowerer) lowerFieldOp(receiver aotir.Expr, fieldName string) (aotir.Expr, error) {
	if receiver.Type() == aotir.TypeString {
		switch fieldName {
		case "contains":
			return &aotir.StrMethodRef{Receiver: receiver, MethodName: fieldName}, nil
		default:
			return nil, fmt.Errorf("string has no field %q (Phase 6.1 supports: contains)", fieldName)
		}
	}
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

// scanFreeVarNames walks the parser FunExpr body and returns, in sorted
// order, the names of all identifiers that are referenced in the body
// but not defined inside it (parameters or let/var declarations). These
// are the candidates for capture from the enclosing scope.
//
// The scanner does NOT recurse into nested FunExpr nodes: a nested
// closure creates its own capture chain at lowering time.
func scanFreeVarNames(fe *parser.FunExpr, paramNames map[string]bool) []string {
	refs := map[string]bool{}
	locals := map[string]bool{}
	for n := range paramNames {
		locals[n] = true
	}
	if fe.ExprBody != nil {
		freeVarCollectExpr(fe.ExprBody, refs)
	}
	for _, st := range fe.BlockBody {
		freeVarCollectStmt(st, refs, locals)
	}
	var free []string
	for n := range refs {
		if !locals[n] {
			free = append(free, n)
		}
	}
	sort.Strings(free)
	return free
}

func freeVarCollectExpr(e *parser.Expr, refs map[string]bool) {
	if e == nil || e.Binary == nil {
		return
	}
	freeVarCollectUnary(e.Binary.Left, refs)
	for _, op := range e.Binary.Right {
		freeVarCollectUnary(op.Right, refs)
	}
}

func freeVarCollectUnary(u *parser.Unary, refs map[string]bool) {
	if u == nil || u.Value == nil {
		return
	}
	freeVarCollectPostfix(u.Value, refs)
}

func freeVarCollectPostfix(pf *parser.PostfixExpr, refs map[string]bool) {
	if pf == nil {
		return
	}
	freeVarCollectPrimary(pf.Target, refs)
	for _, op := range pf.Ops {
		if op.Call != nil {
			for _, arg := range op.Call.Args {
				freeVarCollectExpr(arg, refs)
			}
		}
		if op.Index != nil && op.Index.Start != nil {
			freeVarCollectExpr(op.Index.Start, refs)
		}
	}
}

func freeVarCollectPrimary(pr *parser.Primary, refs map[string]bool) {
	if pr == nil {
		return
	}
	if pr.Selector != nil {
		refs[pr.Selector.Root] = true
		// Don't recurse into Tail -- .field accesses aren't variable refs.
	}
	if pr.Group != nil {
		freeVarCollectExpr(pr.Group, refs)
	}
	if pr.Call != nil {
		for _, arg := range pr.Call.Args {
			freeVarCollectExpr(arg, refs)
		}
	}
	if pr.List != nil {
		for _, el := range pr.List.Elems {
			freeVarCollectExpr(el, refs)
		}
	}
	// Do NOT recurse into pr.FunExpr: nested closures form their own capture chain.
	if pr.If != nil {
		freeVarCollectIfExpr(pr.If, refs)
	}
}

func freeVarCollectIfExpr(ie *parser.IfExpr, refs map[string]bool) {
	if ie == nil {
		return
	}
	freeVarCollectExpr(ie.Cond, refs)
	freeVarCollectExpr(ie.Then, refs)
	freeVarCollectExpr(ie.Else, refs)
}

func freeVarCollectStmt(st *parser.Statement, refs map[string]bool, locals map[string]bool) {
	if st == nil {
		return
	}
	if st.Let != nil {
		if st.Let.Value != nil {
			freeVarCollectExpr(st.Let.Value, refs)
		}
		locals[st.Let.Name] = true
	}
	if st.Var != nil {
		if st.Var.Value != nil {
			freeVarCollectExpr(st.Var.Value, refs)
		}
		locals[st.Var.Name] = true
	}
	if st.Assign != nil {
		refs[st.Assign.Name] = true
		for _, ix := range st.Assign.Index {
			if ix.Start != nil {
				freeVarCollectExpr(ix.Start, refs)
			}
		}
		freeVarCollectExpr(st.Assign.Value, refs)
	}
	if st.Return != nil {
		freeVarCollectExpr(st.Return.Value, refs)
	}
	if st.Expr != nil {
		freeVarCollectExpr(st.Expr.Expr, refs)
	}
	if st.If != nil {
		freeVarCollectExpr(st.If.Cond, refs)
		for _, s := range st.If.Then {
			freeVarCollectStmt(s, refs, locals)
		}
		for _, s := range st.If.Else {
			freeVarCollectStmt(s, refs, locals)
		}
		if st.If.ElseIf != nil {
			freeVarCollectStmtIfChain(st.If.ElseIf, refs, locals)
		}
	}
	if st.While != nil {
		freeVarCollectExpr(st.While.Cond, refs)
		for _, s := range st.While.Body {
			freeVarCollectStmt(s, refs, locals)
		}
	}
	if st.For != nil {
		locals[st.For.Name] = true
		freeVarCollectExpr(st.For.Source, refs)
		if st.For.RangeEnd != nil {
			freeVarCollectExpr(st.For.RangeEnd, refs)
		}
		for _, s := range st.For.Body {
			freeVarCollectStmt(s, refs, locals)
		}
	}
}

func freeVarCollectStmtIfChain(ie *parser.IfStmt, refs map[string]bool, locals map[string]bool) {
	if ie == nil {
		return
	}
	freeVarCollectExpr(ie.Cond, refs)
	for _, s := range ie.Then {
		freeVarCollectStmt(s, refs, locals)
	}
	for _, s := range ie.Else {
		freeVarCollectStmt(s, refs, locals)
	}
	if ie.ElseIf != nil {
		freeVarCollectStmtIfChain(ie.ElseIf, refs, locals)
	}
}

// lowerFunExpr lifts a FunExpr (anonymous function literal) into a
// top-level aotir.Function and returns a FunLit pointing to it.
// Phase 5.0 supports non-capturing closures; Phase 5.1 extends to
// capturing closures by detecting free variables and emitting an env
// struct that the lifted function receives as void *__mochi_env.
func (l *lowerer) lowerFunExpr(fe *parser.FunExpr) (aotir.Expr, error) {
	if fe == nil {
		return nil, fmt.Errorf("nil FunExpr")
	}
	if len(fe.Effects) != 0 {
		return nil, fmt.Errorf("fun expressions with effects are not supported in Phase 5.0")
	}
	if len(fe.TypeParams) != 0 {
		return nil, fmt.Errorf("generic fun expressions are not supported in Phase 5.0")
	}
	if fe.Return == nil {
		return nil, fmt.Errorf("fun expression requires an explicit ': T' return type annotation in Phase 5.0")
	}
	// Build the FunSig from the param and return type annotations.
	sig := &aotir.FunSig{}
	type paramInfo struct {
		name string
		t    aotir.Type
	}
	params := make([]paramInfo, 0, len(fe.Params))
	seen := map[string]bool{}
	for i, p := range fe.Params {
		if p.Name == "" {
			return nil, fmt.Errorf("fun expression param %d has empty name", i)
		}
		if seen[p.Name] {
			return nil, fmt.Errorf("fun expression duplicate parameter %q", p.Name)
		}
		seen[p.Name] = true
		if p.Type == nil {
			return nil, fmt.Errorf("fun expression param %q requires an explicit ': T' type annotation in Phase 5.0", p.Name)
		}
		tr, err := typeFromRef(l.records, l.unions, p.Type)
		if err != nil {
			return nil, fmt.Errorf("fun expression param %q type: %w", p.Name, err)
		}
		switch tr.t {
		case aotir.TypeInt, aotir.TypeFloat, aotir.TypeBool, aotir.TypeString:
			// ok
		default:
			return nil, fmt.Errorf("fun expression param %q type %s not supported in Phase 5.0 (scalar primitives only)", p.Name, tr.t)
		}
		sig.ParamTypes = append(sig.ParamTypes, tr.t)
		params = append(params, paramInfo{name: p.Name, t: tr.t})
	}
	rtr, err := typeFromRef(l.records, l.unions, fe.Return)
	if err != nil {
		return nil, fmt.Errorf("fun expression return type: %w", err)
	}
	switch rtr.t {
	case aotir.TypeInt, aotir.TypeFloat, aotir.TypeBool, aotir.TypeString, aotir.TypeUnit:
		// ok
	default:
		return nil, fmt.Errorf("fun expression return type %s not supported in Phase 5.0 (scalar primitives or unit only)", rtr.t)
	}
	sig.ReturnType = rtr.t

	// Assign a fresh __anon_N name.
	if l.anonCounter == nil {
		return nil, fmt.Errorf("fun expression encountered outside a properly initialized lowerer (anonCounter is nil)")
	}
	*l.anonCounter++
	n := *l.anonCounter
	name := fmt.Sprintf("__anon_%d", n)

	// Phase 5.1: detect free variables (variables referenced in the body
	// but not in the closure's own parameter list). Each free var that
	// resolves in the enclosing scope becomes a captured variable.
	paramNameSet := make(map[string]bool, len(params))
	for _, p := range params {
		paramNameSet[p.name] = true
	}
	freeNames := scanFreeVarNames(fe, paramNameSet)

	// Resolve each free name against the enclosing scope. Names that are
	// not in the enclosing scope are ignored (they may be builtins, type
	// names, etc.); names that resolve are captures.
	var captures []aotir.FunCapture
	captureBindings := map[string]lbinding{} // emitName-keyed for the inner scope
	for _, freeName := range freeNames {
		b, ok := l.scope.lookup(freeName)
		if !ok {
			// Not a free variable from the enclosing scope (builtin, etc.).
			continue
		}
		// Only scalar primitive captures in Phase 5.1.
		switch b.t {
		case aotir.TypeInt, aotir.TypeFloat, aotir.TypeBool, aotir.TypeString:
			// ok
		default:
			return nil, fmt.Errorf("capturing closure: captured variable %q has type %s; only scalar primitives (int, float, bool, string) are supported in Phase 5.1", freeName, b.t)
		}
		captures = append(captures, aotir.FunCapture{
			FieldName: freeName,
			VarType:   b.t,
			SrcName:   freeName,
		})
		// In the inner scope, the captured var emits as __e->fieldname.
		captureBindings[freeName] = lbinding{
			t:        b.t,
			emitName: "__e->" + freeName,
		}
	}

	// Derive env type name (only relevant for capturing closures).
	var envTypeName, envVarName string
	if len(captures) > 0 {
		envTypeName = fmt.Sprintf("__anon_%d_env_t", n)
		envVarName = fmt.Sprintf("__anon_%d_env", n)
	}

	// Build a fresh inner lowerer for the fun body.
	inner := &lowerer{
		funcs:           l.funcs,
		records:         l.records,
		unions:          l.unions,
		variantToUnion:  l.variantToUnion,
		scope:           newLScope(nil), // fresh scope, no outer chain
		currentFnReturn: sig.ReturnType,
		anonCounter:     l.anonCounter,
		liftedFuncs:     l.liftedFuncs,
		shimFuncs:       l.shimFuncs,
	}
	// Seed the fun's own parameters.
	for _, p := range params {
		inner.scope.vars[p.name] = lbinding{t: p.t, mutable: false}
	}
	// Seed captured variables with env-relative emit names.
	for name, b := range captureBindings {
		inner.scope.vars[name] = b
	}

	// Build aotir.Params.
	irParams := make([]aotir.Param, len(params))
	for i, p := range params {
		irParams[i] = aotir.Param{Name: p.name, Type: p.t}
	}

	// Lower the body.
	body := &aotir.Block{}
	if fe.ExprBody != nil {
		// `fun(x): T => expr` lowers as a single return statement.
		val, err := inner.lowerExpr(fe.ExprBody)
		if err != nil {
			return nil, fmt.Errorf("fun expression body: %w", err)
		}
		if val.Type() != sig.ReturnType {
			return nil, fmt.Errorf("fun expression body produces %s, but return type is %s", val.Type(), sig.ReturnType)
		}
		body.Statements = append(body.Statements, &aotir.ReturnStmt{Value: val})
	} else {
		// Block body.
		for i, st := range fe.BlockBody {
			if st == nil {
				return nil, fmt.Errorf("fun expression body stmt %d is nil", i)
			}
			if err := inner.lowerStatement(body, st); err != nil {
				return nil, fmt.Errorf("fun expression body stmt %d: %w", i, err)
			}
		}
	}

	// Build the lifted function.
	lifted := &aotir.Function{
		Name:        name,
		Params:      irParams,
		ReturnType:  sig.ReturnType,
		Body:        body,
		IsLifted:    true,
		EnvTypeName: envTypeName,
		Captures:    captures,
	}
	*l.liftedFuncs = append(*l.liftedFuncs, lifted)

	lit := &aotir.FunLit{
		FuncName:    name,
		Sig:         sig,
		Captures:    captures,
		EnvTypeName: envTypeName,
		EnvVarName:  envVarName,
	}
	return lit, nil
}

// lowerFunRef lifts a bare reference to a named top-level function into a
// non-capturing closure shim (Phase 5.2). It emits a thin __shim_<name>
// function that accepts void *__mochi_env (ignored) and forwards to the
// real named function. The returned FunLit has EnvVarName="" so the
// compound literal carries env=NULL.
//
// Each shim is emitted at most once per translation unit (shimFuncs dedup).
func (l *lowerer) lowerFunRef(funcName string, sig *funcSig) (aotir.Expr, error) {
	shimName := "__shim_" + funcName

	// Build FunSig (scalar primitives only in Phase 5.2).
	funSig := &aotir.FunSig{ReturnType: sig.returnType}
	for _, p := range sig.params {
		switch p.Type {
		case aotir.TypeInt, aotir.TypeFloat, aotir.TypeBool, aotir.TypeString:
		default:
			return nil, fmt.Errorf("free function ref %q: param type %s not supported in Phase 5.2 (scalar primitives only)", funcName, p.Type)
		}
		funSig.ParamTypes = append(funSig.ParamTypes, p.Type)
	}
	switch sig.returnType {
	case aotir.TypeInt, aotir.TypeFloat, aotir.TypeBool, aotir.TypeString, aotir.TypeUnit:
	default:
		return nil, fmt.Errorf("free function ref %q: return type %s not supported in Phase 5.2 (scalar primitives or unit only)", funcName, sig.returnType)
	}

	// Emit the shim function exactly once (dedup via shimFuncs).
	if l.shimFuncs != nil && !(*l.shimFuncs)[shimName] {
		(*l.shimFuncs)[shimName] = true

		// Build shim params and forwarding call args.
		irParams := make([]aotir.Param, len(sig.params))
		args := make([]aotir.Expr, len(sig.params))
		for i, p := range sig.params {
			irParams[i] = aotir.Param{Name: p.Name, Type: p.Type}
			args[i] = &aotir.VarRef{Name: p.Name, VarType: p.Type}
		}

		// Build shim body: forward the call to the real function.
		body := &aotir.Block{}
		if sig.returnType == aotir.TypeUnit {
			body.Statements = append(body.Statements, &aotir.CallStmt{
				Func: funcName,
				Args: args,
			})
		} else {
			body.Statements = append(body.Statements, &aotir.ReturnStmt{
				Value: &aotir.CallExpr{
					Func:   funcName,
					Args:   args,
					Result: sig.returnType,
				},
			})
		}

		shim := &aotir.Function{
			Name:       shimName,
			Params:     irParams,
			ReturnType: sig.returnType,
			Body:       body,
			IsLifted:   true,
		}
		*l.liftedFuncs = append(*l.liftedFuncs, shim)
	}

	return &aotir.FunLit{
		FuncName: shimName,
		Sig:      funSig,
	}, nil
}

// lowerPrimary lowers a Primary into either a literal, a parenthesised
// expression, a variable reference, a record literal, a selector
// chain (variable + zero or more `.field` reads), or a call to a user
// function. Phase 4.0 adds variant constructors and match expressions.
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
	if pr.Match != nil {
		return l.lowerMatchExpr(pr.Match)
	}
	if pr.Selector != nil {
		// Phase 4.0: check if this is a unit variant (no fields) used as a value.
		if len(pr.Selector.Tail) == 0 {
			if ud, ok := l.variantToUnion[pr.Selector.Root]; ok {
				for i := range ud.Variants {
					vd := &ud.Variants[i]
					if vd.Name == pr.Selector.Root && len(vd.Fields) == 0 {
						return &aotir.VariantLit{
							UnionName:   ud.Name,
							VariantName: vd.Name,
							Tag:         vd.Tag,
						}, nil
					}
				}
			}
		}
		b, ok := l.scope.lookup(pr.Selector.Root)
		if !ok {
			// Phase 5.2: bare reference to a named top-level function used as
			// a fun-typed value. Generate (or reuse) a __shim_<name> wrapper.
			if fnSig, isFn := l.funcs[pr.Selector.Root]; isFn && len(pr.Selector.Tail) == 0 {
				return l.lowerFunRef(pr.Selector.Root, fnSig)
			}
			return nil, fmt.Errorf("undeclared variable %q", pr.Selector.Root)
		}
		var expr aotir.Expr
		if b.t == aotir.TypeUnion {
			name := pr.Selector.Root
			if b.emitName != "" {
				name = b.emitName
			}
			expr = &aotir.UnionVarRef{
				Name:      name,
				UnionName: b.union,
			}
		} else {
			name := pr.Selector.Root
			if b.emitName != "" {
				name = b.emitName
			}
			expr = &aotir.VarRef{
				Name:              name,
				VarType:           b.t,
				RecordName:        b.record,
				ElemType:          b.elem,
				ElemRecordName:    b.elemRec,
				InnerElemType:     b.innerElem,
				MapElemKeyType:    b.mapElemKey,
				MapElemValueType:  b.mapElemValue,
				KeyType:           b.key,
				ValueType:         b.value,
				ListValueElemType: b.listValElem,
			}
		}
		for _, field := range pr.Selector.Tail {
			var err error
			if expr.Type() == aotir.TypeUnion {
				return nil, fmt.Errorf("field access on union-typed value requires a match expression (field .%s on union)", field)
			}
			expr, err = l.lowerFieldOp(expr, field)
			if err != nil {
				return nil, err
			}
		}
		return expr, nil
	}
	if pr.Call != nil {
		// Phase 4.0: check if this is a field-bearing variant constructor.
		if ud, ok := l.variantToUnion[pr.Call.Func]; ok {
			return l.lowerVariantConstructor(pr.Call, ud)
		}
		return l.lowerUserCallExpr(pr.Call)
	}
	if pr.List != nil {
		return l.lowerListLit(pr.List)
	}
	if pr.Map != nil {
		return l.lowerMapLit(pr.Map)
	}
	if pr.FunExpr != nil {
		return l.lowerFunExpr(pr.FunExpr)
	}
	if pr.Query != nil {
		return l.lowerQueryExpr(pr.Query)
	}
	return nil, fmt.Errorf("primary %s not supported in Phase 3.2%s", trimPrimary(pr), primaryPhaseHint(pr))
}

// lowerVariantConstructor lowers a call-expression that names a known
// variant, e.g. `Circle(5.0)`, into a VariantLit node.
func (l *lowerer) lowerVariantConstructor(call *parser.CallExpr, ud *aotir.UnionDecl) (aotir.Expr, error) {
	// Find the variant declaration.
	var vd *aotir.VariantDecl
	for i := range ud.Variants {
		if ud.Variants[i].Name == call.Func {
			vd = &ud.Variants[i]
			break
		}
	}
	if vd == nil {
		return nil, fmt.Errorf("variant %q not found in union %q", call.Func, ud.Name)
	}
	if len(call.Args) != len(vd.Fields) {
		return nil, fmt.Errorf("variant %q expects %d fields, got %d", call.Func, len(vd.Fields), len(call.Args))
	}
	fields := make([]aotir.VariantLitArg, 0, len(call.Args))
	for i, arg := range call.Args {
		v, err := l.lowerExpr(arg)
		if err != nil {
			return nil, fmt.Errorf("variant %q field %d: %w", call.Func, i, err)
		}
		if v.Type() != vd.Fields[i].FieldType {
			return nil, fmt.Errorf("variant %q field %q: expected %s, got %s",
				call.Func, vd.Fields[i].Name, vd.Fields[i].FieldType, v.Type())
		}
		fields = append(fields, aotir.VariantLitArg{Name: vd.Fields[i].Name, Value: v})
	}
	return &aotir.VariantLit{
		UnionName:   ud.Name,
		VariantName: vd.Name,
		Tag:         vd.Tag,
		Fields:      fields,
	}, nil
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
	var mapElemKey, mapElemValue aotir.Type
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
			case aotir.TypeMap:
				// Phase 3.4f: list<map<K,V>> where K is int/string and V is a scalar.
				mapElemKey = exprKeyType(v)
				mapElemValue = exprValueType(v)
				switch mapElemKey {
				case aotir.TypeInt, aotir.TypeString:
					// ok
				default:
					return nil, fmt.Errorf("list literal element %d: list<map<K,V>> requires int or string key, got %s", i, mapElemKey)
				}
				switch mapElemValue {
				case aotir.TypeInt, aotir.TypeFloat, aotir.TypeBool, aotir.TypeString:
					// ok
				default:
					return nil, fmt.Errorf("list literal element %d: list<map<K,V>> requires scalar value type, got %s", i, mapElemValue)
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
			if elemType == aotir.TypeMap {
				if k := exprKeyType(v); k != mapElemKey {
					return nil, fmt.Errorf("list literal element %d: first element is map<%s,_>, this is map<%s,_>", i, mapElemKey, k)
				}
				if val := exprValueType(v); val != mapElemValue {
					return nil, fmt.Errorf("list literal element %d: first element is map<_,%s>, this is map<_,%s>", i, mapElemValue, val)
				}
			}
		}
		elems = append(elems, v)
	}
	return &aotir.ListLit{ElemType: elemType, ElemRecordName: elemRec, InnerElemType: innerElem, MapElemKeyType: mapElemKey, MapElemValueType: mapElemValue, Elems: elems}, nil
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
	if call.Func == "substring" {
		return l.lowerSubstringCall(call)
	}
	if call.Func == "reverse" {
		return l.lowerReverseCall(call)
	}
	// Phase 6.3: string case-conversion and split/join.
	if call.Func == "upper" {
		if _, isUserDef := l.funcs[call.Func]; !isUserDef {
			return l.lowerStrUpperCall(call)
		}
	}
	if call.Func == "lower" {
		if _, isUserDef := l.funcs[call.Func]; !isUserDef {
			return l.lowerStrLowerCall(call)
		}
	}
	if call.Func == "split" {
		if _, isUserDef := l.funcs[call.Func]; !isUserDef {
			return l.lowerStrSplitCall(call)
		}
	}
	if call.Func == "join" {
		if _, isUserDef := l.funcs[call.Func]; !isUserDef {
			return l.lowerStrJoinCall(call)
		}
	}
	if call.Func == "str" {
		return l.lowerStrConvertCall(call)
	}
	if call.Func == "int" {
		if _, isUserDef := l.funcs[call.Func]; !isUserDef {
			return l.lowerIntCastCall(call)
		}
	}
	if call.Func == "min" {
		if _, isUserDef := l.funcs[call.Func]; !isUserDef {
			return l.lowerListMinCall(call)
		}
	}
	if call.Func == "max" {
		if _, isUserDef := l.funcs[call.Func]; !isUserDef {
			return l.lowerListMaxCall(call)
		}
	}
	if call.Func == "sum" {
		if _, isUserDef := l.funcs[call.Func]; !isUserDef {
			return l.lowerListSumCall(call)
		}
	}
	if call.Func == "abs" {
		if _, isUserDef := l.funcs[call.Func]; !isUserDef {
			return l.lowerAbsCall(call)
		}
	}
	if call.Func == "floor" {
		if _, isUserDef := l.funcs[call.Func]; !isUserDef {
			return l.lowerFloorCall(call)
		}
	}
	if call.Func == "ceil" {
		if _, isUserDef := l.funcs[call.Func]; !isUserDef {
			return l.lowerCeilCall(call)
		}
	}
	// Phase 5.0: check if this is a call to a fun-typed variable in scope.
	if b, ok := l.scope.lookup(call.Func); ok && b.t == aotir.TypeFun {
		if b.funSig == nil {
			return nil, fmt.Errorf("fun-typed variable %q has nil FunSig in scope", call.Func)
		}
		return l.lowerFunVarCall(call, b.funSig)
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
		ResultUnionName:         sig.returnUnionName,
		ResultElemType:          sig.returnElemType,
		ResultElemRecordName:    sig.returnElemRecord,
		ResultInnerElemType:     sig.returnInnerElem,
		ResultMapElemKeyType:    sig.returnMapElemKey,
		ResultMapElemValueType:  sig.returnMapElemValue,
		ResultKeyType:           sig.returnKeyType,
		ResultValueType:         sig.returnValueType,
		ResultListValueElemType: sig.returnListValElem,
		ResultFunSig:            sig.returnFunSig,
	}, nil
}

// lowerFunVarCall lowers a call to a fun-typed variable. The callee is
// referenced as a VarRef{TypeFun}; args are lowered and type-checked
// against the FunSig's ParamTypes. The result is a FunCallExpr whose
// Result type is sig.ReturnType.
func (l *lowerer) lowerFunVarCall(call *parser.CallExpr, sig *aotir.FunSig) (aotir.Expr, error) {
	if sig.ReturnType == aotir.TypeUnit {
		return nil, fmt.Errorf("call to fun-typed variable %q returns unit and cannot appear in an expression", call.Func)
	}
	if len(call.Args) != len(sig.ParamTypes) {
		return nil, fmt.Errorf("call to %q expects %d args, got %d", call.Func, len(sig.ParamTypes), len(call.Args))
	}
	b, _ := l.scope.lookup(call.Func)
	callee := &aotir.VarRef{Name: call.Func, VarType: aotir.TypeFun, FunSig: b.funSig}
	args := make([]aotir.Expr, 0, len(call.Args))
	for i, a := range call.Args {
		expr, err := l.lowerExpr(a)
		if err != nil {
			return nil, fmt.Errorf("call %q arg %d: %w", call.Func, i, err)
		}
		if expr.Type() != sig.ParamTypes[i] {
			return nil, fmt.Errorf("call %q arg %d: expected %s, got %s", call.Func, i, sig.ParamTypes[i], expr.Type())
		}
		args = append(args, expr)
	}
	return &aotir.FunCallExpr{Callee: callee, Args: args, Result: sig.ReturnType}, nil
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
	case aotir.TypeString:
		return &aotir.StrLenExpr{Receiver: receiver}, nil
	case aotir.TypeList:
		elem := exprElemType(receiver)
		var inner aotir.Type
		if elem == aotir.TypeList {
			inner = exprInnerElemType(receiver)
		}
		var mapKey, mapValue aotir.Type
		if elem == aotir.TypeMap {
			mapKey = exprMapElemKeyType(receiver)
			mapValue = exprMapElemValueType(receiver)
		}
		return &aotir.LenExpr{
			Receiver:         receiver,
			ElemType:         elem,
			ElemRecordName:   exprElemRecordName(receiver),
			InnerElemType:    inner,
			MapElemKeyType:   mapKey,
			MapElemValueType: mapValue,
		}, nil
	case aotir.TypeMap:
		return &aotir.MapLenExpr{
			Receiver:          receiver,
			KeyType:           exprKeyType(receiver),
			ValueType:         exprValueType(receiver),
			ListValueElemType: exprListValueElemType(receiver),
		}, nil
	}
	return nil, fmt.Errorf("len() argument must be a list, map, or string, got %s", receiver.Type())
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

// lowerStrMethodCallOp completes a string method call after the
// lowerFieldOp has produced a StrMethodRef. Phase 6.1 supports
// "contains" which lowers to StrContainsExpr.
func (l *lowerer) lowerStrMethodCallOp(sm *aotir.StrMethodRef, callOp *parser.CallOp) (aotir.Expr, error) {
	switch sm.MethodName {
	case "contains":
		if len(callOp.Args) != 1 {
			return nil, fmt.Errorf("string.contains() takes exactly one argument, got %d", len(callOp.Args))
		}
		sub, err := l.lowerExpr(callOp.Args[0])
		if err != nil {
			return nil, fmt.Errorf("contains arg: %w", err)
		}
		if sub.Type() != aotir.TypeString {
			return nil, fmt.Errorf("string.contains() argument must be string, got %s", sub.Type())
		}
		return &aotir.StrContainsExpr{Receiver: sm.Receiver, Sub: sub}, nil
	default:
		return nil, fmt.Errorf("unknown string method %q", sm.MethodName)
	}
}

// lowerSubstringCall lowers `substring(s, start, end)` to StrSubstringExpr.
func (l *lowerer) lowerSubstringCall(call *parser.CallExpr) (aotir.Expr, error) {
	if len(call.Args) != 3 {
		return nil, fmt.Errorf("substring() takes exactly three arguments (s, start, end), got %d", len(call.Args))
	}
	s, err := l.lowerExpr(call.Args[0])
	if err != nil {
		return nil, fmt.Errorf("substring string: %w", err)
	}
	if s.Type() != aotir.TypeString {
		return nil, fmt.Errorf("substring() first argument must be string, got %s", s.Type())
	}
	start, err := l.lowerExpr(call.Args[1])
	if err != nil {
		return nil, fmt.Errorf("substring start: %w", err)
	}
	if start.Type() != aotir.TypeInt {
		return nil, fmt.Errorf("substring() start must be int, got %s", start.Type())
	}
	end, err := l.lowerExpr(call.Args[2])
	if err != nil {
		return nil, fmt.Errorf("substring end: %w", err)
	}
	if end.Type() != aotir.TypeInt {
		return nil, fmt.Errorf("substring() end must be int, got %s", end.Type())
	}
	return &aotir.StrSubstringExpr{Receiver: s, Start: start, End: end}, nil
}

// lowerReverseCall lowers `reverse(s)` to StrReverseExpr when the
// argument is a string. (reverse(list) is handled by the user-fn path.)
func (l *lowerer) lowerReverseCall(call *parser.CallExpr) (aotir.Expr, error) {
	if len(call.Args) != 1 {
		return nil, fmt.Errorf("reverse() takes exactly one argument, got %d", len(call.Args))
	}
	s, err := l.lowerExpr(call.Args[0])
	if err != nil {
		return nil, fmt.Errorf("reverse arg: %w", err)
	}
	if s.Type() != aotir.TypeString {
		return nil, fmt.Errorf("reverse() argument must be string in Phase 6.1, got %s", s.Type())
	}
	return &aotir.StrReverseExpr{Receiver: s}, nil
}

// lowerStrUpperCall lowers `upper(s)` to StrUpperExpr. Phase 6.3.
func (l *lowerer) lowerStrUpperCall(call *parser.CallExpr) (aotir.Expr, error) {
	if len(call.Args) != 1 {
		return nil, fmt.Errorf("upper() takes exactly one argument, got %d", len(call.Args))
	}
	s, err := l.lowerExpr(call.Args[0])
	if err != nil {
		return nil, fmt.Errorf("upper() arg: %w", err)
	}
	if s.Type() != aotir.TypeString {
		return nil, fmt.Errorf("upper() argument must be string, got %s", s.Type())
	}
	return &aotir.StrUpperExpr{Receiver: s}, nil
}

// lowerStrLowerCall lowers `lower(s)` to StrLowerExpr. Phase 6.3.
func (l *lowerer) lowerStrLowerCall(call *parser.CallExpr) (aotir.Expr, error) {
	if len(call.Args) != 1 {
		return nil, fmt.Errorf("lower() takes exactly one argument, got %d", len(call.Args))
	}
	s, err := l.lowerExpr(call.Args[0])
	if err != nil {
		return nil, fmt.Errorf("lower() arg: %w", err)
	}
	if s.Type() != aotir.TypeString {
		return nil, fmt.Errorf("lower() argument must be string, got %s", s.Type())
	}
	return &aotir.StrLowerExpr{Receiver: s}, nil
}

// lowerStrSplitCall lowers `split(s, sep)` to StrSplitExpr. Returns list<string>.
// Phase 6.3.
func (l *lowerer) lowerStrSplitCall(call *parser.CallExpr) (aotir.Expr, error) {
	if len(call.Args) != 2 {
		return nil, fmt.Errorf("split() takes exactly two arguments, got %d", len(call.Args))
	}
	s, err := l.lowerExpr(call.Args[0])
	if err != nil {
		return nil, fmt.Errorf("split() string arg: %w", err)
	}
	if s.Type() != aotir.TypeString {
		return nil, fmt.Errorf("split() first argument must be string, got %s", s.Type())
	}
	sep, err := l.lowerExpr(call.Args[1])
	if err != nil {
		return nil, fmt.Errorf("split() sep arg: %w", err)
	}
	if sep.Type() != aotir.TypeString {
		return nil, fmt.Errorf("split() second argument must be string, got %s", sep.Type())
	}
	return &aotir.StrSplitExpr{Str: s, Sep: sep}, nil
}

// lowerStrJoinCall lowers `join(xs, sep)` to StrJoinExpr. Expects a
// list<string> as first arg. Phase 6.3.
func (l *lowerer) lowerStrJoinCall(call *parser.CallExpr) (aotir.Expr, error) {
	if len(call.Args) != 2 {
		return nil, fmt.Errorf("join() takes exactly two arguments, got %d", len(call.Args))
	}
	xs, err := l.lowerExpr(call.Args[0])
	if err != nil {
		return nil, fmt.Errorf("join() list arg: %w", err)
	}
	if xs.Type() != aotir.TypeList {
		return nil, fmt.Errorf("join() first argument must be list<string>, got %s", xs.Type())
	}
	sep, err := l.lowerExpr(call.Args[1])
	if err != nil {
		return nil, fmt.Errorf("join() sep arg: %w", err)
	}
	if sep.Type() != aotir.TypeString {
		return nil, fmt.Errorf("join() second argument must be string, got %s", sep.Type())
	}
	return &aotir.StrJoinExpr{List: xs, Sep: sep}, nil
}

// lowerStrConvertCall lowers `str(x)` to StrConvertExpr. Accepts
// int, float, bool, and string operands; string is an identity conversion.
// Phase 6.2.
func (l *lowerer) lowerStrConvertCall(call *parser.CallExpr) (aotir.Expr, error) {
	if len(call.Args) != 1 {
		return nil, fmt.Errorf("str() takes exactly one argument, got %d", len(call.Args))
	}
	operand, err := l.lowerExpr(call.Args[0])
	if err != nil {
		return nil, fmt.Errorf("str() arg: %w", err)
	}
	t := operand.Type()
	if t != aotir.TypeInt && t != aotir.TypeFloat && t != aotir.TypeBool && t != aotir.TypeString {
		return nil, fmt.Errorf("str() argument must be int/float/bool/string, got %s", t)
	}
	return &aotir.StrConvertExpr{Operand: operand}, nil
}

// lowerIntCastCall lowers `int(x)` to a NumCastExpr (float→int truncation)
// or returns the operand directly when it is already an int.
func (l *lowerer) lowerIntCastCall(call *parser.CallExpr) (aotir.Expr, error) {
	if len(call.Args) != 1 {
		return nil, fmt.Errorf("int() takes exactly one argument, got %d", len(call.Args))
	}
	operand, err := l.lowerExpr(call.Args[0])
	if err != nil {
		return nil, fmt.Errorf("int() arg: %w", err)
	}
	switch operand.Type() {
	case aotir.TypeInt:
		return operand, nil
	case aotir.TypeFloat:
		return &aotir.NumCastExpr{Operand: operand}, nil
	default:
		return nil, fmt.Errorf("int() argument must be int or float, got %s", operand.Type())
	}
}

// lowerListMinCall lowers `min(xs)` to a ListMinExpr.
func (l *lowerer) lowerListMinCall(call *parser.CallExpr) (aotir.Expr, error) {
	if len(call.Args) != 1 {
		return nil, fmt.Errorf("min() takes exactly one argument, got %d", len(call.Args))
	}
	recv, err := l.lowerExpr(call.Args[0])
	if err != nil {
		return nil, fmt.Errorf("min() arg: %w", err)
	}
	if recv.Type() != aotir.TypeList {
		return nil, fmt.Errorf("min() argument must be a list, got %s", recv.Type())
	}
	elem := exprElemType(recv)
	if elem != aotir.TypeInt && elem != aotir.TypeFloat && elem != aotir.TypeString {
		return nil, fmt.Errorf("min() list element type must be int/float/string, got %s", elem)
	}
	return &aotir.ListMinExpr{
		Receiver: recv,
		ElemType: elem,
	}, nil
}

// lowerListMaxCall lowers `max(xs)` to a ListMaxExpr.
func (l *lowerer) lowerListMaxCall(call *parser.CallExpr) (aotir.Expr, error) {
	if len(call.Args) != 1 {
		return nil, fmt.Errorf("max() takes exactly one argument, got %d", len(call.Args))
	}
	recv, err := l.lowerExpr(call.Args[0])
	if err != nil {
		return nil, fmt.Errorf("max() arg: %w", err)
	}
	if recv.Type() != aotir.TypeList {
		return nil, fmt.Errorf("max() argument must be a list, got %s", recv.Type())
	}
	elem := exprElemType(recv)
	if elem != aotir.TypeInt && elem != aotir.TypeFloat && elem != aotir.TypeString {
		return nil, fmt.Errorf("max() list element type must be int/float/string, got %s", elem)
	}
	return &aotir.ListMaxExpr{
		Receiver: recv,
		ElemType: elem,
	}, nil
}

// lowerListSumCall lowers `sum(xs)` to a ListSumExpr.
func (l *lowerer) lowerListSumCall(call *parser.CallExpr) (aotir.Expr, error) {
	if len(call.Args) != 1 {
		return nil, fmt.Errorf("sum() takes exactly one argument, got %d", len(call.Args))
	}
	recv, err := l.lowerExpr(call.Args[0])
	if err != nil {
		return nil, fmt.Errorf("sum() arg: %w", err)
	}
	if recv.Type() != aotir.TypeList {
		return nil, fmt.Errorf("sum() argument must be a list, got %s", recv.Type())
	}
	elem := exprElemType(recv)
	if elem != aotir.TypeInt && elem != aotir.TypeFloat {
		return nil, fmt.Errorf("sum() list element type must be int or float, got %s", elem)
	}
	return &aotir.ListSumExpr{
		Receiver: recv,
		ElemType: elem,
	}, nil
}

// lowerAbsCall lowers `abs(x)` to a MathCallExpr.
func (l *lowerer) lowerAbsCall(call *parser.CallExpr) (aotir.Expr, error) {
	if len(call.Args) != 1 {
		return nil, fmt.Errorf("abs() takes exactly one argument, got %d", len(call.Args))
	}
	arg, err := l.lowerExpr(call.Args[0])
	if err != nil {
		return nil, fmt.Errorf("abs() arg: %w", err)
	}
	switch arg.Type() {
	case aotir.TypeInt:
		return &aotir.MathCallExpr{Func: "abs_i64", Arg: arg, Result: aotir.TypeInt}, nil
	case aotir.TypeFloat:
		return &aotir.MathCallExpr{Func: "abs_f64", Arg: arg, Result: aotir.TypeFloat}, nil
	default:
		return nil, fmt.Errorf("abs() argument must be int or float, got %s", arg.Type())
	}
}

// lowerFloorCall lowers `floor(x)` to a MathCallExpr.
func (l *lowerer) lowerFloorCall(call *parser.CallExpr) (aotir.Expr, error) {
	if len(call.Args) != 1 {
		return nil, fmt.Errorf("floor() takes exactly one argument, got %d", len(call.Args))
	}
	arg, err := l.lowerExpr(call.Args[0])
	if err != nil {
		return nil, fmt.Errorf("floor() arg: %w", err)
	}
	if arg.Type() != aotir.TypeFloat {
		return nil, fmt.Errorf("floor() argument must be float, got %s", arg.Type())
	}
	return &aotir.MathCallExpr{Func: "floor", Arg: arg, Result: aotir.TypeFloat}, nil
}

// lowerCeilCall lowers `ceil(x)` to a MathCallExpr.
func (l *lowerer) lowerCeilCall(call *parser.CallExpr) (aotir.Expr, error) {
	if len(call.Args) != 1 {
		return nil, fmt.Errorf("ceil() takes exactly one argument, got %d", len(call.Args))
	}
	arg, err := l.lowerExpr(call.Args[0])
	if err != nil {
		return nil, fmt.Errorf("ceil() arg: %w", err)
	}
	if arg.Type() != aotir.TypeFloat {
		return nil, fmt.Errorf("ceil() argument must be float, got %s", arg.Type())
	}
	return &aotir.MathCallExpr{Func: "ceil", Arg: arg, Result: aotir.TypeFloat}, nil
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
	var mapElemKey, mapElemValue aotir.Type
	if elem == aotir.TypeMap {
		mapElemKey = exprMapElemKeyType(receiver)
		mapElemValue = exprMapElemValueType(receiver)
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
	if elem == aotir.TypeMap {
		if vk := exprKeyType(value); vk != mapElemKey {
			return nil, fmt.Errorf("append: list element is map<%s,_>, value is map<%s,_>", mapElemKey, vk)
		}
		if vval := exprValueType(value); vval != mapElemValue {
			return nil, fmt.Errorf("append: list element is map<_,%s>, value is map<_,%s>", mapElemValue, vval)
		}
	}
	return &aotir.AppendExpr{
		Receiver:         receiver,
		Value:            value,
		ElemType:         elem,
		ElemRecordName:   elemRec,
		InnerElemType:    innerElem,
		MapElemKeyType:   mapElemKey,
		MapElemValueType: mapElemValue,
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

// ---- Phase 4.0: match expression / statement lowering ----

// freshTemp returns a unique variable name for use in match-expression
// result temporaries. The counter is per-lowerer (per-function) so
// names are stable across function boundaries.
func (l *lowerer) freshTemp() string {
	l.tempCounter++
	return fmt.Sprintf("__match%d", l.tempCounter)
}

// callPattern checks if expr is a simple call like `Circle(r)`.
// Used during match arm lowering to detect field-bearing variant patterns.
func callPattern(e *parser.Expr) (*parser.CallExpr, bool) {
	if e == nil || e.Binary == nil || len(e.Binary.Right) != 0 {
		return nil, false
	}
	u := e.Binary.Left
	if u == nil || len(u.Ops) != 0 {
		return nil, false
	}
	p := u.Value
	if p == nil || len(p.Ops) != 0 || p.Target == nil || p.Target.Call == nil {
		return nil, false
	}
	return p.Target.Call, true
}

// identName checks if expr is a simple identifier and returns its name.
// Used during match arm lowering to detect unit variant patterns and wildcards.
func identName(e *parser.Expr) (string, bool) {
	if e == nil || e.Binary == nil || len(e.Binary.Right) != 0 {
		return "", false
	}
	u := e.Binary.Left
	if u == nil || len(u.Ops) != 0 {
		return "", false
	}
	p := u.Value
	if p == nil || len(p.Ops) != 0 || p.Target == nil || p.Target.Selector == nil || len(p.Target.Selector.Tail) != 0 {
		return "", false
	}
	return p.Target.Selector.Root, true
}

// isUnderscoreExpr reports whether e is the wildcard pattern `_`.
func isUnderscoreExpr(e *parser.Expr) bool {
	n, ok := identName(e)
	return ok && n == "_"
}

// lowerQueryExpr lowers a `from x in src [where cond] select expr`
// query expression. Phase 8.0 supports filter+map queries over scalar
// list sources. The approach mirrors lowerMatchExpr: statements are
// emitted into l.currentBlock and a VarRef to a fresh temp list is
// returned as the expression value.
//
// Desugaring:
//   from x in src where cond select expr
// becomes:
//   let __queryN: list<T> = []     (T = type of select expr)
//   for x in src { if cond { __queryN = append(__queryN, expr) } }
// and the expression evaluates to __queryN.
func (l *lowerer) lowerQueryExpr(q *parser.QueryExpr) (aotir.Expr, error) {
	if l.currentBlock == nil {
		return nil, fmt.Errorf("query expression outside a statement block (internal error)")
	}
	// Phase 8.2: cross-join (from) and join clauses are now supported.
	if q.Group != nil {
		return nil, fmt.Errorf("group-by queries land in Phase 8.1")
	}
	// Phase 8.1: Sort, Skip, Take are handled after the main loop below.
	if q.Distinct {
		return nil, fmt.Errorf("distinct queries land in Phase 8.1")
	}

	// Lower the source expression in the current outer scope.
	source, err := l.lowerExpr(q.Source)
	if err != nil {
		return nil, fmt.Errorf("query source: %w", err)
	}
	if source.Type() != aotir.TypeList {
		return nil, fmt.Errorf("query source must be a list, got %s", source.Type())
	}
	sourceElemType := exprElemType(source)
	sourceElemRecord := exprElemRecordName(source)
	sourceInnerElem := exprInnerElemType(source)
	sourceMapKey := exprMapElemKeyType(source)
	sourceMapValue := exprMapElemValueType(source)

	// Lower each from/join right-side source in the current outer scope (before
	// pushing inner vars). Store the lowered source list alongside its elem metadata.
	type joinSrcInfo struct {
		src      aotir.Expr
		elemType aotir.Type
		elemRec  string
		innerElem aotir.Type
		mapKey   aotir.Type
		mapValue aotir.Type
	}
	fromSrcs := make([]joinSrcInfo, len(q.Froms))
	for i, f := range q.Froms {
		fs, err := l.lowerExpr(f.Src)
		if err != nil {
			return nil, fmt.Errorf("query from[%d] source: %w", i, err)
		}
		if fs.Type() != aotir.TypeList {
			return nil, fmt.Errorf("query from source must be a list, got %s", fs.Type())
		}
		fromSrcs[i] = joinSrcInfo{
			src:      fs,
			elemType: exprElemType(fs),
			elemRec:  exprElemRecordName(fs),
			innerElem: exprInnerElemType(fs),
			mapKey:   exprMapElemKeyType(fs),
			mapValue: exprMapElemValueType(fs),
		}
	}
	joinSrcs := make([]joinSrcInfo, len(q.Joins))
	for i, j := range q.Joins {
		js, err := l.lowerExpr(j.Src)
		if err != nil {
			return nil, fmt.Errorf("query join[%d] source: %w", i, err)
		}
		if js.Type() != aotir.TypeList {
			return nil, fmt.Errorf("query join source must be a list, got %s", js.Type())
		}
		joinSrcs[i] = joinSrcInfo{
			src:      js,
			elemType: exprElemType(js),
			elemRec:  exprElemRecordName(js),
			innerElem: exprInnerElemType(js),
			mapKey:   exprMapElemKeyType(js),
			mapValue: exprMapElemValueType(js),
		}
	}

	// Allocate a fresh temp for the result list (in outer scope, mutable).
	l.tempCounter++
	tempName := fmt.Sprintf("__query%d", l.tempCounter)

	// Push inner scope for the outer loop variable plus all from/join vars.
	prev := l.scope
	l.scope = newLScope(prev)
	loopBinding := lbinding{
		t:            sourceElemType,
		record:       sourceElemRecord,
		elem:         sourceInnerElem,
		mapElemKey:   sourceMapKey,
		mapElemValue: sourceMapValue,
	}
	l.scope.vars[q.Var] = loopBinding
	for i, f := range q.Froms {
		si := fromSrcs[i]
		l.scope.vars[f.Var] = lbinding{
			t:            si.elemType,
			record:       si.elemRec,
			elem:         si.innerElem,
			mapElemKey:   si.mapKey,
			mapElemValue: si.mapValue,
		}
	}
	for i, j := range q.Joins {
		si := joinSrcs[i]
		l.scope.vars[j.Var] = lbinding{
			t:            si.elemType,
			record:       si.elemRec,
			elem:         si.innerElem,
			mapElemKey:   si.mapKey,
			mapElemValue: si.mapValue,
		}
	}

	// Lower on-conditions for each join (all vars are in scope at this point).
	joinOns := make([]aotir.Expr, len(q.Joins))
	for i, j := range q.Joins {
		on, err := l.lowerExpr(j.On)
		if err != nil {
			l.scope = prev
			return nil, fmt.Errorf("query join[%d] on: %w", i, err)
		}
		if on.Type() != aotir.TypeBool {
			l.scope = prev
			return nil, fmt.Errorf("query join[%d] on condition must be bool, got %s", i, on.Type())
		}
		joinOns[i] = on
	}

	// Lower the select expression (all vars in scope).
	selectExpr, err := l.lowerExpr(q.Select)
	if err != nil {
		l.scope = prev
		return nil, fmt.Errorf("query select: %w", err)
	}
	selectElemType := selectExpr.Type()

	// Lower the where condition (if any).
	var whereCond aotir.Expr
	if q.Where != nil {
		whereCond, err = l.lowerExpr(q.Where)
		if err != nil {
			l.scope = prev
			return nil, fmt.Errorf("query where: %w", err)
		}
		if whereCond.Type() != aotir.TypeBool {
			l.scope = prev
			return nil, fmt.Errorf("query where condition must be bool, got %s", whereCond.Type())
		}
	}
	l.scope = prev

	// Emit: let __queryN: list<T> = []
	prev.vars[tempName] = lbinding{t: aotir.TypeList, mutable: true, elem: selectElemType}
	l.currentBlock.Statements = append(l.currentBlock.Statements, &aotir.LetStmt{
		Name:     tempName,
		VarType:  aotir.TypeList,
		ElemType: selectElemType,
		Init:     &aotir.ListLit{ElemType: selectElemType},
		Mutable:  true,
	})

	// Build the append statement: __queryN = append(__queryN, selectExpr)
	resultRef := &aotir.VarRef{Name: tempName, VarType: aotir.TypeList, ElemType: selectElemType}
	appendStmt := &aotir.AssignStmt{
		Name: tempName,
		Value: &aotir.AppendExpr{
			Receiver: resultRef,
			Value:    selectExpr,
			ElemType: selectElemType,
		},
	}

	// innerBody starts as the append (possibly wrapped in a where guard).
	var innerBody *aotir.Block
	if whereCond != nil {
		innerBody = &aotir.Block{Statements: []aotir.Stmt{&aotir.IfStmt{
			Cond: whereCond,
			Then: &aotir.Block{Statements: []aotir.Stmt{appendStmt}},
		}}}
	} else {
		innerBody = &aotir.Block{Statements: []aotir.Stmt{appendStmt}}
	}

	// Wrap innerBody with join loops in reverse order (innermost first).
	for i := len(q.Joins) - 1; i >= 0; i-- {
		j := q.Joins[i]
		si := joinSrcs[i]
		on := joinOns[i]
		if j.Side == nil {
			// Inner join: for y in ys { if on { innerBody } }
			innerBody = &aotir.Block{Statements: []aotir.Stmt{
				&aotir.ForEachStmt{
					Var:              j.Var,
					List:             si.src,
					ElemType:         si.elemType,
					ElemRecordName:   si.elemRec,
					InnerElemType:    si.innerElem,
					MapElemKeyType:   si.mapKey,
					MapElemValueType: si.mapValue,
					Body: &aotir.Block{Statements: []aotir.Stmt{
						&aotir.IfStmt{Cond: on, Then: innerBody},
					}},
				},
			}}
		} else {
			// Left join: emit __anyN sentinel + matched rows + unmatched fallback.
			l.tempCounter++
			anyName := fmt.Sprintf("__any%d", l.tempCounter)
			prev.vars[anyName] = lbinding{t: aotir.TypeBool, mutable: true}

			// The matched body sets __anyN = true then appends.
			matchedStmts := append([]aotir.Stmt{
				&aotir.AssignStmt{Name: anyName, Value: &aotir.BoolLit{Value: true}},
			}, innerBody.Statements...)

			// Rebuild a fresh resultRef/appendStmt for the unmatched fallback
			// (innerBody may reference selectExpr which was built with all vars
			// in scope; for left join the select in fixtures only uses left vars,
			// so we can reuse selectExpr directly).
			fallbackAppend := &aotir.AssignStmt{
				Name: tempName,
				Value: &aotir.AppendExpr{
					Receiver: &aotir.VarRef{Name: tempName, VarType: aotir.TypeList, ElemType: selectElemType},
					Value:    selectExpr,
					ElemType: selectElemType,
				},
			}
			var fallbackBody *aotir.Block
			if whereCond != nil {
				fallbackBody = &aotir.Block{Statements: []aotir.Stmt{&aotir.IfStmt{
					Cond: whereCond,
					Then: &aotir.Block{Statements: []aotir.Stmt{fallbackAppend}},
				}}}
			} else {
				fallbackBody = &aotir.Block{Statements: []aotir.Stmt{fallbackAppend}}
			}

			innerBody = &aotir.Block{Statements: []aotir.Stmt{
				&aotir.LetStmt{Name: anyName, VarType: aotir.TypeBool, Init: &aotir.BoolLit{Value: false}, Mutable: true},
				&aotir.ForEachStmt{
					Var:              j.Var,
					List:             si.src,
					ElemType:         si.elemType,
					ElemRecordName:   si.elemRec,
					InnerElemType:    si.innerElem,
					MapElemKeyType:   si.mapKey,
					MapElemValueType: si.mapValue,
					Body: &aotir.Block{Statements: []aotir.Stmt{
						&aotir.IfStmt{
							Cond: on,
							Then: &aotir.Block{Statements: matchedStmts},
						},
					}},
				},
				&aotir.IfStmt{
					Cond: &aotir.UnaryExpr{Op: aotir.UnNotBool, Operand: &aotir.VarRef{Name: anyName, VarType: aotir.TypeBool}, Result: aotir.TypeBool},
					Then: fallbackBody,
				},
			}}
		}
	}

	// Wrap innerBody with from (cross-join) loops in reverse order.
	for i := len(q.Froms) - 1; i >= 0; i-- {
		f := q.Froms[i]
		si := fromSrcs[i]
		innerBody = &aotir.Block{Statements: []aotir.Stmt{
			&aotir.ForEachStmt{
				Var:              f.Var,
				List:             si.src,
				ElemType:         si.elemType,
				ElemRecordName:   si.elemRec,
				InnerElemType:    si.innerElem,
				MapElemKeyType:   si.mapKey,
				MapElemValueType: si.mapValue,
				Body:             innerBody,
			},
		}}
	}

	// Emit: for q.Var in source { innerBody }
	l.currentBlock.Statements = append(l.currentBlock.Statements, &aotir.ForEachStmt{
		Var:              q.Var,
		List:             source,
		ElemType:         sourceElemType,
		ElemRecordName:   sourceElemRecord,
		InnerElemType:    sourceInnerElem,
		MapElemKeyType:   sourceMapKey,
		MapElemValueType: sourceMapValue,
		Body:             innerBody,
	})

	// Phase 8.1: order by -- sort the accumulated result list.
	if q.Sort != nil {
		sortRef := &aotir.VarRef{Name: tempName, VarType: aotir.TypeList, ElemType: selectElemType}
		sortExpr := &aotir.ListSortAscExpr{
			Receiver: sortRef,
			ElemType: selectElemType,
		}
		l.currentBlock.Statements = append(l.currentBlock.Statements, &aotir.AssignStmt{
			Name:  tempName,
			Value: sortExpr,
		})
	}

	// Phase 8.1: skip / take -- slice the (possibly sorted) result list.
	if q.Skip != nil || q.Take != nil {
		var startExpr aotir.Expr = &aotir.IntLit{Value: 0}
		if q.Skip != nil {
			s, err := l.lowerExpr(q.Skip)
			if err != nil {
				return nil, fmt.Errorf("query skip: %w", err)
			}
			if s.Type() != aotir.TypeInt {
				return nil, fmt.Errorf("query skip must be int, got %s", s.Type())
			}
			startExpr = s
		}
		var endExpr aotir.Expr
		if q.Take != nil {
			t, err := l.lowerExpr(q.Take)
			if err != nil {
				return nil, fmt.Errorf("query take: %w", err)
			}
			if t.Type() != aotir.TypeInt {
				return nil, fmt.Errorf("query take must be int, got %s", t.Type())
			}
			// end = skip + take
			endExpr = &aotir.BinaryExpr{
				Op:     aotir.BinAddI64,
				Left:   startExpr,
				Right:  t,
				Result: aotir.TypeInt,
			}
		} else {
			// no take: end = len of result (use a very large sentinel)
			endExpr = &aotir.IntLit{Value: 1<<62 - 1}
		}
		sliceRef := &aotir.VarRef{Name: tempName, VarType: aotir.TypeList, ElemType: selectElemType}
		sliceExpr := &aotir.ListSliceExpr{
			Receiver: sliceRef,
			Start:    startExpr,
			End:      endExpr,
			ElemType: selectElemType,
		}
		l.currentBlock.Statements = append(l.currentBlock.Statements, &aotir.AssignStmt{
			Name:  tempName,
			Value: sliceExpr,
		})
	}

	return &aotir.VarRef{Name: tempName, VarType: aotir.TypeList, ElemType: selectElemType}, nil
}

// lowerMatchExpr lowers a `match x { ... }` used as an expression.
// It allocates a fresh temp variable, emits a LetStmt + MatchStmt into
// the current block (tracked via l.currentBlock), and returns a
// VarRef/UnionVarRef for the temp.
func (l *lowerer) lowerMatchExpr(m *parser.MatchExpr) (aotir.Expr, error) {
	if l.currentBlock == nil {
		return nil, fmt.Errorf("match expression outside a statement block (internal error)")
	}
	// Infer result type from the first non-wildcard arm's result expression.
	resultType, resultUnion, err := l.inferMatchResultType(m)
	if err != nil {
		return nil, fmt.Errorf("match expr: %w", err)
	}
	tempName := l.freshTemp()
	// Register temp as mutable so the match arms can assign to it.
	l.scope.vars[tempName] = lbinding{t: resultType, mutable: true, union: resultUnion}
	if err := l.lowerMatch(l.currentBlock, m, tempName, resultType); err != nil {
		return nil, fmt.Errorf("match expr: %w", err)
	}
	if resultType == aotir.TypeUnion {
		return &aotir.UnionVarRef{Name: tempName, UnionName: resultUnion}, nil
	}
	return &aotir.VarRef{Name: tempName, VarType: resultType}, nil
}

// inferMatchResultType inspects the first non-wildcard arm's result expression
// to determine the match expression's result type.
func (l *lowerer) inferMatchResultType(m *parser.MatchExpr) (aotir.Type, string, error) {
	// Speculatively lower the match target to obtain the union declaration,
	// so we can inject pattern-variable bindings when peeking at arm results.
	var ud *aotir.UnionDecl
	{
		prev := l.scope
		l.scope = newLScope(prev)
		if tgt, err := l.lowerExpr(m.Target); err == nil && tgt.Type() == aotir.TypeUnion {
			if uName := exprUnionName(tgt); uName != "" {
				ud = l.unions[uName]
			}
		}
		l.scope = prev
	}

	for _, c := range m.Cases {
		if c == nil || isUnderscoreExpr(c.Pattern) {
			continue
		}
		if c.Result == nil {
			// Block-arm with no result expr -- result type is unit.
			return aotir.TypeUnit, "", nil
		}
		// Speculatively lower the result in a child scope. When the arm is a
		// call pattern like `Circle(r) => r * r`, inject bindings for each
		// pattern variable using the variant's field types so that `r` resolves.
		prev := l.scope
		l.scope = newLScope(prev)
		if ud != nil {
			if call, ok := callPattern(c.Pattern); ok {
				for i := range ud.Variants {
					vd := &ud.Variants[i]
					if vd.Name == call.Func && len(call.Args) == len(vd.Fields) {
						for j, arg := range call.Args {
							if varName, ok2 := identName(arg); ok2 && varName != "_" {
								l.scope.vars[varName] = lbinding{t: vd.Fields[j].FieldType, mutable: false}
							}
						}
						break
					}
				}
			}
		}
		expr, err := l.lowerExpr(c.Result)
		l.scope = prev
		if err != nil {
			// Could not infer; fall back to TypeUnion derived from target.
			return l.inferMatchTargetType(m)
		}
		unionName := exprUnionName(expr)
		return expr.Type(), unionName, nil
	}
	return aotir.TypeUnit, "", nil
}

// inferMatchTargetType peeks at the match target's type to determine
// the union being matched, used as a fallback for result type inference.
func (l *lowerer) inferMatchTargetType(m *parser.MatchExpr) (aotir.Type, string, error) {
	target, err := l.lowerExpr(m.Target)
	if err != nil {
		return aotir.TypeInvalid, "", fmt.Errorf("match target: %w", err)
	}
	return target.Type(), exprUnionName(target), nil
}

// lowerMatch lowers a `match` expression/statement into the output block.
// When resultVar is non-empty, each arm's body ends with an assignment to
// resultVar and the MatchStmt carries ResultVar/ResultType. When resultVar
// is empty, the match is a statement (arms must produce unit). The function
// emits a LetStmt for the temp (when resultVar is non-empty) followed by the
// MatchStmt into out. Callers from expression context pass l.currentBlock as out.
func (l *lowerer) lowerMatch(out *aotir.Block, m *parser.MatchExpr, resultVar string, resultType aotir.Type) error {
	if out == nil {
		return fmt.Errorf("lowerMatch: nil output block (internal error)")
	}

	// Lower the match target.
	target, err := l.lowerExpr(m.Target)
	if err != nil {
		return fmt.Errorf("match target: %w", err)
	}
	if target.Type() != aotir.TypeUnion {
		return fmt.Errorf("match target must be a union type, got %s", target.Type())
	}
	unionName := exprUnionName(target)
	if unionName == "" {
		return fmt.Errorf("match target has no union name")
	}
	ud, ok := l.unions[unionName]
	if !ok {
		return fmt.Errorf("match: union %q not declared", unionName)
	}

	// If used as expression, emit the LetStmt for the result temp variable.
	if resultVar != "" {
		var letUnionName string
		if resultType == aotir.TypeUnion {
			letUnionName = l.scope.vars[resultVar].union
		}
		out.Statements = append(out.Statements, &aotir.LetStmt{
			Name:      resultVar,
			VarType:   resultType,
			UnionName: letUnionName,
			Mutable:   true,
		})
	}

	// Lower each case arm.
	var arms []aotir.MatchArm
	var defaultArm *aotir.MatchArm
	for caseIdx, c := range m.Cases {
		if c == nil {
			return fmt.Errorf("match case %d is nil", caseIdx)
		}
		arm, isDefault, err := l.lowerMatchArm(c, ud, resultVar, resultType)
		if err != nil {
			return fmt.Errorf("match case %d: %w", caseIdx, err)
		}
		if isDefault {
			if defaultArm != nil {
				return fmt.Errorf("match: multiple wildcard (_) arms")
			}
			defaultArm = arm
		} else {
			arms = append(arms, *arm)
		}
	}

	// Determine ResultUnionName for the MatchStmt.
	var resultUnionName string
	if resultType == aotir.TypeUnion {
		resultUnionName = l.scope.vars[resultVar].union
	}

	out.Statements = append(out.Statements, &aotir.MatchStmt{
		Target:          target,
		UnionName:       unionName,
		Arms:            arms,
		Default:         defaultArm,
		ResultVar:       resultVar,
		ResultType:      resultType,
		ResultUnionName: resultUnionName,
	})
	return nil
}

// lowerMatchArm lowers one case arm. It returns the arm and a bool
// indicating whether this is the wildcard (default) arm.
func (l *lowerer) lowerMatchArm(c *parser.MatchCase, ud *aotir.UnionDecl, resultVar string, resultType aotir.Type) (*aotir.MatchArm, bool, error) {
	// Wildcard arm.
	if isUnderscoreExpr(c.Pattern) {
		body, err := l.lowerMatchBody(c, nil, resultVar, resultType)
		if err != nil {
			return nil, true, err
		}
		return &aotir.MatchArm{VariantName: "", Body: body}, true, nil
	}

	// Field-bearing variant: `Circle(r) => ...`
	if call, ok := callPattern(c.Pattern); ok {
		variantName := call.Func
		var vd *aotir.VariantDecl
		for i := range ud.Variants {
			if ud.Variants[i].Name == variantName {
				vd = &ud.Variants[i]
				break
			}
		}
		if vd == nil {
			return nil, false, fmt.Errorf("pattern variant %q not found in union %q", variantName, ud.Name)
		}
		if len(call.Args) != len(vd.Fields) {
			return nil, false, fmt.Errorf("pattern %q expects %d fields, got %d", variantName, len(vd.Fields), len(call.Args))
		}
		// Build bindings: each arg must be a simple identifier (the pattern variable name).
		bindings := make([]aotir.MatchBinding, 0, len(call.Args))
		bindingScope := make(map[string]lbinding)
		for i, arg := range call.Args {
			varName, ok := identName(arg)
			if !ok {
				return nil, false, fmt.Errorf("pattern %q field %d: pattern variable must be a simple identifier", variantName, i)
			}
			if varName == "_" {
				continue // wildcard binding: skip
			}
			bindings = append(bindings, aotir.MatchBinding{
				VarName:   varName,
				FieldName: vd.Fields[i].Name,
				FieldType: vd.Fields[i].FieldType,
			})
			bindingScope[varName] = lbinding{t: vd.Fields[i].FieldType, mutable: false}
		}
		body, err := l.lowerMatchBodyWithScope(c, bindingScope, resultVar, resultType)
		if err != nil {
			return nil, false, err
		}
		return &aotir.MatchArm{VariantName: variantName, Tag: vd.Tag, Bindings: bindings, Body: body}, false, nil
	}

	// Unit variant: `None => ...` or `MyVariant => ...`
	if variantName, ok := identName(c.Pattern); ok {
		var vd *aotir.VariantDecl
		for i := range ud.Variants {
			if ud.Variants[i].Name == variantName {
				vd = &ud.Variants[i]
				break
			}
		}
		if vd == nil {
			return nil, false, fmt.Errorf("pattern variant %q not found in union %q", variantName, ud.Name)
		}
		body, err := l.lowerMatchBody(c, nil, resultVar, resultType)
		if err != nil {
			return nil, false, err
		}
		return &aotir.MatchArm{VariantName: variantName, Tag: vd.Tag, Body: body}, false, nil
	}

	return nil, false, fmt.Errorf("unsupported pattern shape in Phase 4.0 (expected identifier or call pattern)")
}

// lowerMatchBody lowers the arm's body (either a block or a result expression).
// Any result is assigned to resultVar (when non-empty).
func (l *lowerer) lowerMatchBody(c *parser.MatchCase, extraScope map[string]lbinding, resultVar string, resultType aotir.Type) (*aotir.Block, error) {
	return l.lowerMatchBodyWithScope(c, extraScope, resultVar, resultType)
}

// lowerMatchBodyWithScope lowers an arm body with extra pattern-variable bindings
// injected into the scope.
func (l *lowerer) lowerMatchBodyWithScope(c *parser.MatchCase, extraScope map[string]lbinding, resultVar string, resultType aotir.Type) (*aotir.Block, error) {
	prev := l.scope
	l.scope = newLScope(prev)
	for name, b := range extraScope {
		l.scope.vars[name] = b
	}
	defer func() { l.scope = prev }()

	body := &aotir.Block{}

	if len(c.Block) > 0 {
		// Block-style arm: `Pattern => { stmts }`
		for i, st := range c.Block {
			if st == nil {
				return nil, fmt.Errorf("arm block stmt %d is nil", i)
			}
			if err := l.lowerStatement(body, st); err != nil {
				return nil, fmt.Errorf("arm block stmt %d: %w", i, err)
			}
		}
		if resultVar != "" {
			// If the block ends with an expression statement that is the result,
			// we don't auto-assign; the fixtures must use explicit assignment or
			// have the last stmt be a return.
			// For now: block arms in expression-position match emit the block
			// stmts only (they must assign resultVar themselves via `resultVar = expr`).
		}
		return body, nil
	}

	if c.Result != nil {
		if resultVar == "" {
			// Statement-position match: arm result must be a unit-returning statement.
			// Route through ExprStmt lowering so print() and void calls work.
			dummyStmt := &parser.ExprStmt{Expr: c.Result}
			if err := l.lowerExprStmt(body, dummyStmt); err != nil {
				return nil, fmt.Errorf("arm result: %w", err)
			}
			return body, nil
		}
		// Expression-style arm: `Pattern => expr`
		expr, err := l.lowerExpr(c.Result)
		if err != nil {
			return nil, fmt.Errorf("arm result: %w", err)
		}
		// Assign the result to the temp variable.
		body.Statements = append(body.Statements, &aotir.AssignStmt{
			Name:  resultVar,
			Value: expr,
		})
		return body, nil
	}

	return body, nil
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
