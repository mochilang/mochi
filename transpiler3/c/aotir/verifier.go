package aotir

import (
	"errors"
	"fmt"
)

// Builtins is the set of callable names the verifier (and the
// emit pass) accepts as resolved without a matching Function
// entry. Phase 1 shipped the string print; Phase 2.0 added the
// int/float/bool print entries that match the runtime ABI in
// transpiler3/c/runtime/include/mochi/print.h.
//
// Each entry maps mangled name to parameter types. Return type
// is always TypeUnit (CallStmt is statement-form).
var Builtins = map[string][]Type{
	"mochi_print_str":  {TypeString},
	"mochi_print_i64":  {TypeInt},
	"mochi_print_f64":  {TypeFloat},
	"mochi_print_bool": {TypeBool},
}

// Verify enforces the aotir invariants. Callers run Verify
// after Lower and before Emit; tests run it on hand-built
// fixtures. Invariants:
//
//   - p.Main is a valid index into p.Functions.
//   - Every function name is unique.
//   - The entry function takes no parameters and returns TypeUnit.
//   - Every statement is well-formed for the scope it sits in
//     (variables declared before use, BreakStmt/ContinueStmt
//     inside a loop, assignment only to a mutable binding,
//     ReturnStmt value type matches the enclosing function).
//   - Every expression is well-typed (binary/unary operand types
//     match the operator and the recorded Result; VarRef
//     resolves to a binding of the recorded VarType; CallExpr
//     args match the resolved callee signature).
//
// Later phases extend this list as new IR shapes land.
func Verify(p *Program) error {
	if p == nil {
		return errors.New("aotir.Verify: nil Program")
	}
	if p.Main < 0 || p.Main >= len(p.Functions) {
		return fmt.Errorf("aotir.Verify: Main index %d out of range [0,%d)", p.Main, len(p.Functions))
	}
	records := make(map[string]*RecordDecl, len(p.Records))
	for i, r := range p.Records {
		if r == nil {
			return fmt.Errorf("aotir.Verify: Records[%d] is nil", i)
		}
		if r.Name == "" {
			return fmt.Errorf("aotir.Verify: Records[%d] has empty Name", i)
		}
		if _, dup := records[r.Name]; dup {
			return fmt.Errorf("aotir.Verify: duplicate record name %q at index %d", r.Name, i)
		}
		seen := make(map[string]bool, len(r.Fields))
		for j, f := range r.Fields {
			if f.Name == "" {
				return fmt.Errorf("aotir.Verify: record %q field %d: empty name", r.Name, j)
			}
			if seen[f.Name] {
				return fmt.Errorf("aotir.Verify: record %q: duplicate field %q", r.Name, f.Name)
			}
			seen[f.Name] = true
			switch f.Type {
			case TypeInt, TypeFloat, TypeBool, TypeString:
				if f.RecordName != "" {
					return fmt.Errorf("aotir.Verify: record %q field %q: RecordName set on primitive field", r.Name, f.Name)
				}
			case TypeRecord:
				return fmt.Errorf("aotir.Verify: record %q field %q: nested record fields are not yet supported in Phase 3.0", r.Name, f.Name)
			default:
				return fmt.Errorf("aotir.Verify: record %q field %q: unsupported field type %s", r.Name, f.Name, f.Type)
			}
		}
		records[r.Name] = r
	}
	unions := make(map[string]*UnionDecl, len(p.Unions))
	for i, u := range p.Unions {
		if u == nil {
			return fmt.Errorf("aotir.Verify: Unions[%d] is nil", i)
		}
		if u.Name == "" {
			return fmt.Errorf("aotir.Verify: Unions[%d] has empty Name", i)
		}
		if _, dup := unions[u.Name]; dup {
			return fmt.Errorf("aotir.Verify: duplicate union name %q at index %d", u.Name, i)
		}
		for j, v := range u.Variants {
			if v.Name == "" {
				return fmt.Errorf("aotir.Verify: union %q variant %d: empty name", u.Name, j)
			}
		}
		unions[u.Name] = u
	}
	// Phase 10.0: build extern function map for call resolution.
	externFns := make(map[string]*ExternFuncDecl, len(p.ExternFuncs))
	for i, ef := range p.ExternFuncs {
		if ef == nil {
			return fmt.Errorf("aotir.Verify: ExternFuncs[%d] is nil", i)
		}
		if ef.Name == "" {
			return fmt.Errorf("aotir.Verify: ExternFuncs[%d] has empty Name", i)
		}
		if _, dup := externFns[ef.Name]; dup {
			return fmt.Errorf("aotir.Verify: duplicate extern func name %q at index %d", ef.Name, i)
		}
		externFns[ef.Name] = ef
	}
	// Phase 10.2: treat Go FFI functions as callable extern functions in the verifier.
	// Register under both the original name and the mochi_go_ prefixed name since
	// the lowerer emits mochi_go_<name> as the CallExpr.Func.
	for i, gf := range p.GoFuncs {
		if gf == nil {
			return fmt.Errorf("aotir.Verify: GoFuncs[%d] is nil", i)
		}
		if gf.Name == "" {
			return fmt.Errorf("aotir.Verify: GoFuncs[%d] has empty Name", i)
		}
		decl := &ExternFuncDecl{Name: gf.Name, Params: gf.Params, ReturnType: gf.ReturnType}
		externFns[gf.Name] = decl
		externFns["mochi_go_"+gf.Name] = decl
	}
	// Phase 10.3: Python FFI functions registered under both original and mochi_py_ prefixed name.
	for i, pf := range p.PythonFuncs {
		if pf == nil {
			return fmt.Errorf("aotir.Verify: PythonFuncs[%d] is nil", i)
		}
		if pf.Name == "" {
			return fmt.Errorf("aotir.Verify: PythonFuncs[%d] has empty Name", i)
		}
		decl := &ExternFuncDecl{Name: pf.Name, Params: pf.Params, ReturnType: pf.ReturnType}
		externFns[pf.Name] = decl
		externFns["mochi_py_"+pf.Name] = decl
	}
	// Phase 10.4: JavaScript FFI functions registered under both original and mochi_js_ prefixed name.
	for i, jf := range p.JSFuncs {
		if jf == nil {
			return fmt.Errorf("aotir.Verify: JSFuncs[%d] is nil", i)
		}
		if jf.Name == "" {
			return fmt.Errorf("aotir.Verify: JSFuncs[%d] has empty Name", i)
		}
		decl := &ExternFuncDecl{Name: jf.Name, Params: jf.Params, ReturnType: jf.ReturnType}
		externFns[jf.Name] = decl
		externFns["mochi_js_"+jf.Name] = decl
	}
	// Phase 12.0: Java FFI functions registered under the Mochi alias name.
	for i, jf := range p.JavaFuncs {
		if jf == nil {
			return fmt.Errorf("aotir.Verify: JavaFuncs[%d] is nil", i)
		}
		if jf.MochiName == "" {
			return fmt.Errorf("aotir.Verify: JavaFuncs[%d] has empty MochiName", i)
		}
		externFns[jf.MochiName] = &ExternFuncDecl{Name: jf.MochiName, Params: jf.Params, ReturnType: jf.ReturnType}
	}
	// Phase 9.3: build agent map for intent-call resolution.
	agents := make(map[string]*AgentDecl, len(p.Agents))
	for i, ag := range p.Agents {
		if ag == nil {
			return fmt.Errorf("aotir.Verify: Agents[%d] is nil", i)
		}
		if ag.Name == "" {
			return fmt.Errorf("aotir.Verify: Agents[%d] has empty Name", i)
		}
		if _, dup := agents[ag.Name]; dup {
			return fmt.Errorf("aotir.Verify: duplicate agent name %q at index %d", ag.Name, i)
		}
		agents[ag.Name] = ag
	}
	names := make(map[string]*Function, len(p.Functions))
	for i, fn := range p.Functions {
		if fn == nil {
			return fmt.Errorf("aotir.Verify: Functions[%d] is nil", i)
		}
		if fn.Name == "" {
			return fmt.Errorf("aotir.Verify: Functions[%d] has empty Name", i)
		}
		if _, dup := names[fn.Name]; dup {
			return fmt.Errorf("aotir.Verify: duplicate function name %q at index %d", fn.Name, i)
		}
		names[fn.Name] = fn
		if fn.ReturnType == TypeFun {
			if fn.ReturnFunSig == nil {
				return fmt.Errorf("aotir.Verify: function %q returns fun but ReturnFunSig is nil", fn.Name)
			}
		}
		if fn.ReturnType == TypeRecord {
			if fn.ReturnRecordName == "" {
				return fmt.Errorf("aotir.Verify: function %q returns record but ReturnRecordName is empty", fn.Name)
			}
			if _, ok := records[fn.ReturnRecordName]; !ok {
				return fmt.Errorf("aotir.Verify: function %q return record %q is not declared", fn.Name, fn.ReturnRecordName)
			}
		} else if fn.ReturnRecordName != "" {
			return fmt.Errorf("aotir.Verify: function %q has ReturnRecordName set on non-record return type %s", fn.Name, fn.ReturnType)
		}
		if fn.ReturnType == TypeList {
			if !isListElemType(fn.ReturnElemType) {
				return fmt.Errorf("aotir.Verify: function %q returns list but ReturnElemType is %s (Phase 3.4a supports scalar or record element types)", fn.Name, fn.ReturnElemType)
			}
			if fn.ReturnElemType == TypeRecord {
				if fn.ReturnElemRecordName == "" {
					return fmt.Errorf("aotir.Verify: function %q returns list<record> but ReturnElemRecordName is empty", fn.Name)
				}
				if _, ok := records[fn.ReturnElemRecordName]; !ok {
					return fmt.Errorf("aotir.Verify: function %q return list element record %q is not declared", fn.Name, fn.ReturnElemRecordName)
				}
			} else if fn.ReturnElemRecordName != "" {
				return fmt.Errorf("aotir.Verify: function %q has ReturnElemRecordName set on list<%s> (only valid when element is record)", fn.Name, fn.ReturnElemType)
			}
			if fn.ReturnElemType == TypeList {
				if !isScalarElemType(fn.ReturnInnerElemType) {
					return fmt.Errorf("aotir.Verify: function %q returns list<list<T>> but ReturnInnerElemType is %s (Phase 3.4b requires scalar inner)", fn.Name, fn.ReturnInnerElemType)
				}
			} else if fn.ReturnInnerElemType != TypeInvalid {
				return fmt.Errorf("aotir.Verify: function %q has ReturnInnerElemType set on list<%s> (only valid when element is list)", fn.Name, fn.ReturnElemType)
			}
			if fn.ReturnElemType == TypeMap {
				if !isScalarKeyType(fn.ReturnMapElemKeyType) {
					return fmt.Errorf("aotir.Verify: function %q returns list<map<K,V>> but ReturnMapElemKeyType is %s (Phase 3.4f requires int or string key)", fn.Name, fn.ReturnMapElemKeyType)
				}
				if !isScalarValueType(fn.ReturnMapElemValueType) {
					return fmt.Errorf("aotir.Verify: function %q returns list<map<K,V>> but ReturnMapElemValueType is %s (Phase 3.4f requires scalar value)", fn.Name, fn.ReturnMapElemValueType)
				}
			} else if fn.ReturnMapElemKeyType != TypeInvalid || fn.ReturnMapElemValueType != TypeInvalid {
				return fmt.Errorf("aotir.Verify: function %q has ReturnMapElemKeyType/ReturnMapElemValueType set on list<%s> (only valid when element is map)", fn.Name, fn.ReturnElemType)
			}
		} else {
			if fn.ReturnElemType != TypeInvalid {
				return fmt.Errorf("aotir.Verify: function %q has ReturnElemType set on non-list return type %s", fn.Name, fn.ReturnType)
			}
			if fn.ReturnElemRecordName != "" {
				return fmt.Errorf("aotir.Verify: function %q has ReturnElemRecordName set on non-list return type %s", fn.Name, fn.ReturnType)
			}
			if fn.ReturnInnerElemType != TypeInvalid {
				return fmt.Errorf("aotir.Verify: function %q has ReturnInnerElemType set on non-list return type %s", fn.Name, fn.ReturnType)
			}
			if fn.ReturnMapElemKeyType != TypeInvalid || fn.ReturnMapElemValueType != TypeInvalid {
				return fmt.Errorf("aotir.Verify: function %q has ReturnMapElemKeyType/ReturnMapElemValueType set on non-list return type %s", fn.Name, fn.ReturnType)
			}
		}
		if fn.ReturnType == TypeMap {
			if !isScalarKeyType(fn.ReturnKeyType) {
				return fmt.Errorf("aotir.Verify: function %q returns map but ReturnKeyType is %s (Phase 3.2 supports int or string keys only)", fn.Name, fn.ReturnKeyType)
			}
			if !isMapValueType(fn.ReturnValueType) {
				return fmt.Errorf("aotir.Verify: function %q returns map but ReturnValueType is %s (Phase 3.2/3.4e supports scalar or list values)", fn.Name, fn.ReturnValueType)
			}
			if fn.ReturnValueType == TypeList {
				if !isScalarElemType(fn.ReturnListValueElemType) {
					return fmt.Errorf("aotir.Verify: function %q returns map<_,list<T>> but ReturnListValueElemType is %s (Phase 3.4e requires scalar inner)", fn.Name, fn.ReturnListValueElemType)
				}
			} else if fn.ReturnListValueElemType != TypeInvalid {
				return fmt.Errorf("aotir.Verify: function %q has ReturnListValueElemType set on map<_,%s> (only valid when value is list)", fn.Name, fn.ReturnValueType)
			}
		} else {
			if fn.ReturnKeyType != TypeInvalid {
				return fmt.Errorf("aotir.Verify: function %q has ReturnKeyType set on non-map return type %s", fn.Name, fn.ReturnType)
			}
			if fn.ReturnValueType != TypeInvalid {
				return fmt.Errorf("aotir.Verify: function %q has ReturnValueType set on non-map return type %s", fn.Name, fn.ReturnType)
			}
			if fn.ReturnListValueElemType != TypeInvalid {
				return fmt.Errorf("aotir.Verify: function %q has ReturnListValueElemType set on non-map return type %s", fn.Name, fn.ReturnType)
			}
		}
		for k, pr := range fn.Params {
			if pr.Type == TypeRecord {
				if pr.RecordName == "" {
					return fmt.Errorf("aotir.Verify: function %q param %d: record-typed param missing RecordName", fn.Name, k)
				}
				if _, ok := records[pr.RecordName]; !ok {
					return fmt.Errorf("aotir.Verify: function %q param %d: record %q is not declared", fn.Name, k, pr.RecordName)
				}
			} else if pr.RecordName != "" {
				return fmt.Errorf("aotir.Verify: function %q param %d: RecordName set on non-record type %s", fn.Name, k, pr.Type)
			}
			if pr.Type == TypeList {
				if !isListElemType(pr.ElemType) {
					return fmt.Errorf("aotir.Verify: function %q param %d: list-typed param has ElemType %s (Phase 3.4a supports scalar or record element types)", fn.Name, k, pr.ElemType)
				}
				if pr.ElemType == TypeRecord {
					if pr.ElemRecordName == "" {
						return fmt.Errorf("aotir.Verify: function %q param %d: list<record> param missing ElemRecordName", fn.Name, k)
					}
					if _, ok := records[pr.ElemRecordName]; !ok {
						return fmt.Errorf("aotir.Verify: function %q param %d: list element record %q is not declared", fn.Name, k, pr.ElemRecordName)
					}
				} else if pr.ElemRecordName != "" {
					return fmt.Errorf("aotir.Verify: function %q param %d: ElemRecordName set on list<%s> (only valid when element is record)", fn.Name, k, pr.ElemType)
				}
				if pr.ElemType == TypeList {
					if !isScalarElemType(pr.InnerElemType) {
						return fmt.Errorf("aotir.Verify: function %q param %d: list<list<T>> param has InnerElemType %s (Phase 3.4b requires scalar inner)", fn.Name, k, pr.InnerElemType)
					}
				} else if pr.InnerElemType != TypeInvalid {
					return fmt.Errorf("aotir.Verify: function %q param %d: InnerElemType set on list<%s> (only valid when element is list)", fn.Name, k, pr.ElemType)
				}
				if pr.ElemType == TypeMap {
					if !isScalarKeyType(pr.MapElemKeyType) {
						return fmt.Errorf("aotir.Verify: function %q param %d: list<map<K,V>> param has MapElemKeyType %s (Phase 3.4f requires int or string key)", fn.Name, k, pr.MapElemKeyType)
					}
					if !isScalarValueType(pr.MapElemValueType) {
						return fmt.Errorf("aotir.Verify: function %q param %d: list<map<K,V>> param has MapElemValueType %s (Phase 3.4f requires scalar value)", fn.Name, k, pr.MapElemValueType)
					}
				} else if pr.MapElemKeyType != TypeInvalid || pr.MapElemValueType != TypeInvalid {
					return fmt.Errorf("aotir.Verify: function %q param %d: MapElemKeyType/MapElemValueType set on list<%s> (only valid when element is map)", fn.Name, k, pr.ElemType)
				}
			} else {
				if pr.ElemType != TypeInvalid {
					return fmt.Errorf("aotir.Verify: function %q param %d: ElemType set on non-list type %s", fn.Name, k, pr.Type)
				}
				if pr.ElemRecordName != "" {
					return fmt.Errorf("aotir.Verify: function %q param %d: ElemRecordName set on non-list type %s", fn.Name, k, pr.Type)
				}
				if pr.InnerElemType != TypeInvalid {
					return fmt.Errorf("aotir.Verify: function %q param %d: InnerElemType set on non-list type %s", fn.Name, k, pr.Type)
				}
				if pr.MapElemKeyType != TypeInvalid || pr.MapElemValueType != TypeInvalid {
					return fmt.Errorf("aotir.Verify: function %q param %d: MapElemKeyType/MapElemValueType set on non-list type %s", fn.Name, k, pr.Type)
				}
			}
			if pr.Type == TypeMap {
				if !isScalarKeyType(pr.KeyType) {
					return fmt.Errorf("aotir.Verify: function %q param %d: map-typed param has KeyType %s (Phase 3.2 supports int or string keys only)", fn.Name, k, pr.KeyType)
				}
				if !isMapValueType(pr.ValueType) {
					return fmt.Errorf("aotir.Verify: function %q param %d: map-typed param has ValueType %s (Phase 3.2/3.4e supports scalar or list values)", fn.Name, k, pr.ValueType)
				}
				if pr.ValueType == TypeList {
					if !isScalarElemType(pr.ListValueElemType) {
						return fmt.Errorf("aotir.Verify: function %q param %d: map<_,list<T>> param has ListValueElemType %s (Phase 3.4e requires scalar inner)", fn.Name, k, pr.ListValueElemType)
					}
				} else if pr.ListValueElemType != TypeInvalid {
					return fmt.Errorf("aotir.Verify: function %q param %d: ListValueElemType set on map<_,%s> (only valid when value is list)", fn.Name, k, pr.ValueType)
				}
			} else {
				if pr.KeyType != TypeInvalid {
					return fmt.Errorf("aotir.Verify: function %q param %d: KeyType set on non-map type %s", fn.Name, k, pr.Type)
				}
				if pr.ValueType != TypeInvalid {
					return fmt.Errorf("aotir.Verify: function %q param %d: ValueType set on non-map type %s", fn.Name, k, pr.Type)
				}
				if pr.ListValueElemType != TypeInvalid {
					return fmt.Errorf("aotir.Verify: function %q param %d: ListValueElemType set on non-map type %s", fn.Name, k, pr.Type)
				}
			}
			if pr.Type == TypeFun {
				if pr.FunSig == nil {
					return fmt.Errorf("aotir.Verify: function %q param %d: fun-typed param missing FunSig", fn.Name, k)
				}
			}
		}
	}
	entry := p.Functions[p.Main]
	if entry.ReturnType != TypeUnit {
		return fmt.Errorf("aotir.Verify: entry function %q must return unit, got %s", entry.Name, entry.ReturnType)
	}
	if len(entry.Params) != 0 {
		return fmt.Errorf("aotir.Verify: entry function %q must take no parameters, got %d", entry.Name, len(entry.Params))
	}
	for i, fn := range p.Functions {
		if fn.Body == nil {
			return fmt.Errorf("aotir.Verify: function %q (index %d) has nil Body", fn.Name, i)
		}
		ctx := &verifyCtx{
			fns:               names,
			externFns:         externFns,
			records:           records,
			unions:            unions,
			agents:            agents,
			scope:             newScope(nil),
			loopDepth:         0,
			returnType:        fn.ReturnType,
			returnRec:         fn.ReturnRecordName,
			returnElem:        fn.ReturnElemType,
			returnElemRec:     fn.ReturnElemRecordName,
			returnInnerElem:   fn.ReturnInnerElemType,
			returnKey:         fn.ReturnKeyType,
			returnValue:       fn.ReturnValueType,
			returnListValElem: fn.ReturnListValueElemType,
		}
		// Seed the function's parameter list as immutable
		// bindings in the root scope so the body can reference
		// them by name.
		for _, pr := range fn.Params {
			if pr.Name == "" {
				return fmt.Errorf("aotir.Verify: %s: parameter with empty name", fn.Name)
			}
			if _, dup := ctx.scope.vars[pr.Name]; dup {
				return fmt.Errorf("aotir.Verify: %s: duplicate parameter %q", fn.Name, pr.Name)
			}
			ctx.scope.vars[pr.Name] = binding{t: pr.Type, mutable: false, record: pr.RecordName, union: pr.UnionName, elem: pr.ElemType, elemRec: pr.ElemRecordName, mapElemKey: pr.MapElemKeyType, mapElemValue: pr.MapElemValueType, key: pr.KeyType, value: pr.ValueType, listValElem: pr.ListValueElemType, funSig: pr.FunSig}
		}
		// Phase 5.1: for capturing lifted functions, seed the scope with
		// env-relative bindings (e.g. "__e->x") so the verifier can resolve
		// VarRef nodes whose Name was rewritten to the env-relative form.
		if fn.IsLifted && fn.EnvTypeName != "" {
			for _, c := range fn.Captures {
				envKey := "__e->" + c.FieldName
				ctx.scope.vars[envKey] = binding{t: c.VarType, mutable: false}
			}
		}
		for j, st := range fn.Body.Statements {
			if err := verifyStmt(ctx, st); err != nil {
				return fmt.Errorf("aotir.Verify: %s statement %d: %w", fn.Name, j, err)
			}
		}
	}
	return nil
}

// verifyCtx carries the local state Verify needs to type-check a
// statement: the program's function-name map (full callee
// signatures, so CallExpr can look up params + return type), the
// active variable scope, current loop nesting depth, and the
// enclosing function's return type. The verifier never mutates
// fns; scope is pushed and popped per Block.
type verifyCtx struct {
	fns                 map[string]*Function
	externFns           map[string]*ExternFuncDecl // Phase 10.0: extern C function declarations
	records             map[string]*RecordDecl
	unions              map[string]*UnionDecl
	agents              map[string]*AgentDecl // Phase 9.3: agent declarations
	scope               *scope
	loopDepth           int
	returnType          Type
	returnRec           string
	returnElem          Type
	returnElemRec       string // record name when returnElem==TypeRecord
	returnInnerElem     Type   // inner elem type when returnElem==TypeList (Phase 3.4b)
	returnKey           Type
	returnValue         Type
	returnListValElem   Type   // inner list elem when returnType==TypeMap && returnValue==TypeList (Phase 3.4e)
}

// scope is a single lexical frame. parent==nil marks the function
// root; nested if/while bodies get child scopes so variables
// declared inside a branch are not visible outside it.
type scope struct {
	parent *scope
	vars   map[string]binding
}

type binding struct {
	t            Type
	mutable      bool
	record       string   // record name when t==TypeRecord
	union        string   // union name when t==TypeUnion (Phase 4)
	elem         Type     // element type when t==TypeList or t==TypeSet (Phase 3.3)
	elemRec      string   // element record name when t==TypeList && elem==TypeRecord
	mapElemKey   Type     // map key type when t==TypeList && elem==TypeMap (Phase 3.4f)
	mapElemValue Type     // map value type when t==TypeList && elem==TypeMap (Phase 3.4f)
	key          Type     // key type when t==TypeMap
	value        Type     // value type when t==TypeMap
	listValElem  Type     // inner list elem when t==TypeMap && value==TypeList (Phase 3.4e)
	funSig       *FunSig  // function signature when t==TypeFun (Phase 5.0)
	chanElem     Type     // element type when t==TypeChan (Phase 9.1)
	streamElem   Type     // element type when t==TypeStream (Phase 9.2)
	subElem      Type     // element type when t==TypeSub (Phase 9.2)
	agentName    string   // agent name when t==TypeAgent (Phase 9.3)
}

func newScope(parent *scope) *scope {
	return &scope{parent: parent, vars: map[string]binding{}}
}

// lookup walks parent chain until it finds Name; returns ok=false
// if the binding is not declared in any enclosing scope.
func (s *scope) lookup(name string) (binding, bool) {
	for s != nil {
		if b, ok := s.vars[name]; ok {
			return b, true
		}
		s = s.parent
	}
	return binding{}, false
}

func verifyStmt(ctx *verifyCtx, st Stmt) error {
	switch s := st.(type) {
	case *CallStmt:
		return verifyCallStmt(ctx, s)
	case *LetStmt:
		return verifyLetStmt(ctx, s)
	case *AssignStmt:
		return verifyAssignStmt(ctx, s)
	case *ListSetStmt:
		return verifyListSetStmt(ctx, s)
	case *MapPutStmt:
		return verifyMapPutStmt(ctx, s)
	case *OMapPutStmt:
		return verifyOMapPutStmt(ctx, s)
	case *IfStmt:
		return verifyIfStmt(ctx, s)
	case *WhileStmt:
		return verifyWhileStmt(ctx, s)
	case *ForRangeStmt:
		return verifyForRangeStmt(ctx, s)
	case *ForEachStmt:
		return verifyForEachStmt(ctx, s)
	case *BreakStmt:
		if ctx.loopDepth == 0 {
			return errors.New("break outside a loop")
		}
		return nil
	case *ContinueStmt:
		if ctx.loopDepth == 0 {
			return errors.New("continue outside a loop")
		}
		return nil
	case *ReturnStmt:
		return verifyReturnStmt(ctx, s)
	case *MatchStmt:
		return verifyMatchStmt(ctx, s)
	case *ClosureEnvStmt:
		// Phase 5.1: env allocation for capturing closures. Verify that
		// each capture has a non-empty field name and a valid scalar type.
		if s.EnvTypeName == "" {
			return errors.New("ClosureEnvStmt: empty EnvTypeName")
		}
		if s.EnvVarName == "" {
			return errors.New("ClosureEnvStmt: empty EnvVarName")
		}
		for i, c := range s.Captures {
			if c.FieldName == "" {
				return fmt.Errorf("ClosureEnvStmt: capture %d has empty FieldName", i)
			}
			if c.SrcName == "" {
				return fmt.Errorf("ClosureEnvStmt: capture %d has empty SrcName", i)
			}
		}
		return nil
	case *WriteFileStmt:
		if s.Path == nil {
			return errors.New("WriteFileStmt: nil Path")
		}
		if s.Path.Type() != TypeString {
			return fmt.Errorf("WriteFileStmt: Path must be TypeString, got %s", s.Path.Type())
		}
		if s.Content == nil {
			return errors.New("WriteFileStmt: nil Content")
		}
		if s.Content.Type() != TypeString {
			return fmt.Errorf("WriteFileStmt: Content must be TypeString, got %s", s.Content.Type())
		}
		if err := verifyExprCtx(ctx, s.Path); err != nil {
			return fmt.Errorf("WriteFileStmt Path: %w", err)
		}
		return verifyExprCtx(ctx, s.Content)
	case *AppendFileStmt:
		if s.Path == nil {
			return errors.New("AppendFileStmt: nil Path")
		}
		if s.Path.Type() != TypeString {
			return fmt.Errorf("AppendFileStmt: Path must be TypeString, got %s", s.Path.Type())
		}
		if s.Content == nil {
			return errors.New("AppendFileStmt: nil Content")
		}
		if s.Content.Type() != TypeString {
			return fmt.Errorf("AppendFileStmt: Content must be TypeString, got %s", s.Content.Type())
		}
		if err := verifyExprCtx(ctx, s.Path); err != nil {
			return fmt.Errorf("AppendFileStmt Path: %w", err)
		}
		return verifyExprCtx(ctx, s.Content)
	case *SaveCSVStmt:
		if s.Path == nil {
			return errors.New("SaveCSVStmt: nil Path")
		}
		if s.Path.Type() != TypeString {
			return fmt.Errorf("SaveCSVStmt: Path must be TypeString, got %s", s.Path.Type())
		}
		if s.Data == nil {
			return errors.New("SaveCSVStmt: nil Data")
		}
		if s.Data.Type() != TypeList {
			return fmt.Errorf("SaveCSVStmt: Data must be TypeList, got %s", s.Data.Type())
		}
		if err := verifyExprCtx(ctx, s.Path); err != nil {
			return fmt.Errorf("SaveCSVStmt Path: %w", err)
		}
		return verifyExprCtx(ctx, s.Data)
	case *QueryScopeStmt:
		if s.ResultVar == "" {
			return errors.New("QueryScopeStmt: empty ResultVar")
		}
		if s.ArenaVar == "" {
			return errors.New("QueryScopeStmt: empty ArenaVar")
		}
		if s.ElemType == TypeInvalid {
			return errors.New("QueryScopeStmt: TypeInvalid ElemType")
		}
		if s.Body == nil {
			return errors.New("QueryScopeStmt: nil Body")
		}
		return verifyBlock(ctx, s.Body)
	case *TryCatchStmt:
		return verifyTryCatchStmt(ctx, s)
	case *PanicStmt:
		if s.Code == nil {
			return errors.New("PanicStmt: nil Code")
		}
		if s.Code.Type() != TypeInt {
			return fmt.Errorf("PanicStmt: Code must be TypeInt, got %s", s.Code.Type())
		}
		if s.Msg == nil {
			return errors.New("PanicStmt: nil Msg")
		}
		if s.Msg.Type() != TypeString {
			return fmt.Errorf("PanicStmt: Msg must be TypeString, got %s", s.Msg.Type())
		}
		if err := verifyExprCtx(ctx, s.Code); err != nil {
			return fmt.Errorf("PanicStmt Code: %w", err)
		}
		return verifyExprCtx(ctx, s.Msg)
	case *RawCStmt:
		// Phase 15.0: raw C block; the lowerer is responsible for correctness.
		if s.Code == "" {
			return errors.New("RawCStmt: empty Code")
		}
		return nil
	case *ChanSendStmt:
		if s.Chan == nil {
			return errors.New("ChanSendStmt: nil Chan")
		}
		if s.Chan.Type() != TypeChan {
			return fmt.Errorf("ChanSendStmt: Chan must be TypeChan, got %s", s.Chan.Type())
		}
		if s.Val == nil {
			return errors.New("ChanSendStmt: nil Val")
		}
		if s.Val.Type() != s.ElemType {
			return fmt.Errorf("ChanSendStmt: Val type %s != ElemType %s", s.Val.Type(), s.ElemType)
		}
		if err := verifyExprCtx(ctx, s.Chan); err != nil {
			return fmt.Errorf("ChanSendStmt Chan: %w", err)
		}
		return verifyExprCtx(ctx, s.Val)
	case *StreamEmitStmt:
		if s.Stream == nil {
			return errors.New("StreamEmitStmt: nil Stream")
		}
		if s.Stream.Type() != TypeStream {
			return fmt.Errorf("StreamEmitStmt: Stream must be TypeStream, got %s", s.Stream.Type())
		}
		if s.Val == nil {
			return errors.New("StreamEmitStmt: nil Val")
		}
		if s.Val.Type() != s.ElemType {
			return fmt.Errorf("StreamEmitStmt: Val type %s != ElemType %s", s.Val.Type(), s.ElemType)
		}
		if err := verifyExprCtx(ctx, s.Stream); err != nil {
			return fmt.Errorf("StreamEmitStmt Stream: %w", err)
		}
		return verifyExprCtx(ctx, s.Val)
	case *AgentIntentCallStmt:
		// Phase 9.3: synchronous intent call at statement position.
		if s.AgentName == "" {
			return errors.New("AgentIntentCallStmt: empty AgentName")
		}
		if s.IntentName == "" {
			return errors.New("AgentIntentCallStmt: empty IntentName")
		}
		if s.Receiver == nil {
			return errors.New("AgentIntentCallStmt: nil Receiver")
		}
		if s.Receiver.Type() != TypeAgent {
			return fmt.Errorf("AgentIntentCallStmt: Receiver must be TypeAgent, got %s", s.Receiver.Type())
		}
		if err := verifyExprCtx(ctx, s.Receiver); err != nil {
			return fmt.Errorf("AgentIntentCallStmt receiver: %w", err)
		}
		ag, ok := ctx.agents[s.AgentName]
		if !ok {
			return fmt.Errorf("AgentIntentCallStmt: agent %q is not declared", s.AgentName)
		}
		var intent *AgentIntentDecl
		for i := range ag.Intents {
			if ag.Intents[i].Name == s.IntentName {
				intent = &ag.Intents[i]
				break
			}
		}
		if intent == nil {
			return fmt.Errorf("AgentIntentCallStmt: agent %q has no intent %q", s.AgentName, s.IntentName)
		}
		if len(s.Args) != len(intent.Params) {
			return fmt.Errorf("AgentIntentCallStmt: agent %q intent %q expects %d args, got %d", s.AgentName, s.IntentName, len(intent.Params), len(s.Args))
		}
		for i, arg := range s.Args {
			if arg == nil {
				return fmt.Errorf("AgentIntentCallStmt: agent %q intent %q arg %d is nil", s.AgentName, s.IntentName, i)
			}
			if err := verifyExprCtx(ctx, arg); err != nil {
				return fmt.Errorf("AgentIntentCallStmt %q.%q arg %d: %w", s.AgentName, s.IntentName, i, err)
			}
			if arg.Type() != intent.Params[i].Type {
				return fmt.Errorf("AgentIntentCallStmt %q.%q arg %d: expected %s, got %s", s.AgentName, s.IntentName, i, intent.Params[i].Type, arg.Type())
			}
		}
		return nil
	}
	return fmt.Errorf("unhandled Stmt %T", st)
}

func verifyCallStmt(ctx *verifyCtx, s *CallStmt) error {
	params, err := resolveCallSig(ctx, s.Func)
	if err != nil {
		return err
	}
	if len(params) != len(s.Args) {
		return fmt.Errorf("callee %q expects %d args, got %d", s.Func, len(params), len(s.Args))
	}
	for k, arg := range s.Args {
		if arg == nil {
			return fmt.Errorf("callee %q arg %d is nil", s.Func, k)
		}
		if err := verifyExprCtx(ctx, arg); err != nil {
			return fmt.Errorf("callee %q arg %d: %w", s.Func, k, err)
		}
		if arg.Type() != params[k] {
			return fmt.Errorf("callee %q arg %d: expected %s, got %s", s.Func, k, params[k], arg.Type())
		}
	}
	return nil
}

// resolveCallSig returns the parameter-type list for a call to
// fnName. The lookup checks Builtins first (always wins over a
// user fn of the same name, by construction Lower rejects that
// shadow) then the program's function table, then the extern
// function table (Phase 10.0).
func resolveCallSig(ctx *verifyCtx, fnName string) ([]Type, error) {
	if p, ok := Builtins[fnName]; ok {
		return p, nil
	}
	if fn, ok := ctx.fns[fnName]; ok {
		params := make([]Type, len(fn.Params))
		for i, p := range fn.Params {
			params[i] = p.Type
		}
		return params, nil
	}
	// Phase 10.0: extern C function declarations.
	if ef, ok := ctx.externFns[fnName]; ok {
		params := make([]Type, len(ef.Params))
		for i, p := range ef.Params {
			params[i] = p.Type
		}
		return params, nil
	}
	return nil, fmt.Errorf("unresolved callee %q", fnName)
}

func verifyLetStmt(ctx *verifyCtx, s *LetStmt) error {
	if s.Name == "" {
		return errors.New("let with empty name")
	}
	if _, already := ctx.scope.vars[s.Name]; already {
		return fmt.Errorf("rebinding %q in same scope", s.Name)
	}
	// nil Init is allowed only for mutable pre-declarations (match result-var).
	if s.Init == nil && !s.Mutable {
		return fmt.Errorf("let %q has nil Init", s.Name)
	}
	if s.Init != nil {
		if err := verifyExprCtx(ctx, s.Init); err != nil {
			return fmt.Errorf("let %q init: %w", s.Name, err)
		}
		if s.Init.Type() != s.VarType {
			return fmt.Errorf("let %q: declared %s, init produces %s", s.Name, s.VarType, s.Init.Type())
		}
	}
	if s.VarType == TypeFun {
		if s.FunSig == nil {
			return fmt.Errorf("let %q: fun-typed binding missing FunSig", s.Name)
		}
		ctx.scope.vars[s.Name] = binding{t: s.VarType, mutable: s.Mutable, funSig: s.FunSig}
		return nil
	}
	if s.VarType == TypeRecord {
		if s.RecordName == "" {
			return fmt.Errorf("let %q: record-typed binding missing RecordName", s.Name)
		}
		if _, ok := ctx.records[s.RecordName]; !ok {
			return fmt.Errorf("let %q: record %q is not declared", s.Name, s.RecordName)
		}
		if s.Init != nil {
			initRec := exprRecordName(s.Init)
			if initRec != s.RecordName {
				return fmt.Errorf("let %q: declared record %q, init produces record %q", s.Name, s.RecordName, initRec)
			}
		}
	} else if s.RecordName != "" {
		return fmt.Errorf("let %q: RecordName set on non-record type %s", s.Name, s.VarType)
	}
	if s.VarType == TypeUnion {
		if s.UnionName == "" {
			return fmt.Errorf("let %q: union-typed binding missing UnionName", s.Name)
		}
		if _, ok := ctx.unions[s.UnionName]; !ok {
			return fmt.Errorf("let %q: union %q is not declared", s.Name, s.UnionName)
		}
	} else if s.UnionName != "" {
		return fmt.Errorf("let %q: UnionName set on non-union type %s", s.Name, s.VarType)
	}
	if s.VarType == TypeList {
		if !isListElemType(s.ElemType) {
			return fmt.Errorf("let %q: list binding has ElemType %s (Phase 3.4a supports scalar or record element types)", s.Name, s.ElemType)
		}
		if s.Init != nil {
			if ie := exprElemType(s.Init); ie != s.ElemType {
				return fmt.Errorf("let %q: declared list<%s>, init produces list<%s>", s.Name, s.ElemType, ie)
			}
		}
		if s.ElemType == TypeRecord {
			if s.ElemRecordName == "" {
				return fmt.Errorf("let %q: list<record> binding missing ElemRecordName", s.Name)
			}
			if _, ok := ctx.records[s.ElemRecordName]; !ok {
				return fmt.Errorf("let %q: list element record %q is not declared", s.Name, s.ElemRecordName)
			}
			if s.Init != nil {
				if ier := exprElemRecordName(s.Init); ier != s.ElemRecordName {
					return fmt.Errorf("let %q: declared list<%s>, init produces list<%s>", s.Name, s.ElemRecordName, ier)
				}
			}
		} else if s.ElemRecordName != "" {
			return fmt.Errorf("let %q: ElemRecordName set on list<%s> (only valid when ElemType==record)", s.Name, s.ElemType)
		}
		if s.ElemType == TypeList {
			if !isScalarElemType(s.InnerElemType) {
				return fmt.Errorf("let %q: list<list<T>> binding has InnerElemType %s (Phase 3.4b requires scalar inner)", s.Name, s.InnerElemType)
			}
		} else if s.InnerElemType != TypeInvalid {
			return fmt.Errorf("let %q: InnerElemType set on list<%s> (only valid when ElemType==list)", s.Name, s.ElemType)
		}
		if s.ElemType == TypeMap {
			if !isScalarKeyType(s.MapElemKeyType) {
				return fmt.Errorf("let %q: list<map<K,V>> binding has MapElemKeyType %s (Phase 3.4f requires int or string key)", s.Name, s.MapElemKeyType)
			}
			if !isScalarValueType(s.MapElemValueType) {
				return fmt.Errorf("let %q: list<map<K,V>> binding has MapElemValueType %s (Phase 3.4f requires scalar value)", s.Name, s.MapElemValueType)
			}
		} else if s.MapElemKeyType != TypeInvalid || s.MapElemValueType != TypeInvalid {
			return fmt.Errorf("let %q: MapElemKeyType/MapElemValueType set on list<%s> (only valid when ElemType==map)", s.Name, s.ElemType)
		}
	} else if s.VarType == TypeSet {
		// Phase 3.3: set<T> bindings carry ElemType for the set's element type.
		if s.ElemType == TypeInvalid {
			return fmt.Errorf("let %q: set binding missing ElemType", s.Name)
		}
		if !isScalarElemType(s.ElemType) {
			return fmt.Errorf("let %q: set binding has ElemType %s (Phase 3.3 supports scalar element types)", s.Name, s.ElemType)
		}
		if s.Init != nil {
			if ie := exprSetElemType(s.Init); ie != s.ElemType {
				return fmt.Errorf("let %q: declared set<%s>, init produces set<%s>", s.Name, s.ElemType, ie)
			}
		}
	} else if s.ElemType != TypeInvalid {
		return fmt.Errorf("let %q: ElemType set on non-list type %s", s.Name, s.VarType)
	}
	if s.VarType == TypeMap {
		if !isScalarKeyType(s.KeyType) {
			return fmt.Errorf("let %q: map binding has KeyType %s (Phase 3.2 supports int or string keys only)", s.Name, s.KeyType)
		}
		if !isMapValueType(s.ValueType) {
			return fmt.Errorf("let %q: map binding has ValueType %s (Phase 3.2/3.4e supports scalar or list values)", s.Name, s.ValueType)
		}
		if s.Init != nil {
			if ik := exprKeyType(s.Init); ik != s.KeyType {
				return fmt.Errorf("let %q: declared map<%s,...>, init produces map<%s,...>", s.Name, s.KeyType, ik)
			}
			if iv := exprValueType(s.Init); iv != s.ValueType {
				return fmt.Errorf("let %q: declared map<%s,%s>, init produces map<%s,%s>", s.Name, s.KeyType, s.ValueType, s.KeyType, iv)
			}
			if s.ValueType == TypeList {
				if !isScalarElemType(s.ListValueElemType) {
					return fmt.Errorf("let %q: map<_,list<T>> binding has ListValueElemType %s (Phase 3.4e requires scalar inner)", s.Name, s.ListValueElemType)
				}
				if ilv := exprListValueElemType(s.Init); ilv != s.ListValueElemType {
					return fmt.Errorf("let %q: declared map<_,list<%s>>, init produces map<_,list<%s>>", s.Name, s.ListValueElemType, ilv)
				}
			} else if s.ListValueElemType != TypeInvalid {
				return fmt.Errorf("let %q: ListValueElemType set on map<_,%s> (only valid when value is list)", s.Name, s.ValueType)
			}
		} else if s.ListValueElemType != TypeInvalid {
			return fmt.Errorf("let %q: ListValueElemType set on map<_,%s> (only valid when value is list)", s.Name, s.ValueType)
		}
	} else if s.VarType == TypeOMap {
		// Phase 3.4 omap: scalar key + scalar value only in initial implementation.
		if !isScalarKeyType(s.KeyType) {
			return fmt.Errorf("let %q: omap binding has KeyType %s (requires int or string)", s.Name, s.KeyType)
		}
		if !isScalarElemType(s.ValueType) {
			return fmt.Errorf("let %q: omap binding has ValueType %s (requires scalar)", s.Name, s.ValueType)
		}
		if s.Init != nil {
			if ik := exprKeyType(s.Init); ik != s.KeyType {
				return fmt.Errorf("let %q: declared omap<%s,...>, init produces omap<%s,...>", s.Name, s.KeyType, ik)
			}
			if iv := exprValueType(s.Init); iv != s.ValueType {
				return fmt.Errorf("let %q: declared omap<%s,%s>, init produces omap<%s,%s>", s.Name, s.KeyType, s.ValueType, s.KeyType, iv)
			}
		}
	} else {
		if s.KeyType != TypeInvalid {
			return fmt.Errorf("let %q: KeyType set on non-map type %s", s.Name, s.VarType)
		}
		if s.ValueType != TypeInvalid {
			return fmt.Errorf("let %q: ValueType set on non-map type %s", s.Name, s.VarType)
		}
		if s.ListValueElemType != TypeInvalid {
			return fmt.Errorf("let %q: ListValueElemType set on non-map type %s", s.Name, s.VarType)
		}
	}
	if s.VarType == TypeChan {
		if s.ChanElemType == TypeInvalid {
			return fmt.Errorf("let %q: chan binding missing ChanElemType", s.Name)
		}
	} else if s.ChanElemType != TypeInvalid {
		return fmt.Errorf("let %q: ChanElemType set on non-chan type %s", s.Name, s.VarType)
	}
	if s.VarType == TypeStream {
		if s.StreamElemType == TypeInvalid {
			return fmt.Errorf("let %q: stream binding missing StreamElemType", s.Name)
		}
	} else if s.StreamElemType != TypeInvalid {
		return fmt.Errorf("let %q: StreamElemType set on non-stream type %s", s.Name, s.VarType)
	}
	if s.VarType == TypeSub {
		if s.SubElemType == TypeInvalid {
			return fmt.Errorf("let %q: sub binding missing SubElemType", s.Name)
		}
	} else if s.SubElemType != TypeInvalid {
		return fmt.Errorf("let %q: SubElemType set on non-sub type %s", s.Name, s.VarType)
	}
	if s.VarType == TypeAgent {
		if s.AgentName == "" {
			return fmt.Errorf("let %q: agent binding missing AgentName", s.Name)
		}
		if _, ok := ctx.agents[s.AgentName]; !ok {
			return fmt.Errorf("let %q: agent %q is not declared", s.Name, s.AgentName)
		}
	} else if s.AgentName != "" {
		return fmt.Errorf("let %q: AgentName set on non-agent type %s", s.Name, s.VarType)
	}
	ctx.scope.vars[s.Name] = binding{t: s.VarType, mutable: s.Mutable, record: s.RecordName, union: s.UnionName, elem: s.ElemType, elemRec: s.ElemRecordName, mapElemKey: s.MapElemKeyType, mapElemValue: s.MapElemValueType, key: s.KeyType, value: s.ValueType, listValElem: s.ListValueElemType, funSig: s.FunSig, chanElem: s.ChanElemType, streamElem: s.StreamElemType, subElem: s.SubElemType, agentName: s.AgentName}
	return nil
}

func verifyAssignStmt(ctx *verifyCtx, s *AssignStmt) error {
	b, ok := ctx.scope.lookup(s.Name)
	if !ok {
		return fmt.Errorf("assign to undeclared %q", s.Name)
	}
	if !b.mutable {
		return fmt.Errorf("assign to immutable binding %q (declared with let)", s.Name)
	}
	if s.Value == nil {
		return fmt.Errorf("assign %q has nil Value", s.Name)
	}
	if err := verifyExprCtx(ctx, s.Value); err != nil {
		return fmt.Errorf("assign %q: %w", s.Name, err)
	}
	if s.Value.Type() != b.t {
		return fmt.Errorf("assign %q: binding is %s, value is %s", s.Name, b.t, s.Value.Type())
	}
	if b.t == TypeRecord {
		if vrec := exprRecordName(s.Value); vrec != b.record {
			return fmt.Errorf("assign %q: binding holds record %q, value produces record %q", s.Name, b.record, vrec)
		}
	}
	if b.t == TypeList {
		if ve := exprElemType(s.Value); ve != b.elem {
			return fmt.Errorf("assign %q: binding holds list<%s>, value produces list<%s>", s.Name, b.elem, ve)
		}
		if b.elem == TypeRecord {
			if ver := exprElemRecordName(s.Value); ver != b.elemRec {
				return fmt.Errorf("assign %q: binding holds list<%s>, value produces list<%s>", s.Name, b.elemRec, ver)
			}
		}
	}
	if b.t == TypeMap {
		if vk := exprKeyType(s.Value); vk != b.key {
			return fmt.Errorf("assign %q: binding holds map<%s,...>, value produces map<%s,...>", s.Name, b.key, vk)
		}
		if vv := exprValueType(s.Value); vv != b.value {
			return fmt.Errorf("assign %q: binding holds map<%s,%s>, value produces map<%s,%s>", s.Name, b.key, b.value, b.key, vv)
		}
	}
	if b.t == TypeSet {
		if ve := exprSetElemType(s.Value); ve != b.elem {
			return fmt.Errorf("assign %q: binding holds set<%s>, value produces set<%s>", s.Name, b.elem, ve)
		}
	}
	if b.t == TypeOMap {
		if vk := exprKeyType(s.Value); vk != b.key {
			return fmt.Errorf("assign %q: binding holds omap<%s,...>, value produces omap<%s,...>", s.Name, b.key, vk)
		}
		if vv := exprValueType(s.Value); vv != b.value {
			return fmt.Errorf("assign %q: binding holds omap<%s,%s>, value produces omap<%s,%s>", s.Name, b.key, b.value, b.key, vv)
		}
	}
	return nil
}

func verifyListSetStmt(ctx *verifyCtx, s *ListSetStmt) error {
	b, ok := ctx.scope.lookup(s.Name)
	if !ok {
		return fmt.Errorf("list-set: undeclared %q", s.Name)
	}
	if !b.mutable {
		return fmt.Errorf("list-set: %q is immutable", s.Name)
	}
	if b.t != TypeList {
		return fmt.Errorf("list-set: %q is %s, not a list", s.Name, b.t)
	}
	if s.Index == nil {
		return fmt.Errorf("list-set %q: nil Index", s.Name)
	}
	if err := verifyExprCtx(ctx, s.Index); err != nil {
		return fmt.Errorf("list-set %q index: %w", s.Name, err)
	}
	if s.Index.Type() != TypeInt {
		return fmt.Errorf("list-set %q: index must be int, got %s", s.Name, s.Index.Type())
	}
	if s.Value == nil {
		return fmt.Errorf("list-set %q: nil Value", s.Name)
	}
	if err := verifyExprCtx(ctx, s.Value); err != nil {
		return fmt.Errorf("list-set %q value: %w", s.Name, err)
	}
	if s.Value.Type() != b.elem {
		return fmt.Errorf("list-set %q: binding elem is %s, value is %s", s.Name, b.elem, s.Value.Type())
	}
	return nil
}

func verifyMapPutStmt(ctx *verifyCtx, s *MapPutStmt) error {
	b, ok := ctx.scope.lookup(s.Name)
	if !ok {
		return fmt.Errorf("map-put: undeclared %q", s.Name)
	}
	if !b.mutable {
		return fmt.Errorf("map-put: %q is immutable", s.Name)
	}
	if b.t != TypeMap {
		return fmt.Errorf("map-put: %q is %s, not a map", s.Name, b.t)
	}
	if s.Key == nil {
		return fmt.Errorf("map-put %q: nil Key", s.Name)
	}
	if err := verifyExprCtx(ctx, s.Key); err != nil {
		return fmt.Errorf("map-put %q key: %w", s.Name, err)
	}
	if s.Key.Type() != b.key {
		return fmt.Errorf("map-put %q: binding key is %s, got %s", s.Name, b.key, s.Key.Type())
	}
	if s.Value == nil {
		return fmt.Errorf("map-put %q: nil Value", s.Name)
	}
	if err := verifyExprCtx(ctx, s.Value); err != nil {
		return fmt.Errorf("map-put %q value: %w", s.Name, err)
	}
	if s.Value.Type() != b.value {
		return fmt.Errorf("map-put %q: binding value is %s, got %s", s.Name, b.value, s.Value.Type())
	}
	return nil
}

func verifyIfStmt(ctx *verifyCtx, s *IfStmt) error {
	if s.Cond == nil {
		return errors.New("if with nil Cond")
	}
	if err := verifyExprCtx(ctx, s.Cond); err != nil {
		return fmt.Errorf("if cond: %w", err)
	}
	if s.Cond.Type() != TypeBool {
		return fmt.Errorf("if cond must be bool, got %s", s.Cond.Type())
	}
	if s.Then == nil {
		return errors.New("if with nil Then block")
	}
	if err := verifyBlock(ctx, s.Then); err != nil {
		return fmt.Errorf("if then: %w", err)
	}
	if s.Else != nil {
		if err := verifyBlock(ctx, s.Else); err != nil {
			return fmt.Errorf("if else: %w", err)
		}
	}
	return nil
}

func verifyWhileStmt(ctx *verifyCtx, s *WhileStmt) error {
	if s.Cond == nil {
		return errors.New("while with nil Cond")
	}
	if err := verifyExprCtx(ctx, s.Cond); err != nil {
		return fmt.Errorf("while cond: %w", err)
	}
	if s.Cond.Type() != TypeBool {
		return fmt.Errorf("while cond must be bool, got %s", s.Cond.Type())
	}
	if s.Body == nil {
		return errors.New("while with nil Body block")
	}
	ctx.loopDepth++
	defer func() { ctx.loopDepth-- }()
	return verifyBlock(ctx, s.Body)
}

// verifyForRangeStmt checks `for VAR in START..END { BODY }`. Start
// and End must be TypeInt; Var is introduced into the body scope as
// an immutable TypeInt binding, so assigning to it inside the body
// fails the mutability check.
func verifyForRangeStmt(ctx *verifyCtx, s *ForRangeStmt) error {
	if s.Var == "" {
		return errors.New("for with empty Var name")
	}
	if s.Start == nil || s.End == nil {
		return errors.New("for range with nil Start or End")
	}
	if err := verifyExprCtx(ctx, s.Start); err != nil {
		return fmt.Errorf("for start: %w", err)
	}
	if s.Start.Type() != TypeInt {
		return fmt.Errorf("for start must be int, got %s", s.Start.Type())
	}
	if err := verifyExprCtx(ctx, s.End); err != nil {
		return fmt.Errorf("for end: %w", err)
	}
	if s.End.Type() != TypeInt {
		return fmt.Errorf("for end must be int, got %s", s.End.Type())
	}
	if s.Body == nil {
		return errors.New("for with nil Body block")
	}
	prev := ctx.scope
	ctx.scope = newScope(prev)
	ctx.scope.vars[s.Var] = binding{t: TypeInt, mutable: false}
	ctx.loopDepth++
	defer func() {
		ctx.loopDepth--
		ctx.scope = prev
	}()
	for i, st := range s.Body.Statements {
		if err := verifyStmt(ctx, st); err != nil {
			return fmt.Errorf("for body stmt %d: %w", i, err)
		}
	}
	return nil
}

// verifyForEachStmt checks `for VAR in LIST { BODY }` for a
// list-typed LIST expression. VAR is introduced into the body scope
// as an immutable binding of LIST's ElemType (assigning to it inside
// the body fails the mutability check). Phase 3.1 restricts the
// element type to the four scalar primitives; the verifier rejects
// any other ElemType outright so a future list-of-record fixture
// fails loudly until the wider phase lands.
func verifyForEachStmt(ctx *verifyCtx, s *ForEachStmt) error {
	if s.Var == "" {
		return errors.New("foreach with empty Var name")
	}
	if s.List == nil {
		return errors.New("foreach with nil List")
	}
	if err := verifyExprCtx(ctx, s.List); err != nil {
		return fmt.Errorf("foreach list: %w", err)
	}
	if s.List.Type() != TypeList {
		return fmt.Errorf("foreach list must be list, got %s", s.List.Type())
	}
	if le := exprElemType(s.List); le != s.ElemType {
		return fmt.Errorf("foreach: stamped ElemType %s does not match list's ElemType %s", s.ElemType, le)
	}
	if !isListElemType(s.ElemType) {
		return fmt.Errorf("foreach: ElemType %s not supported (Phase 3.4a supports scalar or record element types)", s.ElemType)
	}
	if s.ElemType == TypeRecord {
		if s.ElemRecordName == "" {
			return errors.New("foreach over list<record> missing ElemRecordName")
		}
		if _, ok := ctx.records[s.ElemRecordName]; !ok {
			return fmt.Errorf("foreach: element record %q is not declared", s.ElemRecordName)
		}
		if ler := exprElemRecordName(s.List); ler != s.ElemRecordName {
			return fmt.Errorf("foreach: stamped ElemRecordName %q does not match list's ElemRecordName %q", s.ElemRecordName, ler)
		}
	} else if s.ElemRecordName != "" {
		return fmt.Errorf("foreach: ElemRecordName set on list<%s> (only valid when ElemType==record)", s.ElemType)
	}
	if s.ElemType == TypeList {
		if !isScalarElemType(s.InnerElemType) {
			return fmt.Errorf("foreach: list<list<T>> body requires scalar inner, got InnerElemType %s", s.InnerElemType)
		}
	} else if s.InnerElemType != TypeInvalid {
		return fmt.Errorf("foreach: InnerElemType set on list<%s> (only valid when ElemType==list)", s.ElemType)
	}
	if s.ElemType == TypeMap {
		if !isScalarKeyType(s.MapElemKeyType) {
			return fmt.Errorf("foreach: list<map<K,V>> requires int or string key, got MapElemKeyType %s", s.MapElemKeyType)
		}
		if !isScalarValueType(s.MapElemValueType) {
			return fmt.Errorf("foreach: list<map<K,V>> requires scalar value, got MapElemValueType %s", s.MapElemValueType)
		}
	} else if s.MapElemKeyType != TypeInvalid || s.MapElemValueType != TypeInvalid {
		return fmt.Errorf("foreach: MapElemKeyType/MapElemValueType set on list<%s> (only valid when ElemType==map)", s.ElemType)
	}
	if s.Body == nil {
		return errors.New("foreach with nil Body block")
	}
	prev := ctx.scope
	ctx.scope = newScope(prev)
	// When ElemType==TypeList, the induction variable is bound to a
	// list<innerT> value, so the binding's elem is the InnerElemType.
	// When ElemType==TypeMap, the induction variable is a map<K,V>;
	// the binding's key and value carry K and V.
	bindElem := TypeInvalid
	if s.ElemType == TypeList {
		bindElem = s.InnerElemType
	}
	ctx.scope.vars[s.Var] = binding{t: s.ElemType, mutable: false, record: s.ElemRecordName, elem: bindElem, key: s.MapElemKeyType, value: s.MapElemValueType}
	ctx.loopDepth++
	defer func() {
		ctx.loopDepth--
		ctx.scope = prev
	}()
	for i, st := range s.Body.Statements {
		if err := verifyStmt(ctx, st); err != nil {
			return fmt.Errorf("foreach body stmt %d: %w", i, err)
		}
	}
	return nil
}

func verifyMatchStmt(ctx *verifyCtx, s *MatchStmt) error {
	if s.Target == nil {
		return errors.New("MatchStmt: nil Target")
	}
	if err := verifyExprCtx(ctx, s.Target); err != nil {
		return fmt.Errorf("MatchStmt target: %w", err)
	}
	if s.Target.Type() != TypeUnion {
		return fmt.Errorf("MatchStmt: target must be TypeUnion, got %s", s.Target.Type())
	}
	u, ok := ctx.unions[s.UnionName]
	if !ok {
		return fmt.Errorf("MatchStmt: union %q not declared", s.UnionName)
	}
	variantByName := make(map[string]VariantDecl, len(u.Variants))
	for _, v := range u.Variants {
		variantByName[v.Name] = v
	}
	verifyArm := func(arm *MatchArm) error {
		prev := ctx.scope
		ctx.scope = newScope(prev)
		defer func() { ctx.scope = prev }()
		for _, b := range arm.Bindings {
			ctx.scope.vars[b.VarName] = binding{t: b.FieldType, mutable: false, record: b.RecordName}
		}
		for i, st := range arm.Body.Statements {
			if err := verifyStmt(ctx, st); err != nil {
				return fmt.Errorf("arm %q stmt %d: %w", arm.VariantName, i, err)
			}
		}
		return nil
	}
	for i := range s.Arms {
		if _, ok := variantByName[s.Arms[i].VariantName]; !ok {
			return fmt.Errorf("MatchStmt: arm %d: unknown variant %q in union %q", i, s.Arms[i].VariantName, s.UnionName)
		}
		if err := verifyArm(&s.Arms[i]); err != nil {
			return fmt.Errorf("MatchStmt arm %d: %w", i, err)
		}
	}
	if s.Default != nil {
		if err := verifyArm(s.Default); err != nil {
			return fmt.Errorf("MatchStmt default arm: %w", err)
		}
	}
	return nil
}

func verifyReturnStmt(ctx *verifyCtx, s *ReturnStmt) error {
	if s.Value == nil {
		if ctx.returnType != TypeUnit {
			return fmt.Errorf("bare return inside function returning %s", ctx.returnType)
		}
		return nil
	}
	if err := verifyExprCtx(ctx, s.Value); err != nil {
		return fmt.Errorf("return value: %w", err)
	}
	if s.Value.Type() != ctx.returnType {
		return fmt.Errorf("return value type %s does not match function return %s",
			s.Value.Type(), ctx.returnType)
	}
	if ctx.returnType == TypeRecord {
		if vr := exprRecordName(s.Value); vr != ctx.returnRec {
			return fmt.Errorf("return record %q does not match function return record %q", vr, ctx.returnRec)
		}
	}
	if ctx.returnType == TypeList {
		if ve := exprElemType(s.Value); ve != ctx.returnElem {
			return fmt.Errorf("return list<%s> does not match function return list<%s>", ve, ctx.returnElem)
		}
		if ctx.returnElem == TypeRecord {
			if ver := exprElemRecordName(s.Value); ver != ctx.returnElemRec {
				return fmt.Errorf("return list<%s> does not match function return list<%s>", ver, ctx.returnElemRec)
			}
		}
		if ctx.returnElem == TypeList {
			if vi := exprInnerElemType(s.Value); vi != ctx.returnInnerElem {
				return fmt.Errorf("return list<list<%s>> does not match function return list<list<%s>>", vi, ctx.returnInnerElem)
			}
		}
	}
	if ctx.returnType == TypeMap {
		if vk := exprKeyType(s.Value); vk != ctx.returnKey {
			return fmt.Errorf("return map<%s,...> does not match function return map<%s,...>", vk, ctx.returnKey)
		}
		if vv := exprValueType(s.Value); vv != ctx.returnValue {
			return fmt.Errorf("return map<%s,%s> does not match function return map<%s,%s>", ctx.returnKey, vv, ctx.returnKey, ctx.returnValue)
		}
		if ctx.returnValue == TypeList {
			if vlv := exprListValueElemType(s.Value); vlv != ctx.returnListValElem {
				return fmt.Errorf("return map<_,list<%s>> does not match function return map<_,list<%s>>", vlv, ctx.returnListValElem)
			}
		}
	}
	return nil
}

// exprRecordName returns the record-name identity of a record-typed
// expression, or "" if the expression is not record-typed. Phase 3.0
// covers the small set of nodes that can carry a record value: VarRef,
// RecordLit, FieldAccess, CallExpr. Hand-built fixtures that bypass the
// lowerer must still set these fields correctly.
func exprRecordName(e Expr) string {
	switch v := e.(type) {
	case *VarRef:
		return v.RecordName
	case *RecordLit:
		return v.TypeName
	case *FieldAccess:
		return v.ResultRecordName
	case *CallExpr:
		return v.ResultRecordName
	case *IndexExpr:
		// Phase 3.4a: list<R> indexing returns a record value.
		return v.ElemRecordName
	}
	return ""
}

// exprElemRecordName returns the element-record identity of a
// list-of-record-typed expression, or "" if the expression is not a
// list of record. Phase 3.4a node coverage: VarRef, ListLit, CallExpr,
// AppendExpr.
func exprElemRecordName(e Expr) string {
	switch v := e.(type) {
	case *VarRef:
		return v.ElemRecordName
	case *ListLit:
		return v.ElemRecordName
	case *CallExpr:
		return v.ResultElemRecordName
	case *AppendExpr:
		return v.ElemRecordName
	case *ListSortAscExpr:
		return v.ElemRecordName
	case *ListSliceExpr:
		return v.ElemRecordName
	}
	return ""
}

// exprElemType returns the element type of a list-typed expression,
// or TypeInvalid if the expression is not list-typed. Phase 3.1
// covers VarRef, ListLit, CallExpr, AppendExpr. Lists never appear
// as field reads in 3.1 because record fields cannot hold lists.
// Phase 3.2 adds MapKeysExpr / MapValuesExpr because both produce
// list-typed values (list<K> and list<V> respectively). Phase 3.4b
// adds IndexExpr: indexing a list<list<T>> produces a list<T> value,
// whose element type T is carried on IndexExpr.InnerElemType.
func exprElemType(e Expr) Type {
	switch v := e.(type) {
	case *VarRef:
		return v.ElemType
	case *ListLit:
		return v.ElemType
	case *CallExpr:
		return v.ResultElemType
	case *AppendExpr:
		return v.ElemType
	case *ListSortAscExpr:
		return v.ElemType
	case *ListSliceExpr:
		return v.ElemType
	case *IndexExpr:
		// When the IndexExpr itself produces a list value (i.e.,
		// receiver was list<list<T>>), its own element type is T,
		// recorded on InnerElemType.
		if v.ElemType == TypeList {
			return v.InnerElemType
		}
		return TypeInvalid
	case *MapGetExpr:
		// When a map<K,list<T>> is indexed, the result is list<T>; return T.
		if v.ValueType == TypeList {
			return v.ListValueElemType
		}
		return TypeInvalid
	case *MapKeysExpr:
		return v.KeyType
	case *MapValuesExpr:
		return v.ValueType
	case *StrSplitExpr:
		return TypeString
	case *LinesExpr:
		return TypeString // lines() always returns list<string>
	case *LoadCSVExpr:
		return TypeList // loadCSV() always returns list<list<string>>; inner is TypeString
	case *RawCExpr:
		// Phase 15.0: Datalog query results are list<string>; elem is TypeString.
		if v.RawType == TypeList {
			return TypeString
		}
		return TypeInvalid
	case *DatalogQueryExpr:
		// Phase 8.0: Datalog query results are list<string>; elem is TypeString.
		return TypeString
	case *ListMapExpr:
		return v.ElemType
	case *ListFilterExpr:
		return v.ElemType
	case *SetToListExpr:
		// Phase 3.3: sets:to_list(S) produces a list<T> where T is the set's elem.
		return v.ElemType
	}
	return TypeInvalid
}

// exprInnerElemType returns the inner element type of a
// list<list<T>>-typed expression, or TypeInvalid otherwise.
// Phase 3.4b node coverage: VarRef, ListLit, CallExpr, AppendExpr,
// IndexExpr. Phase 3.4e adds MapValuesExpr: when values(m) is called
// on a map<K,list<V>>, the result is list<list<V>> and the inner V
// is the map's ListValueElemType.
func exprInnerElemType(e Expr) Type {
	switch v := e.(type) {
	case *VarRef:
		return v.InnerElemType
	case *ListLit:
		return v.InnerElemType
	case *CallExpr:
		return v.ResultInnerElemType
	case *AppendExpr:
		return v.InnerElemType
	case *ListSortAscExpr:
		return v.InnerElemType
	case *ListSliceExpr:
		return v.InnerElemType
	case *IndexExpr:
		return v.InnerElemType
	case *MapValuesExpr:
		// values(m) on map<K,list<V>> produces list<list<V>>.
		// The inner V is carried on MapValuesExpr.ListValueElemType.
		return v.ListValueElemType
	case *LoadCSVExpr:
		return TypeString // loadCSV() returns list<list<string>>; inner is TypeString
	}
	return TypeInvalid
}

// exprKeyType returns the key type of a map-typed expression, or
// TypeInvalid if the expression is not map-typed. Phase 3.2 covers
// VarRef, MapLit, CallExpr. Maps never appear as field reads in
// 3.2 because record fields cannot hold maps and (until Phase 3.4)
// map values cannot themselves be maps. Phase 3.4f adds IndexExpr:
// indexing a list<map<K,V>> produces a map<K,V> value whose key type
// is carried on IndexExpr.MapElemKeyType.
func exprKeyType(e Expr) Type {
	switch v := e.(type) {
	case *VarRef:
		return v.KeyType
	case *MapLit:
		return v.KeyType
	case *CallExpr:
		return v.ResultKeyType
	case *IndexExpr:
		// list<map<K,V>> indexing produces a map<K,V>.
		if v.ElemType == TypeMap {
			return v.MapElemKeyType
		}
	case *JsonDecodeExpr:
		return TypeString
	case *OMapLiteralExpr:
		return v.KeyType
	case *OMapSetExpr:
		return v.KeyType
	}
	return TypeInvalid
}

// exprValueType returns the value type of a map-typed expression,
// or TypeInvalid if the expression is not map-typed. See exprKeyType
// for the node coverage rationale. Phase 3.4f adds IndexExpr.
func exprValueType(e Expr) Type {
	switch v := e.(type) {
	case *VarRef:
		return v.ValueType
	case *MapLit:
		return v.ValueType
	case *CallExpr:
		return v.ResultValueType
	case *IndexExpr:
		// list<map<K,V>> indexing produces a map<K,V>.
		if v.ElemType == TypeMap {
			return v.MapElemValueType
		}
	case *JsonDecodeExpr:
		return TypeString
	case *OMapLiteralExpr:
		return v.ValueType
	case *OMapSetExpr:
		return v.ValueType
	}
	return TypeInvalid
}

// exprListValueElemType returns the inner scalar element type of the
// list value in a map<K,list<V>>-typed expression, or TypeInvalid
// if the expression is not such a map. Only meaningful on expressions
// whose Type() is TypeMap. MapValuesExpr is excluded: values(m) returns
// a list, not a map, so the binding's inner type is InnerElemType.
func exprListValueElemType(e Expr) Type {
	switch v := e.(type) {
	case *VarRef:
		return v.ListValueElemType
	case *MapLit:
		return v.ListValueElemType
	case *CallExpr:
		return v.ResultListValueElemType
	}
	return TypeInvalid
}

// exprMapElemKeyType returns the key type of the map element in a
// list<map<K,V>>-typed expression, or TypeInvalid otherwise. Only
// meaningful on expressions whose Type() is TypeList with ElemType==TypeMap.
func exprMapElemKeyType(e Expr) Type {
	switch v := e.(type) {
	case *VarRef:
		return v.MapElemKeyType
	case *ListLit:
		return v.MapElemKeyType
	case *CallExpr:
		return v.ResultMapElemKeyType
	case *AppendExpr:
		return v.MapElemKeyType
	case *ListSortAscExpr:
		return v.MapElemKeyType
	case *ListSliceExpr:
		return v.MapElemKeyType
	case *IndexExpr:
		// IndexExpr over list<map<K,V>> produces a map<K,V>; no further unwrap here.
		return TypeInvalid
	}
	return TypeInvalid
}

// exprMapElemValueType returns the value type of the map element in a
// list<map<K,V>>-typed expression, or TypeInvalid otherwise.
func exprMapElemValueType(e Expr) Type {
	switch v := e.(type) {
	case *VarRef:
		return v.MapElemValueType
	case *ListLit:
		return v.MapElemValueType
	case *CallExpr:
		return v.ResultMapElemValueType
	case *AppendExpr:
		return v.MapElemValueType
	case *ListSortAscExpr:
		return v.MapElemValueType
	case *ListSliceExpr:
		return v.MapElemValueType
	case *IndexExpr:
		return TypeInvalid
	}
	return TypeInvalid
}

// isScalarElemType reports whether t is a Phase 3.1 scalar list
// element type. Kept for the carriers that still require a scalar
// (e.g. map keys/values surfaces); the broader list-element gate
// is isListElemType.
func isScalarElemType(t Type) bool {
	switch t {
	case TypeInt, TypeFloat, TypeBool, TypeString:
		return true
	}
	return false
}

// isListElemType reports whether t is a valid list element type.
// Phase 3.1 accepted the four scalar primitives; Phase 3.4a widens
// this to TypeRecord (records as list elements); Phase 3.4b widens
// it again to TypeList (one-level nested list<list<T>> where T is a
// scalar primitive carried on InnerElemType). Phase 3.4f widens it
// to TypeMap (list<map<K,V>> where K and V are scalars).
func isListElemType(t Type) bool {
	switch t {
	case TypeInt, TypeFloat, TypeBool, TypeString, TypeRecord, TypeList, TypeMap, TypeFuture:
		return true
	}
	return false
}

// isScalarKeyType reports whether t is a valid Phase 3.2 map key
// type. Only int and string keys are supported (matches the 8
// runtime instantiations in transpiler3/c/runtime/src/map.c).
func isScalarKeyType(t Type) bool {
	switch t {
	case TypeInt, TypeString:
		return true
	}
	return false
}

// isScalarValueType reports whether t is a valid Phase 3.2 map
// value type. The four scalar primitives (int, float, bool,
// string); record / list / nested-map values land in later
// sub-phases.
func isScalarValueType(t Type) bool {
	switch t {
	case TypeInt, TypeFloat, TypeBool, TypeString:
		return true
	}
	return false
}

// isMapValueType reports whether t is a valid map value type.
// Phase 3.2 accepted the four scalar primitives; Phase 3.4e widens
// this to TypeList (map<K, list<V>> where V is a scalar primitive
// carried on ListValueElemType). Nested maps remain rejected.
func isMapValueType(t Type) bool {
	switch t {
	case TypeInt, TypeFloat, TypeBool, TypeString, TypeList:
		return true
	}
	return false
}

// verifyTryCatchStmt verifies a TryCatchStmt. The try body runs in a fresh
// scope. The catch body runs in a fresh scope with CatchVar bound as TypeInt
// (holds mochi_except_code).
func verifyTryCatchStmt(ctx *verifyCtx, s *TryCatchStmt) error {
	if s.BufName == "" {
		return errors.New("TryCatchStmt: empty BufName")
	}
	if s.CatchVar == "" {
		return errors.New("TryCatchStmt: empty CatchVar")
	}
	if s.TryBody == nil {
		return errors.New("TryCatchStmt: nil TryBody")
	}
	if s.CatchBody == nil {
		return errors.New("TryCatchStmt: nil CatchBody")
	}
	if err := verifyBlock(ctx, s.TryBody); err != nil {
		return fmt.Errorf("TryCatchStmt try: %w", err)
	}
	// Verify catch body with CatchVar in scope.
	prev := ctx.scope
	ctx.scope = newScope(prev)
	ctx.scope.vars[s.CatchVar] = binding{t: TypeInt, mutable: false}
	defer func() { ctx.scope = prev }()
	return verifyBlock(ctx, s.CatchBody)
}

func verifyBlock(ctx *verifyCtx, b *Block) error {
	prev := ctx.scope
	ctx.scope = newScope(prev)
	defer func() { ctx.scope = prev }()
	for i, st := range b.Statements {
		if err := verifyStmt(ctx, st); err != nil {
			return fmt.Errorf("stmt %d: %w", i, err)
		}
	}
	return nil
}

// verifyExpr is the public entry for hand-built fixtures that
// don't construct a verifyCtx. It builds an empty one and reuses
// the scoped verifier; VarRef nodes will fail because nothing is
// in scope, which is the intent (positive tests should go via
// Verify(Program)).
func verifyExpr(e Expr) error {
	ctx := &verifyCtx{scope: newScope(nil)}
	return verifyExprCtx(ctx, e)
}

func verifyExprCtx(ctx *verifyCtx, e Expr) error {
	switch v := e.(type) {
	case *StringLit, *IntLit, *FloatLit, *BoolLit:
		return nil
	case *VarRef:
		b, ok := ctx.scope.lookup(v.Name)
		if !ok {
			return fmt.Errorf("unresolved variable %q", v.Name)
		}
		if v.VarType != b.t {
			return fmt.Errorf("variable %q has type %s in scope, ref says %s", v.Name, b.t, v.VarType)
		}
		if b.t == TypeRecord && v.RecordName != b.record {
			return fmt.Errorf("variable %q has record %q in scope, ref says %q", v.Name, b.record, v.RecordName)
		}
		if b.t == TypeList && v.ElemType != b.elem {
			return fmt.Errorf("variable %q has list<%s> in scope, ref says list<%s>", v.Name, b.elem, v.ElemType)
		}
		if b.t == TypeList && b.elem == TypeRecord && v.ElemRecordName != b.elemRec {
			return fmt.Errorf("variable %q has list<%s> in scope, ref says list<%s>", v.Name, b.elemRec, v.ElemRecordName)
		}
		if b.t == TypeList && b.elem == TypeMap {
			if v.MapElemKeyType != b.mapElemKey {
				return fmt.Errorf("variable %q has list<map<%s,_>> in scope, ref says list<map<%s,_>>", v.Name, b.mapElemKey, v.MapElemKeyType)
			}
			if v.MapElemValueType != b.mapElemValue {
				return fmt.Errorf("variable %q has list<map<_,%s>> in scope, ref says list<map<_,%s>>", v.Name, b.mapElemValue, v.MapElemValueType)
			}
		}
		if b.t == TypeMap {
			if v.KeyType != b.key {
				return fmt.Errorf("variable %q has map<%s,_> in scope, ref says map<%s,_>", v.Name, b.key, v.KeyType)
			}
			if v.ValueType != b.value {
				return fmt.Errorf("variable %q has map<_,%s> in scope, ref says map<_,%s>", v.Name, b.value, v.ValueType)
			}
			if b.value == TypeList && v.ListValueElemType != b.listValElem {
				return fmt.Errorf("variable %q has map<_,list<%s>> in scope, ref says map<_,list<%s>>", v.Name, b.listValElem, v.ListValueElemType)
			}
		}
		if b.t == TypeChan && v.ChanElemType != b.chanElem {
			return fmt.Errorf("variable %q has chan<%s> in scope, ref says chan<%s>", v.Name, b.chanElem, v.ChanElemType)
		}
		if b.t == TypeStream && v.StreamElemType != b.streamElem {
			return fmt.Errorf("variable %q has stream<%s> in scope, ref says stream<%s>", v.Name, b.streamElem, v.StreamElemType)
		}
		if b.t == TypeSub && v.SubElemType != b.subElem {
			return fmt.Errorf("variable %q has sub<%s> in scope, ref says sub<%s>", v.Name, b.subElem, v.SubElemType)
		}
		if b.t == TypeAgent && v.AgentName != b.agentName {
			return fmt.Errorf("variable %q has agent %q in scope, ref says %q", v.Name, b.agentName, v.AgentName)
		}
		return nil
	case *AgentSpawnExpr:
		// Phase 9.1: spawn AgentType() is valid when the agent is declared.
		if v.AgentName == "" {
			return errors.New("AgentSpawnExpr with empty AgentName")
		}
		if _, ok := ctx.agents[v.AgentName]; !ok {
			return fmt.Errorf("AgentSpawnExpr: agent %q is not declared", v.AgentName)
		}
		for i, a := range v.InitArgs {
			if a == nil {
				return fmt.Errorf("AgentSpawnExpr %q init arg %d is nil", v.AgentName, i)
			}
			if err := verifyExprCtx(ctx, a); err != nil {
				return fmt.Errorf("AgentSpawnExpr %q init arg %d: %w", v.AgentName, i, err)
			}
		}
		return nil
	case *AgentLit:
		if v.AgentName == "" {
			return errors.New("AgentLit with empty AgentName")
		}
		if _, ok := ctx.agents[v.AgentName]; !ok {
			return fmt.Errorf("AgentLit: agent %q is not declared", v.AgentName)
		}
		for i, f := range v.Fields {
			if f.Value == nil {
				return fmt.Errorf("AgentLit %q field %d has nil Value", v.AgentName, i)
			}
			if err := verifyExprCtx(ctx, f.Value); err != nil {
				return fmt.Errorf("AgentLit %q field %q: %w", v.AgentName, f.Name, err)
			}
		}
		return nil
	case *AgentIntentCallExpr:
		if v.AgentName == "" {
			return errors.New("AgentIntentCallExpr: empty AgentName")
		}
		if v.IntentName == "" {
			return errors.New("AgentIntentCallExpr: empty IntentName")
		}
		if v.Receiver == nil {
			return errors.New("AgentIntentCallExpr: nil Receiver")
		}
		if v.Receiver.Type() != TypeAgent {
			return fmt.Errorf("AgentIntentCallExpr: Receiver must be TypeAgent, got %s", v.Receiver.Type())
		}
		if err := verifyExprCtx(ctx, v.Receiver); err != nil {
			return fmt.Errorf("AgentIntentCallExpr receiver: %w", err)
		}
		ag, ok := ctx.agents[v.AgentName]
		if !ok {
			return fmt.Errorf("AgentIntentCallExpr: agent %q is not declared", v.AgentName)
		}
		var intent *AgentIntentDecl
		for i := range ag.Intents {
			if ag.Intents[i].Name == v.IntentName {
				intent = &ag.Intents[i]
				break
			}
		}
		if intent == nil {
			return fmt.Errorf("AgentIntentCallExpr: agent %q has no intent %q", v.AgentName, v.IntentName)
		}
		if v.Result != intent.ReturnType {
			return fmt.Errorf("AgentIntentCallExpr %q.%q: Result %s != intent ReturnType %s", v.AgentName, v.IntentName, v.Result, intent.ReturnType)
		}
		if len(v.Args) != len(intent.Params) {
			return fmt.Errorf("AgentIntentCallExpr: agent %q intent %q expects %d args, got %d", v.AgentName, v.IntentName, len(intent.Params), len(v.Args))
		}
		for i, arg := range v.Args {
			if arg == nil {
				return fmt.Errorf("AgentIntentCallExpr %q.%q arg %d is nil", v.AgentName, v.IntentName, i)
			}
			if err := verifyExprCtx(ctx, arg); err != nil {
				return fmt.Errorf("AgentIntentCallExpr %q.%q arg %d: %w", v.AgentName, v.IntentName, i, err)
			}
			if arg.Type() != intent.Params[i].Type {
				return fmt.Errorf("AgentIntentCallExpr %q.%q arg %d: expected %s, got %s", v.AgentName, v.IntentName, i, intent.Params[i].Type, arg.Type())
			}
		}
		return nil
	case *RecordLit:
		return verifyRecordLit(ctx, v)
	case *FieldAccess:
		return verifyFieldAccess(ctx, v)
	case *ListLit:
		return verifyListLit(ctx, v)
	case *IndexExpr:
		return verifyIndexExpr(ctx, v)
	case *LenExpr:
		return verifyLenExpr(ctx, v)
	case *StrLenExpr:
		if v.Receiver.Type() != TypeString {
			return fmt.Errorf("StrLenExpr: receiver must be TypeString, got %s", v.Receiver.Type())
		}
		return verifyExprCtx(ctx, v.Receiver)
	case *StrIndexExpr:
		if v.Receiver.Type() != TypeString {
			return fmt.Errorf("StrIndexExpr: receiver must be TypeString, got %s", v.Receiver.Type())
		}
		if v.Index.Type() != TypeInt {
			return fmt.Errorf("StrIndexExpr: index must be TypeInt, got %s", v.Index.Type())
		}
		if err := verifyExprCtx(ctx, v.Receiver); err != nil {
			return err
		}
		return verifyExprCtx(ctx, v.Index)
	case *StrContainsExpr:
		if v.Receiver.Type() != TypeString {
			return fmt.Errorf("StrContainsExpr: receiver must be TypeString, got %s", v.Receiver.Type())
		}
		if v.Sub.Type() != TypeString {
			return fmt.Errorf("StrContainsExpr: sub must be TypeString, got %s", v.Sub.Type())
		}
		if err := verifyExprCtx(ctx, v.Receiver); err != nil {
			return err
		}
		return verifyExprCtx(ctx, v.Sub)
	case *StrSubstringExpr:
		if v.Receiver.Type() != TypeString {
			return fmt.Errorf("StrSubstringExpr: receiver must be TypeString, got %s", v.Receiver.Type())
		}
		if v.Start.Type() != TypeInt {
			return fmt.Errorf("StrSubstringExpr: start must be TypeInt, got %s", v.Start.Type())
		}
		if v.End.Type() != TypeInt {
			return fmt.Errorf("StrSubstringExpr: end must be TypeInt, got %s", v.End.Type())
		}
		if err := verifyExprCtx(ctx, v.Receiver); err != nil {
			return err
		}
		if err := verifyExprCtx(ctx, v.Start); err != nil {
			return err
		}
		return verifyExprCtx(ctx, v.End)
	case *StrReverseExpr:
		if v.Receiver.Type() != TypeString {
			return fmt.Errorf("StrReverseExpr: receiver must be TypeString, got %s", v.Receiver.Type())
		}
		return verifyExprCtx(ctx, v.Receiver)
	// Phase 6.3: string case-conversion and split/join.
	case *StrUpperExpr:
		if v.Receiver.Type() != TypeString {
			return fmt.Errorf("StrUpperExpr: receiver must be TypeString, got %s", v.Receiver.Type())
		}
		return verifyExprCtx(ctx, v.Receiver)
	case *StrLowerExpr:
		if v.Receiver.Type() != TypeString {
			return fmt.Errorf("StrLowerExpr: receiver must be TypeString, got %s", v.Receiver.Type())
		}
		return verifyExprCtx(ctx, v.Receiver)
	case *StrSplitExpr:
		if v.Str.Type() != TypeString {
			return fmt.Errorf("StrSplitExpr: str must be TypeString, got %s", v.Str.Type())
		}
		if v.Sep.Type() != TypeString {
			return fmt.Errorf("StrSplitExpr: sep must be TypeString, got %s", v.Sep.Type())
		}
		if err := verifyExprCtx(ctx, v.Str); err != nil {
			return err
		}
		return verifyExprCtx(ctx, v.Sep)
	case *StrJoinExpr:
		if v.List.Type() != TypeList {
			return fmt.Errorf("StrJoinExpr: list must be TypeList, got %s", v.List.Type())
		}
		if v.Sep.Type() != TypeString {
			return fmt.Errorf("StrJoinExpr: sep must be TypeString, got %s", v.Sep.Type())
		}
		if err := verifyExprCtx(ctx, v.List); err != nil {
			return err
		}
		return verifyExprCtx(ctx, v.Sep)
	case *StrConvertExpr:
		if v.Operand == nil {
			return fmt.Errorf("StrConvertExpr: nil operand")
		}
		t := v.Operand.Type()
		if t != TypeInt && t != TypeFloat && t != TypeBool && t != TypeString {
			return fmt.Errorf("StrConvertExpr: operand must be int/float/bool/string, got %s", t)
		}
		return verifyExprCtx(ctx, v.Operand)
	case *NumCastExpr:
		if v.Operand == nil {
			return fmt.Errorf("NumCastExpr: nil operand")
		}
		if v.Operand.Type() != TypeFloat {
			return fmt.Errorf("NumCastExpr: operand must be TypeFloat, got %s", v.Operand.Type())
		}
		return verifyExprCtx(ctx, v.Operand)
	case *ListMinExpr:
		if v.Receiver == nil {
			return fmt.Errorf("ListMinExpr: nil receiver")
		}
		if v.Receiver.Type() != TypeList {
			return fmt.Errorf("ListMinExpr: receiver must be TypeList, got %s", v.Receiver.Type())
		}
		if !isScalarElemType(v.ElemType) {
			return fmt.Errorf("ListMinExpr: element type must be scalar, got %s", v.ElemType)
		}
		return verifyExprCtx(ctx, v.Receiver)
	case *ListMaxExpr:
		if v.Receiver == nil {
			return fmt.Errorf("ListMaxExpr: nil receiver")
		}
		if v.Receiver.Type() != TypeList {
			return fmt.Errorf("ListMaxExpr: receiver must be TypeList, got %s", v.Receiver.Type())
		}
		if !isScalarElemType(v.ElemType) {
			return fmt.Errorf("ListMaxExpr: element type must be scalar, got %s", v.ElemType)
		}
		return verifyExprCtx(ctx, v.Receiver)
	case *ListContainsExpr:
		if v.List == nil {
			return fmt.Errorf("ListContainsExpr: nil List")
		}
		if v.List.Type() != TypeList {
			return fmt.Errorf("ListContainsExpr: List must be TypeList, got %s", v.List.Type())
		}
		if v.Value == nil {
			return fmt.Errorf("ListContainsExpr: nil Value")
		}
		if !isScalarElemType(v.ElemType) {
			return fmt.Errorf("ListContainsExpr: ElemType must be scalar, got %s", v.ElemType)
		}
		if v.Value.Type() != v.ElemType {
			return fmt.Errorf("ListContainsExpr: Value type %s does not match ElemType %s", v.Value.Type(), v.ElemType)
		}
		if err := verifyExprCtx(ctx, v.List); err != nil {
			return fmt.Errorf("ListContainsExpr list: %w", err)
		}
		return verifyExprCtx(ctx, v.Value)
	case *ListSumExpr:
		if v.Receiver == nil {
			return fmt.Errorf("ListSumExpr: nil receiver")
		}
		if v.Receiver.Type() != TypeList {
			return fmt.Errorf("ListSumExpr: receiver must be TypeList, got %s", v.Receiver.Type())
		}
		if v.ElemType != TypeInt && v.ElemType != TypeFloat {
			return fmt.Errorf("ListSumExpr: ElemType must be int or float, got %s", v.ElemType)
		}
		return verifyExprCtx(ctx, v.Receiver)
	case *JsonDecodeExpr:
		if v.Input == nil {
			return fmt.Errorf("JsonDecodeExpr: nil Input")
		}
		if v.Input.Type() != TypeString {
			return fmt.Errorf("JsonDecodeExpr: input must be string, got %s", v.Input.Type())
		}
		return verifyExprCtx(ctx, v.Input)
	case *ListMapExpr:
		if v.List == nil {
			return fmt.Errorf("ListMapExpr: nil List")
		}
		if v.Fn == nil {
			return fmt.Errorf("ListMapExpr: nil Fn")
		}
		if err := verifyExprCtx(ctx, v.List); err != nil {
			return fmt.Errorf("ListMapExpr list: %w", err)
		}
		return verifyExprCtx(ctx, v.Fn)
	case *ListFilterExpr:
		if v.List == nil {
			return fmt.Errorf("ListFilterExpr: nil List")
		}
		if v.Fn == nil {
			return fmt.Errorf("ListFilterExpr: nil Fn")
		}
		if err := verifyExprCtx(ctx, v.List); err != nil {
			return fmt.Errorf("ListFilterExpr list: %w", err)
		}
		return verifyExprCtx(ctx, v.Fn)
	case *ListFoldlExpr:
		if v.List == nil {
			return fmt.Errorf("ListFoldlExpr: nil List")
		}
		if v.Fn == nil {
			return fmt.Errorf("ListFoldlExpr: nil Fn")
		}
		if v.Init == nil {
			return fmt.Errorf("ListFoldlExpr: nil Init")
		}
		if err := verifyExprCtx(ctx, v.List); err != nil {
			return fmt.Errorf("ListFoldlExpr list: %w", err)
		}
		if err := verifyExprCtx(ctx, v.Fn); err != nil {
			return fmt.Errorf("ListFoldlExpr fn: %w", err)
		}
		return verifyExprCtx(ctx, v.Init)
	case *MathCallExpr:
		if v.Arg == nil {
			return fmt.Errorf("MathCallExpr: nil Arg")
		}
		switch v.Func {
		case "abs_i64":
			if v.Arg.Type() != TypeInt {
				return fmt.Errorf("MathCallExpr abs_i64: arg must be int, got %s", v.Arg.Type())
			}
			if v.Result != TypeInt {
				return fmt.Errorf("MathCallExpr abs_i64: result must be int, got %s", v.Result)
			}
		case "abs_f64":
			if v.Arg.Type() != TypeFloat {
				return fmt.Errorf("MathCallExpr abs_f64: arg must be float, got %s", v.Arg.Type())
			}
			if v.Result != TypeFloat {
				return fmt.Errorf("MathCallExpr abs_f64: result must be float, got %s", v.Result)
			}
		case "floor", "ceil":
			if v.Arg.Type() != TypeFloat {
				return fmt.Errorf("MathCallExpr %s: arg must be float, got %s", v.Func, v.Arg.Type())
			}
			if v.Result != TypeFloat {
				return fmt.Errorf("MathCallExpr %s: result must be float, got %s", v.Func, v.Result)
			}
		default:
			return fmt.Errorf("MathCallExpr: unknown Func %q", v.Func)
		}
		return verifyExprCtx(ctx, v.Arg)
	case *AppendExpr:
		return verifyAppendExpr(ctx, v)
	case *ListSortAscExpr:
		if v.Receiver == nil {
			return fmt.Errorf("ListSortAscExpr: nil receiver")
		}
		if v.Receiver.Type() != TypeList {
			return fmt.Errorf("ListSortAscExpr: receiver must be TypeList, got %s", v.Receiver.Type())
		}
		return verifyExprCtx(ctx, v.Receiver)
	case *ListSliceExpr:
		if v.Receiver == nil {
			return fmt.Errorf("ListSliceExpr: nil receiver")
		}
		if v.Receiver.Type() != TypeList {
			return fmt.Errorf("ListSliceExpr: receiver must be TypeList, got %s", v.Receiver.Type())
		}
		if v.Start == nil || v.End == nil {
			return fmt.Errorf("ListSliceExpr: nil start or end")
		}
		if v.Start.Type() != TypeInt || v.End.Type() != TypeInt {
			return fmt.Errorf("ListSliceExpr: start/end must be TypeInt")
		}
		if err := verifyExprCtx(ctx, v.Receiver); err != nil {
			return fmt.Errorf("ListSliceExpr receiver: %w", err)
		}
		if err := verifyExprCtx(ctx, v.Start); err != nil {
			return fmt.Errorf("ListSliceExpr start: %w", err)
		}
		return verifyExprCtx(ctx, v.End)
	case *MapLit:
		return verifyMapLit(ctx, v)
	case *MapGetExpr:
		return verifyMapGetExpr(ctx, v)
	case *MapHasExpr:
		return verifyMapHasExpr(ctx, v)
	case *MapLenExpr:
		return verifyMapLenExpr(ctx, v)
	case *MapKeysExpr:
		return verifyMapKeysExpr(ctx, v)
	case *MapValuesExpr:
		return verifyMapValuesExpr(ctx, v)
	case *SetLiteralExpr:
		return verifySetLiteralExpr(ctx, v)
	case *SetAddExpr:
		return verifySetAddExpr(ctx, v)
	case *SetHasExpr:
		return verifySetHasExpr(ctx, v)
	case *SetLenExpr:
		return verifySetLenExpr(ctx, v)
	case *OMapLiteralExpr:
		return verifyOMapLiteralExpr(ctx, v)
	case *OMapGetExpr:
		return verifyOMapGetExpr(ctx, v)
	case *OMapSetExpr:
		return verifyOMapSetExpr(ctx, v)
	case *OMapHasExpr:
		return verifyOMapHasExpr(ctx, v)
	case *OMapLenExpr:
		return verifyOMapLenExpr(ctx, v)
	case *SetToListExpr:
		if v.Receiver == nil {
			return errors.New("SetToListExpr: nil Receiver")
		}
		if err := verifyExprCtx(ctx, v.Receiver); err != nil {
			return fmt.Errorf("SetToListExpr receiver: %w", err)
		}
		if v.Receiver.Type() != TypeSet {
			return fmt.Errorf("SetToListExpr: receiver must be TypeSet, got %s", v.Receiver.Type())
		}
		return nil
	case *VariantLit:
		if _, ok := ctx.unions[v.UnionName]; !ok {
			return fmt.Errorf("VariantLit: union %q not declared", v.UnionName)
		}
		for i, f := range v.Fields {
			if err := verifyExprCtx(ctx, f.Value); err != nil {
				return fmt.Errorf("VariantLit %q.%s field %d: %w", v.VariantName, f.Name, i, err)
			}
		}
		return nil
	case *UnionVarRef:
		b, ok := ctx.scope.lookup(v.Name)
		if !ok {
			return fmt.Errorf("unresolved variable %q", v.Name)
		}
		if b.t != TypeUnion {
			return fmt.Errorf("variable %q is not union-typed (got %s)", v.Name, b.t)
		}
		if b.union != v.UnionName {
			return fmt.Errorf("variable %q has union %q in scope, ref says %q", v.Name, b.union, v.UnionName)
		}
		return nil
	case *VariantFieldAccess:
		if err := verifyExprCtx(ctx, v.Receiver); err != nil {
			return fmt.Errorf("VariantFieldAccess receiver: %w", err)
		}
		if v.Receiver.Type() != TypeUnion {
			return fmt.Errorf("VariantFieldAccess: receiver must be TypeUnion, got %s", v.Receiver.Type())
		}
		return nil
	case *FunLit:
		if v.Sig == nil {
			return fmt.Errorf("FunLit %q has nil Sig", v.FuncName)
		}
		if v.FuncName == "" {
			return fmt.Errorf("FunLit has empty FuncName")
		}
		return nil
	case *FunCallExpr:
		if v.Callee == nil {
			return fmt.Errorf("FunCallExpr has nil Callee")
		}
		if err := verifyExprCtx(ctx, v.Callee); err != nil {
			return fmt.Errorf("FunCallExpr callee: %w", err)
		}
		if v.Callee.Type() != TypeFun {
			return fmt.Errorf("FunCallExpr: callee must be TypeFun, got %s", v.Callee.Type())
		}
		for i, arg := range v.Args {
			if err := verifyExprCtx(ctx, arg); err != nil {
				return fmt.Errorf("FunCallExpr arg %d: %w", i, err)
			}
		}
		return nil
	case *ReadFileExpr:
		if v.Path == nil {
			return errors.New("ReadFileExpr: nil Path")
		}
		if v.Path.Type() != TypeString {
			return fmt.Errorf("ReadFileExpr: Path must be TypeString, got %s", v.Path.Type())
		}
		return verifyExprCtx(ctx, v.Path)
	case *LinesExpr:
		if v.Path == nil {
			return errors.New("LinesExpr: nil Path")
		}
		if v.Path.Type() != TypeString {
			return fmt.Errorf("LinesExpr: Path must be TypeString, got %s", v.Path.Type())
		}
		return verifyExprCtx(ctx, v.Path)
	case *HttpGetExpr:
		if v.URL == nil {
			return errors.New("HttpGetExpr: nil URL")
		}
		if v.URL.Type() != TypeString {
			return fmt.Errorf("HttpGetExpr: URL must be TypeString, got %s", v.URL.Type())
		}
		return verifyExprCtx(ctx, v.URL)
	case *LoadCSVExpr:
		if v.Path == nil {
			return errors.New("LoadCSVExpr: nil Path")
		}
		if v.Path.Type() != TypeString {
			return fmt.Errorf("LoadCSVExpr: Path must be TypeString, got %s", v.Path.Type())
		}
		return verifyExprCtx(ctx, v.Path)
	case *LLMGenerateExpr:
		// Phase 14.0: LLM text generation.
		if v.Provider == "" {
			return errors.New("LLMGenerateExpr: empty Provider")
		}
		if v.Model == nil {
			return errors.New("LLMGenerateExpr: nil Model")
		}
		if v.Model.Type() != TypeString {
			return fmt.Errorf("LLMGenerateExpr: Model must be TypeString, got %s", v.Model.Type())
		}
		if v.Prompt == nil {
			return errors.New("LLMGenerateExpr: nil Prompt")
		}
		if v.Prompt.Type() != TypeString {
			return fmt.Errorf("LLMGenerateExpr: Prompt must be TypeString, got %s", v.Prompt.Type())
		}
		if err := verifyExprCtx(ctx, v.Model); err != nil {
			return fmt.Errorf("LLMGenerateExpr model: %w", err)
		}
		return verifyExprCtx(ctx, v.Prompt)
	case *RawCExpr:
		// Phase 15.0: raw C expression; the lowerer is responsible for correctness.
		if v.Code == "" {
			return errors.New("RawCExpr: empty Code")
		}
		return nil
	case *DatalogQueryExpr:
		if v.QueryName == "" {
			return errors.New("DatalogQueryExpr: empty QueryName")
		}
		if v.Prog == nil {
			return errors.New("DatalogQueryExpr: nil Prog")
		}
		return nil
	case *CallExpr:
		// Phase 11.2: await_all pseudo-builtin lowered to mochi_async:await_all/1.
		if v.Func == "__await_all__" {
			if len(v.Args) != 1 {
				return fmt.Errorf("__await_all__: expected 1 arg, got %d", len(v.Args))
			}
			if err := verifyExprCtx(ctx, v.Args[0]); err != nil {
				return fmt.Errorf("__await_all__ arg: %w", err)
			}
			return nil
		}
		// Phase 10.0: extern C functions can appear in expression position.
		if ef, isExtern := ctx.externFns[v.Func]; isExtern {
			if ef.ReturnType == TypeUnit {
				return fmt.Errorf("extern callee %q returns unit; use a statement form, not an expression", v.Func)
			}
			if v.Result != ef.ReturnType {
				return fmt.Errorf("extern call %q result %s does not match declared return %s",
					v.Func, v.Result, ef.ReturnType)
			}
			if len(ef.Params) != len(v.Args) {
				return fmt.Errorf("extern call %q expects %d args, got %d", v.Func, len(ef.Params), len(v.Args))
			}
			for i, a := range v.Args {
				if a == nil {
					return fmt.Errorf("extern call %q arg %d is nil", v.Func, i)
				}
				if err := verifyExprCtx(ctx, a); err != nil {
					return fmt.Errorf("extern call %q arg %d: %w", v.Func, i, err)
				}
				if a.Type() != ef.Params[i].Type {
					return fmt.Errorf("extern call %q arg %d: expected %s, got %s",
						v.Func, i, ef.Params[i].Type, a.Type())
				}
			}
			return nil
		}
		fn, ok := ctx.fns[v.Func]
		if !ok {
			return fmt.Errorf("unresolved callee %q in expression position", v.Func)
		}
		if fn.ReturnType == TypeUnit {
			return fmt.Errorf("callee %q returns unit; use a statement form, not an expression", v.Func)
		}
		if v.Result != fn.ReturnType {
			return fmt.Errorf("call %q result %s does not match callee return %s",
				v.Func, v.Result, fn.ReturnType)
		}
		if fn.ReturnType == TypeRecord && v.ResultRecordName != fn.ReturnRecordName {
			return fmt.Errorf("call %q result record %q does not match callee return record %q",
				v.Func, v.ResultRecordName, fn.ReturnRecordName)
		}
		if fn.ReturnType == TypeList && v.ResultElemType != fn.ReturnElemType {
			return fmt.Errorf("call %q result list<%s> does not match callee return list<%s>",
				v.Func, v.ResultElemType, fn.ReturnElemType)
		}
		if fn.ReturnType == TypeList && fn.ReturnElemType == TypeRecord && v.ResultElemRecordName != fn.ReturnElemRecordName {
			return fmt.Errorf("call %q result list<%s> does not match callee return list<%s>",
				v.Func, v.ResultElemRecordName, fn.ReturnElemRecordName)
		}
		if fn.ReturnType == TypeList && fn.ReturnElemType == TypeList && v.ResultInnerElemType != fn.ReturnInnerElemType {
			return fmt.Errorf("call %q result list<list<%s>> does not match callee return list<list<%s>>",
				v.Func, v.ResultInnerElemType, fn.ReturnInnerElemType)
		}
		if fn.ReturnType == TypeList && fn.ReturnElemType == TypeMap {
			if v.ResultMapElemKeyType != fn.ReturnMapElemKeyType {
				return fmt.Errorf("call %q result list<map<%s,_>> does not match callee return list<map<%s,_>>",
					v.Func, v.ResultMapElemKeyType, fn.ReturnMapElemKeyType)
			}
			if v.ResultMapElemValueType != fn.ReturnMapElemValueType {
				return fmt.Errorf("call %q result list<map<_,%s>> does not match callee return list<map<_,%s>>",
					v.Func, v.ResultMapElemValueType, fn.ReturnMapElemValueType)
			}
		}
		if fn.ReturnType == TypeMap {
			if v.ResultKeyType != fn.ReturnKeyType {
				return fmt.Errorf("call %q result map<%s,_> does not match callee return map<%s,_>",
					v.Func, v.ResultKeyType, fn.ReturnKeyType)
			}
			if v.ResultValueType != fn.ReturnValueType {
				return fmt.Errorf("call %q result map<_,%s> does not match callee return map<_,%s>",
					v.Func, v.ResultValueType, fn.ReturnValueType)
			}
			if fn.ReturnValueType == TypeList && v.ResultListValueElemType != fn.ReturnListValueElemType {
				return fmt.Errorf("call %q result map<_,list<%s>> does not match callee return map<_,list<%s>>",
					v.Func, v.ResultListValueElemType, fn.ReturnListValueElemType)
			}
		}
		if len(fn.Params) != len(v.Args) {
			return fmt.Errorf("call %q expects %d args, got %d", v.Func, len(fn.Params), len(v.Args))
		}
		for i, a := range v.Args {
			if a == nil {
				return fmt.Errorf("call %q arg %d is nil", v.Func, i)
			}
			if err := verifyExprCtx(ctx, a); err != nil {
				return fmt.Errorf("call %q arg %d: %w", v.Func, i, err)
			}
			if a.Type() != fn.Params[i].Type {
				return fmt.Errorf("call %q arg %d: expected %s, got %s",
					v.Func, i, fn.Params[i].Type, a.Type())
			}
			if fn.Params[i].Type == TypeRecord {
				if argRec := exprRecordName(a); argRec != fn.Params[i].RecordName {
					return fmt.Errorf("call %q arg %d: expected record %q, got %q",
						v.Func, i, fn.Params[i].RecordName, argRec)
				}
			}
			if fn.Params[i].Type == TypeList {
				if argElem := exprElemType(a); argElem != fn.Params[i].ElemType {
					return fmt.Errorf("call %q arg %d: expected list<%s>, got list<%s>",
						v.Func, i, fn.Params[i].ElemType, argElem)
				}
				if fn.Params[i].ElemType == TypeRecord {
					if argElemRec := exprElemRecordName(a); argElemRec != fn.Params[i].ElemRecordName {
						return fmt.Errorf("call %q arg %d: expected list<%s>, got list<%s>",
							v.Func, i, fn.Params[i].ElemRecordName, argElemRec)
					}
				}
				if fn.Params[i].ElemType == TypeList {
					if argInner := exprInnerElemType(a); argInner != fn.Params[i].InnerElemType {
						return fmt.Errorf("call %q arg %d: expected list<list<%s>>, got list<list<%s>>",
							v.Func, i, fn.Params[i].InnerElemType, argInner)
					}
				}
				if fn.Params[i].ElemType == TypeMap {
					if argMK := exprMapElemKeyType(a); argMK != fn.Params[i].MapElemKeyType {
						return fmt.Errorf("call %q arg %d: expected list<map<%s,_>>, got list<map<%s,_>>",
							v.Func, i, fn.Params[i].MapElemKeyType, argMK)
					}
					if argMV := exprMapElemValueType(a); argMV != fn.Params[i].MapElemValueType {
						return fmt.Errorf("call %q arg %d: expected list<map<_,%s>>, got list<map<_,%s>>",
							v.Func, i, fn.Params[i].MapElemValueType, argMV)
					}
				}
			}
			if fn.Params[i].Type == TypeMap {
				if argKey := exprKeyType(a); argKey != fn.Params[i].KeyType {
					return fmt.Errorf("call %q arg %d: expected map<%s,_>, got map<%s,_>",
						v.Func, i, fn.Params[i].KeyType, argKey)
				}
				if argVal := exprValueType(a); argVal != fn.Params[i].ValueType {
					return fmt.Errorf("call %q arg %d: expected map<_,%s>, got map<_,%s>",
						v.Func, i, fn.Params[i].ValueType, argVal)
				}
				if fn.Params[i].ValueType == TypeList {
					if argLV := exprListValueElemType(a); argLV != fn.Params[i].ListValueElemType {
						return fmt.Errorf("call %q arg %d: expected map<_,list<%s>>, got map<_,list<%s>>",
							v.Func, i, fn.Params[i].ListValueElemType, argLV)
					}
				}
			}
		}
		return nil
	case *BinaryExpr:
		if v.Left == nil || v.Right == nil {
			return fmt.Errorf("binary %v has nil operand", v.Op)
		}
		if err := verifyExprCtx(ctx, v.Left); err != nil {
			return err
		}
		if err := verifyExprCtx(ctx, v.Right); err != nil {
			return err
		}
		lhs, rhs, res, ok := binOpSignature(v.Op)
		if !ok {
			return fmt.Errorf("unhandled BinOp %d", v.Op)
		}
		if v.Left.Type() != lhs || v.Right.Type() != rhs {
			return fmt.Errorf("binary %v expects %s op %s, got %s op %s",
				v.Op, lhs, rhs, v.Left.Type(), v.Right.Type())
		}
		if v.Result != res {
			return fmt.Errorf("binary %v result %s does not match expected %s",
				v.Op, v.Result, res)
		}
		if v.Op == BinEqRec || v.Op == BinNeRec {
			lrec := exprRecordName(v.Left)
			rrec := exprRecordName(v.Right)
			if lrec == "" || rrec == "" {
				return fmt.Errorf("binary %v: record operands missing RecordName (left=%q right=%q)", v.Op, lrec, rrec)
			}
			if lrec != rrec {
				return fmt.Errorf("binary %v: cannot compare record %q with record %q", v.Op, lrec, rrec)
			}
			if _, ok := ctx.records[lrec]; !ok {
				return fmt.Errorf("binary %v: record %q is not declared", v.Op, lrec)
			}
		}
		return nil
	case *UnaryExpr:
		if v.Operand == nil {
			return fmt.Errorf("unary %v has nil operand", v.Op)
		}
		if err := verifyExprCtx(ctx, v.Operand); err != nil {
			return err
		}
		operand, res, ok := unOpSignature(v.Op)
		if !ok {
			return fmt.Errorf("unhandled UnOp %d", v.Op)
		}
		if v.Operand.Type() != operand {
			return fmt.Errorf("unary %v expects %s, got %s", v.Op, operand, v.Operand.Type())
		}
		if v.Result != res {
			return fmt.Errorf("unary %v result %s does not match expected %s", v.Op, v.Result, res)
		}
		return nil
	case *ChanMakeExpr:
		if v.Cap == nil {
			return errors.New("ChanMakeExpr: nil Cap")
		}
		if v.Cap.Type() != TypeInt {
			return fmt.Errorf("ChanMakeExpr: Cap must be TypeInt, got %s", v.Cap.Type())
		}
		if v.ElemType == TypeInvalid {
			return errors.New("ChanMakeExpr: ElemType is TypeInvalid")
		}
		return verifyExprCtx(ctx, v.Cap)
	case *ChanRecvExpr:
		if v.Chan == nil {
			return errors.New("ChanRecvExpr: nil Chan")
		}
		if v.Chan.Type() != TypeChan {
			return fmt.Errorf("ChanRecvExpr: Chan must be TypeChan, got %s", v.Chan.Type())
		}
		if v.ElemType == TypeInvalid {
			return errors.New("ChanRecvExpr: ElemType is TypeInvalid")
		}
		return verifyExprCtx(ctx, v.Chan)
	case *StreamMakeExpr:
		if v.Cap == nil {
			return errors.New("StreamMakeExpr: nil Cap")
		}
		if v.Cap.Type() != TypeInt {
			return fmt.Errorf("StreamMakeExpr: Cap must be TypeInt, got %s", v.Cap.Type())
		}
		if v.ElemType == TypeInvalid {
			return errors.New("StreamMakeExpr: ElemType is TypeInvalid")
		}
		return verifyExprCtx(ctx, v.Cap)
	case *SubMakeExpr:
		if v.Stream == nil {
			return errors.New("SubMakeExpr: nil Stream")
		}
		if v.Stream.Type() != TypeStream {
			return fmt.Errorf("SubMakeExpr: Stream must be TypeStream, got %s", v.Stream.Type())
		}
		if v.ElemType == TypeInvalid {
			return errors.New("SubMakeExpr: ElemType is TypeInvalid")
		}
		return verifyExprCtx(ctx, v.Stream)
	case *SubMakeLimitExpr:
		// Phase 10.2: subscribe_limit(stream, N)
		if v.Stream == nil {
			return errors.New("SubMakeLimitExpr: nil Stream")
		}
		if v.Stream.Type() != TypeStream {
			return fmt.Errorf("SubMakeLimitExpr: Stream must be TypeStream, got %s", v.Stream.Type())
		}
		if v.Limit == nil {
			return errors.New("SubMakeLimitExpr: nil Limit")
		}
		if v.Limit.Type() != TypeInt {
			return fmt.Errorf("SubMakeLimitExpr: Limit must be TypeInt, got %s", v.Limit.Type())
		}
		if v.ElemType == TypeInvalid {
			return errors.New("SubMakeLimitExpr: ElemType is TypeInvalid")
		}
		if err := verifyExprCtx(ctx, v.Stream); err != nil {
			return err
		}
		return verifyExprCtx(ctx, v.Limit)
	case *SubRecvExpr:
		if v.Sub == nil {
			return errors.New("SubRecvExpr: nil Sub")
		}
		if v.Sub.Type() != TypeSub {
			return fmt.Errorf("SubRecvExpr: Sub must be TypeSub, got %s", v.Sub.Type())
		}
		if v.ElemType == TypeInvalid {
			return errors.New("SubRecvExpr: ElemType is TypeInvalid")
		}
		return verifyExprCtx(ctx, v.Sub)
	// Phase 11.0: async expr → TypeFuture
	case *AsyncExpr:
		if v.Body == nil {
			return errors.New("AsyncExpr: nil Body")
		}
		if v.ElemType == TypeInvalid {
			return errors.New("AsyncExpr: ElemType is TypeInvalid")
		}
		return verifyExprCtx(ctx, v.Body)
	// Phase 11.1: await fut → ElemType
	case *AwaitExpr:
		if v.Future == nil {
			return errors.New("AwaitExpr: nil Future")
		}
		if v.Future.Type() != TypeFuture {
			return fmt.Errorf("AwaitExpr: Future must be TypeFuture, got %s", v.Future.Type())
		}
		if v.ElemType == TypeInvalid {
			return errors.New("AwaitExpr: ElemType is TypeInvalid")
		}
		return verifyExprCtx(ctx, v.Future)
	default:
		return fmt.Errorf("unhandled Expr %T", e)
	}
}

// binOpSignature reports (left, right, result, ok) for a BinOp.
// Returning the triple from one place keeps the verifier and
// the emit pass in lockstep on operator typing.
func binOpSignature(op BinOp) (Type, Type, Type, bool) {
	switch op {
	case BinAddI64, BinSubI64, BinMulI64, BinDivI64, BinModI64:
		return TypeInt, TypeInt, TypeInt, true
	case BinAddF64, BinSubF64, BinMulF64, BinDivF64:
		return TypeFloat, TypeFloat, TypeFloat, true
	case BinEqI64, BinNeI64, BinLtI64, BinLeI64, BinGtI64, BinGeI64:
		return TypeInt, TypeInt, TypeBool, true
	case BinEqF64, BinNeF64, BinLtF64, BinLeF64, BinGtF64, BinGeF64:
		return TypeFloat, TypeFloat, TypeBool, true
	case BinEqBool, BinNeBool, BinAndBool, BinOrBool:
		return TypeBool, TypeBool, TypeBool, true
	case BinEqStr, BinNeStr:
		return TypeString, TypeString, TypeBool, true
	case BinStrCat:
		return TypeString, TypeString, TypeString, true
	case BinEqRec, BinNeRec:
		return TypeRecord, TypeRecord, TypeBool, true
	case BinEqList, BinNeList:
		return TypeList, TypeList, TypeBool, true
	case BinEqMap, BinNeMap:
		return TypeMap, TypeMap, TypeBool, true
	}
	return TypeInvalid, TypeInvalid, TypeInvalid, false
}

// verifyRecordLit checks a record-literal expression:
//   - the named record exists in the program,
//   - every field of the record is present exactly once in Fields,
//   - the per-field expression's Type and (for record fields) RecordName
//     match the record's declaration.
//
// Field order in the literal must match the record's declaration
// order so the emit pass can render the C99 designated init in a
// stable order. The lowerer is responsible for reordering.
func verifyRecordLit(ctx *verifyCtx, r *RecordLit) error {
	if r.TypeName == "" {
		return errors.New("record literal with empty TypeName")
	}
	decl, ok := ctx.records[r.TypeName]
	if !ok {
		return fmt.Errorf("record literal %q: record not declared", r.TypeName)
	}
	if len(r.Fields) != len(decl.Fields) {
		return fmt.Errorf("record literal %q: expected %d fields, got %d", r.TypeName, len(decl.Fields), len(r.Fields))
	}
	for i, f := range r.Fields {
		want := decl.Fields[i]
		if f.Name != want.Name {
			return fmt.Errorf("record literal %q field %d: expected %q, got %q", r.TypeName, i, want.Name, f.Name)
		}
		if f.Value == nil {
			return fmt.Errorf("record literal %q field %q: nil Value", r.TypeName, f.Name)
		}
		if err := verifyExprCtx(ctx, f.Value); err != nil {
			return fmt.Errorf("record literal %q field %q: %w", r.TypeName, f.Name, err)
		}
		if f.Value.Type() != want.Type {
			return fmt.Errorf("record literal %q field %q: declared %s, value produces %s",
				r.TypeName, f.Name, want.Type, f.Value.Type())
		}
	}
	return nil
}

// verifyFieldAccess checks a record-field read:
//   - the receiver is record-typed and its RecordName resolves to a
//     declared record,
//   - the named field exists on that record,
//   - the recorded Result type matches the field's declared type.
func verifyFieldAccess(ctx *verifyCtx, f *FieldAccess) error {
	if f.Receiver == nil {
		return errors.New("field access with nil Receiver")
	}
	if err := verifyExprCtx(ctx, f.Receiver); err != nil {
		return fmt.Errorf("field access receiver: %w", err)
	}
	if f.Receiver.Type() != TypeRecord {
		return fmt.Errorf("field access %q: receiver is %s, expected record", f.FieldName, f.Receiver.Type())
	}
	recv := exprRecordName(f.Receiver)
	if recv != f.RecordName {
		return fmt.Errorf("field access %q: receiver record %q does not match annotated %q", f.FieldName, recv, f.RecordName)
	}
	decl, ok := ctx.records[f.RecordName]
	if !ok {
		return fmt.Errorf("field access %q: record %q is not declared", f.FieldName, f.RecordName)
	}
	for _, df := range decl.Fields {
		if df.Name == f.FieldName {
			if f.Result != df.Type {
				return fmt.Errorf("field access %s.%s: declared %s, access result %s",
					f.RecordName, f.FieldName, df.Type, f.Result)
			}
			if df.Type == TypeRecord && f.ResultRecordName != df.RecordName {
				return fmt.Errorf("field access %s.%s: declared record %q, access result record %q",
					f.RecordName, f.FieldName, df.RecordName, f.ResultRecordName)
			}
			return nil
		}
	}
	return fmt.Errorf("field access %q: record %q has no field %q", f.FieldName, f.RecordName, f.FieldName)
}

// verifyListLit checks a list-literal expression. Phase 3.4a accepts
// the four scalar primitive element types plus TypeRecord; every
// element's Type must equal ElemType, and for record elements every
// element's record name must match the stamped ElemRecordName.
func verifyListLit(ctx *verifyCtx, v *ListLit) error {
	if !isListElemType(v.ElemType) {
		return fmt.Errorf("list literal: ElemType %s not supported (Phase 3.4a supports scalar or record element types)", v.ElemType)
	}
	if v.ElemType == TypeRecord {
		if v.ElemRecordName == "" {
			return errors.New("list literal of records missing ElemRecordName")
		}
		if _, ok := ctx.records[v.ElemRecordName]; !ok {
			return fmt.Errorf("list literal: element record %q is not declared", v.ElemRecordName)
		}
	} else if v.ElemRecordName != "" {
		return fmt.Errorf("list literal: ElemRecordName set on list<%s> (only valid when ElemType==record)", v.ElemType)
	}
	if v.ElemType == TypeList {
		if !isScalarElemType(v.InnerElemType) {
			return fmt.Errorf("list literal: list<list<T>> requires scalar inner, got InnerElemType %s", v.InnerElemType)
		}
	} else if v.InnerElemType != TypeInvalid {
		return fmt.Errorf("list literal: InnerElemType set on list<%s> (only valid when ElemType==list)", v.ElemType)
	}
	if v.ElemType == TypeMap {
		if !isScalarKeyType(v.MapElemKeyType) {
			return fmt.Errorf("list literal: list<map<K,V>> requires int or string key, got MapElemKeyType %s", v.MapElemKeyType)
		}
		if !isScalarValueType(v.MapElemValueType) {
			return fmt.Errorf("list literal: list<map<K,V>> requires scalar value, got MapElemValueType %s", v.MapElemValueType)
		}
	} else if v.MapElemKeyType != TypeInvalid || v.MapElemValueType != TypeInvalid {
		return fmt.Errorf("list literal: MapElemKeyType/MapElemValueType set on list<%s> (only valid when ElemType==map)", v.ElemType)
	}
	for i, e := range v.Elems {
		if e == nil {
			return fmt.Errorf("list literal element %d is nil", i)
		}
		if err := verifyExprCtx(ctx, e); err != nil {
			return fmt.Errorf("list literal element %d: %w", i, err)
		}
		if e.Type() != v.ElemType {
			return fmt.Errorf("list literal element %d: declared %s, got %s", i, v.ElemType, e.Type())
		}
		if v.ElemType == TypeRecord {
			if rec := exprRecordName(e); rec != v.ElemRecordName {
				return fmt.Errorf("list literal element %d: declared record %q, got %q", i, v.ElemRecordName, rec)
			}
		}
		if v.ElemType == TypeList {
			if inner := exprElemType(e); inner != v.InnerElemType {
				return fmt.Errorf("list literal element %d: declared list<%s>, got list<%s>", i, v.InnerElemType, inner)
			}
		}
		if v.ElemType == TypeMap {
			if k := exprKeyType(e); k != v.MapElemKeyType {
				return fmt.Errorf("list literal element %d: declared map<%s,_>, got map<%s,_>", i, v.MapElemKeyType, k)
			}
			if val := exprValueType(e); val != v.MapElemValueType {
				return fmt.Errorf("list literal element %d: declared map<_,%s>, got map<_,%s>", i, v.MapElemValueType, val)
			}
		}
	}
	return nil
}

// verifyIndexExpr checks `xs[i]`. Receiver must be TypeList with
// the same ElemType stamped on the IndexExpr; Index must be TypeInt.
// Phase 3.4a widens to record element types (the result of the
// indexing is a record value identified by ElemRecordName).
func verifyIndexExpr(ctx *verifyCtx, v *IndexExpr) error {
	if v.Receiver == nil || v.Index == nil {
		return errors.New("index expression with nil Receiver or Index")
	}
	if err := verifyExprCtx(ctx, v.Receiver); err != nil {
		return fmt.Errorf("index receiver: %w", err)
	}
	if v.Receiver.Type() != TypeList {
		return fmt.Errorf("index receiver must be list, got %s", v.Receiver.Type())
	}
	if re := exprElemType(v.Receiver); re != v.ElemType {
		return fmt.Errorf("index: receiver is list<%s>, ElemType stamped is %s", re, v.ElemType)
	}
	if !isListElemType(v.ElemType) {
		return fmt.Errorf("index: ElemType %s not supported (Phase 3.4a supports scalar or record element types)", v.ElemType)
	}
	if v.ElemType == TypeRecord {
		if v.ElemRecordName == "" {
			return errors.New("index over list<record> missing ElemRecordName")
		}
		if rer := exprElemRecordName(v.Receiver); rer != v.ElemRecordName {
			return fmt.Errorf("index: receiver is list<%s>, ElemRecordName stamped is %s", rer, v.ElemRecordName)
		}
	} else if v.ElemRecordName != "" {
		return fmt.Errorf("index: ElemRecordName set on list<%s> (only valid when ElemType==record)", v.ElemType)
	}
	if v.ElemType == TypeList {
		if !isScalarElemType(v.InnerElemType) {
			return fmt.Errorf("index over list<list<T>> requires scalar inner, got InnerElemType %s", v.InnerElemType)
		}
		if ri := exprInnerElemType(v.Receiver); ri != v.InnerElemType {
			return fmt.Errorf("index: receiver is list<list<%s>>, InnerElemType stamped is %s", ri, v.InnerElemType)
		}
	} else if v.InnerElemType != TypeInvalid {
		return fmt.Errorf("index: InnerElemType set on list<%s> (only valid when ElemType==list)", v.ElemType)
	}
	if v.ElemType == TypeMap {
		if !isScalarKeyType(v.MapElemKeyType) {
			return fmt.Errorf("index over list<map<K,V>> requires int or string key, got MapElemKeyType %s", v.MapElemKeyType)
		}
		if !isScalarValueType(v.MapElemValueType) {
			return fmt.Errorf("index over list<map<K,V>> requires scalar value, got MapElemValueType %s", v.MapElemValueType)
		}
	} else if v.MapElemKeyType != TypeInvalid || v.MapElemValueType != TypeInvalid {
		return fmt.Errorf("index: MapElemKeyType/MapElemValueType set on list<%s> (only valid when ElemType==map)", v.ElemType)
	}
	if err := verifyExprCtx(ctx, v.Index); err != nil {
		return fmt.Errorf("index value: %w", err)
	}
	if v.Index.Type() != TypeInt {
		return fmt.Errorf("index must be int, got %s", v.Index.Type())
	}
	return nil
}

// verifyLenExpr checks `len(xs)` for a list-typed receiver. Phase
// 3.4a widens to record element types so `len(list<R>)` resolves to
// the per-record helper at emit time.
func verifyLenExpr(ctx *verifyCtx, v *LenExpr) error {
	if v.Receiver == nil {
		return errors.New("len() with nil Receiver")
	}
	if err := verifyExprCtx(ctx, v.Receiver); err != nil {
		return fmt.Errorf("len receiver: %w", err)
	}
	if v.Receiver.Type() != TypeList {
		return fmt.Errorf("len() argument must be list, got %s", v.Receiver.Type())
	}
	if re := exprElemType(v.Receiver); re != v.ElemType {
		return fmt.Errorf("len: receiver is list<%s>, ElemType stamped is %s", re, v.ElemType)
	}
	if !isListElemType(v.ElemType) {
		return fmt.Errorf("len: ElemType %s not supported (Phase 3.4a supports scalar or record element types)", v.ElemType)
	}
	if v.ElemType == TypeRecord {
		if v.ElemRecordName == "" {
			return errors.New("len() over list<record> missing ElemRecordName")
		}
		if rer := exprElemRecordName(v.Receiver); rer != v.ElemRecordName {
			return fmt.Errorf("len: receiver is list<%s>, ElemRecordName stamped is %s", rer, v.ElemRecordName)
		}
	} else if v.ElemRecordName != "" {
		return fmt.Errorf("len: ElemRecordName set on list<%s> (only valid when ElemType==record)", v.ElemType)
	}
	if v.ElemType == TypeList {
		if !isScalarElemType(v.InnerElemType) {
			return fmt.Errorf("len over list<list<T>> requires scalar inner, got InnerElemType %s", v.InnerElemType)
		}
		if ri := exprInnerElemType(v.Receiver); ri != v.InnerElemType {
			return fmt.Errorf("len: receiver is list<list<%s>>, InnerElemType stamped is %s", ri, v.InnerElemType)
		}
	} else if v.InnerElemType != TypeInvalid {
		return fmt.Errorf("len: InnerElemType set on list<%s> (only valid when ElemType==list)", v.ElemType)
	}
	if v.ElemType == TypeMap {
		if !isScalarKeyType(v.MapElemKeyType) {
			return fmt.Errorf("len over list<map<K,V>> requires int or string key, got MapElemKeyType %s", v.MapElemKeyType)
		}
		if !isScalarValueType(v.MapElemValueType) {
			return fmt.Errorf("len over list<map<K,V>> requires scalar value, got MapElemValueType %s", v.MapElemValueType)
		}
	} else if v.MapElemKeyType != TypeInvalid || v.MapElemValueType != TypeInvalid {
		return fmt.Errorf("len: MapElemKeyType/MapElemValueType set on list<%s> (only valid when ElemType==map)", v.ElemType)
	}
	return nil
}

// verifyAppendExpr checks `append(xs, v)`. Receiver must be a list,
// Value's Type must match ElemType, and the produced list shares
// ElemType (the lowerer stamps this; the verifier double-checks).
// Phase 3.4a widens to record element types.
func verifyAppendExpr(ctx *verifyCtx, v *AppendExpr) error {
	if v.Receiver == nil || v.Value == nil {
		return errors.New("append with nil Receiver or Value")
	}
	if err := verifyExprCtx(ctx, v.Receiver); err != nil {
		return fmt.Errorf("append receiver: %w", err)
	}
	if v.Receiver.Type() != TypeList {
		return fmt.Errorf("append receiver must be list, got %s", v.Receiver.Type())
	}
	if re := exprElemType(v.Receiver); re != v.ElemType {
		return fmt.Errorf("append: receiver is list<%s>, ElemType stamped is %s", re, v.ElemType)
	}
	if !isListElemType(v.ElemType) {
		return fmt.Errorf("append: ElemType %s not supported (Phase 3.4a supports scalar or record element types)", v.ElemType)
	}
	if v.ElemType == TypeRecord {
		if v.ElemRecordName == "" {
			return errors.New("append over list<record> missing ElemRecordName")
		}
		if rer := exprElemRecordName(v.Receiver); rer != v.ElemRecordName {
			return fmt.Errorf("append: receiver is list<%s>, ElemRecordName stamped is %s", rer, v.ElemRecordName)
		}
	} else if v.ElemRecordName != "" {
		return fmt.Errorf("append: ElemRecordName set on list<%s> (only valid when ElemType==record)", v.ElemType)
	}
	if err := verifyExprCtx(ctx, v.Value); err != nil {
		return fmt.Errorf("append value: %w", err)
	}
	if v.Value.Type() != v.ElemType {
		return fmt.Errorf("append value: expected %s, got %s", v.ElemType, v.Value.Type())
	}
	if v.ElemType == TypeRecord {
		if vr := exprRecordName(v.Value); vr != v.ElemRecordName {
			return fmt.Errorf("append value: expected record %q, got %q", v.ElemRecordName, vr)
		}
	}
	if v.ElemType == TypeList {
		if !isScalarElemType(v.InnerElemType) {
			return fmt.Errorf("append over list<list<T>> requires scalar inner, got InnerElemType %s", v.InnerElemType)
		}
		if ri := exprInnerElemType(v.Receiver); ri != v.InnerElemType {
			return fmt.Errorf("append: receiver is list<list<%s>>, InnerElemType stamped is %s", ri, v.InnerElemType)
		}
		if vi := exprElemType(v.Value); vi != v.InnerElemType {
			return fmt.Errorf("append value: expected list<%s>, got list<%s>", v.InnerElemType, vi)
		}
	} else if v.InnerElemType != TypeInvalid {
		return fmt.Errorf("append: InnerElemType set on list<%s> (only valid when ElemType==list)", v.ElemType)
	}
	if v.ElemType == TypeMap {
		if !isScalarKeyType(v.MapElemKeyType) {
			return fmt.Errorf("append over list<map<K,V>> requires int or string key, got MapElemKeyType %s", v.MapElemKeyType)
		}
		if !isScalarValueType(v.MapElemValueType) {
			return fmt.Errorf("append over list<map<K,V>> requires scalar value, got MapElemValueType %s", v.MapElemValueType)
		}
		if vk := exprKeyType(v.Value); vk != v.MapElemKeyType {
			return fmt.Errorf("append value: expected map<%s,_>, got map<%s,_>", v.MapElemKeyType, vk)
		}
		if vval := exprValueType(v.Value); vval != v.MapElemValueType {
			return fmt.Errorf("append value: expected map<_,%s>, got map<_,%s>", v.MapElemValueType, vval)
		}
	} else if v.MapElemKeyType != TypeInvalid || v.MapElemValueType != TypeInvalid {
		return fmt.Errorf("append: MapElemKeyType/MapElemValueType set on list<%s> (only valid when ElemType==map)", v.ElemType)
	}
	return nil
}

// verifyMapLit checks a map-literal expression. Phase 3.2 accepts
// int or string keys with one of the four scalar value types; Keys
// and Values must be parallel slices of equal length. Each key's
// Type must equal KeyType; each value's Type must equal ValueType.
// An empty map is legal so long as KeyType + ValueType are stamped.
// Duplicate keys are NOT rejected here (the runtime _lit helper
// resolves them via last-write-wins, matching the vm semantics).
func verifyMapLit(ctx *verifyCtx, v *MapLit) error {
	if !isScalarKeyType(v.KeyType) {
		return fmt.Errorf("map literal: KeyType %s not supported (Phase 3.2 supports int or string keys only)", v.KeyType)
	}
	if !isMapValueType(v.ValueType) {
		return fmt.Errorf("map literal: ValueType %s not supported (Phase 3.2/3.4e supports scalar or list values)", v.ValueType)
	}
	if v.ValueType == TypeList {
		if !isScalarElemType(v.ListValueElemType) {
			return fmt.Errorf("map literal: map<_,list<T>> requires scalar inner, got ListValueElemType %s", v.ListValueElemType)
		}
	} else if v.ListValueElemType != TypeInvalid {
		return fmt.Errorf("map literal: ListValueElemType set on map<_,%s> (only valid when value is list)", v.ValueType)
	}
	if len(v.Keys) != len(v.Values) {
		return fmt.Errorf("map literal: %d keys but %d values", len(v.Keys), len(v.Values))
	}
	for i, k := range v.Keys {
		if k == nil {
			return fmt.Errorf("map literal key %d is nil", i)
		}
		if err := verifyExprCtx(ctx, k); err != nil {
			return fmt.Errorf("map literal key %d: %w", i, err)
		}
		if k.Type() != v.KeyType {
			return fmt.Errorf("map literal key %d: declared %s, got %s", i, v.KeyType, k.Type())
		}
	}
	for i, val := range v.Values {
		if val == nil {
			return fmt.Errorf("map literal value %d is nil", i)
		}
		if err := verifyExprCtx(ctx, val); err != nil {
			return fmt.Errorf("map literal value %d: %w", i, err)
		}
		if val.Type() != v.ValueType {
			return fmt.Errorf("map literal value %d: declared %s, got %s", i, v.ValueType, val.Type())
		}
		if v.ValueType == TypeList {
			if ie := exprElemType(val); ie != v.ListValueElemType {
				return fmt.Errorf("map literal value %d: declared list<%s>, got list<%s>", i, v.ListValueElemType, ie)
			}
		}
	}
	return nil
}

// verifyMapGetExpr checks `m[k]` for a map-typed receiver. Receiver
// must be TypeMap with KeyType + ValueType matching the stamped
// fields; Key must match KeyType. Phase 3.2 panics on miss with
// MOCHI_ERR_INDEX (no Option until Phase 4); the verifier does not
// reason about miss-vs-hit.
func verifyMapGetExpr(ctx *verifyCtx, v *MapGetExpr) error {
	if v.Receiver == nil || v.Key == nil {
		return errors.New("map get with nil Receiver or Key")
	}
	if err := verifyExprCtx(ctx, v.Receiver); err != nil {
		return fmt.Errorf("map get receiver: %w", err)
	}
	if v.Receiver.Type() != TypeMap {
		return fmt.Errorf("map get receiver must be map, got %s", v.Receiver.Type())
	}
	if rk := exprKeyType(v.Receiver); rk != v.KeyType {
		return fmt.Errorf("map get: receiver is map<%s,_>, KeyType stamped is %s", rk, v.KeyType)
	}
	if rv := exprValueType(v.Receiver); rv != v.ValueType {
		return fmt.Errorf("map get: receiver is map<_,%s>, ValueType stamped is %s", rv, v.ValueType)
	}
	if !isScalarKeyType(v.KeyType) {
		return fmt.Errorf("map get: KeyType %s not supported (Phase 3.2 supports int or string keys only)", v.KeyType)
	}
	if !isMapValueType(v.ValueType) {
		return fmt.Errorf("map get: ValueType %s not supported (Phase 3.2/3.4e supports scalar or list values)", v.ValueType)
	}
	if v.ValueType == TypeList {
		if rlv := exprListValueElemType(v.Receiver); rlv != v.ListValueElemType {
			return fmt.Errorf("map get: receiver is map<_,list<%s>>, ListValueElemType stamped is %s", rlv, v.ListValueElemType)
		}
		if !isScalarElemType(v.ListValueElemType) {
			return fmt.Errorf("map get: map<_,list<T>> requires scalar inner, got ListValueElemType %s", v.ListValueElemType)
		}
	} else if v.ListValueElemType != TypeInvalid {
		return fmt.Errorf("map get: ListValueElemType set on map<_,%s> (only valid when value is list)", v.ValueType)
	}
	if err := verifyExprCtx(ctx, v.Key); err != nil {
		return fmt.Errorf("map get key: %w", err)
	}
	if v.Key.Type() != v.KeyType {
		return fmt.Errorf("map get key: expected %s, got %s", v.KeyType, v.Key.Type())
	}
	return nil
}

// verifyMapHasExpr checks `has(m, k)`. Receiver must be TypeMap;
// Key's Type must match KeyType. Result type is fixed to TypeBool
// at the IR level (no field on the node, so nothing to cross-check
// here beyond the receiver + key shape).
func verifyMapHasExpr(ctx *verifyCtx, v *MapHasExpr) error {
	if v.Receiver == nil || v.Key == nil {
		return errors.New("map has with nil Receiver or Key")
	}
	if err := verifyExprCtx(ctx, v.Receiver); err != nil {
		return fmt.Errorf("map has receiver: %w", err)
	}
	if v.Receiver.Type() != TypeMap {
		return fmt.Errorf("map has receiver must be map, got %s", v.Receiver.Type())
	}
	if rk := exprKeyType(v.Receiver); rk != v.KeyType {
		return fmt.Errorf("map has: receiver is map<%s,_>, KeyType stamped is %s", rk, v.KeyType)
	}
	if rv := exprValueType(v.Receiver); rv != v.ValueType {
		return fmt.Errorf("map has: receiver is map<_,%s>, ValueType stamped is %s", rv, v.ValueType)
	}
	if !isScalarKeyType(v.KeyType) {
		return fmt.Errorf("map has: KeyType %s not supported (Phase 3.2 supports int or string keys only)", v.KeyType)
	}
	if !isMapValueType(v.ValueType) {
		return fmt.Errorf("map has: ValueType %s not supported (Phase 3.2/3.4e supports scalar or list values)", v.ValueType)
	}
	if err := verifyExprCtx(ctx, v.Key); err != nil {
		return fmt.Errorf("map has key: %w", err)
	}
	if v.Key.Type() != v.KeyType {
		return fmt.Errorf("map has key: expected %s, got %s", v.KeyType, v.Key.Type())
	}
	return nil
}

// verifyMapLenExpr checks `len(m)` for a map-typed receiver.
func verifyMapLenExpr(ctx *verifyCtx, v *MapLenExpr) error {
	if v.Receiver == nil {
		return errors.New("map len with nil Receiver")
	}
	if err := verifyExprCtx(ctx, v.Receiver); err != nil {
		return fmt.Errorf("map len receiver: %w", err)
	}
	if v.Receiver.Type() != TypeMap {
		return fmt.Errorf("map len receiver must be map, got %s", v.Receiver.Type())
	}
	if rk := exprKeyType(v.Receiver); rk != v.KeyType {
		return fmt.Errorf("map len: receiver is map<%s,_>, KeyType stamped is %s", rk, v.KeyType)
	}
	if rv := exprValueType(v.Receiver); rv != v.ValueType {
		return fmt.Errorf("map len: receiver is map<_,%s>, ValueType stamped is %s", rv, v.ValueType)
	}
	if !isScalarKeyType(v.KeyType) {
		return fmt.Errorf("map len: KeyType %s not supported (Phase 3.2 supports int or string keys only)", v.KeyType)
	}
	if !isMapValueType(v.ValueType) {
		return fmt.Errorf("map len: ValueType %s not supported (Phase 3.2/3.4e supports scalar or list values)", v.ValueType)
	}
	return nil
}

// verifyMapKeysExpr checks `keys(m)`. Result type is TypeList; the
// list's element type is KeyType (carried on MapKeysExpr.KeyType
// and surfaced via exprElemType).
func verifyMapKeysExpr(ctx *verifyCtx, v *MapKeysExpr) error {
	if v.Receiver == nil {
		return errors.New("map keys with nil Receiver")
	}
	if err := verifyExprCtx(ctx, v.Receiver); err != nil {
		return fmt.Errorf("map keys receiver: %w", err)
	}
	if v.Receiver.Type() != TypeMap {
		return fmt.Errorf("map keys receiver must be map, got %s", v.Receiver.Type())
	}
	if rk := exprKeyType(v.Receiver); rk != v.KeyType {
		return fmt.Errorf("map keys: receiver is map<%s,_>, KeyType stamped is %s", rk, v.KeyType)
	}
	if rv := exprValueType(v.Receiver); rv != v.ValueType {
		return fmt.Errorf("map keys: receiver is map<_,%s>, ValueType stamped is %s", rv, v.ValueType)
	}
	if !isScalarKeyType(v.KeyType) {
		return fmt.Errorf("map keys: KeyType %s not supported (Phase 3.2 supports int or string keys only)", v.KeyType)
	}
	if !isMapValueType(v.ValueType) {
		return fmt.Errorf("map keys: ValueType %s not supported (Phase 3.2/3.4e supports scalar or list values)", v.ValueType)
	}
	return nil
}

// verifyMapValuesExpr checks `values(m)`. Result is TypeList; the
// list's element type is ValueType. For map<K,list<V>>, the result is
// list<list<V>> and ListValueElemType carries the inner V.
func verifyMapValuesExpr(ctx *verifyCtx, v *MapValuesExpr) error {
	if v.Receiver == nil {
		return errors.New("map values with nil Receiver")
	}
	if err := verifyExprCtx(ctx, v.Receiver); err != nil {
		return fmt.Errorf("map values receiver: %w", err)
	}
	if v.Receiver.Type() != TypeMap {
		return fmt.Errorf("map values receiver must be map, got %s", v.Receiver.Type())
	}
	if rk := exprKeyType(v.Receiver); rk != v.KeyType {
		return fmt.Errorf("map values: receiver is map<%s,_>, KeyType stamped is %s", rk, v.KeyType)
	}
	if rv := exprValueType(v.Receiver); rv != v.ValueType {
		return fmt.Errorf("map values: receiver is map<_,%s>, ValueType stamped is %s", rv, v.ValueType)
	}
	if !isScalarKeyType(v.KeyType) {
		return fmt.Errorf("map values: KeyType %s not supported (Phase 3.2 supports int or string keys only)", v.KeyType)
	}
	if !isMapValueType(v.ValueType) {
		return fmt.Errorf("map values: ValueType %s not supported (Phase 3.2/3.4e supports scalar or list values)", v.ValueType)
	}
	if v.ValueType == TypeList {
		if rlv := exprListValueElemType(v.Receiver); rlv != v.ListValueElemType {
			return fmt.Errorf("map values: receiver is map<_,list<%s>>, ListValueElemType stamped is %s", rlv, v.ListValueElemType)
		}
		if !isScalarElemType(v.ListValueElemType) {
			return fmt.Errorf("map values: map<_,list<T>> requires scalar inner, got ListValueElemType %s", v.ListValueElemType)
		}
	} else if v.ListValueElemType != TypeInvalid {
		return fmt.Errorf("map values: ListValueElemType set on map<_,%s> (only valid when value is list)", v.ValueType)
	}
	return nil
}

// --- Phase 3.3: set verifier helpers ---

// exprSetElemType returns the element type of a set-typed expression,
// or TypeInvalid if the expression is not set-typed.
func exprSetElemType(e Expr) Type {
	switch v := e.(type) {
	case *VarRef:
		if v.VarType == TypeSet {
			return v.ElemType
		}
	case *SetLiteralExpr:
		return v.ElemType
	case *SetAddExpr:
		return v.ElemType
	}
	return TypeInvalid
}

func verifySetLiteralExpr(ctx *verifyCtx, v *SetLiteralExpr) error {
	if !isScalarElemType(v.ElemType) {
		return fmt.Errorf("SetLiteralExpr: ElemType %s not supported (Phase 3.3 supports scalar element types)", v.ElemType)
	}
	for i, e := range v.Elems {
		if err := verifyExprCtx(ctx, e); err != nil {
			return fmt.Errorf("SetLiteralExpr elem %d: %w", i, err)
		}
		if e.Type() != v.ElemType {
			return fmt.Errorf("SetLiteralExpr elem %d: expected %s, got %s", i, v.ElemType, e.Type())
		}
	}
	return nil
}

func verifySetAddExpr(ctx *verifyCtx, v *SetAddExpr) error {
	if v.Receiver == nil {
		return errors.New("SetAddExpr: nil Receiver")
	}
	if err := verifyExprCtx(ctx, v.Receiver); err != nil {
		return fmt.Errorf("SetAddExpr receiver: %w", err)
	}
	if v.Receiver.Type() != TypeSet {
		return fmt.Errorf("SetAddExpr: receiver must be TypeSet, got %s", v.Receiver.Type())
	}
	if v.Elem == nil {
		return errors.New("SetAddExpr: nil Elem")
	}
	if err := verifyExprCtx(ctx, v.Elem); err != nil {
		return fmt.Errorf("SetAddExpr elem: %w", err)
	}
	if v.Elem.Type() != v.ElemType {
		return fmt.Errorf("SetAddExpr: elem type %s does not match ElemType %s", v.Elem.Type(), v.ElemType)
	}
	return nil
}

func verifySetHasExpr(ctx *verifyCtx, v *SetHasExpr) error {
	if v.Receiver == nil {
		return errors.New("SetHasExpr: nil Receiver")
	}
	if err := verifyExprCtx(ctx, v.Receiver); err != nil {
		return fmt.Errorf("SetHasExpr receiver: %w", err)
	}
	if v.Receiver.Type() != TypeSet {
		return fmt.Errorf("SetHasExpr: receiver must be TypeSet, got %s", v.Receiver.Type())
	}
	if v.Elem == nil {
		return errors.New("SetHasExpr: nil Elem")
	}
	if err := verifyExprCtx(ctx, v.Elem); err != nil {
		return fmt.Errorf("SetHasExpr elem: %w", err)
	}
	if v.Elem.Type() != v.ElemType {
		return fmt.Errorf("SetHasExpr: elem type %s does not match ElemType %s", v.Elem.Type(), v.ElemType)
	}
	return nil
}

func verifySetLenExpr(ctx *verifyCtx, v *SetLenExpr) error {
	if v.Receiver == nil {
		return errors.New("SetLenExpr: nil Receiver")
	}
	if err := verifyExprCtx(ctx, v.Receiver); err != nil {
		return fmt.Errorf("SetLenExpr receiver: %w", err)
	}
	if v.Receiver.Type() != TypeSet {
		return fmt.Errorf("SetLenExpr: receiver must be TypeSet, got %s", v.Receiver.Type())
	}
	return nil
}

// --- Phase 3.4 (omap) verifiers ---

func verifyOMapLiteralExpr(ctx *verifyCtx, v *OMapLiteralExpr) error {
	if !isScalarKeyType(v.KeyType) {
		return fmt.Errorf("OMapLiteralExpr: KeyType %s not supported (requires int or string)", v.KeyType)
	}
	if !isScalarElemType(v.ValueType) {
		return fmt.Errorf("OMapLiteralExpr: ValueType %s not supported (requires scalar)", v.ValueType)
	}
	if len(v.Keys) != len(v.Values) {
		return fmt.Errorf("OMapLiteralExpr: keys/values length mismatch (%d vs %d)", len(v.Keys), len(v.Values))
	}
	for i, k := range v.Keys {
		if err := verifyExprCtx(ctx, k); err != nil {
			return fmt.Errorf("OMapLiteralExpr key %d: %w", i, err)
		}
		if k.Type() != v.KeyType {
			return fmt.Errorf("OMapLiteralExpr key %d: expected %s, got %s", i, v.KeyType, k.Type())
		}
		if err := verifyExprCtx(ctx, v.Values[i]); err != nil {
			return fmt.Errorf("OMapLiteralExpr value %d: %w", i, err)
		}
		if v.Values[i].Type() != v.ValueType {
			return fmt.Errorf("OMapLiteralExpr value %d: expected %s, got %s", i, v.ValueType, v.Values[i].Type())
		}
	}
	return nil
}

func verifyOMapGetExpr(ctx *verifyCtx, v *OMapGetExpr) error {
	if v.Receiver == nil {
		return errors.New("OMapGetExpr: nil Receiver")
	}
	if err := verifyExprCtx(ctx, v.Receiver); err != nil {
		return fmt.Errorf("OMapGetExpr receiver: %w", err)
	}
	if v.Receiver.Type() != TypeOMap {
		return fmt.Errorf("OMapGetExpr: receiver must be TypeOMap, got %s", v.Receiver.Type())
	}
	if v.Key == nil {
		return errors.New("OMapGetExpr: nil Key")
	}
	if err := verifyExprCtx(ctx, v.Key); err != nil {
		return fmt.Errorf("OMapGetExpr key: %w", err)
	}
	if v.Key.Type() != v.KeyType {
		return fmt.Errorf("OMapGetExpr: key type %s does not match KeyType %s", v.Key.Type(), v.KeyType)
	}
	return nil
}

func verifyOMapSetExpr(ctx *verifyCtx, v *OMapSetExpr) error {
	if v.Receiver == nil {
		return errors.New("OMapSetExpr: nil Receiver")
	}
	if err := verifyExprCtx(ctx, v.Receiver); err != nil {
		return fmt.Errorf("OMapSetExpr receiver: %w", err)
	}
	if v.Receiver.Type() != TypeOMap {
		return fmt.Errorf("OMapSetExpr: receiver must be TypeOMap, got %s", v.Receiver.Type())
	}
	if v.Key == nil {
		return errors.New("OMapSetExpr: nil Key")
	}
	if err := verifyExprCtx(ctx, v.Key); err != nil {
		return fmt.Errorf("OMapSetExpr key: %w", err)
	}
	if v.Key.Type() != v.KeyType {
		return fmt.Errorf("OMapSetExpr: key type %s does not match KeyType %s", v.Key.Type(), v.KeyType)
	}
	if v.Value == nil {
		return errors.New("OMapSetExpr: nil Value")
	}
	if err := verifyExprCtx(ctx, v.Value); err != nil {
		return fmt.Errorf("OMapSetExpr value: %w", err)
	}
	if v.Value.Type() != v.ValueType {
		return fmt.Errorf("OMapSetExpr: value type %s does not match ValueType %s", v.Value.Type(), v.ValueType)
	}
	return nil
}

func verifyOMapHasExpr(ctx *verifyCtx, v *OMapHasExpr) error {
	if v.Receiver == nil {
		return errors.New("OMapHasExpr: nil Receiver")
	}
	if err := verifyExprCtx(ctx, v.Receiver); err != nil {
		return fmt.Errorf("OMapHasExpr receiver: %w", err)
	}
	if v.Receiver.Type() != TypeOMap {
		return fmt.Errorf("OMapHasExpr: receiver must be TypeOMap, got %s", v.Receiver.Type())
	}
	if v.Key == nil {
		return errors.New("OMapHasExpr: nil Key")
	}
	if err := verifyExprCtx(ctx, v.Key); err != nil {
		return fmt.Errorf("OMapHasExpr key: %w", err)
	}
	if v.Key.Type() != v.KeyType {
		return fmt.Errorf("OMapHasExpr: key type %s does not match KeyType %s", v.Key.Type(), v.KeyType)
	}
	return nil
}

func verifyOMapLenExpr(ctx *verifyCtx, v *OMapLenExpr) error {
	if v.Receiver == nil {
		return errors.New("OMapLenExpr: nil Receiver")
	}
	if err := verifyExprCtx(ctx, v.Receiver); err != nil {
		return fmt.Errorf("OMapLenExpr receiver: %w", err)
	}
	if v.Receiver.Type() != TypeOMap {
		return fmt.Errorf("OMapLenExpr: receiver must be TypeOMap, got %s", v.Receiver.Type())
	}
	return nil
}

func verifyOMapPutStmt(ctx *verifyCtx, s *OMapPutStmt) error {
	b, ok := ctx.scope.lookup(s.Name)
	if !ok {
		return fmt.Errorf("omap-put: undeclared %q", s.Name)
	}
	if !b.mutable {
		return fmt.Errorf("omap-put: %q is immutable", s.Name)
	}
	if b.t != TypeOMap {
		return fmt.Errorf("omap-put: %q is %s, not an omap", s.Name, b.t)
	}
	if s.Key == nil {
		return fmt.Errorf("omap-put %q: nil Key", s.Name)
	}
	if err := verifyExprCtx(ctx, s.Key); err != nil {
		return fmt.Errorf("omap-put %q key: %w", s.Name, err)
	}
	if s.Key.Type() != b.key {
		return fmt.Errorf("omap-put %q: binding key is %s, got %s", s.Name, b.key, s.Key.Type())
	}
	if s.Value == nil {
		return fmt.Errorf("omap-put %q: nil Value", s.Name)
	}
	if err := verifyExprCtx(ctx, s.Value); err != nil {
		return fmt.Errorf("omap-put %q value: %w", s.Name, err)
	}
	if s.Value.Type() != b.value {
		return fmt.Errorf("omap-put %q: binding value is %s, got %s", s.Name, b.value, s.Value.Type())
	}
	return nil
}

func unOpSignature(op UnOp) (Type, Type, bool) {
	switch op {
	case UnNegI64:
		return TypeInt, TypeInt, true
	case UnNegF64:
		return TypeFloat, TypeFloat, true
	case UnNotBool:
		return TypeBool, TypeBool, true
	}
	return TypeInvalid, TypeInvalid, false
}
