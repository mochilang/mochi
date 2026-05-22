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
		} else {
			if fn.ReturnElemType != TypeInvalid {
				return fmt.Errorf("aotir.Verify: function %q has ReturnElemType set on non-list return type %s", fn.Name, fn.ReturnType)
			}
			if fn.ReturnElemRecordName != "" {
				return fmt.Errorf("aotir.Verify: function %q has ReturnElemRecordName set on non-list return type %s", fn.Name, fn.ReturnType)
			}
		}
		if fn.ReturnType == TypeMap {
			if !isScalarKeyType(fn.ReturnKeyType) {
				return fmt.Errorf("aotir.Verify: function %q returns map but ReturnKeyType is %s (Phase 3.2 supports int or string keys only)", fn.Name, fn.ReturnKeyType)
			}
			if !isScalarValueType(fn.ReturnValueType) {
				return fmt.Errorf("aotir.Verify: function %q returns map but ReturnValueType is %s (Phase 3.2 supports scalar values only)", fn.Name, fn.ReturnValueType)
			}
		} else {
			if fn.ReturnKeyType != TypeInvalid {
				return fmt.Errorf("aotir.Verify: function %q has ReturnKeyType set on non-map return type %s", fn.Name, fn.ReturnType)
			}
			if fn.ReturnValueType != TypeInvalid {
				return fmt.Errorf("aotir.Verify: function %q has ReturnValueType set on non-map return type %s", fn.Name, fn.ReturnType)
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
			} else {
				if pr.ElemType != TypeInvalid {
					return fmt.Errorf("aotir.Verify: function %q param %d: ElemType set on non-list type %s", fn.Name, k, pr.Type)
				}
				if pr.ElemRecordName != "" {
					return fmt.Errorf("aotir.Verify: function %q param %d: ElemRecordName set on non-list type %s", fn.Name, k, pr.Type)
				}
			}
			if pr.Type == TypeMap {
				if !isScalarKeyType(pr.KeyType) {
					return fmt.Errorf("aotir.Verify: function %q param %d: map-typed param has KeyType %s (Phase 3.2 supports int or string keys only)", fn.Name, k, pr.KeyType)
				}
				if !isScalarValueType(pr.ValueType) {
					return fmt.Errorf("aotir.Verify: function %q param %d: map-typed param has ValueType %s (Phase 3.2 supports scalar values only)", fn.Name, k, pr.ValueType)
				}
			} else {
				if pr.KeyType != TypeInvalid {
					return fmt.Errorf("aotir.Verify: function %q param %d: KeyType set on non-map type %s", fn.Name, k, pr.Type)
				}
				if pr.ValueType != TypeInvalid {
					return fmt.Errorf("aotir.Verify: function %q param %d: ValueType set on non-map type %s", fn.Name, k, pr.Type)
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
			fns:           names,
			records:       records,
			scope:         newScope(nil),
			loopDepth:     0,
			returnType:    fn.ReturnType,
			returnRec:     fn.ReturnRecordName,
			returnElem:    fn.ReturnElemType,
			returnElemRec: fn.ReturnElemRecordName,
			returnKey:     fn.ReturnKeyType,
			returnValue:   fn.ReturnValueType,
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
			ctx.scope.vars[pr.Name] = binding{t: pr.Type, mutable: false, record: pr.RecordName, elem: pr.ElemType, elemRec: pr.ElemRecordName, key: pr.KeyType, value: pr.ValueType}
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
	fns           map[string]*Function
	records       map[string]*RecordDecl
	scope         *scope
	loopDepth     int
	returnType    Type
	returnRec     string
	returnElem    Type
	returnElemRec string // record name when returnElem==TypeRecord
	returnKey     Type
	returnValue   Type
}

// scope is a single lexical frame. parent==nil marks the function
// root; nested if/while bodies get child scopes so variables
// declared inside a branch are not visible outside it.
type scope struct {
	parent *scope
	vars   map[string]binding
}

type binding struct {
	t       Type
	mutable bool
	record  string // record name when t==TypeRecord
	elem    Type   // element type when t==TypeList
	elemRec string // element record name when t==TypeList && elem==TypeRecord
	key     Type   // key type when t==TypeMap
	value   Type   // value type when t==TypeMap
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
// shadow) then the program's function table.
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
	return nil, fmt.Errorf("unresolved callee %q", fnName)
}

func verifyLetStmt(ctx *verifyCtx, s *LetStmt) error {
	if s.Name == "" {
		return errors.New("let with empty name")
	}
	if _, already := ctx.scope.vars[s.Name]; already {
		return fmt.Errorf("rebinding %q in same scope", s.Name)
	}
	if s.Init == nil {
		return fmt.Errorf("let %q has nil Init", s.Name)
	}
	if err := verifyExprCtx(ctx, s.Init); err != nil {
		return fmt.Errorf("let %q init: %w", s.Name, err)
	}
	if s.Init.Type() != s.VarType {
		return fmt.Errorf("let %q: declared %s, init produces %s", s.Name, s.VarType, s.Init.Type())
	}
	if s.VarType == TypeRecord {
		if s.RecordName == "" {
			return fmt.Errorf("let %q: record-typed binding missing RecordName", s.Name)
		}
		if _, ok := ctx.records[s.RecordName]; !ok {
			return fmt.Errorf("let %q: record %q is not declared", s.Name, s.RecordName)
		}
		initRec := exprRecordName(s.Init)
		if initRec != s.RecordName {
			return fmt.Errorf("let %q: declared record %q, init produces record %q", s.Name, s.RecordName, initRec)
		}
	} else if s.RecordName != "" {
		return fmt.Errorf("let %q: RecordName set on non-record type %s", s.Name, s.VarType)
	}
	if s.VarType == TypeList {
		if !isListElemType(s.ElemType) {
			return fmt.Errorf("let %q: list binding has ElemType %s (Phase 3.4a supports scalar or record element types)", s.Name, s.ElemType)
		}
		if ie := exprElemType(s.Init); ie != s.ElemType {
			return fmt.Errorf("let %q: declared list<%s>, init produces list<%s>", s.Name, s.ElemType, ie)
		}
		if s.ElemType == TypeRecord {
			if s.ElemRecordName == "" {
				return fmt.Errorf("let %q: list<record> binding missing ElemRecordName", s.Name)
			}
			if _, ok := ctx.records[s.ElemRecordName]; !ok {
				return fmt.Errorf("let %q: list element record %q is not declared", s.Name, s.ElemRecordName)
			}
			if ier := exprElemRecordName(s.Init); ier != s.ElemRecordName {
				return fmt.Errorf("let %q: declared list<%s>, init produces list<%s>", s.Name, s.ElemRecordName, ier)
			}
		} else if s.ElemRecordName != "" {
			return fmt.Errorf("let %q: ElemRecordName set on list<%s> (only valid when ElemType==record)", s.Name, s.ElemType)
		}
	} else if s.ElemType != TypeInvalid {
		return fmt.Errorf("let %q: ElemType set on non-list type %s", s.Name, s.VarType)
	}
	if s.VarType == TypeMap {
		if !isScalarKeyType(s.KeyType) {
			return fmt.Errorf("let %q: map binding has KeyType %s (Phase 3.2 supports int or string keys only)", s.Name, s.KeyType)
		}
		if !isScalarValueType(s.ValueType) {
			return fmt.Errorf("let %q: map binding has ValueType %s (Phase 3.2 supports scalar values only)", s.Name, s.ValueType)
		}
		if ik := exprKeyType(s.Init); ik != s.KeyType {
			return fmt.Errorf("let %q: declared map<%s,...>, init produces map<%s,...>", s.Name, s.KeyType, ik)
		}
		if iv := exprValueType(s.Init); iv != s.ValueType {
			return fmt.Errorf("let %q: declared map<%s,%s>, init produces map<%s,%s>", s.Name, s.KeyType, s.ValueType, s.KeyType, iv)
		}
	} else {
		if s.KeyType != TypeInvalid {
			return fmt.Errorf("let %q: KeyType set on non-map type %s", s.Name, s.VarType)
		}
		if s.ValueType != TypeInvalid {
			return fmt.Errorf("let %q: ValueType set on non-map type %s", s.Name, s.VarType)
		}
	}
	ctx.scope.vars[s.Name] = binding{t: s.VarType, mutable: s.Mutable, record: s.RecordName, elem: s.ElemType, elemRec: s.ElemRecordName, key: s.KeyType, value: s.ValueType}
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
	if s.Body == nil {
		return errors.New("foreach with nil Body block")
	}
	prev := ctx.scope
	ctx.scope = newScope(prev)
	ctx.scope.vars[s.Var] = binding{t: s.ElemType, mutable: false, record: s.ElemRecordName}
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
	}
	if ctx.returnType == TypeMap {
		if vk := exprKeyType(s.Value); vk != ctx.returnKey {
			return fmt.Errorf("return map<%s,...> does not match function return map<%s,...>", vk, ctx.returnKey)
		}
		if vv := exprValueType(s.Value); vv != ctx.returnValue {
			return fmt.Errorf("return map<%s,%s> does not match function return map<%s,%s>", ctx.returnKey, vv, ctx.returnKey, ctx.returnValue)
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
	}
	return ""
}

// exprElemType returns the element type of a list-typed expression,
// or TypeInvalid if the expression is not list-typed. Phase 3.1
// covers VarRef, ListLit, CallExpr, AppendExpr. Lists never appear
// as field reads in 3.1 because record fields cannot hold lists.
// Phase 3.2 adds MapKeysExpr / MapValuesExpr because both produce
// list-typed values (list<K> and list<V> respectively).
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
	case *MapKeysExpr:
		return v.KeyType
	case *MapValuesExpr:
		return v.ValueType
	}
	return TypeInvalid
}

// exprKeyType returns the key type of a map-typed expression, or
// TypeInvalid if the expression is not map-typed. Phase 3.2 covers
// VarRef, MapLit, CallExpr. Maps never appear as field reads in
// 3.2 because record fields cannot hold maps and (until Phase 3.4)
// map values cannot themselves be maps.
func exprKeyType(e Expr) Type {
	switch v := e.(type) {
	case *VarRef:
		return v.KeyType
	case *MapLit:
		return v.KeyType
	case *CallExpr:
		return v.ResultKeyType
	}
	return TypeInvalid
}

// exprValueType returns the value type of a map-typed expression,
// or TypeInvalid if the expression is not map-typed. See exprKeyType
// for the node coverage rationale.
func exprValueType(e Expr) Type {
	switch v := e.(type) {
	case *VarRef:
		return v.ValueType
	case *MapLit:
		return v.ValueType
	case *CallExpr:
		return v.ResultValueType
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
// this to TypeRecord (records as list elements). Nested list and
// map elements remain rejected pending Phase 3.4b/c.
func isListElemType(t Type) bool {
	switch t {
	case TypeInt, TypeFloat, TypeBool, TypeString, TypeRecord:
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
		if b.t == TypeMap {
			if v.KeyType != b.key {
				return fmt.Errorf("variable %q has map<%s,_> in scope, ref says map<%s,_>", v.Name, b.key, v.KeyType)
			}
			if v.ValueType != b.value {
				return fmt.Errorf("variable %q has map<_,%s> in scope, ref says map<_,%s>", v.Name, b.value, v.ValueType)
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
	case *AppendExpr:
		return verifyAppendExpr(ctx, v)
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
	case *CallExpr:
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
		if fn.ReturnType == TypeMap {
			if v.ResultKeyType != fn.ReturnKeyType {
				return fmt.Errorf("call %q result map<%s,_> does not match callee return map<%s,_>",
					v.Func, v.ResultKeyType, fn.ReturnKeyType)
			}
			if v.ResultValueType != fn.ReturnValueType {
				return fmt.Errorf("call %q result map<_,%s> does not match callee return map<_,%s>",
					v.Func, v.ResultValueType, fn.ReturnValueType)
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
	case BinEqRec, BinNeRec:
		return TypeRecord, TypeRecord, TypeBool, true
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
	if !isScalarValueType(v.ValueType) {
		return fmt.Errorf("map literal: ValueType %s not supported (Phase 3.2 supports scalar values only)", v.ValueType)
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
	if !isScalarValueType(v.ValueType) {
		return fmt.Errorf("map get: ValueType %s not supported (Phase 3.2 supports scalar values only)", v.ValueType)
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
	if !isScalarValueType(v.ValueType) {
		return fmt.Errorf("map has: ValueType %s not supported (Phase 3.2 supports scalar values only)", v.ValueType)
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
	if !isScalarValueType(v.ValueType) {
		return fmt.Errorf("map len: ValueType %s not supported (Phase 3.2 supports scalar values only)", v.ValueType)
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
	if !isScalarValueType(v.ValueType) {
		return fmt.Errorf("map keys: ValueType %s not supported (Phase 3.2 supports scalar values only)", v.ValueType)
	}
	return nil
}

// verifyMapValuesExpr checks `values(m)`. Result is TypeList; the
// list's element type is ValueType.
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
	if !isScalarValueType(v.ValueType) {
		return fmt.Errorf("map values: ValueType %s not supported (Phase 3.2 supports scalar values only)", v.ValueType)
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
