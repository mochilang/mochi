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
//   - The entry function returns TypeUnit.
//   - Every statement is well-formed for the scope it sits in
//     (variables declared before use, BreakStmt/ContinueStmt
//     inside a loop, assignment only to a mutable binding).
//   - Every expression is well-typed (binary/unary operand types
//     match the operator and the recorded Result; VarRef
//     resolves to a binding of the recorded VarType).
//
// Later phases extend this list as new IR shapes land.
func Verify(p *Program) error {
	if p == nil {
		return errors.New("aotir.Verify: nil Program")
	}
	if p.Main < 0 || p.Main >= len(p.Functions) {
		return fmt.Errorf("aotir.Verify: Main index %d out of range [0,%d)", p.Main, len(p.Functions))
	}
	names := make(map[string]int, len(p.Functions))
	for i, fn := range p.Functions {
		if fn == nil {
			return fmt.Errorf("aotir.Verify: Functions[%d] is nil", i)
		}
		if fn.Name == "" {
			return fmt.Errorf("aotir.Verify: Functions[%d] has empty Name", i)
		}
		if prev, dup := names[fn.Name]; dup {
			return fmt.Errorf("aotir.Verify: duplicate function name %q at indices %d and %d", fn.Name, prev, i)
		}
		names[fn.Name] = i
	}
	if entry := p.Functions[p.Main]; entry.ReturnType != TypeUnit {
		return fmt.Errorf("aotir.Verify: entry function %q must return unit, got %s", entry.Name, entry.ReturnType)
	}
	for i, fn := range p.Functions {
		if fn.Body == nil {
			return fmt.Errorf("aotir.Verify: function %q (index %d) has nil Body", fn.Name, i)
		}
		ctx := &verifyCtx{
			fns:        names,
			scope:      newScope(nil),
			loopDepth:  0,
			returnType: fn.ReturnType,
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
// statement: the program's function-name map, the active variable
// scope, current loop nesting depth, and the enclosing function's
// return type. The verifier never mutates fns; scope is pushed and
// popped per Block.
type verifyCtx struct {
	fns        map[string]int
	scope      *scope
	loopDepth  int
	returnType Type
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
	var params []Type
	if p, ok := Builtins[s.Func]; ok {
		params = p
	} else if _, ok := ctx.fns[s.Func]; ok {
		// User-defined callees take no arguments until Phase 2.2.
		params = nil
	} else {
		return fmt.Errorf("unresolved callee %q", s.Func)
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
	ctx.scope.vars[s.Name] = binding{t: s.VarType, mutable: s.Mutable}
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
	return nil
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
	}
	return TypeInvalid, TypeInvalid, TypeInvalid, false
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
