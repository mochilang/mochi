// Package lower translates an aotir.Program into csharpsrc compilation units.
// Entry point: Lower(prog, colours, className) → []*CompilationUnit.
package lower

import (
	"fmt"
	"math"
	"path/filepath"
	"strconv"
	"strings"
	"unicode"

	"mochi/transpiler3/c/aotir"
	"mochi/transpiler3/dotnet/colour"
	"mochi/transpiler3/dotnet/csharpsrc"
)

type lowerer struct {
	className    string
	colours      colour.ColourMap
	matchCaseVar string // name of the case-bound variable in the current match arm
}

// Lower translates an aotir.Program into one CompilationUnit per type plus one
// for the main class. The first element is always the main class CU.
func Lower(prog *aotir.Program, colours colour.ColourMap, className string) ([]*csharpsrc.CompilationUnit, error) {
	l := &lowerer{
		className: className,
		colours:   colours,
	}

	mainFn := prog.Functions[prog.Main]
	body, err := l.lowerBlock(mainFn.Body)
	if err != nil {
		return nil, err
	}

	mainMethod := &csharpsrc.MethodDecl{
		Modifiers:  []string{"public", "static"},
		ReturnType: csharpsrc.TypeVoid,
		Name:       "Main",
		Params: []csharpsrc.Param{
			{Type: csharpsrc.TypeRef{Name: "string", Array: true}, Name: "args"},
		},
		Body: body,
	}
	members := []csharpsrc.Member{mainMethod}

	for i, fn := range prog.Functions {
		if i == prog.Main {
			continue
		}
		method, err := l.lowerFunction(fn)
		if err != nil {
			return nil, err
		}
		members = append(members, method)
	}

	classDecl := &csharpsrc.ClassDecl{
		Modifiers: []string{"public", "static"},
		Name:      className,
		Members:   members,
	}

	// Union and record declarations come first so the class can reference them.
	types := make([]csharpsrc.TypeDecl, 0, len(prog.Records)+len(prog.Unions)+1)
	for _, ud := range prog.Unions {
		types = append(types, lowerUnionDecl(ud)...)
	}
	for _, rd := range prog.Records {
		types = append(types, lowerRecordDecl(rd))
	}
	types = append(types, classDecl)

	mainCU := &csharpsrc.CompilationUnit{
		Namespace: "Mochi.User",
		Usings:    []string{"System", "System.Collections.Generic", "System.Linq"},
		Types:     types,
	}

	return []*csharpsrc.CompilationUnit{mainCU}, nil
}

// lowerFunction translates a non-main aotir.Function to a static MethodDecl.
// For lifted closure functions, captured variables are prepended as extra params
// and env-ref VarRefs in the body are rewritten to plain field names.
func (l *lowerer) lowerFunction(fn *aotir.Function) (*csharpsrc.MethodDecl, error) {
	bodyToLower := fn.Body
	if fn.IsLifted && len(fn.Captures) > 0 {
		bodyToLower = rewriteEnvRefs(fn.Body, fn.Captures)
	}
	body, err := l.lowerBlock(bodyToLower)
	if err != nil {
		return nil, err
	}
	retType := lowerReturnType(fn)
	params, err := lowerParams(fn.Params)
	if err != nil {
		return nil, err
	}
	// Prepend capture params for lifted functions.
	if fn.IsLifted && len(fn.Captures) > 0 {
		capParams := make([]csharpsrc.Param, len(fn.Captures))
		for i, cap := range fn.Captures {
			capParams[i] = csharpsrc.Param{Type: funcCaptureType(cap.VarType), Name: cap.FieldName}
		}
		params = append(capParams, params...)
	}
	return &csharpsrc.MethodDecl{
		Modifiers:  []string{"public", "static"},
		ReturnType: retType,
		Name:       fn.Name,
		Params:     params,
		Body:       body,
	}, nil
}

// rewriteEnvRefs returns a copy of b where every VarRef whose Name starts with
// "__e->" is replaced with a VarRef using just the field name after "->".
func rewriteEnvRefs(b *aotir.Block, captures []aotir.FunCapture) *aotir.Block {
	if b == nil {
		return nil
	}
	renames := make(map[string]string, len(captures))
	for _, cap := range captures {
		renames["__e->"+cap.FieldName] = cap.FieldName
	}
	stmts := make([]aotir.Stmt, len(b.Statements))
	for i, s := range b.Statements {
		stmts[i] = rewriteStmtEnvRefs(s, renames)
	}
	return &aotir.Block{Statements: stmts}
}

func rewriteStmtEnvRefs(s aotir.Stmt, renames map[string]string) aotir.Stmt {
	switch s := s.(type) {
	case *aotir.ReturnStmt:
		if s.Value == nil {
			return s
		}
		return &aotir.ReturnStmt{Value: rewriteExprEnvRefs(s.Value, renames)}
	case *aotir.LetStmt:
		cp := *s
		if s.Init != nil {
			v := rewriteExprEnvRefs(s.Init, renames)
			cp.Init = v
		}
		return &cp
	case *aotir.AssignStmt:
		cp := *s
		cp.Value = rewriteExprEnvRefs(s.Value, renames)
		return &cp
	case *aotir.CallStmt:
		args := make([]aotir.Expr, len(s.Args))
		for i, a := range s.Args {
			args[i] = rewriteExprEnvRefs(a, renames)
		}
		cp := *s
		cp.Args = args
		return &cp
	default:
		return s
	}
}

func rewriteExprEnvRefs(e aotir.Expr, renames map[string]string) aotir.Expr {
	switch e := e.(type) {
	case *aotir.VarRef:
		if newName, ok := renames[e.Name]; ok {
			cp := *e
			cp.Name = newName
			return &cp
		}
		return e
	case *aotir.BinaryExpr:
		cp := *e
		cp.Left = rewriteExprEnvRefs(e.Left, renames)
		cp.Right = rewriteExprEnvRefs(e.Right, renames)
		return &cp
	case *aotir.UnaryExpr:
		cp := *e
		cp.Operand = rewriteExprEnvRefs(e.Operand, renames)
		return &cp
	case *aotir.CallExpr:
		args := make([]aotir.Expr, len(e.Args))
		for i, a := range e.Args {
			args[i] = rewriteExprEnvRefs(a, renames)
		}
		cp := *e
		cp.Args = args
		return &cp
	case *aotir.FunCallExpr:
		callee := rewriteExprEnvRefs(e.Callee, renames)
		args := make([]aotir.Expr, len(e.Args))
		for i, a := range e.Args {
			args[i] = rewriteExprEnvRefs(a, renames)
		}
		return &aotir.FunCallExpr{Callee: callee, Args: args, Result: e.Result}
	default:
		return e
	}
}

// funcCaptureType returns the C# TypeRef for a captured variable's type.
func funcCaptureType(t aotir.Type) csharpsrc.TypeRef {
	switch t {
	case aotir.TypeInt:
		return csharpsrc.TypeLong
	case aotir.TypeFloat:
		return csharpsrc.TypeDouble
	case aotir.TypeBool:
		return csharpsrc.TypeBool
	case aotir.TypeString:
		return csharpsrc.TypeString
	default:
		return csharpsrc.TypeObject
	}
}

func (l *lowerer) lowerBlock(b *aotir.Block) (*csharpsrc.Block, error) {
	if b == nil {
		return &csharpsrc.Block{}, nil
	}
	stmts := make([]csharpsrc.Stmt, 0, len(b.Statements))
	for _, s := range b.Statements {
		cs, err := l.lowerStmt(s)
		if err != nil {
			return nil, err
		}
		stmts = append(stmts, cs)
	}
	return &csharpsrc.Block{Stmts: stmts}, nil
}

func (l *lowerer) lowerStmt(s aotir.Stmt) (csharpsrc.Stmt, error) {
	switch s := s.(type) {
	case *aotir.CallStmt:
		return l.lowerCallStmt(s)
	case *aotir.ReturnStmt:
		if s.Value == nil {
			return &csharpsrc.ReturnStmt{}, nil
		}
		v, err := l.lowerExpr(s.Value)
		if err != nil {
			return nil, err
		}
		return &csharpsrc.ReturnStmt{Value: v}, nil
	case *aotir.LetStmt:
		return l.lowerLetStmt(s)
	case *aotir.AssignStmt:
		v, err := l.lowerExpr(s.Value)
		if err != nil {
			return nil, err
		}
		return &csharpsrc.AssignStmt{
			Target: &csharpsrc.NameExpr{Name: s.Name},
			Value:  v,
		}, nil
	case *aotir.IfStmt:
		return l.lowerIfStmt(s)
	case *aotir.WhileStmt:
		return l.lowerWhileStmt(s)
	case *aotir.ForEachStmt:
		return l.lowerForEachStmt(s)
	case *aotir.ForRangeStmt:
		return l.lowerForRangeStmt(s)
	case *aotir.BreakStmt:
		return &csharpsrc.BreakStmt{}, nil
	case *aotir.ContinueStmt:
		return &csharpsrc.ContinueStmt{}, nil
	case *aotir.ListSetStmt:
		return l.lowerListSetStmt(s)
	case *aotir.MapPutStmt:
		return l.lowerMapPutStmt(s)
	case *aotir.MatchStmt:
		return l.lowerMatchStmt(s)
	case *aotir.ClosureEnvStmt:
		// C# closures capture from enclosing scope; the C env struct is not needed.
		return &csharpsrc.EmptyStmt{}, nil
	default:
		return nil, fmt.Errorf("dotnet/lower: unsupported statement %T", s)
	}
}

func (l *lowerer) lowerCallStmt(s *aotir.CallStmt) (csharpsrc.Stmt, error) {
	switch s.Func {
	case "mochi_print_str", "mochi_print_i64", "mochi_print_f64", "mochi_print_bool":
		if len(s.Args) != 1 {
			return nil, fmt.Errorf("dotnet/lower: %s wants 1 arg, got %d", s.Func, len(s.Args))
		}
		arg, err := l.lowerExpr(s.Args[0])
		if err != nil {
			return nil, err
		}
		call := &csharpsrc.StaticCallExpr{
			Class:  "Mochi.Runtime.IO.Print",
			Method: "Line",
			Args:   []csharpsrc.Expr{arg},
		}
		return &csharpsrc.ExprStmt{X: call}, nil
	default:
		if !strings.HasPrefix(s.Func, "mochi_") {
			args, err := l.lowerExprs(s.Args)
			if err != nil {
				return nil, err
			}
			call := &csharpsrc.StaticCallExpr{
				Class:  l.className,
				Method: s.Func,
				Args:   args,
			}
			return &csharpsrc.ExprStmt{X: call}, nil
		}
		return nil, fmt.Errorf("dotnet/lower: unsupported builtin %q", s.Func)
	}
}

func (l *lowerer) lowerLetStmt(s *aotir.LetStmt) (csharpsrc.Stmt, error) {
	var init csharpsrc.Expr
	var err error
	if s.Init != nil {
		init, err = l.lowerExpr(s.Init)
		if err != nil {
			return nil, err
		}
	}
	var typ *csharpsrc.TypeRef
	// Use explicit type when:
	// - no initializer (C# forbids 'var x;')
	// - union type binding (var would infer the variant, breaking switch pattern matching)
	// - function type binding (C# CS8917: delegate type cannot be inferred from lambda)
	if init == nil || s.VarType == aotir.TypeUnion || s.VarType == aotir.TypeFun {
		t := lowerLetStmtType(s)
		typ = &t
	}
	return &csharpsrc.LocalDeclStmt{Type: typ, Name: s.Name, Init: init}, nil
}

func (l *lowerer) lowerIfStmt(s *aotir.IfStmt) (csharpsrc.Stmt, error) {
	cond, err := l.lowerExpr(s.Cond)
	if err != nil {
		return nil, err
	}
	then, err := l.lowerBlock(s.Then)
	if err != nil {
		return nil, err
	}
	result := &csharpsrc.IfStmt{Cond: cond, Then: *then}
	if s.Else != nil {
		elseBlk, err := l.lowerBlock(s.Else)
		if err != nil {
			return nil, err
		}
		result.Else = elseBlk
	}
	return result, nil
}

func (l *lowerer) lowerWhileStmt(s *aotir.WhileStmt) (csharpsrc.Stmt, error) {
	cond, err := l.lowerExpr(s.Cond)
	if err != nil {
		return nil, err
	}
	body, err := l.lowerBlock(s.Body)
	if err != nil {
		return nil, err
	}
	return &csharpsrc.WhileStmt{Cond: cond, Body: *body}, nil
}

func (l *lowerer) lowerForRangeStmt(s *aotir.ForRangeStmt) (csharpsrc.Stmt, error) {
	start, err := l.lowerExpr(s.Start)
	if err != nil {
		return nil, err
	}
	end, err := l.lowerExpr(s.End)
	if err != nil {
		return nil, err
	}
	body, err := l.lowerBlock(s.Body)
	if err != nil {
		return nil, err
	}
	varName := s.Var
	return &csharpsrc.ForStmt{
		Init: &csharpsrc.LocalDeclStmt{
			Type: &csharpsrc.TypeLong,
			Name: varName,
			Init: start,
		},
		Cond: &csharpsrc.BinaryExpr{
			Left:  &csharpsrc.NameExpr{Name: varName},
			Op:    "<",
			Right: end,
		},
		Update: &csharpsrc.ExprStmt{
			X: &csharpsrc.UnaryExpr{Op: "++", Operand: &csharpsrc.NameExpr{Name: varName}, Postfix: true},
		},
		Body: *body,
	}, nil
}

func (l *lowerer) lowerForEachStmt(s *aotir.ForEachStmt) (csharpsrc.Stmt, error) {
	iter, err := l.lowerExpr(s.List)
	if err != nil {
		return nil, err
	}
	body, err := l.lowerBlock(s.Body)
	if err != nil {
		return nil, err
	}
	return &csharpsrc.ForeachStmt{
		ElemType: lowerType(s.ElemType),
		ElemName: s.Var,
		Iter:     iter,
		Body:     *body,
	}, nil
}

func (l *lowerer) lowerExprs(exprs []aotir.Expr) ([]csharpsrc.Expr, error) {
	result := make([]csharpsrc.Expr, len(exprs))
	for i, e := range exprs {
		ce, err := l.lowerExpr(e)
		if err != nil {
			return nil, err
		}
		result[i] = ce
	}
	return result, nil
}

func (l *lowerer) lowerExpr(e aotir.Expr) (csharpsrc.Expr, error) {
	switch e := e.(type) {
	case *aotir.StringLit:
		return csharpsrc.Lit(quoteCS(e.Value)), nil
	case *aotir.IntLit:
		return csharpsrc.Lit(fmt.Sprintf("%dL", e.Value)), nil
	case *aotir.FloatLit:
		return csharpsrc.Lit(formatDouble(e.Value)), nil
	case *aotir.BoolLit:
		if e.Value {
			return csharpsrc.Lit("true"), nil
		}
		return csharpsrc.Lit("false"), nil
	case *aotir.VarRef:
		return &csharpsrc.NameExpr{Name: e.Name}, nil
	case *aotir.BinaryExpr:
		return l.lowerBinaryExpr(e)
	case *aotir.UnaryExpr:
		return l.lowerUnaryExpr(e)
	case *aotir.CallExpr:
		return l.lowerCallExpr(e)
	case *aotir.NumCastExpr:
		operand, err := l.lowerExpr(e.Operand)
		if err != nil {
			return nil, err
		}
		return &csharpsrc.CastExpr{Type: csharpsrc.TypeLong, X: operand}, nil
	case *aotir.StrLenExpr:
		recv, err := l.lowerExpr(e.Receiver)
		if err != nil {
			return nil, err
		}
		return &csharpsrc.CastExpr{
			Type: csharpsrc.TypeLong,
			X:    &csharpsrc.FieldAccessExpr{Receiver: recv, Field: "Length"},
		}, nil
	case *aotir.StrIndexExpr:
		return l.lowerStrIndexExpr(e)
	case *aotir.StrContainsExpr:
		recv, err := l.lowerExpr(e.Receiver)
		if err != nil {
			return nil, err
		}
		sub, err := l.lowerExpr(e.Sub)
		if err != nil {
			return nil, err
		}
		return &csharpsrc.CallExpr{Receiver: recv, Method: "Contains", Args: []csharpsrc.Expr{sub}}, nil
	case *aotir.MathCallExpr:
		return l.lowerMathCallExpr(e)
	// --- records (Phase 4) ---
	case *aotir.RecordLit:
		return l.lowerRecordLit(e)
	case *aotir.FieldAccess:
		return l.lowerFieldAccess(e)
	// --- sum types (Phase 5) ---
	case *aotir.VariantLit:
		return l.lowerVariantLit(e)
	case *aotir.VariantFieldAccess:
		return l.lowerVariantFieldAccess(e)
	case *aotir.UnionVarRef:
		return &csharpsrc.NameExpr{Name: e.Name}, nil
	// --- collections (Phase 3) ---
	case *aotir.ListLit:
		return l.lowerListLit(e)
	case *aotir.IndexExpr:
		return l.lowerIndexExpr(e)
	case *aotir.LenExpr:
		return l.lowerLenExpr(e)
	case *aotir.AppendExpr:
		return l.lowerAppendExpr(e)
	case *aotir.MapLit:
		return l.lowerMapLit(e)
	case *aotir.MapGetExpr:
		return l.lowerMapGetExpr(e)
	case *aotir.MapHasExpr:
		return l.lowerMapHasExpr(e)
	case *aotir.MapLenExpr:
		return l.lowerMapLenExpr(e)
	case *aotir.MapKeysExpr:
		return l.lowerMapKeysExpr(e)
	case *aotir.MapValuesExpr:
		return l.lowerMapValuesExpr(e)
	case *aotir.SetLiteralExpr:
		return l.lowerSetLiteralExpr(e)
	case *aotir.SetAddExpr:
		return l.lowerSetAddExpr(e)
	case *aotir.SetHasExpr:
		return l.lowerSetHasExpr(e)
	case *aotir.SetLenExpr:
		return l.lowerSetLenExpr(e)
	case *aotir.ListFilterExpr:
		return l.lowerListFilterExpr(e)
	case *aotir.ListMapExpr:
		return l.lowerListMapExpr(e)
	case *aotir.ListFoldlExpr:
		return l.lowerListFoldlExpr(e)
	case *aotir.FunLit:
		return l.lowerFunLit(e)
	case *aotir.FunCallExpr:
		return l.lowerFunCallExpr(e)
	default:
		return nil, fmt.Errorf("dotnet/lower: unsupported expression %T", e)
	}
}

func (l *lowerer) lowerBinaryExpr(e *aotir.BinaryExpr) (csharpsrc.Expr, error) {
	left, err := l.lowerExpr(e.Left)
	if err != nil {
		return nil, err
	}
	right, err := l.lowerExpr(e.Right)
	if err != nil {
		return nil, err
	}
	if e.Op == aotir.BinStrCat {
		return &csharpsrc.StaticCallExpr{
			Class:  "string",
			Method: "Concat",
			Args:   []csharpsrc.Expr{left, right},
		}, nil
	}
	if e.Op == aotir.BinEqList {
		return &csharpsrc.CallExpr{
			Receiver: left,
			Method:   "SequenceEqual",
			Args:     []csharpsrc.Expr{right},
		}, nil
	}
	if e.Op == aotir.BinNeList {
		return &csharpsrc.UnaryExpr{
			Op: "!",
			Operand: &csharpsrc.CallExpr{
				Receiver: left,
				Method:   "SequenceEqual",
				Args:     []csharpsrc.Expr{right},
			},
		}, nil
	}
	op := lowerBinOp(e.Op)
	return &csharpsrc.BinaryExpr{Left: left, Op: op, Right: right}, nil
}

func (l *lowerer) lowerUnaryExpr(e *aotir.UnaryExpr) (csharpsrc.Expr, error) {
	operand, err := l.lowerExpr(e.Operand)
	if err != nil {
		return nil, err
	}
	switch e.Op {
	case aotir.UnNegI64, aotir.UnNegF64:
		return &csharpsrc.UnaryExpr{Op: "-", Operand: operand}, nil
	case aotir.UnNotBool:
		return &csharpsrc.UnaryExpr{Op: "!", Operand: operand}, nil
	default:
		return nil, fmt.Errorf("dotnet/lower: unsupported unary op %v", e.Op)
	}
}

func (l *lowerer) lowerCallExpr(e *aotir.CallExpr) (csharpsrc.Expr, error) {
	args, err := l.lowerExprs(e.Args)
	if err != nil {
		return nil, err
	}
	return &csharpsrc.StaticCallExpr{
		Class:  l.className,
		Method: e.Func,
		Args:   args,
	}, nil
}

func lowerBinOp(op aotir.BinOp) string {
	switch op {
	case aotir.BinAddI64, aotir.BinAddF64, aotir.BinStrCat:
		return "+"
	case aotir.BinSubI64, aotir.BinSubF64:
		return "-"
	case aotir.BinMulI64, aotir.BinMulF64:
		return "*"
	case aotir.BinDivI64, aotir.BinDivF64:
		return "/"
	case aotir.BinModI64:
		return "%"
	case aotir.BinEqI64, aotir.BinEqF64, aotir.BinEqBool, aotir.BinEqStr, aotir.BinEqRec:
		return "=="
	case aotir.BinNeI64, aotir.BinNeF64, aotir.BinNeBool, aotir.BinNeStr, aotir.BinNeRec:
		return "!="
	case aotir.BinLtI64, aotir.BinLtF64:
		return "<"
	case aotir.BinLeI64, aotir.BinLeF64:
		return "<="
	case aotir.BinGtI64, aotir.BinGtF64:
		return ">"
	case aotir.BinGeI64, aotir.BinGeF64:
		return ">="
	case aotir.BinAndBool:
		return "&&"
	case aotir.BinOrBool:
		return "||"
	default:
		return "?"
	}
}

func lowerType(t aotir.Type) csharpsrc.TypeRef {
	switch t {
	case aotir.TypeString:
		return csharpsrc.TypeString
	case aotir.TypeInt:
		return csharpsrc.TypeLong
	case aotir.TypeFloat:
		return csharpsrc.TypeDouble
	case aotir.TypeBool:
		return csharpsrc.TypeBool
	case aotir.TypeUnit:
		return csharpsrc.TypeVoid
	default:
		return csharpsrc.TypeObject
	}
}

func lowerParams(params []aotir.Param) ([]csharpsrc.Param, error) {
	result := make([]csharpsrc.Param, len(params))
	for i, p := range params {
		result[i] = csharpsrc.Param{
			Type: lowerParamType(p),
			Name: p.Name,
		}
	}
	return result, nil
}

// lowerParamType returns the C# TypeRef for an aotir Param, including record names.
func lowerParamType(p aotir.Param) csharpsrc.TypeRef {
	switch p.Type {
	case aotir.TypeRecord:
		return csharpsrc.TypeRef{Name: p.RecordName}
	case aotir.TypeList:
		if p.ElemType == aotir.TypeRecord && p.ElemRecordName != "" {
			return csharpsrc.ListTypeRef(csharpsrc.TypeRef{Name: p.ElemRecordName})
		}
		return csharpsrc.ListTypeRef(lowerElemType(p.ElemType))
	case aotir.TypeMap:
		return csharpsrc.DictTypeRef(lowerType(p.KeyType), lowerType(p.ValueType))
	case aotir.TypeSet:
		return csharpsrc.HashSetTypeRef(lowerElemType(p.ElemType))
	case aotir.TypeFun:
		return funcTypeRef(p.FunSig)
	default:
		return lowerType(p.Type)
	}
}

// lowerReturnType returns the C# TypeRef for a function's return type.
func lowerReturnType(fn *aotir.Function) csharpsrc.TypeRef {
	switch fn.ReturnType {
	case aotir.TypeRecord:
		return csharpsrc.TypeRef{Name: fn.ReturnRecordName}
	case aotir.TypeList:
		if fn.ReturnElemType == aotir.TypeRecord && fn.ReturnElemRecordName != "" {
			return csharpsrc.ListTypeRef(csharpsrc.TypeRef{Name: fn.ReturnElemRecordName})
		}
		return csharpsrc.ListTypeRef(lowerElemType(fn.ReturnElemType))
	case aotir.TypeMap:
		return csharpsrc.DictTypeRef(lowerType(fn.ReturnKeyType), lowerType(fn.ReturnValueType))
	case aotir.TypeFun:
		return funcTypeRef(fn.ReturnFunSig)
	default:
		return lowerType(fn.ReturnType)
	}
}

// lowerRecordDecl converts an aotir.RecordDecl into a C# sealed record declaration.
func lowerRecordDecl(rd *aotir.RecordDecl) csharpsrc.TypeDecl {
	components := make([]csharpsrc.RecordComponent, len(rd.Fields))
	for i, f := range rd.Fields {
		var t csharpsrc.TypeRef
		if f.Type == aotir.TypeRecord && f.RecordName != "" {
			t = csharpsrc.TypeRef{Name: f.RecordName}
		} else {
			t = lowerType(f.Type)
		}
		components[i] = csharpsrc.RecordComponent{
			Type: t,
			Name: snakeToPascal(f.Name),
		}
	}
	return &csharpsrc.RecordDecl{
		Modifiers:  []string{"public", "sealed"},
		Name:       rd.Name,
		Components: components,
	}
}

// snakeToPascal converts a snake_case identifier to PascalCase.
// "field_name" → "FieldName", "x" → "X".
func snakeToPascal(name string) string {
	parts := strings.Split(name, "_")
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
		return name
	}
	return sb.String()
}

func (l *lowerer) lowerStrIndexExpr(e *aotir.StrIndexExpr) (csharpsrc.Expr, error) {
	recv, err := l.lowerExpr(e.Receiver)
	if err != nil {
		return nil, err
	}
	idx, err := l.lowerExpr(e.Index)
	if err != nil {
		return nil, err
	}
	// recv.Substring((int)idx, 1)
	return &csharpsrc.CallExpr{
		Receiver: recv,
		Method:   "Substring",
		Args: []csharpsrc.Expr{
			&csharpsrc.CastExpr{Type: csharpsrc.TypeInt, X: idx},
			csharpsrc.Lit("1"),
		},
	}, nil
}

func (l *lowerer) lowerMathCallExpr(e *aotir.MathCallExpr) (csharpsrc.Expr, error) {
	arg, err := l.lowerExpr(e.Arg)
	if err != nil {
		return nil, err
	}
	var method string
	switch e.Func {
	case "abs_i64", "abs_f64":
		method = "Abs"
	case "floor":
		method = "Floor"
	case "ceil":
		method = "Ceiling"
	default:
		return nil, fmt.Errorf("dotnet/lower: unknown MathCallExpr func %q", e.Func)
	}
	result := &csharpsrc.StaticCallExpr{
		Class:  "Math",
		Method: method,
		Args:   []csharpsrc.Expr{arg},
	}
	// floor/ceil return double; cast to long for int result.
	if e.Result == aotir.TypeInt && (e.Func == "floor" || e.Func == "ceil") {
		return &csharpsrc.CastExpr{Type: csharpsrc.TypeLong, X: result}, nil
	}
	return result, nil
}

// --- Phase 4 record lowering helpers ---

func (l *lowerer) lowerRecordLit(e *aotir.RecordLit) (csharpsrc.Expr, error) {
	args := make([]csharpsrc.Expr, len(e.Fields))
	for i, f := range e.Fields {
		v, err := l.lowerExpr(f.Value)
		if err != nil {
			return nil, err
		}
		args[i] = v
	}
	return &csharpsrc.NewExpr{
		Type: csharpsrc.TypeRef{Name: e.TypeName},
		Args: args,
	}, nil
}

func (l *lowerer) lowerFieldAccess(e *aotir.FieldAccess) (csharpsrc.Expr, error) {
	recv, err := l.lowerExpr(e.Receiver)
	if err != nil {
		return nil, err
	}
	return &csharpsrc.FieldAccessExpr{
		Receiver: recv,
		Field:    snakeToPascal(e.FieldName),
	}, nil
}

// --- Phase 3 collection lowering helpers ---

func (l *lowerer) lowerListLit(e *aotir.ListLit) (csharpsrc.Expr, error) {
	elems, err := l.lowerExprs(e.Elems)
	if err != nil {
		return nil, err
	}
	elemType := lowerElemType(e.ElemType)
	if e.ElemType == aotir.TypeRecord && e.ElemRecordName != "" {
		elemType = csharpsrc.TypeRef{Name: e.ElemRecordName}
	}
	return &csharpsrc.CollectionInitExpr{
		Type:  csharpsrc.ListTypeRef(elemType),
		Elems: elems,
	}, nil
}

func (l *lowerer) lowerIndexExpr(e *aotir.IndexExpr) (csharpsrc.Expr, error) {
	recv, err := l.lowerExpr(e.Receiver)
	if err != nil {
		return nil, err
	}
	idx, err := l.lowerExpr(e.Index)
	if err != nil {
		return nil, err
	}
	return &csharpsrc.IndexAccessExpr{
		Receiver: recv,
		Index:    &csharpsrc.CastExpr{Type: csharpsrc.TypeInt, X: idx},
	}, nil
}

func (l *lowerer) lowerLenExpr(e *aotir.LenExpr) (csharpsrc.Expr, error) {
	recv, err := l.lowerExpr(e.Receiver)
	if err != nil {
		return nil, err
	}
	return &csharpsrc.CastExpr{
		Type: csharpsrc.TypeLong,
		X:    &csharpsrc.FieldAccessExpr{Receiver: recv, Field: "Count"},
	}, nil
}

func (l *lowerer) lowerAppendExpr(e *aotir.AppendExpr) (csharpsrc.Expr, error) {
	recv, err := l.lowerExpr(e.Receiver)
	if err != nil {
		return nil, err
	}
	val, err := l.lowerExpr(e.Value)
	if err != nil {
		return nil, err
	}
	return &csharpsrc.CollectionInitExpr{
		Type:     csharpsrc.ListTypeRef(lowerElemType(e.ElemType)),
		CtorArgs: []csharpsrc.Expr{recv},
		Elems:    []csharpsrc.Expr{val},
	}, nil
}

func (l *lowerer) lowerMapLit(e *aotir.MapLit) (csharpsrc.Expr, error) {
	keys, err := l.lowerExprs(e.Keys)
	if err != nil {
		return nil, err
	}
	vals, err := l.lowerExprs(e.Values)
	if err != nil {
		return nil, err
	}
	entries := make([]csharpsrc.DictEntry, len(keys))
	for i := range keys {
		entries[i] = csharpsrc.DictEntry{Key: keys[i], Value: vals[i]}
	}
	return &csharpsrc.DictInitExpr{
		Type:    csharpsrc.DictTypeRef(lowerType(e.KeyType), lowerType(e.ValueType)),
		Entries: entries,
	}, nil
}

func (l *lowerer) lowerMapGetExpr(e *aotir.MapGetExpr) (csharpsrc.Expr, error) {
	recv, err := l.lowerExpr(e.Receiver)
	if err != nil {
		return nil, err
	}
	key, err := l.lowerExpr(e.Key)
	if err != nil {
		return nil, err
	}
	return &csharpsrc.IndexAccessExpr{Receiver: recv, Index: key}, nil
}

func (l *lowerer) lowerMapHasExpr(e *aotir.MapHasExpr) (csharpsrc.Expr, error) {
	recv, err := l.lowerExpr(e.Receiver)
	if err != nil {
		return nil, err
	}
	key, err := l.lowerExpr(e.Key)
	if err != nil {
		return nil, err
	}
	return &csharpsrc.CallExpr{Receiver: recv, Method: "ContainsKey", Args: []csharpsrc.Expr{key}}, nil
}

func (l *lowerer) lowerMapLenExpr(e *aotir.MapLenExpr) (csharpsrc.Expr, error) {
	recv, err := l.lowerExpr(e.Receiver)
	if err != nil {
		return nil, err
	}
	return &csharpsrc.CastExpr{
		Type: csharpsrc.TypeLong,
		X:    &csharpsrc.FieldAccessExpr{Receiver: recv, Field: "Count"},
	}, nil
}

func (l *lowerer) lowerMapKeysExpr(e *aotir.MapKeysExpr) (csharpsrc.Expr, error) {
	recv, err := l.lowerExpr(e.Receiver)
	if err != nil {
		return nil, err
	}
	// recv.Keys.ToList() — Dictionary preserves insertion order in .NET Core
	keys := &csharpsrc.FieldAccessExpr{Receiver: recv, Field: "Keys"}
	return &csharpsrc.CallExpr{Receiver: keys, Method: "ToList", Args: nil}, nil
}

func (l *lowerer) lowerMapValuesExpr(e *aotir.MapValuesExpr) (csharpsrc.Expr, error) {
	recv, err := l.lowerExpr(e.Receiver)
	if err != nil {
		return nil, err
	}
	vals := &csharpsrc.FieldAccessExpr{Receiver: recv, Field: "Values"}
	return &csharpsrc.CallExpr{Receiver: vals, Method: "ToList", Args: nil}, nil
}

func (l *lowerer) lowerSetLiteralExpr(e *aotir.SetLiteralExpr) (csharpsrc.Expr, error) {
	elems, err := l.lowerExprs(e.Elems)
	if err != nil {
		return nil, err
	}
	return &csharpsrc.CollectionInitExpr{
		Type:  csharpsrc.HashSetTypeRef(lowerElemType(e.ElemType)),
		Elems: elems,
	}, nil
}

func (l *lowerer) lowerSetAddExpr(e *aotir.SetAddExpr) (csharpsrc.Expr, error) {
	recv, err := l.lowerExpr(e.Receiver)
	if err != nil {
		return nil, err
	}
	elem, err := l.lowerExpr(e.Elem)
	if err != nil {
		return nil, err
	}
	return &csharpsrc.CollectionInitExpr{
		Type:     csharpsrc.HashSetTypeRef(lowerElemType(e.ElemType)),
		CtorArgs: []csharpsrc.Expr{recv},
		Elems:    []csharpsrc.Expr{elem},
	}, nil
}

func (l *lowerer) lowerSetHasExpr(e *aotir.SetHasExpr) (csharpsrc.Expr, error) {
	recv, err := l.lowerExpr(e.Receiver)
	if err != nil {
		return nil, err
	}
	elem, err := l.lowerExpr(e.Elem)
	if err != nil {
		return nil, err
	}
	return &csharpsrc.CallExpr{Receiver: recv, Method: "Contains", Args: []csharpsrc.Expr{elem}}, nil
}

func (l *lowerer) lowerSetLenExpr(e *aotir.SetLenExpr) (csharpsrc.Expr, error) {
	recv, err := l.lowerExpr(e.Receiver)
	if err != nil {
		return nil, err
	}
	return &csharpsrc.CastExpr{
		Type: csharpsrc.TypeLong,
		X:    &csharpsrc.FieldAccessExpr{Receiver: recv, Field: "Count"},
	}, nil
}

func (l *lowerer) lowerListFilterExpr(e *aotir.ListFilterExpr) (csharpsrc.Expr, error) {
	list, err := l.lowerExpr(e.List)
	if err != nil {
		return nil, err
	}
	fn, err := l.lowerExpr(e.Fn)
	if err != nil {
		return nil, err
	}
	filtered := &csharpsrc.CallExpr{Receiver: list, Method: "Where", Args: []csharpsrc.Expr{fn}}
	return &csharpsrc.CallExpr{Receiver: filtered, Method: "ToList", Args: nil}, nil
}

// lowerListMapExpr lowers ListMapExpr (map(xs, fn)) to xs.Select(fn).ToList().
func (l *lowerer) lowerListMapExpr(e *aotir.ListMapExpr) (csharpsrc.Expr, error) {
	list, err := l.lowerExpr(e.List)
	if err != nil {
		return nil, err
	}
	fn, err := l.lowerExpr(e.Fn)
	if err != nil {
		return nil, err
	}
	sel := &csharpsrc.CallExpr{Receiver: list, Method: "Select", Args: []csharpsrc.Expr{fn}}
	return &csharpsrc.CallExpr{Receiver: sel, Method: "ToList", Args: nil}, nil
}

// lowerListFoldlExpr lowers ListFoldlExpr (reduce(xs, fn, init)) to xs.Aggregate(init, fn).
func (l *lowerer) lowerListFoldlExpr(e *aotir.ListFoldlExpr) (csharpsrc.Expr, error) {
	list, err := l.lowerExpr(e.List)
	if err != nil {
		return nil, err
	}
	fn, err := l.lowerExpr(e.Fn)
	if err != nil {
		return nil, err
	}
	init, err := l.lowerExpr(e.Init)
	if err != nil {
		return nil, err
	}
	return &csharpsrc.CallExpr{Receiver: list, Method: "Aggregate", Args: []csharpsrc.Expr{init, fn}}, nil
}

// lowerFunLit translates an aotir.FunLit (closure literal) to a C# lambda expression.
// The lambda body calls the lifted static method, threading any captured variables.
//
// Non-capturing: (__p0, __p1) => ClassName.__anon_1(__p0, __p1)
// Capturing:     (__p0, __p1) => ClassName.__anon_2(captureVar, __p0, __p1)
// Void return:   (__p0) => { ClassName.__anon_3(__p0); }
func (l *lowerer) lowerFunLit(e *aotir.FunLit) (csharpsrc.Expr, error) {
	sig := e.Sig
	if sig == nil {
		return nil, fmt.Errorf("dotnet/lower: FunLit %q has nil Sig", e.FuncName)
	}

	params := make([]csharpsrc.Param, len(sig.ParamTypes))
	paramNames := make([]string, len(sig.ParamTypes))
	for i := range sig.ParamTypes {
		name := fmt.Sprintf("__p%d", i)
		paramNames[i] = name
		params[i] = csharpsrc.Param{Name: name}
	}

	// Build call args: captures first, then sig params.
	callArgs := make([]csharpsrc.Expr, 0, len(e.Captures)+len(paramNames))
	for _, cap := range e.Captures {
		callArgs = append(callArgs, &csharpsrc.NameExpr{Name: cap.SrcName})
	}
	for _, pn := range paramNames {
		callArgs = append(callArgs, &csharpsrc.NameExpr{Name: pn})
	}

	callExpr := &csharpsrc.StaticCallExpr{
		Class:  l.className,
		Method: e.FuncName,
		Args:   callArgs,
	}

	if sig.ReturnType == aotir.TypeUnit {
		block := &csharpsrc.Block{
			Stmts: []csharpsrc.Stmt{&csharpsrc.ExprStmt{X: callExpr}},
		}
		return &csharpsrc.LambdaExpr{Params: params, Block: block}, nil
	}
	return &csharpsrc.LambdaExpr{Params: params, Body: callExpr}, nil
}

// lowerFunCallExpr translates an aotir.FunCallExpr (indirect call through a
// function-typed value) to a C# delegate invocation: callee(args...).
func (l *lowerer) lowerFunCallExpr(e *aotir.FunCallExpr) (csharpsrc.Expr, error) {
	callee, err := l.lowerExpr(e.Callee)
	if err != nil {
		return nil, err
	}
	args := make([]csharpsrc.Expr, len(e.Args))
	for i, a := range e.Args {
		a2, err := l.lowerExpr(a)
		if err != nil {
			return nil, err
		}
		args[i] = a2
	}
	return &csharpsrc.DelegateCallExpr{Callee: callee, Args: args}, nil
}

// funcTypeRef returns the C# Func<>/Action<> TypeRef for a FunSig.
func funcTypeRef(sig *aotir.FunSig) csharpsrc.TypeRef {
	if sig == nil {
		return csharpsrc.TypeRef{Name: "System.Func", TypeArgs: []csharpsrc.TypeRef{csharpsrc.TypeObject, csharpsrc.TypeObject}}
	}
	boxed := func(t aotir.Type) csharpsrc.TypeRef {
		switch t {
		case aotir.TypeInt:
			return csharpsrc.TypeRef{Name: "long"}
		case aotir.TypeFloat:
			return csharpsrc.TypeRef{Name: "double"}
		case aotir.TypeBool:
			return csharpsrc.TypeRef{Name: "bool"}
		case aotir.TypeString:
			return csharpsrc.TypeString
		default:
			return csharpsrc.TypeObject
		}
	}
	if sig.ReturnType == aotir.TypeUnit {
		// Action<T1, T2, ...>
		if len(sig.ParamTypes) == 0 {
			return csharpsrc.TypeRef{Name: "System.Action"}
		}
		args := make([]csharpsrc.TypeRef, len(sig.ParamTypes))
		for i, p := range sig.ParamTypes {
			args[i] = boxed(p)
		}
		return csharpsrc.TypeRef{Name: "System.Action", TypeArgs: args}
	}
	// Func<T1, T2, ..., TResult>
	args := make([]csharpsrc.TypeRef, 0, len(sig.ParamTypes)+1)
	for _, p := range sig.ParamTypes {
		args = append(args, boxed(p))
	}
	args = append(args, boxed(sig.ReturnType))
	return csharpsrc.TypeRef{Name: "System.Func", TypeArgs: args}
}

func (l *lowerer) lowerListSetStmt(s *aotir.ListSetStmt) (csharpsrc.Stmt, error) {
	idx, err := l.lowerExpr(s.Index)
	if err != nil {
		return nil, err
	}
	val, err := l.lowerExpr(s.Value)
	if err != nil {
		return nil, err
	}
	return &csharpsrc.AssignStmt{
		Target: &csharpsrc.IndexAccessExpr{
			Receiver: &csharpsrc.NameExpr{Name: s.Name},
			Index:    &csharpsrc.CastExpr{Type: csharpsrc.TypeInt, X: idx},
		},
		Value: val,
	}, nil
}

func (l *lowerer) lowerMapPutStmt(s *aotir.MapPutStmt) (csharpsrc.Stmt, error) {
	key, err := l.lowerExpr(s.Key)
	if err != nil {
		return nil, err
	}
	val, err := l.lowerExpr(s.Value)
	if err != nil {
		return nil, err
	}
	return &csharpsrc.AssignStmt{
		Target: &csharpsrc.IndexAccessExpr{
			Receiver: &csharpsrc.NameExpr{Name: s.Name},
			Index:    key,
		},
		Value: val,
	}, nil
}

// --- Phase 5 sum-type lowering helpers ---

// lowerUnionDecl converts an aotir.UnionDecl to an abstract base record +
// one sealed record per variant. All are emitted as top-level CompilationUnit types.
func lowerUnionDecl(ud *aotir.UnionDecl) []csharpsrc.TypeDecl {
	base := &csharpsrc.AbstractRecordDecl{
		Modifiers: []string{"public", "abstract"},
		Name:      ud.Name,
	}
	result := []csharpsrc.TypeDecl{base}
	for _, v := range ud.Variants {
		components := make([]csharpsrc.RecordComponent, len(v.Fields))
		for i, f := range v.Fields {
			var t csharpsrc.TypeRef
			if f.FieldType == aotir.TypeRecord && f.RecordName != "" {
				t = csharpsrc.TypeRef{Name: f.RecordName}
			} else {
				t = lowerType(f.FieldType)
			}
			components[i] = csharpsrc.RecordComponent{
				Type: t,
				Name: snakeToPascal(f.Name),
			}
		}
		variant := &csharpsrc.RecordDecl{
			Modifiers:  []string{"public", "sealed"},
			Name:       v.Name,
			Components: components,
			Interfaces: []csharpsrc.TypeRef{{Name: ud.Name}},
		}
		result = append(result, variant)
	}
	return result
}

func (l *lowerer) lowerVariantLit(e *aotir.VariantLit) (csharpsrc.Expr, error) {
	args := make([]csharpsrc.Expr, len(e.Fields))
	for i, f := range e.Fields {
		v, err := l.lowerExpr(f.Value)
		if err != nil {
			return nil, err
		}
		args[i] = v
	}
	return &csharpsrc.NewExpr{
		Type: csharpsrc.TypeRef{Name: e.VariantName},
		Args: args,
	}, nil
}

func (l *lowerer) lowerVariantFieldAccess(e *aotir.VariantFieldAccess) (csharpsrc.Expr, error) {
	// Inside a match arm, fields are accessed via the case-bound variable.
	if l.matchCaseVar != "" {
		return &csharpsrc.FieldAccessExpr{
			Receiver: &csharpsrc.NameExpr{Name: l.matchCaseVar},
			Field:    snakeToPascal(e.FieldName),
		}, nil
	}
	// Outside a match arm (rare), lower the receiver and access the field.
	recv, err := l.lowerExpr(e.Receiver)
	if err != nil {
		return nil, err
	}
	return &csharpsrc.FieldAccessExpr{
		Receiver: recv,
		Field:    snakeToPascal(e.FieldName),
	}, nil
}

// lowerMatchStmt lowers an aotir.MatchStmt to a C# switch statement.
// If ResultVar is non-empty, declares the result variable before the switch.
func (l *lowerer) lowerMatchStmt(s *aotir.MatchStmt) (csharpsrc.Stmt, error) {
	target, err := l.lowerExpr(s.Target)
	if err != nil {
		return nil, err
	}

	var cases []csharpsrc.SwitchCaseClause
	for i, arm := range s.Arms {
		c, err := l.lowerMatchArm(&arm, i)
		if err != nil {
			return nil, fmt.Errorf("match arm %q: %w", arm.VariantName, err)
		}
		cases = append(cases, c)
	}
	if s.Default != nil {
		dc, err := l.lowerMatchArm(s.Default, -1)
		if err != nil {
			return nil, fmt.Errorf("match default arm: %w", err)
		}
		dc.IsDefault = true
		dc.Label = ""
		cases = append(cases, dc)
	} else {
		// Synthetic default so C# definite-assignment analysis is satisfied.
		cases = append(cases, csharpsrc.SwitchCaseClause{
			IsDefault: true,
			NoBreak:   true,
			Body: []csharpsrc.Stmt{
				&csharpsrc.ThrowStmt{
					Value: &csharpsrc.NewExpr{
						Type: csharpsrc.TypeRef{Name: "InvalidOperationException"},
						Args: []csharpsrc.Expr{csharpsrc.StringLit("unreachable match")},
					},
				},
			},
		})
	}

	return &csharpsrc.SwitchStmt{Tag: target, Cases: cases}, nil
}

// lowerMatchArm lowers one arm of a MatchStmt.
func (l *lowerer) lowerMatchArm(arm *aotir.MatchArm, idx int) (csharpsrc.SwitchCaseClause, error) {
	var caseVar string
	var label string

	if arm.VariantName != "" {
		if idx >= 0 {
			caseVar = fmt.Sprintf("__mc_%s_%d", arm.VariantName, idx)
		} else {
			caseVar = fmt.Sprintf("__mc_%s", arm.VariantName)
		}
		label = arm.VariantName + " " + caseVar
	}

	// Set case variable context for VariantFieldAccess.
	saved := l.matchCaseVar
	l.matchCaseVar = caseVar
	defer func() { l.matchCaseVar = saved }()

	var body []csharpsrc.Stmt

	// Materialize bindings: var r = __mc_Circle.R;
	for _, binding := range arm.Bindings {
		var t csharpsrc.TypeRef
		if binding.FieldType == aotir.TypeRecord && binding.RecordName != "" {
			t = csharpsrc.TypeRef{Name: binding.RecordName}
		} else {
			t = lowerType(binding.FieldType)
		}
		body = append(body, &csharpsrc.LocalDeclStmt{
			Type: &t,
			Name: binding.VarName,
			Init: &csharpsrc.FieldAccessExpr{
				Receiver: &csharpsrc.NameExpr{Name: caseVar},
				Field:    snakeToPascal(binding.FieldName),
			},
		})
	}

	blk, err := l.lowerBlock(arm.Body)
	if err != nil {
		return csharpsrc.SwitchCaseClause{}, err
	}
	body = append(body, blk.Stmts...)

	return csharpsrc.SwitchCaseClause{
		Label: label,
		Body:  body,
	}, nil
}

// lowerLetStmtType returns the explicit C# TypeRef for a LetStmt variable.
// Used when an explicit type annotation is required (no init, or union type).
func lowerLetStmtType(s *aotir.LetStmt) csharpsrc.TypeRef {
	switch s.VarType {
	case aotir.TypeRecord:
		if s.RecordName != "" {
			return csharpsrc.TypeRef{Name: s.RecordName}
		}
	case aotir.TypeUnion:
		if s.UnionName != "" {
			return csharpsrc.TypeRef{Name: s.UnionName}
		}
	case aotir.TypeList:
		if s.ElemType == aotir.TypeRecord && s.ElemRecordName != "" {
			return csharpsrc.ListTypeRef(csharpsrc.TypeRef{Name: s.ElemRecordName})
		}
		return csharpsrc.ListTypeRef(lowerElemType(s.ElemType))
	case aotir.TypeMap:
		return csharpsrc.DictTypeRef(lowerType(s.KeyType), lowerType(s.ValueType))
	case aotir.TypeSet:
		return csharpsrc.HashSetTypeRef(lowerElemType(s.ElemType))
	case aotir.TypeFun:
		return funcTypeRef(s.FunSig)
	}
	return lowerType(s.VarType)
}

// lowerElemType converts an aotir element type to a csharpsrc TypeRef.
// Unlike lowerType, this always returns a concrete scalar; TypeList/Map/Set
// fall back to object (nested collections are Phase 3.4).
func lowerElemType(t aotir.Type) csharpsrc.TypeRef {
	switch t {
	case aotir.TypeString:
		return csharpsrc.TypeString
	case aotir.TypeInt:
		return csharpsrc.TypeLong
	case aotir.TypeFloat:
		return csharpsrc.TypeDouble
	case aotir.TypeBool:
		return csharpsrc.TypeBool
	default:
		return csharpsrc.TypeObject
	}
}

// quoteCS converts a Go string to a C# double-quoted string literal.
// Go and C# share the same escape sequences for the common subset.
func quoteCS(s string) string {
	return strconv.Quote(s)
}

// formatDouble converts a Go float64 to a C# double literal.
func formatDouble(f float64) string {
	if math.IsNaN(f) {
		return "double.NaN"
	}
	if math.IsInf(f, 1) {
		return "double.PositiveInfinity"
	}
	if math.IsInf(f, -1) {
		return "double.NegativeInfinity"
	}
	s := strconv.FormatFloat(f, 'g', -1, 64)
	if !strings.ContainsAny(s, ".eE") {
		s += ".0"
	}
	return s
}

// ClassName converts a Mochi source filename to a PascalCase class name.
// "hello.mochi"      -> "Hello"
// "my_program.mochi" -> "MyProgram"
func ClassName(src string) string {
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
