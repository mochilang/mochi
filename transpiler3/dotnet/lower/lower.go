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
	agents       map[string]*aotir.AgentDecl
	javaFuncs    map[string]*aotir.JavaFuncDecl // mochiName → JavaFuncDecl for FFI dispatch
}

// Lower translates an aotir.Program into one CompilationUnit per type plus one
// for the main class. The first element is always the main class CU.
func Lower(prog *aotir.Program, colours colour.ColourMap, className string) ([]*csharpsrc.CompilationUnit, error) {
	agentIndex := make(map[string]*aotir.AgentDecl, len(prog.Agents))
	for _, ad := range prog.Agents {
		agentIndex[ad.Name] = ad
	}
	javaFuncIndex := make(map[string]*aotir.JavaFuncDecl, len(prog.JavaFuncs))
	for _, jf := range prog.JavaFuncs {
		javaFuncIndex[jf.MochiName] = jf
	}
	l := &lowerer{
		className: className,
		colours:   colours,
		agents:    agentIndex,
		javaFuncs: javaFuncIndex,
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
		Usings:    []string{"System", "System.Collections.Concurrent", "System.Collections.Generic", "System.Linq", "Mochi.Runtime"},
		Types:     types,
	}

	cus := []*csharpsrc.CompilationUnit{mainCU}
	for _, ad := range prog.Agents {
		agentCU, err := l.lowerAgentDecl(ad)
		if err != nil {
			return nil, fmt.Errorf("agent %q: %w", ad.Name, err)
		}
		cus = append(cus, agentCU)
	}
	return cus, nil
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
	case *aotir.ChanSendStmt:
		return l.lowerChanSendStmt(s)
	case *aotir.StreamEmitStmt:
		return l.lowerStreamEmitStmt(s)
	case *aotir.RawCStmt:
		// C-specific setup code (e.g. Datalog result vars) not needed for .NET.
		return &csharpsrc.EmptyStmt{}, nil
	case *aotir.AgentIntentCallStmt:
		return l.lowerAgentIntentCallStmt(s)
	case *aotir.QueryScopeStmt:
		return l.lowerQueryScopeStmt(s)
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
	case *aotir.ListSortAscExpr:
		return l.lowerListSortAscExpr(e)
	case *aotir.ListSliceExpr:
		return l.lowerListSliceExpr(e)
	case *aotir.DatalogQueryExpr:
		return l.lowerDatalogQueryExpr(e)
	case *aotir.JavaCallExpr:
		return l.lowerJavaCallExpr(e)
	case *aotir.LLMGenerateExpr:
		return l.lowerLLMGenerateExpr(e)
	case *aotir.HttpGetExpr:
		return l.lowerHttpGetExpr(e)
	case *aotir.JsonDecodeExpr:
		return l.lowerJsonDecodeExpr(e)
	case *aotir.AsyncExpr:
		return l.lowerAsyncExpr(e)
	case *aotir.AwaitExpr:
		return l.lowerAwaitExpr(e)
	case *aotir.ChanMakeExpr:
		return l.lowerChanMakeExpr(e)
	case *aotir.ChanRecvExpr:
		return l.lowerChanRecvExpr(e)
	case *aotir.StreamMakeExpr:
		return l.lowerStreamMakeExpr(e)
	case *aotir.SubMakeExpr:
		return l.lowerSubMakeExpr(e)
	case *aotir.SubMakeLimitExpr:
		return l.lowerSubMakeLimitExpr(e)
	case *aotir.SubRecvExpr:
		return l.lowerSubRecvExpr(e)
	case *aotir.AgentLit:
		return l.lowerAgentLitExpr(e)
	case *aotir.AgentSpawnExpr:
		return l.lowerAgentSpawnExpr(e)
	case *aotir.AgentIntentCallExpr:
		return l.lowerAgentIntentCallExpr(e)
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
	// FFI dispatch: if the function name is a Java extern, map to .NET BCL equivalent.
	if jf, ok := l.javaFuncs[e.Func]; ok {
		return lowerJavaCallToDotnet(jf, args)
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

// lowerListSortAscExpr lowers ListSortAscExpr to xs.OrderBy(x => x).ToList().
func (l *lowerer) lowerListSortAscExpr(e *aotir.ListSortAscExpr) (csharpsrc.Expr, error) {
	recv, err := l.lowerExpr(e.Receiver)
	if err != nil {
		return nil, err
	}
	// xs.OrderBy(x => x).ToList()
	identity := &csharpsrc.LambdaExpr{
		Params: []csharpsrc.Param{{Name: "__sx"}},
		Body:   &csharpsrc.NameExpr{Name: "__sx"},
	}
	ordered := &csharpsrc.CallExpr{Receiver: recv, Method: "OrderBy", Args: []csharpsrc.Expr{identity}}
	return &csharpsrc.CallExpr{Receiver: ordered, Method: "ToList", Args: nil}, nil
}

// lowerListSliceExpr lowers ListSliceExpr (skip/take) to xs.Skip(start).Take(count).ToList().
// When End is the "no-take" sentinel (1<<62-1), emits xs.Skip(start).ToList() instead.
func (l *lowerer) lowerListSliceExpr(e *aotir.ListSliceExpr) (csharpsrc.Expr, error) {
	recv, err := l.lowerExpr(e.Receiver)
	if err != nil {
		return nil, err
	}
	start, err := l.lowerExpr(e.Start)
	if err != nil {
		return nil, err
	}
	startInt := &csharpsrc.CastExpr{Type: csharpsrc.TypeInt, X: start}
	skipped := &csharpsrc.CallExpr{Receiver: recv, Method: "Skip", Args: []csharpsrc.Expr{startInt}}

	// Detect skip-only sentinel: 1<<62 - 1 = 4611686018427387903
	if lit, ok := e.End.(*aotir.IntLit); ok && lit.Value == 1<<62-1 {
		return &csharpsrc.CallExpr{Receiver: skipped, Method: "ToList", Args: nil}, nil
	}

	end, err := l.lowerExpr(e.End)
	if err != nil {
		return nil, err
	}
	endInt := &csharpsrc.CastExpr{Type: csharpsrc.TypeInt, X: end}
	lengthExpr := &csharpsrc.BinaryExpr{Left: endInt, Op: "-", Right: startInt}
	taken := &csharpsrc.CallExpr{Receiver: skipped, Method: "Take", Args: []csharpsrc.Expr{lengthExpr}}
	return &csharpsrc.CallExpr{Receiver: taken, Method: "ToList", Args: nil}, nil
}

// lowerQueryScopeStmt lowers a QueryScopeStmt by lowering its body block.
// C# needs no arena; the GC handles allocation.
func (l *lowerer) lowerQueryScopeStmt(s *aotir.QueryScopeStmt) (csharpsrc.Stmt, error) {
	body, err := l.lowerBlock(s.Body)
	if err != nil {
		return nil, fmt.Errorf("dotnet/lower: QueryScopeStmt body: %w", err)
	}
	return body, nil
}

// lowerDatalogQueryExpr evaluates the Datalog program at compile time and emits a static
// list literal containing the pre-computed result strings.
func (l *lowerer) lowerDatalogQueryExpr(e *aotir.DatalogQueryExpr) (csharpsrc.Expr, error) {
	results := datalogEval(e)
	elems := make([]csharpsrc.Expr, len(results))
	for i, r := range results {
		elems[i] = csharpsrc.StringLit(r)
	}
	return &csharpsrc.CollectionInitExpr{
		Type:    csharpsrc.ListTypeRef(csharpsrc.TypeString),
		CtorArgs: nil,
		Elems:   elems,
	}, nil
}

// datalogEval performs semi-naive bottom-up evaluation and returns the flat list
// of free-variable values from matching tuples.
func datalogEval(e *aotir.DatalogQueryExpr) []string {
	if e.Prog == nil {
		return nil
	}
	state := map[string][][]string{}
	for _, f := range e.Prog.Facts {
		args := make([]string, len(f.Args))
		copy(args, f.Args)
		state[f.Name] = append(state[f.Name], args)
	}
	for {
		changed := false
		for _, rule := range e.Prog.Rules {
			newTuples := dlDeriveRule(rule, state)
			for _, t := range newTuples {
				if !dlTupleInRelation(state[rule.HeadName], t) {
					state[rule.HeadName] = append(state[rule.HeadName], t)
					changed = true
				}
			}
		}
		if !changed {
			break
		}
	}
	rel := state[e.QueryName]
	var out []string
	for _, tuple := range rel {
		if len(tuple) != len(e.QueryArgs) {
			continue
		}
		match := true
		for i, qa := range e.QueryArgs {
			if qa != "" {
				expected := qa
				if len(expected) >= 2 && expected[0] == '"' && expected[len(expected)-1] == '"' {
					expected = expected[1 : len(expected)-1]
				}
				if tuple[i] != expected {
					match = false
					break
				}
			}
		}
		if match {
			for i, qa := range e.QueryArgs {
				if qa == "" {
					out = append(out, tuple[i])
				}
			}
		}
	}
	return out
}

func dlDeriveRule(rule aotir.DatalogRule, state map[string][][]string) [][]string {
	results := []map[string]string{{}}
	for _, lit := range rule.Body {
		if lit.IsNeq {
			var next []map[string]string
			for _, env := range results {
				a, aok := env[lit.NeqA]
				b, bok := env[lit.NeqB]
				if !aok || !bok || a != b {
					next = append(next, env)
				}
			}
			results = next
			continue
		}
		if lit.IsNot {
			var next []map[string]string
			for _, env := range results {
				matched := false
				for _, t := range state[lit.Name] {
					if len(t) != len(lit.Args) {
						continue
					}
					ok := true
					for i, arg := range lit.Args {
						if dlResolveArg(arg, env) != t[i] {
							ok = false
							break
						}
					}
					if ok {
						matched = true
						break
					}
				}
				if !matched {
					next = append(next, env)
				}
			}
			results = next
			continue
		}
		var next []map[string]string
		for _, env := range results {
			for _, t := range state[lit.Name] {
				if len(t) != len(lit.Args) {
					continue
				}
				newEnv := dlCopyEnv(env)
				ok := true
				for i, arg := range lit.Args {
					if dlIsVar(arg) {
						if existing, found := newEnv[arg]; found {
							if existing != t[i] {
								ok = false
								break
							}
						} else {
							newEnv[arg] = t[i]
						}
					} else {
						if dlResolveArg(arg, env) != t[i] {
							ok = false
							break
						}
					}
				}
				if ok {
					next = append(next, newEnv)
				}
			}
		}
		results = next
	}
	var heads [][]string
	for _, env := range results {
		head := make([]string, len(rule.HeadArgs))
		valid := true
		for i, arg := range rule.HeadArgs {
			if dlIsVar(arg) {
				v, ok := env[arg]
				if !ok {
					valid = false
					break
				}
				head[i] = v
			} else {
				head[i] = dlResolveArg(arg, env)
			}
		}
		if valid {
			heads = append(heads, head)
		}
	}
	return heads
}

func dlResolveArg(arg string, env map[string]string) string {
	if len(arg) >= 2 && arg[0] == '"' && arg[len(arg)-1] == '"' {
		return arg[1 : len(arg)-1]
	}
	if v, ok := env[arg]; ok {
		return v
	}
	return arg
}

func dlIsVar(arg string) bool {
	return len(arg) > 0 && arg[0] != '"'
}

func dlTupleInRelation(rel [][]string, t []string) bool {
	for _, r := range rel {
		if len(r) != len(t) {
			continue
		}
		match := true
		for i := range r {
			if r[i] != t[i] {
				match = false
				break
			}
		}
		if match {
			return true
		}
	}
	return false
}

func dlCopyEnv(env map[string]string) map[string]string {
	cp := make(map[string]string, len(env))
	for k, v := range env {
		cp[k] = v
	}
	return cp
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
	case aotir.TypeChan:
		return csharpsrc.BlockingCollectionTypeRef(lowerElemType(s.ChanElemType))
	case aotir.TypeStream:
		return csharpsrc.MochiStreamTypeRef(lowerElemType(s.StreamElemType))
	case aotir.TypeSub:
		return csharpsrc.BlockingCollectionTypeRef(lowerElemType(s.SubElemType))
	case aotir.TypeFuture:
		return taskTypeRef(s.FutureElemType)
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

// ---- Agent lowering ----

// lowerAgentDecl lowers an aotir.AgentDecl to a C# class with mutable fields and instance methods.
// Each agent becomes a simple mutable object: no async actor overhead for the sequential fixture set.
//
//	public class MochiAgent_Counter {
//	    public long count = 0L;
//	    public void increment() { count = count + 1L; }
//	    public long value() { return count; }
//	}
func (l *lowerer) lowerAgentDecl(ad *aotir.AgentDecl) (*csharpsrc.CompilationUnit, error) {
	stateFieldNames := make(map[string]bool, len(ad.Fields))
	for _, f := range ad.Fields {
		stateFieldNames[f.Name] = true
	}

	members := make([]csharpsrc.Member, 0, len(ad.Fields)+len(ad.Intents))

	// Mutable fields with default values to satisfy nullable analysis.
	for _, f := range ad.Fields {
		ft := agentFieldType(f.Type)
		members = append(members, &csharpsrc.FieldDecl{
			Modifiers: []string{"public"},
			Type:      ft,
			Name:      f.Name,
			Init:      agentZeroValue(f.Type),
		})
	}

	// Intent methods.
	for _, intent := range ad.Intents {
		method, err := l.lowerIntentMethod(intent, stateFieldNames)
		if err != nil {
			return nil, fmt.Errorf("intent %q: %w", intent.Name, err)
		}
		members = append(members, method)
	}

	agentClass := &csharpsrc.ClassDecl{
		Modifiers: []string{"public"},
		Name:      "MochiAgent_" + ad.Name,
		Members:   members,
	}

	return &csharpsrc.CompilationUnit{
		Namespace: "Mochi.User",
		Usings:    []string{"System", "System.Collections.Concurrent", "System.Collections.Generic", "System.Linq", "Mochi.Runtime"},
		Types:     []csharpsrc.TypeDecl{agentClass},
	}, nil
}

// lowerIntentMethod lowers one intent to a C# instance method.
// `__self->field` VarRefs are rewritten to plain field names (in an instance method, `field` refers to `this.field`).
func (l *lowerer) lowerIntentMethod(intent aotir.AgentIntentDecl, stateFields map[string]bool) (*csharpsrc.MethodDecl, error) {
	body := rewriteAgentSelfRefs(intent.Body, stateFields)
	block, err := l.lowerBlock(body)
	if err != nil {
		return nil, err
	}
	params := make([]csharpsrc.Param, len(intent.Params))
	for i, p := range intent.Params {
		params[i] = csharpsrc.Param{Type: agentFieldType(p.Type), Name: p.Name}
	}
	retType := agentFieldType(intent.ReturnType)
	if intent.ReturnType == aotir.TypeUnit {
		retType = csharpsrc.TypeVoid
	}
	return &csharpsrc.MethodDecl{
		Modifiers:  []string{"public"},
		ReturnType: retType,
		Name:       intent.Name,
		Params:     params,
		Body:       block,
	}, nil
}

// rewriteAgentSelfRefs rewrites `__self->field` VarRefs to plain field names.
func rewriteAgentSelfRefs(b *aotir.Block, stateFields map[string]bool) *aotir.Block {
	if b == nil {
		return nil
	}
	stmts := make([]aotir.Stmt, len(b.Statements))
	for i, s := range b.Statements {
		stmts[i] = rewriteAgentSelfStmt(s, stateFields)
	}
	return &aotir.Block{Statements: stmts}
}

func rewriteAgentSelfStmt(s aotir.Stmt, sf map[string]bool) aotir.Stmt {
	switch s := s.(type) {
	case *aotir.ReturnStmt:
		if s.Value == nil {
			return s
		}
		return &aotir.ReturnStmt{Value: rewriteAgentSelfExpr(s.Value, sf)}
	case *aotir.AssignStmt:
		target := s.Name
		if field := agentSelfField(s.Name); field != "" && sf[field] {
			target = field
		} else if sf[s.Name] {
			target = s.Name
		}
		return &aotir.AssignStmt{Name: target, Value: rewriteAgentSelfExpr(s.Value, sf)}
	case *aotir.LetStmt:
		cp := *s
		if s.Init != nil {
			cp.Init = rewriteAgentSelfExpr(s.Init, sf)
		}
		return &cp
	case *aotir.CallStmt:
		args := make([]aotir.Expr, len(s.Args))
		for i, a := range s.Args {
			args[i] = rewriteAgentSelfExpr(a, sf)
		}
		return &aotir.CallStmt{Func: s.Func, Args: args}
	case *aotir.IfStmt:
		cp := *s
		cp.Cond = rewriteAgentSelfExpr(s.Cond, sf)
		cp.Then = rewriteAgentSelfBlock(s.Then, sf)
		if s.Else != nil {
			cp.Else = rewriteAgentSelfBlock(s.Else, sf)
		}
		return &cp
	default:
		return s
	}
}

func rewriteAgentSelfBlock(b *aotir.Block, sf map[string]bool) *aotir.Block {
	if b == nil {
		return nil
	}
	stmts := make([]aotir.Stmt, len(b.Statements))
	for i, s := range b.Statements {
		stmts[i] = rewriteAgentSelfStmt(s, sf)
	}
	return &aotir.Block{Statements: stmts}
}

func rewriteAgentSelfExpr(e aotir.Expr, sf map[string]bool) aotir.Expr {
	if e == nil {
		return nil
	}
	switch e := e.(type) {
	case *aotir.VarRef:
		if field := agentSelfField(e.Name); field != "" && sf[field] {
			cp := *e
			cp.Name = field
			return &cp
		}
		return e
	case *aotir.BinaryExpr:
		cp := *e
		cp.Left = rewriteAgentSelfExpr(e.Left, sf)
		cp.Right = rewriteAgentSelfExpr(e.Right, sf)
		return &cp
	case *aotir.UnaryExpr:
		cp := *e
		cp.Operand = rewriteAgentSelfExpr(e.Operand, sf)
		return &cp
	case *aotir.CallExpr:
		args := make([]aotir.Expr, len(e.Args))
		for i, a := range e.Args {
			args[i] = rewriteAgentSelfExpr(a, sf)
		}
		cp := *e
		cp.Args = args
		return &cp
	default:
		return e
	}
}

// agentSelfField extracts the field name from "__self->fieldname".
func agentSelfField(name string) string {
	const prefix = "__self->"
	if len(name) > len(prefix) && name[:len(prefix)] == prefix {
		return name[len(prefix):]
	}
	return ""
}

// agentFieldType maps an aotir scalar type to a C# TypeRef for agent fields/params.
func agentFieldType(t aotir.Type) csharpsrc.TypeRef {
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

// lowerAgentLitExpr lowers AgentLit to new MochiAgent_Name { field1 = v1, ... }.
func (l *lowerer) lowerAgentLitExpr(e *aotir.AgentLit) (csharpsrc.Expr, error) {
	// Look up the agent decl to know field order.
	ad, ok := l.agents[e.AgentName]
	if !ok {
		return nil, fmt.Errorf("agent lit: unknown agent %q", e.AgentName)
	}
	// Build args in field-declaration order.
	args := make([]csharpsrc.Expr, len(ad.Fields))
	fieldMap := make(map[string]aotir.Expr, len(e.Fields))
	for _, f := range e.Fields {
		fieldMap[f.Name] = f.Value
	}
	for i, f := range ad.Fields {
		v, err := l.lowerExpr(fieldMap[f.Name])
		if err != nil {
			return nil, fmt.Errorf("agent lit %q field %q: %w", e.AgentName, f.Name, err)
		}
		args[i] = v
	}
	// Emit: new MochiAgent_Name() { field1 = v1, field2 = v2, ... }
	// C# object initializer syntax.
	inits := make([]csharpsrc.DictEntry, len(ad.Fields))
	for i, f := range ad.Fields {
		inits[i] = csharpsrc.DictEntry{Key: &csharpsrc.NameExpr{Name: f.Name}, Value: args[i]}
	}
	return &csharpsrc.AgentNewExpr{
		Type:  csharpsrc.TypeRef{Name: "MochiAgent_" + e.AgentName},
		Inits: inits,
	}, nil
}

// lowerAgentSpawnExpr lowers AgentSpawnExpr to new MochiAgent_Name() with zero-value init.
func (l *lowerer) lowerAgentSpawnExpr(e *aotir.AgentSpawnExpr) (csharpsrc.Expr, error) {
	ad, ok := l.agents[e.AgentName]
	if !ok {
		return nil, fmt.Errorf("spawn: unknown agent %q", e.AgentName)
	}
	inits := make([]csharpsrc.DictEntry, len(ad.Fields))
	for i, f := range ad.Fields {
		inits[i] = csharpsrc.DictEntry{
			Key:   &csharpsrc.NameExpr{Name: f.Name},
			Value: agentZeroValue(f.Type),
		}
	}
	return &csharpsrc.AgentNewExpr{
		Type:  csharpsrc.TypeRef{Name: "MochiAgent_" + e.AgentName},
		Inits: inits,
	}, nil
}

// agentZeroValue returns the C# zero-value expression for an aotir type.
func agentZeroValue(t aotir.Type) csharpsrc.Expr {
	switch t {
	case aotir.TypeInt:
		return &csharpsrc.LiteralExpr{Value: "0L"}
	case aotir.TypeFloat:
		return &csharpsrc.LiteralExpr{Value: "0.0"}
	case aotir.TypeBool:
		return &csharpsrc.LiteralExpr{Value: "false"}
	case aotir.TypeString:
		return &csharpsrc.LiteralExpr{Value: `""`}
	default:
		return &csharpsrc.LiteralExpr{Value: "null"}
	}
}

// lowerAgentIntentCallExpr lowers AgentIntentCallExpr to recv.IntentName(args...).
func (l *lowerer) lowerAgentIntentCallExpr(e *aotir.AgentIntentCallExpr) (csharpsrc.Expr, error) {
	recv, err := l.lowerExpr(e.Receiver)
	if err != nil {
		return nil, err
	}
	args := make([]csharpsrc.Expr, len(e.Args))
	for i, a := range e.Args {
		v, err := l.lowerExpr(a)
		if err != nil {
			return nil, err
		}
		args[i] = v
	}
	return &csharpsrc.CallExpr{Receiver: recv, Method: e.IntentName, Args: args}, nil
}

// lowerAgentIntentCallStmt lowers AgentIntentCallStmt to recv.IntentName(args...);
func (l *lowerer) lowerAgentIntentCallStmt(s *aotir.AgentIntentCallStmt) (csharpsrc.Stmt, error) {
	recv, err := l.lowerExpr(s.Receiver)
	if err != nil {
		return nil, err
	}
	args := make([]csharpsrc.Expr, len(s.Args))
	for i, a := range s.Args {
		v, err := l.lowerExpr(a)
		if err != nil {
			return nil, err
		}
		args[i] = v
	}
	return &csharpsrc.ExprStmt{X: &csharpsrc.CallExpr{Receiver: recv, Method: s.IntentName, Args: args}}, nil
}

// --- Phase 12: FFI (Java extern mapped to .NET BCL) ---

// javaClassToDotnet maps common Java class names to .NET equivalents.
var javaClassToDotnet = map[string]string{
	"java.lang.Math":    "Math",
	"java.lang.String":  "string",
	"java.util.UUID":    "Guid",
	"java.lang.Integer": "int",
	"java.lang.Long":    "long",
	"java.lang.Double":  "double",
}

// javaMethodToDotnet maps (javaClass, javaMethod) to a .NET static call class+method.
var javaMethodToDotnet = map[[2]string][2]string{
	{"java.lang.Math", "abs"}:            {"Math", "Abs"},
	{"java.lang.Math", "max"}:            {"Math", "Max"},
	{"java.lang.Math", "min"}:            {"Math", "Min"},
	{"java.lang.Math", "sqrt"}:           {"Math", "Sqrt"},
	{"java.lang.Math", "pow"}:            {"Math", "Pow"},
	{"java.lang.Math", "floor"}:          {"Math", "Floor"},
	{"java.lang.Math", "ceil"}:           {"Math", "Ceiling"},
	{"java.util.UUID", "randomUUID"}:     {"Guid", "NewGuid"},
	{"java.lang.String", "valueOf"}:      {"Convert", "ToString"},
	{"java.lang.Integer", "parseInt"}:    {"long", "Parse"},
	{"java.lang.Double", "parseDouble"}:  {"double", "Parse"},
}

// lowerJavaCallToDotnet maps a JavaFuncDecl + args to the .NET static call.
func lowerJavaCallToDotnet(jf *aotir.JavaFuncDecl, args []csharpsrc.Expr) (csharpsrc.Expr, error) {
	key := [2]string{jf.ClassName, jf.MethodName}
	if mapping, ok := javaMethodToDotnet[key]; ok {
		return &csharpsrc.StaticCallExpr{
			Class:  mapping[0],
			Method: mapping[1],
			Args:   args,
		}, nil
	}
	// Fallback: derive class from last segment of Java class name and PascalCase method.
	parts := strings.Split(jf.ClassName, ".")
	cls := parts[len(parts)-1]
	method := strings.ToUpper(jf.MethodName[:1]) + jf.MethodName[1:]
	return &csharpsrc.StaticCallExpr{Class: cls, Method: method, Args: args}, nil
}

// lowerJavaCallExpr handles JavaCallExpr nodes directly.
func (l *lowerer) lowerJavaCallExpr(e *aotir.JavaCallExpr) (csharpsrc.Expr, error) {
	args, err := l.lowerExprs(e.Args)
	if err != nil {
		return nil, err
	}
	return lowerJavaCallToDotnet(e.Decl, args)
}

// --- Phase 14: HTTP fetch and JSON decode ---

// lowerHttpGetExpr → Mochi.Runtime.IO.Fetch.Get(url)
func (l *lowerer) lowerHttpGetExpr(e *aotir.HttpGetExpr) (csharpsrc.Expr, error) {
	url, err := l.lowerExpr(e.URL)
	if err != nil {
		return nil, err
	}
	return &csharpsrc.StaticCallExpr{
		Class:  "Mochi.Runtime.IO.Fetch",
		Method: "Get",
		Args:   []csharpsrc.Expr{url},
	}, nil
}

// lowerJsonDecodeExpr → Mochi.Runtime.IO.JSON.Decode(input)
func (l *lowerer) lowerJsonDecodeExpr(e *aotir.JsonDecodeExpr) (csharpsrc.Expr, error) {
	input, err := l.lowerExpr(e.Input)
	if err != nil {
		return nil, err
	}
	return &csharpsrc.StaticCallExpr{
		Class:  "Mochi.Runtime.IO.JSON",
		Method: "Decode",
		Args:   []csharpsrc.Expr{input},
	}, nil
}

// --- Phase 13: LLM generate ---

// lowerLLMGenerateExpr → Mochi.Runtime.Llm.Ai.Call(provider, prompt)
func (l *lowerer) lowerLLMGenerateExpr(e *aotir.LLMGenerateExpr) (csharpsrc.Expr, error) {
	prompt, err := l.lowerExpr(e.Prompt)
	if err != nil {
		return nil, err
	}
	return &csharpsrc.StaticCallExpr{
		Class:  "Mochi.Runtime.Llm.Ai",
		Method: "Call",
		Args:   []csharpsrc.Expr{csharpsrc.StringLit(e.Provider), prompt},
	}, nil
}

// --- Phase 11: async/await ---

// taskTypeRef returns Task<T> for the given element type.
func taskTypeRef(elemType aotir.Type) csharpsrc.TypeRef {
	return csharpsrc.TypeRef{Name: "Task", TypeArgs: []csharpsrc.TypeRef{lowerElemType(elemType)}}
}

// lowerAsyncExpr → Task.Run(() => <body>)
func (l *lowerer) lowerAsyncExpr(e *aotir.AsyncExpr) (csharpsrc.Expr, error) {
	body, err := l.lowerExpr(e.Body)
	if err != nil {
		return nil, err
	}
	// Task.Run(() => body)
	lambda := &csharpsrc.LambdaExpr{Params: nil, Body: body}
	return &csharpsrc.CallExpr{
		Receiver: csharpsrc.Lit("Task"),
		Method:   "Run",
		Args:     []csharpsrc.Expr{lambda},
	}, nil
}

// lowerAwaitExpr → future.GetAwaiter().GetResult()
// Blocking wait: safe for sync Main in console apps.
func (l *lowerer) lowerAwaitExpr(e *aotir.AwaitExpr) (csharpsrc.Expr, error) {
	fut, err := l.lowerExpr(e.Future)
	if err != nil {
		return nil, err
	}
	getAwaiter := &csharpsrc.CallExpr{Receiver: fut, Method: "GetAwaiter", Args: nil}
	return &csharpsrc.CallExpr{Receiver: getAwaiter, Method: "GetResult", Args: nil}, nil
}

// --- Phase 10: channels and streams ---

// lowerChanMakeExpr → new BlockingCollection<T>(cap)
func (l *lowerer) lowerChanMakeExpr(e *aotir.ChanMakeExpr) (csharpsrc.Expr, error) {
	cap, err := l.lowerExpr(e.Cap)
	if err != nil {
		return nil, err
	}
	// BlockingCollection<T>(int cap) takes an int, but cap is long; cast to int.
	capInt := &csharpsrc.CastExpr{Type: csharpsrc.TypeRef{Name: "int"}, X: cap}
	return &csharpsrc.NewExpr{
		Type: csharpsrc.BlockingCollectionTypeRef(lowerElemType(e.ElemType)),
		Args: []csharpsrc.Expr{capInt},
	}, nil
}

// lowerChanRecvExpr → chan.Take()
func (l *lowerer) lowerChanRecvExpr(e *aotir.ChanRecvExpr) (csharpsrc.Expr, error) {
	ch, err := l.lowerExpr(e.Chan)
	if err != nil {
		return nil, err
	}
	return &csharpsrc.CallExpr{Receiver: ch, Method: "Take", Args: nil}, nil
}

// lowerChanSendStmt → chan.Add(val);
func (l *lowerer) lowerChanSendStmt(s *aotir.ChanSendStmt) (csharpsrc.Stmt, error) {
	ch, err := l.lowerExpr(s.Chan)
	if err != nil {
		return nil, err
	}
	val, err := l.lowerExpr(s.Val)
	if err != nil {
		return nil, err
	}
	return &csharpsrc.ExprStmt{X: &csharpsrc.CallExpr{Receiver: ch, Method: "Add", Args: []csharpsrc.Expr{val}}}, nil
}

// lowerStreamMakeExpr → new MochiStream<T>()
func (l *lowerer) lowerStreamMakeExpr(e *aotir.StreamMakeExpr) (csharpsrc.Expr, error) {
	return &csharpsrc.NewExpr{
		Type: csharpsrc.MochiStreamTypeRef(lowerElemType(e.ElemType)),
		Args: nil,
	}, nil
}

// lowerStreamEmitStmt → stream.Emit(val);
func (l *lowerer) lowerStreamEmitStmt(s *aotir.StreamEmitStmt) (csharpsrc.Stmt, error) {
	stream, err := l.lowerExpr(s.Stream)
	if err != nil {
		return nil, err
	}
	val, err := l.lowerExpr(s.Val)
	if err != nil {
		return nil, err
	}
	return &csharpsrc.ExprStmt{X: &csharpsrc.CallExpr{Receiver: stream, Method: "Emit", Args: []csharpsrc.Expr{val}}}, nil
}

// lowerSubMakeExpr → stream.Subscribe()
func (l *lowerer) lowerSubMakeExpr(e *aotir.SubMakeExpr) (csharpsrc.Expr, error) {
	stream, err := l.lowerExpr(e.Stream)
	if err != nil {
		return nil, err
	}
	return &csharpsrc.CallExpr{Receiver: stream, Method: "Subscribe", Args: nil}, nil
}

// lowerSubMakeLimitExpr → stream.Subscribe() (limit ignored for .NET; BlockingCollection is unbounded)
func (l *lowerer) lowerSubMakeLimitExpr(e *aotir.SubMakeLimitExpr) (csharpsrc.Expr, error) {
	stream, err := l.lowerExpr(e.Stream)
	if err != nil {
		return nil, err
	}
	return &csharpsrc.CallExpr{Receiver: stream, Method: "Subscribe", Args: nil}, nil
}

// lowerSubRecvExpr → sub.Take()
func (l *lowerer) lowerSubRecvExpr(e *aotir.SubRecvExpr) (csharpsrc.Expr, error) {
	sub, err := l.lowerExpr(e.Sub)
	if err != nil {
		return nil, err
	}
	return &csharpsrc.CallExpr{Receiver: sub, Method: "Take", Args: nil}, nil
}
