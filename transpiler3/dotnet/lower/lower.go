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
	className string
	colours   colour.ColourMap
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

	mainCU := &csharpsrc.CompilationUnit{
		Namespace: "Mochi.User",
		Usings:    []string{"System"},
		Types:     []csharpsrc.TypeDecl{classDecl},
	}

	return []*csharpsrc.CompilationUnit{mainCU}, nil
}

// lowerFunction translates a non-main aotir.Function to a static MethodDecl.
func (l *lowerer) lowerFunction(fn *aotir.Function) (*csharpsrc.MethodDecl, error) {
	body, err := l.lowerBlock(fn.Body)
	if err != nil {
		return nil, err
	}
	retType := lowerType(fn.ReturnType)
	params, err := lowerParams(fn.Params)
	if err != nil {
		return nil, err
	}
	return &csharpsrc.MethodDecl{
		Modifiers:  []string{"public", "static"},
		ReturnType: retType,
		Name:       fn.Name,
		Params:     params,
		Body:       body,
	}, nil
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
		return &csharpsrc.ExprStmt{
			X: &csharpsrc.BinaryExpr{
				Left:  &csharpsrc.NameExpr{Name: s.Name},
				Op:    "=",
				Right: v,
			},
		}, nil
	case *aotir.IfStmt:
		return l.lowerIfStmt(s)
	case *aotir.WhileStmt:
		return l.lowerWhileStmt(s)
	case *aotir.ForEachStmt:
		return l.lowerForEachStmt(s)
	case *aotir.BreakStmt:
		return &csharpsrc.BreakStmt{}, nil
	case *aotir.ContinueStmt:
		return &csharpsrc.ContinueStmt{}, nil
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
	return &csharpsrc.LocalDeclStmt{Name: s.Name, Init: init}, nil
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
			Type: lowerType(p.Type),
			Name: p.Name,
		}
	}
	return result, nil
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
