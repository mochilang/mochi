// Package lower translates an aotir.Program into a rtree.File.
// Entry point: Lower(prog, colours, crateName) (*rtree.File, error).
package lower

import (
	"fmt"
	"path/filepath"
	"strings"
	"unicode"

	"mochi/transpiler3/c/aotir"
	"mochi/transpiler3/rust/colour"
	"mochi/transpiler3/rust/rtree"
)

// CrateName converts a Mochi source filename to a snake_case crate name.
// "hello.mochi"       -> "hello"
// "hello_world.mochi" -> "hello_world"
// "hello-world.mochi" -> "hello_world"
func CrateName(src string) string {
	src = filepath.Base(src)
	src = strings.TrimSuffix(src, ".mochi")
	var sb strings.Builder
	for i, r := range src {
		switch {
		case r == '-':
			sb.WriteByte('_')
		case unicode.IsUpper(r):
			if i > 0 {
				sb.WriteByte('_')
			}
			sb.WriteRune(unicode.ToLower(r))
		default:
			sb.WriteRune(r)
		}
	}
	out := sb.String()
	if out == "" {
		out = "mochi_out"
	}
	return out
}

type lowerer struct {
	colours colour.ColourMap
	prog    *aotir.Program
}

// Lower translates an aotir.Program into a rtree.File.
func Lower(prog *aotir.Program, colours colour.ColourMap, crateName string) (*rtree.File, error) {
	l := &lowerer{colours: colours, prog: prog}

	mainFn := prog.Functions[prog.Main]
	mainBody, err := l.lowerBlock(mainFn.Body)
	if err != nil {
		return nil, err
	}

	mainItem := &rtree.FnDecl{
		Name: "main",
		Body: mainBody,
	}

	file := &rtree.File{
		Name: crateName,
		Uses: []*rtree.Use{},
		Items: []rtree.Item{mainItem},
	}

	for i, fn := range prog.Functions {
		if i == prog.Main {
			continue
		}
		fd, err := l.lowerFunction(fn)
		if err != nil {
			return nil, err
		}
		file.Items = append(file.Items, fd)
	}

	return file, nil
}

func (l *lowerer) lowerFunction(fn *aotir.Function) (*rtree.FnDecl, error) {
	body, err := l.lowerBlock(fn.Body)
	if err != nil {
		return nil, err
	}
	params := make([]rtree.FnParam, 0, len(fn.Params))
	for _, p := range fn.Params {
		params = append(params, rtree.FnParam{
			Name:     p.Name,
			TypeName: rustTypeName(p.Type),
		})
	}
	return &rtree.FnDecl{
		Name:       fn.Name,
		Params:     params,
		ReturnType: rustReturnType(fn.ReturnType),
		Body:       body,
		IsAsync:    l.colours[fn.Name] == colour.Red,
	}, nil
}

func (l *lowerer) lowerBlock(b *aotir.Block) ([]rtree.Stmt, error) {
	if b == nil {
		return nil, nil
	}
	out := make([]rtree.Stmt, 0, len(b.Statements))
	for _, s := range b.Statements {
		ls, err := l.lowerStmt(s)
		if err != nil {
			return nil, err
		}
		out = append(out, ls)
	}
	return out, nil
}

func (l *lowerer) lowerStmt(s aotir.Stmt) (rtree.Stmt, error) {
	switch n := s.(type) {
	case *aotir.CallStmt:
		return l.lowerCallStmt(n)
	case *aotir.LetStmt:
		return l.lowerLetStmt(n)
	case *aotir.AssignStmt:
		return l.lowerAssignStmt(n)
	case *aotir.IfStmt:
		return l.lowerIfStmt(n)
	case *aotir.WhileStmt:
		return l.lowerWhileStmt(n)
	case *aotir.ForRangeStmt:
		return l.lowerForRangeStmt(n)
	case *aotir.BreakStmt:
		return &rtree.BreakStmt{}, nil
	case *aotir.ContinueStmt:
		return &rtree.ContinueStmt{}, nil
	case *aotir.ReturnStmt:
		return l.lowerReturnStmt(n)
	}
	return nil, fmt.Errorf("rust lower: unsupported stmt %T", s)
}

func (l *lowerer) lowerLetStmt(s *aotir.LetStmt) (rtree.Stmt, error) {
	v, err := l.lowerExpr(s.Init)
	if err != nil {
		return nil, err
	}
	ty := ""
	// Only emit explicit type when needed (e.g. integer literal without inference target).
	// For now leave inferred to keep output simple and rustfmt-friendly.
	_ = ty
	return &rtree.LetStmt{
		Mutable: s.Mutable,
		Name:    s.Name,
		Value:   v,
	}, nil
}

func (l *lowerer) lowerAssignStmt(s *aotir.AssignStmt) (rtree.Stmt, error) {
	v, err := l.lowerExpr(s.Value)
	if err != nil {
		return nil, err
	}
	return &rtree.AssignStmt{Target: &rtree.Ident{Name: s.Name}, Value: v}, nil
}

func (l *lowerer) lowerIfStmt(s *aotir.IfStmt) (rtree.Stmt, error) {
	cond, err := l.lowerExpr(s.Cond)
	if err != nil {
		return nil, err
	}
	then, err := l.lowerBlock(s.Then)
	if err != nil {
		return nil, err
	}
	var els []rtree.Stmt
	if s.Else != nil {
		els, err = l.lowerBlock(s.Else)
		if err != nil {
			return nil, err
		}
	}
	return &rtree.IfStmt{Cond: cond, Then: then, Else: els}, nil
}

func (l *lowerer) lowerWhileStmt(s *aotir.WhileStmt) (rtree.Stmt, error) {
	cond, err := l.lowerExpr(s.Cond)
	if err != nil {
		return nil, err
	}
	body, err := l.lowerBlock(s.Body)
	if err != nil {
		return nil, err
	}
	return &rtree.WhileStmt{Cond: cond, Body: body}, nil
}

func (l *lowerer) lowerForRangeStmt(s *aotir.ForRangeStmt) (rtree.Stmt, error) {
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
	return &rtree.ForRangeStmt{Var: s.Var, Start: start, End: end, Body: body}, nil
}

func (l *lowerer) lowerReturnStmt(s *aotir.ReturnStmt) (rtree.Stmt, error) {
	if s.Value == nil {
		return &rtree.ReturnStmt{}, nil
	}
	v, err := l.lowerExpr(s.Value)
	if err != nil {
		return nil, err
	}
	return &rtree.ReturnStmt{Value: v}, nil
}

func (l *lowerer) lowerCallStmt(c *aotir.CallStmt) (rtree.Stmt, error) {
	if mapped, ok := builtinCall(c.Func); ok {
		args := make([]rtree.Expr, 0, len(c.Args))
		for _, a := range c.Args {
			ea, err := l.lowerExpr(a)
			if err != nil {
				return nil, err
			}
			args = append(args, ea)
		}
		return &rtree.ExprStmt{Expr: &rtree.CallExpr{Func: mapped, Args: args}}, nil
	}
	// User-defined function call as a statement.
	args := make([]rtree.Expr, 0, len(c.Args))
	for _, a := range c.Args {
		ea, err := l.lowerExpr(a)
		if err != nil {
			return nil, err
		}
		args = append(args, ea)
	}
	return &rtree.ExprStmt{Expr: &rtree.CallExpr{Func: c.Func, Args: args}}, nil
}

// builtinCall returns the Rust runtime path for a mangled C-lowerer builtin
// name. The second return is false for non-builtins.
func builtinCall(name string) (string, bool) {
	switch name {
	case "mochi_print_str":
		return "mochi_runtime::io::print_str", true
	case "mochi_print_i64":
		return "mochi_runtime::io::print_i64", true
	case "mochi_print_f64":
		return "mochi_runtime::io::print_f64", true
	case "mochi_print_bool":
		return "mochi_runtime::io::print_bool", true
	}
	return "", false
}

func (l *lowerer) lowerExpr(e aotir.Expr) (rtree.Expr, error) {
	switch n := e.(type) {
	case *aotir.StringLit:
		return &rtree.StringLit{Value: n.Value}, nil
	case *aotir.IntLit:
		return &rtree.IntLit{Value: n.Value}, nil
	case *aotir.FloatLit:
		return &rtree.FloatLit{Value: n.Value}, nil
	case *aotir.BoolLit:
		return &rtree.BoolLit{Value: n.Value}, nil
	case *aotir.VarRef:
		return &rtree.Ident{Name: n.Name}, nil
	case *aotir.BinaryExpr:
		return l.lowerBinaryExpr(n)
	case *aotir.UnaryExpr:
		return l.lowerUnaryExpr(n)
	case *aotir.CallExpr:
		return l.lowerCallExpr(n)
	case *aotir.NumCastExpr:
		op, err := l.lowerExpr(n.Operand)
		if err != nil {
			return nil, err
		}
		return &rtree.CallExpr{Func: "mochi_runtime::conv::float_to_int", Args: []rtree.Expr{op}}, nil
	case *aotir.StrLenExpr:
		r, err := l.lowerExpr(n.Receiver)
		if err != nil {
			return nil, err
		}
		return &rtree.CallExpr{Func: "mochi_runtime::strings::len", Args: []rtree.Expr{r}}, nil
	case *aotir.StrIndexExpr:
		r, err := l.lowerExpr(n.Receiver)
		if err != nil {
			return nil, err
		}
		i, err := l.lowerExpr(n.Index)
		if err != nil {
			return nil, err
		}
		return &rtree.CallExpr{Func: "mochi_runtime::strings::index", Args: []rtree.Expr{r, i}}, nil
	case *aotir.StrContainsExpr:
		r, err := l.lowerExpr(n.Receiver)
		if err != nil {
			return nil, err
		}
		s, err := l.lowerExpr(n.Sub)
		if err != nil {
			return nil, err
		}
		return &rtree.CallExpr{Func: "mochi_runtime::strings::contains", Args: []rtree.Expr{r, s}}, nil
	case *aotir.StrSubstringExpr:
		r, err := l.lowerExpr(n.Receiver)
		if err != nil {
			return nil, err
		}
		st, err := l.lowerExpr(n.Start)
		if err != nil {
			return nil, err
		}
		en, err := l.lowerExpr(n.End)
		if err != nil {
			return nil, err
		}
		return &rtree.CallExpr{Func: "mochi_runtime::strings::substring", Args: []rtree.Expr{r, st, en}}, nil
	case *aotir.StrReverseExpr:
		r, err := l.lowerExpr(n.Receiver)
		if err != nil {
			return nil, err
		}
		return &rtree.CallExpr{Func: "mochi_runtime::strings::reverse", Args: []rtree.Expr{r}}, nil
	case *aotir.MathCallExpr:
		op, err := l.lowerExpr(n.Arg)
		if err != nil {
			return nil, err
		}
		switch n.Func {
		case "abs_i64":
			return &rtree.MethodCall{Receiver: op, Method: "abs"}, nil
		case "abs_f64":
			return &rtree.MethodCall{Receiver: op, Method: "abs"}, nil
		case "floor":
			return &rtree.MethodCall{Receiver: op, Method: "floor"}, nil
		case "ceil":
			return &rtree.MethodCall{Receiver: op, Method: "ceil"}, nil
		}
		return nil, fmt.Errorf("rust lower: unknown math fn %s", n.Func)
	}
	return nil, fmt.Errorf("rust lower: unsupported expr %T", e)
}

func (l *lowerer) lowerBinaryExpr(b *aotir.BinaryExpr) (rtree.Expr, error) {
	left, err := l.lowerExpr(b.Left)
	if err != nil {
		return nil, err
	}
	right, err := l.lowerExpr(b.Right)
	if err != nil {
		return nil, err
	}
	switch b.Op {
	case aotir.BinStrCat:
		return &rtree.CallExpr{
			Func: "mochi_runtime::strings::cat",
			Args: []rtree.Expr{left, right},
		}, nil
	case aotir.BinEqStr, aotir.BinNeStr:
		op := rtree.OpEq
		if b.Op == aotir.BinNeStr {
			op = rtree.OpNe
		}
		return &rtree.BinaryExpr{Op: op, Left: left, Right: right}, nil
	}
	return &rtree.BinaryExpr{Op: rustBinOp(b.Op), Left: left, Right: right}, nil
}

func rustBinOp(op aotir.BinOp) rtree.BinOp {
	switch op {
	case aotir.BinAddI64, aotir.BinAddF64:
		return rtree.OpAdd
	case aotir.BinSubI64, aotir.BinSubF64:
		return rtree.OpSub
	case aotir.BinMulI64, aotir.BinMulF64:
		return rtree.OpMul
	case aotir.BinDivI64, aotir.BinDivF64:
		return rtree.OpDiv
	case aotir.BinModI64:
		return rtree.OpMod
	case aotir.BinEqI64, aotir.BinEqF64, aotir.BinEqBool, aotir.BinEqStr,
		aotir.BinEqRec, aotir.BinEqList, aotir.BinEqMap:
		return rtree.OpEq
	case aotir.BinNeI64, aotir.BinNeF64, aotir.BinNeBool, aotir.BinNeStr,
		aotir.BinNeRec, aotir.BinNeList, aotir.BinNeMap:
		return rtree.OpNe
	case aotir.BinLtI64, aotir.BinLtF64:
		return rtree.OpLt
	case aotir.BinLeI64, aotir.BinLeF64:
		return rtree.OpLe
	case aotir.BinGtI64, aotir.BinGtF64:
		return rtree.OpGt
	case aotir.BinGeI64, aotir.BinGeF64:
		return rtree.OpGe
	case aotir.BinAndBool:
		return rtree.OpAnd
	case aotir.BinOrBool:
		return rtree.OpOr
	}
	return rtree.OpAdd
}

func (l *lowerer) lowerUnaryExpr(u *aotir.UnaryExpr) (rtree.Expr, error) {
	op, err := l.lowerExpr(u.Operand)
	if err != nil {
		return nil, err
	}
	switch u.Op {
	case aotir.UnNegI64, aotir.UnNegF64:
		return &rtree.UnaryExpr{Op: "-", Operand: op}, nil
	case aotir.UnNotBool:
		return &rtree.UnaryExpr{Op: "!", Operand: op}, nil
	}
	return nil, fmt.Errorf("rust lower: unknown unary op %v", u.Op)
}

func (l *lowerer) lowerCallExpr(c *aotir.CallExpr) (rtree.Expr, error) {
	if mapped, ok := builtinValueCall(c.Func); ok {
		args := make([]rtree.Expr, 0, len(c.Args))
		for _, a := range c.Args {
			ea, err := l.lowerExpr(a)
			if err != nil {
				return nil, err
			}
			args = append(args, ea)
		}
		return &rtree.CallExpr{Func: mapped, Args: args}, nil
	}
	args := make([]rtree.Expr, 0, len(c.Args))
	for _, a := range c.Args {
		ea, err := l.lowerExpr(a)
		if err != nil {
			return nil, err
		}
		args = append(args, ea)
	}
	return &rtree.CallExpr{Func: c.Func, Args: args}, nil
}

func builtinValueCall(name string) (string, bool) {
	switch name {
	case "mochi_int_to_float", "mochi_float_to_int":
		// These are scalar casts: int_to_float -> f64 cast, float_to_int -> i64 cast.
		return "mochi_runtime::conv::" + strings.TrimPrefix(name, "mochi_"), true
	case "mochi_str_to_int":
		return "mochi_runtime::conv::str_to_int", true
	case "mochi_int_to_str":
		return "mochi_runtime::conv::int_to_str", true
	case "mochi_str_len":
		return "mochi_runtime::strings::len", true
	case "mochi_str_index":
		return "mochi_runtime::strings::index", true
	case "mochi_str_contains":
		return "mochi_runtime::strings::contains", true
	}
	return "", false
}

func rustTypeName(t aotir.Type) string {
	switch t {
	case aotir.TypeUnit:
		return "()"
	case aotir.TypeString:
		return "String"
	case aotir.TypeInt:
		return "i64"
	case aotir.TypeFloat:
		return "f64"
	case aotir.TypeBool:
		return "bool"
	}
	return "()"
}

func rustReturnType(t aotir.Type) string {
	if t == aotir.TypeUnit {
		return ""
	}
	return rustTypeName(t)
}
