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
	}
	return nil, fmt.Errorf("rust lower: unsupported stmt %T", s)
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
	}
	return nil, fmt.Errorf("rust lower: unsupported expr %T", e)
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
