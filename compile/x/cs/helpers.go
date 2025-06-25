package cscode

import (
	"fmt"
	"os"
	"path/filepath"
	"strings"

	"mochi/parser"
	"mochi/types"
)

func delegateType(params []string, ret string) string {
	if ret == "" || ret == "void" {
		if len(params) == 0 {
			return "Action"
		}
		return fmt.Sprintf("Action<%s>", strings.Join(params, ", "))
	}
	generics := append(params, ret)
	return fmt.Sprintf("Func<%s>", strings.Join(generics, ", "))
}

func repoRoot() string {
	dir, _ := os.Getwd()
	for i := 0; i < 10; i++ {
		if _, err := os.Stat(filepath.Join(dir, "go.mod")); err == nil {
			return dir
		}
		parent := filepath.Dir(dir)
		if parent == dir {
			break
		}
		dir = parent
	}
	return ""
}

func csTypeOf(t types.Type) string {
	switch tt := t.(type) {
	case types.IntType, types.Int64Type:
		return "long"
	case types.FloatType:
		return "double"
	case types.StringType:
		return "string"
	case types.BoolType:
		return "bool"
	case types.ListType:
		return csTypeOf(tt.Elem) + "[]"
	case types.MapType:
		return fmt.Sprintf("Dictionary<%s, %s>", csTypeOf(tt.Key), csTypeOf(tt.Value))
	case types.StructType:
		return sanitizeName(tt.Name)
	case types.UnionType:
		return sanitizeName(tt.Name)
	case types.FuncType:
		params := make([]string, len(tt.Params))
		for i, p := range tt.Params {
			params[i] = csTypeOf(p)
		}
		ret := csTypeOf(tt.Return)
		return delegateType(params, ret)
	case types.VoidType:
		return "void"
	case types.AnyType:
		return "dynamic"
	default:
		return "dynamic"
	}
}

func isAny(t types.Type) bool { _, ok := t.(types.AnyType); return ok }

func contains(sl []string, s string) bool {
	for _, v := range sl {
		if v == s {
			return true
		}
	}
	return false
}

func (c *Compiler) resolveTypeRef(t *parser.TypeRef) types.Type {
	return types.ResolveTypeRef(t, c.env)
}
func exprVars(e *parser.Expr, vars map[string]struct{}) {
	if e == nil {
		return
	}
	unaryVars(e.Binary.Left, vars)
	for _, op := range e.Binary.Right {
		postfixVars(op.Right, vars)
	}
}

func unaryVars(u *parser.Unary, vars map[string]struct{}) {
	if u == nil {
		return
	}
	postfixVars(u.Value, vars)
}

func postfixVars(p *parser.PostfixExpr, vars map[string]struct{}) {
	if p == nil {
		return
	}
	primaryVars(p.Target, vars)
	for _, op := range p.Ops {
		if op.Index != nil {
			exprVars(op.Index.Start, vars)
			exprVars(op.Index.End, vars)
		}
		if op.Call != nil {
			for _, a := range op.Call.Args {
				exprVars(a, vars)
			}
		}
	}
}

func primaryVars(p *parser.Primary, vars map[string]struct{}) {
	if p == nil {
		return
	}
	switch {
	case p.Selector != nil:
		vars[p.Selector.Root] = struct{}{}
	case p.Group != nil:
		exprVars(p.Group, vars)
	case p.FunExpr != nil:
		exprVars(p.FunExpr.ExprBody, vars)
		for _, st := range p.FunExpr.BlockBody {
			// nested statements not needed for simple conds
			_ = st // ignore
		}
	case p.List != nil:
		for _, e := range p.List.Elems {
			exprVars(e, vars)
		}
	case p.Map != nil:
		for _, it := range p.Map.Items {
			exprVars(it.Key, vars)
			exprVars(it.Value, vars)
		}
	case p.Call != nil:
		for _, a := range p.Call.Args {
			exprVars(a, vars)
		}
	case p.Query != nil:
		exprVars(p.Query.Source, vars)
		for _, f := range p.Query.Froms {
			exprVars(f.Src, vars)
		}
		for _, j := range p.Query.Joins {
			exprVars(j.Src, vars)
			exprVars(j.On, vars)
		}
		exprVars(p.Query.Where, vars)
		if p.Query.Group != nil {
			exprVars(p.Query.Group.Expr, vars)
		}
		exprVars(p.Query.Sort, vars)
		exprVars(p.Query.Skip, vars)
		exprVars(p.Query.Take, vars)
		exprVars(p.Query.Select, vars)
	}
}

func varsForExpr(e *parser.Expr) map[string]struct{} {
	m := map[string]struct{}{}
	exprVars(e, m)
	return m
}
