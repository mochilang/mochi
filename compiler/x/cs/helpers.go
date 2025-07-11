//go:build slow

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
	case types.IntType:
		return "int"
	case types.Int64Type:
		return "long"
	case types.FloatType:
		return "double"
	case types.StringType:
		return "string"
	case types.BoolType:
		return "bool"
	case types.ListType:
		return fmt.Sprintf("List<%s>", csTypeOf(tt.Elem))
	case types.MapType:
		return fmt.Sprintf("Dictionary<%s, %s>", csTypeOf(tt.Key), csTypeOf(tt.Value))
	case types.StructType:
		name := sanitizeName(tt.Name)
		if name == "" {
			return "dynamic"
		}
		return name
	case types.UnionType:
		name := sanitizeName(tt.Name)
		if name == "" {
			return "dynamic"
		}
		return name
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

func isInt(t types.Type) bool     { _, ok := t.(types.IntType); return ok }
func isInt64(t types.Type) bool   { _, ok := t.(types.Int64Type); return ok }
func isFloat(t types.Type) bool   { _, ok := t.(types.FloatType); return ok }
func isNumeric(t types.Type) bool { return isInt(t) || isInt64(t) || isFloat(t) }

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

// exprAliases returns the set of root identifiers referenced in e.
func exprAliases(e *parser.Expr) map[string]struct{} {
	set := map[string]struct{}{}
	collectAliases(e, set)
	return set
}

func collectAliases(e *parser.Expr, out map[string]struct{}) {
	if e == nil {
		return
	}
	if e.Binary == nil {
		return
	}
	collectUnaryAliases(e.Binary.Left, out)
	for _, op := range e.Binary.Right {
		collectPostfixAliases(op.Right, out)
	}
}

func collectUnaryAliases(u *parser.Unary, out map[string]struct{}) {
	if u == nil {
		return
	}
	collectPostfixAliases(u.Value, out)
}

func collectPostfixAliases(p *parser.PostfixExpr, out map[string]struct{}) {
	if p == nil {
		return
	}
	collectPrimaryAliases(p.Target, out)
	for _, op := range p.Ops {
		if op.Call != nil {
			for _, a := range op.Call.Args {
				collectAliases(a, out)
			}
		}
		if op.Index != nil {
			if op.Index.Start != nil {
				collectAliases(op.Index.Start, out)
			}
			if op.Index.End != nil {
				collectAliases(op.Index.End, out)
			}
			if op.Index.Step != nil {
				collectAliases(op.Index.Step, out)
			}
		}
	}
}

func collectPrimaryAliases(p *parser.Primary, out map[string]struct{}) {
	if p == nil {
		return
	}
	if p.Selector != nil {
		out[p.Selector.Root] = struct{}{}
	}
	if p.Group != nil {
		collectAliases(p.Group, out)
	}
}

// splitAnd returns a slice of expressions separated by logical AND operators.
func splitAnd(e *parser.Expr) []*parser.Expr {
	if e == nil || e.Binary == nil {
		return nil
	}
	parts := []*parser.Expr{}
	left := e.Binary.Left
	ops := []*parser.BinaryOp{}
	for _, op := range e.Binary.Right {
		if op.Op == "&&" {
			exp := &parser.Expr{Binary: &parser.BinaryExpr{Left: left, Right: ops}}
			parts = append(parts, exp)
			left = &parser.Unary{Value: op.Right}
			ops = nil
			continue
		}
		ops = append(ops, op)
	}
	parts = append(parts, &parser.Expr{Binary: &parser.BinaryExpr{Left: left, Right: ops}})
	return parts
}

// isFetchExpr reports whether e is a plain fetch expression with no postfix operations.
func isFetchExpr(e *parser.Expr) bool {
	if e == nil || e.Binary == nil {
		return false
	}
	if len(e.Binary.Right) != 0 {
		return false
	}
	u := e.Binary.Left
	if u == nil || u.Value == nil {
		return false
	}
	p := u.Value.Target
	if p == nil {
		return false
	}
	return p.Fetch != nil
}
