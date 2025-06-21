package cobolcode

import (
	"fmt"
	"mochi/ast"
	"mochi/types"
)

// inferType attempts to determine the static type of n using the
// environment when available. It only handles a small subset of
// expressions sufficient for the COBOL backend.
func (c *Compiler) inferType(n *ast.Node) types.Type {
	if n == nil {
		return types.AnyType{}
	}
	switch n.Kind {
	case "int":
		return types.IntType{}
	case "float":
		return types.FloatType{}
	case "string":
		return types.StringType{}
	case "bool":
		return types.BoolType{}
	case "list":
		var elem types.Type = types.AnyType{}
		if len(n.Children) > 0 {
			elem = c.inferType(n.Children[0])
		}
		return types.ListType{Elem: elem}
	case "selector":
		if c.env != nil {
			if t, err := c.env.GetVar(n.Value.(string)); err == nil {
				return t
			}
		}
		return types.AnyType{}
	case "call":
		if c.env != nil {
			if t, err := c.env.GetVar(n.Value.(string)); err == nil {
				if ft, ok := t.(types.FuncType); ok {
					return ft.Return
				}
			}
		}
		return types.AnyType{}
	case "binary":
		lt := c.inferType(n.Children[0])
		rt := c.inferType(n.Children[1])
		op := n.Value.(string)
		switch op {
		case "+", "-", "*", "/", "%":
			if isFloat(lt) || isFloat(rt) {
				return types.FloatType{}
			}
			if isInt(lt) && isInt(rt) {
				return types.IntType{}
			}
			if op == "+" && isString(lt) && isString(rt) {
				return types.StringType{}
			}
			if op == "+" && isList(lt) && isList(rt) {
				llt := lt.(types.ListType)
				rlt := rt.(types.ListType)
				if equalTypes(llt.Elem, rlt.Elem) {
					return llt
				}
			}
			return types.AnyType{}
		case "==", "!=", "<", "<=", ">", ">=", "&&", "||":
			return types.BoolType{}
		}
	case "group":
		return c.inferType(n.Children[0])
	case "unary":
		return c.inferType(n.Children[0])
	case "index":
		base := c.inferType(n.Children[0])
		if lt, ok := base.(types.ListType); ok {
			if len(n.Children) > 1 && (n.Children[1].Kind == "start" || n.Children[1].Kind == "end" || len(n.Children) > 2) {
				return lt
			}
			return lt.Elem
		}
		if isString(base) {
			return types.StringType{}
		}
	case "if_expr":
		thenT := c.inferType(n.Children[1])
		if len(n.Children) > 2 {
			elseT := c.inferType(n.Children[2])
			if equalTypes(thenT, elseT) {
				return thenT
			}
			return types.AnyType{}
		}
		return thenT
	case "match":
		var t types.Type
		for _, cs := range n.Children[1:] {
			r := c.inferType(cs.Children[1])
			if t == nil {
				t = r
				continue
			}
			if !equalTypes(t, r) {
				t = types.AnyType{}
			}
		}
		if t == nil {
			return types.AnyType{}
		}
		return t
	}
	return types.AnyType{}
}

func (c *Compiler) isStringExpr(n *ast.Node) bool {
	_, ok := c.inferType(n).(types.StringType)
	return ok
}

func (c *Compiler) isFloatExpr(n *ast.Node) bool {
	_, ok := c.inferType(n).(types.FloatType)
	return ok
}

func (c *Compiler) isListExpr(n *ast.Node) bool {
	_, ok := c.inferType(n).(types.ListType)
	return ok
}

func (c *Compiler) picForExpr(n *ast.Node) string {
	switch c.inferType(n).(type) {
	case types.StringType:
		return "PIC X(100)."
	case types.FloatType:
		return "PIC 9(4)V9(4)."
	case types.IntType:
		if n.Kind == "int" {
			v := extractInt(n)
			if v < 0 {
				v = -v
			}
			width := 1
			for v >= 10 {
				width++
				v /= 10
			}
			if width > 1 {
				return fmt.Sprintf("PIC 9(%d).", width)
			}
		}
		return "PIC 9."
	default:
		return "PIC 9."
	}
}

func (c *Compiler) picForVar(name string) string {
	if c.env != nil {
		if t, err := c.env.GetVar(name); err == nil {
			switch t.(type) {
			case types.StringType:
				return "PIC X(100)."
			case types.FloatType:
				return "PIC 9(4)V9(4)."
			}
		}
	}
	return "PIC 9."
}
