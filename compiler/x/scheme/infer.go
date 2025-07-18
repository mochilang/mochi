//go:build slow

package schemecode

import (
	"mochi/parser"
	"mochi/types"
)

func (c *Compiler) varType(name string) string {
	if t, ok := c.vars[name]; ok {
		return t
	}
	if c.env != nil {
		if tt, err := c.env.GetVar(name); err == nil {
			switch tt.(type) {
			case types.StringType:
				return "string"
			case types.MapType:
				return "map"
			case types.StructType:
				return "map"
			case types.IntType, types.Int64Type:
				return "int"
			case types.FloatType:
				return "float"
			}
		}
	}
	return ""
}

// fieldType attempts to resolve the static type of a field access rooted at
// the given variable. It returns one of "string", "int", "float", "map" or
// the empty string when the type cannot be determined.
func (c *Compiler) fieldType(root string, fields []string) string {
	if c.env == nil || root == "" || len(fields) == 0 {
		return ""
	}
	t, err := c.env.GetVar(root)
	if err != nil {
		return ""
	}
	for i, f := range fields {
		st, ok := t.(types.StructType)
		if !ok {
			return ""
		}
		ft, ok := st.Fields[f]
		if !ok {
			return ""
		}
		if i == len(fields)-1 {
			switch ft.(type) {
			case types.StringType:
				return "string"
			case types.IntType, types.Int64Type:
				return "int"
			case types.FloatType:
				return "float"
			case types.MapType, types.StructType:
				return "map"
			default:
				return ""
			}
		}
		t = ft
	}
	return ""
}

func (c *Compiler) isStringExpr(e *parser.Expr) bool {
	if types.IsStringExpr(e, c.env) {
		return true
	}
	root := rootNameExpr(e)
	if c.varType(root) == "string" {
		return true
	}
	if e != nil && e.Binary != nil && len(e.Binary.Right) == 0 {
		u := e.Binary.Left
		if len(u.Ops) == 0 && u.Value != nil && u.Value.Target != nil && u.Value.Target.Call != nil {
			fn := u.Value.Target.Call.Func
			if fn == "str" || fn == "input" {
				return true
			}
		}
	}
	return false
}

func (c *Compiler) isStringUnary(u *parser.Unary) bool {
	if types.IsStringUnary(u, c.env) {
		return true
	}
	root := rootNameUnary(u)
	return c.varType(root) == "string"
}

func (c *Compiler) isStringPostfix(p *parser.PostfixExpr) bool {
	if types.IsStringPostfix(p, c.env) {
		return true
	}
	root := rootNamePostfix(p)
	return c.varType(root) == "string"
}

func (c *Compiler) isStringPrimary(p *parser.Primary) bool {
	if types.IsStringPrimary(p, c.env) {
		return true
	}
	if p != nil && p.Selector != nil {
		if c.varType(p.Selector.Root) == "string" {
			return true
		}
		if t := c.fieldType(p.Selector.Root, p.Selector.Tail); t == "string" {
			return true
		}
		return false
	}
	return false
}

func (c *Compiler) isMapExpr(e *parser.Expr) bool {
	if types.IsMapExpr(e, c.env) {
		return true
	}
	root := rootNameExpr(e)
	return c.varType(root) == "map"
}

func (c *Compiler) isMapUnary(u *parser.Unary) bool {
	if types.IsMapUnary(u, c.env) {
		return true
	}
	root := rootNameUnary(u)
	return c.varType(root) == "map"
}

func (c *Compiler) isMapPostfix(p *parser.PostfixExpr) bool {
	if types.IsMapPostfix(p, c.env) {
		return true
	}
	root := rootNamePostfix(p)
	return c.varType(root) == "map"
}

func (c *Compiler) isMapPrimary(p *parser.Primary) bool {
	if types.IsMapPrimary(p, c.env) {
		return true
	}
	if p != nil && p.Selector != nil {
		if c.varType(p.Selector.Root) == "map" {
			return true
		}
		if t := c.fieldType(p.Selector.Root, p.Selector.Tail); t == "map" {
			return true
		}
		return false
	}
	return false
}

func (c *Compiler) isListExpr(e *parser.Expr) bool {
	if e == nil {
		return false
	}
	if name, ok := identName(e); ok && c.env != nil {
		if t, err := c.env.GetVar(name); err == nil {
			if _, ok := t.(types.ListType); ok {
				return true
			}
		}
	}
	if len(e.Binary.Right) == 0 {
		u := e.Binary.Left
		if len(u.Ops) == 0 {
			p := u.Value
			if len(p.Ops) == 0 && p.Target.List != nil {
				return true
			}
		}
	}
	return false
}

// numericBinary examines a binary expression and reports whether all operands
// are statically numeric. It returns three values: allInt indicates that every
// operand is an int, hasFloat indicates at least one operand is a float and ok
// is true when the expression only contains arithmetic operators.
func (c *Compiler) numericBinary(b *parser.BinaryExpr) (allInt bool, hasFloat bool, ok bool) {
	if b == nil {
		return false, false, false
	}
	ops := []string{"+", "-", "*", "/", "%"}
	allInt = c.isIntUnary(b.Left)
	hasFloat = c.isFloatUnary(b.Left)
	numeric := allInt || hasFloat
	for _, part := range b.Right {
		if !contains(ops, part.Op) {
			return false, false, false
		}
		i := c.isIntPostfix(part.Right)
		f := c.isFloatPostfix(part.Right)
		if !i {
			allInt = false
		}
		if f {
			hasFloat = true
		}
		numeric = numeric || i || f
	}
	if !numeric {
		return false, false, false
	}
	return allInt, hasFloat, true
}

func (c *Compiler) isFloatExpr(e *parser.Expr) bool {
	if types.IsFloatExpr(e, c.env) {
		return true
	}
	root := rootNameExpr(e)
	if root != "" && c.env != nil {
		if t, err := c.env.GetVar(root); err == nil {
			if _, ok := t.(types.FloatType); ok {
				return true
			}
		}
	}
	if len(e.Binary.Right) == 0 {
		u := e.Binary.Left
		if len(u.Ops) == 1 && u.Ops[0] == "-" {
			if c.isFloatPostfix(u.Value) {
				return true
			}
		}
	} else if allInt, hasFloat, ok := c.numericBinary(e.Binary); ok {
		if hasFloat {
			return true
		}
		if !allInt {
			return true
		}
	}
	return false
}

func (c *Compiler) isIntExpr(e *parser.Expr) bool {
	if e == nil {
		return false
	}
	if name, ok := identName(e); ok {
		if t, ok2 := c.vars[name]; ok2 && t == "int" {
			return true
		}
		if c.env != nil {
			if tt, err := c.env.GetVar(name); err == nil {
				if _, ok := tt.(types.IntType); ok {
					return true
				}
			}
		}
	}
	if len(e.Binary.Right) == 0 {
		u := e.Binary.Left
		if len(u.Ops) == 0 && u.Value != nil {
			p := u.Value
			if len(p.Ops) == 0 && p.Target != nil && p.Target.Lit != nil && p.Target.Lit.Int != nil {
				return true
			}
		}
		if len(u.Ops) == 1 && u.Ops[0] == "-" {
			if c.isIntPostfix(u.Value) {
				return true
			}
		}
	} else if allInt, hasFloat, ok := c.numericBinary(e.Binary); ok {
		if allInt && !hasFloat {
			return true
		}
	}
	return false
}

func (c *Compiler) isIntUnary(u *parser.Unary) bool {
	if u == nil {
		return false
	}
	if c.isIntPostfix(u.Value) {
		return true
	}
	root := rootNameUnary(u)
	if root != "" {
		if t, ok := c.vars[root]; ok && t == "int" {
			return true
		}
		if c.env != nil {
			if tt, err := c.env.GetVar(root); err == nil {
				if _, ok := tt.(types.IntType); ok {
					return true
				}
			}
		}
	}
	return false
}

func (c *Compiler) isIntPostfix(p *parser.PostfixExpr) bool {
	if p == nil {
		return false
	}
	if c.isIntPrimary(p.Target) {
		return true
	}
	for _, op := range p.Ops {
		if op.Cast != nil && op.Cast.Type != nil && op.Cast.Type.Simple != nil && *op.Cast.Type.Simple == "int" {
			return true
		}
	}
	root := rootNamePostfix(p)
	if root != "" {
		if t, ok := c.vars[root]; ok && t == "int" {
			return true
		}
		if c.env != nil {
			if tt, err := c.env.GetVar(root); err == nil {
				if _, ok := tt.(types.IntType); ok {
					return true
				}
			}
		}
	}
	return false
}

func (c *Compiler) isIntPrimary(p *parser.Primary) bool {
	if p == nil {
		return false
	}
	switch {
	case p.Lit != nil && p.Lit.Int != nil:
		return true
	case p.Selector != nil:
		if t, ok := c.vars[p.Selector.Root]; ok && t == "int" {
			return true
		}
		if c.env != nil {
			if tt, err := c.env.GetVar(p.Selector.Root); err == nil {
				if _, ok := tt.(types.IntType); ok {
					return true
				}
			}
		}
		if ft := c.fieldType(p.Selector.Root, p.Selector.Tail); ft == "int" {
			return true
		}
	case p.Call != nil && c.env != nil:
		if t, err := c.env.GetVar(p.Call.Func); err == nil {
			if ft, ok := t.(types.FuncType); ok {
				if _, ok2 := ft.Return.(types.IntType); ok2 {
					return true
				}
			}
		}
	case p.Group != nil:
		return c.isIntExpr(p.Group)
	}
	return false
}

func (c *Compiler) isFloatUnary(u *parser.Unary) bool {
	if u == nil {
		return false
	}
	if c.isFloatPostfix(u.Value) {
		return true
	}
	root := rootNameUnary(u)
	if root != "" && c.env != nil {
		if t, err := c.env.GetVar(root); err == nil {
			if _, ok := t.(types.FloatType); ok {
				return true
			}
		}
	}
	return false
}

func (c *Compiler) isFloatPostfix(p *parser.PostfixExpr) bool {
	if p == nil {
		return false
	}
	if c.isFloatPrimary(p.Target) {
		return true
	}
	for _, op := range p.Ops {
		if op.Cast != nil {
			if op.Cast.Type != nil && op.Cast.Type.Simple != nil && *op.Cast.Type.Simple == "float" {
				return true
			}
		}
	}
	root := rootNamePostfix(p)
	if root != "" && c.env != nil {
		if t, err := c.env.GetVar(root); err == nil {
			if _, ok := t.(types.FloatType); ok {
				return true
			}
		}
	}
	return false
}

func (c *Compiler) isFloatPrimary(p *parser.Primary) bool {
	if p == nil {
		return false
	}
	switch {
	case p.Lit != nil && p.Lit.Float != nil:
		return true
	case p.Selector != nil && c.env != nil:
		if t, err := c.env.GetVar(p.Selector.Root); err == nil {
			if _, ok := t.(types.FloatType); ok {
				return true
			}
		}
		if ft := c.fieldType(p.Selector.Root, p.Selector.Tail); ft == "float" {
			return true
		}
	case p.Call != nil && c.env != nil:
		if t, err := c.env.GetVar(p.Call.Func); err == nil {
			if ft, ok := t.(types.FuncType); ok {
				if _, ok2 := ft.Return.(types.FloatType); ok2 {
					return true
				}
			}
		}
	case p.Group != nil:
		return c.isFloatExpr(p.Group)
	case p.List != nil:
		for _, e := range p.List.Elems {
			if c.isFloatExpr(e) {
				return true
			}
		}
	}
	return false
}

// isNumericListExpr returns true if e is a list whose elements are
// statically known to be numbers (ints or floats).
func (c *Compiler) isNumericListExpr(e *parser.Expr) bool {
	if !c.isListExpr(e) {
		return false
	}
	if name, ok := identName(e); ok && c.env != nil {
		if t, err := c.env.GetVar(name); err == nil {
			if lt, ok := t.(types.ListType); ok {
				switch lt.Elem.(type) {
				case types.IntType, types.Int64Type, types.FloatType:
					return true
				}
			}
		}
	}
	if e.Binary != nil && len(e.Binary.Right) == 0 {
		u := e.Binary.Left
		if len(u.Ops) == 0 && u.Value != nil {
			p := u.Value
			if len(p.Ops) == 0 && p.Target.List != nil {
				lst := p.Target.List.Elems
				if len(lst) == 0 {
					return true
				}
				ok := true
				for _, el := range lst {
					if !(c.isIntExpr(el) || c.isFloatExpr(el)) {
						ok = false
						break
					}
				}
				return ok
			}
		}
	}
	return false
}
