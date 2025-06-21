package cppcode

import (
	"strings"

	"mochi/parser"
	"mochi/types"
)

// guessExprType returns the result type of an expression.
func (c *Compiler) guessExprType(e *parser.Expr) string {
	if e == nil || e.Binary == nil {
		return ""
	}
	return c.guessBinaryExprType(e.Binary)
}

// guessBinaryExprType infers the type of a binary expression chain.
func (c *Compiler) guessBinaryExprType(b *parser.BinaryExpr) string {
	typ := c.guessUnaryType(b.Left)
	for _, op := range b.Right {
		rtyp := c.guessPostfixType(op.Right)
		switch op.Op {
		case "+":
			if strings.HasPrefix(typ, "vector<") || strings.HasPrefix(rtyp, "vector<") {
				elem := strings.TrimSuffix(strings.TrimPrefix(typ, "vector<"), ">")
				if !strings.HasPrefix(typ, "vector<") {
					elem = strings.TrimSuffix(strings.TrimPrefix(rtyp, "vector<"), ">")
				}
				typ = "vector<" + elem + ">"
			} else if typ == "string" || rtyp == "string" {
				typ = "string"
			} else {
				typ = guessBinaryResultType(typ, op.Op, rtyp)
			}
		default:
			typ = guessBinaryResultType(typ, op.Op, rtyp)
		}
	}
	return typ
}

// guessUnaryType infers the type of a unary expression.
func (c *Compiler) guessUnaryType(u *parser.Unary) string {
	return c.guessPostfixType(u.Value)
}

// guessPostfixType infers the type of a postfix expression.
func (c *Compiler) guessPostfixType(p *parser.PostfixExpr) string {
	typ := c.guessPrimaryType(p.Target)
	for _, op := range p.Ops {
		if op.Index != nil && op.Index.Colon == nil {
			if strings.HasPrefix(typ, "vector<") {
				typ = strings.TrimSuffix(strings.TrimPrefix(typ, "vector<"), ">")
			} else if typ == "string" {
				typ = "char"
			} else if strings.HasPrefix(typ, "unordered_map<") {
				inside := strings.TrimSuffix(strings.TrimPrefix(typ, "unordered_map<"), ">")
				if parts := strings.SplitN(inside, ",", 2); len(parts) == 2 {
					typ = strings.TrimSpace(parts[1])
				} else {
					typ = "auto"
				}
			}
		}
	}
	return typ
}

// guessPrimaryType infers the type of a primary expression.
func (c *Compiler) guessPrimaryType(p *parser.Primary) string {
	switch {
	case p.Lit != nil:
		if p.Lit.Int != nil {
			return "int"
		}
		if p.Lit.Float != nil {
			return "double"
		}
		if p.Lit.Bool != nil {
			return "bool"
		}
		if p.Lit.Str != nil {
			return "string"
		}
	case p.Struct != nil:
		return p.Struct.Name
	case p.List != nil:
		if len(p.List.Elems) > 0 {
			if t := c.guessExprType(p.List.Elems[0]); t != "" {
				return "vector<" + t + ">"
			}
		}
		return "vector<int>"
	case p.Map != nil:
		keyType := "string"
		valType := "int"
		if len(p.Map.Items) > 0 {
			if t := c.guessExprType(p.Map.Items[0].Key); t != "" {
				keyType = t
			}
			if t := c.guessExprType(p.Map.Items[0].Value); t != "" {
				valType = t
			}
		}
		return "unordered_map<" + keyType + ", " + valType + ">"
	case p.Selector != nil:
		if t, ok := c.getVar(p.Selector.Root); ok {
			return t
		}
		if typ, err := c.env.GetVar(p.Selector.Root); err == nil {
			if st, ok := typ.(types.StructType); ok {
				ft := st.Fields[p.Selector.Tail[len(p.Selector.Tail)-1]]
				return c.cppTypeRef(ft)
			}
			return c.cppTypeRef(typ)
		}
	}
	return ""
}

// cppTypeRef converts a static type to a C++ type reference.
func (c *Compiler) cppTypeRef(t types.Type) string {
	switch tt := t.(type) {
	case types.IntType, types.Int64Type:
		return "int"
	case types.FloatType:
		return "double"
	case types.BoolType:
		return "bool"
	case types.StringType:
		return "string"
	case types.ListType:
		return "vector<" + c.cppTypeRef(tt.Elem) + ">"
	case types.MapType:
		return "unordered_map<" + c.cppTypeRef(tt.Key) + ", " + c.cppTypeRef(tt.Value) + ">"
	case types.StructType:
		return tt.Name
	}
	return "auto"
}

func isPrimitive(t string) bool {
	switch t {
	case "int", "double", "bool", "char":
		return true
	}
	return false
}

func getEmptyListLiteral(e *parser.Expr) *parser.ListLiteral {
	if e == nil || e.Binary == nil {
		return nil
	}
	u := e.Binary.Left
	if len(u.Ops) != 0 {
		return nil
	}
	post := u.Value
	if post == nil || post.Target == nil {
		return nil
	}
	if post.Target.List != nil && len(post.Target.List.Elems) == 0 {
		return post.Target.List
	}
	return nil
}

func isListLiteral(e *parser.Expr) bool {
	if e == nil || e.Binary == nil {
		return false
	}
	u := e.Binary.Left
	if len(u.Ops) != 0 {
		return false
	}
	post := u.Value
	return post != nil && post.Target != nil && post.Target.List != nil
}

func isStringLiteral(e *parser.Expr) bool {
	if e == nil || e.Binary == nil {
		return false
	}
	u := e.Binary.Left
	if len(u.Ops) != 0 {
		return false
	}
	post := u.Value
	return post != nil && post.Target != nil && post.Target.Lit != nil && post.Target.Lit.Str != nil
}

func getEmptyMapLiteral(e *parser.Expr) *parser.MapLiteral {
	if e == nil || e.Binary == nil {
		return nil
	}
	u := e.Binary.Left
	if len(u.Ops) != 0 {
		return nil
	}
	post := u.Value
	if post == nil || post.Target == nil {
		return nil
	}
	if post.Target.Map != nil && len(post.Target.Map.Items) == 0 {
		return post.Target.Map
	}
	return nil
}

func getStructLiteral(e *parser.Expr) *parser.StructLiteral {
	if e == nil || e.Binary == nil {
		return nil
	}
	u := e.Binary.Left
	if len(u.Ops) != 0 {
		return nil
	}
	post := u.Value
	if post == nil || post.Target == nil {
		return nil
	}
	return post.Target.Struct
}

func getCallExpr(e *parser.Expr) *parser.CallExpr {
	if e == nil || e.Binary == nil {
		return nil
	}
	u := e.Binary.Left
	if len(u.Ops) != 0 {
		return nil
	}
	post := u.Value
	if post == nil || post.Target == nil {
		return nil
	}
	return post.Target.Call
}

func selectorName(e *parser.Expr) (string, bool) {
	if e == nil || e.Binary == nil {
		return "", false
	}
	u := e.Binary.Left
	if len(u.Ops) != 0 {
		return "", false
	}
	post := u.Value
	if post == nil || post.Target == nil || post.Target.Selector == nil {
		return "", false
	}
	if len(post.Target.Selector.Tail) != 0 {
		return "", false
	}
	return post.Target.Selector.Root, true
}

// guessBinaryResultType returns the resulting type of applying op between l and r.
func guessBinaryResultType(ltyp, op, rtyp string) string {
	switch op {
	case "+", "-", "*", "/", "%":
		if ltyp == "double" || rtyp == "double" {
			return "double"
		}
		if ltyp == "int" && rtyp == "int" {
			return "int"
		}
		return ltyp
	case "==", "!=", "<", ">", "<=", ">=", "&&", "||", "in":
		return "bool"
	}
	return ltyp
}
