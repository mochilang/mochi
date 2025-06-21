package ccode

import (
	"mochi/parser"
	"mochi/types"
)

func (c *Compiler) inferExprType(e *parser.Expr) types.Type {
	if e == nil {
		return types.AnyType{}
	}
	return c.inferBinaryType(e.Binary)
}

func (c *Compiler) inferBinaryType(b *parser.BinaryExpr) types.Type {
	if b == nil {
		return types.AnyType{}
	}
	t := c.inferUnaryType(b.Left)
	for _, op := range b.Right {
		rt := c.inferPostfixType(op.Right)
		switch op.Op {
               case "+", "-", "*", "/", "%", "union", "except", "intersect":
                        if isNumber(t) && isNumber(rt) {
                                if isFloat(t) || isFloat(rt) {
                                        t = types.FloatType{}
                                } else {
                                        t = types.IntType{}
                                }
                                continue
                        }
                        if op.Op == "+" || op.Op == "union" || op.Op == "except" || op.Op == "intersect" {
                                if llist, ok := t.(types.ListType); ok {
                                        if rlist, ok := rt.(types.ListType); ok && equalTypes(llist.Elem, rlist.Elem) {
                                                t = llist
                                                continue
                                        }
                                }
                                if isString(t) && isString(rt) {
                                        t = types.StringType{}
                                        continue
                                }
                        }
                        t = types.AnyType{}
                case "==", "!=", "<", "<=", ">", ">=":
                        t = types.BoolType{}
               case "&&", "||":
                       if _, ok := t.(types.BoolType); ok {
                               if _, ok2 := rt.(types.BoolType); ok2 {
                                       t = types.BoolType{}
                                       continue
                               }
                       }
                       t = types.AnyType{}
                default:
                        t = types.AnyType{}
                }
	}
	return t
}

func (c *Compiler) inferUnaryType(u *parser.Unary) types.Type {
        if u == nil {
                return types.AnyType{}
        }
        t := c.inferPostfixType(u.Value)
        for i := len(u.Ops) - 1; i >= 0; i-- {
                op := u.Ops[i]
                switch op {
                case "!":
                        t = types.BoolType{}
                case "-":
                        if !isNumber(t) {
                                t = types.AnyType{}
                        }
                }
        }
        return t
}

func (c *Compiler) inferPostfixType(p *parser.PostfixExpr) types.Type {
	if p == nil {
		return types.AnyType{}
	}
	t := c.inferPrimaryType(p.Target)
	for _, op := range p.Ops {
		if op.Index != nil && op.Index.Colon == nil {
			switch tt := t.(type) {
			case types.ListType:
				t = tt.Elem
			case types.StringType:
				t = types.StringType{}
			default:
				t = types.AnyType{}
			}
		} else if op.Index != nil {
			switch tt := t.(type) {
			case types.ListType:
				t = tt
			case types.StringType:
				t = types.StringType{}
			default:
				t = types.AnyType{}
			}
		} else if op.Call != nil {
			if ft, ok := t.(types.FuncType); ok {
				t = ft.Return
			} else {
				t = types.AnyType{}
			}
		} else if op.Cast != nil {
			t = resolveTypeRef(op.Cast.Type, c.env)
		}
	}
	return t
}

func (c *Compiler) inferPrimaryType(p *parser.Primary) types.Type {
	if p == nil {
		return types.AnyType{}
	}
	switch {
	case p.Lit != nil:
		switch {
		case p.Lit.Int != nil:
			return types.IntType{}
		case p.Lit.Float != nil:
			return types.FloatType{}
		case p.Lit.Str != nil:
			return types.StringType{}
		case p.Lit.Bool != nil:
			return types.BoolType{}
		}
	case p.Selector != nil:
		if c.env != nil {
			if t, err := c.env.GetVar(p.Selector.Root); err == nil {
				if len(p.Selector.Tail) == 0 {
					return t
				}
				if st, ok := t.(types.StructType); ok {
					cur := st
					for idx, field := range p.Selector.Tail {
						ft, ok := cur.Fields[field]
						if !ok {
							return types.AnyType{}
						}
						if idx == len(p.Selector.Tail)-1 {
							return ft
						}
						if next, ok := ft.(types.StructType); ok {
							cur = next
						} else {
							return types.AnyType{}
						}
					}
				}
			}
		}
		return types.AnyType{}
	case p.Struct != nil:
		if c.env != nil {
			if st, ok := c.env.GetStruct(p.Struct.Name); ok {
				return st
			}
		}
		return types.AnyType{}
	case p.Call != nil:
		switch p.Call.Func {
		case "len", "count":
			return types.IntType{}
		case "str", "input":
			return types.StringType{}
		case "avg":
			return types.FloatType{}
		default:
			if c.env != nil {
				if t, err := c.env.GetVar(p.Call.Func); err == nil {
					if ft, ok := t.(types.FuncType); ok {
						return ft.Return
					}
				}
			}
			return types.AnyType{}
		}
       case p.If != nil:
               thenType := c.inferExprType(p.If.Then)
               var elseType types.Type = types.AnyType{}
               if p.If.ElseIf != nil {
                       elseType = c.inferPrimaryType(&parser.Primary{If: p.If.ElseIf})
               } else if p.If.Else != nil {
                       elseType = c.inferExprType(p.If.Else)
               }
		if equalTypes(thenType, elseType) {
			return thenType
		}
		if isNumber(thenType) && isNumber(elseType) {
			if isFloat(thenType) || isFloat(elseType) {
				return types.FloatType{}
			}
			return types.IntType{}
		}
		if _, ok := thenType.(types.StringType); ok {
			if _, ok2 := elseType.(types.StringType); ok2 {
				return types.StringType{}
			}
		}
		if lt1, ok1 := thenType.(types.ListType); ok1 {
			if lt2, ok2 := elseType.(types.ListType); ok2 && equalTypes(lt1.Elem, lt2.Elem) {
				return lt1
			}
		}
               if _, ok := thenType.(types.BoolType); ok {
                       if _, ok2 := elseType.(types.BoolType); ok2 {
                               return types.BoolType{}
                       }
               }
               return types.AnyType{}
       case p.Match != nil:
               var result types.Type = types.AnyType{}
               for i, mc := range p.Match.Cases {
                       t := c.inferExprType(mc.Result)
                       if i == 0 {
                               result = t
                               continue
                       }
                       if equalTypes(result, t) {
                               continue
                       }
                       if isNumber(result) && isNumber(t) {
                               if isFloat(result) || isFloat(t) {
                                       result = types.FloatType{}
                               } else {
                                       result = types.IntType{}
                               }
                               continue
                       }
                       if _, ok := result.(types.StringType); ok {
                               if _, ok2 := t.(types.StringType); ok2 {
                                       result = types.StringType{}
                                       continue
                               }
                       }
                       if lt1, ok1 := result.(types.ListType); ok1 {
                               if lt2, ok2 := t.(types.ListType); ok2 && equalTypes(lt1.Elem, lt2.Elem) {
                                       result = lt1
                                       continue
                               }
                       }
                       if _, ok := result.(types.BoolType); ok {
                               if _, ok2 := t.(types.BoolType); ok2 {
                                       result = types.BoolType{}
                                       continue
                               }
                       }
                       result = types.AnyType{}
               }
               return result
       case p.Group != nil:
               return c.inferExprType(p.Group)
	case p.List != nil:
		var elemType types.Type = types.AnyType{}
		if len(p.List.Elems) > 0 {
			elemType = c.inferExprType(p.List.Elems[0])
			for _, e := range p.List.Elems[1:] {
				t := c.inferExprType(e)
				if !equalTypes(elemType, t) {
					elemType = types.AnyType{}
					break
				}
			}
		}
		return types.ListType{Elem: elemType}
	}
	return types.AnyType{}
}
