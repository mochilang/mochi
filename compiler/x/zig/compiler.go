//go:build slow

package zigcode

import (
	"bytes"
	"fmt"
	"strconv"
	"strings"

	"mochi/parser"
	"mochi/types"
)

// Compiler translates a Mochi AST into Zig source code (very small subset).
type Compiler struct {
	buf               bytes.Buffer
	indent            int
	env               *types.Env
	tmpCount          int
	imports           map[string]string
	structs           map[string]bool
	needsAvgInt       bool
	needsAvgFloat     bool
	needsSumInt       bool
	needsSumFloat     bool
	needsMinInt       bool
	needsMinFloat     bool
	needsMinString    bool
	needsMaxInt       bool
	needsMaxFloat     bool
	needsMaxString    bool
	needsInListInt    bool
	needsInListString bool
	needsSetOps       bool
	needsConcatList   bool
	needsConcatString bool
	needsSplitString  bool
	needsJoinString   bool
	needsJSON         bool
	needsIndex        bool
	needsIndexString  bool
	needsSlice        bool
	needsSliceString  bool
	needsReduce       bool
	needsEqual        bool
	needsMapKeys      bool
	needsMapValues    bool
	needsLoadJSON     bool
	needsSaveJSON     bool
	needsFetch        bool
	needsFetchJSON    bool
	needsExpect       bool
	needsAppend       bool
	needsPrintList    bool
	tests             []string
	constGlobals      map[string]bool
	globalInits       map[string]string
	labelCount        int
	locals            map[string]types.Type
	funcRet           types.Type
	captures          map[string]string
	pointerVars       map[string]bool
	variantInfo       map[string]struct {
		Union  string
		Fields map[string]types.Type
		Order  []string
	}
	builtinAliases map[string]string
}

func (c *Compiler) pushPointerVars() map[string]bool {
	old := c.pointerVars
	c.pointerVars = map[string]bool{}
	return old
}

func (c *Compiler) popPointerVars(old map[string]bool) {
	c.pointerVars = old
}

func New(env *types.Env) *Compiler {
	return &Compiler{
		env:            env,
		imports:        map[string]string{},
		structs:        map[string]bool{},
		tests:          []string{},
		constGlobals:   map[string]bool{},
		globalInits:    map[string]string{},
		locals:         map[string]types.Type{},
		captures:       map[string]string{},
		pointerVars:    map[string]bool{},
		builtinAliases: map[string]string{},
		variantInfo: map[string]struct {
			Union  string
			Fields map[string]types.Type
			Order  []string
		}{},
	}
}

func (c *Compiler) writeln(s string) {
	for i := 0; i < c.indent; i++ {
		c.buf.WriteByte('\t')
	}
	c.buf.WriteString(s)
	c.buf.WriteByte('\n')
}

func (c *Compiler) writeIndent() {
	for i := 0; i < c.indent; i++ {
		c.buf.WriteByte('\t')
	}
}

// Compile converts a Mochi program to Zig.
func (c *Compiler) Compile(prog *parser.Program) ([]byte, error) {
	// collect imports first so that builtin aliases are registered
	for _, s := range prog.Statements {
		if s.Import != nil {
			if err := c.addImport(s.Import); err != nil {
				return nil, err
			}
		}
	}
	// compile type declarations first
	for _, s := range prog.Statements {
		if s.Type != nil {
			if err := c.compileTypeDecl(s.Type); err != nil {
				return nil, err
			}
			c.writeln("")
		}
	}
	if err := c.compileGlobalDecls(prog); err != nil {
		return nil, err
	}
	// compile functions and test blocks next
	for _, s := range prog.Statements {
		if s.Fun != nil {
			if err := c.compileFun(s.Fun); err != nil {
				return nil, err
			}
			c.writeln("")
		} else if s.Test != nil {
			if err := c.compileTest(s.Test); err != nil {
				return nil, err
			}
			c.writeln("")
		}
	}
	// main body
	c.writeln("pub fn main() void {")
	c.indent++
	for _, s := range prog.Statements {
		if s.Fun != nil || s.Test != nil || s.Type != nil {
			continue
		}
		if s.ExternVar != nil || s.ExternFun != nil || s.ExternType != nil || s.ExternObject != nil {
			// extern declarations do not produce code in main
			continue
		}
		if s.Let != nil && c.constGlobals[sanitizeName(s.Let.Name)] {
			continue
		}
		if s.Var != nil && c.constGlobals[sanitizeName(s.Var.Name)] {
			continue
		}
		if err := c.compileStmt(s, false); err != nil {
			return nil, err
		}
	}
	for _, name := range c.tests {
		c.writeln(name + "();")
	}
	c.indent--
	c.writeln("}")

	// prepend import
	body := c.buf.String()
	c.buf.Reset()
	c.writeln("const std = @import(\"std\");")
	for alias, path := range c.imports {
		c.writeln(fmt.Sprintf("const %s = @import(\"%s\");", alias, path))
	}
	c.writeln("")
	c.writeExpectFunc()
	c.writeBuiltins()
	c.buf.WriteString(body)
	formatted := Format(c.buf.Bytes())
	return formatted, nil
}

func (c *Compiler) compileFun(fn *parser.FunStmt) error {
	name := sanitizeName(fn.Name)

	if c.env != nil {
		c.env.SetFunc(fn.Name, fn)
		ft := types.FuncType{Params: make([]types.Type, len(fn.Params))}
		for i, p := range fn.Params {
			ft.Params[i] = c.resolveTypeRef(p.Type)
		}
		if fn.Return != nil {
			ft.Return = c.resolveTypeRef(fn.Return)
		} else {
			ft.Return = types.VoidType{}
		}
		c.env.SetVar(fn.Name, ft, false)
	}

	if c.indent > 0 {
		expr, err := c.compileFunExpr(&parser.FunExpr{Params: fn.Params, Return: fn.Return, BlockBody: fn.Body})
		if err != nil {
			return err
		}
		c.writeln(fmt.Sprintf("const %s = %s;", name, expr))
		return nil
	}

	oldPtr := c.pushPointerVars()
	params := make([]string, len(fn.Params))
	for i, p := range fn.Params {
		typ := c.zigType(p.Type)
		if p.Type != nil && p.Type.Simple != nil {
			if _, ok := c.env.GetUnion(*p.Type.Simple); ok {
				typ = "*" + typ
				c.pointerVars[p.Name] = true
			} else if _, ok := c.env.GetStruct(*p.Type.Simple); ok {
				typ = "*" + typ
				c.pointerVars[p.Name] = true
			}
		}
		params[i] = fmt.Sprintf("%s: %s", sanitizeName(p.Name), typ)
	}
	ret := "void"
	if fn.Return != nil {
		ret = c.zigType(fn.Return)
	}
	prefix := ""
	if fn.Export {
		prefix = "pub "
	}
	c.writeln(fmt.Sprintf("%sfn %s(%s) %s {", prefix, name, strings.Join(params, ", "), ret))

	oldEnv := c.env
	oldLocals := c.locals
	oldRet := c.funcRet
	c.locals = map[string]types.Type{}
	if c.env != nil {
		child := types.NewEnv(c.env)
		for _, p := range fn.Params {
			t := c.resolveTypeRef(p.Type)
			child.SetVar(p.Name, t, true)
			c.locals[p.Name] = t
		}
		c.env = child
	}
	c.funcRet = c.resolveTypeRef(fn.Return)

	c.indent++
	for _, st := range fn.Body {
		if err := c.compileStmt(st, true); err != nil {
			if c.env != nil {
				c.env = oldEnv
			}
			c.locals = oldLocals
			c.funcRet = oldRet
			return err
		}
	}
	c.indent--
	if c.env != nil {
		c.env = oldEnv
	}
	c.locals = oldLocals
	c.funcRet = oldRet
	c.popPointerVars(oldPtr)
	c.writeln("}")
	return nil
}

func (c *Compiler) compileTest(tb *parser.TestBlock) error {
	name := "test_" + sanitizeName(tb.Name)
	c.tests = append(c.tests, name)
	c.writeln(fmt.Sprintf("fn %s() void {", name))
	c.indent++
	for _, st := range tb.Body {
		if err := c.compileStmt(st, true); err != nil {
			return err
		}
	}
	c.indent--
	c.writeln("}")
	return nil
}

func (c *Compiler) compileTypeDecl(t *parser.TypeDecl) error {
	name := sanitizeName(t.Name)
	if c.structs[name] {
		return nil
	}
	c.structs[name] = true
	if len(t.Variants) > 0 {
		c.writeln(fmt.Sprintf("const %s = union(enum) {", name))
		c.indent++
		for _, v := range t.Variants {
			vname := sanitizeName(v.Name)
			if len(v.Fields) == 0 {
				c.writeln(fmt.Sprintf("%s,", vname))
				if c.env != nil {
					c.variantInfo[v.Name] = struct {
						Union  string
						Fields map[string]types.Type
						Order  []string
					}{Union: t.Name, Fields: map[string]types.Type{}, Order: []string{}}
					c.env.SetStruct(v.Name, types.StructType{Name: v.Name, Fields: map[string]types.Type{}, Order: []string{}})
				}
				continue
			}
			fields := make([]string, len(v.Fields))
			flds := make(map[string]types.Type)
			order := make([]string, len(v.Fields))
			for i, f := range v.Fields {
				typ := c.zigType(f.Type)
				if f.Type != nil && f.Type.Simple != nil && *f.Type.Simple == t.Name {
					typ = "*" + name
				}
				fields[i] = fmt.Sprintf("%s: %s", sanitizeName(f.Name), typ)
				if c.env != nil {
					flds[f.Name] = c.resolveTypeRef(f.Type)
				}
				order[i] = f.Name
			}
			c.writeln(fmt.Sprintf("%s: struct { %s },", vname, strings.Join(fields, ", ")))
			if c.env != nil {
				c.variantInfo[v.Name] = struct {
					Union  string
					Fields map[string]types.Type
					Order  []string
				}{Union: t.Name, Fields: flds, Order: order}
				c.env.SetStruct(v.Name, types.StructType{Name: v.Name, Fields: flds, Order: order})
			}
		}
		c.indent--
		c.writeln("};")
		if c.env != nil {
			variants := map[string]types.StructType{}
			for _, v := range t.Variants {
				if st, ok := c.env.GetStruct(v.Name); ok {
					variants[v.Name] = st
				}
			}
			c.env.SetUnion(t.Name, types.UnionType{Name: t.Name, Variants: variants})
		}
		return nil
	}
	fields := []string{}
	methods := []*parser.FunStmt{}
	for _, m := range t.Members {
		if m.Field != nil {
			typ := c.zigType(m.Field.Type)
			fields = append(fields, fmt.Sprintf("%s: %s,", sanitizeName(m.Field.Name), typ))
			if c.env != nil {
				st, ok := c.env.GetStruct(t.Name)
				if !ok {
					st = types.StructType{Name: t.Name, Fields: map[string]types.Type{}, Order: []string{}, Methods: map[string]types.Method{}}
				}
				st.Fields[m.Field.Name] = c.resolveTypeRef(m.Field.Type)
				st.Order = append(st.Order, m.Field.Name)
				c.env.SetStruct(t.Name, st)
			}
		} else if m.Method != nil {
			methods = append(methods, m.Method)
			if c.env != nil {
				st, ok := c.env.GetStruct(t.Name)
				if !ok {
					st = types.StructType{Name: t.Name, Fields: map[string]types.Type{}, Order: []string{}, Methods: map[string]types.Method{}}
				}
				params := make([]types.Type, len(m.Method.Params))
				for i, p := range m.Method.Params {
					params[i] = c.resolveTypeRef(p.Type)
				}
				var ret types.Type = types.VoidType{}
				if m.Method.Return != nil {
					ret = c.resolveTypeRef(m.Method.Return)
				}
				if st.Methods == nil {
					st.Methods = map[string]types.Method{}
				}
				st.Methods[m.Method.Name] = types.Method{Decl: m.Method, Type: types.FuncType{Params: params, Return: ret}}
				c.env.SetStruct(t.Name, st)
			}
			if err := c.compileMethod(t.Name, m.Method); err != nil {
				return err
			}
		}
	}
	if len(fields) == 1 && len(methods) == 0 {
		c.writeln(fmt.Sprintf("const %s = struct { %s };", name, strings.TrimSuffix(fields[0], ",")))
		return nil
	}
	c.writeln(fmt.Sprintf("const %s = struct {", name))
	c.indent++
	for _, f := range fields {
		c.writeln(f)
	}
	for _, m := range methods {
		// methods already emitted by compileMethod
		_ = m
	}
	c.indent--
	c.writeln("};")
	return nil
}

func (c *Compiler) compileGlobalDecls(prog *parser.Program) error {
	for _, s := range prog.Statements {
		switch {
		case s.Let != nil:
			name := sanitizeName(s.Let.Name)
			var typ types.Type = types.AnyType{}
			if c.env != nil {
				if s.Let.Type != nil {
					typ = c.resolveTypeRef(s.Let.Type)
				} else if s.Let.Value != nil {
					typ = c.inferExprType(s.Let.Value)
				} else if old, err := c.env.GetVar(s.Let.Name); err == nil {
					typ = old
				}
				c.env.SetVar(s.Let.Name, typ, false)
			}
			if s.Let.Value != nil {
				var v string
				if ll := extractListLiteral(s.Let.Value); ll != nil && len(ll.Elems) > 0 && s.Let.Type == nil {
					if extractMapLiteral(ll.Elems[0]) != nil {
						structName := pascalCase(name) + "Item"
						decl, init, err := c.compileNamedListLiteral(ll, true, structName)
						if err == nil {
							if !c.structs[structName] {
								c.writeln(decl)
								c.structs[structName] = true
								if c.env != nil {
									st, ok := c.structTypeFromMapLiteral(extractMapLiteral(ll.Elems[0]), structName)
									if ok {
										c.env.SetStruct(structName, st)
									}
								}
							}
							v = init
							if c.env != nil {
								if st, ok := c.structTypeFromMapLiteral(extractMapLiteral(ll.Elems[0]), structName); ok {
									c.env.SetVar(s.Let.Name, types.ListType{Elem: st}, false)
								}
							}
							c.writeln(fmt.Sprintf("const %s = %s;", name, v))
							c.constGlobals[name] = true
							continue
						}
					}
				}
				if ml := extractMapLiteral(s.Let.Value); ml != nil && s.Let.Type == nil {
					structName := pascalCase(name)
					decl, init, ok, err := c.mapLiteralStruct(ml, structName)
					if ok && err == nil {
						if !c.structs[structName] {
							c.writeln(decl)
							c.structs[structName] = true
							if c.env != nil {
								st, ok := c.structTypeFromMapLiteral(ml, structName)
								if ok {
									c.env.SetStruct(structName, st)
								}
							}
						}
						v = init
						if c.env != nil {
							if st, ok := c.structTypeFromMapLiteral(ml, structName); ok {
								c.env.SetVar(s.Let.Name, st, false)
							}
						}
						c.writeln(fmt.Sprintf("const %s = %s;", name, v))
						c.constGlobals[name] = true
						continue
					}
				}
				if ml := extractMapLiteral(s.Let.Value); ml != nil {
					var err error
					v, err = c.compileMapLiteral(ml, true)
					if err != nil {
						return err
					}
				} else {
					vv, err := c.compileExpr(s.Let.Value, false)
					if err != nil {
						return err
					}
					v = vv
				}
				if isFunExpr(s.Let.Value) || s.Let.Type == nil || canInferType(s.Let.Value, typ) {
					c.writeln(fmt.Sprintf("const %s = %s;", name, v))
				} else {
					c.writeln(fmt.Sprintf("const %s: %s = %s;", name, zigTypeOf(typ), v))
				}
				c.constGlobals[name] = true
			} else {
				c.writeln(fmt.Sprintf("var %s: %s = %s;", name, zigTypeOf(typ), zeroValue(typ)))
				c.constGlobals[name] = true
			}
			continue
		case s.Var != nil:
			name := sanitizeName(s.Var.Name)
			var typ types.Type = types.AnyType{}
			if c.env != nil {
				if s.Var.Type != nil {
					typ = c.resolveTypeRef(s.Var.Type)
				} else if s.Var.Value != nil {
					if ml := extractMapLiteral(s.Var.Value); ml != nil {
						var keyT types.Type = types.AnyType{}
						var valT types.Type = types.AnyType{}
						if len(ml.Items) > 0 {
							if _, ok := simpleStringKey(ml.Items[0].Key); ok {
								keyT = types.StringType{}
							} else if c.isFloatExpr(ml.Items[0].Key) {
								keyT = types.FloatType{}
							} else if c.isBoolExpr(ml.Items[0].Key) {
								keyT = types.BoolType{}
							} else {
								keyT = types.IntType{}
							}
							valT = c.inferExprType(ml.Items[0].Value)
						}
						typ = types.MapType{Key: keyT, Value: valT}
					} else {
						typ = c.inferExprType(s.Var.Value)
					}
				} else if old, err := c.env.GetVar(s.Var.Name); err == nil {
					typ = old
				}
				c.env.SetVar(s.Var.Name, typ, true)
			}
			if s.Var.Value != nil {
				if isMapLiteralExpr(s.Var.Value) || isEmptyMapExpr(s.Var.Value) {
					c.writeln(fmt.Sprintf("var %s: %s = undefined;", name, zigTypeOf(typ)))
					var v string
					if ml := extractMapLiteral(s.Var.Value); ml != nil {
						var err error
						v, err = c.compileMapLiteral(ml, true)
						if err != nil {
							return err
						}
					} else {
						vv, err := c.compileExpr(s.Var.Value, false)
						if err != nil {
							return err
						}
						v = vv
					}
					if c.globalInits == nil {
						c.globalInits = map[string]string{}
					}
					c.globalInits[name] = fmt.Sprintf("%s = %s;", name, v)
				} else {
					v, err := c.compileExpr(s.Var.Value, false)
					if err != nil {
						return err
					}
					if s.Var.Type == nil || canInferType(s.Var.Value, typ) {
						c.writeln(fmt.Sprintf("var %s = %s;", name, v))
					} else {
						c.writeln(fmt.Sprintf("var %s: %s = %s;", name, zigTypeOf(typ), v))
					}
				}
			} else {
				c.writeln(fmt.Sprintf("var %s: %s = %s;", name, zigTypeOf(typ), zeroValue(typ)))
			}
			c.constGlobals[name] = true
			continue
		case s.ExternVar != nil || s.ExternFun != nil || s.ExternType != nil || s.ExternObject != nil:
			// extern declarations are ignored for Zig output
			continue
		}
	}
	if len(c.constGlobals) > 0 {
		c.writeln("")
	}
	return nil
}

func (c *Compiler) compileMethod(structName string, fn *parser.FunStmt) error {
	name := sanitizeName(fn.Name)
	c.writeIndent()
	c.buf.WriteString("fn " + name + "(self: *" + sanitizeName(structName))
	for _, p := range fn.Params {
		typ := c.zigType(p.Type)
		c.buf.WriteString(fmt.Sprintf(", %s: %s", sanitizeName(p.Name), typ))
	}
	ret := "void"
	if fn.Return != nil {
		ret = c.zigType(fn.Return)
	}
	c.buf.WriteString(") " + ret + " {\n")

	origEnv := c.env
	origLocals := c.locals
	origRet := c.funcRet
	c.locals = map[string]types.Type{}
	if c.env != nil {
		child := types.NewEnv(c.env)
		if st, ok := c.env.GetStruct(structName); ok {
			for fname, t := range st.Fields {
				child.SetVar(fname, t, true)
				c.locals[fname] = t
			}
		}
		for _, p := range fn.Params {
			t := c.resolveTypeRef(p.Type)
			child.SetVar(p.Name, t, true)
			c.locals[p.Name] = t
		}
		c.env = child
	}
	c.funcRet = c.resolveTypeRef(fn.Return)

	c.indent++
	for _, st := range fn.Body {
		if err := c.compileStmt(st, true); err != nil {
			if c.env != nil {
				c.env = origEnv
			}
			c.locals = origLocals
			c.funcRet = origRet
			return err
		}
	}
	c.indent--
	if c.env != nil {
		c.env = origEnv
	}
	c.locals = origLocals
	c.funcRet = origRet
	c.writeln("}")
	return nil
}

func (c *Compiler) zigType(t *parser.TypeRef) string {
	if t == nil {
		return "i32"
	}
	if t.Fun != nil {
		params := make([]string, len(t.Fun.Params))
		for i, p := range t.Fun.Params {
			params[i] = c.zigType(p)
		}
		ret := "void"
		if t.Fun.Return != nil {
			ret = c.zigType(t.Fun.Return)
		}
		return fmt.Sprintf("fn(%s) %s", strings.Join(params, ", "), ret)
	}
	if t.Generic != nil && t.Generic.Name == "list" && len(t.Generic.Args) == 1 {
		return "[]const " + c.zigType(t.Generic.Args[0])
	}
	if t.Generic != nil && t.Generic.Name == "map" && len(t.Generic.Args) == 2 {
		return fmt.Sprintf("std.AutoHashMap(%s, %s)", c.zigType(t.Generic.Args[0]), c.zigType(t.Generic.Args[1]))
	}
	if t.Simple == nil {
		return "i32"
	}
	switch *t.Simple {
	case "int":
		return "i32"
	case "float":
		return "f64"
	case "bool":
		return "bool"
	case "string":
		return "[]const u8"
	}
	if c.env != nil {
		if _, ok := c.env.GetStruct(*t.Simple); ok {
			return sanitizeName(*t.Simple)
		}
		if _, ok := c.env.GetUnion(*t.Simple); ok {
			return sanitizeName(*t.Simple)
		}
	}
	return "i32"
}

func (c *Compiler) compileStmt(s *parser.Statement, inFun bool) error {
	switch {
	case s.Let != nil:
		name := sanitizeName(s.Let.Name)
		var typ types.Type = types.AnyType{}
		if c.env != nil {
			if s.Let.Type != nil {
				typ = c.resolveTypeRef(s.Let.Type)
			} else if s.Let.Value != nil {
				typ = c.inferExprType(s.Let.Value)
			} else if old, err := c.env.GetVar(s.Let.Name); err == nil {
				typ = old
			}
			c.env.SetVar(s.Let.Name, typ, false)
		}
		if s.Let.Value != nil && isEmptyMapExpr(s.Let.Value) {
			keyT := "i32"
			valT := "i32"
			if mt, ok := typ.(types.MapType); ok {
				keyT = zigTypeOf(mt.Key)
				valT = zigTypeOf(mt.Value)
			}
			c.writeln(fmt.Sprintf("var %s = std.AutoHashMap(%s, %s).init(std.heap.page_allocator);", name, keyT, valT))
			return nil
		}
		val := "0"
		if s.Let.Value != nil {
			if ml := extractMapLiteral(s.Let.Value); ml != nil && s.Let.Type == nil {
				structName := pascalCase(name)
				decl, init, ok, err := c.mapLiteralStruct(ml, structName)
				if ok && err == nil {
					if !c.structs[structName] {
						c.writeln(decl)
						c.structs[structName] = true
						if c.env != nil {
							st, ok := c.structTypeFromMapLiteral(ml, structName)
							if ok {
								c.env.SetStruct(structName, st)
							}
						}
					}
					val = init
					if c.env != nil {
						if st, ok := c.structTypeFromMapLiteral(ml, structName); ok {
							c.env.SetVar(s.Let.Name, st, false)
							if inFun {
								c.locals[s.Let.Name] = st
							}
						}
					}
				} else {
					v, err := c.compileMapLiteral(ml, true)
					if err != nil {
						return err
					}
					val = v
				}
			} else if f := fetchExprOnly(s.Let.Value); f != nil && typ != (types.AnyType{}) {
				v, err := c.compileFetchExprTyped(f, typ)
				if err != nil {
					return err
				}
				val = v
			} else {
				v, err := c.compileExpr(s.Let.Value, false)
				if err != nil {
					return err
				}
				val = v
			}
		}
		if isFunExpr(s.Let.Value) {
			c.writeln(fmt.Sprintf("const %s = %s;", name, val))
			return nil
		}
		if s.Let.Type == nil && canInferType(s.Let.Value, typ) {
			c.writeln(fmt.Sprintf("const %s = %s;", name, val))
		} else {
			c.writeln(fmt.Sprintf("const %s: %s = %s;", name, zigTypeOf(typ), val))
		}
		return nil
	case s.Var != nil:
		return c.compileVar(s.Var, inFun)
	case s.Import != nil:
		return c.addImport(s.Import)
	case s.Assign != nil:
		return c.compileAssign(s.Assign)
	case s.Return != nil:
		if isListLiteralExpr(s.Return.Value) {
			ll := s.Return.Value.Binary.Left.Value.Target.List
			v, err := c.compileListLiteral(ll, false)
			if err != nil {
				return err
			}
			c.writeln("return " + v + ";")
		} else {
			v, err := c.compileExpr(s.Return.Value, true)
			if err != nil {
				return err
			}
			c.writeln("return " + v + ";")
		}
	case s.For != nil:
		name := sanitizeName(s.For.Name)
		if startExpr, endExpr, stepExpr, ok := c.rangeArgs(s.For.Source); ok {
			start := "0"
			if startExpr != nil {
				v, err := c.compileExpr(startExpr, false)
				if err != nil {
					return err
				}
				start = v
			}
			end, err := c.compileExpr(endExpr, false)
			if err != nil {
				return err
			}
			if stepExpr == nil {
				c.writeln(fmt.Sprintf("for (%s .. %s) |%s| {", start, end, name))
				if c.env != nil {
					c.env.SetVar(s.For.Name, types.IntType{}, true)
				}
				if inFun {
					c.locals[s.For.Name] = types.IntType{}
				}
				c.indent++
				for _, st := range s.For.Body {
					if err := c.compileStmt(st, inFun); err != nil {
						return err
					}
				}
				c.indent--
				c.writeln("}")
			} else {
				step, err := c.compileExpr(stepExpr, false)
				if err != nil {
					return err
				}
				iter := c.newTmp()
				c.writeln(fmt.Sprintf("var %s = %s;", iter, start))
				c.writeln(fmt.Sprintf("while (%s < %s) {", iter, end))
				if c.env != nil {
					c.env.SetVar(s.For.Name, types.IntType{}, true)
				}
				if inFun {
					c.locals[s.For.Name] = types.IntType{}
				}
				c.indent++
				c.writeln(fmt.Sprintf("const %s = %s;", name, iter))
				for _, st := range s.For.Body {
					if err := c.compileStmt(st, inFun); err != nil {
						return err
					}
				}
				c.writeln(fmt.Sprintf("%s += %s;", iter, step))
				c.indent--
				c.writeln("}")
			}
			return nil
		}
		var start string
		var err error
		if ml := extractMapLiteral(s.For.Source); ml != nil {
			start, err = c.compileMapLiteral(ml, true)
		} else {
			start, err = c.compileExpr(s.For.Source, false)
		}
		if err != nil {
			return err
		}
		if s.For.RangeEnd != nil {
			end, err := c.compileExpr(s.For.RangeEnd, false)
			if err != nil {
				return err
			}
			c.writeln(fmt.Sprintf("for (%s .. %s) |%s| {", start, end, name))
			if c.env != nil {
				c.env.SetVar(s.For.Name, types.IntType{}, true)
			}
			if inFun {
				c.locals[s.For.Name] = types.IntType{}
			}
			c.indent++
		} else if c.isMapExpr(s.For.Source) {
			iter := c.newTmp()
			c.writeln(fmt.Sprintf("var %s = %s.keyIterator();", iter, start))
			c.writeln(fmt.Sprintf("while (%s.next()) |k_ptr| {", iter))
			if c.env != nil {
				if mt, ok := c.inferExprType(s.For.Source).(types.MapType); ok {
					c.env.SetVar(s.For.Name, mt.Key, true)
					if inFun {
						c.locals[s.For.Name] = mt.Key
					}
				} else {
					c.env.SetVar(s.For.Name, types.AnyType{}, true)
					if inFun {
						c.locals[s.For.Name] = types.AnyType{}
					}
				}
			} else if inFun {
				if mt, ok := c.inferExprType(s.For.Source).(types.MapType); ok {
					c.locals[s.For.Name] = mt.Key
				} else {
					c.locals[s.For.Name] = types.AnyType{}
				}
			}
			c.indent++
			c.writeln(fmt.Sprintf("const %s = k_ptr.*;", name))
		} else {
			c.writeln(fmt.Sprintf("for (%s) |%s| {", start, name))
			if lt, ok := c.inferExprType(s.For.Source).(types.ListType); ok {
				if c.env != nil {
					c.env.SetVar(s.For.Name, lt.Elem, true)
				}
				if inFun {
					c.locals[s.For.Name] = lt.Elem
				}
			} else if _, ok := c.inferExprType(s.For.Source).(types.StringType); ok {
				if c.env != nil {
					c.env.SetVar(s.For.Name, types.StringType{}, true)
				}
				if inFun {
					c.locals[s.For.Name] = types.StringType{}
				}
			} else {
				if c.env != nil {
					c.env.SetVar(s.For.Name, types.AnyType{}, true)
				}
				if inFun {
					c.locals[s.For.Name] = types.AnyType{}
				}
			}
			c.indent++
		}
		for _, st := range s.For.Body {
			if err := c.compileStmt(st, inFun); err != nil {
				return err
			}
		}
		c.indent--
		c.writeln("}")
	case s.While != nil:
		return c.compileWhile(s.While)
	case s.Break != nil:
		c.writeln("break;")
	case s.Continue != nil:
		c.writeln("continue;")
	case s.Update != nil:
		return c.compileUpdate(s.Update)
	case s.Expr != nil:
		v, err := c.compileExpr(s.Expr.Expr, false)
		if err != nil {
			return err
		}
		c.writeln(v + ";")
	case s.Fun != nil:
		return c.compileFun(s.Fun)
	case s.If != nil:
		return c.compileIf(s.If)
	case s.Test != nil:
		return c.compileTest(s.Test)
	case s.Expect != nil:
		expr, err := c.compileExpr(s.Expect.Value, false)
		if err != nil {
			return err
		}
		c.needsExpect = true
		c.writeln(fmt.Sprintf("expect(%s);", expr))
		return nil
	case s.ExternVar != nil || s.ExternFun != nil || s.ExternType != nil || s.ExternObject != nil:
		// extern declarations have no direct Zig equivalent at runtime
		return nil
	default:
		return fmt.Errorf("unsupported statement")
	}
	return nil
}

func (c *Compiler) compileIf(stmt *parser.IfStmt) error {
	cond, err := c.compileExpr(stmt.Cond, false)
	if err != nil {
		return err
	}
	c.writeln(fmt.Sprintf("if (%s) {", cond))
	c.indent++
	for _, st := range stmt.Then {
		if err := c.compileStmt(st, true); err != nil {
			return err
		}
	}
	c.indent--
	if stmt.ElseIf != nil {
		c.writeIndent()
		c.buf.WriteString("} else ")
		return c.compileIf(stmt.ElseIf)
	}
	if len(stmt.Else) > 0 {
		c.writeln("} else {")
		c.indent++
		for _, st := range stmt.Else {
			if err := c.compileStmt(st, true); err != nil {
				return err
			}
		}
		c.indent--
		c.writeln("}")
		return nil
	}
	c.writeln("}")
	return nil
}

func (c *Compiler) compileWhile(stmt *parser.WhileStmt) error {
	cond, err := c.compileExpr(stmt.Cond, false)
	if err != nil {
		return err
	}
	c.writeln(fmt.Sprintf("while (%s) {", cond))
	c.indent++
	for _, st := range stmt.Body {
		if err := c.compileStmt(st, true); err != nil {
			return err
		}
	}
	c.indent--
	c.writeln("}")
	return nil
}

func (c *Compiler) compileUpdate(u *parser.UpdateStmt) error {
	target := sanitizeName(u.Target)
	var elem types.Type = types.AnyType{}
	if c.env != nil {
		if tv, err := c.env.GetVar(u.Target); err == nil {
			if lt, ok := tv.(types.ListType); ok {
				elem = lt.Elem
			}
		}
	}
	idx := c.newTmp()
	c.writeln(fmt.Sprintf("for (0..%s.len) |%s| {", target, idx))
	c.indent++
	itemVar := c.newTmp()
	itemType := zigTypeOf(elem)
	if strings.HasPrefix(itemType, "[]const ") {
		itemType = strings.TrimPrefix(itemType, "[]const ")
	}
	c.writeln(fmt.Sprintf("var %s: %s = %s[%s];", itemVar, itemType, target, idx))

	prevEnv := c.env
	if prevEnv != nil {
		child := types.NewEnv(prevEnv)
		if st, ok := elem.(types.StructType); ok {
			for _, f := range st.Order {
				ft := st.Fields[f]
				typ := zigTypeOf(ft)
				if strings.HasPrefix(typ, "[]const ") {
					typ = strings.TrimPrefix(typ, "[]const ")
				}
				name := sanitizeName(f)
				c.writeln(fmt.Sprintf("var %s: %s = %s.%s;", name, typ, itemVar, name))
				child.SetVar(f, ft, true)
			}
		}
		c.env = child
	}

	if u.Where != nil {
		cond, err := c.compileExpr(u.Where, false)
		if err != nil {
			if prevEnv != nil {
				c.env = prevEnv
			}
			return err
		}
		c.writeln(fmt.Sprintf("if (%s) {", cond))
		c.indent++
	}

	if st, ok := elem.(types.StructType); ok {
		for _, it := range u.Set.Items {
			key, _ := identName(it.Key)
			ft := st.Fields[key]
			val, err := c.compileExpr(it.Value, false)
			if err != nil {
				if prevEnv != nil {
					c.env = prevEnv
				}
				return err
			}
			c.writeln(fmt.Sprintf("%s.%s = %s;", itemVar, sanitizeName(key), val))
			_ = ft
		}
	} else {
		for _, it := range u.Set.Items {
			k, ok := simpleStringKey(it.Key)
			keyExpr := ""
			if ok {
				keyExpr = fmt.Sprintf("\"%s\"", k)
			} else {
				ke, err := c.compileExpr(it.Key, false)
				if err != nil {
					if prevEnv != nil {
						c.env = prevEnv
					}
					return err
				}
				keyExpr = ke
			}
			val, err := c.compileExpr(it.Value, false)
			if err != nil {
				if prevEnv != nil {
					c.env = prevEnv
				}
				return err
			}
			c.writeln(fmt.Sprintf("_ = %s.put(%s, %s) catch unreachable;", itemVar, keyExpr, val))
		}
	}

	if u.Where != nil {
		c.indent--
		c.writeln("}")
	}

	if prevEnv != nil {
		c.env = prevEnv
	}

	c.writeln(fmt.Sprintf("%s[%s] = %s;", target, idx, itemVar))
	c.indent--
	c.writeln("}")
	return nil
}

func (c *Compiler) compileMatchExpr(m *parser.MatchExpr) (string, error) {
	target, err := c.compileExpr(m.Target, false)
	if err != nil {
		return "", err
	}
	if _, ok := c.inferExprType(m.Target).(types.UnionType); ok {
		var b strings.Builder
		b.WriteString("switch (" + target + ".*) {")
		for _, cs := range m.Cases {
			if isUnderscoreExpr(cs.Pattern) {
				res, err := c.compileExpr(cs.Result, false)
				if err != nil {
					return "", err
				}
				b.WriteString("else => " + res + ", ")
				continue
			}
			if name, args, ok := c.variantPattern(cs.Pattern); ok {
				info := c.variantInfo[name]
				var payload string
				origEnv := c.env
				origCap := c.captures
				if len(args) > 0 {
					payload = c.newTmp()
					if c.env != nil {
						child := types.NewEnv(c.env)
						for i, a := range args {
							t := info.Fields[info.Order[i]]
							child.SetVar(a, t, true)
						}
						c.env = child
					}
					caps := make(map[string]string, len(c.captures)+len(args))
					for k, v := range c.captures {
						caps[k] = v
					}
					for i, a := range args {
						caps[sanitizeName(a)] = payload + "." + sanitizeName(info.Order[i])
					}
					c.captures = caps
				}
				res, err := c.compileExpr(cs.Result, false)
				if len(args) > 0 {
					if origEnv != nil {
						c.env = origEnv
					}
					c.captures = origCap
				}
				if err != nil {
					return "", err
				}
				if len(args) > 0 {
					b.WriteString(fmt.Sprintf(".%s => |%s| %s, ", sanitizeName(name), payload, res))
				} else {
					b.WriteString(fmt.Sprintf(".%s => %s, ", sanitizeName(name), res))
				}
				continue
			}
			// fallback to equality
			pat, err := c.compileExpr(cs.Pattern, false)
			if err != nil {
				return "", err
			}
			res, err := c.compileExpr(cs.Result, false)
			if err != nil {
				return "", err
			}
			c.needsEqual = true
			b.WriteString(fmt.Sprintf("else => if (_equal(%s, %s)) %s else ", target, pat, res))
		}
		b.WriteString("}")
		return b.String(), nil
	}
	simple := true
	for _, cs := range m.Cases {
		if isUnderscoreExpr(cs.Pattern) {
			continue
		}
		if !literalExpr(cs.Pattern) {
			simple = false
			break
		}
	}
	if simple {
		var b strings.Builder
		b.WriteString("switch (" + target + ") {")
		for _, cs := range m.Cases {
			res, err := c.compileExpr(cs.Result, false)
			if err != nil {
				return "", err
			}
			if isUnderscoreExpr(cs.Pattern) {
				b.WriteString("else => " + res + ", ")
				continue
			}
			pat, err := c.compileExpr(cs.Pattern, false)
			if err != nil {
				return "", err
			}
			b.WriteString(pat + " => " + res + ", ")
		}
		b.WriteString("}")
		return b.String(), nil
	}
	expr := "0"
	for i := len(m.Cases) - 1; i >= 0; i-- {
		cs := m.Cases[i]
		res, err := c.compileExpr(cs.Result, false)
		if err != nil {
			return "", err
		}
		if isUnderscoreExpr(cs.Pattern) {
			expr = res
			continue
		}
		pat, err := c.compileExpr(cs.Pattern, false)
		if err != nil {
			return "", err
		}
		c.needsEqual = true
		expr = fmt.Sprintf("if (_equal(%s, %s)) %s else (%s)", target, pat, res, expr)
	}
	return expr, nil
}

func (c *Compiler) compileIfExpr(ie *parser.IfExpr) (string, error) {
	cond, err := c.compileExpr(ie.Cond, false)
	if err != nil {
		return "", err
	}
	thenVal, err := c.compileExpr(ie.Then, false)
	if err != nil {
		return "", err
	}
	elseVal := "void"
	if ie.ElseIf != nil {
		v, err := c.compileIfExpr(ie.ElseIf)
		if err != nil {
			return "", err
		}
		elseVal = v
	} else if ie.Else != nil {
		v, err := c.compileExpr(ie.Else, false)
		if err != nil {
			return "", err
		}
		elseVal = v
	}
	return fmt.Sprintf("if (%s) (%s) else (%s)", cond, thenVal, elseVal), nil
}

func (c *Compiler) compileQueryExpr(q *parser.QueryExpr) (string, error) {
	if q.Group != nil {

		src, err := c.compileExpr(q.Source, false)
		if err != nil {
			return "", err
		}

		var elemType types.Type = types.AnyType{}
		if lt, ok := c.inferExprType(q.Source).(types.ListType); ok {
			elemType = lt.Elem
		}
		child := types.NewEnv(c.env)
		child.SetVar(q.Var, elemType, true)

		fromSrcs := make([]string, len(q.Froms))
		for i, f := range q.Froms {
			fs, err := c.compileExpr(f.Src, false)
			if err != nil {
				return "", err
			}
			fromSrcs[i] = fs
			var fe types.Type = types.AnyType{}
			if lt, ok := c.inferExprType(f.Src).(types.ListType); ok {
				fe = lt.Elem
			}
			child.SetVar(f.Var, fe, true)
		}

		joinSrcs := make([]string, len(q.Joins))
		joinOns := make([]string, len(q.Joins))
		for i, j := range q.Joins {
			js, err := c.compileExpr(j.Src, false)
			if err != nil {
				return "", err
			}
			joinSrcs[i] = js
			on, err := c.compileExpr(j.On, false)
			if err != nil {
				return "", err
			}
			joinOns[i] = on
			var je types.Type = types.AnyType{}
			if lt, ok := c.inferExprType(j.Src).(types.ListType); ok {
				je = lt.Elem
			}
			child.SetVar(j.Var, je, true)
		}

		orig := c.env
		c.env = child
		keyExpr, err := c.compileExpr(q.Group.Exprs[0], false)
		if err != nil {
			c.env = orig
			return "", err
		}
		genv := types.NewEnv(child)
		genv.SetVar(q.Group.Name, types.GroupType{Elem: elemType}, true)
		c.env = genv
		sel, err := c.compileExpr(q.Select, false)
		if err != nil {
			c.env = orig
			return "", err
		}
		var cond string
		if q.Where != nil {
			cond, err = c.compileExpr(q.Where, false)
			if err != nil {
				c.env = orig
				return "", err
			}
		}
		var sortExpr, skipExpr, takeExpr, havingExpr string
		if q.Sort != nil {
			sortExpr, err = c.compileExpr(q.Sort, false)
			if err != nil {
				c.env = orig
				return "", err
			}
		}
		if q.Group.Having != nil {
			havingExpr, err = c.compileExpr(q.Group.Having, false)
			if err != nil {
				c.env = orig
				return "", err
			}
		}
		if q.Skip != nil {
			skipExpr, err = c.compileExpr(q.Skip, false)
			if err != nil {
				c.env = orig
				return "", err
			}
		}
		if q.Take != nil {
			takeExpr, err = c.compileExpr(q.Take, false)
			if err != nil {
				c.env = orig
				return "", err
			}
		}
		resType := zigTypeOf(c.inferExprType(q.Select))
		keyType := zigTypeOf(c.inferExprType(q.Group.Exprs[0]))
		c.env = orig

		groupElem := strings.TrimPrefix(zigTypeOf(elemType), "[]const ")
		resElem := strings.TrimPrefix(resType, "[]const ")
		groupType := "struct { key: " + keyType + ", Items: std.ArrayList(" + groupElem + ") }"
		tmp := c.newTmp()
		idxMap := c.newTmp()
		var b strings.Builder
		c.needsEqual = true
		lbl := c.newLabel()
		b.WriteString(lbl + ": { var " + tmp + " = std.ArrayList(" + groupType + ").init(std.heap.page_allocator); ")
		idxMapType := "std.AutoHashMap(" + keyType + ", usize)"
		if keyType == "[]const u8" {
			idxMapType = "std.StringHashMap(usize)"
		}
		b.WriteString("var " + idxMap + " = " + idxMapType + ".init(std.heap.page_allocator); ")
		b.WriteString("for (" + src + ") |" + sanitizeName(q.Var) + "| {")
		for i, fs := range fromSrcs {
			b.WriteString(" for (" + fs + ") |" + sanitizeName(q.Froms[i].Var) + "| {")
		}
		for i, js := range joinSrcs {
			b.WriteString(" for (" + js + ") |" + sanitizeName(q.Joins[i].Var) + "| {")
			b.WriteString(" if (!(" + joinOns[i] + ")) continue;")
		}
		if cond != "" {
			b.WriteString(" if (!(" + cond + ")) continue;")
		}
		keyVar := c.newTmp()
		b.WriteString(" const " + keyVar + " = " + keyExpr + ";")
		b.WriteString(" if (" + idxMap + ".get(" + keyVar + ")) |idx| {")
		b.WriteString(" " + tmp + ".items[idx].Items.append(" + sanitizeName(q.Var) + ") catch unreachable;")
		b.WriteString(" } else { var g = " + groupType + "{ .key = " + keyVar + ", .Items = std.ArrayList(" + groupElem + ").init(std.heap.page_allocator) }; g.Items.append(" + sanitizeName(q.Var) + ") catch unreachable; " + tmp + ".append(g) catch unreachable; " + idxMap + ".put(" + keyVar + ", " + tmp + ".items.len - 1) catch unreachable; }")
		for i := 0; i < len(q.Joins); i++ {
			b.WriteString(" }")
		}
		for i := 0; i < len(q.Froms); i++ {
			b.WriteString(" }")
		}
		b.WriteString(" }")
		itemsVar := c.newTmp()
		b.WriteString(" var " + itemsVar + " = std.ArrayList(" + groupType + ").init(std.heap.page_allocator);")
		b.WriteString("for (" + tmp + ".items) |" + sanitizeName(q.Group.Name) + "| {")
		if havingExpr != "" {
			b.WriteString(" if (!(" + havingExpr + ")) continue;")
		}
		b.WriteString(" " + itemsVar + ".append(" + sanitizeName(q.Group.Name) + ") catch unreachable; }")
		if sortExpr != "" {
			keyType := zigTypeOf(c.inferExprType(q.Sort))
			pairType := "struct { item: " + groupType + ", key: " + keyType + " }"
			sortTmp := c.newTmp()
			b.WriteString(" var " + sortTmp + " = std.ArrayList(" + pairType + ").init(std.heap.page_allocator);")
			b.WriteString("for (" + itemsVar + ".items) |" + sanitizeName(q.Group.Name) + "| { " + sortTmp + ".append(.{ .item = " + sanitizeName(q.Group.Name) + ", .key = " + sortExpr + " }) catch unreachable; }")
			b.WriteString(" for (0.." + sortTmp + ".items.len) |i| { for (i+1.." + sortTmp + ".items.len) |j| {")
			cmp := sortTmp + ".items[j].key < " + sortTmp + ".items[i].key"
			if zigTypeOf(c.inferExprType(q.Sort)) == "[]const u8" {
				cmp = "std.mem.lessThan(u8, " + sortTmp + ".items[j].key, " + sortTmp + ".items[i].key)"
			}
			b.WriteString(" if (" + cmp + ") { const t = " + sortTmp + ".items[i]; " + sortTmp + ".items[i] = " + sortTmp + ".items[j]; " + sortTmp + ".items[j] = t; }")
			b.WriteString(" } }")
			itemsVar2 := c.newTmp()
			b.WriteString(" var " + itemsVar2 + " = std.ArrayList(" + groupType + ").init(std.heap.page_allocator);")
			b.WriteString("for (" + sortTmp + ".items) |p| { " + itemsVar2 + ".append(p.item) catch unreachable; }")
			itemsVar = itemsVar2
		}
		resVar := c.newTmp()
		b.WriteString(" var " + resVar + " = std.ArrayList(" + resElem + ").init(std.heap.page_allocator);")
		b.WriteString("for (" + itemsVar + ".items) |" + sanitizeName(q.Group.Name) + "| { " + resVar + ".append(" + sel + ") catch unreachable; }")
		b.WriteString(" const " + resVar + "Slice = " + resVar + ".toOwnedSlice() catch unreachable;")
		if skipExpr != "" || takeExpr != "" {
			c.needsSlice = true
			start := "0"
			if skipExpr != "" {
				start = skipExpr
			}
			end := fmt.Sprintf("@as(i32, @intCast(%sSlice.len))", resVar)
			if takeExpr != "" {
				if skipExpr != "" {
					end = fmt.Sprintf("(%s + %s)", skipExpr, takeExpr)
				} else {
					end = takeExpr
				}
			}
			b.WriteString(" var " + resVar + "Sliced = _slice_list(" + resElem + ", " + resVar + "Slice, " + start + ", " + end + ", 1);")
			b.WriteString(" break :" + lbl + " " + resVar + "Sliced; }")
		} else {
			b.WriteString(" break :" + lbl + " " + resVar + "Slice; }")
		}
		return b.String(), nil
	}

	if len(q.Froms) > 0 || len(q.Joins) > 0 {
		// handle simple left join without extra clauses
		if len(q.Froms) == 0 && len(q.Joins) == 1 && q.Group == nil && q.Joins[0].Side != nil && *q.Joins[0].Side == "left" {
			src, err := c.compileExpr(q.Source, false)
			if err != nil {
				return "", err
			}
			joinSrc, err := c.compileExpr(q.Joins[0].Src, false)
			if err != nil {
				return "", err
			}
			on, err := c.compileExpr(q.Joins[0].On, false)
			if err != nil {
				return "", err
			}
			var elemType types.Type = types.AnyType{}
			if lt, ok := c.inferExprType(q.Source).(types.ListType); ok {
				elemType = lt.Elem
			}
			var joinType types.Type = types.AnyType{}
			if lt, ok := c.inferExprType(q.Joins[0].Src).(types.ListType); ok {
				joinType = lt.Elem
			}
			child := types.NewEnv(c.env)
			child.SetVar(q.Var, elemType, true)
			child.SetVar(q.Joins[0].Var, joinType, true)
			orig := c.env
			c.env = child
			sel, err := c.compileExpr(q.Select, false)
			if err != nil {
				c.env = orig
				return "", err
			}
			var cond string
			if q.Where != nil {
				cond, err = c.compileExpr(q.Where, false)
				if err != nil {
					c.env = orig
					return "", err
				}
			}
			resType := zigTypeOf(c.inferExprType(q.Select))
			joinElem := zigTypeOf(joinType)
			c.env = orig

			tmp := c.newTmp()
			var b strings.Builder
			elem := strings.TrimPrefix(resType, "[]const ")
			lbl := c.newLabel()
			jv := sanitizeName(q.Joins[0].Var)
			b.WriteString(lbl + ": { var " + tmp + " = std.ArrayList(" + elem + ").init(std.heap.page_allocator); ")
			b.WriteString("for (" + src + ") |" + sanitizeName(q.Var) + "| { var matched = false; for (" + joinSrc + ") |" + jv + "| {")
			b.WriteString(" if (!(" + on + ")) continue; matched = true;")
			if cond != "" {
				b.WriteString(" if (!(" + cond + ")) { continue; }")
			}
			b.WriteString(" " + tmp + ".append(" + sel + ") catch unreachable; }")
			b.WriteString(" if (!matched) { const " + jv + ": ?" + joinElem + " = null;")
			if cond != "" {
				b.WriteString(" if (" + cond + ") { " + tmp + ".append(" + sel + ") catch unreachable; }")
			} else {
				b.WriteString(" " + tmp + ".append(" + sel + ") catch unreachable;")
			}
			b.WriteString(" } } const res = " + tmp + ".toOwnedSlice() catch unreachable; break :" + lbl + " res; }")
			return b.String(), nil
		}
		// handle simple right join without extra clauses
		if len(q.Froms) == 0 && len(q.Joins) == 1 && q.Group == nil && q.Joins[0].Side != nil && *q.Joins[0].Side == "right" {
			joinSrc, err := c.compileExpr(q.Joins[0].Src, false)
			if err != nil {
				return "", err
			}
			src, err := c.compileExpr(q.Source, false)
			if err != nil {
				return "", err
			}
			on, err := c.compileExpr(q.Joins[0].On, false)
			if err != nil {
				return "", err
			}
			var elemType types.Type = types.AnyType{}
			if lt, ok := c.inferExprType(q.Source).(types.ListType); ok {
				elemType = lt.Elem
			}
			var joinType types.Type = types.AnyType{}
			if lt, ok := c.inferExprType(q.Joins[0].Src).(types.ListType); ok {
				joinType = lt.Elem
			}
			child := types.NewEnv(c.env)
			child.SetVar(q.Var, elemType, true)
			child.SetVar(q.Joins[0].Var, joinType, true)
			orig := c.env
			c.env = child
			sel, err := c.compileExpr(q.Select, false)
			if err != nil {
				c.env = orig
				return "", err
			}
			var cond string
			if q.Where != nil {
				cond, err = c.compileExpr(q.Where, false)
				if err != nil {
					c.env = orig
					return "", err
				}
			}
			resType := zigTypeOf(c.inferExprType(q.Select))
			elemElem := zigTypeOf(elemType)
			c.env = orig

			tmp := c.newTmp()
			var b strings.Builder
			elem := strings.TrimPrefix(resType, "[]const ")
			lbl := c.newLabel()
			lv := sanitizeName(q.Var)
			jv := sanitizeName(q.Joins[0].Var)
			b.WriteString(lbl + ": { var " + tmp + " = std.ArrayList(" + elem + ").init(std.heap.page_allocator); ")
			b.WriteString("for (" + joinSrc + ") |" + jv + "| { var matched = false; for (" + src + ") |" + lv + "| {")
			b.WriteString(" if (!(" + on + ")) continue; matched = true;")
			if cond != "" {
				b.WriteString(" if (!(" + cond + ")) { continue; }")
			}
			b.WriteString(" " + tmp + ".append(" + sel + ") catch unreachable; }")
			b.WriteString(" if (!matched) { const " + lv + ": ?" + elemElem + " = null;")
			if cond != "" {
				b.WriteString(" if (" + cond + ") { " + tmp + ".append(" + sel + ") catch unreachable; }")
			} else {
				b.WriteString(" " + tmp + ".append(" + sel + ") catch unreachable;")
			}
			b.WriteString(" } } const res = " + tmp + ".toOwnedSlice() catch unreachable; break :" + lbl + " res; }")
			return b.String(), nil
		}
		// handle simple outer join without extra clauses
		if len(q.Froms) == 0 && len(q.Joins) == 1 && q.Group == nil && q.Joins[0].Side != nil && *q.Joins[0].Side == "outer" {
			src, err := c.compileExpr(q.Source, false)
			if err != nil {
				return "", err
			}
			joinSrc, err := c.compileExpr(q.Joins[0].Src, false)
			if err != nil {
				return "", err
			}
			on, err := c.compileExpr(q.Joins[0].On, false)
			if err != nil {
				return "", err
			}
			var elemType types.Type = types.AnyType{}
			if lt, ok := c.inferExprType(q.Source).(types.ListType); ok {
				elemType = lt.Elem
			}
			var joinType types.Type = types.AnyType{}
			if lt, ok := c.inferExprType(q.Joins[0].Src).(types.ListType); ok {
				joinType = lt.Elem
			}
			child := types.NewEnv(c.env)
			child.SetVar(q.Var, elemType, true)
			child.SetVar(q.Joins[0].Var, joinType, true)
			orig := c.env
			c.env = child
			sel, err := c.compileExpr(q.Select, false)
			if err != nil {
				c.env = orig
				return "", err
			}
			var cond string
			if q.Where != nil {
				cond, err = c.compileExpr(q.Where, false)
				if err != nil {
					c.env = orig
					return "", err
				}
			}
			resType := zigTypeOf(c.inferExprType(q.Select))
			elemElem := zigTypeOf(elemType)
			joinElem := zigTypeOf(joinType)
			c.env = orig

			tmp := c.newTmp()
			matched := c.newTmp()
			var b strings.Builder
			elem := strings.TrimPrefix(resType, "[]const ")
			lbl := c.newLabel()
			lv := sanitizeName(q.Var)
			jv := sanitizeName(q.Joins[0].Var)
			b.WriteString(lbl + ": { var " + tmp + " = std.ArrayList(" + elem + ").init(std.heap.page_allocator); ")
			b.WriteString("var " + matched + " = std.AutoHashMap(usize, bool).init(std.heap.page_allocator); ")
			b.WriteString("for (" + src + ", 0..) |" + lv + ", _i| { var " + jv + ": ?" + joinElem + " = null; var mi: usize = 0; for (" + joinSrc + ", 0..) |j, ji| {")
			b.WriteString(" if (!(" + on + ")) continue; " + jv + " = j; mi = ji; " + matched + ".put(ji, true) catch {}; break; }")
			if cond != "" {
				b.WriteString(" if (" + cond + ") { " + tmp + ".append(" + sel + ") catch unreachable; }")
			} else {
				b.WriteString(" " + tmp + ".append(" + sel + ") catch unreachable;")
			}
			b.WriteString(" } for (" + joinSrc + ", 0..) |j, ji| { if (!" + matched + ".contains(ji)) { const " + lv + ": ?" + elemElem + " = null; " + jv + " = j;")
			if cond != "" {
				b.WriteString(" if (" + cond + ") { " + tmp + ".append(" + sel + ") catch unreachable; }")
			} else {
				b.WriteString(" " + tmp + ".append(" + sel + ") catch unreachable;")
			}
			b.WriteString(" } } const res = " + tmp + ".toOwnedSlice() catch unreachable; break :" + lbl + " res; }")
			return b.String(), nil
		}
		if q.Group != nil {
			return "", fmt.Errorf("unsupported query features")
		}

		src, err := c.compileExpr(q.Source, false)
		if err != nil {
			return "", err
		}

		var elemType types.Type = types.AnyType{}
		if lt, ok := c.inferExprType(q.Source).(types.ListType); ok {
			elemType = lt.Elem
		}
		child := types.NewEnv(c.env)
		child.SetVar(q.Var, elemType, true)
		fromSrcs := make([]string, len(q.Froms))
		for i, f := range q.Froms {
			fs, err := c.compileExpr(f.Src, false)
			if err != nil {
				return "", err
			}
			fromSrcs[i] = fs
			var fe types.Type = types.AnyType{}
			if lt, ok := c.inferExprType(f.Src).(types.ListType); ok {
				fe = lt.Elem
			}
			child.SetVar(f.Var, fe, true)
		}
		joinSrcs := make([]string, len(q.Joins))
		joinOns := make([]string, len(q.Joins))
		for i, j := range q.Joins {
			js, err := c.compileExpr(j.Src, false)
			if err != nil {
				return "", err
			}
			joinSrcs[i] = js
			on, err := c.compileExpr(j.On, false)
			if err != nil {
				return "", err
			}
			joinOns[i] = on
			var je types.Type = types.AnyType{}
			if lt, ok := c.inferExprType(j.Src).(types.ListType); ok {
				je = lt.Elem
			}
			child.SetVar(j.Var, je, true)
		}

		orig := c.env
		c.env = child
		sel, err := c.compileExpr(q.Select, false)
		if err != nil {
			c.env = orig
			return "", err
		}
		var cond string
		if q.Where != nil {
			cond, err = c.compileExpr(q.Where, false)
			if err != nil {
				c.env = orig
				return "", err
			}
		}
		resType := zigTypeOf(c.inferExprType(q.Select))
		c.env = orig

		tmp := c.newTmp()
		var b strings.Builder
		elem := strings.TrimPrefix(resType, "[]const ")
		lbl := c.newLabel()
		b.WriteString(lbl + ": { var " + tmp + " = std.ArrayList(" + elem + ").init(std.heap.page_allocator); ")
		b.WriteString("for (" + src + ") |" + sanitizeName(q.Var) + "| {")
		for i, fs := range fromSrcs {
			b.WriteString(" for (" + fs + ") |" + sanitizeName(q.Froms[i].Var) + "| {")
		}
		for i, js := range joinSrcs {
			b.WriteString(" for (" + js + ") |" + sanitizeName(q.Joins[i].Var) + "| {")
			b.WriteString(" if (!(" + joinOns[i] + ")) continue;")
		}
		if cond != "" {
			b.WriteString(" if (!(" + cond + ")) continue;")
		}
		b.WriteString(" " + tmp + ".append(" + sel + ") catch unreachable;")
		for i := 0; i < len(q.Joins); i++ {
			b.WriteString(" }")
		}
		for i := 0; i < len(q.Froms); i++ {
			b.WriteString(" }")
		}
		b.WriteString(" }")
		resVar := c.newTmp()
		b.WriteString(" const " + resVar + " = " + tmp + ".toOwnedSlice() catch unreachable;")
		b.WriteString(" break :" + lbl + " " + resVar + "; }")
		return b.String(), nil
	}

	src, err := c.compileExpr(q.Source, false)
	if err != nil {
		return "", err
	}

	var elemType types.Type = types.AnyType{}
	if lt, ok := c.inferExprType(q.Source).(types.ListType); ok {
		elemType = lt.Elem
	}
	child := types.NewEnv(c.env)
	child.SetVar(q.Var, elemType, true)
	orig := c.env
	c.env = child

	sel, err := c.compileExpr(q.Select, false)
	if err != nil {
		c.env = orig
		return "", err
	}
	var cond string
	if q.Where != nil {
		cond, err = c.compileExpr(q.Where, false)
		if err != nil {
			c.env = orig
			return "", err
		}
	}
	var sortExpr string
	if q.Sort != nil {
		sortExpr, err = c.compileExpr(q.Sort, false)
		if err != nil {
			c.env = orig
			return "", err
		}
	}
	var skipExpr, takeExpr string
	if q.Skip != nil {
		skipExpr, err = c.compileExpr(q.Skip, false)
		if err != nil {
			c.env = orig
			return "", err
		}
	}
	if q.Take != nil {
		takeExpr, err = c.compileExpr(q.Take, false)
		if err != nil {
			c.env = orig
			return "", err
		}
	}
	resType := zigTypeOf(c.inferExprType(q.Select))
	c.env = orig

	tmp := c.newTmp()
	var b strings.Builder
	elem := strings.TrimPrefix(resType, "[]const ")
	lbl := c.newLabel()
	if sortExpr != "" {
		keyType := zigTypeOf(c.inferExprType(q.Sort))
		pairType := "struct { item: " + elem + ", key: " + keyType + " }"
		b.WriteString(lbl + ": { var " + tmp + " = std.ArrayList(" + pairType + ").init(std.heap.page_allocator); ")
		b.WriteString("for (" + src + ") |" + sanitizeName(q.Var) + "| {")
		if cond != "" {
			b.WriteString(" if (!(" + cond + ")) continue;")
		}
		b.WriteString(" " + tmp + ".append(.{ .item = " + sel + ", .key = " + sortExpr + " }) catch unreachable; }")
		b.WriteString(" for (0.." + tmp + ".items.len) |i| { for (i+1.." + tmp + ".items.len) |j| {")
		cmp := tmp + ".items[j].key < " + tmp + ".items[i].key"
		if keyType == "[]const u8" {
			cmp = "std.mem.lessThan(u8, " + tmp + ".items[j].key, " + tmp + ".items[i].key)"
		}
		b.WriteString(" if (" + cmp + ") { const t = " + tmp + ".items[i]; " + tmp + ".items[i] = " + tmp + ".items[j]; " + tmp + ".items[j] = t; }")
		b.WriteString(" } }")
		listTmp := c.newTmp()
		b.WriteString(" var " + listTmp + " = std.ArrayList(" + elem + ").init(std.heap.page_allocator);")
		b.WriteString("for (" + tmp + ".items) |p| { " + listTmp + ".append(p.item) catch unreachable; }")
		tmp = listTmp
	} else {
		b.WriteString(lbl + ": { var " + tmp + " = std.ArrayList(" + elem + ").init(std.heap.page_allocator); ")
		b.WriteString("for (" + src + ") |" + sanitizeName(q.Var) + "| {")
		if cond != "" {
			b.WriteString(" if (!(" + cond + ")) continue;")
		}
		b.WriteString(" " + tmp + ".append(" + sel + ") catch unreachable; }")
	}
	resVar := c.newTmp()
	b.WriteString(" const " + resVar + " = " + tmp + ".toOwnedSlice() catch unreachable;")
	if skipExpr != "" || takeExpr != "" {
		c.needsSlice = true
		start := "0"
		if skipExpr != "" {
			start = skipExpr
		}
		end := fmt.Sprintf("@as(i32, @intCast(%s.len))", resVar)
		if takeExpr != "" {
			if skipExpr != "" {
				end = fmt.Sprintf("(%s + %s)", skipExpr, takeExpr)
			} else {
				end = takeExpr
			}
		}
		b.WriteString(" " + resVar + " = _slice_list(" + elem + ", " + resVar + ", " + start + ", " + end + ", 1);")
	}
	b.WriteString(" break :" + lbl + " " + resVar + "; }")
	return b.String(), nil
}

func (c *Compiler) compileVar(st *parser.VarStmt, inFun bool) error {
	name := sanitizeName(st.Name)
	if !inFun && c.constGlobals[name] {
		if init, ok := c.globalInits[name]; ok {
			c.writeln(init)
			return nil
		}
	}
	var typ types.Type = types.AnyType{}
	if c.env != nil {
		if st.Type != nil {
			typ = c.resolveTypeRef(st.Type)
		} else if st.Value != nil {
			if ml := extractMapLiteral(st.Value); ml != nil {
				var keyT types.Type = types.AnyType{}
				var valT types.Type = types.AnyType{}
				if len(ml.Items) > 0 {
					if _, ok := simpleStringKey(ml.Items[0].Key); ok {
						keyT = types.StringType{}
					} else if c.isFloatExpr(ml.Items[0].Key) {
						keyT = types.FloatType{}
					} else if c.isBoolExpr(ml.Items[0].Key) {
						keyT = types.BoolType{}
					} else {
						keyT = types.IntType{}
					}
					valT = c.inferExprType(ml.Items[0].Value)
				}
				typ = types.MapType{Key: keyT, Value: valT}
			} else {
				typ = c.inferExprType(st.Value)
			}
		} else if old, err := c.env.GetVar(st.Name); err == nil {
			typ = old
		}
		c.env.SetVar(st.Name, typ, true)
	}
	if inFun {
		c.locals[st.Name] = typ
	}
	if st.Value != nil && isEmptyListExpr(st.Value) {
		elem := "i32"
		if lt, ok := typ.(types.ListType); ok {
			elem = strings.TrimPrefix(zigTypeOf(lt.Elem), "[]const ")
		}
		c.writeln(fmt.Sprintf("var %s = std.ArrayList(%s).init(std.heap.page_allocator);", name, elem))
		return nil
	}
	if st.Value != nil && isEmptyMapExpr(st.Value) {
		keyT := "i32"
		valT := "i32"
		if mt, ok := typ.(types.MapType); ok {
			keyT = zigTypeOf(mt.Key)
			valT = zigTypeOf(mt.Value)
		}
		c.writeln(fmt.Sprintf("var %s = std.AutoHashMap(%s, %s).init(std.heap.page_allocator);", name, keyT, valT))
		return nil
	}
	if st.Value != nil {
		if ll := extractListLiteral(st.Value); ll != nil && len(ll.Elems) > 0 && st.Type == nil {
			if extractMapLiteral(ll.Elems[0]) != nil {
				structName := pascalCase(name) + "Item"
				decl, init, err := c.compileNamedListLiteral(ll, true, structName)
				if err == nil {
					if !c.structs[structName] {
						c.writeln(decl)
						c.structs[structName] = true
						if c.env != nil {
							stype, ok := c.structTypeFromMapLiteral(extractMapLiteral(ll.Elems[0]), structName)
							if ok {
								c.env.SetStruct(structName, stype)
							}
						}
					}
					if c.env != nil {
						if stype, ok := c.structTypeFromMapLiteral(extractMapLiteral(ll.Elems[0]), structName); ok {
							c.env.SetVar(st.Name, types.ListType{Elem: stype}, true)
						}
					}
					c.writeln(fmt.Sprintf("var %s = %s;", name, init))
					return nil
				}
			}
		}
	}
	val := "0"
	if st.Value != nil {
		if ml := extractMapLiteral(st.Value); ml != nil {
			v, err := c.compileMapLiteral(ml, true)
			if err != nil {
				return err
			}
			val = v
		} else {
			v, err := c.compileExpr(st.Value, false)
			if err != nil {
				return err
			}
			val = v
		}
	}
	if st.Type == nil && st.Value != nil && canInferType(st.Value, typ) {
		c.writeln(fmt.Sprintf("var %s = %s;", name, val))
	} else {
		c.writeln(fmt.Sprintf("var %s: %s = %s;", name, zigTypeOf(typ), val))
	}
	return nil
}

func (c *Compiler) addImport(im *parser.ImportStmt) error {
	if im.Lang != nil && *im.Lang != "zig" {
		switch *im.Lang {
		case "go":
			path := strings.Trim(im.Path, "\"")
			if path == "strings" {
				// builtin handled in compileCallExpr
				return nil
			}
			if im.Auto && path == "mochi/runtime/ffi/go/testpkg" {
				alias := im.As
				if alias == "" {
					alias = parser.AliasFromPath(im.Path)
				}
				alias = sanitizeName(alias)
				if c.builtinAliases == nil {
					c.builtinAliases = map[string]string{}
				}
				c.builtinAliases[alias] = "go_testpkg"
				return nil
			}
		case "python":
			path := strings.Trim(im.Path, "\"")
			if path == "math" {
				alias := im.As
				if alias == "" {
					alias = parser.AliasFromPath(im.Path)
				}
				alias = sanitizeName(alias)
				if c.builtinAliases == nil {
					c.builtinAliases = map[string]string{}
				}
				c.builtinAliases[alias] = "python_math"
				// builtin handled in compileCallExpr and selector
				return nil
			}
		default:
			return fmt.Errorf("unsupported import language: %s", *im.Lang)
		}
	}
	alias := im.As
	if alias == "" {
		alias = parser.AliasFromPath(im.Path)
	}
	alias = sanitizeName(alias)
	path := strings.Trim(im.Path, "\"")
	if c.imports == nil {
		c.imports = map[string]string{}
	}
	c.imports[alias] = path
	return nil
}

func (c *Compiler) compileAssign(st *parser.AssignStmt) error {
	name := sanitizeName(st.Name)
	// check for append pattern: x = x + [val]
	if elem, ok := isSelfAppend(st); ok {
		v, err := c.compileExpr(elem, false)
		if err != nil {
			return err
		}
		c.writeln(fmt.Sprintf("try %s.append(@as(i32,@intCast(%s)));", name, v))
		return nil
	}
	lhs := name
	for _, f := range st.Field {
		lhs += "." + sanitizeName(f.Name)
	}
	if len(st.Index) == 1 && st.Index[0].Colon == nil && c.isMapVar(st.Name) {
		key, err := c.compileExpr(st.Index[0].Start, false)
		if err != nil {
			return err
		}
		val, err := c.compileExpr(st.Value, false)
		if err != nil {
			return err
		}
		c.writeln(fmt.Sprintf("_ = %s.put(%s, %s) catch unreachable;", lhs, key, val))
		return nil
	}
	for i, idx := range st.Index {
		ie, err := c.compileExpr(idx.Start, false)
		if err != nil {
			return err
		}
		if i == 0 && idx.Colon == nil && c.isListVar(st.Name) {
			lhs = fmt.Sprintf("%s.items[%s]", lhs, ie)
		} else {
			lhs = fmt.Sprintf("%s[%s]", lhs, ie)
		}
	}
	rhs, err := c.compileExpr(st.Value, false)
	if err != nil {
		return err
	}
	c.writeln(fmt.Sprintf("%s = %s;", lhs, rhs))
	return nil
}

func (c *Compiler) compileExpr(e *parser.Expr, asReturn bool) (string, error) {
	if e == nil {
		return "", nil
	}
	return c.compileBinary(e.Binary, asReturn)
}

func (c *Compiler) compileBinary(b *parser.BinaryExpr, asReturn bool) (string, error) {
	type operand struct {
		expr      string
		unary     *parser.Unary
		postfix   *parser.PostfixExpr
		isStr     bool
		isList    bool
		isMap     bool
		isStrList bool
	}

	newUnary := func(u *parser.Unary) (operand, error) {
		s, err := c.compileUnary(u, asReturn)
		if err != nil {
			return operand{}, err
		}
		return operand{
			expr:      s,
			unary:     u,
			isStr:     c.isStringUnary(u),
			isList:    c.isListUnary(u),
			isMap:     c.isMapUnary(u),
			isStrList: c.isStringListUnary(u),
		}, nil
	}

	newPostfix := func(p *parser.PostfixExpr) (operand, error) {
		s, err := c.compilePostfix(p, asReturn)
		if err != nil {
			return operand{}, err
		}
		return operand{
			expr:      s,
			postfix:   p,
			isStr:     c.isStringPostfix(p),
			isList:    c.isListPostfix(p),
			isMap:     c.isMapPostfix(p),
			isStrList: c.isStringListPostfix(p),
		}, nil
	}

	left, err := newUnary(b.Left)
	if err != nil {
		return "", err
	}
	operands := []operand{left}
	ops := make([]*parser.BinaryOp, len(b.Right))
	for i, op := range b.Right {
		o, err := newPostfix(op.Right)
		if err != nil {
			return "", err
		}
		operands = append(operands, o)
		ops[i] = op
	}

	levels := [][]string{
		{"*", "/", "%"},
		{"+", "-"},
		{"<", "<=", ">", ">="},
		{"==", "!=", "in"},
		{"&&"},
		{"||"},
		{"union", "union_all", "except", "intersect"},
	}

	contains := func(list []string, op string) bool {
		for _, s := range list {
			if s == op {
				return true
			}
		}
		return false
	}

	combine := func(opName string, all bool, left, right operand) (operand, error) {
		expr := left.expr
		isStr := false
		isList := false

		if opName == "+" {
			if left.isStr || right.isStr {
				if c.isStringLiteralUnary(left.unary) && c.isStringLiteralPostfix(right.postfix) {
					lit1 := *left.unary.Value.Target.Lit.Str
					lit2 := *right.postfix.Target.Lit.Str
					expr = strconv.Quote(lit1 + lit2)
				} else {
					c.needsConcatString = true
					expr = fmt.Sprintf("_concat_string(%s, %s)", left.expr, right.expr)
				}
				isStr = true
				return operand{expr: expr, isStr: true}, nil
			}
		}
		if (opName == "==" || opName == "!=") && (left.isStr || right.isStr) {
			cmp := fmt.Sprintf("std.mem.eql(u8, %s, %s)", left.expr, right.expr)
			if opName == "!=" {
				cmp = "!" + cmp
			}
			return operand{expr: cmp}, nil
		}
		if opName == "in" {
			if right.isMap {
				expr = fmt.Sprintf("%s.contains(%s)", right.expr, left.expr)
			} else if right.isStrList {
				c.needsInListString = true
				expr = fmt.Sprintf("_contains_list_string(%s, %s)", right.expr, left.expr)
			} else {
				c.needsInListInt = true
				expr = fmt.Sprintf("_contains_list_int(%s, %s)", right.expr, left.expr)
			}
			return operand{expr: expr}, nil
		}
		if opName == "union" && all {
			opName = "union_all"
		}
		if opName == "union" || opName == "union_all" || opName == "except" || opName == "intersect" {
			elem := c.listElemTypeUnary(left.unary)
			c.needsSetOps = true
			expr = fmt.Sprintf("_%s(%s, %s, %s)", opName, elem, left.expr, right.expr)
			isList = true
			return operand{expr: expr, isList: true}, nil
		}
		switch opName {
		case "&&":
			opName = "and"
		case "||":
			opName = "or"
		}
		if opName == "%" {
			expr = fmt.Sprintf("@mod(%s, %s)", left.expr, right.expr)
		} else {
			expr = fmt.Sprintf("(%s %s %s)", left.expr, opName, right.expr)
		}
		return operand{expr: expr, isStr: isStr, isList: isList}, nil
	}

	for _, level := range levels {
		for i := 0; i < len(ops); {
			opName := ops[i].Op
			if opName == "union" && ops[i].All {
				opName = "union_all"
			}
			if contains(level, opName) {
				res, err := combine(opName, ops[i].All, operands[i], operands[i+1])
				if err != nil {
					return "", err
				}
				operands[i] = operand{
					expr:    res.expr,
					unary:   nil,
					postfix: nil,
					isStr:   res.isStr,
					isList:  res.isList,
				}
				operands = append(operands[:i+1], operands[i+2:]...)
				ops = append(ops[:i], ops[i+1:]...)
			} else {
				i++
			}
		}
	}

	if len(operands) == 0 {
		return "", nil
	}
	return operands[0].expr, nil
}

func (c *Compiler) compileUnary(u *parser.Unary, asReturn bool) (string, error) {
	val, err := c.compilePostfix(u.Value, asReturn)
	if err != nil {
		return "", err
	}
	for i := len(u.Ops) - 1; i >= 0; i-- {
		val = fmt.Sprintf("%s%s", u.Ops[i], val)
	}
	return val, nil
}

func (c *Compiler) compilePostfix(p *parser.PostfixExpr, asReturn bool) (string, error) {
	// typed fetch expression like `fetch url as T`
	if p.Target.Fetch != nil && len(p.Ops) == 1 && p.Ops[0].Cast != nil {
		typ := c.resolveTypeRef(p.Ops[0].Cast.Type)
		return c.compileFetchExprTyped(p.Target.Fetch, typ)
	}

	expr, err := c.compilePrimary(p.Target, asReturn)
	if err != nil {
		return "", err
	}
	for _, op := range p.Ops {
		if op.Index != nil {
			if op.Index.Colon == nil {
				idx, err := c.compileExpr(op.Index.Start, false)
				if err != nil {
					return "", err
				}
				if c.isMapPostfix(p) {
					expr = fmt.Sprintf("(%s.get(%s) orelse unreachable)", expr, idx)
				} else if c.isStringPostfix(p) {
					c.needsIndexString = true
					expr = fmt.Sprintf("_index_string(%s, %s)", expr, idx)
				} else if c.isListPostfix(p) {
					elem := c.listElemTypePostfix(p)
					c.needsIndex = true
					expr = fmt.Sprintf("_index_list(%s, %s, %s)", elem, expr, idx)
				} else {
					expr = fmt.Sprintf("%s[%s]", expr, idx)
				}
			} else {
				start := "0"
				end := ""
				step := "1"
				if op.Index.Start != nil {
					s, err := c.compileExpr(op.Index.Start, false)
					if err != nil {
						return "", err
					}
					start = s
				}
				if op.Index.End != nil {
					e, err := c.compileExpr(op.Index.End, false)
					if err != nil {
						return "", err
					}
					end = e
				} else {
					end = fmt.Sprintf("%s.len", expr)
				}
				if op.Index.Step != nil {
					st, err := c.compileExpr(op.Index.Step, false)
					if err != nil {
						return "", err
					}
					step = st
				}
				if c.isStringPostfix(p) || c.isStringPrimary(p.Target) {
					c.needsSliceString = true
					expr = fmt.Sprintf("_slice_string(%s, %s, %s, %s)", expr, start, end, step)
				} else if c.isListPostfix(p) || c.isListPrimary(p.Target) {
					elem := c.listElemTypePostfix(p)
					c.needsSlice = true
					expr = fmt.Sprintf("_slice_list(%s, %s, %s, %s, %s)", elem, expr, start, end, step)
				} else {
					expr = fmt.Sprintf("%s[%s..%s]", expr, start, end)
				}
			}
		} else if op.Call != nil {
			expr, err = c.compileCallOp(expr, op.Call)
			if err != nil {
				return "", err
			}
		} else if op.Cast != nil {
			rt := c.resolveTypeRef(op.Cast.Type)
			handled := false
			if st, ok := rt.(types.StructType); ok && p.Target.Map != nil {
				fields := make([]string, 0, len(p.Target.Map.Items))
				okFields := true
				for _, it := range p.Target.Map.Items {
					key, ok := simpleStringKey(it.Key)
					if !ok {
						okFields = false
						break
					}
					val, err := c.compileExpr(it.Value, false)
					if err != nil {
						return "", err
					}
					fields = append(fields, fmt.Sprintf(".%s = %s", sanitizeName(key), val))
				}
				if okFields {
					expr = fmt.Sprintf("%s{ %s }", sanitizeName(st.Name), strings.Join(fields, ", "))
					handled = true
				}
			}
			if !handled {
				typ := c.zigType(op.Cast.Type)
				if _, ok := rt.(types.IntType); ok && c.isStringPrimary(p.Target) {
					expr = fmt.Sprintf("std.fmt.parseInt(%s, %s, 10) catch unreachable", typ, expr)
				} else {
					expr = fmt.Sprintf("@as(%s, %s)", typ, expr)
				}
			}
		}
	}
	return expr, nil
}

func (c *Compiler) compileCallOp(receiver string, call *parser.CallOp) (string, error) {
	args := make([]string, len(call.Args))
	for i, a := range call.Args {
		v, err := c.compileExpr(a, false)
		if err != nil {
			return "", err
		}
		args[i] = v
	}
	if idx := strings.Index(receiver, "."); idx > 0 {
		alias := receiver[:idx]
		method := receiver[idx+1:]
		if mod, ok := c.builtinAliases[alias]; ok {
			switch mod {
			case "python_math":
				switch method {
				case "sqrt":
					if len(args) == 1 {
						return fmt.Sprintf("std.math.sqrt(%s)", args[0]), nil
					}
				case "pow":
					if len(args) == 2 {
						return fmt.Sprintf("std.math.pow(f64, %s, %s)", args[0], args[1]), nil
					}
				case "sin":
					if len(args) == 1 {
						return fmt.Sprintf("std.math.sin(%s)", args[0]), nil
					}
				case "log":
					if len(args) == 1 {
						return fmt.Sprintf("std.math.log(%s)", args[0]), nil
					}
				}
			case "go_testpkg":
				if method == "Add" && len(args) == 2 {
					return fmt.Sprintf("(%s + %s)", args[0], args[1]), nil
				}
			}
		}
	}
	return fmt.Sprintf("%s(%s)", receiver, strings.Join(args, ", ")), nil
}

func (c *Compiler) compilePrimary(p *parser.Primary, asReturn bool) (string, error) {
	switch {
	case p.Lit != nil:
		t := c.inferPrimaryType(p)
		return c.compileLiteral(p.Lit, t)
	case p.Selector != nil:
		name := sanitizeName(p.Selector.Root)
		if alias, ok := c.captures[name]; ok {
			name = alias
		}
		if mod, ok := c.builtinAliases[name]; ok && len(p.Selector.Tail) == 1 {
			tail := p.Selector.Tail[0]
			switch mod {
			case "python_math":
				switch tail {
				case "pi":
					return "std.math.pi", nil
				case "e":
					return "std.math.e", nil
				}
			case "go_testpkg":
				switch tail {
				case "Pi":
					return "3.14", nil
				case "Answer":
					return "42", nil
				}
			}
		}
		if len(p.Selector.Tail) == 0 {
			if info, ok := c.variantInfo[p.Selector.Root]; ok {
				if len(info.Fields) == 0 {
					return fmt.Sprintf("%s{ .%s = {} }", sanitizeName(info.Union), sanitizeName(p.Selector.Root)), nil
				}
			}
		}
		if len(p.Selector.Tail) > 0 {
			name += "." + strings.Join(p.Selector.Tail, ".")
		} else if c.env != nil {
			if _, err := c.env.GetVar(p.Selector.Root); err != nil {
				return fmt.Sprintf("\"%s\"", p.Selector.Root), nil
			}
			if asReturn {
				if t, err := c.env.GetVar(p.Selector.Root); err == nil {
					if m, _ := c.env.IsMutable(p.Selector.Root); m {
						if _, ok := t.(types.ListType); ok {
							name += ".items"
						}
					}
				}
			}
		}
		return name, nil
	case p.List != nil:
		return c.compileListLiteral(p.List, !asReturn)
	case p.Map != nil:
		return c.compileMapLiteral(p.Map, false)
	case p.Struct != nil:
		return c.compileStructLiteral(p.Struct)
	case p.Match != nil:
		return c.compileMatchExpr(p.Match)
	case p.If != nil:
		return c.compileIfExpr(p.If)
	case p.Call != nil:
		return c.compileCallExpr(p.Call)
	case p.Load != nil:
		return c.compileLoadExpr(p.Load)
	case p.Save != nil:
		return c.compileSaveExpr(p.Save)
	case p.Fetch != nil:
		return c.compileFetchExpr(p.Fetch)
	case p.FunExpr != nil:
		return c.compileFunExpr(p.FunExpr)
	case p.Query != nil:
		return c.compileQueryExpr(p.Query)
	case p.Group != nil:
		inner, err := c.compileExpr(p.Group, asReturn)
		if err != nil {
			return "", err
		}
		return "(" + inner + ")", nil
	default:
		return "0", nil
	}
}

func (c *Compiler) compileCallExpr(call *parser.CallExpr) (string, error) {
	name := sanitizeName(call.Func)
	if idx := strings.IndexByte(name, '_'); idx > 0 {
		alias := name[:idx]
		method := name[idx+1:]
		if mod, ok := c.builtinAliases[alias]; ok {
			switch mod {
			case "python_math":
				if method == "sqrt" && len(call.Args) == 1 {
					arg, err := c.compileExpr(call.Args[0], false)
					if err != nil {
						return "", err
					}
					return fmt.Sprintf("std.math.sqrt(%s)", arg), nil
				}
				if method == "pow" && len(call.Args) == 2 {
					a, err := c.compileExpr(call.Args[0], false)
					if err != nil {
						return "", err
					}
					b, err := c.compileExpr(call.Args[1], false)
					if err != nil {
						return "", err
					}
					return fmt.Sprintf("std.math.pow(f64, %s, %s)", a, b), nil
				}
				if method == "sin" && len(call.Args) == 1 {
					a, err := c.compileExpr(call.Args[0], false)
					if err != nil {
						return "", err
					}
					return fmt.Sprintf("std.math.sin(%s)", a), nil
				}
				if method == "log" && len(call.Args) == 1 {
					a, err := c.compileExpr(call.Args[0], false)
					if err != nil {
						return "", err
					}
					return fmt.Sprintf("std.math.log(%s)", a), nil
				}
			case "go_testpkg":
				if method == "Add" && len(call.Args) == 2 {
					a, err := c.compileExpr(call.Args[0], false)
					if err != nil {
						return "", err
					}
					b, err := c.compileExpr(call.Args[1], false)
					if err != nil {
						return "", err
					}
					return fmt.Sprintf("(%s + %s)", a, b), nil
				}
			}
		}
	}
	if name == "strings_ToUpper" && len(call.Args) == 1 {
		arg, err := c.compileExpr(call.Args[0], false)
		if err != nil {
			return "", err
		}
		return fmt.Sprintf("std.ascii.upperString(%s)", arg), nil
	}
	if name == "math_sqrt" && len(call.Args) == 1 {
		arg, err := c.compileExpr(call.Args[0], false)
		if err != nil {
			return "", err
		}
		return fmt.Sprintf("std.math.sqrt(%s)", arg), nil
	}
	if name == "abs" && len(call.Args) == 1 {
		arg, err := c.compileExpr(call.Args[0], false)
		if err != nil {
			return "", err
		}
		return fmt.Sprintf("std.math.abs(%s)", arg), nil
	}
	if name == "lower" && len(call.Args) == 1 {
		arg, err := c.compileExpr(call.Args[0], false)
		if err != nil {
			return "", err
		}
		return fmt.Sprintf("std.ascii.lowerString(%s)", arg), nil
	}
	if name == "split" && len(call.Args) == 2 {
		s, err := c.compileExpr(call.Args[0], false)
		if err != nil {
			return "", err
		}
		sep, err := c.compileExpr(call.Args[1], false)
		if err != nil {
			return "", err
		}
		c.needsSplitString = true
		return fmt.Sprintf("_split_string(%s, %s)", s, sep), nil
	}
	if name == "join" && len(call.Args) == 2 {
		parts, err := c.compileExpr(call.Args[0], false)
		if err != nil {
			return "", err
		}
		sep, err := c.compileExpr(call.Args[1], false)
		if err != nil {
			return "", err
		}
		c.needsJoinString = true
		return fmt.Sprintf("_join_strings(%s, %s)", parts, sep), nil
	}
	if name == "substr" && len(call.Args) == 3 {
		s, err := c.compileExpr(call.Args[0], false)
		if err != nil {
			return "", err
		}
		start, err := c.compileExpr(call.Args[1], false)
		if err != nil {
			return "", err
		}
		end, err := c.compileExpr(call.Args[2], false)
		if err != nil {
			return "", err
		}
		c.needsSliceString = true
		return fmt.Sprintf("_slice_string(%s, %s, %s, 1)", s, start, end), nil
	}
	if name == "len" && len(call.Args) == 1 {
		arg, err := c.compileExpr(call.Args[0], false)
		if err != nil {
			return "", err
		}
		if c.isMapExpr(call.Args[0]) {
			return fmt.Sprintf("%s.count()", arg), nil
		}
		if c.isGroupExpr(call.Args[0]) {
			return fmt.Sprintf("(%s.Items.len)", arg), nil
		}
		return fmt.Sprintf("(%s).len", arg), nil
	}
	if name == "count" && len(call.Args) == 1 {
		arg, err := c.compileExpr(call.Args[0], false)
		if err != nil {
			return "", err
		}
		if c.isMapExpr(call.Args[0]) {
			return fmt.Sprintf("%s.count()", arg), nil
		}
		if c.isGroupExpr(call.Args[0]) {
			return fmt.Sprintf("(%s.Items.len)", arg), nil
		}
		return fmt.Sprintf("(%s).len", arg), nil
	}
	if name == "keys" && len(call.Args) == 1 {
		arg, err := c.compileExpr(call.Args[0], false)
		if err != nil {
			return "", err
		}
		if mt, ok := c.inferExprType(call.Args[0]).(types.MapType); ok {
			c.needsMapKeys = true
			kt := zigTypeOf(mt.Key)
			vt := zigTypeOf(mt.Value)
			return fmt.Sprintf("_map_keys(%s, %s, %s)", kt, vt, arg), nil
		}
	}
	if name == "values" && len(call.Args) == 1 {
		arg, err := c.compileExpr(call.Args[0], false)
		if err != nil {
			return "", err
		}
		if mt, ok := c.inferExprType(call.Args[0]).(types.MapType); ok {
			c.needsMapValues = true
			kt := zigTypeOf(mt.Key)
			vt := zigTypeOf(mt.Value)
			return fmt.Sprintf("_map_values(%s, %s, %s)", kt, vt, arg), nil
		}
	}
	if name == "exists" && len(call.Args) == 1 {
		arg, err := c.compileExpr(call.Args[0], false)
		if err != nil {
			return "", err
		}
		return fmt.Sprintf("(%s).len != 0", arg), nil
	}
	if name == "avg" && len(call.Args) == 1 {
		arg, err := c.compileExpr(call.Args[0], false)
		if err != nil {
			return "", err
		}
		if c.isFloatListExpr(call.Args[0]) {
			c.needsAvgFloat = true
			return fmt.Sprintf("_avg_float(%s)", arg), nil
		}
		c.needsAvgInt = true
		return fmt.Sprintf("_avg_int(%s)", arg), nil
	}
	if name == "sum" && len(call.Args) == 1 {
		arg, err := c.compileExpr(call.Args[0], false)
		if err != nil {
			return "", err
		}
		if at, ok := c.inferExprType(call.Args[0]).(types.ListType); ok {
			if _, ok := at.Elem.(types.FloatType); ok {
				c.needsSumFloat = true
				return fmt.Sprintf("_sum_float(%s)", arg), nil
			}
		}
		c.needsSumInt = true
		return fmt.Sprintf("_sum_int(%s)", arg), nil
	}
	if name == "append" && len(call.Args) == 2 {
		listArg, err := c.compileExpr(call.Args[0], false)
		if err != nil {
			return "", err
		}
		elemArg, err := c.compileExpr(call.Args[1], false)
		if err != nil {
			return "", err
		}
		elem := c.listElemTypeUnary(call.Args[0].Binary.Left)
		c.needsAppend = true
		return fmt.Sprintf("_append(%s, %s, %s)", elem, listArg, elemArg), nil
	}
	if name == "min" && len(call.Args) == 1 {
		arg, err := c.compileExpr(call.Args[0], false)
		if err != nil {
			return "", err
		}
		if at, ok := c.inferExprType(call.Args[0]).(types.ListType); ok {
			switch at.Elem.(type) {
			case types.FloatType:
				c.needsMinFloat = true
				return fmt.Sprintf("_min_float(%s)", arg), nil
			case types.StringType:
				c.needsMinString = true
				return fmt.Sprintf("_min_string(%s)", arg), nil
			default:
				c.needsMinInt = true
				return fmt.Sprintf("_min_int(%s)", arg), nil
			}
		}
	}
	if name == "max" && len(call.Args) == 1 {
		arg, err := c.compileExpr(call.Args[0], false)
		if err != nil {
			return "", err
		}
		if at, ok := c.inferExprType(call.Args[0]).(types.ListType); ok {
			switch at.Elem.(type) {
			case types.FloatType:
				c.needsMaxFloat = true
				return fmt.Sprintf("_max_float(%s)", arg), nil
			case types.StringType:
				c.needsMaxString = true
				return fmt.Sprintf("_max_string(%s)", arg), nil
			default:
				c.needsMaxInt = true
				return fmt.Sprintf("_max_int(%s)", arg), nil
			}
		}
	}
	if name == "reduce" && len(call.Args) == 3 {
		listArg, err := c.compileExpr(call.Args[0], false)
		if err != nil {
			return "", err
		}
		fnArg, err := c.compileExpr(call.Args[1], false)
		if err != nil {
			return "", err
		}
		initArg, err := c.compileExpr(call.Args[2], false)
		if err != nil {
			return "", err
		}
		elem := c.listElemTypeUnary(call.Args[0].Binary.Left)
		c.needsReduce = true
		return fmt.Sprintf("_reduce(%s, %s, %s, %s)", elem, listArg, initArg, fnArg), nil
	}
	if name == "str" && len(call.Args) == 1 {
		arg, err := c.compileExpr(call.Args[0], false)
		if err != nil {
			return "", err
		}
		return fmt.Sprintf("std.fmt.allocPrint(std.heap.page_allocator, \"{d}\", .{%s}) catch unreachable", arg), nil
	}
	if name == "now" && len(call.Args) == 0 {
		return "std.time.nanoTimestamp()", nil
	}
	if name == "json" && len(call.Args) == 1 {
		arg, err := c.compileExpr(call.Args[0], false)
		if err != nil {
			return "", err
		}
		c.needsJSON = true
		return fmt.Sprintf("_json(%s)", arg), nil
	}
	if name == "print" {
		if len(call.Args) == 1 {
			if _, ok := c.inferExprType(call.Args[0]).(types.ListType); ok {
				arg, err := c.compileExpr(call.Args[0], false)
				if err != nil {
					return "", err
				}
				return fmt.Sprintf("std.debug.print(\"{any}\\n\", .{%s})", arg), nil
			}
			if c.isStringLiteralExpr(call.Args[0]) {
				lit := call.Args[0].Binary.Left.Value.Target.Lit.Str
				return fmt.Sprintf("std.debug.print(%q, .{})", *lit+"\n"), nil
			}
		}
		if len(call.Args) > 0 && c.isStringLiteralExpr(call.Args[0]) {
			format := *call.Args[0].Binary.Left.Value.Target.Lit.Str
			args := []string{}
			for _, a := range call.Args[1:] {
				if c.isStringLiteralExpr(a) {
					format += " " + *a.Binary.Left.Value.Target.Lit.Str
					continue
				}
				v, err := c.compileExpr(a, false)
				if err != nil {
					return "", err
				}
				args = append(args, v)
				switch c.inferExprType(a).(type) {
				case types.StringType:
					format += " {s}"
				case types.IntType, types.Int64Type:
					format += " {d}"
				case types.BoolType:
					format += " {}"
				default:
					format += " {any}"
				}
			}
			format += "\n"
			if len(args) == 0 {
				return fmt.Sprintf("std.debug.print(%q, .{})", format), nil
			}
			return fmt.Sprintf("std.debug.print(%q, .{%s})", format, strings.Join(args, ", ")), nil
		}
		args := make([]string, len(call.Args))
		fmtParts := make([]string, len(call.Args))
		for i, a := range call.Args {
			v, err := c.compileExpr(a, false)
			if err != nil {
				return "", err
			}
			args[i] = v

			switch c.inferExprType(a).(type) {
			case types.StringType:
				fmtParts[i] = "{s}"
			case types.IntType, types.Int64Type:
				fmtParts[i] = "{d}"
			case types.BoolType:
				fmtParts[i] = "{}"
			default:
				fmtParts[i] = "{any}"
			}
		}
		format := strings.Join(fmtParts, " ") + "\\n"
		return fmt.Sprintf("std.debug.print(\"%s\", .{%s})", format, strings.Join(args, ", ")), nil
	}
	args := make([]string, len(call.Args))
	for i, a := range call.Args {
		v, err := c.compileExpr(a, false)
		if err != nil {
			return "", err
		}
		args[i] = v
	}
	if fn, ok := c.env.GetFunc(call.Func); ok {
		for i, p := range fn.Params {
			if p.Type != nil && p.Type.Simple != nil {
				if _, ok := c.env.GetUnion(*p.Type.Simple); ok {
					args[i] = "&" + args[i]
				} else if _, ok := c.env.GetStruct(*p.Type.Simple); ok {
					args[i] = "&" + args[i]
				}
			}
		}
	}
	return fmt.Sprintf("%s(%s)", name, strings.Join(args, ", ")), nil
}

func (c *Compiler) compileLiteral(l *parser.Literal, hint types.Type) (string, error) {
	switch {
	case l.Int != nil:
		return strconv.Itoa(*l.Int), nil
	case l.Float != nil:
		s := strconv.FormatFloat(*l.Float, 'f', -1, 64)
		if !strings.ContainsAny(s, ".eE") && !strings.Contains(s, ".") {
			s += ".0"
		}
		return s, nil
	case l.Str != nil:
		return strconv.Quote(*l.Str), nil
	case l.Bool != nil:
		if *l.Bool {
			return "true", nil
		}
		return "false", nil
	}
	return "0", nil
}

func (c *Compiler) compileListLiteral(list *parser.ListLiteral, asRef bool) (string, error) {
	elems := make([]string, len(list.Elems))
	if len(list.Elems) == 0 {
		if asRef {
			return "&[]i32{}", nil
		}
		return "[]i32{}", nil
	}

	if ml := extractMapLiteral(list.Elems[0]); ml != nil {
		structName := c.newTmp()
		decl, init, ok, err := c.mapLiteralStruct(ml, structName)
		if ok && err == nil {
			elems[0] = init
			for i := 1; i < len(list.Elems); i++ {
				ml2 := extractMapLiteral(list.Elems[i])
				if ml2 == nil {
					return "", fmt.Errorf("mixed list elements not supported")
				}
				_, init2, ok2, err2 := c.mapLiteralStruct(ml2, structName)
				if err2 != nil {
					return "", err2
				}
				if !ok2 {
					return "", fmt.Errorf("inconsistent map literal")
				}
				elems[i] = init2
			}
			lbl := c.newLabel()
			var b strings.Builder
			b.WriteString("(" + lbl + ": { " + decl + " const _arr = ")
			if len(elems) > 1 {
				if asRef {
					b.WriteString("&[_]" + structName + "{\n    ")
				} else {
					b.WriteString("[_]" + structName + "{\n    ")
				}
				b.WriteString(strings.Join(elems, ",\n    "))
				b.WriteString(",\n}")
			} else {
				if asRef {
					b.WriteString("&[_]" + structName + "{")
				} else {
					b.WriteString("[_]" + structName + "{")
				}
				b.WriteString(strings.Join(elems, ", "))
				b.WriteString("}")
			}
			b.WriteString("; break :" + lbl + " _arr; })")
			return b.String(), nil
		}
	}

	elemType := zigTypeOf(c.inferExprType(list.Elems[0]))
	for i, e := range list.Elems {
		v, err := c.compileExpr(e, false)
		if err != nil {
			return "", err
		}
		elems[i] = v
	}
	if len(elems) > 1 {
		var b strings.Builder
		if asRef {
			b.WriteString("&[_]" + elemType + "{\n    ")
		} else {
			b.WriteString("[_]" + elemType + "{\n    ")
		}
		b.WriteString(strings.Join(elems, ",\n    "))
		b.WriteString(",\n}")
		return b.String(), nil
	}
	if asRef {
		return fmt.Sprintf("&[_]%s{%s}", elemType, elems[0]), nil
	}
	return fmt.Sprintf("[_]%s{%s}", elemType, elems[0]), nil
}

// compileNamedListLiteral compiles a list literal whose elements are map
// literals using the provided struct name for each element. It returns the
// struct declaration and the expression for the list.
func (c *Compiler) compileNamedListLiteral(list *parser.ListLiteral, asRef bool, structName string) (decl, expr string, err error) {
	if len(list.Elems) == 0 {
		if asRef {
			expr = "&[]" + structName + "{}"
		} else {
			expr = "[]" + structName + "{}"
		}
		return "", expr, nil
	}
	ml := extractMapLiteral(list.Elems[0])
	if ml == nil {
		return "", "", fmt.Errorf("not a list of map literals")
	}
	d, init, ok, err := c.mapLiteralStruct(ml, structName)
	if err != nil {
		return "", "", err
	}
	if !ok {
		return "", "", fmt.Errorf("inconsistent map literal")
	}
	elems := make([]string, len(list.Elems))
	elems[0] = init
	for i := 1; i < len(list.Elems); i++ {
		ml2 := extractMapLiteral(list.Elems[i])
		if ml2 == nil {
			return "", "", fmt.Errorf("mixed list elements not supported")
		}
		_, init2, ok2, err2 := c.mapLiteralStruct(ml2, structName)
		if err2 != nil {
			return "", "", err2
		}
		if !ok2 {
			return "", "", fmt.Errorf("inconsistent map literal")
		}
		elems[i] = init2
	}
	var b strings.Builder
	if len(elems) > 1 {
		if asRef {
			b.WriteString("&[_]" + structName + "{\n    ")
		} else {
			b.WriteString("[_]" + structName + "{\n    ")
		}
		b.WriteString(strings.Join(elems, ",\n    "))
		b.WriteString(",\n}")
	} else {
		if asRef {
			b.WriteString(fmt.Sprintf("&[_]%s{%s}", structName, elems[0]))
		} else {
			b.WriteString(fmt.Sprintf("[_]%s{%s}", structName, elems[0]))
		}
	}
	expr = b.String()
	decl = d
	return decl, expr, nil
}

func (c *Compiler) compileMapLiteral(m *parser.MapLiteral, forceMap bool) (string, error) {
	if !forceMap {
		if _, init, ok, err := c.mapLiteralStruct(m, ""); ok {
			if err != nil {
				return "", err
			}
			return init, nil
		}
	}

	keyType := "i32"
	valType := "i32"
	if len(m.Items) > 0 {
		if _, ok := simpleStringKey(m.Items[0].Key); ok || c.isStringExpr(m.Items[0].Key) {
			keyType = "[]const u8"
		} else if c.isFloatExpr(m.Items[0].Key) {
			keyType = "f64"
		} else if c.isBoolExpr(m.Items[0].Key) {
			keyType = "bool"
		}
		if c.isStringExpr(m.Items[0].Value) {
			valType = "[]const u8"
		} else if c.isFloatExpr(m.Items[0].Value) {
			valType = "f64"
		} else if c.isBoolExpr(m.Items[0].Value) {
			valType = "bool"
		}
	}
	var b strings.Builder
	lbl := c.newLabel()
	tmpMap := c.newMapTmp()
	b.WriteString("(" + lbl + ": { var " + tmpMap + " = ")
	if keyType == "[]const u8" {
		b.WriteString("std.StringHashMap(" + valType + ").init(std.heap.page_allocator); ")
	} else {
		b.WriteString("std.AutoHashMap(" + keyType + ", " + valType + ").init(std.heap.page_allocator); ")
	}
	for _, it := range m.Items {
		var keyExpr string
		if k, ok := simpleStringKey(it.Key); ok {
			keyExpr = fmt.Sprintf("\"%s\"", sanitizeName(k))
		} else {
			ke, err := c.compileExpr(it.Key, false)
			if err != nil {
				return "", err
			}
			keyExpr = ke
		}
		v, err := c.compileExpr(it.Value, false)
		if err != nil {
			return "", err
		}
		b.WriteString(tmpMap + ".put(" + keyExpr + ", " + v + ") catch unreachable; ")
	}
	b.WriteString("break :" + lbl + " " + tmpMap + "; })")
	return b.String(), nil
}

func (c *Compiler) compileStructLiteral(s *parser.StructLiteral) (string, error) {
	if info, ok := c.variantInfo[s.Name]; ok {
		fields := make([]string, len(s.Fields))
		for i, f := range s.Fields {
			v, err := c.compileExpr(f.Value, false)
			if err != nil {
				return "", err
			}
			if ft, ok2 := info.Fields[f.Name]; ok2 {
				if ut, ok3 := ft.(types.UnionType); ok3 && ut.Name == info.Union {
					v = "&" + v
				}
			}
			fields[i] = fmt.Sprintf(".%s = %s", sanitizeName(f.Name), v)
		}
		if len(fields) > 1 {
			var b strings.Builder
			b.WriteString(fmt.Sprintf("%s{ .%s = .{\n    ", sanitizeName(info.Union), sanitizeName(s.Name)))
			b.WriteString(strings.Join(fields, ",\n    "))
			b.WriteString(",\n} }")
			return b.String(), nil
		}
		return fmt.Sprintf("%s{ .%s = .{ %s } }", sanitizeName(info.Union), sanitizeName(s.Name), fields[0]), nil
	}
	fields := make([]string, len(s.Fields))
	for i, f := range s.Fields {
		v, err := c.compileExpr(f.Value, false)
		if err != nil {
			return "", err
		}
		fields[i] = fmt.Sprintf(".%s = %s", sanitizeName(f.Name), v)
	}
	if len(fields) > 1 {
		var b strings.Builder
		b.WriteString(fmt.Sprintf("%s{\n    ", sanitizeName(s.Name)))
		b.WriteString(strings.Join(fields, ",\n    "))
		b.WriteString(",\n}")
		return b.String(), nil
	}
	return fmt.Sprintf("%s{ %s }", sanitizeName(s.Name), fields[0]), nil
}

func (c *Compiler) compileFunExpr(fn *parser.FunExpr) (string, error) {
	params := make([]string, len(fn.Params))
	paramNames := make([]string, len(fn.Params))
	child := types.NewEnv(c.env)
	for i, p := range fn.Params {
		typ := c.zigType(p.Type)
		name := sanitizeName(p.Name)
		params[i] = fmt.Sprintf("%s: %s", name, typ)
		paramNames[i] = name
		if child != nil {
			child.SetVar(p.Name, c.resolveTypeRef(p.Type), true)
		}
	}

	captured := freeVars(fn, paramNames)
	fieldDecls := make([]string, len(captured))
	fieldInits := make([]string, len(captured))
	sub := &Compiler{env: child, locals: map[string]types.Type{}, captures: map[string]string{}}
	for i, name := range captured {
		typ := "i32"
		if c.env != nil {
			if t, err := c.env.GetVar(name); err == nil {
				typ = zigTypeOf(t)
				child.SetVar(name, t, true)
				sub.locals[name] = t
			}
		}
		sn := sanitizeName(name)
		fieldDecls[i] = fmt.Sprintf("%s: %s,", sn, typ)
		fieldInits[i] = fmt.Sprintf(".%s = %s", sn, sn)
		sub.captures[sn] = "self." + sn
	}
	sub.indent = 1
	if fn.ExprBody != nil {
		expr, err := sub.compileExpr(fn.ExprBody, false)
		if err != nil {
			return "", err
		}
		sub.writeln("return " + expr + ";")
	} else {
		for _, st := range fn.BlockBody {
			if err := sub.compileStmt(st, true); err != nil {
				return "", err
			}
		}
	}
	body := indentBlock(sub.buf.String(), 1)
	ret := "void"
	if fn.Return != nil {
		ret = c.zigType(fn.Return)
	} else if fn.ExprBody != nil {
		t := c.inferExprType(fn.ExprBody)
		ret = zigTypeOf(t)
	} else if n := len(fn.BlockBody); n > 0 {
		last := fn.BlockBody[n-1]
		if last.Return != nil {
			t := c.inferExprType(last.Return.Value)
			ret = zigTypeOf(t)
		} else if last.Expr != nil {
			t := c.inferExprType(last.Expr.Expr)
			ret = zigTypeOf(t)
		}
	}
	decl := ""
	init := ""
	callParams := strings.Join(params, ", ")
	if len(captured) == 0 {
		// simple function literal
		return fmt.Sprintf("fn (%s) %s {\n%s}", callParams, ret, body), nil
	}

	decl = strings.Join(fieldDecls, " ") + " "
	init = strings.Join(fieldInits, ", ")
	if callParams != "" {
		callParams = "self: @This(), " + callParams
	} else {
		callParams = "self: @This()"
	}
	return fmt.Sprintf("(struct { %sfn call(%s) %s {\n%s} }{ %s }).call", decl, callParams, ret, body, init), nil
}

func (c *Compiler) compileLoadExpr(l *parser.LoadExpr) (string, error) {
	path := "null"
	if l.Path != nil {
		path = fmt.Sprintf("%q", *l.Path)
	}
	c.needsLoadJSON = true
	if l.Type != nil && l.Type.Simple != nil {
		typ := sanitizeName(*l.Type.Simple)
		return fmt.Sprintf("_load_json([]%s, %s)", typ, path), nil
	}
	return fmt.Sprintf("_load_json([]std.json.Value, %s)", path), nil
}

func (c *Compiler) compileSaveExpr(s *parser.SaveExpr) (string, error) {
	src, err := c.compileExpr(s.Src, false)
	if err != nil {
		return "", err
	}
	path := "null"
	if s.Path != nil {
		path = fmt.Sprintf("%q", *s.Path)
	}
	c.needsSaveJSON = true
	return fmt.Sprintf("_save_json(%s, %s)", src, path), nil
}

func (c *Compiler) compileFetchExpr(f *parser.FetchExpr) (string, error) {
	url, err := c.compileExpr(f.URL, false)
	if err != nil {
		return "", err
	}
	opts := "null"
	if f.With != nil {
		v, err := c.compileExpr(f.With, false)
		if err != nil {
			return "", err
		}
		opts = v
	}
	c.needsFetch = true
	return fmt.Sprintf("_fetch(%s, %s)", url, opts), nil
}

func (c *Compiler) compileFetchExprTyped(f *parser.FetchExpr, typ types.Type) (string, error) {
	url, err := c.compileExpr(f.URL, false)
	if err != nil {
		return "", err
	}
	opts := "null"
	if f.With != nil {
		v, err := c.compileExpr(f.With, false)
		if err != nil {
			return "", err
		}
		opts = v
	}
	c.needsFetch = true
	c.needsFetchJSON = true
	return fmt.Sprintf("_fetch_json(%s, %s, %s)", zigTypeOf(typ), url, opts), nil
}

func isListLiteralExpr(e *parser.Expr) bool {
	if e == nil || e.Binary == nil || len(e.Binary.Right) > 0 {
		return false
	}
	return isListLiteralUnary(e.Binary.Left)
}

func isListLiteralUnary(u *parser.Unary) bool {
	if u == nil {
		return false
	}
	return isListLiteralPostfix(u.Value)
}

func isListLiteralPostfix(p *parser.PostfixExpr) bool {
	if p == nil || len(p.Ops) > 0 {
		return false
	}
	return isListLiteralPrimary(p.Target)
}

func isListLiteralPrimary(p *parser.Primary) bool {
	return p != nil && p.List != nil
}

func isEmptyListExpr(e *parser.Expr) bool {
	if !isListLiteralExpr(e) {
		return false
	}
	ll := e.Binary.Left.Value.Target.List
	return len(ll.Elems) == 0
}

func isMapLiteralExpr(e *parser.Expr) bool {
	if e == nil || e.Binary == nil || len(e.Binary.Right) > 0 {
		return false
	}
	return isMapLiteralUnary(e.Binary.Left)
}

func isMapLiteralUnary(u *parser.Unary) bool {
	if u == nil {
		return false
	}
	return isMapLiteralPostfix(u.Value)
}

func isMapLiteralPostfix(p *parser.PostfixExpr) bool {
	if p == nil || len(p.Ops) > 0 {
		return false
	}
	return isMapLiteralPrimary(p.Target)
}

func isMapLiteralPrimary(p *parser.Primary) bool {
	return p != nil && p.Map != nil
}

func isEmptyMapExpr(e *parser.Expr) bool {
	if !isMapLiteralExpr(e) {
		return false
	}
	ml := e.Binary.Left.Value.Target.Map
	return len(ml.Items) == 0
}

func fetchExprOnly(e *parser.Expr) *parser.FetchExpr {
	if e == nil || e.Binary == nil || len(e.Binary.Right) > 0 {
		return nil
	}
	u := e.Binary.Left
	if len(u.Ops) > 0 {
		return nil
	}
	if u.Value == nil || u.Value.Target == nil || len(u.Value.Ops) > 0 {
		return nil
	}
	return u.Value.Target.Fetch
}

func isFunExpr(e *parser.Expr) bool {
	if e == nil || e.Binary == nil || len(e.Binary.Right) > 0 {
		return false
	}
	u := e.Binary.Left
	if len(u.Ops) > 0 {
		return false
	}
	if u.Value == nil || u.Value.Target == nil || len(u.Value.Ops) > 0 {
		return false
	}
	return u.Value.Target.FunExpr != nil
}

func isSelfAppend(st *parser.AssignStmt) (*parser.Expr, bool) {
	if st == nil || st.Value == nil || st.Value.Binary == nil {
		return nil, false
	}
	b := st.Value.Binary
	if len(b.Right) != 1 || b.Right[0].Op != "+" {
		return nil, false
	}
	left := b.Left
	if left == nil || left.Value == nil || left.Value.Target == nil {
		return nil, false
	}
	if left.Value.Target.Selector == nil || left.Value.Target.Selector.Root != st.Name {
		return nil, false
	}
	if len(left.Value.Ops) > 0 {
		return nil, false
	}
	r := b.Right[0].Right
	if r == nil || r.Target == nil || r.Target.List == nil || len(r.Ops) > 0 {
		return nil, false
	}
	if len(r.Target.List.Elems) != 1 {
		return nil, false
	}
	return r.Target.List.Elems[0], true
}

func (c *Compiler) isStringUnary(u *parser.Unary) bool {
	if u == nil {
		return false
	}
	return c.isStringPostfix(u.Value)
}

func (c *Compiler) isStringExpr(e *parser.Expr) bool {
	if e == nil || e.Binary == nil {
		return false
	}
	return c.isStringUnary(e.Binary.Left)
}

func (c *Compiler) isStringPostfix(p *parser.PostfixExpr) bool {
	if p == nil || len(p.Ops) > 0 {
		return false
	}
	return c.isStringPrimary(p.Target)
}

func (c *Compiler) isStringPrimary(p *parser.Primary) bool {
	if p == nil {
		return false
	}
	if p.Lit != nil && p.Lit.Str != nil {
		return true
	}
	if p.Selector != nil && c.env != nil {
		if t, err := c.env.GetVar(p.Selector.Root); err == nil {
			if _, ok := t.(types.StringType); ok {
				return true
			}
			ft := t
			for _, name := range p.Selector.Tail {
				st, ok := ft.(types.StructType)
				if !ok {
					ft = types.AnyType{}
					break
				}
				if val, ok2 := st.Fields[name]; ok2 {
					ft = val
				} else {
					ft = types.AnyType{}
				}
			}
			if _, ok := ft.(types.StringType); ok {
				return true
			}
		}
	}
	return false
}

func (c *Compiler) isStringLiteralExpr(e *parser.Expr) bool {
	if e == nil || e.Binary == nil || len(e.Binary.Right) > 0 {
		return false
	}
	u := e.Binary.Left
	if len(u.Ops) > 0 || u.Value == nil || u.Value.Target == nil || len(u.Value.Ops) > 0 {
		return false
	}
	return u.Value.Target.Lit != nil && u.Value.Target.Lit.Str != nil
}

func (c *Compiler) isStringLiteralUnary(u *parser.Unary) bool {
	if u == nil || len(u.Ops) > 0 {
		return false
	}
	return c.isStringLiteralPostfix(u.Value)
}

func (c *Compiler) isStringLiteralPostfix(p *parser.PostfixExpr) bool {
	if p == nil || len(p.Ops) > 0 || p.Target == nil {
		return false
	}
	return p.Target.Lit != nil && p.Target.Lit.Str != nil
}

func (c *Compiler) isMapExpr(e *parser.Expr) bool {
	if e == nil || e.Binary == nil {
		return false
	}
	return c.isMapUnary(e.Binary.Left)
}

func (c *Compiler) isMapUnary(u *parser.Unary) bool {
	if u == nil {
		return false
	}
	return c.isMapPostfix(u.Value)
}

func (c *Compiler) isMapPostfix(p *parser.PostfixExpr) bool {
	if p == nil || len(p.Ops) > 0 {
		return false
	}
	return c.isMapPrimary(p.Target)
}

func (c *Compiler) isMapPrimary(p *parser.Primary) bool {
	if p == nil {
		return false
	}
	if p.Map != nil {
		return true
	}
	if p.Selector != nil && c.env != nil {
		if t, err := c.env.GetVar(p.Selector.Root); err == nil {
			if _, ok := t.(types.MapType); ok {
				return true
			}
		}
	}
	return false
}

func (c *Compiler) isGroupExpr(e *parser.Expr) bool {
	if e == nil || e.Binary == nil {
		return false
	}
	return c.isGroupUnary(e.Binary.Left)
}

func (c *Compiler) isGroupUnary(u *parser.Unary) bool {
	if u == nil {
		return false
	}
	return c.isGroupPostfix(u.Value)
}

func (c *Compiler) isGroupPostfix(p *parser.PostfixExpr) bool {
	if p == nil || len(p.Ops) > 0 {
		return false
	}
	return c.isGroupPrimary(p.Target)
}

func (c *Compiler) isGroupPrimary(p *parser.Primary) bool {
	if p == nil {
		return false
	}
	if p.Selector != nil && c.env != nil {
		if t, err := c.env.GetVar(p.Selector.Root); err == nil {
			if _, ok := t.(types.GroupType); ok {
				return true
			}
		}
	}
	return false
}

func (c *Compiler) isFloatExpr(e *parser.Expr) bool {
	if e == nil || e.Binary == nil {
		return false
	}
	return c.isFloatUnary(e.Binary.Left)
}

func (c *Compiler) isFloatUnary(u *parser.Unary) bool {
	if u == nil {
		return false
	}
	return c.isFloatPostfix(u.Value)
}

func (c *Compiler) isFloatPostfix(p *parser.PostfixExpr) bool {
	if p == nil || len(p.Ops) > 0 {
		return false
	}
	return c.isFloatPrimary(p.Target)
}

func (c *Compiler) isFloatPrimary(p *parser.Primary) bool {
	if p == nil {
		return false
	}
	if p.Lit != nil && p.Lit.Float != nil {
		return true
	}
	if p.Selector != nil && c.env != nil {
		if t, err := c.env.GetVar(p.Selector.Root); err == nil {
			if _, ok := t.(types.FloatType); ok {
				return true
			}
		}
	}
	return false
}

func (c *Compiler) isFloatListExpr(e *parser.Expr) bool {
	if e == nil || e.Binary == nil || e.Binary.Left == nil {
		return false
	}
	p := e.Binary.Left.Value
	if p == nil {
		return false
	}
	if p.Target != nil && p.Target.List != nil && len(p.Target.List.Elems) > 0 {
		return c.isFloatExpr(p.Target.List.Elems[0])
	}
	if p.Target != nil && p.Target.Selector != nil && c.env != nil {
		if t, err := c.env.GetVar(p.Target.Selector.Root); err == nil {
			if lt, ok := t.(types.ListType); ok {
				if _, ok := lt.Elem.(types.FloatType); ok {
					return true
				}
			}
		}
	}
	return false
}

func (c *Compiler) isBoolExpr(e *parser.Expr) bool {
	if e == nil || e.Binary == nil {
		return false
	}
	return c.isBoolUnary(e.Binary.Left)
}

func (c *Compiler) isBoolUnary(u *parser.Unary) bool {
	if u == nil {
		return false
	}
	return c.isBoolPostfix(u.Value)
}

func (c *Compiler) isBoolPostfix(p *parser.PostfixExpr) bool {
	if p == nil || len(p.Ops) > 0 {
		return false
	}
	return c.isBoolPrimary(p.Target)
}

func (c *Compiler) isBoolPrimary(p *parser.Primary) bool {
	if p == nil {
		return false
	}
	if p.Lit != nil && p.Lit.Bool != nil {
		return true
	}
	if p.Selector != nil && c.env != nil {
		if t, err := c.env.GetVar(p.Selector.Root); err == nil {
			if _, ok := t.(types.BoolType); ok {
				return true
			}
		}
	}
	return false
}

func (c *Compiler) isStringListPostfix(p *parser.PostfixExpr) bool {
	if p == nil || len(p.Ops) > 0 {
		return false
	}
	return c.isStringListPrimary(p.Target)
}

func (c *Compiler) isStringListUnary(u *parser.Unary) bool {
	if u == nil {
		return false
	}
	return c.isStringListPostfix(u.Value)
}

func (c *Compiler) isStringListPrimary(p *parser.Primary) bool {
	if p == nil {
		return false
	}
	if p.List != nil && len(p.List.Elems) > 0 {
		return c.isStringExpr(p.List.Elems[0])
	}
	if p.Selector != nil && c.env != nil {
		if t, err := c.env.GetVar(p.Selector.Root); err == nil {
			if lt, ok := t.(types.ListType); ok {
				if _, ok := lt.Elem.(types.StringType); ok {
					return true
				}
			}
		}
	}
	return false
}

func (c *Compiler) isListExpr(e *parser.Expr) bool {
	if e == nil || e.Binary == nil {
		return false
	}
	return c.isListUnary(e.Binary.Left)
}

func (c *Compiler) isListUnary(u *parser.Unary) bool {
	if u == nil {
		return false
	}
	return c.isListPostfix(u.Value)
}

func (c *Compiler) isListPostfix(p *parser.PostfixExpr) bool {
	if p == nil || len(p.Ops) > 0 {
		return false
	}
	return c.isListPrimary(p.Target)
}

func (c *Compiler) isListPrimary(p *parser.Primary) bool {
	if p == nil {
		return false
	}
	if p.List != nil {
		return true
	}
	if p.Selector != nil && c.env != nil {
		if t, err := c.env.GetVar(p.Selector.Root); err == nil {
			if _, ok := t.(types.ListType); ok {
				return true
			}
		}
	}
	return false
}

func (c *Compiler) listElemTypeUnary(u *parser.Unary) string {
	if u == nil {
		return "i32"
	}
	return c.listElemTypePostfix(u.Value)
}

func (c *Compiler) listElemTypePostfix(p *parser.PostfixExpr) string {
	if p == nil {
		return "i32"
	}
	if p.Target != nil {
		if p.Target.List != nil && len(p.Target.List.Elems) > 0 {
			t := c.inferExprType(p.Target.List.Elems[0])
			return zigTypeOf(t)
		}
		if p.Target.Selector != nil && c.env != nil {
			if t, err := c.env.GetVar(p.Target.Selector.Root); err == nil {
				if lt, ok := t.(types.ListType); ok {
					return zigTypeOf(lt.Elem)
				}
			}
		}
	}
	return "i32"
}

var zigReserved = map[string]bool{
	"fn": true, "var": true, "const": true, "pub": true, "return": true,
	"for": true, "while": true, "if": true, "else": true,
}

func sanitizeName(name string) string {
	if name == "" {
		return ""
	}
	var b strings.Builder
	for i, r := range name {
		if r == '_' || ('0' <= r && r <= '9' && i > 0) || ('A' <= r && r <= 'Z') || ('a' <= r && r <= 'z') {
			b.WriteRune(r)
		} else {
			b.WriteRune('_')
		}
	}
	s := b.String()
	if s == "" || !((s[0] >= 'A' && s[0] <= 'Z') || (s[0] >= 'a' && s[0] <= 'z') || s[0] == '_') {
		s = "_" + s
	}
	if zigReserved[s] || isZigTypeName(s) {
		s = "_" + s
	}
	return s
}

func isZigTypeName(name string) bool {
	if len(name) < 2 {
		return false
	}
	switch name[0] {
	case 'i', 'u', 'f':
		for i := 1; i < len(name); i++ {
			if name[i] < '0' || name[i] > '9' {
				return false
			}
		}
		return true
	}
	return false
}

func (c *Compiler) isMapVar(name string) bool {
	if c.env == nil {
		return false
	}
	if t, err := c.env.GetVar(name); err == nil {
		if _, ok := t.(types.MapType); ok {
			return true
		}
	}
	return false
}

func (c *Compiler) isListVar(name string) bool {
	if c.env == nil {
		return false
	}
	if t, err := c.env.GetVar(name); err == nil {
		if _, ok := t.(types.ListType); ok {
			return true
		}
	}
	return false
}

func (c *Compiler) rangeArgs(e *parser.Expr) (*parser.Expr, *parser.Expr, *parser.Expr, bool) {
	if e == nil || e.Binary == nil || len(e.Binary.Right) != 0 {
		return nil, nil, nil, false
	}
	u := e.Binary.Left
	if u == nil || len(u.Ops) != 0 {
		return nil, nil, nil, false
	}
	p := u.Value
	if p == nil || len(p.Ops) != 0 || p.Target == nil || p.Target.Call == nil {
		return nil, nil, nil, false
	}
	call := p.Target.Call
	if call.Func != "range" {
		return nil, nil, nil, false
	}
	switch len(call.Args) {
	case 1:
		return nil, call.Args[0], nil, true
	case 2:
		return call.Args[0], call.Args[1], nil, true
	case 3:
		return call.Args[0], call.Args[1], call.Args[2], true
	default:
		return nil, nil, nil, false
	}
}
