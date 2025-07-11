//go:build slow

package gocode

import (
	"bytes"
	"fmt"
	"path/filepath"
	"sort"
	"strconv"
	"strings"

	"mochi/interpreter"
	"mochi/parser"
	"mochi/types"
)

// Compiler translates a Mochi AST into Go source code.
type Compiler struct {
	buf          bytes.Buffer
	indent       int
	imports      map[string]bool
	env          *types.Env
	helpers      map[string]bool
	structs      map[string]bool
	handlerCount int
	handlerDones []string
	streams      []string
	usesHandlers bool
	memo         map[string]*parser.Literal

	pyModules     map[string]string
	goModules     map[string]string
	tsModules     map[string]string
	pyAuto        map[string]bool
	goAuto        map[string]bool
	tsAuto        map[string]bool
	externObjects map[string]bool

	tempVarCount int

	returnType types.Type

	varUsage map[any]bool

	receiver string

	anonStructCount int
}

// New creates a new Go compiler instance.
func New(env *types.Env) *Compiler {
	return &Compiler{
		imports:      make(map[string]bool),
		env:          env,
		helpers:      make(map[string]bool),
		structs:      make(map[string]bool),
		handlerDones: []string{},
		memo:         map[string]*parser.Literal{},

		pyModules:       map[string]string{},
		goModules:       map[string]string{},
		tsModules:       map[string]string{},
		pyAuto:          map[string]bool{},
		goAuto:          map[string]bool{},
		tsAuto:          map[string]bool{},
		externObjects:   map[string]bool{},
		tempVarCount:    0,
		anonStructCount: 0,
	}
}

// Compile returns Go source code implementing prog.
func (c *Compiler) Compile(prog *parser.Program) ([]byte, error) {
	// Compile the program body first so we know all helper imports.
	var body bytes.Buffer
	oldBuf := c.buf
	c.buf = body
	c.indent = 0

	for _, stmt := range prog.Statements {
		c.scanImports(stmt)
	}

	// Generate program body.
	c.writeExpectFunc(prog)
	c.writeTestHelpers(prog)

	if err := c.compileTypeDecls(prog); err != nil {
		return nil, err
	}
	if err := c.compileFunDecls(prog); err != nil {
		return nil, err
	}
	if err := c.compileTestBlocks(prog); err != nil {
		return nil, err
	}
	if err := c.compileMainFunc(prog); err != nil {
		return nil, err
	}

	c.writeln("")
	c.emitRuntime()

	bodyBytes := c.buf.Bytes()

	// Restore buffer and write final output with updated imports.
	c.buf = oldBuf
	c.indent = 0
	c.writeln("//go:build ignore")
	c.writeln("")
	c.writeln("package main")
	c.writeln("")
	c.writeImports()
	c.buf.Write(bodyBytes)

	src := c.buf.Bytes()
	formatted := FormatGo(src)
	return formatted, nil
}

func (c *Compiler) writeImports() {
	if len(c.imports) == 0 {
		return
	}
	c.writeln("import (")
	c.indent++
	keys := make([]string, 0, len(c.imports))
	for k := range c.imports {
		keys = append(keys, k)
	}
	sort.Strings(keys)
	for _, imp := range keys {
		if strings.Contains(imp, " ") {
			c.writeln(imp)
		} else {
			c.writeln(fmt.Sprintf("\"%s\"", imp))
		}
	}
	c.indent--
	c.writeln(")")
	c.writeln("")
}

func (c *Compiler) writeExpectFunc(prog *parser.Program) {
	if !hasExpect(prog) {
		return
	}
	c.writeln("func expect(cond bool) {")
	c.indent++
	c.writeln("if !cond { panic(\"expect failed\") }")
	c.indent--
	c.writeln("}")
	c.writeln("")
}

func (c *Compiler) writeTestHelpers(prog *parser.Program) {
	if !hasTest(prog) {
		return
	}
	c.imports["fmt"] = true
	c.imports["time"] = true
	c.writeln("func formatDuration(d time.Duration) string {")
	c.indent++
	c.writeln("switch {")
	c.indent++
	c.writeln("case d < time.Microsecond:")
	c.indent++
	c.writeln("return fmt.Sprintf(\"%dns\", d.Nanoseconds())")
	c.indent--
	c.writeln("case d < time.Millisecond:")
	c.indent++
	c.writeln("return fmt.Sprintf(\"%.1fµs\", float64(d.Microseconds()))")
	c.indent--
	c.writeln("case d < time.Second:")
	c.indent++
	c.writeln("return fmt.Sprintf(\"%.1fms\", float64(d.Milliseconds()))")
	c.indent--
	c.writeln("default:")
	c.indent++
	c.writeln("return fmt.Sprintf(\"%.2fs\", d.Seconds())")
	c.indent--
	c.writeln("}")
	c.indent--
	c.writeln("}")
	c.writeln("")

	c.writeln("func printTestStart(name string) {")
	c.indent++
	c.writeln("fmt.Printf(\"   test %-30s ...\", name)")
	c.indent--
	c.writeln("}")
	c.writeln("")

	c.writeln("func printTestPass(d time.Duration) {")
	c.indent++
	c.writeln("fmt.Printf(\" ok (%s)\\n\", formatDuration(d))")
	c.indent--
	c.writeln("}")
	c.writeln("")

	c.writeln("func printTestFail(err error, d time.Duration) {")
	c.indent++
	c.writeln("fmt.Printf(\" fail %v (%s)\\n\", err, formatDuration(d))")
	c.indent--
	c.writeln("}")
	c.writeln("")
}

// --- Statement Compilation ---

func (c *Compiler) compileStmt(s *parser.Statement) error {
	switch {
	case s.Let != nil:
		return c.compileLet(s.Let)
	case s.Var != nil:
		return c.compileVar(s.Var)
	case s.Assign != nil:
		return c.compileAssign(s.Assign)
	case s.Type != nil:
		return c.compileTypeDecl(s.Type)
	case s.Expr != nil:
		expr, err := c.compileExpr(s.Expr.Expr)
		if err != nil {
			return err
		}
		c.writeln(expr)
		return nil
	case s.Return != nil:
		retT := c.returnType
		var (
			expr string
			err  error
		)
		if retT != nil && (isEmptyListLiteral(s.Return.Value) || isEmptyMapLiteral(s.Return.Value)) {
			expr, err = c.compileExprHint(s.Return.Value, retT)
		} else {
			expr, err = c.compileExpr(s.Return.Value)
		}
		if err != nil {
			return err
		}
		exprT := c.inferExprTypeHint(s.Return.Value, retT)
		if retT != nil {
			retGo := goType(retT)
			exprGo := goType(exprT)
			if rl, ok := retT.(types.ListType); ok {
				if el, ok := exprT.(types.ListType); ok && equalTypes(rl.Elem, el.Elem) {
					if retGo != exprGo {
						c.use("_convSlice")
						expr = fmt.Sprintf("_convSlice[%s,%s](%s)", goType(el.Elem), goType(rl.Elem), expr)
					}
				} else if retGo != exprGo || !equalTypes(retT, exprT) || isAny(exprT) {
					expr = c.castExpr(expr, exprT, retT)
				}
			} else if retGo != exprGo || !equalTypes(retT, exprT) || isAny(exprT) {
				if ut, ok := retT.(types.UnionType); ok {
					if st, ok := exprT.(types.StructType); ok {
						if _, ok := ut.Variants[st.Name]; ok {
							// struct variant implements union interface
							// no cast needed
						} else {
							expr = c.castExpr(expr, exprT, retT)
						}
					} else {
						expr = c.castExpr(expr, exprT, retT)
					}
				} else {
					expr = c.castExpr(expr, exprT, retT)
				}
			}
		}
		c.writeln("return " + expr)
		return nil
	case s.If != nil:
		return c.compileIf(s.If)
	case s.While != nil:
		return c.compileWhile(s.While)
	case s.For != nil:
		return c.compileFor(s.For)
	case s.Update != nil:
		return c.compileUpdate(s.Update)
	case s.Break != nil:
		c.writeln("break")
		return nil
	case s.Continue != nil:
		c.writeln("continue")
		return nil
	case s.Stream != nil:
		return c.compileStreamDecl(s.Stream)
	case s.On != nil:
		return c.compileOnHandler(s.On)
	case s.Emit != nil:
		return c.compileEmit(s.Emit)
	case s.Agent != nil:
		return c.compileAgentDecl(s.Agent)
	case s.Import != nil:
		return c.addImport(s.Import)
	case s.ExternObject != nil:
		c.externObjects[s.ExternObject.Name] = true
		return nil
	case s.Fun != nil:
		return c.compileFunStmt(s.Fun)
	case s.ExternVar != nil, s.ExternFun != nil, s.ExternType != nil:
		return nil
	case s.Expect != nil:
		expr, err := c.compileExpr(s.Expect.Value)
		if err != nil {
			return err
		}
		c.writeln("expect(" + expr + ")")
		return nil
	default:
		return nil
	}
}

func (c *Compiler) compileLet(s *parser.LetStmt) error {
	if s.Doc != "" {
		for _, ln := range strings.Split(s.Doc, "\n") {
			c.writeln("// " + ln)
		}
	}
	name := sanitizeName(s.Name)
	var t types.Type = types.AnyType{}
	if c.env != nil {
		envTyp, _ := c.env.GetVar(s.Name)
		if s.Type != nil {
			t = c.resolveTypeRef(s.Type)
			if s.Value != nil {
				// If the declared type is a map with string keys and any values,
				// but the initializer is a map literal, infer a struct type from
				// the literal to avoid runtime casts.
				if mt, ok := t.(types.MapType); ok && isString(mt.Key) && isAny(mt.Value) {
					if ml := s.Value.Binary.Left.Value.Target.Map; ml != nil {
						if st, ok := c.inferStructFromMap(ml, s.Name); ok {
							t = st
							c.env.SetStruct(st.Name, st)
							c.compileStructType(st)
						}
					}
				} else if lt, ok := t.(types.ListType); ok {
					if mt, ok2 := lt.Elem.(types.MapType); ok2 && isString(mt.Key) && isAny(mt.Value) {
						if ll := s.Value.Binary.Left.Value.Target.List; ll != nil {
							if st, ok := c.inferStructFromList(ll, s.Name); ok {
								t = types.ListType{Elem: st}
								c.env.SetStruct(st.Name, st)
								c.compileStructType(st)
							}
						}
					}
				}
			}
		} else if s.Value != nil {
			t = c.inferExprType(s.Value)
			if ll := s.Value.Binary.Left.Value.Target.List; ll != nil {
				if st, ok := c.inferStructFromList(ll, s.Name); ok {
					t = types.ListType{Elem: st}
					c.env.SetStruct(st.Name, st)
					c.compileStructType(st)
				}
			} else if ml := s.Value.Binary.Left.Value.Target.Map; ml != nil {
				if st, ok := c.inferStructFromMap(ml, s.Name); ok {
					t = st
					c.env.SetStruct(st.Name, st)
					c.compileStructType(st)
				}
			} else if qe := s.Value.Binary.Left.Value.Target.Query; qe != nil {
				if ml := mapLiteral(qe.Select); ml != nil {
					if st, ok := c.inferStructFromMap(ml, s.Name); ok {
						t = types.ListType{Elem: st}
						c.env.SetStruct(st.Name, st)
						c.compileStructType(st)
					}
				}
			}
		} else if envTyp != nil {
			t = envTyp
		}
		c.env.SetVar(s.Name, t, false)
	}
	typStr := goType(t)
	value := "nil"
	var lit *parser.Literal
	if s.Value != nil {
		if ml := s.Value.Binary.Left.Value.Target.Map; ml != nil && len(ml.Items) == 0 {
			if mt, ok := t.(types.MapType); ok {
				value = fmt.Sprintf("map[%s]%s{}", goType(mt.Key), goType(mt.Value))
			}
		} else if ll := s.Value.Binary.Left.Value.Target.List; ll != nil && len(ll.Elems) == 0 {
			if lt, ok := t.(types.ListType); ok {
				value = fmt.Sprintf("[]%s{}", goType(lt.Elem))
			}
		}
		if value == "nil" {
			v, err := c.compileExprHint(s.Value, t)
			if err != nil {
				return err
			}
			value = v
		}
		if l, ok := c.evalConstExpr(s.Value); ok {
			lit = l
		}
	}
	if s.Value == nil {
		c.writeln(fmt.Sprintf("var %s %s", name, typStr))
	} else if fn := pureFunExpr(s.Value); fn != nil && exprUsesVarFun(fn, s.Name) {
		c.writeln(fmt.Sprintf("var %s %s", name, typStr))
		c.writeln(fmt.Sprintf("%s = %s", name, value))
	} else {
		c.writeln(fmt.Sprintf("var %s %s = %s", name, typStr, value))
	}
	if !c.varUsed(s) {
		c.writeln(fmt.Sprintf("_ = %s", name))
	}
	if lit != nil && c.env != nil {
		c.env.SetValue(s.Name, literalValue(lit), false)
	}
	return nil
}

func (c *Compiler) compileVar(s *parser.VarStmt) error {
	if s.Doc != "" {
		for _, ln := range strings.Split(s.Doc, "\n") {
			c.writeln("// " + ln)
		}
	}
	name := sanitizeName(s.Name)

	var typ types.Type = types.AnyType{}
	if c.env != nil {
		envTyp, _ := c.env.GetVar(s.Name)
		if s.Type != nil {
			typ = c.resolveTypeRef(s.Type)
			if s.Value != nil {
				if mt, ok := typ.(types.MapType); ok && isString(mt.Key) && isAny(mt.Value) {
					if ml := s.Value.Binary.Left.Value.Target.Map; ml != nil {
						if st, ok := c.inferStructFromMap(ml, s.Name); ok {
							typ = st
							c.env.SetStruct(st.Name, st)
							c.compileStructType(st)
						}
					}
				} else if lt, ok := typ.(types.ListType); ok {
					if mt, ok2 := lt.Elem.(types.MapType); ok2 && isString(mt.Key) && isAny(mt.Value) {
						if ll := s.Value.Binary.Left.Value.Target.List; ll != nil {
							if st, ok := c.inferStructFromList(ll, s.Name); ok {
								typ = types.ListType{Elem: st}
								c.env.SetStruct(st.Name, st)
								c.compileStructType(st)
							}
						}
					}
				}
			}
		} else if s.Value != nil {
			typ = c.inferExprTypeHint(s.Value, typ)
			if ll := s.Value.Binary.Left.Value.Target.List; ll != nil {
				if st, ok := c.inferStructFromList(ll, s.Name); ok {
					typ = types.ListType{Elem: st}
					c.env.SetStruct(st.Name, st)
					c.compileStructType(st)
				}
			} else if ml := s.Value.Binary.Left.Value.Target.Map; ml != nil {
				if st, ok := c.inferStructFromMap(ml, s.Name); ok {
					typ = st
					c.env.SetStruct(st.Name, st)
					c.compileStructType(st)
				}
			}
		} else if envTyp != nil {
			typ = envTyp
		}
		c.env.SetVar(s.Name, typ, true)
	}
	typStr := goType(typ)

	value := "nil"
	if s.Value != nil {
		if ml := s.Value.Binary.Left.Value.Target.Map; ml != nil && len(ml.Items) == 0 {
			if mt, ok := typ.(types.MapType); ok {
				value = fmt.Sprintf("map[%s]%s{}", goType(mt.Key), goType(mt.Value))
			}
		} else if ll := s.Value.Binary.Left.Value.Target.List; ll != nil && len(ll.Elems) == 0 {
			if lt, ok := typ.(types.ListType); ok {
				value = fmt.Sprintf("[]%s{}", goType(lt.Elem))
			}
		}
		if value == "nil" {
			v, err := c.compileExprHint(s.Value, typ)
			if err != nil {
				return err
			}
			exprT := c.inferExprTypeHint(s.Value, typ)
			if lt, ok := typ.(types.ListType); ok {
				if et, ok := exprT.(types.ListType); ok && !containsAny(et.Elem) && equalTypes(lt.Elem, et.Elem) && goType(lt.Elem) != goType(et.Elem) {
					c.use("_convSlice")
					v = fmt.Sprintf("_convSlice[%s,%s](%s)", goType(et.Elem), goType(lt.Elem), v)
				}
			}
			value = v
		}
	}

	if s.Value == nil {
		c.writeln(fmt.Sprintf("var %s %s", name, typStr))
	} else if fn := pureFunExpr(s.Value); fn != nil && exprUsesVarFun(fn, s.Name) {
		c.writeln(fmt.Sprintf("var %s %s", name, typStr))
		c.writeln(fmt.Sprintf("%s = %s", name, value))
	} else {
		c.writeln(fmt.Sprintf("var %s %s = %s", name, typStr, value))
	}
	if !c.varUsed(s) {
		c.writeln(fmt.Sprintf("_ = %s", name))
	}
	return nil
}

func (c *Compiler) compileAssign(s *parser.AssignStmt) error {
	lhs := sanitizeName(s.Name)
	if c.receiver != "" && len(s.Index) == 0 {
		if st, ok := c.env.GetStruct(c.receiver); ok {
			if _, ok := st.Fields[s.Name]; ok {
				lhs = fmt.Sprintf("s.%s", exportName(lhs))
			}
		}
	}
	var t types.Type
	if c.env != nil {
		if v, err := c.env.GetVar(s.Name); err == nil {
			t = v
		}
	}
	for _, f := range s.Field {
		if st, ok := t.(types.StructType); ok {
			if ft, ok2 := st.Fields[f.Name]; ok2 {
				t = ft
			} else {
				t = types.AnyType{}
			}
		} else {
			t = types.AnyType{}
		}
		lhs += "." + exportName(sanitizeName(f.Name))
	}
	for _, idx := range s.Index {
		iexpr, err := c.compileExpr(idx.Start)
		if err != nil {
			return err
		}
		if mt, ok := t.(types.MapType); ok {
			keyT := c.inferExprType(idx.Start)
			if !equalTypes(keyT, mt.Key) || isAny(keyT) {
				iexpr = fmt.Sprintf("(%s).(%s)", iexpr, goType(mt.Key))
			}
			t = mt.Value
		} else if lt, ok := t.(types.ListType); ok {
			t = lt.Elem
		}
		lhs = fmt.Sprintf("%s[%s]", lhs, iexpr)
	}
	value := ""
	if ml := s.Value.Binary.Left.Value.Target.Map; ml != nil && len(ml.Items) == 0 {
		if mt, ok := t.(types.MapType); ok {
			value = fmt.Sprintf("map[%s]%s{}", goType(mt.Key), goType(mt.Value))
		}
	} else if ll := s.Value.Binary.Left.Value.Target.List; ll != nil && len(ll.Elems) == 0 {
		if lt, ok := t.(types.ListType); ok {
			value = fmt.Sprintf("[]%s{}", goType(lt.Elem))
		}
	}
	targetType := t
	if value == "" {
		var err error
		var typ types.Type
		if c.env != nil {
			if _, err2 := c.env.GetVar(s.Name); err2 == nil {
				typ = targetType
				value, err = c.compileExprHint(s.Value, targetType)
			} else {
				value, err = c.compileExpr(s.Value)
			}
		} else {
			value, err = c.compileExpr(s.Value)
		}
		if err != nil {
			return err
		}
		if typ != nil {
			exprT := c.inferExprTypeHint(s.Value, typ)
			if lt, ok := typ.(types.ListType); ok {
				if et, ok := exprT.(types.ListType); ok && !containsAny(et.Elem) && equalTypes(lt.Elem, et.Elem) && goType(lt.Elem) != goType(et.Elem) {
					c.use("_convSlice")
					value = fmt.Sprintf("_convSlice[%s,%s](%s)", goType(et.Elem), goType(lt.Elem), value)
				}
			}
			exprGo := goType(exprT)
			typGo := goType(typ)
			if typGo != "" && typGo != exprGo && (isAny(exprT) || !equalTypes(typ, exprT)) {
				value = c.castExpr(value, exprT, typ)
			}
		}
		typ = targetType
	}
	finalTyp := targetType
	exprT := c.inferExprTypeHint(s.Value, finalTyp)
	if finalTyp != nil {
		exprGo := goType(exprT)
		typGo := goType(finalTyp)
		if typGo != "" && typGo != exprGo && (isAny(exprT) || !equalTypes(finalTyp, exprT)) {
			value = c.castExpr(value, exprT, finalTyp)
		}
	}
	c.writeln(fmt.Sprintf("%s = %s", lhs, value))
	return nil
}

func (c *Compiler) compileStreamDecl(s *parser.StreamDecl) error {
	st, ok := c.env.GetStream(s.Name)
	if !ok {
		return fmt.Errorf("unknown stream: %s", s.Name)
	}
	c.compileStructType(st)
	varName := unexportName(sanitizeName(s.Name)) + "Stream"
	c.imports["mochi/runtime/stream"] = true
	c.writeln(fmt.Sprintf("var %s = stream.New(%q, 64, nil)", varName, s.Name))
	c.streams = append(c.streams, varName)
	return nil
}

func (c *Compiler) compileOnHandler(h *parser.OnHandler) error {
	st, ok := c.env.GetStream(h.Stream)
	if !ok {
		return fmt.Errorf("unknown stream: %s", h.Stream)
	}
	c.usesHandlers = true
	streamVar := unexportName(sanitizeName(h.Stream)) + "Stream"
	handlerID := c.handlerCount
	c.handlerCount++
	doneVar := fmt.Sprintf("_done%d", handlerID)
	c.writeln(fmt.Sprintf("%s := make(chan struct{})", doneVar))
	c.writeln(fmt.Sprintf("%s.Subscribe(\"handler-%d\", func(ev *stream.Event) error {", streamVar, handlerID))
	c.indent++
	alias := sanitizeName(h.Alias)
	c.writeln(fmt.Sprintf("%s := ev.Data.(%s)", alias, sanitizeName(st.Name)))
	child := types.NewEnv(c.env)
	child.SetVar(h.Alias, st, true)
	orig := c.env
	c.env = child
	if err := c.compileStmtList(h.Body); err != nil {
		c.env = orig
		return err
	}
	c.env = orig
	c.writeln(fmt.Sprintf("close(%s)", doneVar))
	c.writeln("return nil")
	c.indent--
	c.writeln("})")
	c.handlerDones = append(c.handlerDones, doneVar)
	return nil
}

func (c *Compiler) compileEmit(e *parser.EmitStmt) error {
	st, ok := c.env.GetStream(e.Stream)
	if !ok {
		return fmt.Errorf("unknown stream: %s", e.Stream)
	}
	parts := make([]string, len(e.Fields))
	for i, f := range e.Fields {
		v, err := c.compileExpr(f.Value)
		if err != nil {
			return err
		}
		parts[i] = fmt.Sprintf("%s: %s", exportName(sanitizeName(f.Name)), v)
	}
	lit := fmt.Sprintf("%s{%s}", sanitizeName(st.Name), strings.Join(parts, ", "))
	streamVar := unexportName(sanitizeName(e.Stream)) + "Stream"
	c.writeln(fmt.Sprintf("_, _ = %s.Emit(context.Background(), %s)", streamVar, lit))
	return nil
}

func (c *Compiler) compileAgentDecl(a *parser.AgentDecl) error {
	st, ok := c.env.GetStruct(a.Name)
	if !ok {
		return fmt.Errorf("unknown agent: %s", a.Name)
	}

	name := sanitizeName(a.Name)
	if !c.structs[name] {
		c.structs[name] = true
		c.writeln(fmt.Sprintf("type %s struct {", name))
		c.indent++
		c.writeln("*agent.Agent")
		for _, fn := range st.Order {
			ft := st.Fields[fn]
			c.writeln(fmt.Sprintf("%s %s", exportName(sanitizeName(fn)), goType(ft)))
		}
		c.indent--
		c.writeln("}")
		c.writeln("")
	}

	baseEnv := types.NewEnv(c.env)
	for _, fn := range st.Order {
		baseEnv.SetVar(fn, st.Fields[fn], true)
	}

	onHandlers := []struct{ name, stream string }{}
	handlerID := 0

	for _, blk := range a.Body {
		switch {
		case blk.Intent != nil:
			if _, err := c.compileAgentIntent(name, baseEnv, blk.Intent); err != nil {
				return err
			}
		case blk.On != nil:
			hname, err := c.compileAgentOn(name, baseEnv, blk.On, handlerID)
			if err != nil {
				return err
			}
			onHandlers = append(onHandlers, struct{ name, stream string }{hname, blk.On.Stream})
			handlerID++
		case blk.Let != nil:
			baseEnv.SetVar(blk.Let.Name, st.Fields[blk.Let.Name], false)
		case blk.Var != nil:
			baseEnv.SetVar(blk.Var.Name, st.Fields[blk.Var.Name], true)
		}
	}

	// constructor
	c.writeln(fmt.Sprintf("func New%s() *%s {", name, name))
	c.indent++
	c.writeln(fmt.Sprintf("inst := &%s{Agent: agent.New(agent.Config{Name: %q, BufSize: 16, WG: nil})}", name, a.Name))

	origEnv := c.env
	c.env = baseEnv
	for _, blk := range a.Body {
		switch {
		case blk.Let != nil:
			val := "nil"
			if blk.Let.Value != nil {
				v, err := c.compileExpr(blk.Let.Value)
				if err != nil {
					c.env = origEnv
					return err
				}
				val = v
			}
			c.writeln(fmt.Sprintf("inst.%s = %s", exportName(sanitizeName(blk.Let.Name)), val))
		case blk.Var != nil:
			val := "nil"
			if blk.Var.Value != nil {
				v, err := c.compileExpr(blk.Var.Value)
				if err != nil {
					c.env = origEnv
					return err
				}
				val = v
			}
			c.writeln(fmt.Sprintf("inst.%s = %s", exportName(sanitizeName(blk.Var.Name)), val))
		}
	}
	c.env = origEnv

	for _, h := range onHandlers {
		streamVar := unexportName(sanitizeName(h.stream)) + "Stream"
		c.writeln(fmt.Sprintf("inst.Agent.On(%s, inst.%s)", streamVar, h.name))
	}
	for _, blk := range a.Body {
		if blk.Intent != nil {
			c.writeln(fmt.Sprintf("inst.Agent.RegisterIntent(%q, func(ctx context.Context, args ...agent.Value) (agent.Value, error) {", blk.Intent.Name))
			c.indent++
			paramNames := make([]string, len(blk.Intent.Params))
			for i, p := range blk.Intent.Params {
				typ := goType(st.Methods[blk.Intent.Name].Type.Params[i])
				pname := sanitizeName(p.Name)
				c.writeln(fmt.Sprintf("var %s %s", pname, typ))
				c.writeln(fmt.Sprintf("if len(args) > %d { %s = args[%d].(%s) }", i, pname, i, typ))
				paramNames[i] = pname
			}
			call := fmt.Sprintf("inst.%s(%s)", sanitizeName(blk.Intent.Name), strings.Join(paramNames, ", "))
			if rt := goType(st.Methods[blk.Intent.Name].Type.Return); rt != "" {
				c.writeln(fmt.Sprintf("res := %s", call))
				c.writeln("return res, nil")
			} else {
				c.writeln(call)
				c.writeln("return nil, nil")
			}
			c.indent--
			c.writeln("})")
		}
	}

	c.writeln("inst.Agent.Start(context.Background())")
	c.writeln("return inst")
	c.indent--
	c.writeln("}")
	c.writeln("")

	return nil
}

func (c *Compiler) compileAgentIntent(agentName string, env *types.Env, in *parser.IntentDecl) (string, error) {
	st, _ := c.env.GetStruct(agentName)
	ft := st.Methods[in.Name].Type
	name := sanitizeName(in.Name)
	c.writeIndent()
	c.buf.WriteString(fmt.Sprintf("func (a *%s) %s(", agentName, name))
	for i, p := range in.Params {
		if i > 0 {
			c.buf.WriteString(", ")
		}
		paramType := "any"
		if i < len(ft.Params) {
			paramType = goType(ft.Params[i])
		}
		c.buf.WriteString(sanitizeName(p.Name) + " " + paramType)
	}
	retType := goType(ft.Return)
	if retType == "" {
		c.buf.WriteString(") {\n")
	} else {
		c.buf.WriteString(") " + retType + " {\n")
	}
	child := types.NewEnv(env)
	for i, p := range in.Params {
		if i < len(ft.Params) {
			child.SetVar(p.Name, ft.Params[i], true)
		}
	}
	orig := c.env
	c.env = child
	c.indent++
	if err := c.compileStmtList(in.Body); err != nil {
		c.env = orig
		return "", err
	}
	c.indent--
	c.env = orig
	c.writeIndent()
	c.buf.WriteString("}\n\n")
	return name, nil
}

func (c *Compiler) compileAgentOn(agentName string, env *types.Env, h *parser.OnHandler, id int) (string, error) {
	st, ok := c.env.GetStream(h.Stream)
	if !ok {
		return "", fmt.Errorf("unknown stream: %s", h.Stream)
	}
	fname := fmt.Sprintf("_on%d", id)
	c.writeIndent()
	c.buf.WriteString(fmt.Sprintf("func (a *%s) %s(ctx context.Context, ev *stream.Event) {\n", agentName, fname))
	alias := sanitizeName(h.Alias)
	c.indent++
	c.writeln(fmt.Sprintf("%s := ev.Data.(%s)", alias, sanitizeName(st.Name)))
	child := types.NewEnv(env)
	child.SetVar(h.Alias, st, true)
	orig := c.env
	c.env = child
	if err := c.compileStmtList(h.Body); err != nil {
		c.env = orig
		return "", err
	}
	c.env = orig
	c.indent--
	c.writeln("}")
	c.writeln("")
	return fname, nil
}

func (c *Compiler) compileMethod(structName string, fun *parser.FunStmt) error {
	st, ok := c.env.GetStruct(structName)
	if !ok {
		return fmt.Errorf("unknown struct: %s", structName)
	}
	ft := st.Methods[fun.Name].Type
	name := exportName(sanitizeName(fun.Name))
	recv := sanitizeName(structName)
	c.writeIndent()
	c.buf.WriteString(fmt.Sprintf("func (s *%s) %s(", recv, name))
	for i, p := range fun.Params {
		if i > 0 {
			c.buf.WriteString(", ")
		}
		paramType := "any"
		if i < len(ft.Params) {
			paramType = goType(ft.Params[i])
		}
		c.buf.WriteString(sanitizeName(p.Name) + " " + paramType)
	}
	retType := goType(ft.Return)
	if retType == "" {
		c.buf.WriteString(") {\n")
	} else {
		c.buf.WriteString(") " + retType + " {\n")
	}
	child := types.NewEnv(c.env)
	for fname, t := range st.Fields {
		child.SetVar(fname, t, true)
	}
	for i, p := range fun.Params {
		if i < len(ft.Params) {
			child.SetVar(p.Name, ft.Params[i], true)
		}
	}
	orig := c.env
	origRecv := c.receiver
	c.env = child
	c.receiver = structName
	c.indent++
	if err := c.compileStmtList(fun.Body); err != nil {
		c.env = orig
		c.receiver = origRecv
		return err
	}
	c.indent--
	c.env = orig
	c.receiver = origRecv
	c.writeIndent()
	c.buf.WriteString("}\n\n")
	return nil
}

func (c *Compiler) compileStructType(st types.StructType) {
	name := sanitizeName(st.Name)
	if c.structs[name] {
		return
	}
	c.structs[name] = true
	c.writeln(fmt.Sprintf("type %s struct {", name))
	c.indent++
	for _, fn := range st.Order {
		ft := st.Fields[fn]
		c.writeln(fmt.Sprintf("%s %s `json:\"%s\"`", exportName(sanitizeName(fn)), goType(ft), fn))
	}
	c.indent--
	c.writeln("}")
	c.writeln("")
	for _, ft := range st.Fields {
		if sub, ok := ft.(types.StructType); ok {
			c.compileStructType(sub)
		}
	}
}

func (c *Compiler) inferStructFromList(ll *parser.ListLiteral, name string) (types.StructType, bool) {
	if ll == nil || len(ll.Elems) == 0 {
		return types.StructType{}, false
	}
	first := ll.Elems[0]
	if first.Binary == nil || len(first.Binary.Right) != 0 {
		return types.StructType{}, false
	}
	fm := first.Binary.Left.Value.Target.Map
	if fm == nil {
		return types.StructType{}, false
	}
	fields := map[string]types.Type{}
	order := make([]string, len(fm.Items))
	for i, it := range fm.Items {
		key, ok := simpleStringKey(it.Key)
		if !ok {
			return types.StructType{}, false
		}
		order[i] = key
		fields[key] = types.ExprType(it.Value, c.env)
	}
	for _, el := range ll.Elems[1:] {
		if el.Binary == nil || len(el.Binary.Right) != 0 {
			return types.StructType{}, false
		}
		ml := el.Binary.Left.Value.Target.Map
		if ml == nil || len(ml.Items) != len(order) {
			return types.StructType{}, false
		}
		for i, it := range ml.Items {
			key, ok := simpleStringKey(it.Key)
			if !ok || key != order[i] {
				return types.StructType{}, false
			}
			t := types.ExprType(it.Value, c.env)
			if !equalTypes(fields[key], t) {
				return types.StructType{}, false
			}
		}
	}
	stName := exportName(sanitizeName(name)) + "Item"
	idx := 1
	base := stName
	for {
		if _, ok := c.env.GetStruct(stName); ok || c.structs[stName] {
			stName = fmt.Sprintf("%s%d", base, idx)
			idx++
		} else {
			break
		}
	}
	st := types.StructType{Name: stName, Fields: fields, Order: order}
	return st, true
}

func (c *Compiler) inferStructFromMap(ml *parser.MapLiteral, name string) (types.StructType, bool) {
	if ml == nil || len(ml.Items) == 0 {
		return types.StructType{}, false
	}
	fields := map[string]types.Type{}
	order := make([]string, len(ml.Items))
	for i, it := range ml.Items {
		key, ok := simpleStringKey(it.Key)
		if !ok {
			return types.StructType{}, false
		}
		order[i] = key
		fields[key] = types.ExprType(it.Value, c.env)
	}
	stName := exportName(sanitizeName(name))
	if stName == "" {
		stName = "AnonStruct"
	}
	idx := 1
	base := stName
	for {
		if _, ok := c.env.GetStruct(stName); ok || c.structs[stName] {
			stName = fmt.Sprintf("%s%d", base, idx)
			idx++
		} else {
			break
		}
	}
	st := types.StructType{Name: stName, Fields: fields, Order: order}
	return st, true
}

func unexportName(name string) string {
	if name == "" {
		return ""
	}
	runes := []rune(name)
	if runes[0] >= 'A' && runes[0] <= 'Z' {
		runes[0] = runes[0] - 'A' + 'a'
	}
	return string(runes)
}

func (c *Compiler) compileTypeDecl(t *parser.TypeDecl) error {
	if t.Doc != "" {
		for _, ln := range strings.Split(t.Doc, "\n") {
			c.writeln("// " + ln)
		}
	}
	name := sanitizeName(t.Name)
	if c.structs[name] {
		return nil
	}
	c.structs[name] = true
	if len(t.Variants) > 0 {
		iface := fmt.Sprintf("type %s interface { is%s() }", name, name)
		c.writeln(iface)
		for _, v := range t.Variants {
			vname := sanitizeName(v.Name)
			c.writeln(fmt.Sprintf("type %s struct {", vname))
			c.indent++
			for _, f := range v.Fields {
				fieldName := exportName(sanitizeName(f.Name))
				rtype := c.resolveTypeRef(f.Type)
				typ := goType(rtype)
				if f.Doc != "" {
					for _, ln := range strings.Split(f.Doc, "\n") {
						c.writeln("// " + ln)
					}
				}
				c.writeln(fmt.Sprintf("%s %s `json:\"%s\"`", fieldName, typ, f.Name))
				if c.env != nil {
					if st, ok := c.env.GetStruct(v.Name); ok {
						st.Fields[f.Name] = rtype
						c.env.SetStruct(v.Name, st)
					}
				}
			}
			c.indent--
			c.writeln("}")
			c.writeln(fmt.Sprintf("func (%s) is%s() {}", vname, name))
		}
		if c.env != nil {
			if ut, ok := c.env.GetUnion(t.Name); ok {
				for _, v := range t.Variants {
					if st, ok := c.env.GetStruct(v.Name); ok {
						ut.Variants[v.Name] = st
					}
				}
				c.env.SetUnion(t.Name, ut)
			}
		}
		return nil
	}
	c.writeln(fmt.Sprintf("type %s struct {", name))
	c.indent++
	for _, m := range t.Members {
		if m.Field != nil {
			fieldName := exportName(sanitizeName(m.Field.Name))
			typ := goType(c.resolveTypeRef(m.Field.Type))
			if m.Field.Doc != "" {
				for _, ln := range strings.Split(m.Field.Doc, "\n") {
					c.writeln("// " + ln)
				}
			}
			c.writeln(fmt.Sprintf("%s %s `json:\"%s\"`", fieldName, typ, m.Field.Name))
		}
	}
	c.indent--
	c.writeln("}")
	for _, m := range t.Members {
		if m.Method != nil {
			if err := c.compileMethod(t.Name, m.Method); err != nil {
				return err
			}
		}
	}
	return nil
}

func (c *Compiler) compileIf(stmt *parser.IfStmt) error {
	cond, err := c.compileExpr(stmt.Cond)
	if err != nil {
		return err
	}
	ct := c.inferExprType(stmt.Cond)
	if !isBool(ct) {
		cond = c.castExpr(cond, ct, types.BoolType{})
	}
	c.writeIndent()
	c.buf.WriteString("if " + cond + " {")
	c.buf.WriteByte('\n')
	c.indent++
	if err := c.compileStmtList(stmt.Then); err != nil {
		return err
	}
	c.indent--
	c.writeIndent()
	c.buf.WriteString("}")
	if stmt.ElseIf != nil {
		c.buf.WriteString(" else ")
		return c.compileIf(stmt.ElseIf)
	}
	if len(stmt.Else) > 0 {
		c.buf.WriteString(" else {")
		c.buf.WriteByte('\n')
		c.indent++
		if err := c.compileStmtList(stmt.Else); err != nil {
			return err
		}
		c.indent--
		c.writeIndent()
		c.buf.WriteString("}")
	}
	c.buf.WriteByte('\n')
	return nil
}

func (c *Compiler) compileWhile(stmt *parser.WhileStmt) error {
	// Special case: constant true condition compiles to an infinite loop.
	if lit, ok := c.evalConstExpr(stmt.Cond); ok && lit.Bool != nil && bool(*lit.Bool) {
		c.writeIndent()
		c.buf.WriteString("for {\n")
		c.indent++
		if err := c.compileStmtList(stmt.Body); err != nil {
			return err
		}
		c.indent--
		c.writeIndent()
		c.buf.WriteString("}\n")
		return nil
	}

	c.writeIndent()
	c.buf.WriteString("for {\n")
	c.indent++

	// Evaluate the loop condition at the start of each iteration.
	cond, err := c.compileExpr(stmt.Cond)
	if err != nil {
		return err
	}
	ct := c.inferExprType(stmt.Cond)
	if !isBool(ct) {
		cond = c.castExpr(cond, ct, types.BoolType{})
	}
	c.writeIndent()
	c.buf.WriteString("if !(" + cond + ") {\n")
	c.indent++
	c.writeln("break")
	c.indent--
	c.writeIndent()
	c.buf.WriteString("}\n")

	if err := c.compileStmtList(stmt.Body); err != nil {
		return err
	}

	c.indent--
	c.writeIndent()
	c.buf.WriteString("}\n")
	return nil
}

func (c *Compiler) compileFor(stmt *parser.ForStmt) error {
	name := sanitizeName(stmt.Name)
	useVar := name != "_"
	if stmt.RangeEnd != nil {
		start, err := c.compileExpr(stmt.Source)
		if err != nil {
			return err
		}
		end, err := c.compileExpr(stmt.RangeEnd)
		if err != nil {
			return err
		}
		loopVar := name
		if !useVar {
			loopVar = c.newNamedVar("i")
		}
		c.writeIndent()
		c.buf.WriteString(fmt.Sprintf("for %s := %s; %s < %s; %s++ {\n", loopVar, start, loopVar, end, loopVar))
		if useVar && c.env != nil {
			c.env.SetVar(stmt.Name, types.IntType{}, true)
		}
		c.indent++
		for _, s := range stmt.Body {
			if err := c.compileStmt(s); err != nil {
				return err
			}
		}
		c.indent--
		c.writeIndent()
		c.buf.WriteString("}\n")
		return nil
	}

	src, err := c.compileExpr(stmt.Source)
	if err != nil {
		return err
	}
	t := c.inferExprType(stmt.Source)
	preBody := ""
	switch tt := t.(type) {
	case types.ListType:
		c.writeIndent()
		if useVar {
			c.buf.WriteString(fmt.Sprintf("for _, %s := range %s {\n", name, src))
			if c.env != nil {
				c.env.SetVar(stmt.Name, tt.Elem, true)
			}
		} else {
			c.buf.WriteString(fmt.Sprintf("for range %s {\n", src))
		}
	case types.StringType:
		c.writeIndent()
		if useVar {
			c.buf.WriteString(fmt.Sprintf("for _, r := range []rune(%s) {\n", src))
			if c.env != nil {
				c.env.SetVar(stmt.Name, types.StringType{}, true)
			}
			preBody = fmt.Sprintf("%s := string(r)\n", name)
		} else {
			c.buf.WriteString(fmt.Sprintf("for range []rune(%s) {\n", src))
		}
	case types.MapType:
		c.writeIndent()
		if useVar {
			c.buf.WriteString(fmt.Sprintf("for %s := range %s {\n", name, src))
			if c.env != nil {
				c.env.SetVar(stmt.Name, tt.Key, true)
			}
		} else {
			c.buf.WriteString(fmt.Sprintf("for range %s {\n", src))
		}
	case types.AnyType:
		c.writeIndent()
		c.use("_toAnySlice")
		if useVar {
			c.buf.WriteString(fmt.Sprintf("for _, %s := range _toAnySlice(%s) {\n", name, src))
			if c.env != nil {
				c.env.SetVar(stmt.Name, types.AnyType{}, true)
			}
		} else {
			c.buf.WriteString(fmt.Sprintf("for range _toAnySlice(%s) {\n", src))
		}
	default:
		return fmt.Errorf("cannot iterate over type %s", t)
	}
	c.indent++
	if preBody != "" {
		c.writeIndent()
		c.buf.WriteString(preBody)
	}
	if err := c.compileStmtList(stmt.Body); err != nil {
		return err
	}
	c.indent--
	c.writeIndent()
	c.buf.WriteString("}\n")
	return nil
}

func (c *Compiler) compileUpdate(u *parser.UpdateStmt) error {
	listVar := sanitizeName(u.Target)
	idxVar := c.newNamedVar("i")
	itemVar := c.newNamedVar("v")

	var elemType types.Type = types.AnyType{}
	if c.env != nil {
		if t, err := c.env.GetVar(u.Target); err == nil {
			if lt, ok := t.(types.ListType); ok {
				elemType = lt.Elem
			}
		}
	}

	origEnv := c.env
	child := types.NewEnv(c.env)

	c.writeIndent()
	c.buf.WriteString(fmt.Sprintf("for %s, %s := range %s {\n", idxVar, itemVar, listVar))
	c.indent++

	if st, ok := elemType.(types.StructType); ok {
		c.compileStructType(st)
		used := map[string]struct{}{}
		if u.Where != nil {
			collectIdents(u.Where, used)
		}
		for _, it := range u.Set.Items {
			collectIdents(it.Value, used)
		}
		for _, f := range st.Order {
			if _, ok := used[f]; !ok {
				continue
			}
			fieldVar := sanitizeName(f)
			c.writeln(fmt.Sprintf("%s := %s.%s", fieldVar, itemVar, exportName(sanitizeName(f))))
			child.SetVar(f, st.Fields[f], true)
		}
	} else if mt, ok := elemType.(types.MapType); ok && equalTypes(mt.Key, types.StringType{}) {
		used := map[string]struct{}{}
		if u.Where != nil {
			collectIdents(u.Where, used)
		}
		for _, it := range u.Set.Items {
			if key, ok2 := identName(it.Key); ok2 {
				used[key] = struct{}{}
			}
			collectIdents(it.Value, used)
		}
		for f := range used {
			fieldVar := sanitizeName(f)
			c.writeln(fmt.Sprintf("%s := %s[%q]", fieldVar, itemVar, f))
			child.SetVar(f, mt.Value, true)
		}
	}

	c.env = child

	if u.Where != nil {
		cond, err := c.compileExpr(u.Where)
		if err != nil {
			c.env = origEnv
			return err
		}
		c.writeIndent()
		c.buf.WriteString("if !(" + cond + ") {\n")
		c.indent++
		c.writeln("continue")
		c.indent--
		c.writeIndent()
		c.buf.WriteString("}\n")
	}

	for _, it := range u.Set.Items {
		if st, ok := elemType.(types.StructType); ok {
			if key, ok2 := identName(it.Key); ok2 {
				val, err := c.compileExprHint(it.Value, st.Fields[key])
				if err != nil {
					c.env = origEnv
					return err
				}
				dest := fmt.Sprintf("%s.%s", itemVar, exportName(sanitizeName(key)))
				c.writeln(fmt.Sprintf("%s = %s", dest, val))
				continue
			}
		}

		keyExpr, err := c.compileExpr(it.Key)
		if err != nil {
			c.env = origEnv
			return err
		}
		valExpr, err := c.compileExpr(it.Value)
		if err != nil {
			c.env = origEnv
			return err
		}
		c.writeln(fmt.Sprintf("%s[%s] = %s", itemVar, keyExpr, valExpr))
	}

	c.writeln(fmt.Sprintf("%s[%s] = %s", listVar, idxVar, itemVar))

	c.env = origEnv
	c.indent--
	c.writeIndent()
	c.buf.WriteString("}\n")
	return nil
}

func (c *Compiler) compileFunStmt(fun *parser.FunStmt) error {
	if fun.Doc != "" {
		for _, ln := range strings.Split(fun.Doc, "\n") {
			c.writeln("// " + ln)
		}
	}
	c.writeln(fmt.Sprintf("// line %d", fun.Pos.Line))
	name := sanitizeName(fun.Name)
	if c.env != nil {
		c.env.SetFunc(fun.Name, fun)
	}
	var ft types.FuncType
	if c.env != nil {
		if t, err := c.env.GetVar(fun.Name); err == nil {
			if f, ok := t.(types.FuncType); ok {
				ft = f
			}
		}
	}
	// Fallback to declared parameter and return types when type checker
	// hasn't populated the env (e.g. for nested functions).
	if ft.Params == nil {
		ft.Params = make([]types.Type, len(fun.Params))
		for i, p := range fun.Params {
			if p.Type != nil {
				ft.Params[i] = c.resolveTypeRef(p.Type)
			}
		}
	}
	if ft.Return == nil && fun.Return != nil {
		ft.Return = c.resolveTypeRef(fun.Return)
	}
	if ft.Return == nil {
		ft.Return = types.VoidType{}
	}
	if c.env != nil {
		c.env.SetVar(fun.Name, ft, false)
	}

	// Nested functions are compiled as function literals assigned to a variable.
	if c.indent > 0 {
		expr, err := c.compileFunExpr(&parser.FunExpr{Params: fun.Params, Return: fun.Return, BlockBody: fun.Body})
		if err != nil {
			return err
		}
		if exprUsesVarFun(&parser.FunExpr{Params: fun.Params, Return: fun.Return, BlockBody: fun.Body}, fun.Name) {
			c.writeln(fmt.Sprintf("var %s %s", name, goType(ft)))
			c.writeln(fmt.Sprintf("%s = %s", name, expr))
		} else {
			c.writeln("var " + name + " = " + expr)
		}
		return nil
	}

	mut := map[string]bool{}
	for _, p := range fun.Params {
		if paramModified(p.Name, fun.Body) {
			mut[p.Name] = true
		}
	}

	c.writeIndent()
	c.buf.WriteString("func " + name + "(")
	for i, p := range fun.Params {
		if i > 0 {
			c.buf.WriteString(", ")
		}
		paramType := "any"
		if i < len(ft.Params) {
			paramType = goType(ft.Params[i])
		}
		if mut[p.Name] && paramType != "" {
			paramType = "*" + paramType
		}
		c.buf.WriteString(sanitizeName(p.Name) + " " + paramType)
	}
	retType := goType(ft.Return)
	if retType == "" {
		c.buf.WriteString(") {\n")
	} else {
		c.buf.WriteString(") " + retType + " {\n")
	}
	child := types.NewEnv(c.env)
	for i, p := range fun.Params {
		if i < len(ft.Params) {
			child.SetVar(p.Name, ft.Params[i], true)
		}
	}
	originalEnv := c.env
	origRet := c.returnType
	c.returnType = ft.Return
	c.env = child
	c.indent++
	if err := c.compileStmtList(fun.Body); err != nil {
		c.env = originalEnv
		c.returnType = origRet
		return err
	}
	c.indent--
	c.env = originalEnv
	c.returnType = origRet
	c.writeIndent()
	c.buf.WriteString("}\n")
	return nil
}

func (c *Compiler) compileTestBlock(t *parser.TestBlock) error {
	name := "test_" + sanitizeName(t.Name)
	c.writeIndent()
	c.buf.WriteString("func " + name + "() {\n")
	c.indent++
	if err := c.compileStmtList(t.Body); err != nil {
		return err
	}
	c.indent--
	c.writeIndent()
	c.buf.WriteString("}\n")
	return nil
}

// --- Expression Compilation ---

func (c *Compiler) compileExpr(e *parser.Expr) (string, error) {
	return c.compileBinaryExpr(e.Binary)
}

func (c *Compiler) compileBinaryExpr(b *parser.BinaryExpr) (string, error) {
	if b == nil {
		return "", nil
	}

	leftExpr, err := c.compileUnary(b.Left)
	if err != nil {
		return "", err
	}
	leftType := c.inferUnaryType(b.Left)

	operands := []string{leftExpr}
	typesList := []types.Type{leftType}
	ops := []string{}

	for _, part := range b.Right {
		rightType := c.inferPostfixType(part.Right)
		right := ""
		var err error
		if part.Op == "+" && isList(leftType) && len(part.Right.Ops) == 0 && part.Right.Target.List != nil {
			lt := leftType.(types.ListType)
			right, err = c.compilePostfixHint(part.Right, leftType)
			if err != nil {
				return "", err
			}
			rightType = lt
		} else if (part.Op == "==" || part.Op == "!=") && len(part.Right.Ops) == 0 {
			if ll := part.Right.Target.List; ll != nil && len(ll.Elems) == 0 && isList(leftType) {
				lt := leftType.(types.ListType)
				right, err = c.compilePostfixHint(part.Right, leftType)
				if err != nil {
					return "", err
				}
				rightType = lt
			} else if ml := part.Right.Target.Map; ml != nil && len(ml.Items) == 0 && isMap(leftType) {
				mt := leftType.(types.MapType)
				right, err = c.compilePostfixHint(part.Right, leftType)
				if err != nil {
					return "", err
				}
				rightType = mt
			}
		}
		if right == "" {
			right, err = c.compilePostfix(part.Right)
			if err != nil {
				return "", err
			}
		}
		op := part.Op
		if part.Op == "union" && part.All {
			op = "union_all"
		}
		ops = append(ops, op)
		operands = append(operands, right)
		typesList = append(typesList, rightType)
	}

	precLevels := [][]string{
		{"*", "/", "%"},
		{"+", "-"},
		{"<", "<=", ">", ">="},
		{"==", "!=", "in"},
		{"&&"},
		{"||"},
		{"union", "union_all", "except", "intersect"},
	}

	for _, level := range precLevels {
		for i := 0; i < len(ops); {
			if contains(level, ops[i]) {
				expr, typ, err := c.compileBinaryOp(operands[i], typesList[i], ops[i], operands[i+1], typesList[i+1])
				if err != nil {
					return "", err
				}
				operands[i] = expr
				typesList[i] = typ
				operands = append(operands[:i+1], operands[i+2:]...)
				typesList = append(typesList[:i+1], typesList[i+2:]...)
				ops = append(ops[:i], ops[i+1:]...)
			} else {
				i++
			}
		}
	}

	if len(operands) != 1 {
		return "", fmt.Errorf("unexpected state after binary compilation")
	}
	return operands[0], nil
}

func (c *Compiler) compileBinaryOp(left string, leftType types.Type, op string, right string, rightType types.Type) (string, types.Type, error) {
	var expr string
	var next types.Type
	switch op {
	case "+", "-", "*", "/", "%":
		if op != "+" && (isAny(leftType) || isAny(rightType)) {
			left = c.castExpr(left, leftType, types.FloatType{})
			right = c.castExpr(right, rightType, types.FloatType{})
			expr = fmt.Sprintf("(%s %s %s)", left, op, right)
			next = types.FloatType{}
			return expr, next, nil
		}
		switch {
		case (isInt64(leftType) && (isInt64(rightType) || isInt(rightType))) ||
			(isInt64(rightType) && (isInt64(leftType) || isInt(leftType))):
			expr = fmt.Sprintf("(int64(%s) %s int64(%s))", left, op, right)
			next = types.Int64Type{}
		case isInt(leftType) && isInt(rightType):
			if op == "/" {
				expr = fmt.Sprintf("(float64(%s) / float64(%s))", left, right)
				next = types.FloatType{}
			} else {
				expr = fmt.Sprintf("(%s %s %s)", left, op, right)
				next = types.IntType{}
			}
		case isFloat(leftType) && isFloat(rightType):
			expr = fmt.Sprintf("(%s %s %s)", left, op, right)
			next = types.FloatType{}
		case isFloat(leftType) && isInt(rightType):
			right = fmt.Sprintf("float64(%s)", right)
			expr = fmt.Sprintf("(%s %s %s)", left, op, right)
			next = types.FloatType{}
		case isInt(leftType) && isFloat(rightType):
			left = fmt.Sprintf("float64(%s)", left)
			expr = fmt.Sprintf("(%s %s %s)", left, op, right)
			next = types.FloatType{}
		case op == "+" && isAny(leftType) && isList(rightType):
			rt := rightType.(types.ListType)
			left = c.castExpr(left, leftType, rightType)
			expr = fmt.Sprintf("append(append([]%s{}, %s...), %s...)", goType(rt.Elem), left, right)
			next = rightType
		case op == "+" && isList(leftType) && isAny(rightType):
			lt := leftType.(types.ListType)
			right = c.castExpr(right, rightType, leftType)
			expr = fmt.Sprintf("append(append([]%s{}, %s...), %s...)", goType(lt.Elem), left, right)
			next = leftType
		case op == "+" && isAny(leftType) && isString(rightType):
			expr = fmt.Sprintf("fmt.Sprint(%s) + %s", left, right)
			c.imports["fmt"] = true
			next = types.StringType{}
		case op == "+" && isString(leftType) && isAny(rightType):
			expr = fmt.Sprintf("%s + fmt.Sprint(%s)", left, right)
			c.imports["fmt"] = true
			next = types.StringType{}
		case op == "+" && isList(leftType) && isList(rightType):
			lt := leftType.(types.ListType)
			rt := rightType.(types.ListType)
			if _, ok := lt.Elem.(types.AnyType); ok && !isAny(rt.Elem) {
				c.use("_toAnySlice")
				right = fmt.Sprintf("_toAnySlice(%s)", right)
				expr = fmt.Sprintf("append(append([]%s{}, %s...), %s...)", goType(lt.Elem), left, right)
				next = leftType
			} else if !isAny(lt.Elem) && isAny(rt.Elem) {
				if strings.HasPrefix(right, "[]any{") && strings.HasSuffix(right, "}") {
					right = "[]" + goType(lt.Elem) + right[len("[]any"):]
					expr = fmt.Sprintf("append(append([]%s{}, %s...), %s...)", goType(lt.Elem), left, right)
					next = leftType
				} else {
					right = c.castExpr(right, rightType, leftType)
					expr = fmt.Sprintf("append(append([]%s{}, %s...), %s...)", goType(lt.Elem), left, right)
					next = leftType
				}
			} else if equalTypes(lt.Elem, rt.Elem) {
				if goType(lt.Elem) != goType(rt.Elem) {
					c.use("_convSlice")
					right = fmt.Sprintf("_convSlice[%s,%s](%s)", goType(rt.Elem), goType(lt.Elem), right)
				}
				expr = fmt.Sprintf("append(append([]%s{}, %s...), %s...)", goType(lt.Elem), left, right)
				next = leftType
			} else {
				c.use("_toAnySlice")
				if !isAny(lt.Elem) {
					left = fmt.Sprintf("_toAnySlice(%s)", left)
				}
				if !isAny(rt.Elem) {
					right = fmt.Sprintf("_toAnySlice(%s)", right)
				}
				expr = fmt.Sprintf("append(append([]any{}, %s...), %s...)", left, right)
				next = types.ListType{Elem: types.AnyType{}}
			}
		case op == "+" && isString(leftType) && isString(rightType):
			expr = fmt.Sprintf("%s + %s", left, right)
			next = types.StringType{}
		case op == "+" && isInt(leftType) && isAny(rightType):
			left = fmt.Sprintf("float64(%s)", left)
			right = c.castExpr(right, rightType, types.FloatType{})
			expr = fmt.Sprintf("(%s + %s)", left, right)
			next = types.FloatType{}
		case op == "+" && isAny(leftType) && isInt(rightType):
			left = c.castExpr(left, leftType, types.FloatType{})
			right = fmt.Sprintf("float64(%s)", right)
			expr = fmt.Sprintf("(%s + %s)", left, right)
			next = types.FloatType{}
		case op == "+" && isFloat(leftType) && isAny(rightType):
			right = c.castExpr(right, rightType, types.FloatType{})
			expr = fmt.Sprintf("(%s + %s)", left, right)
			next = types.FloatType{}
		case op == "+" && isAny(leftType) && isFloat(rightType):
			left = c.castExpr(left, leftType, types.FloatType{})
			expr = fmt.Sprintf("(%s + %s)", left, right)
			next = types.FloatType{}
		case op == "+" && isAny(leftType) && isAny(rightType):
			left = c.castExpr(left, leftType, types.FloatType{})
			right = c.castExpr(right, rightType, types.FloatType{})
			expr = fmt.Sprintf("(%s + %s)", left, right)
			next = types.FloatType{}
		default:
			return "", types.AnyType{}, fmt.Errorf("operator %q cannot be used on types %s and %s", op, leftType, rightType)
		}
	case "==", "!=", "<", "<=", ">", ">=":
		if op == "==" || op == "!=" {
			if isAny(leftType) || isAny(rightType) || isList(leftType) || isList(rightType) || isMap(leftType) || isMap(rightType) || isStruct(leftType) || isStruct(rightType) {
				c.use("_equal")
				c.imports["reflect"] = true
				if op == "==" {
					expr = fmt.Sprintf("_equal(%s, %s)", left, right)
				} else {
					expr = fmt.Sprintf("!_equal(%s, %s)", left, right)
				}
			} else {
				expr = fmt.Sprintf("(%s %s %s)", left, op, right)
			}
		} else {
			if isAny(leftType) || isAny(rightType) {
				switch {
				case isAny(leftType) && isAny(rightType):
					left = c.castExpr(left, leftType, types.FloatType{})
					right = c.castExpr(right, rightType, types.FloatType{})
				case isAny(leftType) && isString(rightType):
					left = c.castExpr(left, leftType, types.StringType{})
				case isString(leftType) && isAny(rightType):
					right = c.castExpr(right, rightType, types.StringType{})
				case isAny(leftType) && isNumeric(rightType):
					left = c.castExpr(left, leftType, rightType)
				case isNumeric(leftType) && isAny(rightType):
					right = c.castExpr(right, rightType, leftType)
				default:
					return "", types.AnyType{}, fmt.Errorf("incompatible types in comparison: %s and %s", leftType, rightType)
				}
			} else if isFloat(leftType) && (isInt(rightType) || isInt64(rightType)) {
				right = fmt.Sprintf("float64(%s)", right)
			} else if isFloat(rightType) && (isInt(leftType) || isInt64(leftType)) {
				left = fmt.Sprintf("float64(%s)", left)
			}
			if isList(leftType) || isList(rightType) || isMap(leftType) || isMap(rightType) || isStruct(leftType) || isStruct(rightType) {
				return "", types.AnyType{}, fmt.Errorf("operator %q cannot be used on types %s and %s", op, leftType, rightType)
			}
			expr = fmt.Sprintf("(%s %s %s)", left, op, right)
		}
		next = types.BoolType{}
	case "&&", "||":
		if !(isBool(leftType) && isBool(rightType)) {
			return "", types.AnyType{}, fmt.Errorf("operator %q cannot be used on types %s and %s", op, leftType, rightType)
		}
		expr = fmt.Sprintf("(%s %s %s)", left, op, right)
		next = types.BoolType{}
	case "in":
		switch rightType.(type) {
		case types.MapType:
			mt := rightType.(types.MapType)
			keyTemp := c.newNamedVar("key")
			mapTemp := c.newNamedVar("m")
			c.writeln(fmt.Sprintf("%s := %s", keyTemp, left))
			c.writeln(fmt.Sprintf("%s := %s", mapTemp, right))
			keyExpr := keyTemp
			if !equalTypes(leftType, mt.Key) || isAny(leftType) {
				keyExpr = fmt.Sprintf("(%s).(%s)", keyTemp, goType(mt.Key))
			}
			okVar := c.newNamedVar("ok")
			c.writeln(fmt.Sprintf("_, %s := %s[%s]", okVar, mapTemp, keyExpr))
			expr = okVar
			next = types.BoolType{}
		case types.ListType:
			lt := rightType.(types.ListType)
			elemGo := goType(lt.Elem)
			itemExpr := left
			if !equalTypes(leftType, lt.Elem) || isAny(leftType) {
				itemExpr = fmt.Sprintf("(%s).(%s)", left, elemGo)
			}
			if !isAny(lt.Elem) && isComparableSimple(lt.Elem) {
				c.imports["slices"] = true
				expr = fmt.Sprintf("slices.Contains(%s, %s)", right, itemExpr)
			} else {
				c.use("_contains")
				expr = fmt.Sprintf("_contains(%s, %s)", right, itemExpr)
			}
			next = types.BoolType{}
		case types.StringType:
			c.imports["strings"] = true
			expr = fmt.Sprintf("strings.Contains(%s, %s)", right, left)
			next = types.BoolType{}
		default:
			return "", types.AnyType{}, fmt.Errorf("operator %q cannot be used on types %s and %s", op, leftType, rightType)
		}
	case "union_all":
		if !isList(leftType) || !isList(rightType) {
			return "", types.AnyType{}, fmt.Errorf("operator %q cannot be used on types %s and %s", op, leftType, rightType)
		}
		return c.compileBinaryOp(left, leftType, "+", right, rightType)
	case "union", "except", "intersect":
		if !isList(leftType) || !isList(rightType) {
			return "", types.AnyType{}, fmt.Errorf("operator %q cannot be used on types %s and %s", op, leftType, rightType)
		}
		lt := leftType.(types.ListType)
		rt := rightType.(types.ListType)
		elemGo := "any"
		if equalTypes(lt.Elem, rt.Elem) {
			elemGo = goType(lt.Elem)
			if goType(lt.Elem) != goType(rt.Elem) {
				c.use("_convSlice")
				right = fmt.Sprintf("_convSlice[%s,%s](%s)", goType(rt.Elem), goType(lt.Elem), right)
			}
		} else {
			c.use("_toAnySlice")
			if !isAny(lt.Elem) {
				left = fmt.Sprintf("_toAnySlice(%s)", left)
			}
			if !isAny(rt.Elem) {
				right = fmt.Sprintf("_toAnySlice(%s)", right)
			}
			lt = types.ListType{Elem: types.AnyType{}}
			elemGo = "any"
			leftType = lt
			rightType = lt
		}
		switch op {
		case "union":
			c.use("_union")
			c.use("_equal")
			expr = fmt.Sprintf("_union[%s](%s, %s)", elemGo, left, right)
		case "except":
			c.use("_except")
			c.use("_equal")
			expr = fmt.Sprintf("_except[%s](%s, %s)", elemGo, left, right)
		case "intersect":
			c.use("_intersect")
			c.use("_equal")
			expr = fmt.Sprintf("_intersect[%s](%s, %s)", elemGo, left, right)
		}
		next = lt
		return expr, next, nil
	default:
		return "", types.AnyType{}, fmt.Errorf("unsupported operator: %s", op)
	}
	return expr, next, nil
}

func (c *Compiler) compileUnary(u *parser.Unary) (string, error) {
	val, err := c.compilePostfix(u.Value)
	if err != nil {
		return "", err
	}
	t := c.inferUnaryType(u)
	for i := len(u.Ops) - 1; i >= 0; i-- {
		op := u.Ops[i]
		switch op {
		case "-":
			if isAny(t) {
				val = c.castExpr(val, t, types.FloatType{})
			}
			val = fmt.Sprintf("-%s", val)
		case "!":
			val = fmt.Sprintf("!%s", val)
		}
	}
	return val, nil
}

func (c *Compiler) compilePostfix(p *parser.PostfixExpr) (string, error) {
	// Special handling for FFI selectors
	if sel := p.Target.Selector; sel != nil {
		if mod, ok := c.pyModules[sel.Root]; ok {
			attr := strings.Join(sel.Tail, ".")
			if len(p.Ops) > 0 && p.Ops[0].Call != nil {
				args := make([]string, len(p.Ops[0].Call.Args))
				for i, a := range p.Ops[0].Call.Args {
					v, err := c.compileExpr(a)
					if err != nil {
						return "", err
					}
					args[i] = v
				}
				c.imports["mochi/runtime/ffi/python"] = true
				var rtype types.Type = types.AnyType{}
				if t, err := c.env.GetVar(sel.Root + "." + attr); err == nil {
					if ft, ok := t.(types.FuncType); ok {
						rtype = ft.Return
					}
				}
				val := fmt.Sprintf("func() %s { v, _ := python.Attr(%q, %q, %s); return v.(%s) }()", goType(rtype), mod, attr, strings.Join(args, ", "), goType(rtype))
				if goType(rtype) == "any" {
					val = fmt.Sprintf("func() any { v, _ := python.Attr(%q, %q, %s); return v }()", mod, attr, strings.Join(args, ", "))
				}
				return val, nil
			}
			c.imports["mochi/runtime/ffi/python"] = true
			var rtype types.Type = types.AnyType{}
			if t, err := c.env.GetVar(sel.Root + "." + attr); err == nil {
				rtype = t
			}
			if goType(rtype) == "any" {
				val := fmt.Sprintf("func() any { v, _ := python.Attr(%q, %q); return v }()", mod, attr)
				return val, nil
			}
			val := fmt.Sprintf("func() %s { v, _ := python.Attr(%q, %q); return v.(%s) }()", goType(rtype), mod, attr, goType(rtype))
			return val, nil
		}
		if mod, ok := c.goModules[sel.Root]; ok {
			auto := c.goAuto[sel.Root]
			attr := strings.Join(sel.Tail, ".")
			name := mod
			if attr != "" {
				name += "." + attr
			}
			c.imports["mochi/runtime/ffi/go"] = true
			if len(p.Ops) > 0 && p.Ops[0].Call != nil {
				args := make([]string, len(p.Ops[0].Call.Args))
				for i, a := range p.Ops[0].Call.Args {
					v, err := c.compileExpr(a)
					if err != nil {
						return "", err
					}
					args[i] = v
				}
				if auto {
					val := fmt.Sprintf("func() any { v, _ := goffi.AttrAuto(%q, %q, %s); return v }()", mod, attr, strings.Join(args, ", "))
					return val, nil
				}
				val := fmt.Sprintf("func() any { v, _ := goffi.Call(%q, %s); return v }()", name, strings.Join(args, ", "))
				return val, nil
			}
			if auto {
				val := fmt.Sprintf("func() any { v, _ := goffi.AttrAuto(%q, %q); return v }()", mod, attr)
				return val, nil
			}
			val := fmt.Sprintf("func() any { v, _ := goffi.Call(%q); return v }()", name)
			return val, nil
		}
		if mod, ok := c.tsModules[sel.Root]; ok {
			attr := strings.Join(sel.Tail, ":")
			if len(p.Ops) > 0 && p.Ops[0].Call != nil {
				args := make([]string, len(p.Ops[0].Call.Args))
				for i, a := range p.Ops[0].Call.Args {
					v, err := c.compileExpr(a)
					if err != nil {
						return "", err
					}
					args[i] = v
				}
				c.imports["mochi/runtime/ffi/deno"] = true
				val := fmt.Sprintf("func() any { v, _ := denoffi.Attr(%q, %q, %s); return v }()", mod, attr, strings.Join(args, ", "))
				return val, nil
			}
			c.imports["mochi/runtime/ffi/deno"] = true
			val := fmt.Sprintf("func() any { v, _ := denoffi.Attr(%q, %q); return v }()", mod, attr)
			return val, nil
		}
	}

	if sel := p.Target.Selector; sel != nil && len(sel.Tail) > 0 && len(p.Ops) == 1 && p.Ops[0].Call != nil && sel.Tail[len(sel.Tail)-1] == "contains" {
		recvSel := &parser.Primary{Selector: &parser.SelectorExpr{Root: sel.Root, Tail: sel.Tail[:len(sel.Tail)-1]}}
		recv, err := c.compilePrimary(recvSel)
		if err != nil {
			return "", err
		}
		arg, err := c.compileExpr(p.Ops[0].Call.Args[0])
		if err != nil {
			return "", err
		}
		if _, ok := c.inferPrimaryType(recvSel).(types.StringType); ok {
			c.imports["strings"] = true
			return fmt.Sprintf("strings.Contains(%s, %s)", recv, arg), nil
		}
	}

	val, err := c.compilePrimary(p.Target)
	if err != nil {
		return "", err
	}
	typ := c.inferPrimaryType(p.Target)
	for _, op := range p.Ops {
		switch {
		case op.Call != nil:
			args := make([]string, len(op.Call.Args))
			for i, a := range op.Call.Args {
				v, err := c.compileExpr(a)
				if err != nil {
					return "", err
				}
				args[i] = v
			}
			val = fmt.Sprintf("%s(%s)", val, strings.Join(args, ", "))
			if ft, ok := typ.(types.FuncType); ok {
				typ = ft.Return
			} else {
				typ = types.AnyType{}
			}
		case op.Index != nil:
			idx := op.Index
			if idx.Colon == nil {
				key, err := c.compileExpr(idx.Start)
				if err != nil {
					return "", err
				}
				switch tt := typ.(type) {
				case types.ListType:
					val = fmt.Sprintf("%s[%s]", val, key)
					typ = tt.Elem
				case types.MapType:
					keyT := c.inferExprType(idx.Start)
					keyExpr := key
					if !equalTypes(keyT, tt.Key) || isAny(keyT) {
						keyExpr = fmt.Sprintf("(%s).(%s)", key, goType(tt.Key))
					}
					val = fmt.Sprintf("%s[%s]", val, keyExpr)
					typ = tt.Value
				case types.StringType:
					c.use("_indexString")
					val = fmt.Sprintf("_indexString(%s, %s)", val, key)
					typ = types.StringType{}
				default:
					return "", fmt.Errorf("cannot index into type %s", typ)
				}
			} else {
				start := "0"
				if idx.Start != nil {
					s, err := c.compileExpr(idx.Start)
					if err != nil {
						return "", err
					}
					start = s
				}
				end := fmt.Sprintf("len(%s)", val)
				if idx.End != nil {
					e, err := c.compileExpr(idx.End)
					if err != nil {
						return "", err
					}
					end = e
				}
				switch typ.(type) {
				case types.ListType:
					if idx.End == nil {
						end = fmt.Sprintf("len(%s)", val)
					}
					val = fmt.Sprintf("%s[%s:%s]", val, start, end)
				case types.StringType:
					if idx.End == nil {
						end = fmt.Sprintf("len([]rune(%s))", val)
					}
					c.use("_sliceString")
					val = fmt.Sprintf("_sliceString(%s, %s, %s)", val, start, end)
				default:
					if idx.End == nil {
						end = fmt.Sprintf("len(%s)", val)
					}
					val = fmt.Sprintf("%s[%s:%s]", val, start, end)
				}
			}
		case op.Cast != nil:
			t := c.resolveTypeRef(op.Cast.Type)
			val = c.castExpr(val, typ, t)
			typ = t
		}
	}
	return val, nil
}

func (c *Compiler) compilePrimary(p *parser.Primary) (string, error) {
	switch {
	case p.Lit != nil:
		return c.compileLiteral(p.Lit)
	case p.Group != nil:
		expr, err := c.compileExpr(p.Group)
		if err != nil {
			return "", err
		}
		return "(" + expr + ")", nil
	case p.Call != nil:
		return c.compileCallExpr(p.Call)
	case p.FunExpr != nil:
		return c.compileFunExpr(p.FunExpr)
	case p.Selector != nil:
		base := sanitizeName(p.Selector.Root)
		var typ types.Type = types.AnyType{}
		if c.env != nil {
			if t, err := c.env.GetVar(p.Selector.Root); err == nil {
				typ = t
			}
		}
		if len(p.Selector.Tail) == 0 {
			if ut, ok := c.env.FindUnionByVariant(p.Selector.Root); ok {
				st := ut.Variants[p.Selector.Root]
				if len(st.Fields) == 0 {
					return base + "{}", nil
				}
			}
		}
		if c.receiver != "" && len(p.Selector.Tail) == 0 {
			if st, ok := c.env.GetStruct(c.receiver); ok {
				if _, ok := st.Fields[p.Selector.Root]; ok {
					base = fmt.Sprintf("s.%s", exportName(base))
					return base, nil
				}
			}
		}
		if len(p.Selector.Tail) > 0 {
			for i, field := range p.Selector.Tail {
				switch tt := typ.(type) {
				case types.GroupType:
					if field == "key" {
						base = fmt.Sprintf("%s.Key", base)
						typ = types.AnyType{}
					} else if field == "items" {
						base = fmt.Sprintf("%s.Items", base)
						typ = types.ListType{Elem: tt.Elem}
					} else {
						base = fmt.Sprintf("%s[%q]", base, field)
						typ = types.AnyType{}
					}
				case types.MapType:
					base = fmt.Sprintf("%s[%q]", base, field)
					typ = tt.Value
				case types.StructType:
					base = fmt.Sprintf("%s.%s", base, exportName(sanitizeName(field)))
					if ft, ok := tt.Fields[field]; ok {
						typ = ft
					} else {
						typ = types.AnyType{}
					}
				default:
					// treat as dynamic map
					base = fmt.Sprintf("(%s).(map[string]any)[%q]", base, field)
					typ = types.AnyType{}
				}
				if i == len(p.Selector.Tail)-1 {
					return base, nil
				}
			}
		}
		if ut, ok := typ.(types.UnionType); ok && len(p.Selector.Tail) > 0 {
			field := p.Selector.Tail[0]
			var variant string
			var ftyp types.Type
			for name, st := range ut.Variants {
				if ft, ok := st.Fields[field]; ok {
					if variant != "" {
						variant = ""
						break
					}
					variant = name
					ftyp = ft
				}
			}
			if variant != "" {
				base = fmt.Sprintf("%s.(%s).%s", base, sanitizeName(variant), exportName(sanitizeName(field)))
				typ = ftyp
				for _, f := range p.Selector.Tail[1:] {
					if st, ok := typ.(types.StructType); ok {
						if ft, ok := st.Fields[f]; ok {
							typ = ft
						} else {
							typ = types.AnyType{}
						}
					} else {
						typ = types.AnyType{}
					}
					base += "." + exportName(sanitizeName(f))
				}
				return base, nil
			}
		}
		for _, field := range p.Selector.Tail {
			base += "." + exportName(sanitizeName(field))
		}
		return base, nil
	case p.Struct != nil:
		if _, ok := c.env.GetAgent(p.Struct.Name); ok {
			if len(p.Struct.Fields) > 0 {
				return "", fmt.Errorf("agent initialization with fields not supported")
			}
			return fmt.Sprintf("New%s()", sanitizeName(p.Struct.Name)), nil
		}
		parts := make([]string, len(p.Struct.Fields))
		var st types.StructType
		var ok bool
		if c.env != nil {
			if s, found := c.env.GetStruct(p.Struct.Name); found {
				st = s
				ok = true
			}
		}
		for i, f := range p.Struct.Fields {
			var v string
			var err error
			if ok {
				if ft, ok2 := st.Fields[f.Name]; ok2 {
					v, err = c.compileExprHint(f.Value, ft)
				} else {
					v, err = c.compileExpr(f.Value)
				}
			} else {
				v, err = c.compileExpr(f.Value)
			}
			if err != nil {
				return "", err
			}
			parts[i] = fmt.Sprintf("%s: %s", exportName(sanitizeName(f.Name)), v)
		}
		return fmt.Sprintf("%s{%s}", sanitizeName(p.Struct.Name), strings.Join(parts, ", ")), nil
	case p.List != nil:
		elems := make([]string, len(p.List.Elems))
		typ := c.inferPrimaryType(p)
		elemType := "any"
		var elemT types.Type = types.AnyType{}
		if lt, ok := typ.(types.ListType); ok {
			elemType = goType(lt.Elem)
			elemT = lt.Elem
		}
		for i, e := range p.List.Elems {
			v, err := c.compileExpr(e)
			if err != nil {
				return "", err
			}
			et := c.inferExprType(e)
			if _, ok := elemT.(types.AnyType); ok && isList(et) && !isAny(et.(types.ListType).Elem) {
				c.use("_toAnySlice")
				v = fmt.Sprintf("_toAnySlice(%s)", v)
			} else if elt, ok := elemT.(types.ListType); ok {
				if etl, ok := et.(types.ListType); ok && equalTypes(elt.Elem, etl.Elem) && goType(elt.Elem) != goType(etl.Elem) {
					c.use("_convSlice")
					v = fmt.Sprintf("_convSlice[%s,%s](%s)", goType(etl.Elem), goType(elt.Elem), v)
				}
			}
			elems[i] = v
		}
		return "[]" + elemType + "{" + joinItems(elems, c.indent, 3) + "}", nil
	case p.Map != nil:
		typ := c.inferPrimaryType(p)
		if st, ok := typ.(types.StructType); ok {
			parts := make([]string, len(p.Map.Items))
			for i, item := range p.Map.Items {
				key, ok2 := simpleStringKey(item.Key)
				if !ok2 {
					return "", fmt.Errorf("struct field must be identifier")
				}
				ft := st.Fields[key]
				v, err := c.compileExprHint(item.Value, ft)
				if err != nil {
					return "", err
				}
				parts[i] = fmt.Sprintf("%s: %s", exportName(sanitizeName(key)), v)
			}
			c.compileStructType(st)
			return fmt.Sprintf("%s{%s}", sanitizeName(st.Name), joinItems(parts, c.indent, 1)), nil
		}
		keyType := "string"
		valType := "any"
		if mt, ok := typ.(types.MapType); ok {
			keyType = goType(mt.Key)
			valType = goType(mt.Value)
		}

		parts := make([]string, len(p.Map.Items))
		for i, item := range p.Map.Items {
			var k string
			if s, ok := simpleStringKey(item.Key); ok {
				k = fmt.Sprintf("\"%s\"", s)
			} else {
				kk, err := c.compileExpr(item.Key)
				if err != nil {
					return "", err
				}
				k = fmt.Sprintf("%s", kk)
				if !(strings.HasPrefix(k, "\"") && strings.HasSuffix(k, "\"")) {
					k = fmt.Sprintf("%s", kk)
				}
			}
			v, err := c.compileExpr(item.Value)
			if err != nil {
				return "", err
			}
			if valType == "int64" && goType(c.inferExprType(item.Value)) == "int" {
				v = fmt.Sprintf("int64(%s)", v)
			}
			parts[i] = fmt.Sprintf("%s: %s", k, v)
		}

		return fmt.Sprintf("map[%s]%s{%s}", keyType, valType, joinItems(parts, c.indent, 2)), nil

	case p.Query != nil:
		return c.compileQueryExpr(p.Query, nil)

	case p.Match != nil:
		return c.compileMatchExpr(p.Match)

	case p.If != nil:
		return c.compileIfExpr(p.If)

	case p.Fetch != nil:
		return c.compileFetchExpr(p.Fetch)

	case p.Generate != nil:
		return c.compileGenerateExpr(p.Generate)
	case p.Load != nil:
		return c.compileLoadExpr(p.Load)
	case p.Save != nil:
		return c.compileSaveExpr(p.Save)
	default:
		return "nil", fmt.Errorf("unsupported primary expression")
	}
}

func (c *Compiler) compileFetchExpr(f *parser.FetchExpr) (string, error) {
	urlStr, err := c.compileExpr(f.URL)
	if err != nil {
		return "", err
	}

	var withStr string
	if f.With != nil {
		w, err := c.compileExpr(f.With)
		if err != nil {
			return "", err
		}
		c.use("_toAnyMap")
		withStr = fmt.Sprintf("_toAnyMap(%s)", w)
	} else {
		withStr = "nil"
	}
	// _fetch uses _toAnyMap internally for headers and query handling, so
	// ensure it is always included to avoid undefined symbol errors.
	c.use("_toAnyMap")

	c.imports["net/http"] = true
	c.imports["io"] = true
	c.imports["encoding/json"] = true
	c.imports["bytes"] = true
	c.imports["neturl \"net/url\""] = true
	c.imports["os"] = true
	c.imports["time"] = true
	c.imports["fmt"] = true
	c.use("_fetch")
	return fmt.Sprintf("_fetch(%s, %s)", urlStr, withStr), nil
}

func (c *Compiler) compileGenerateExpr(g *parser.GenerateExpr) (string, error) {
	var (
		prompt string
		text   string
		model  string
		params []string
	)
	for _, f := range g.Fields {
		v, err := c.compileExpr(f.Value)
		if err != nil {
			return "", err
		}
		switch f.Name {
		case "prompt":
			prompt = v
		case "text":
			text = v
		case "model":
			model = v
		default:
			params = append(params, fmt.Sprintf("%q: %s", f.Name, v))
		}
	}

	if prompt == "" && g.Target != "embedding" {
		prompt = "\"\""
	}
	if text == "" && g.Target == "embedding" {
		text = "\"\""
	}

	paramStr := "nil"
	if len(params) > 0 {
		paramStr = fmt.Sprintf("map[string]any{%s}", strings.Join(params, ", "))
	}

	if model == "" {
		model = "\"\""
	}

	c.imports["context"] = true
	c.imports["mochi/runtime/llm"] = true

	if g.Target == "embedding" {
		c.use("_genEmbed")
		return fmt.Sprintf("_genEmbed(%s, %s, %s)", text, model, paramStr), nil
	}

	if _, ok := c.env.GetStruct(g.Target); ok {
		c.use("_genStruct")
		c.imports["encoding/json"] = true
		return fmt.Sprintf("_genStruct[%s](%s, %s, %s)", sanitizeName(g.Target), prompt, model, paramStr), nil
	}

	c.use("_genText")
	return fmt.Sprintf("_genText(%s, %s, %s)", prompt, model, paramStr), nil
}

func (c *Compiler) compileLoadExpr(l *parser.LoadExpr) (string, error) {
	path := "\"\""
	if l.Path != nil {
		p := *l.Path
		if strings.HasPrefix(p, "../") {
			p = filepath.Join("..", "..", "..", "tests", p[3:])
		}
		path = fmt.Sprintf("%q", p)
	}
	opts := "nil"
	if l.With != nil {
		v, err := c.compileExpr(l.With)
		if err != nil {
			return "", err
		}
		c.use("_toAnyMap")
		opts = fmt.Sprintf("_toAnyMap(%s)", v)
	}
	c.imports["mochi/runtime/data"] = true
	c.imports["os"] = true
	c.use("_load")
	if l.Type != nil {
		t := c.resolveTypeRef(l.Type)
		if st, ok := c.env.GetStruct(*l.Type.Simple); t == (types.AnyType{}) && ok {
			t = st
		}
		goT := goType(t)
		if goT == "" {
			goT = "any"
		}
		var buf bytes.Buffer
		buf.WriteString(fmt.Sprintf("func() []%s {\n", goT))
		buf.WriteString(fmt.Sprintf("\trows := _load(%s, %s)\n", path, opts))
		buf.WriteString(fmt.Sprintf("\tout := make([]%s, len(rows))\n", goT))
		buf.WriteString("\tfor i, r := range rows {\n")
		if goT == "any" {
			buf.WriteString("\t\tout[i] = r\n")
		} else {
			buf.WriteString(fmt.Sprintf("\t\tout[i] = r.(%s)\n", goT))
		}
		buf.WriteString("\t}\n")
		buf.WriteString("\treturn out\n")
		buf.WriteString("}()")
		return buf.String(), nil
	}
	return fmt.Sprintf("_load(%s, %s)", path, opts), nil
}

func (c *Compiler) compileSaveExpr(s *parser.SaveExpr) (string, error) {
	src, err := c.compileExpr(s.Src)
	if err != nil {
		return "", err
	}
	path := "\"\""
	if s.Path != nil {
		path = fmt.Sprintf("%q", *s.Path)
	}
	opts := "nil"
	if s.With != nil {
		v, err := c.compileExpr(s.With)
		if err != nil {
			return "", err
		}
		c.use("_toAnyMap")
		opts = fmt.Sprintf("_toAnyMap(%s)", v)
	}
	c.imports["mochi/runtime/data"] = true
	c.imports["os"] = true
	c.use("_save")
	c.use("_toMapSlice")
	return fmt.Sprintf("_save(%s, %s, %s)", src, path, opts), nil
}

func (c *Compiler) compileIfExpr(ie *parser.IfExpr) (string, error) {
	expr := &parser.Expr{Binary: &parser.BinaryExpr{Left: &parser.Unary{Value: &parser.PostfixExpr{Target: &parser.Primary{If: ie}}}}}
	retT := c.inferExprType(expr)
	retType := goType(retT)
	if retType == "" {
		retType = "any"
	}

	cond, err := c.compileExpr(ie.Cond)
	if err != nil {
		return "", err
	}
	ct := c.inferExprType(ie.Cond)
	if !isBool(ct) {
		cond = c.castExpr(cond, ct, types.BoolType{})
	}
	thenExpr, err := c.compileExpr(ie.Then)
	if err != nil {
		return "", err
	}
	thenType := c.inferExprType(ie.Then)
	if retType != "any" && !equalTypes(retT, thenType) {
		thenExpr = c.castExpr(thenExpr, thenType, retT)
	} else if retType == "[]any" {
		if lt, ok := thenType.(types.ListType); ok && !isAny(lt.Elem) {
			c.use("_toAnySlice")
			thenExpr = fmt.Sprintf("_toAnySlice(%s)", thenExpr)
		}
	}

	var elseExpr string
	var elseType types.Type = types.AnyType{}
	if ie.ElseIf != nil {
		elseExpr, err = c.compileIfExpr(ie.ElseIf)
		elseType = types.IfExprType(ie.ElseIf, c.env)
	} else if ie.Else != nil {
		elseExpr, err = c.compileExpr(ie.Else)
		elseType = c.inferExprType(ie.Else)
	}
	if err != nil {
		return "", err
	}
	if elseExpr != "" {
		if retType != "any" && !equalTypes(retT, elseType) {
			elseExpr = c.castExpr(elseExpr, elseType, retT)
		} else if retType == "[]any" {
			if lt, ok := elseType.(types.ListType); ok && !isAny(lt.Elem) {
				c.use("_toAnySlice")
				elseExpr = fmt.Sprintf("_toAnySlice(%s)", elseExpr)
			}
		}
	}

	var buf bytes.Buffer
	buf.WriteString("func() " + retType + " {\n")
	buf.WriteString("\tif " + cond + " {\n")
	buf.WriteString("\t\treturn " + thenExpr + "\n")
	if elseExpr != "" {
		buf.WriteString("\t} else {\n")
		buf.WriteString("\t\treturn " + elseExpr + "\n")
		buf.WriteString("\t}\n")
	} else {
		buf.WriteString("\t}\n")
		if retType == "any" {
			buf.WriteString("\treturn nil\n")
		} else {
			buf.WriteString(fmt.Sprintf("\tvar _zero %s\n\treturn _zero\n", retType))
		}
	}
	buf.WriteString("}()")
	return buf.String(), nil
}

func (c *Compiler) compileQueryExpr(q *parser.QueryExpr, hint types.Type) (string, error) {
	src, err := c.compileExpr(q.Source)
	if err != nil {
		return "", err
	}

	needsHelper := q.Sort != nil
	for _, j := range q.Joins {
		if j.Side != nil {
			needsHelper = true
			break
		}
	}

	// Prepare environment for the query variable
	srcType := c.inferExprType(q.Source)
	var elemType types.Type = types.AnyType{}
	directRange := false
	if lt, ok := srcType.(types.ListType); ok {
		elemType = lt.Elem
		directRange = true
	}
	original := c.env
	child := types.NewEnv(c.env)
	child.SetVar(q.Var, elemType, true)
	// Add cross join variables to environment
	for _, f := range q.Froms {
		ft := c.inferExprType(f.Src)
		var felem types.Type = types.AnyType{}
		if lt, ok := ft.(types.ListType); ok {
			felem = lt.Elem
		}
		child.SetVar(f.Var, felem, true)
	}
	// Add join variables to environment
	for _, j := range q.Joins {
		jt := c.inferExprType(j.Src)
		var jelem types.Type = types.AnyType{}
		if lt, ok := jt.(types.ListType); ok {
			jelem = lt.Elem
		}
		child.SetVar(j.Var, jelem, true)
	}

	var groupKey string
	if q.Group != nil {
		gtype := types.GroupType{Elem: elemType}
		keyEnv := child
		c.env = keyEnv
		var err error
		groupKey, err = c.compileExpr(q.Group.Exprs[0])
		if err != nil {
			c.env = original
			return "", err
		}
		child.SetVar(q.Group.Name, gtype, true)
	}
	c.env = child

	// compile cross join sources
	fromSrcs := make([]string, len(q.Froms))
	fromDirect := make([]bool, len(q.Froms))
	for i, f := range q.Froms {
		fs, err := c.compileExpr(f.Src)
		if err != nil {
			c.env = original
			return "", err
		}
		fromSrcs[i] = fs
		if _, ok := c.inferExprType(f.Src).(types.ListType); ok {
			fromDirect[i] = true
		}
	}

	var sel string
	var retElem string
	if lt, ok := hint.(types.ListType); ok {
		sel, err = c.compileExprHint(q.Select, lt.Elem)
		if err != nil {
			c.env = original
			return "", err
		}
		retElem = goType(lt.Elem)
		if retElem == "" {
			retElem = "any"
		}
	} else {
		sel, err = c.compileExpr(q.Select)
		if err != nil {
			c.env = original
			return "", err
		}
		retElem = goType(c.inferExprType(q.Select))
		if retElem == "" {
			retElem = "any"
		}
	}
	var havingExpr string
	if q.Group != nil && q.Group.Having != nil {
		havingExpr, err = c.compileExpr(q.Group.Having)
		if err != nil {
			c.env = original
			return "", err
		}
	}
	var cond string
	if q.Where != nil {
		cond, err = c.compileExpr(q.Where)
		if err != nil {
			c.env = original
			return "", err
		}
		ct := c.inferExprType(q.Where)
		if !isBool(ct) {
			cond = c.castExpr(cond, ct, types.BoolType{})
		}
	}
	var sortExpr string
	if q.Sort != nil {
		sortExpr, err = c.compileExpr(q.Sort)
		if err != nil {
			c.env = original
			return "", err
		}
	}
	var skipExpr string
	if q.Skip != nil {
		skipExpr, err = c.compileExpr(q.Skip)
		if err != nil {
			c.env = original
			return "", err
		}
	}
	var takeExpr string
	if q.Take != nil {
		takeExpr, err = c.compileExpr(q.Take)
		if err != nil {
			c.env = original
			return "", err
		}
	}

	// compile join sources and conditions while join vars are in scope
	joinSrcs := make([]string, len(q.Joins))
	joinOns := make([]string, len(q.Joins))
	joinDirect := make([]bool, len(q.Joins))
	joinSides := make([]string, len(q.Joins))
	joinTypes := make([]string, len(q.Joins))
	joinLeftKeys := make([]string, len(q.Joins))
	joinRightKeys := make([]string, len(q.Joins))
	for i, j := range q.Joins {
		js, err := c.compileExpr(j.Src)
		if err != nil {
			c.env = original
			return "", err
		}
		joinSrcs[i] = js
		jt := c.inferExprType(j.Src)
		var je types.Type = types.AnyType{}
		if lt, ok := jt.(types.ListType); ok {
			joinDirect[i] = true
			je = lt.Elem
		}
		on, err := c.compileExpr(j.On)
		if err != nil {
			c.env = original
			return "", err
		}
		joinOns[i] = on
		if lk, rk, ok := c.eqJoinKeys(j.On, q.Var, j.Var); ok {
			joinLeftKeys[i] = lk
			joinRightKeys[i] = rk
		}
		if j.Side != nil {
			joinSides[i] = *j.Side
		}
		t := goType(je)
		if t == "" {
			t = "any"
		}
		joinTypes[i] = t
	}
	varNames := []string{q.Var}
	for _, f := range q.Froms {
		varNames = append(varNames, f.Var)
	}
	for _, j := range q.Joins {
		varNames = append(varNames, j.Var)
	}
	condStep := len(varNames) - 1
	if q.Where != nil {
		deps := map[string]struct{}{}
		collectIdents(q.Where, deps)
		idx := -1
		for i, n := range varNames {
			if _, ok := deps[n]; ok && i > idx {
				idx = i
			}
		}
		if idx >= 0 {
			condStep = idx
		} else {
			condStep = 0
		}
	}

	if q.Group != nil && len(q.Froms) == 0 && len(q.Joins) == 0 && q.Where == nil && q.Sort == nil && q.Skip == nil && q.Take == nil {
		c.env = original
		c.imports["fmt"] = true
		c.imports["mochi/runtime/data"] = true
		var buf bytes.Buffer
		buf.WriteString(fmt.Sprintf("func() []%s {\n", retElem))
		buf.WriteString("\tgroups := map[string]*data.Group{}\n")
		buf.WriteString("\torder := []string{}\n")
		buf.WriteString(fmt.Sprintf("\tfor _, %s := range %s {\n", sanitizeName(q.Var), src))
		buf.WriteString(fmt.Sprintf("\t\tkey := %s\n", groupKey))
		buf.WriteString("\t\tks := fmt.Sprint(key)\n")
		buf.WriteString("\t\tg, ok := groups[ks]\n")
		buf.WriteString("\t\tif !ok {\n")
		buf.WriteString("\t\t\tg = &data.Group{Key: key}\n")
		buf.WriteString("\t\t\tgroups[ks] = g\n")
		buf.WriteString("\t\t\torder = append(order, ks)\n")
		buf.WriteString("\t\t}\n")
		buf.WriteString(fmt.Sprintf("\t\tg.Items = append(g.Items, %s)\n", sanitizeName(q.Var)))
		buf.WriteString("\t}\n")
		buf.WriteString(fmt.Sprintf("\t_res := []%s{}\n", retElem))
		alias := sanitizeName(q.Group.Name)
		buf.WriteString("\tfor _, ks := range order {\n")
		buf.WriteString("\t\tg := groups[ks]\n")
		if alias != "g" {
			buf.WriteString(fmt.Sprintf("\t\t%s := g\n", alias))
		}
		if havingExpr != "" {
			buf.WriteString(fmt.Sprintf("\t\tif !(%s) { continue }\n", havingExpr))
		}
		buf.WriteString(fmt.Sprintf("\t\t_res = append(_res, %s)\n", sel))
		buf.WriteString("\t}\n")
		buf.WriteString("\treturn _res\n")
		buf.WriteString("}()")
		return buf.String(), nil
	}

	if q.Group != nil {
		c.env = original
		c.imports["fmt"] = true
		c.imports["mochi/runtime/data"] = true
		var buf bytes.Buffer
		buf.WriteString(fmt.Sprintf("func() []%s {\n", retElem))
		buf.WriteString("\tgroups := map[string]*data.Group{}\n")
		buf.WriteString("\torder := []string{}\n")
		buf.WriteString(fmt.Sprintf("\tfor _, %s := range %s {\n", sanitizeName(q.Var), src))
		indent := "\t\t"
		for i := range q.Froms {
			fvar := sanitizeName(q.Froms[i].Var)
			loopVar := fvar
			if fvar == "_" {
				loopVar = "v"
			}
			fsrc := fromSrcs[i]
			if fromDirect[i] {
				buf.WriteString(fmt.Sprintf(indent+"for _, %s := range %s {\n", loopVar, fsrc))
			} else {
				return "", fmt.Errorf("query from source must be list")
			}
			indent += "\t"
		}
		specialLeft := len(q.Joins) == 1 && (joinSides[0] == "left")
		specialRight := len(q.Joins) == 1 && (joinSides[0] == "right")
		if specialLeft || specialRight {
			jvar := sanitizeName(q.Joins[0].Var)
			loopVar := jvar
			if jvar == "_" {
				loopVar = "v"
			}
			jsrc := joinSrcs[0]
			buf.WriteString(indent + "matched := false\n")
			if joinDirect[0] {
				buf.WriteString(fmt.Sprintf(indent+"for _, %s := range %s {\n", loopVar, jsrc))
			} else {
				return "", fmt.Errorf("join source must be list")
			}
			indent += "\t"
			buf.WriteString(fmt.Sprintf(indent+"if !(%s) { continue }\n", joinOns[0]))
			buf.WriteString(indent + "matched = true\n")
		} else {
			for i := range q.Joins {
				jvar := sanitizeName(q.Joins[i].Var)
				loopVar := jvar
				if jvar == "_" {
					loopVar = "v"
				}
				jsrc := joinSrcs[i]
				if joinDirect[i] {
					buf.WriteString(fmt.Sprintf(indent+"for _, %s := range %s {\n", loopVar, jsrc))
				} else {
					return "", fmt.Errorf("join source must be list")
				}
				indent += "\t"
				buf.WriteString(fmt.Sprintf(indent+"if !(%s) { continue }\n", joinOns[i]))
			}
		}
		if cond != "" {
			buf.WriteString(fmt.Sprintf(indent+"if %s {\n", cond))
			indent += "\t"
		}
		buf.WriteString(fmt.Sprintf(indent+"key := %s\n", groupKey))
		buf.WriteString(indent + "ks := fmt.Sprint(key)\n")
		buf.WriteString(indent + "g, ok := groups[ks]\n")
		buf.WriteString(indent + "if !ok {\n")
		buf.WriteString(indent + "\tg = &data.Group{Key: key}\n")
		buf.WriteString(indent + "\tgroups[ks] = g\n")
		buf.WriteString(indent + "\torder = append(order, ks)\n")
		buf.WriteString(indent + "}\n")
		itemVar := "_item"
		buf.WriteString(fmt.Sprintf(indent+"%s := map[string]any{}\n", itemVar))
		c.use("_toAnyMap")
		buf.WriteString(fmt.Sprintf(indent+"for k, v := range _toAnyMap(%s) { %s[k] = v }\n", sanitizeName(q.Var), itemVar))
		buf.WriteString(fmt.Sprintf(indent+"%s[\"%s\"] = %s\n", itemVar, sanitizeName(q.Var), sanitizeName(q.Var)))
		for _, f := range q.Froms {
			c.use("_toAnyMap")
			buf.WriteString(fmt.Sprintf(indent+"for k, v := range _toAnyMap(%s) { %s[k] = v }\n", sanitizeName(f.Var), itemVar))
			buf.WriteString(fmt.Sprintf(indent+"%s[\"%s\"] = %s\n", itemVar, sanitizeName(f.Var), sanitizeName(f.Var)))
		}
		for _, j := range q.Joins {
			c.use("_toAnyMap")
			buf.WriteString(fmt.Sprintf(indent+"for k, v := range _toAnyMap(%s) { %s[k] = v }\n", sanitizeName(j.Var), itemVar))
			buf.WriteString(fmt.Sprintf(indent+"%s[\"%s\"] = %s\n", itemVar, sanitizeName(j.Var), sanitizeName(j.Var)))
		}
		buf.WriteString(fmt.Sprintf(indent+"g.Items = append(g.Items, %s)\n", itemVar))
		if cond != "" {
			indent = indent[:len(indent)-1]
			buf.WriteString(indent + "}\n")
		}
		if specialLeft || specialRight {
			indent = indent[:len(indent)-1]
			buf.WriteString(indent + "}\n")
			buf.WriteString(indent + "if !matched {\n")
			indent += "\t"
			buf.WriteString(fmt.Sprintf(indent+"var %s %s\n", sanitizeName(q.Joins[0].Var), joinTypes[0]))
			if cond != "" {
				buf.WriteString(fmt.Sprintf(indent+"if %s {\n", cond))
				indent += "\t"
			}
			buf.WriteString(fmt.Sprintf(indent+"key := %s\n", groupKey))
			buf.WriteString(indent + "ks := fmt.Sprint(key)\n")
			buf.WriteString(indent + "g, ok := groups[ks]\n")
			buf.WriteString(indent + "if !ok {\n")
			buf.WriteString(indent + "\tg = &data.Group{Key: key}\n")
			buf.WriteString(indent + "\tgroups[ks] = g\n")
			buf.WriteString(indent + "\torder = append(order, ks)\n")
			buf.WriteString(indent + "}\n")
			itemVar := "_item"
			buf.WriteString(fmt.Sprintf(indent+"%s := map[string]any{}\n", itemVar))
			c.use("_toAnyMap")
			buf.WriteString(fmt.Sprintf(indent+"for k, v := range _toAnyMap(%s) { %s[k] = v }\n", sanitizeName(q.Var), itemVar))
			buf.WriteString(fmt.Sprintf(indent+"%s[\"%s\"] = %s\n", itemVar, sanitizeName(q.Var), sanitizeName(q.Var)))
			for _, f := range q.Froms {
				c.use("_toAnyMap")
				buf.WriteString(fmt.Sprintf(indent+"for k, v := range _toAnyMap(%s) { %s[k] = v }\n", sanitizeName(f.Var), itemVar))
				buf.WriteString(fmt.Sprintf(indent+"%s[\"%s\"] = %s\n", itemVar, sanitizeName(f.Var), sanitizeName(f.Var)))
			}
			for _, j := range q.Joins {
				c.use("_toAnyMap")
				buf.WriteString(fmt.Sprintf(indent+"for k, v := range _toAnyMap(%s) { %s[k] = v }\n", sanitizeName(j.Var), itemVar))
				buf.WriteString(fmt.Sprintf(indent+"%s[\"%s\"] = %s\n", itemVar, sanitizeName(j.Var), sanitizeName(j.Var)))
			}
			buf.WriteString(fmt.Sprintf(indent+"g.Items = append(g.Items, %s)\n", itemVar))
			if cond != "" {
				indent = indent[:len(indent)-1]
				buf.WriteString(indent + "}\n")
			}
			indent = indent[:len(indent)-1]
			buf.WriteString(indent + "}\n")
		} else {
			for i := len(q.Joins) - 1; i >= 0; i-- {
				indent = indent[:len(indent)-1]
				buf.WriteString(indent + "}\n")
			}
		}
		for i := len(q.Froms) - 1; i >= 0; i-- {
			indent = indent[:len(indent)-1]
			buf.WriteString(indent + "}\n")
		}
		indent = indent[:len(indent)-1]
		buf.WriteString(indent + "}\n")

		buf.WriteString("\titems := []*data.Group{}\n")
		buf.WriteString("\tfor _, ks := range order {\n")
		buf.WriteString("\t\titems = append(items, groups[ks])\n")
		buf.WriteString("\t}\n")

		if sortExpr != "" {
			buf.WriteString("\ttype pair struct { item *data.Group; key any }\n")
			buf.WriteString("\tpairs := make([]pair, len(items))\n")
			buf.WriteString("\tfor idx, it := range items {\n")
			buf.WriteString(fmt.Sprintf("\t\t%s := it\n", sanitizeName(q.Group.Name)))
			if _, ok := c.inferExprType(q.Sort).(types.ListType); ok {
				c.use("_toAnySlice")
				buf.WriteString(fmt.Sprintf("\t\tpairs[idx] = pair{item: it, key: _toAnySlice(%s)}\n", sortExpr))
			} else {
				buf.WriteString(fmt.Sprintf("\t\tpairs[idx] = pair{item: it, key: %s}\n", sortExpr))
			}
			buf.WriteString("\t}\n")
			buf.WriteString("\tsort.Slice(pairs, func(i, j int) bool {\n")
			buf.WriteString("\t\ta, b := pairs[i].key, pairs[j].key\n")
			buf.WriteString("\t\tswitch av := a.(type) {\n")
			buf.WriteString("\t\tcase int:\n")
			buf.WriteString("\t\t\tswitch bv := b.(type) {\n")
			buf.WriteString("\t\t\tcase int:\n")
			buf.WriteString("\t\t\t\treturn av < bv\n")
			buf.WriteString("\t\t\tcase float64:\n")
			buf.WriteString("\t\t\t\treturn float64(av) < bv\n")
			buf.WriteString("\t\t\t}\n")
			buf.WriteString("\t\tcase float64:\n")
			buf.WriteString("\t\t\tswitch bv := b.(type) {\n")
			buf.WriteString("\t\t\tcase int:\n")
			buf.WriteString("\t\t\t\treturn av < float64(bv)\n")
			buf.WriteString("\t\t\tcase float64:\n")
			buf.WriteString("\t\t\t\treturn av < bv\n")
			buf.WriteString("\t\t\t}\n")
			buf.WriteString("\t\tcase string:\n")
			buf.WriteString("\t\t\tbs, _ := b.(string)\n")
			buf.WriteString("\t\t\treturn av < bs\n")
			buf.WriteString("\t\t}\n")
			buf.WriteString("\t\treturn fmt.Sprint(a) < fmt.Sprint(b)\n")
			buf.WriteString("\t})\n")
			buf.WriteString("\tfor idx, p := range pairs {\n")
			buf.WriteString("\t\titems[idx] = p.item\n")
			buf.WriteString("\t}\n")
		}

		if skipExpr != "" || takeExpr != "" {
			c.use("_paginate")
			sk := skipExpr
			if sk == "" {
				sk = "-1"
			}
			tk := takeExpr
			if tk == "" {
				tk = "-1"
			}
			buf.WriteString(fmt.Sprintf("\titems = _paginate[*data.Group](items, %s, %s)\n", sk, tk))
		}

		buf.WriteString(fmt.Sprintf("\t_res := []%s{}\n", retElem))
		buf.WriteString(fmt.Sprintf("\tfor _, %s := range items {\n", sanitizeName(q.Group.Name)))
		if havingExpr != "" {
			buf.WriteString(fmt.Sprintf("\t\tif !(%s) { continue }\n", havingExpr))
		}
		buf.WriteString(fmt.Sprintf("\t\t_res = append(_res, %s)\n", sel))
		buf.WriteString("\t}\n")
		buf.WriteString("\treturn _res\n")
		buf.WriteString("}()")
		return buf.String(), nil
	}

	c.env = original
	if needsHelper {
		varNames := []string{sanitizeName(q.Var)}
		paramTypes := []string{goType(elemType)}
		for _, f := range q.Froms {
			varNames = append(varNames, sanitizeName(f.Var))
			ft := c.inferExprType(f.Src)
			var felem types.Type = types.AnyType{}
			if lt, ok := ft.(types.ListType); ok {
				felem = lt.Elem
			}
			paramTypes = append(paramTypes, goType(felem))
		}
		params := append([]string(nil), varNames...)
		if lt, ok := srcType.(types.ListType); !ok || !isAny(lt.Elem) {
			c.use("_toAnySlice")
			src = fmt.Sprintf("_toAnySlice(%s)", src)
		}
		for i := range joinSrcs {
			if lt, ok := c.inferExprType(q.Joins[i].Src).(types.ListType); !ok || !isAny(lt.Elem) {
				c.use("_toAnySlice")
				joinSrcs[i] = fmt.Sprintf("_toAnySlice(%s)", joinSrcs[i])
			}
		}
		assign := func(names []string, types []string) string {
			parts := make([]string, len(names))
			for i, n := range names {
				typ := types[i]
				if typ == "" {
					typ = "any"
				}
				if typ != "any" {
					tmp := fmt.Sprintf("tmp%d", i)
					parts[i] = fmt.Sprintf("%s := _a[%d]; var %s %s; if %s != nil { %s = %s.(%s) }; _ = %s", tmp, i, n, typ, tmp, n, tmp, typ, n)
				} else {
					parts[i] = fmt.Sprintf("%s := _a[%d]; _ = %s", n, i, n)
				}
			}
			return strings.Join(parts, "; ")
		}
		joins := make([]string, 0, len(q.Froms)+len(q.Joins))
		for _, fs := range fromSrcs {
			joins = append(joins, fmt.Sprintf("{items: %s}", fs))
		}
		for i, js := range joinSrcs {
			onParams := append(params, sanitizeName(q.Joins[i].Var))
			onTypes := append(paramTypes, joinTypes[i])
			onFn := fmt.Sprintf("func(_a ...any) bool { %s; return %s }", assign(onParams, onTypes), joinOns[i])
			spec := fmt.Sprintf("{items: %s, on: %s", js, onFn)
			if joinLeftKeys[i] != "" && joinRightKeys[i] != "" {
				lkAssign := assign(params, paramTypes)
				lk := fmt.Sprintf("func(_a ...any) any { %s; return %s }", lkAssign, joinLeftKeys[i])
				var rk string
				if joinTypes[i] != "any" {
					rk = fmt.Sprintf("func(_v any) any { %s := _v.(%s); _ = %s; return %s }", sanitizeName(q.Joins[i].Var), joinTypes[i], sanitizeName(q.Joins[i].Var), joinRightKeys[i])
				} else {
					rk = fmt.Sprintf("func(_v any) any { %s := _v; _ = %s; return %s }", sanitizeName(q.Joins[i].Var), sanitizeName(q.Joins[i].Var), joinRightKeys[i])
				}
				spec += ", leftKey: " + lk + ", rightKey: " + rk
			}
			if joinSides[i] == "left" || joinSides[i] == "outer" {
				spec += ", left: true"
			}
			if joinSides[i] == "right" || joinSides[i] == "outer" {
				spec += ", right: true"
			}
			spec += "}"
			joins = append(joins, spec)
			params = append(params, sanitizeName(q.Joins[i].Var))
			paramTypes = append(paramTypes, joinTypes[i])
		}
		assignParams := assign(params, paramTypes)
		selectFn := fmt.Sprintf("func(_a ...any) any { %s; return %s }", assignParams, sel)
		var whereFn, sortFn string
		if cond != "" {
			whereFn = fmt.Sprintf("func(_a ...any) bool { %s; return %s }", assignParams, cond)
		}
		if sortExpr != "" {
			sortFn = fmt.Sprintf("func(_a ...any) any { %s; return %s }", assignParams, sortExpr)
		}

		var buf bytes.Buffer
		buf.WriteString(fmt.Sprintf("func() []%s {\n", retElem))
		c.use("_query")
		buf.WriteString(fmt.Sprintf("\tsrc := %s\n", src))
		buf.WriteString("\tresAny := _query(src, []_joinSpec{\n")
		for _, j := range joins {
			buf.WriteString("\t\t" + j + ",\n")
		}
		buf.WriteString("\t}, _queryOpts{selectFn: " + selectFn)
		if whereFn != "" {
			buf.WriteString(", where: " + whereFn)
		}
		if sortFn != "" {
			buf.WriteString(", sortKey: " + sortFn)
		}
		if skipExpr != "" {
			buf.WriteString(", skip: " + skipExpr)
		} else {
			buf.WriteString(", skip: -1")
		}
		if takeExpr != "" {
			buf.WriteString(", take: " + takeExpr)
		} else {
			buf.WriteString(", take: -1")
		}
		buf.WriteString("})\n")
		buf.WriteString(fmt.Sprintf("\tout := make([]%s, len(resAny))\n", retElem))
		buf.WriteString("\tfor i, v := range resAny {\n")
		if retElem == "any" {
			buf.WriteString("\t\tout[i] = v\n")
		} else {
			buf.WriteString(fmt.Sprintf("\t\tout[i] = v.(%s)\n", retElem))
		}
		buf.WriteString("\t}\n")
		buf.WriteString("\treturn out\n")
		buf.WriteString("}()")
		return buf.String(), nil
	}
	if !directRange {
		switch srcType.(type) {
		case types.GroupType:
			src = fmt.Sprintf("%s.Items", src)
		default:
			c.use("_toAnySlice")
			src = fmt.Sprintf("_toAnySlice(%s)", src)
		}
		directRange = true
	}

	if len(q.Joins) == 0 && len(q.Froms) == 0 && q.Group == nil && q.Sort == nil && q.Skip == nil && q.Take == nil {
		if call, ok := callPattern(q.Select); ok && len(call.Args) == 1 {
			fn := call.Func
			if fn == "sum" || fn == "count" || fn == "avg" || fn == "min" || fn == "max" {
				if name, ok := identName(call.Args[0]); ok && name == q.Var {
					tmpQ := *q
					tmpQ.Select = &parser.Expr{Binary: &parser.BinaryExpr{Left: &parser.Unary{Value: &parser.PostfixExpr{Target: &parser.Primary{Selector: &parser.SelectorExpr{Root: q.Var}}}}}}
					items, err := c.compileQueryExpr(&tmpQ, nil)
					if err != nil {
						return "", err
					}
					elemGo := goType(elemType)
					if elemGo == "" {
						elemGo = "any"
					}
					switch fn {
					case "sum":
						if isNumeric(elemType) {
							c.use("_sumOrdered")
							return fmt.Sprintf("_sumOrdered[%s](%s)", elemGo, items), nil
						}
						c.use("_sum")
						return fmt.Sprintf("_sum(%s)", items), nil
					case "count":
						return fmt.Sprintf("len(%s)", items), nil
					case "avg":
						if isNumeric(elemType) {
							c.use("_avgOrdered")
							return fmt.Sprintf("_avgOrdered[%s](%s)", elemGo, items), nil
						}
						c.use("_avg")
						return fmt.Sprintf("_avg(%s)", items), nil
					case "min":
						if isComparableSimple(elemType) {
							c.use("_minOrdered")
							return fmt.Sprintf("_minOrdered[%s](%s)", elemGo, items), nil
						}
						c.use("_min")
						return fmt.Sprintf("_min(%s)", items), nil
					case "max":
						if isComparableSimple(elemType) {
							c.use("_maxOrdered")
							return fmt.Sprintf("_maxOrdered[%s](%s)", elemGo, items), nil
						}
						c.use("_max")
						return fmt.Sprintf("_max(%s)", items), nil
					}
				}
			}
		}
	}

	simple := q.Sort == nil && q.Skip == nil && q.Take == nil

	elemGo := goType(elemType)
	if elemGo == "" {
		elemGo = "any"
	}

	var buf bytes.Buffer
	buf.WriteString(fmt.Sprintf("func() []%s {\n", retElem))
	if simple {
		buf.WriteString(fmt.Sprintf("\t_res := []%s{}\n", retElem))
	} else {
		buf.WriteString(fmt.Sprintf("\titems := []%s{}\n", elemGo))
	}
	indent := "\t"
	step := 0
	condOpened := false
	vname := sanitizeName(q.Var)
	loopVar := vname
	if vname == "_" {
		loopVar = "v"
	}
	buf.WriteString(fmt.Sprintf(indent+"for _, %s := range %s {\n", loopVar, src))
	indent += "\t"
	if cond != "" && condStep == step {
		buf.WriteString(fmt.Sprintf(indent+"if %s {\n", cond))
		indent += "\t"
		condOpened = true
	}
	step++

	for i := range q.Froms {
		fvar := sanitizeName(q.Froms[i].Var)
		fsrc := fromSrcs[i]
		if fromDirect[i] {
			buf.WriteString(fmt.Sprintf(indent+"for _, %s := range %s {\n", fvar, fsrc))
		} else {
			return "", fmt.Errorf("query from source must be list")
		}
		indent += "\t"
		if cond != "" && condStep == step {
			buf.WriteString(fmt.Sprintf(indent+"if %s {\n", cond))
			indent += "\t"
			condOpened = true
		}
		step++
	}

	specialLeft := len(q.Joins) == 1 && (joinSides[0] == "left")
	specialRight := len(q.Joins) == 1 && (joinSides[0] == "right")
	if specialLeft || specialRight {
		condStep = len(varNames) - 1
	}
	if specialLeft {
		jvar := sanitizeName(q.Joins[0].Var)
		loopVar := jvar
		if jvar == "_" {
			loopVar = "v"
		}
		jsrc := joinSrcs[0]
		buf.WriteString(indent + "matched := false\n")
		if joinDirect[0] {
			buf.WriteString(fmt.Sprintf(indent+"for _, %s := range %s {\n", loopVar, jsrc))
		} else {
			return "", fmt.Errorf("join source must be list")
		}
		indent += "\t"
		buf.WriteString(fmt.Sprintf(indent+"if !(%s) { continue }\n", joinOns[0]))
		buf.WriteString(indent + "matched = true\n")
	} else if specialRight {
		jvar := sanitizeName(q.Joins[0].Var)
		loopVar := jvar
		if jvar == "_" {
			loopVar = "v"
		}
		jsrc := joinSrcs[0]
		buf.WriteString(indent + "matched := false\n")
		if joinDirect[0] {
			buf.WriteString(fmt.Sprintf(indent+"for _, %s := range %s {\n", loopVar, jsrc))
		} else {
			return "", fmt.Errorf("join source must be list")
		}
		indent += "\t"
		buf.WriteString(fmt.Sprintf(indent+"if !(%s) { continue }\n", joinOns[0]))
		buf.WriteString(indent + "matched = true\n")
	} else {
		for i := range q.Joins {
			jvar := sanitizeName(q.Joins[i].Var)
			jsrc := joinSrcs[i]
			if joinDirect[i] {
				buf.WriteString(fmt.Sprintf(indent+"for _, %s := range %s {\n", jvar, jsrc))
			} else {
				return "", fmt.Errorf("join source must be list")
			}
			indent += "\t"
			buf.WriteString(fmt.Sprintf(indent+"if !(%s) { continue }\n", joinOns[i]))
			if cond != "" && condStep == step {
				buf.WriteString(fmt.Sprintf(indent+"if %s {\n", cond))
				indent += "\t"
				condOpened = true
			}
			step++
		}
	}

	if specialLeft || specialRight {
		if cond != "" {
			if simple {
				buf.WriteString(fmt.Sprintf(indent+"if %s {\n", cond))
				buf.WriteString(fmt.Sprintf(indent+"\t_res = append(_res, %s)\n", sel))
				buf.WriteString(indent + "}\n")
			} else {
				buf.WriteString(fmt.Sprintf(indent+"if %s {\n", cond))
				buf.WriteString(fmt.Sprintf(indent+"\titems = append(items, %s)\n", sanitizeName(q.Var)))
				buf.WriteString(indent + "}\n")
			}
		} else {
			if simple {
				buf.WriteString(fmt.Sprintf(indent+"_res = append(_res, %s)\n", sel))
			} else {
				buf.WriteString(fmt.Sprintf(indent+"items = append(items, %s)\n", sanitizeName(q.Var)))
			}
		}
		indent = indent[:len(indent)-1]
		buf.WriteString(indent + "}\n")
		buf.WriteString(indent + "if !matched {\n")
		indent += "\t"
		buf.WriteString(fmt.Sprintf(indent+"var %s %s\n", sanitizeName(q.Joins[0].Var), joinTypes[0]))
		if cond != "" {
			if simple {
				buf.WriteString(fmt.Sprintf(indent+"if %s {\n", cond))
				buf.WriteString(fmt.Sprintf(indent+"\t_res = append(_res, %s)\n", sel))
				buf.WriteString(indent + "}\n")
			} else {
				buf.WriteString(fmt.Sprintf(indent+"if %s {\n", cond))
				buf.WriteString(fmt.Sprintf(indent+"\titems = append(items, %s)\n", sanitizeName(q.Var)))
				buf.WriteString(indent + "}\n")
			}
		} else {
			if simple {
				buf.WriteString(fmt.Sprintf(indent+"_res = append(_res, %s)\n", sel))
			} else {
				buf.WriteString(fmt.Sprintf(indent+"items = append(items, %s)\n", sanitizeName(q.Var)))
			}
		}
		indent = indent[:len(indent)-1]
		buf.WriteString(indent + "}\n")
	} else {
		if cond != "" && condStep > len(q.Froms)+len(q.Joins)-1 {
			// condition after all loops
			if simple {
				buf.WriteString(fmt.Sprintf(indent+"if %s {\n", cond))
				buf.WriteString(fmt.Sprintf(indent+"\t_res = append(_res, %s)\n", sel))
				buf.WriteString(indent + "}\n")
			} else {
				buf.WriteString(fmt.Sprintf(indent+"if %s {\n", cond))
				buf.WriteString(fmt.Sprintf(indent+"\titems = append(items, %s)\n", sanitizeName(q.Var)))
				buf.WriteString(indent + "}\n")
			}
		} else {
			if simple {
				buf.WriteString(fmt.Sprintf(indent+"_res = append(_res, %s)\n", sel))
			} else {
				buf.WriteString(fmt.Sprintf(indent+"items = append(items, %s)\n", sanitizeName(q.Var)))
			}
		}

		if condOpened {
			indent = indent[:len(indent)-1]
			buf.WriteString(indent + "}\n")
		}
		for i := len(q.Joins) - 1; i >= 0; i-- {
			indent = indent[:len(indent)-1]
			buf.WriteString(indent + "}\n")
		}
	}
	for i := len(q.Froms) - 1; i >= 0; i-- {
		indent = indent[:len(indent)-1]
		buf.WriteString(indent + "}\n")
	}
	indent = indent[:len(indent)-1]
	buf.WriteString(indent + "}\n")

	if simple {
		buf.WriteString("\treturn _res\n")
		buf.WriteString("}()")
		return buf.String(), nil
	}

	if sortExpr != "" {
		buf.WriteString("\ttype pair struct { item " + elemGo + "; key any }\n")
		buf.WriteString("\tpairs := make([]pair, len(items))\n")
		buf.WriteString("\tfor idx, it := range items {\n")
		buf.WriteString(fmt.Sprintf("\t\t%s := it\n", sanitizeName(q.Var)))
		buf.WriteString(fmt.Sprintf("\t\tpairs[idx] = pair{item: it, key: %s}\n", sortExpr))
		buf.WriteString("\t}\n")
		buf.WriteString("\tsort.Slice(pairs, func(i, j int) bool {\n")
		buf.WriteString("\t\ta, b := pairs[i].key, pairs[j].key\n")
		buf.WriteString("\t\tswitch av := a.(type) {\n")
		buf.WriteString("\t\tcase int:\n")
		buf.WriteString("\t\t\tswitch bv := b.(type) {\n")
		buf.WriteString("\t\t\tcase int:\n")
		buf.WriteString("\t\t\t\treturn av < bv\n")
		buf.WriteString("\t\t\tcase float64:\n")
		buf.WriteString("\t\t\t\treturn float64(av) < bv\n")
		buf.WriteString("\t\t\t}\n")
		buf.WriteString("\t\tcase float64:\n")
		buf.WriteString("\t\t\tswitch bv := b.(type) {\n")
		buf.WriteString("\t\t\tcase int:\n")
		buf.WriteString("\t\t\t\treturn av < float64(bv)\n")
		buf.WriteString("\t\t\tcase float64:\n")
		buf.WriteString("\t\t\t\treturn av < bv\n")
		buf.WriteString("\t\t\t}\n")
		buf.WriteString("\t\tcase string:\n")
		buf.WriteString("\t\t\tbs, _ := b.(string)\n")
		buf.WriteString("\t\t\treturn av < bs\n")
		buf.WriteString("\t\t}\n")
		buf.WriteString("\t\treturn fmt.Sprint(a) < fmt.Sprint(b)\n")
		buf.WriteString("\t})\n")
		buf.WriteString("\tfor idx, p := range pairs {\n")
		buf.WriteString("\t\titems[idx] = p.item\n")
		buf.WriteString("\t}\n")
	}

	if skipExpr != "" || takeExpr != "" {
		c.use("_paginate")
		sk := skipExpr
		if sk == "" {
			sk = "-1"
		}
		tk := takeExpr
		if tk == "" {
			tk = "-1"
		}
		buf.WriteString(fmt.Sprintf("\titems = _paginate[%s](items, %s, %s)\n", elemGo, sk, tk))
	}

	buf.WriteString(fmt.Sprintf("\t_res := []%s{}\n", retElem))
	buf.WriteString(fmt.Sprintf("\tfor _, %s := range items {\n", sanitizeName(q.Var)))
	buf.WriteString(fmt.Sprintf("\t\t_res = append(_res, %s)\n", sel))
	buf.WriteString("\t}\n")
	buf.WriteString("\treturn _res\n")
	buf.WriteString("}()")
	return buf.String(), nil
}

func (c *Compiler) compileMatchExpr(m *parser.MatchExpr) (string, error) {
	target, err := c.compileExpr(m.Target)
	if err != nil {
		return "", err
	}
	expr := &parser.Expr{Binary: &parser.BinaryExpr{Left: &parser.Unary{Value: &parser.PostfixExpr{Target: &parser.Primary{Match: m}}}}}
	retT := c.inferExprType(expr)
	retType := goType(retT)
	if retType == "" {
		retType = "any"
	}
	var buf bytes.Buffer
	buf.WriteString("func() " + retType + " {\n")
	buf.WriteString("\t_t := " + target + "\n")
	for _, cse := range m.Cases {
		caseEnv := types.NewEnv(c.env)
		var variantSt types.StructType
		if call, ok := callPattern(cse.Pattern); ok {
			if ut, ok := c.env.FindUnionByVariant(call.Func); ok {
				variantSt = ut.Variants[call.Func]
				for idx, arg := range call.Args {
					if id, ok := identName(arg); ok && id != "_" {
						caseEnv.SetVar(id, variantSt.Fields[variantSt.Order[idx]], true)
					}
				}
			}
		}
		origEnv := c.env
		c.env = caseEnv
		res, err := c.compileExpr(cse.Result)
		resType := c.inferExprType(cse.Result)
		c.env = origEnv
		if err != nil {
			return "", err
		}
		if retType != "any" && !equalTypes(retT, resType) {
			res = c.castExpr(res, resType, retT)
		} else if retType == "[]any" {
			if lt, ok := resType.(types.ListType); ok && !isAny(lt.Elem) {
				c.use("_toAnySlice")
				res = fmt.Sprintf("_toAnySlice(%s)", res)
			}
		}

		if isUnderscoreExpr(cse.Pattern) {
			buf.WriteString("\treturn " + res + "\n")
			buf.WriteString("}()")
			return buf.String(), nil
		}

		// Union variant pattern
		if call, ok := callPattern(cse.Pattern); ok {
			if ut, ok := c.env.FindUnionByVariant(call.Func); ok {
				st := ut.Variants[call.Func]
				varName := c.newNamedVar("tmp")
				cond := fmt.Sprintf("%s, ok := _t.(%s); ok", varName, sanitizeName(call.Func))
				buf.WriteString("\tif " + cond + " {\n")
				for idx, arg := range call.Args {
					if id, ok := identName(arg); ok && id != "_" {
						field := exportName(sanitizeName(st.Order[idx]))
						buf.WriteString(fmt.Sprintf("\t\t%s := %s.%s\n", sanitizeName(id), varName, field))
					}
				}
				buf.WriteString("\t\treturn " + res + "\n")
				buf.WriteString("\t}\n")
				continue
			}
		}

		if ident, ok := identName(cse.Pattern); ok {
			if _, ok := c.env.FindUnionByVariant(ident); ok {
				cond := fmt.Sprintf("_, ok := _t.(%s); ok", sanitizeName(ident))
				buf.WriteString("\tif " + cond + " {\n")
				buf.WriteString("\t\treturn " + res + "\n")
				buf.WriteString("\t}\n")
				continue
			}
		}

		pat, err := c.compileExpr(cse.Pattern)
		if err != nil {
			return "", err
		}
		c.use("_equal")
		c.imports["reflect"] = true
		buf.WriteString(fmt.Sprintf("\tif _equal(_t, %s) {\n", pat))
		buf.WriteString("\t\treturn " + res + "\n")
		buf.WriteString("\t}\n")
	}
	if retType == "any" {
		buf.WriteString("\treturn nil\n")
	} else {
		buf.WriteString(fmt.Sprintf("\tvar _zero %s\n\treturn _zero\n", retType))
	}
	buf.WriteString("}()")
	return buf.String(), nil
}

func (c *Compiler) compileLiteral(l *parser.Literal) (string, error) {
	switch {
	case l.Int != nil:
		return fmt.Sprintf("%d", *l.Int), nil
	case l.Float != nil:
		s := strconv.FormatFloat(*l.Float, 'f', -1, 64)
		if !strings.ContainsAny(s, ".eE") {
			s += ".0"
		}
		return s, nil
	case l.Str != nil:
		return fmt.Sprintf("%q", *l.Str), nil
	case l.Bool != nil:
		if *l.Bool {
			return "true", nil
		}
		return "false", nil
	case l.Null:
		return "nil", nil
	default:
		return "nil", fmt.Errorf("invalid literal")
	}
}

func literalKey(l *parser.Literal) string {
	switch {
	case l.Int != nil:
		return fmt.Sprintf("i%v", *l.Int)
	case l.Float != nil:
		return fmt.Sprintf("f%v", *l.Float)
	case l.Str != nil:
		return fmt.Sprintf("s%q", *l.Str)
	case l.Bool != nil:
		if *l.Bool {
			return "btrue"
		}
		return "bfalse"
	default:
		return "nil"
	}
}

func extractLiteral(e *parser.Expr) *parser.Literal {
	if e == nil {
		return nil
	}
	if len(e.Binary.Right) != 0 {
		return nil
	}
	u := e.Binary.Left
	if len(u.Ops) != 0 {
		return nil
	}
	p := u.Value
	if len(p.Ops) != 0 {
		return nil
	}
	if p.Target != nil {
		return p.Target.Lit
	}
	return nil
}

func callPattern(e *parser.Expr) (*parser.CallExpr, bool) {
	if e == nil {
		return nil, false
	}
	if len(e.Binary.Right) != 0 {
		return nil, false
	}
	u := e.Binary.Left
	if len(u.Ops) != 0 {
		return nil, false
	}
	p := u.Value
	if len(p.Ops) != 0 || p.Target.Call == nil {
		return nil, false
	}
	return p.Target.Call, true
}

func identName(e *parser.Expr) (string, bool) {
	if e == nil {
		return "", false
	}
	if len(e.Binary.Right) != 0 {
		return "", false
	}
	u := e.Binary.Left
	if len(u.Ops) != 0 {
		return "", false
	}
	p := u.Value
	if len(p.Ops) != 0 {
		return "", false
	}
	if p.Target.Selector != nil && len(p.Target.Selector.Tail) == 0 {
		return p.Target.Selector.Root, true
	}
	return "", false
}

func (c *Compiler) evalConstExpr(e *parser.Expr) (*parser.Literal, bool) {
	if lit := extractLiteral(e); lit != nil {
		return lit, true
	}
	if call, ok := callPattern(e); ok {
		return interpreter.EvalPureCall(call, c.env)
	}
	return nil, false
}

func literalValue(l *parser.Literal) any {
	switch {
	case l.Int != nil:
		return *l.Int
	case l.Float != nil:
		return *l.Float
	case l.Str != nil:
		return *l.Str
	case l.Bool != nil:
		return *l.Bool
	default:
		return nil
	}
}

func isEmptyListLiteral(e *parser.Expr) bool {
	if e == nil || e.Binary == nil || len(e.Binary.Right) != 0 {
		return false
	}
	if ll := e.Binary.Left.Value.Target.List; ll != nil {
		return len(ll.Elems) == 0
	}
	return false
}

func isEmptyMapLiteral(e *parser.Expr) bool {
	if e == nil || e.Binary == nil || len(e.Binary.Right) != 0 {
		return false
	}
	if ml := e.Binary.Left.Value.Target.Map; ml != nil {
		return len(ml.Items) == 0
	}
	return false
}

func mapLiteral(e *parser.Expr) *parser.MapLiteral {
	if e == nil || e.Binary == nil || len(e.Binary.Right) != 0 {
		return nil
	}
	return e.Binary.Left.Value.Target.Map
}

func (c *Compiler) callKey(call *parser.CallExpr) string {
	parts := make([]string, len(call.Args)+1)
	parts[0] = call.Func
	for i, arg := range call.Args {
		if lit := extractLiteral(arg); lit != nil {
			parts[i+1] = literalKey(lit)
		} else if name, ok := identName(arg); ok {
			if val, err := c.env.GetValue(name); err == nil {
				if l := types.AnyToLiteral(val); l != nil {
					parts[i+1] = literalKey(l)
				}
			}
		}
	}
	return strings.Join(parts, ":")
}

func (c *Compiler) foldCall(call *parser.CallExpr) (*parser.Literal, bool) {
	key := c.callKey(call)
	if lit, ok := c.memo[key]; ok {
		return lit, true
	}
	args := make([]*parser.Expr, len(call.Args))
	for i, a := range call.Args {
		if lit := extractLiteral(a); lit != nil {
			args[i] = &parser.Expr{Binary: &parser.BinaryExpr{Left: &parser.Unary{Value: &parser.PostfixExpr{Target: &parser.Primary{Lit: lit}}}}}
			continue
		}
		if name, ok := identName(a); ok {
			if val, err := c.env.GetValue(name); err == nil {
				if l := types.AnyToLiteral(val); l != nil {
					args[i] = &parser.Expr{Binary: &parser.BinaryExpr{Left: &parser.Unary{Value: &parser.PostfixExpr{Target: &parser.Primary{Lit: l}}}}}
					continue
				}
			}
		}
		return nil, false
	}
	lit, ok := interpreter.EvalPureCall(&parser.CallExpr{Func: call.Func, Args: args}, c.env)
	if ok {
		c.memo[key] = lit
	}
	return lit, ok
}

// compileExprHint compiles an expression using a type hint when dealing with
// literals that would otherwise default to `any`, such as empty list literals.
// The hint is currently only used for list literals and is applied recursively
// for nested lists.
func (c *Compiler) compileExprHint(e *parser.Expr, hint types.Type) (string, error) {
	if e != nil && e.Binary != nil && len(e.Binary.Right) == 0 {
		if q := e.Binary.Left.Value.Target.Query; q != nil {
			return c.compileQueryExpr(q, hint)
		}
	}
	if lt, ok := hint.(types.ListType); ok {
		if e.Binary != nil && len(e.Binary.Right) == 0 {
			if ll := e.Binary.Left.Value.Target.List; ll != nil {
				elems := make([]string, len(ll.Elems))
				for i, el := range ll.Elems {
					ev, err := c.compileExprHint(el, lt.Elem)
					if err != nil {
						return "", err
					}
					elems[i] = ev
				}
				return "[]" + goType(lt.Elem) + "{" + joinItems(elems, c.indent, 3) + "}", nil
			}
		}
	}
	if mt, ok := hint.(types.MapType); ok {
		if e.Binary != nil && len(e.Binary.Right) == 0 {
			if ml := e.Binary.Left.Value.Target.Map; ml != nil {
				keyType := goType(mt.Key)
				valType := goType(mt.Value)
				parts := make([]string, len(ml.Items))
				for i, item := range ml.Items {
					var k string
					if s, ok := simpleStringKey(item.Key); ok {
						k = fmt.Sprintf("\"%s\"", s)
					} else {
						kk, err := c.compileExprHint(item.Key, mt.Key)
						if err != nil {
							return "", err
						}
						k = kk
					}
					v, err := c.compileExprHint(item.Value, mt.Value)
					if err != nil {
						return "", err
					}
					if valType == "int64" && goType(c.inferExprType(item.Value)) == "int" {
						v = fmt.Sprintf("int64(%s)", v)
					}
					parts[i] = fmt.Sprintf("%s: %s", k, v)
				}
				return fmt.Sprintf("map[%s]%s{%s}", keyType, valType, joinItems(parts, c.indent, 2)), nil
			}
		}
	}
	if st, ok := hint.(types.StructType); ok {
		if e.Binary != nil && len(e.Binary.Right) == 0 {
			if ml := e.Binary.Left.Value.Target.Map; ml != nil {
				parts := make([]string, len(ml.Items))
				for i, item := range ml.Items {
					key, ok2 := simpleStringKey(item.Key)
					if !ok2 {
						return "", fmt.Errorf("struct field must be identifier")
					}
					ft := st.Fields[key]
					v, err := c.compileExprHint(item.Value, ft)
					if err != nil {
						return "", err
					}
					parts[i] = fmt.Sprintf("%s: %s", exportName(sanitizeName(key)), v)
				}
				c.compileStructType(st)
				return fmt.Sprintf("%s{%s}", sanitizeName(st.Name), joinItems(parts, c.indent, 1)), nil
			}
		}
	}
	expr, err := c.compileExpr(e)
	if err != nil {
		return "", err
	}
	exprT := c.inferExprType(e)
	hintGo := goType(hint)
	exprGo := goType(exprT)
	if hintGo != "" && hintGo != exprGo && (isAny(exprT) || !equalTypes(hint, exprT)) {
		expr = c.castExpr(expr, exprT, hint)
	}
	return expr, nil
}

func (c *Compiler) compilePostfixHint(p *parser.PostfixExpr, hint types.Type) (string, error) {
	e := &parser.Expr{Binary: &parser.BinaryExpr{Left: &parser.Unary{Value: p}}}
	return c.compileExprHint(e, hint)
}

func (c *Compiler) compileCallExpr(call *parser.CallExpr) (string, error) {
	if lit, ok := c.foldCall(call); ok {
		return c.compileLiteral(lit)
	}
	args := make([]string, len(call.Args))
	var paramTypes []types.Type
	if call.Func != "contains" {
		if t, err := c.env.GetVar(call.Func); err == nil {
			if ft, ok := t.(types.FuncType); ok {
				paramTypes = ft.Params
			}
		}
		if call.Func == "values" {
			paramTypes = nil
		}
	}
	for i, a := range call.Args {
		if len(paramTypes) > i {
			if ll := a.Binary.Left.Value.Target.List; ll != nil && len(ll.Elems) == 0 {
				if lt, ok := paramTypes[i].(types.ListType); ok {
					args[i] = fmt.Sprintf("[]%s{}", goType(lt.Elem))
					continue
				}
			}
			v, err := c.compileExprHint(a, paramTypes[i])
			if err != nil {
				return "", err
			}
			at := c.inferExprTypeHint(a, paramTypes[i])
			if lt, ok := paramTypes[i].(types.ListType); ok {
				if et, ok := at.(types.ListType); ok {
					if isListOfAny(et) && !isListOfAny(lt) {
						c.use("_convSlice")
						v = fmt.Sprintf("_convSlice[%s,%s](%s)", goType(et.Elem), goType(lt.Elem), v)
					} else if !isListOfAny(et) && equalTypes(lt.Elem, et.Elem) && goType(lt.Elem) != goType(et.Elem) {
						c.use("_convSlice")
						v = fmt.Sprintf("_convSlice[%s,%s](%s)", goType(et.Elem), goType(lt.Elem), v)
					}
				}
			}
			if _, ok := paramTypes[i].(types.StructType); ok {
				v = "&" + v
			}
			args[i] = v
			continue
		}
		v, err := c.compileExpr(a)
		if err != nil {
			return "", err
		}
		args[i] = v
	}
	argStr := strings.Join(args, ", ")

	if len(paramTypes) > 0 && len(args) < len(paramTypes) {
		// partial application
		missing := paramTypes[len(args):]
		names := make([]string, len(missing))
		for i := range missing {
			names[i] = fmt.Sprintf("p%d", i)
			args = append(args, names[i])
		}
		ret := ""
		if t, err := c.env.GetVar(call.Func); err == nil {
			if ft, ok := t.(types.FuncType); ok {
				ret = goType(ft.Return)
			}
		}
		params := make([]string, len(missing))
		for i, pt := range missing {
			params[i] = fmt.Sprintf("%s %s", names[i], goType(pt))
		}
		body := fmt.Sprintf("%s(%s)", sanitizeName(call.Func), strings.Join(args, ", "))
		if ret != "" {
			return fmt.Sprintf("func(%s) %s { return %s }", strings.Join(params, ", "), ret, body), nil
		}
		return fmt.Sprintf("func(%s) { %s }", strings.Join(params, ", "), body), nil
	}

	switch call.Func {
	case "print":
		c.imports["fmt"] = true
		if len(call.Args) == 1 {
			if _, ok := c.inferExprType(call.Args[0]).(types.ListType); ok {
				c.imports["strings"] = true
				return fmt.Sprintf("fmt.Println(strings.TrimSuffix(strings.TrimPrefix(fmt.Sprint(%s), \"[\"), \"]\"))", args[0]), nil
			}
		}
		return fmt.Sprintf("fmt.Println(%s)", strings.Join(args, ", ")), nil
	case "str":
		c.imports["fmt"] = true
		return fmt.Sprintf("fmt.Sprint(%s)", argStr), nil
	case "input":
		c.imports["fmt"] = true
		c.use("_input")
		return "_input()", nil
	case "contains":
		if len(args) != 2 {
			return "", fmt.Errorf("contains expects 2 args")
		}
		c.use("_contains")
		return fmt.Sprintf("_contains(%s, %s)", args[0], args[1]), nil
	case "values":
		if len(args) != 1 {
			return "", fmt.Errorf("values expects 1 arg")
		}
		c.use("_values")
		return fmt.Sprintf("_values(%s)", args[0]), nil
	case "count":
		if len(call.Args) == 1 {
			at := c.inferExprType(call.Args[0])
			switch at.(type) {
			case types.ListType, types.MapType:
				return fmt.Sprintf("len(%s)", args[0]), nil
			case types.StringType:
				return fmt.Sprintf("len([]rune(%s))", args[0]), nil
			case types.GroupType:
				return fmt.Sprintf("len(%s.Items)", args[0]), nil
			}
		}
		c.imports["mochi/runtime/data"] = true
		c.imports["reflect"] = true
		c.use("_count")
		if len(call.Args) == 1 {
			at := c.inferExprType(call.Args[0])
			if lt, ok := at.(types.ListType); ok && !isAny(lt.Elem) {
				c.use("_toAnySlice")
				argStr = fmt.Sprintf("_toAnySlice(%s)", args[0])
			}
		}
		return fmt.Sprintf("_count(%s)", argStr), nil
	case "exists":
		if len(call.Args) == 1 {
			at := c.inferExprType(call.Args[0])
			switch at.(type) {
			case types.ListType, types.MapType:
				return fmt.Sprintf("len(%s) > 0", args[0]), nil
			case types.StringType:
				return fmt.Sprintf("len([]rune(%s)) > 0", args[0]), nil
			case types.GroupType:
				return fmt.Sprintf("len(%s.Items) > 0", args[0]), nil
			}
		}
		// Fallback to the runtime helper for imprecise types.
		c.use("_exists")
		return fmt.Sprintf("_exists(%s)", argStr), nil
	case "substring":
		if len(call.Args) != 3 {
			return "", fmt.Errorf("substring expects 3 args")
		}
		c.use("_sliceString")
		return fmt.Sprintf("_sliceString(%s)", argStr), nil
	case "avg":
		if len(call.Args) == 1 {
			at := c.inferExprType(call.Args[0])
			if lt, ok := at.(types.ListType); ok {
				if isInt(lt.Elem) || isInt64(lt.Elem) || isFloat(lt.Elem) {
					c.use("_avgOrdered")
					elemGo := goType(lt.Elem)
					return fmt.Sprintf("_avgOrdered[%s](%s)", elemGo, args[0]), nil
				}
			}
		}
		c.imports["mochi/runtime/data"] = true
		c.use("_avg")
		return fmt.Sprintf("_avg(%s)", argStr), nil
	case "sum":
		if len(call.Args) == 1 {
			at := c.inferExprType(call.Args[0])
			if lt, ok := at.(types.ListType); ok {
				if isInt(lt.Elem) || isInt64(lt.Elem) || isFloat(lt.Elem) {
					c.use("_sumOrdered")
					elemGo := goType(lt.Elem)
					return fmt.Sprintf("_sumOrdered[%s](%s)", elemGo, args[0]), nil
				}
			}
		}
		c.imports["mochi/runtime/data"] = true
		c.use("_sum")
		return fmt.Sprintf("_sum(%s)", argStr), nil
	case "min":
		if len(call.Args) == 1 {
			at := c.inferExprType(call.Args[0])
			if lt, ok := at.(types.ListType); ok {
				if isInt(lt.Elem) || isInt64(lt.Elem) || isFloat(lt.Elem) || isString(lt.Elem) {
					c.use("_minOrdered")
					elemGo := goType(lt.Elem)
					return fmt.Sprintf("_minOrdered[%s](%s)", elemGo, args[0]), nil
				}
			}
		}
		c.imports["mochi/runtime/data"] = true
		c.use("_min")
		return fmt.Sprintf("_min(%s)", argStr), nil
	case "max":
		if len(call.Args) == 1 {
			at := c.inferExprType(call.Args[0])
			if lt, ok := at.(types.ListType); ok {
				if isInt(lt.Elem) || isInt64(lt.Elem) || isFloat(lt.Elem) || isString(lt.Elem) {
					c.use("_maxOrdered")
					elemGo := goType(lt.Elem)
					return fmt.Sprintf("_maxOrdered[%s](%s)", elemGo, args[0]), nil
				}
			}
		}
		c.imports["mochi/runtime/data"] = true
		c.use("_max")
		return fmt.Sprintf("_max(%s)", argStr), nil
	case "reduce":
		if len(call.Args) != 3 {
			return "", fmt.Errorf("reduce expects 3 args")
		}
		arg0 := args[0]
		elemGo := "any"
		if lt, ok := c.inferExprType(call.Args[0]).(types.ListType); ok {
			elemGo = goType(lt.Elem)
			if isAny(lt.Elem) {
				c.use("_toAnySlice")
				arg0 = fmt.Sprintf("_toAnySlice(%s)", arg0)
				elemGo = "any"
			}
		} else {
			c.use("_toAnySlice")
			arg0 = fmt.Sprintf("_toAnySlice(%s)", arg0)
		}
		c.use("_reduce")
		return fmt.Sprintf("_reduce[%s](%s, %s, %s)", elemGo, arg0, args[1], args[2]), nil
	case "first":
		if len(call.Args) == 1 {
			at := c.inferExprType(call.Args[0])
			if lt, ok := at.(types.ListType); ok {
				if !isAny(lt.Elem) {
					c.use("_firstSlice")
					elemGo := goType(lt.Elem)
					return fmt.Sprintf("_firstSlice[%s](%s)", elemGo, args[0]), nil
				}
				c.use("_toAnySlice")
				argStr = fmt.Sprintf("_toAnySlice(%s)", args[0])
			}
		}
		c.imports["mochi/runtime/data"] = true
		c.imports["reflect"] = true
		c.use("_first")
		return fmt.Sprintf("_first(%s)", argStr), nil
	case "substr":
		if len(call.Args) != 3 {
			return "", fmt.Errorf("substr expects 3 args")
		}
		c.use("_sliceString")
		return fmt.Sprintf("_sliceString(%s)", argStr), nil
	case "lower":
		if len(call.Args) != 1 {
			return "", fmt.Errorf("lower expects 1 arg")
		}
		at := c.inferExprType(call.Args[0])
		if isString(at) {
			c.imports["strings"] = true
			return fmt.Sprintf("strings.ToLower(%s)", args[0]), nil
		}
		c.use("_lower")
		return fmt.Sprintf("_lower(%s)", args[0]), nil
	case "upper":
		if len(call.Args) != 1 {
			return "", fmt.Errorf("upper expects 1 arg")
		}
		at := c.inferExprType(call.Args[0])
		if isString(at) {
			c.imports["strings"] = true
			return fmt.Sprintf("strings.ToUpper(%s)", args[0]), nil
		}
		c.use("_upper")
		return fmt.Sprintf("_upper(%s)", args[0]), nil
	case "reverse":
		if len(call.Args) != 1 {
			return "", fmt.Errorf("reverse expects 1 arg")
		}
		at := c.inferExprType(call.Args[0])
		if isString(at) {
			c.use("_reverseString")
			return fmt.Sprintf("_reverseString(%s)", args[0]), nil
		}
		if lt, ok := at.(types.ListType); ok {
			c.use("_reverseSlice")
			return fmt.Sprintf("_reverseSlice[%s](%s)", goType(lt.Elem), args[0]), nil
		}
		return "", fmt.Errorf("reverse expects string or list")
	case "concat":
		if len(call.Args) < 2 {
			return "", fmt.Errorf("concat expects at least 2 args")
		}
		at := c.inferExprType(call.Args[0])
		lt, ok := at.(types.ListType)
		if !ok {
			return "", fmt.Errorf("concat expects lists")
		}
		elemType := lt.Elem
		same := !isAny(elemType)
		for _, a := range call.Args[1:] {
			t := c.inferExprType(a)
			lt2, ok2 := t.(types.ListType)
			if !ok2 || !equalTypes(elemType, lt2.Elem) || isAny(lt2.Elem) || goType(lt2.Elem) != goType(elemType) {
				same = false
				break
			}
		}
		if same {
			expr := args[0]
			for i := 1; i < len(args); i++ {
				expr = fmt.Sprintf("append(%s, %s...)", expr, args[i])
			}
			return expr, nil
		}

		elemGo := "any"
		for i := range args {
			c.use("_toAnySlice")
			args[i] = fmt.Sprintf("_toAnySlice(%s)", args[i])
		}
		c.use("_concat")
		expr := args[0]
		for i := 1; i < len(args); i++ {
			expr = fmt.Sprintf("_concat[%s](%s, %s)", elemGo, expr, args[i])
		}
		return expr, nil
	case "len":
		return fmt.Sprintf("len(%s)", argStr), nil
	case "now":
		c.imports["time"] = true
		// time.Now().UnixNano() already returns an int64. Use it directly
		// so `now()` provides nanosecond precision consistent with the
		// interpreter.
		return "time.Now().UnixNano()", nil
	case "json":
		c.imports["encoding/json"] = true
		c.imports["fmt"] = true
		return fmt.Sprintf("func(){b,_:=json.Marshal(%s);fmt.Println(string(b))}()", argStr), nil
	default:
		return fmt.Sprintf("%s(%s)", sanitizeName(call.Func), argStr), nil
	}
}

func (c *Compiler) compileFunExpr(fn *parser.FunExpr) (string, error) {
	params := make([]string, len(fn.Params))
	for i, p := range fn.Params {
		typ := "any"
		if p.Type != nil {
			typ = goType(c.resolveTypeRef(p.Type))
		}
		params[i] = sanitizeName(p.Name) + " " + typ
	}
	child := types.NewEnv(c.env)
	for _, p := range fn.Params {
		if p.Type != nil {
			child.SetVar(p.Name, c.resolveTypeRef(p.Type), true)
		}
	}
	sub := &Compiler{imports: c.imports, helpers: c.helpers, env: child, memo: map[string]*parser.Literal{}}
	sub.indent = 1
	if fn.Return != nil {
		sub.returnType = c.resolveTypeRef(fn.Return)
	}
	if fn.ExprBody != nil {
		expr, err := sub.compileExpr(fn.ExprBody)
		if err != nil {
			return "", err
		}
		sub.writeln("return " + expr)
	} else {
		if err := sub.compileStmtList(fn.BlockBody); err != nil {
			return "", err
		}
	}
	body := indentBlock(sub.buf.String(), 1)
	retType := ""
	if fn.Return != nil {
		retType = goType(c.resolveTypeRef(fn.Return))
	}
	if retType == "" {
		return "func(" + strings.Join(params, ", ") + ") {\n" + body + "}", nil
	}
	return "func(" + strings.Join(params, ", ") + ") " + retType + " {\n" + body + "}", nil
}

func hasExpect(p *parser.Program) bool {
	for _, s := range p.Statements {
		if containsExpect(s) {
			return true
		}
	}
	return false
}

func hasTest(p *parser.Program) bool {
	for _, s := range p.Statements {
		if s.Test != nil {
			return true
		}
	}
	return false
}

func containsExpect(s *parser.Statement) bool {
	switch {
	case s.Expect != nil:
		return true
	case s.If != nil:
		for _, t := range s.If.Then {
			if containsExpect(t) {
				return true
			}
		}
		if s.If.ElseIf != nil {
			if containsExpect(&parser.Statement{If: s.If.ElseIf}) {
				return true
			}
		}
		for _, t := range s.If.Else {
			if containsExpect(t) {
				return true
			}
		}
	case s.For != nil:
		for _, t := range s.For.Body {
			if containsExpect(t) {
				return true
			}
		}
	case s.While != nil:
		for _, t := range s.While.Body {
			if containsExpect(t) {
				return true
			}
		}
	case s.Test != nil:
		for _, t := range s.Test.Body {
			if containsExpect(t) {
				return true
			}
		}
	case s.Fun != nil:
		for _, t := range s.Fun.Body {
			if containsExpect(t) {
				return true
			}
		}
	}
	return false
}

func paramModified(name string, stmts []*parser.Statement) bool {
	for _, s := range stmts {
		switch {
		case s.Assign != nil:
			if s.Assign.Name == name && (len(s.Assign.Field) > 0 || len(s.Assign.Index) > 0) {
				return true
			}
		case s.Update != nil:
			if s.Update.Target == name {
				return true
			}
		case s.If != nil:
			if paramModified(name, s.If.Then) || paramModifiedIfStmt(name, s.If.ElseIf) || paramModified(name, s.If.Else) {
				return true
			}
		case s.For != nil:
			if paramModified(name, s.For.Body) {
				return true
			}
		case s.While != nil:
			if paramModified(name, s.While.Body) {
				return true
			}
		case s.On != nil:
			if paramModified(name, s.On.Body) {
				return true
			}
		}
	}
	return false
}

func paramModifiedIfStmt(name string, is *parser.IfStmt) bool {
	if is == nil {
		return false
	}
	if paramModified(name, is.Then) || paramModifiedIfStmt(name, is.ElseIf) || paramModified(name, is.Else) {
		return true
	}
	return false
}

func (c *Compiler) scanImports(s *parser.Statement) {
	if s.Expr != nil {
		c.scanExprImports(s.Expr.Expr)
	}
	if s.Expect != nil {
		c.scanExprImports(s.Expect.Value)
	}
	if s.Let != nil {
		if s.Let.Value != nil {
			c.scanExprImports(s.Let.Value)
		}
	}
	if s.Var != nil {
		if s.Var.Value != nil {
			c.scanExprImports(s.Var.Value)
		}
	}
	if s.Assign != nil {
		c.scanExprImports(s.Assign.Value)
	}
	if s.If != nil {
		c.scanExprImports(s.If.Cond)
		for _, t := range s.If.Then {
			c.scanImports(t)
		}
		if s.If.ElseIf != nil {
			c.scanImports(&parser.Statement{If: s.If.ElseIf})
		}
		for _, t := range s.If.Else {
			c.scanImports(t)
		}
	}
	if s.For != nil {
		c.scanExprImports(s.For.Source)
		if s.For.RangeEnd != nil {
			c.scanExprImports(s.For.RangeEnd)
		}
		for _, t := range s.For.Body {
			c.scanImports(t)
		}
	}
	if s.While != nil {
		c.scanExprImports(s.While.Cond)
		for _, t := range s.While.Body {
			c.scanImports(t)
		}
	}
	if s.Stream != nil {
		c.imports["mochi/runtime/stream"] = true
	}
	if s.On != nil {
		c.imports["context"] = true
		c.imports["mochi/runtime/stream"] = true
		c.usesHandlers = true
		for _, t := range s.On.Body {
			c.scanImports(t)
		}
	}
	if s.Agent != nil {
		c.imports["context"] = true
		c.imports["mochi/runtime/agent"] = true
		for _, blk := range s.Agent.Body {
			switch {
			case blk.Let != nil && blk.Let.Value != nil:
				c.scanExprImports(blk.Let.Value)
			case blk.Var != nil && blk.Var.Value != nil:
				c.scanExprImports(blk.Var.Value)
			case blk.On != nil:
				c.imports["mochi/runtime/stream"] = true
				for _, t := range blk.On.Body {
					c.scanImports(t)
				}
			case blk.Intent != nil:
				for _, t := range blk.Intent.Body {
					c.scanImports(t)
				}
			}
		}
	}
	if s.Emit != nil {
		c.imports["context"] = true
		c.imports["mochi/runtime/stream"] = true
		for _, f := range s.Emit.Fields {
			c.scanExprImports(f.Value)
		}
	}
	if s.Fun != nil {
		for _, t := range s.Fun.Body {
			c.scanImports(t)
		}
	}
	if s.Test != nil {
		for _, t := range s.Test.Body {
			c.scanImports(t)
		}
	}
	if s.Import != nil {
		c.addImport(s.Import)
	}
}

func (c *Compiler) scanExprImports(e *parser.Expr) {
	if e == nil {
		return
	}
	c.scanBinaryImports(e.Binary)
}

func (c *Compiler) scanBinaryImports(b *parser.BinaryExpr) {
	if b == nil {
		return
	}
	leftType := c.inferUnaryType(b.Left)
	c.scanUnaryImports(b.Left)
	for _, op := range b.Right {
		c.scanPostfixImports(op.Right)
		rightType := c.inferPostfixType(op.Right)
		if (op.Op == "==" || op.Op == "!=") && (isList(leftType) || isList(rightType) || isMap(leftType) || isMap(rightType) || isStruct(leftType) || isStruct(rightType)) {
			c.imports["reflect"] = true
			c.use("_equal")
		}
		leftType = resultType(op.Op, leftType, rightType)
	}
}

func (c *Compiler) scanUnaryImports(u *parser.Unary) {
	if u == nil {
		return
	}
	c.scanPostfixImports(u.Value)
}

func (c *Compiler) scanPostfixImports(p *parser.PostfixExpr) {
	if p == nil {
		return
	}
	c.scanPrimaryImports(p.Target)
	for _, op := range p.Ops {
		switch {
		case op.Call != nil:
			for _, a := range op.Call.Args {
				c.scanExprImports(a)
			}
		case op.Index != nil:
			idx := op.Index
			c.scanExprImports(idx.Start)
			c.scanExprImports(idx.End)
		case op.Cast != nil:
			// casting may require helper imports handled in castExpr
		}
	}
}

func (c *Compiler) scanPrimaryImports(p *parser.Primary) {
	if p == nil {
		return
	}
	switch {
	case p.Call != nil:
		if p.Call.Func == "print" {
			c.imports["fmt"] = true
		}
		if p.Call.Func == "str" {
			c.imports["fmt"] = true
		}
		if p.Call.Func == "count" || p.Call.Func == "avg" || p.Call.Func == "sum" {
			c.imports["mochi/runtime/data"] = true
		}
		if p.Call.Func == "now" {
			c.imports["time"] = true
		}
		if p.Call.Func == "json" {
			c.imports["encoding/json"] = true
			c.imports["fmt"] = true
		}
		for _, a := range p.Call.Args {
			c.scanExprImports(a)
		}
	case p.Group != nil:
		c.scanExprImports(p.Group)
	case p.FunExpr != nil:
		if p.FunExpr.ExprBody != nil {
			c.scanExprImports(p.FunExpr.ExprBody)
		}
		for _, s := range p.FunExpr.BlockBody {
			c.scanImports(s)
		}
	case p.List != nil:
		for _, e := range p.List.Elems {
			c.scanExprImports(e)
		}
	case p.Map != nil:
		for _, it := range p.Map.Items {
			c.scanExprImports(it.Key)
			c.scanExprImports(it.Value)
		}
	case p.Query != nil:
		c.scanExprImports(p.Query.Source)
		if p.Query.Where != nil {
			c.scanExprImports(p.Query.Where)
		}
		if p.Query.Sort != nil {
			c.scanExprImports(p.Query.Sort)
			c.imports["sort"] = true
			c.imports["fmt"] = true
		}
		if p.Query.Skip != nil {
			c.scanExprImports(p.Query.Skip)
		}
		if p.Query.Take != nil {
			c.scanExprImports(p.Query.Take)
		}
		c.scanExprImports(p.Query.Select)
	case p.Match != nil:
		c.scanExprImports(p.Match.Target)
		for _, cs := range p.Match.Cases {
			c.scanExprImports(cs.Pattern)
			c.scanExprImports(cs.Result)
		}
	case p.Generate != nil:
		c.imports["fmt"] = true
		c.imports["context"] = true
		c.imports["mochi/runtime/llm"] = true
		c.imports["_ \"mochi/runtime/llm/provider/echo\""] = true
		if _, ok := c.env.GetStruct(p.Generate.Target); ok {
			c.imports["encoding/json"] = true
		}
		for _, f := range p.Generate.Fields {
			c.scanExprImports(f.Value)
		}
	case p.Load != nil:
		if p.Load.With != nil {
			c.scanExprImports(p.Load.With)
		}
		if p.Load.Type != nil {
			if st, ok := c.env.GetStruct(*p.Load.Type.Simple); ok {
				c.imports["encoding/json"] = true
				_ = st
			}
		}
		c.imports["mochi/runtime/data"] = true
		c.imports["os"] = true
		c.use("_load")
	case p.Save != nil:
		c.scanExprImports(p.Save.Src)
		if p.Save.With != nil {
			c.scanExprImports(p.Save.With)
		}
		c.imports["mochi/runtime/data"] = true
		c.imports["os"] = true
		c.use("_save")
	case p.Selector != nil:
		// no imports
	case p.Lit != nil:
		// none
	}
}

func (c *Compiler) addImport(im *parser.ImportStmt) error {
	mod := strings.Trim(im.Path, "\"")
	alias := im.As
	if alias == "" {
		alias = parser.AliasFromPath(im.Path)
	}
	if im.Lang == nil {
		return fmt.Errorf("unsupported import language: <nil>")
	}
	switch *im.Lang {
	case "python":
		c.pyModules[alias] = mod
		if im.Auto {
			c.pyAuto[alias] = true
		}
		c.imports["mochi/runtime/ffi/python"] = true
	case "go":
		c.goModules[alias] = mod
		if im.Auto {
			c.goAuto[alias] = true
		}
		c.imports["mochi/runtime/ffi/go"] = true
	case "typescript":
		c.tsModules[alias] = mod
		if im.Auto {
			c.tsAuto[alias] = true
		}
		c.imports["mochi/runtime/ffi/deno"] = true
	default:
		return fmt.Errorf("unsupported import language: %s", *im.Lang)
	}
	return nil
}
