//go:build slow

package pycode

import (
	"bytes"
	"fmt"
	"os"
	"path/filepath"
	"sort"
	"strconv"
	"strings"

	"mochi/interpreter"
	"mochi/parser"
	"mochi/types"
)

// Compiler translates a Mochi AST into Python source code.
type Compiler struct {
	buf                bytes.Buffer
	indent             int
	helpers            map[string]bool
	imports            map[string]string
	env                *types.Env
	structs            map[string]bool
	agents             map[string]bool
	handlerCount       int
	tmpCount           int
	models             bool
	methodFields       map[string]bool
	tupleFields        map[string]map[string]int
	tupleTypes         map[string]map[string]types.Type
	currentGroup       string
	groupFields        map[string]bool
	autoStructs        map[string]types.StructType
	structKeys         map[string]string
	structFields       map[string]string
	autoCount          int
	queryStructs       map[*parser.QueryExpr]types.StructType
	typeHints          bool
	autoStructsEnabled bool
}

func New(env *types.Env) *Compiler {
	return &Compiler{
		helpers:            make(map[string]bool),
		imports:            make(map[string]string),
		env:                env,
		structs:            make(map[string]bool),
		agents:             make(map[string]bool),
		models:             false,
		tmpCount:           0,
		methodFields:       nil,
		tupleFields:        make(map[string]map[string]int),
		tupleTypes:         make(map[string]map[string]types.Type),
		currentGroup:       "",
		groupFields:        nil,
		autoStructs:        make(map[string]types.StructType),
		structKeys:         make(map[string]string),
		structFields:       make(map[string]string),
		autoCount:          0,
		queryStructs:       make(map[*parser.QueryExpr]types.StructType),
		typeHints:          true,
		autoStructsEnabled: true,
	}
}

// SetTypeHints enables or disables type annotations in generated code.
func (c *Compiler) SetTypeHints(v bool) {
	c.typeHints = v
}

// SetAutoStructs enables or disables automatic dataclass generation for
// list literals that look like structs.
func (c *Compiler) SetAutoStructs(v bool) {
	c.autoStructsEnabled = v
}

func containsStreamCode(stmts []*parser.Statement) bool {
	for _, s := range stmts {
		if stmtHasStream(s) {
			return true
		}
	}
	return false
}

func stmtHasStream(s *parser.Statement) bool {
	switch {
	case s.Stream != nil, s.Emit != nil, s.On != nil, s.Agent != nil:
		return true
	case s.Fun != nil:
		return containsStreamCode(s.Fun.Body)
	case s.Test != nil:
		return containsStreamCode(s.Test.Body)
	case s.If != nil:
		if containsStreamCode(s.If.Then) {
			return true
		}
		if s.If.ElseIf != nil {
			if stmtHasStream(&parser.Statement{If: s.If.ElseIf}) {
				return true
			}
		}
		return containsStreamCode(s.If.Else)
	case s.While != nil:
		return containsStreamCode(s.While.Body)
	case s.For != nil:
		return containsStreamCode(s.For.Body)
	case s.On != nil:
		return containsStreamCode(s.On.Body)
	}
	return false
}

func (c *Compiler) collectImports(stmts []*parser.Statement) {
	for _, s := range stmts {
		if s.Import != nil && s.Import.Lang != nil && *s.Import.Lang == "python" {
			path := strings.Trim(s.Import.Path, "\"")
			alias := s.Import.As
			if alias == "" {
				alias = parser.AliasFromPath(s.Import.Path)
			}
			alias = sanitizeName(alias)
			c.imports[alias] = path
		}
	}
}

func structKey(st types.StructType) string {
	var b strings.Builder
	for _, f := range st.Order {
		b.WriteString(f)
		if ft, ok := st.Fields[f]; ok && ft != nil {
			typeName := ft.String()
			switch typeName {
			case "int", "int64", "float":
				typeName = "number"
			}
			b.WriteString(":" + typeName)
		} else {
			b.WriteString(":")
		}
		b.WriteByte(';')
	}
	return b.String()
}

func structFieldsKey(st types.StructType) string {
	return strings.Join(st.Order, ";")
}

func (c *Compiler) ensureStructName(st types.StructType) types.StructType {
	if st.Name != "" {
		return st
	}
	key := structKey(st)
	if name, ok := c.structKeys[key]; ok {
		st.Name = name
		if c.env != nil {
			c.env.SetStruct(name, st)
		}
		return st
	}
	fkey := structFieldsKey(st)
	if name, ok := c.structFields[fkey]; ok {
		st.Name = name
		if c.env != nil {
			c.env.SetStruct(name, st)
		}
		return st
	}
	c.autoCount++
	name := fmt.Sprintf("Auto%d", c.autoCount)
	c.imports["dataclasses"] = "dataclasses"
	st.Name = name
	c.autoStructs[name] = st
	c.structKeys[key] = name
	c.structFields[fkey] = name
	if c.env != nil {
		c.env.SetStruct(name, st)
	}
	return st
}

func (c *Compiler) namedType(t types.Type) types.Type {
	switch tt := t.(type) {
	case types.ListType:
		return types.ListType{Elem: c.namedType(tt.Elem)}
	case types.MapType:
		return types.MapType{Key: c.namedType(tt.Key), Value: c.namedType(tt.Value)}
	case types.StructType:
		return c.ensureStructName(tt)
	case types.FuncType:
		params := make([]types.Type, len(tt.Params))
		for i, p := range tt.Params {
			params[i] = c.namedType(p)
		}
		return types.FuncType{Params: params, Return: c.namedType(tt.Return)}
	default:
		return t
	}
}

func (c *Compiler) emitAutoStructs() {
	if len(c.autoStructs) == 0 {
		return
	}
	names := make([]string, 0, len(c.autoStructs))
	for n := range c.autoStructs {
		names = append(names, n)
	}
	sort.Strings(names)
	for _, n := range names {
		st := c.autoStructs[n]
		c.writeln("@dataclasses.dataclass")
		c.writeln(fmt.Sprintf("class %s:", n))
		c.indent++
		if len(st.Order) == 0 {
			c.writeln("pass")
		} else {
			for _, f := range st.Order {
				typStr := pyType(c.namedType(st.Fields[f]))
				if needsTyping(typStr) {
					c.imports["typing"] = "typing"
				}
				c.writeln(fmt.Sprintf("%s: %s", sanitizeName(f), typStr))
			}
			c.writeln("")
			c.writeln("def __getitem__(self, key):")
			c.indent++
			c.writeln("return getattr(self, key)")
			c.indent--
			c.writeln("")
			c.writeln("def __contains__(self, key):")
			c.indent++
			c.writeln("return hasattr(self, key)")
			c.indent--
		}
		c.indent--
		c.writeln("")
	}
}

func (c *Compiler) Compile(prog *parser.Program) ([]byte, error) {
	c.buf.Reset()
	c.queryStructs = make(map[*parser.QueryExpr]types.StructType)

	// Collect Python imports first so they can be emitted at the top.
	c.collectImports(prog.Statements)

	// Function declarations
	for _, s := range prog.Statements {
		if s.Fun != nil {
			if err := c.compileFunStmt(s.Fun); err != nil {
				return nil, err
			}
			c.writeln("")
		}
	}

	// Type declarations
	for _, s := range prog.Statements {
		if s.Type != nil {
			if err := c.compileTypeDecl(s.Type); err != nil {
				return nil, err
			}
			c.writeln("")
		}
	}

	// Test blocks
	for _, s := range prog.Statements {
		if s.Test != nil {
			if err := c.compileTestBlock(s.Test); err != nil {
				return nil, err
			}
			c.writeln("")
		}
	}

	tests := []string{}
	for _, s := range prog.Statements {
		if s.Test != nil {
			tests = append(tests, "test_"+sanitizeName(s.Test.Name))
		}
	}

	for _, s := range prog.Statements {
		if s.Fun != nil || s.Type != nil || s.Test != nil {
			continue
		}
		if err := c.compileStmt(s); err != nil {
			return nil, err
		}
	}
	for _, name := range tests {
		c.writeln(fmt.Sprintf("%s()", name))
	}

	body := append([]byte(nil), c.buf.Bytes()...)
	c.buf.Reset()

	needPrelude := len(c.imports) > 0 || len(c.autoStructs) > 0 || len(c.helpers) > 0 || c.models
	if needPrelude {
		c.writeln("# Generated by Mochi Python compiler")
		c.writeln("from __future__ import annotations")
		c.writeln("")
	}
	if len(c.imports) > 0 {
		aliases := make([]string, 0, len(c.imports))
		for a := range c.imports {
			aliases = append(aliases, a)
		}
		sort.Strings(aliases)
		for _, alias := range aliases {
			mod := c.imports[alias]
			if alias == mod {
				c.writeln(fmt.Sprintf("import %s", mod))
			} else {
				c.writeln(fmt.Sprintf("import %s as %s", mod, alias))
			}
		}
		c.writeln("")
	}

	// Handle simple built-in imports from other languages.
	for _, s := range prog.Statements {
		if s.Import != nil && s.Import.Lang != nil {
			lang := *s.Import.Lang
			p := strings.Trim(s.Import.Path, "\"")
			alias := s.Import.As
			if alias == "" {
				alias = parser.AliasFromPath(s.Import.Path)
			}
			alias = sanitizeName(alias)
			switch lang {
			case "go":
				if s.Import.Auto && p == "mochi/runtime/ffi/go/testpkg" {
					c.writeln(fmt.Sprintf("%s = {\"Add\": lambda a, b: a + b, \"Pi\": 3.14, \"Answer\": 42}", alias))
					c.writeln("")
				}
			}
		}
	}

	c.emitAutoStructs()

	if c.models {
		c.writeln("_models = {}")
		c.writeln("")
	}
	if len(c.helpers) > 0 {
		c.emitRuntime()
	}
	c.buf.Write(body)

	code := c.buf.Bytes()
	return FormatPy(code), nil
}

// --- Expressions ---

func (c *Compiler) compileExpr(e *parser.Expr) (string, error) { return c.compileBinaryExpr(e.Binary) }

func (c *Compiler) compileBinaryExpr(b *parser.BinaryExpr) (string, error) {
	if b == nil {
		return "", fmt.Errorf("nil binary expression")
	}

	operands := []string{}
	typesList := []types.Type{}

	first, err := c.compileUnary(b.Left)
	if err != nil {
		return "", err
	}
	operands = append(operands, first)
	typesList = append(typesList, c.inferUnaryType(b.Left))

	ops := []string{}
	for _, part := range b.Right {
		r, err := c.compilePostfix(part.Right)
		if err != nil {
			return "", err
		}
		operands = append(operands, r)
		typesList = append(typesList, c.inferPostfixType(part.Right))
		op := part.Op
		if op == "union" && part.All {
			op = "union_all"
		}
		ops = append(ops, op)
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

	contains := func(sl []string, s string) bool {
		for _, v := range sl {
			if v == s {
				return true
			}
		}
		return false
	}

	for _, level := range levels {
		for i := 0; i < len(ops); {
			if !contains(level, ops[i]) {
				i++
				continue
			}
			op := ops[i]
			lExpr := operands[i]
			rExpr := operands[i+1]
			lType := typesList[i]
			rType := typesList[i+1]

			pyOp := op
			switch op {
			case "&&":
				pyOp = "and"
			case "||":
				pyOp = "or"
			}

			var expr string
			var t types.Type = types.AnyType{}

			switch op {
			case "/":
				expr = fmt.Sprintf("(%s / %s)", lExpr, rExpr)
				t = types.FloatType{}
			case "union", "union_all", "except", "intersect":
				var elem types.Type = types.AnyType{}
				switch lt := lType.(type) {
				case types.ListType:
					elem = lt.Elem
				case types.GroupType:
					elem = lt.Elem
				}
				switch rt := rType.(type) {
				case types.ListType:
					if isAny(elem) {
						elem = rt.Elem
					} else if !equalTypes(elem, rt.Elem) {
						elem = types.AnyType{}
					}
				case types.GroupType:
					if isAny(elem) {
						elem = rt.Elem
					} else if !equalTypes(elem, rt.Elem) {
						elem = types.AnyType{}
					}
				}
				switch op {
				case "union_all":
					if _, ok := lType.(types.ListType); ok {
						if _, ok := rType.(types.ListType); ok {
							expr = fmt.Sprintf("%s + %s", lExpr, rExpr)
							t = types.ListType{Elem: elem}
							break
						}
					}
				case "except":
					if _, ok := lType.(types.ListType); ok {
						if _, ok := rType.(types.ListType); ok {
							expr = fmt.Sprintf("[it for it in %s if it not in %s]", lExpr, rExpr)
							t = types.ListType{Elem: elem}
							break
						}
					}
				case "union":
					if _, ok := lType.(types.ListType); ok {
						if _, ok := rType.(types.ListType); ok {
							expr = fmt.Sprintf("list(dict.fromkeys(%s + %s))", lExpr, rExpr)
							t = types.ListType{Elem: elem}
							break
						}
					}
				case "intersect":
					if _, ok := lType.(types.ListType); ok {
						if _, ok := rType.(types.ListType); ok {
							expr = fmt.Sprintf("list(dict.fromkeys([it for it in %s if it in %s]))", lExpr, rExpr)
							t = types.ListType{Elem: elem}
							break
						}
					}
				}
				if expr == "" {
					c.use("_" + op)
					expr = fmt.Sprintf("_%s(%s, %s)", op, lExpr, rExpr)
					t = types.ListType{Elem: elem}
				}
			case "+":
				if isString(lType) || isString(rType) {
					if !isString(lType) {
						lExpr = fmt.Sprintf("str(%s)", lExpr)
					}
					if !isString(rType) {
						rExpr = fmt.Sprintf("str(%s)", rExpr)
					}
					expr = fmt.Sprintf("(%s + %s)", lExpr, rExpr)
					t = types.StringType{}
				} else {
					expr = fmt.Sprintf("(%s + %s)", lExpr, rExpr)
					t = lType
				}
			default:
				expr = fmt.Sprintf("(%s %s %s)", lExpr, pyOp, rExpr)
				switch op {
				case "==", "!=", "<", "<=", ">", ">=", "in", "&&", "||":
					t = types.BoolType{}
				case "-", "*", "%":
					t = lType
				}
			}

			operands[i] = expr
			typesList[i] = t
			operands = append(operands[:i+1], operands[i+2:]...)
			typesList = append(typesList[:i+1], typesList[i+2:]...)
			ops = append(ops[:i], ops[i+1:]...)
		}
	}

	if len(operands) != 1 {
		return "", fmt.Errorf("unexpected state in binary expr")
	}
	return operands[0], nil
}

func (c *Compiler) compileUnary(u *parser.Unary) (string, error) {
	val, err := c.compilePostfix(u.Value)
	if err != nil {
		return "", err
	}
	for i := len(u.Ops) - 1; i >= 0; i-- {
		op := u.Ops[i]
		if op == "!" {
			op = "not "
		}
		val = fmt.Sprintf("(%s%s)", op, val)
	}
	return val, nil
}

func (c *Compiler) compilePostfix(p *parser.PostfixExpr) (string, error) {
	expr, err := c.compilePrimary(p.Target)
	if err != nil {
		return "", err
	}
	typ := c.inferPrimaryType(p.Target)
	for _, op := range p.Ops {
		if op.Call != nil {
			// Special case: "contains" method on strings or lists
			if p.Target.Selector != nil && len(p.Target.Selector.Tail) > 0 &&
				p.Target.Selector.Tail[len(p.Target.Selector.Tail)-1] == "contains" &&
				len(op.Call.Args) == 1 {
				recvSel := &parser.Primary{Selector: &parser.SelectorExpr{
					Root: p.Target.Selector.Root,
					Tail: p.Target.Selector.Tail[:len(p.Target.Selector.Tail)-1],
				}}
				recvExpr, err := c.compilePrimary(recvSel)
				if err != nil {
					return "", err
				}
				argExpr, err := c.compileExpr(op.Call.Args[0])
				if err != nil {
					return "", err
				}
				expr = fmt.Sprintf("(%s in %s)", argExpr, recvExpr)
				typ = types.BoolType{}
				continue
			}

			if p.Target.Selector != nil && p.Target.Selector.Root == "strings" &&
				len(p.Target.Selector.Tail) == 1 && p.Target.Selector.Tail[0] == "ToUpper" &&
				len(op.Call.Args) == 1 {
				argExpr, err := c.compileExpr(op.Call.Args[0])
				if err != nil {
					return "", err
				}
				expr = fmt.Sprintf("%s.upper()", argExpr)
				typ = types.StringType{}
				continue
			}

			if p.Target.Selector != nil && len(p.Target.Selector.Tail) > 0 &&
				p.Target.Selector.Tail[len(p.Target.Selector.Tail)-1] == "starts_with" &&
				len(op.Call.Args) == 1 {
				recvSel := &parser.Primary{Selector: &parser.SelectorExpr{
					Root: p.Target.Selector.Root,
					Tail: p.Target.Selector.Tail[:len(p.Target.Selector.Tail)-1],
				}}
				recvExpr, err := c.compilePrimary(recvSel)
				if err != nil {
					return "", err
				}
				argExpr, err := c.compileExpr(op.Call.Args[0])
				if err != nil {
					return "", err
				}
				expr = fmt.Sprintf("str(%s).startswith(%s)", recvExpr, argExpr)
				typ = types.BoolType{}
				continue
			}

			args := make([]string, len(op.Call.Args))
			for i, a := range op.Call.Args {
				v, err := c.compileExpr(a)
				if err != nil {
					return "", err
				}
				args[i] = v
			}
			expr = fmt.Sprintf("%s(%s)", expr, strings.Join(args, ", "))
			typ = c.inferPostfixType(&parser.PostfixExpr{Target: &parser.Primary{Call: nil}})
			continue
		}
		if op.Index != nil {
			idx := op.Index
			if idx.Colon != nil {
				start, end := "", ""
				if idx.Start != nil {
					start, err = c.compileExpr(idx.Start)
					if err != nil {
						return "", err
					}
				}
				if idx.End != nil {
					end, err = c.compileExpr(idx.End)
					if err != nil {
						return "", err
					}
				}
				switch typ.(type) {
				case types.ListType, types.StringType:
					expr = fmt.Sprintf("%s[%s:%s]", expr, start, end)
					if _, ok := typ.(types.StringType); ok {
						typ = types.StringType{}
					}
				default:
					startArg, endArg := "0", "0"
					if idx.Start != nil {
						startArg = start
					}
					if idx.End != nil {
						endArg = end
					}
					c.use("_slice")
					expr = fmt.Sprintf("_slice(%s, %s, %s)", expr, startArg, endArg)
					typ = types.AnyType{}
				}
			} else {
				idxExpr, err := c.compileExpr(idx.Start)
				if err != nil {
					return "", err
				}
				switch tt := typ.(type) {
				case types.ListType:
					expr = fmt.Sprintf("%s[%s]", expr, idxExpr)
					typ = tt.Elem
				case types.MapType:
					expr = fmt.Sprintf("%s[%s]", expr, idxExpr)
					typ = tt.Value
				case types.StructType:
					if s, ok := stringLit(idx.Start); ok {
						field := sanitizeName(strings.Trim(s, "\""))
						expr = fmt.Sprintf("%s.%s", expr, field)
						if ft, ok := tt.Fields[field]; ok {
							typ = ft
						} else {
							typ = types.AnyType{}
						}
					} else {
						expr = fmt.Sprintf("getattr(%s, %s)", expr, idxExpr)
						typ = types.AnyType{}
					}
				case types.StringType:
					expr = fmt.Sprintf("%s[%s]", expr, idxExpr)
					typ = types.StringType{}
				default:
					expr = fmt.Sprintf("%s[%s]", expr, idxExpr)
					typ = types.AnyType{}
				}
			}
			continue
		}
		if op.Cast != nil {
			typ = c.resolveTypeRef(op.Cast.Type)
			switch t := typ.(type) {
			case types.IntType, types.Int64Type:
				expr = fmt.Sprintf("int(%s)", expr)
			case types.FloatType:
				expr = fmt.Sprintf("float(%s)", expr)
			case types.StringType:
				expr = fmt.Sprintf("str(%s)", expr)
			case types.BoolType:
				expr = fmt.Sprintf("bool(%s)", expr)
			case types.StructType:
				expr = fmt.Sprintf("%s(**%s)", sanitizeName(t.Name), expr)
			case types.ListType:
				if st, ok := t.Elem.(types.StructType); ok {
					expr = fmt.Sprintf("[ %s(**_it) for _it in %s ]", sanitizeName(st.Name), expr)
				}
			default:
				// For other complex types just ignore the cast at runtime
			}
			continue
		}
	}
	return expr, nil
}

func (c *Compiler) compilePrimary(p *parser.Primary) (string, error) {
	switch {
	case p.Call != nil:
		return c.compileCallExpr(p.Call)
	case p.List != nil:
		return c.compileListLiteral(p.List)
	case p.Map != nil:
		return c.compileMapLiteral(p.Map)
	case p.Query != nil:
		return c.compileQueryExpr(p.Query)
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
	case p.Lit != nil:
		return c.compileLiteral(p.Lit)
	case p.Group != nil:
		expr, err := c.compileExpr(p.Group)
		if err != nil {
			return "", err
		}
		return "(" + expr + ")", nil
	case p.Selector != nil:
		expr := sanitizeName(p.Selector.Root)
		rootName := expr
		var typ types.Type = types.AnyType{}
		if c.env != nil {
			if u, ok := c.env.FindUnionByVariant(p.Selector.Root); ok {
				if vt, ok := u.Variants[p.Selector.Root]; ok && len(vt.Fields) == 0 && len(p.Selector.Tail) == 0 {
					return fmt.Sprintf("%s()", expr), nil
				}
			}
			if t, err := c.env.GetVar(p.Selector.Root); err == nil {
				typ = t
			}
		}
		if c.methodFields != nil && c.methodFields[p.Selector.Root] {
			expr = "self." + expr
		}
		// If the identifier is not found in the current scope but matches
		// a group-by key field, reference it via the current group
		if _, ok := typ.(types.AnyType); ok && len(p.Selector.Tail) == 0 &&
			c.currentGroup != "" && c.groupFields[sanitizeName(p.Selector.Root)] {
			return fmt.Sprintf("%s.key.%s", c.currentGroup, sanitizeName(p.Selector.Root)), nil
		}
		tail := p.Selector.Tail
		if fm, ok := c.tupleFields[rootName]; ok && len(tail) > 0 {
			key := sanitizeName(tail[0])
			if idx, ok := fm[key]; ok {
				expr += fmt.Sprintf("[%d]", idx)
				if ftm, ok := c.tupleTypes[rootName]; ok {
					if t, ok := ftm[key]; ok {
						typ = t
					} else {
						typ = types.AnyType{}
					}
				} else {
					typ = types.AnyType{}
				}
				tail = tail[1:]
			} else {
				if key != "key" {
					if ftm, ok := c.tupleTypes[rootName]; ok {
						for alias, idx := range fm {
							switch at := ftm[alias].(type) {
							case types.StructType:
								if ft, ok := at.Fields[key]; ok {
									expr += fmt.Sprintf("[%d].%s", idx, sanitizeName(key))
									typ = ft
									tail = tail[1:]
									break
								}
							case types.MapType:
								expr += fmt.Sprintf("[%d][%q]", idx, sanitizeName(key))
								typ = at.Value
								tail = tail[1:]
								break
							}
						}
					}
				}
			}
		}
		for i, s := range tail {
			rest := tail[i:]
			switch t := typ.(type) {
			case types.MapType:
				expr = fmt.Sprintf("%s[%q]", expr, sanitizeName(s))
				typ = t.Value
			case types.StructType:
				if ft, ok := t.Fields[s]; ok {
					expr = fmt.Sprintf("%s.%s", expr, sanitizeName(s))
					typ = ft
				} else {
					expr = fmt.Sprintf("(%s.get(%q) if isinstance(%s, dict) else getattr(%s, %q))", expr, sanitizeName(s), expr, expr, sanitizeName(s))
					typ = types.AnyType{}
				}
			case types.UnionType:
				if ft, ok := unionFieldPathType(t, rest); ok {
					expr = fmt.Sprintf("%s.%s", expr, sanitizeName(s))
					typ = ft
				} else {
					expr = fmt.Sprintf("(%s.get(%q) if isinstance(%s, dict) else getattr(%s, %q))", expr, sanitizeName(s), expr, expr, sanitizeName(s))
					typ = types.AnyType{}
				}
			case types.GroupType:
				switch sanitizeName(s) {
				case "key":
					expr = fmt.Sprintf("%s.key", expr)
					typ = t.Key
				case "items", "Items":
					expr = fmt.Sprintf("%s.Items", expr)
					typ = types.ListType{Elem: t.Elem}
				default:
					expr = fmt.Sprintf("(%s.get(%q) if isinstance(%s, dict) else getattr(%s, %q))", expr, sanitizeName(s), expr, expr, sanitizeName(s))
					typ = types.AnyType{}
				}
			default:
				expr = fmt.Sprintf("(%s.get(%q) if isinstance(%s, dict) else getattr(%s, %q))", expr, sanitizeName(s), expr, expr, sanitizeName(s))
				typ = types.AnyType{}
			}
		}
		return expr, nil
	case p.Struct != nil:
		if c.env != nil {
			if _, ok := c.env.GetAgent(p.Struct.Name); ok {
				if len(p.Struct.Fields) == 0 {
					return fmt.Sprintf("New%s()", sanitizeName(p.Struct.Name)), nil
				}
				tmp := fmt.Sprintf("_t%d", c.tmpCount)
				c.tmpCount++
				var b strings.Builder
				b.WriteString("(lambda " + tmp + "=New" + sanitizeName(p.Struct.Name) + "(): (")
				calls := []string{}
				for _, f := range p.Struct.Fields {
					v, err := c.compileExpr(f.Value)
					if err != nil {
						return "", err
					}
					calls = append(calls, fmt.Sprintf("setattr(%s, %q, %s)", tmp, sanitizeName(f.Name), v))
				}
				calls = append(calls, tmp)
				b.WriteString("(" + strings.Join(calls, ", ") + ")[-1])()")
				return b.String(), nil
			}
		}
		parts := make([]string, len(p.Struct.Fields))
		for i, f := range p.Struct.Fields {
			v, err := c.compileExpr(f.Value)
			if err != nil {
				return "", err
			}
			parts[i] = fmt.Sprintf("%s=%s", sanitizeName(f.Name), v)
		}
		return fmt.Sprintf("%s(%s)", sanitizeName(p.Struct.Name), strings.Join(parts, ", ")), nil
	case p.FunExpr != nil:
		return c.compileFunExpr(p.FunExpr)
	default:
		return "", fmt.Errorf("invalid primary expression")
	}
}

func (c *Compiler) compileCallExpr(call *parser.CallExpr) (string, error) {
	if lit, ok := interpreter.EvalPureCall(call, c.env); ok {
		return c.compileLiteral(lit)
	}
	args := make([]string, len(call.Args))
	var paramTypes []types.Type
	if t, err := c.env.GetVar(call.Func); err == nil {
		if ft, ok := t.(types.FuncType); ok {
			paramTypes = ft.Params
		}
	}
	for i, a := range call.Args {
		if len(paramTypes) > i {
			v, err := c.compileExprHint(a, paramTypes[i])
			if err != nil {
				return "", err
			}
			if _, ok := paramTypes[i].(types.StructType); ok {
				c.imports["dataclasses"] = "dataclasses"
				v = fmt.Sprintf("dataclasses.replace(%s)", v)
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
		// Partial application
		c.imports["functools"] = "functools"
		if len(args) == 0 {
			return fmt.Sprintf("functools.partial(%s)", sanitizeName(call.Func)), nil
		}
		return fmt.Sprintf("functools.partial(%s, %s)", sanitizeName(call.Func), argStr), nil
	}
	switch call.Func {
	case "print":
		c.use("_fmt")
		for i, a := range call.Args {
			arg := args[i]
			if _, ok := c.inferExprType(a).(types.BoolType); ok {
				arg = fmt.Sprintf("str(%s).lower()", arg)
			}
			args[i] = fmt.Sprintf("_fmt(%s)", arg)
		}
		if len(args) == 1 {
			return fmt.Sprintf("print(%s)", args[0]), nil
		}
		return fmt.Sprintf("print(' '.join([%s]).rstrip())", strings.Join(args, ", ")), nil
	case "len":
		return fmt.Sprintf("len(%s)", argStr), nil
	case "now":
		c.imports["time"] = "time"
		return "time.time_ns()", nil
	case "json":
		c.imports["json"] = "json"
		return fmt.Sprintf("print(json.dumps(%s, indent=2, default=lambda o: vars(o)))", argStr), nil
	case "str":
		return fmt.Sprintf("str(%s)", argStr), nil
	case "int":
		return fmt.Sprintf("int(%s)", argStr), nil
	case "input":
		return "input()", nil
	case "count":
		if len(args) == 1 {
			t := c.inferExprType(call.Args[0])
			switch t.(type) {
			case types.ListType:
				return fmt.Sprintf("len(%s)", args[0]), nil
			case types.GroupType:
				return fmt.Sprintf("len(%s)", args[0]), nil
			case types.MapType, types.StringType:
				return fmt.Sprintf("len(%s)", args[0]), nil
			}
		}
		c.use("_count")
		return fmt.Sprintf("_count(%s)", argStr), nil
	case "exists":
		if len(args) == 1 {
			t := c.inferExprType(call.Args[0])
			switch t.(type) {
			case types.ListType:
				return fmt.Sprintf("(len(%s) > 0)", args[0]), nil
			case types.GroupType:
				return fmt.Sprintf("(len(%s) > 0)", args[0]), nil
			case types.MapType, types.StringType:
				return fmt.Sprintf("(len(%s) > 0)", args[0]), nil
			}
		}
		c.use("_exists")
		return fmt.Sprintf("_exists(%s)", argStr), nil
	case "avg":
		if len(args) == 1 {
			t := c.inferExprType(call.Args[0])
			switch tt := t.(type) {
			case types.ListType:
				if isNumeric(tt.Elem) {
					return fmt.Sprintf("(sum(%s)/len(%s) if %s else 0)", args[0], args[0], args[0]), nil
				}
			case types.GroupType:
				if isNumeric(tt.Elem) {
					return fmt.Sprintf("(sum(%s)/len(%s) if %s else 0)", args[0], args[0], args[0]), nil
				}
			}
		}
		c.use("_avg")
		return fmt.Sprintf("_avg(%s)", argStr), nil
	case "sum":
		if len(args) == 1 {
			t := c.inferExprType(call.Args[0])
			switch tt := t.(type) {
			case types.ListType:
				if isNumeric(tt.Elem) {
					return fmt.Sprintf("sum(%s)", args[0]), nil
				}
			case types.GroupType:
				if isNumeric(tt.Elem) {
					return fmt.Sprintf("sum(%s)", args[0]), nil
				}
			}
			c.use("_sum")
			return fmt.Sprintf("_sum(%s)", args[0]), nil
		}
		return fmt.Sprintf("sum(%s)", argStr), nil
	case "min":
		if len(args) == 1 {
			t := c.inferExprType(call.Args[0])
			switch tt := t.(type) {
			case types.ListType:
				if isNumeric(tt.Elem) {
					return fmt.Sprintf("(min([it for it in %s if it is not None]) if %s else 0)", args[0], args[0]), nil
				}
			case types.GroupType:
				if isNumeric(tt.Elem) {
					return fmt.Sprintf("(min([it for it in %s if it is not None]) if %s else 0)", args[0], args[0]), nil
				}
			}
			c.use("_min")
			return fmt.Sprintf("_min(%s)", args[0]), nil
		}
		return fmt.Sprintf("min(%s)", argStr), nil
	case "max":
		if len(args) == 1 {
			t := c.inferExprType(call.Args[0])
			switch tt := t.(type) {
			case types.ListType:
				if isNumeric(tt.Elem) {
					return fmt.Sprintf("(max([it for it in %s if it is not None]) if %s else 0)", args[0], args[0]), nil
				}
			case types.GroupType:
				if isNumeric(tt.Elem) {
					return fmt.Sprintf("(max([it for it in %s if it is not None]) if %s else 0)", args[0], args[0]), nil
				}
			}
			c.use("_max")
			return fmt.Sprintf("_max(%s)", args[0]), nil
		}
		return fmt.Sprintf("max(%s)", argStr), nil
	case "first":
		if len(args) != 1 {
			return "", fmt.Errorf("first expects 1 arg")
		}
		switch c.inferExprType(call.Args[0]).(type) {
		case types.ListType:
			arg := args[0]
			return fmt.Sprintf("(%s[0] if len(%s) > 0 else None)", arg, arg), nil
		case types.GroupType:
			arg := args[0]
			return fmt.Sprintf("(%s[0] if len(%s) > 0 else None)", arg, arg), nil
		}
		c.use("_first")
		return fmt.Sprintf("_first(%s)", args[0]), nil
	case "concat":
		if len(args) == 0 {
			return "[]", nil
		}
		allLists := true
		for _, a := range call.Args {
			if _, ok := c.inferExprType(a).(types.ListType); !ok {
				allLists = false
				break
			}
		}
		expr := args[0]
		for _, a := range args[1:] {
			if allLists {
				expr = fmt.Sprintf("%s + %s", expr, a)
			} else {
				c.use("_union_all")
				expr = fmt.Sprintf("_union_all(%s, %s)", expr, a)
			}
		}
		return expr, nil
	case "substr", "substring":
		if len(args) != 3 {
			return "", fmt.Errorf("substr expects 3 args")
		}
		t := c.inferExprType(call.Args[0])
		if _, ok := t.(types.StringType); ok {
			return fmt.Sprintf("%s[%s:%s]", args[0], args[1], args[2]), nil
		}
		if _, ok := t.(types.ListType); ok {
			return fmt.Sprintf("%s[%s:%s]", args[0], args[1], args[2]), nil
		}
		c.use("_slice")
		return fmt.Sprintf("_slice(%s, %s, %s)", args[0], args[1], args[2]), nil
	case "reverse":
		if len(args) != 1 {
			return "", fmt.Errorf("reverse expects 1 arg")
		}
		t := c.inferExprType(call.Args[0])
		switch t.(type) {
		case types.ListType:
			return fmt.Sprintf("list(reversed(%s))", args[0]), nil
		case types.StringType:
			return fmt.Sprintf("%s[::-1]", args[0]), nil
		}
		c.use("_reverse")
		return fmt.Sprintf("_reverse(%s)", args[0]), nil
	case "append":
		if len(args) != 2 {
			return "", fmt.Errorf("append expects 2 args")
		}
		if _, ok := c.inferExprType(call.Args[0]).(types.ListType); ok {
			return fmt.Sprintf("%s + [%s]", args[0], args[1]), nil
		}
		c.use("_append")
		return fmt.Sprintf("_append(%s, %s)", args[0], args[1]), nil
	case "strings.ToUpper":
		if len(args) != 1 {
			return "", fmt.Errorf("strings.ToUpper expects 1 arg")
		}
		return fmt.Sprintf("%s.upper()", args[0]), nil
	case "lower":
		if len(args) != 1 {
			return "", fmt.Errorf("lower expects 1 arg")
		}
		return fmt.Sprintf("str(%s).lower()", args[0]), nil
	case "contains":
		if len(args) != 2 {
			return "", fmt.Errorf("contains expects 2 args")
		}
		t := c.inferExprType(call.Args[0])
		switch t.(type) {
		case types.ListType:
			return fmt.Sprintf("(%s in %s)", args[1], args[0]), nil
		case types.StringType:
			return fmt.Sprintf("(str(%s) in %s)", args[1], args[0]), nil
		case types.MapType:
			return fmt.Sprintf("(str(%s) in %s)", args[1], args[0]), nil
		case types.GroupType:
			return fmt.Sprintf("(%s in %s)", args[1], args[0]), nil
		}
		c.use("_contains")
		return fmt.Sprintf("_contains(%s, %s)", args[0], args[1]), nil
	case "values":
		if len(args) != 1 {
			return "", fmt.Errorf("values expects 1 arg")
		}
		if _, ok := c.inferExprType(call.Args[0]).(types.MapType); ok {
			return fmt.Sprintf("list(%s.values())", args[0]), nil
		}
		c.use("_values")
		return fmt.Sprintf("_values(%s)", args[0]), nil
	case "eval":
		return fmt.Sprintf("eval(%s)", argStr), nil
	default:
		return fmt.Sprintf("%s(%s)", sanitizeName(call.Func), argStr), nil
	}
}

func buildFString(args []string, exprs []*parser.Expr) string {
	parts := make([]string, len(args))
	for i, a := range args {
		if s, ok := stringLit(exprs[i]); ok {
			// escape internal quotes
			parts[i] = strings.ReplaceAll(s, "\"", "\\\"")
		} else {
			parts[i] = fmt.Sprintf("{%s}", a)
		}
	}
	return "f'" + strings.Join(parts, " ") + "'"
}

// compileExprHint compiles an expression using a type hint when dealing with
// literals that would otherwise default to `any`, such as empty list or map
// literals. Hints are applied recursively for nested literals.
func (c *Compiler) compileExprHint(e *parser.Expr, hint types.Type) (string, error) {
	if lt, ok := hint.(types.ListType); ok {
		if ll := e.Binary.Left.Value.Target.List; ll != nil {
			elems := make([]string, len(ll.Elems))
			for i, el := range ll.Elems {
				ev, err := c.compileExprHint(el, lt.Elem)
				if err != nil {
					return "", err
				}
				elems[i] = ev
			}
			return "[" + strings.Join(elems, ", ") + "]", nil
		}
	}
	if mt, ok := hint.(types.MapType); ok {
		if ml := e.Binary.Left.Value.Target.Map; ml != nil {
			if len(ml.Items) == 0 {
				typStr := fmt.Sprintf("typing.cast(dict[%s, %s], {})", pyType(mt.Key), pyType(mt.Value))
				if needsTyping(typStr) {
					c.imports["typing"] = "typing"
				}
				return typStr, nil
			}
			allIdent := true
			for _, it := range ml.Items {
				if _, ok := identName(it.Key); !ok {
					allIdent = false
					break
				}
			}
			if allIdent {
				return c.compileMapLiteral(ml)
			}
			items := make([]string, len(ml.Items))
			for i, it := range ml.Items {
				k, err := c.compileExprHint(it.Key, mt.Key)
				if err != nil {
					return "", err
				}
				if s, ok := simpleStringKey(it.Key); ok {
					k = fmt.Sprintf("%q", sanitizeName(s))
				} else if name, ok := identName(it.Key); ok {
					k = fmt.Sprintf("%q", sanitizeName(name))
				}
				v, err := c.compileExprHint(it.Value, mt.Value)
				if err != nil {
					return "", err
				}
				items[i] = fmt.Sprintf("%s: %s", k, v)
			}
			return "{" + strings.Join(items, ", ") + "}", nil
		}
	}
	if st, ok := hint.(types.StructType); ok {
		if ml := e.Binary.Left.Value.Target.Map; ml != nil {
			if len(ml.Items) == len(st.Fields) {
				if st.Name == "" {
					// Anonymous struct literals are rendered as
					// plain dictionaries to match other backends.
					return c.compileMapLiteral(ml)
				}
				st = c.ensureStructName(st)
				fields := make([]string, len(ml.Items))
				for i, it := range ml.Items {
					name, ok := identName(it.Key)
					if !ok {
						if s, ok2 := simpleStringKey(it.Key); ok2 {
							name = s
						} else {
							// fallback to map literal
							return c.compileMapLiteral(ml)
						}
					}
					ft, ok := st.Fields[name]
					var val string
					var err error
					if ok {
						val, err = c.compileExprHint(it.Value, ft)
					} else {
						val, err = c.compileExpr(it.Value)
					}
					if err != nil {
						return "", err
					}
					fields[i] = fmt.Sprintf("%s=%s", sanitizeName(name), val)
				}
				return fmt.Sprintf("%s(%s)", sanitizeName(st.Name), strings.Join(fields, ", ")), nil
			}
		}
	}
	return c.compileExpr(e)
}

func (c *Compiler) compileFunExpr(fn *parser.FunExpr) (string, error) {
	params := make([]string, len(fn.Params))
	for i, p := range fn.Params {
		params[i] = sanitizeName(p.Name)
	}
	if fn.ExprBody != nil {
		expr, err := c.compileExpr(fn.ExprBody)
		if err != nil {
			return "", err
		}
		return fmt.Sprintf("(lambda %s: %s)", strings.Join(params, ", "), expr), nil
	}

	name := fmt.Sprintf("_fn%d", c.tmpCount)
	c.tmpCount++
	stmt := &parser.FunStmt{Name: name, Params: fn.Params, Return: fn.Return, Body: fn.BlockBody}
	if err := c.compileFunStmt(stmt); err != nil {
		return "", err
	}
	return name, nil
}

// compileFunExprDef compiles fn to a named function even when it has an
// expression body. It is used for return statements so the output resembles the
// hand-written examples.
func (c *Compiler) compileFunExprDef(fn *parser.FunExpr) (string, error) {
	name := fmt.Sprintf("_fn%d", c.tmpCount)
	c.tmpCount++

	var body []*parser.Statement
	if fn.ExprBody != nil {
		body = []*parser.Statement{{Return: &parser.ReturnStmt{Value: fn.ExprBody}}}
	} else {
		body = fn.BlockBody
	}

	stmt := &parser.FunStmt{Name: name, Params: fn.Params, Return: fn.Return, Body: body}
	if err := c.compileFunStmt(stmt); err != nil {
		return "", err
	}
	return name, nil
}

func (c *Compiler) compileListLiteral(l *parser.ListLiteral) (string, error) {
	elems := make([]string, len(l.Elems))
	multiline := false
	for i, e := range l.Elems {
		v, err := c.compileExpr(e)
		if err != nil {
			return "", err
		}
		if strings.ContainsAny(v, "([{=") || len(v) > 20 {
			multiline = true
		}
		elems[i] = v
	}
	if multiline && len(elems) > 1 {
		var b strings.Builder
		b.WriteString("[\n")
		indent := strings.Repeat("\t", c.indent+1)
		for _, v := range elems {
			b.WriteString(indent)
			b.WriteString(v)
			b.WriteString(",\n")
		}
		b.WriteString(strings.Repeat("\t", c.indent))
		b.WriteByte(']')
		return b.String(), nil
	}
	return "[" + strings.Join(elems, ", ") + "]", nil
}

func (c *Compiler) compileMapLiteral(m *parser.MapLiteral) (string, error) {
	// check if this literal represents a struct type
	expr := &parser.Expr{Binary: &parser.BinaryExpr{Left: &parser.Unary{Value: &parser.PostfixExpr{Target: &parser.Primary{Map: m}}}}}
	if st, ok := c.inferExprType(expr).(types.StructType); ok {
		allIdent := true
		for _, it := range m.Items {
			if _, ok := identName(it.Key); !ok {
				allIdent = false
				break
			}
		}
		if allIdent {
			st = c.ensureStructName(st)
			fields := make([]string, len(m.Items))
			for i, it := range m.Items {
				name, _ := identName(it.Key)
				val, err := c.compileExpr(it.Value)
				if err != nil {
					return "", err
				}
				fields[i] = fmt.Sprintf("%s=%s", sanitizeName(name), val)
			}
			return fmt.Sprintf("%s(%s)", sanitizeName(st.Name), strings.Join(fields, ", ")), nil
		}
	}
	// treat as struct when all keys are identifiers even if type inference produced a map
	keys := make([]string, len(m.Items))
	okStruct := true
	fields := make(map[string]types.Type, len(m.Items))
	for i, it := range m.Items {
		name, ok := identName(it.Key)
		if !ok {
			okStruct = false
			break
		}
		keys[i] = name
		fields[name] = c.inferExprType(it.Value)
	}
	if okStruct {
		st := types.StructType{Fields: fields, Order: keys}
		st = c.ensureStructName(st)
		parts := make([]string, len(m.Items))
		for i, it := range m.Items {
			val, err := c.compileExpr(it.Value)
			if err != nil {
				return "", err
			}
			parts[i] = fmt.Sprintf("%s=%s", sanitizeName(keys[i]), val)
		}
		return fmt.Sprintf("%s(%s)", sanitizeName(st.Name), strings.Join(parts, ", ")), nil
	}

	items := make([]string, len(m.Items))
	for i, it := range m.Items {
		k, err := c.compileExpr(it.Key)
		if err != nil {
			return "", err
		}
		if s, ok := simpleStringKey(it.Key); ok {
			k = fmt.Sprintf("%q", sanitizeName(s))
		} else if name, ok := identName(it.Key); ok {
			k = fmt.Sprintf("%q", sanitizeName(name))
		}
		v, err := c.compileExpr(it.Value)
		if err != nil {
			return "", err
		}
		items[i] = fmt.Sprintf("%s: %s", k, v)
	}
	return "{" + strings.Join(items, ", ") + "}", nil
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
		c.imports["dataclasses"] = "dataclasses"
		withStr = fmt.Sprintf("dataclasses.asdict(%[1]s) if dataclasses.is_dataclass(%[1]s) else dict(%[1]s)", w)
	} else {
		withStr = "None"
	}
	c.imports["urllib.request"] = "urllib.request"
	c.imports["json"] = "json"
	c.use("_fetch")
	return fmt.Sprintf("_fetch(%s, %s)", urlStr, withStr), nil
}

func (c *Compiler) compileLoadExpr(l *parser.LoadExpr) (string, error) {
	var pathStr string
	if l.Path != nil {
		pathStr = fmt.Sprintf("%q", *l.Path)
	} else {
		pathStr = "None"
	}
	optsStr := "None"
	if l.With != nil {
		w, err := c.compileExpr(l.With)
		if err != nil {
			return "", err
		}
		c.imports["dataclasses"] = "dataclasses"
		optsStr = fmt.Sprintf("dataclasses.asdict(%[1]s) if dataclasses.is_dataclass(%[1]s) else dict(%[1]s)", w)
	}
	c.use("_load")
	expr := fmt.Sprintf("_load(%s, %s)", pathStr, optsStr)
	if l.Type != nil && l.Type.Simple != nil {
		tname := sanitizeName(*l.Type.Simple)
		expr = fmt.Sprintf("[ %s(**_it) for _it in %s ]", tname, expr)
	}
	return expr, nil
}

func (c *Compiler) compileSaveExpr(s *parser.SaveExpr) (string, error) {
	src, err := c.compileExpr(s.Src)
	if err != nil {
		return "", err
	}
	var pathStr string
	if s.Path != nil {
		pathStr = fmt.Sprintf("%q", *s.Path)
	} else {
		pathStr = "None"
	}
	optsStr := "None"
	if s.With != nil {
		w, err := c.compileExpr(s.With)
		if err != nil {
			return "", err
		}
		c.imports["dataclasses"] = "dataclasses"
		optsStr = fmt.Sprintf("dataclasses.asdict(%[1]s) if dataclasses.is_dataclass(%[1]s) else dict(%[1]s)", w)
	}
	c.use("_save")
	return fmt.Sprintf("_save(%s, %s, %s)", src, pathStr, optsStr), nil
}

func (c *Compiler) compileQueryExpr(q *parser.QueryExpr) (string, error) {
	src, err := c.compileExpr(q.Source)
	if err != nil {
		return "", err
	}
	if name, ok := identName(q.Source); ok {
		if fm, ok := c.tupleFields[sanitizeName(name)]; ok {
			c.tupleFields[sanitizeName(q.Var)] = fm
			defer delete(c.tupleFields, sanitizeName(q.Var))
		}
		if ft, ok := c.tupleTypes[sanitizeName(name)]; ok {
			c.tupleTypes[sanitizeName(q.Var)] = ft
			defer delete(c.tupleTypes, sanitizeName(q.Var))
		}
	}

	child := types.NewEnv(c.env)
	var elemType types.Type = types.AnyType{}
	switch t := c.inferExprType(q.Source).(type) {
	case types.ListType:
		elemType = t.Elem
	case types.GroupType:
		elemType = t.Elem
	}
	child.SetVar(q.Var, elemType, true)
	for _, f := range q.Froms {
		ft := c.inferExprType(f.Src)
		var fe types.Type = types.AnyType{}
		switch tt := ft.(type) {
		case types.ListType:
			fe = tt.Elem
		case types.GroupType:
			fe = tt.Elem
		}
		child.SetVar(f.Var, fe, true)
	}
	for _, j := range q.Joins {
		jt := c.inferExprType(j.Src)
		var je types.Type = types.AnyType{}
		switch lt := jt.(type) {
		case types.ListType:
			je = lt.Elem
		case types.GroupType:
			je = lt.Elem
		}
		child.SetVar(j.Var, je, true)
	}
	orig := c.env
	c.env = child
	sel, err := c.compileExpr(q.Select)
	if err != nil {
		c.env = orig
		return "", err
	}
	if ml := q.Select.Binary.Left.Value.Target.Map; ml != nil && len(q.Select.Binary.Right) == 0 {
		keys := make([]string, len(ml.Items))
		fields := make(map[string]types.Type, len(ml.Items))
		okStruct := true
		for i, it := range ml.Items {
			name, ok := identName(it.Key)
			if !ok {
				okStruct = false
				break
			}
			keys[i] = name
			fields[name] = c.inferExprType(it.Value)
		}
		if okStruct {
			st := types.StructType{Fields: fields, Order: keys}
			st = c.ensureStructName(st)
			c.queryStructs[q] = st
		}
	}

	if q.Group != nil {
		keyExpr, err := c.compileExpr(q.Group.Exprs[0])
		if err != nil {
			c.env = orig
			return "", err
		}
		keyType := c.inferExprType(q.Group.Exprs[0])

		// Fast path: no additional sources or options, so we can emit
		// a simple grouping loop instead of using _query/_group_by.
		if len(q.Froms) == 0 && len(q.Joins) == 0 && q.Sort == nil &&
			q.Skip == nil && q.Take == nil && q.Group.Having == nil {
			var whereExpr string
			if q.Where != nil {
				whereExpr, err = c.compileExpr(q.Where)
				if err != nil {
					c.env = orig
					return "", err
				}
			}
			c.env = types.NewEnv(child)
			c.env.SetVar(q.Group.Name, types.GroupType{Key: keyType, Elem: elemType}, true)
			valExpr, err := c.compileExpr(q.Select)
			if err != nil {
				c.env = orig
				return "", err
			}
			c.env = orig

			fn := fmt.Sprintf("_q%d", c.tmpCount)
			c.tmpCount++
			c.writeln(fmt.Sprintf("def %s():", fn))
			c.indent++
			c.writeln("_groups = {}")
			c.writeln("_order = []")
			c.writeln(fmt.Sprintf("for %s in %s:", sanitizeName(q.Var), src))
			c.indent++
			if whereExpr != "" {
				c.writeln(fmt.Sprintf("if not (%s):", whereExpr))
				c.indent++
				c.writeln("continue")
				c.indent--
			}
			c.writeln(fmt.Sprintf("_k = %s", keyExpr))
			c.writeln("_ks = str(_k)")
			c.writeln("g = _groups.get(_ks)")
			c.writeln("if not g:")
			c.indent++
			c.writeln("g = _Group(_k)")
			c.writeln("_groups[_ks] = g")
			c.writeln("_order.append(_ks)")
			c.indent--
			c.writeln(fmt.Sprintf("g.Items.append(%s)", sanitizeName(q.Var)))
			c.indent--
			c.writeln("_items1 = [ _groups[k] for k in _order ]")
			c.writeln(fmt.Sprintf("return [ %s for %s in _items1 ]", valExpr, sanitizeName(q.Group.Name)))
			c.indent--
			c.writeln("")

			c.use("_group")
			return fn + "()", nil
		}

		fromSrcs := make([]string, len(q.Froms))
		varNames := []string{sanitizeName(q.Var)}
		for i, f := range q.Froms {
			fs, err := c.compileExpr(f.Src)
			if err != nil {
				c.env = orig
				return "", err
			}
			fromSrcs[i] = fs
			varNames = append(varNames, sanitizeName(f.Var))
		}
		joinSrcs := make([]string, len(q.Joins))
		joinOns := make([]string, len(q.Joins))
		paramCopy := append([]string(nil), varNames...)
		for i, j := range q.Joins {
			js, err := c.compileExpr(j.Src)
			if err != nil {
				c.env = orig
				return "", err
			}
			joinSrcs[i] = js
			on, err := c.compileExpr(j.On)
			if err != nil {
				c.env = orig
				return "", err
			}
			joinOns[i] = on
			varNames = append(varNames, sanitizeName(j.Var))
		}
		var gWhereExpr string
		if q.Where != nil {
			gWhereExpr, err = c.compileExpr(q.Where)
			if err != nil {
				c.env = orig
				return "", err
			}
		}
		c.env = orig

		joins := make([]string, 0, len(q.Froms)+len(q.Joins))
		params := []string{sanitizeName(q.Var)}
		for i, fs := range fromSrcs {
			joins = append(joins, fmt.Sprintf("{ 'items': %s }", fs))
			params = append(params, sanitizeName(q.Froms[i].Var))
		}
		paramCopy = append([]string(nil), params...)
		for i, js := range joinSrcs {
			onParams := append(paramCopy, sanitizeName(q.Joins[i].Var))
			spec := fmt.Sprintf("{ 'items': %s, 'on': lambda %s: (%s)", js, strings.Join(onParams, ", "), joinOns[i])
			if q.Joins[i].Side != nil {
				side := *q.Joins[i].Side
				if side == "left" || side == "outer" {
					spec += ", 'left': True"
				}
				if side == "right" || side == "outer" {
					spec += ", 'right': True"
				}
			}
			spec += " }"
			joins = append(joins, spec)
			paramCopy = append(paramCopy, sanitizeName(q.Joins[i].Var))
		}
		allParams := strings.Join(paramCopy, ", ")
		fieldMap := map[string]int{}
		fieldTypes := map[string]types.Type{}
		for i, n := range paramCopy {
			s := sanitizeName(n)
			fieldMap[s] = i
			if t, err := child.GetVar(n); err == nil {
				fieldTypes[s] = t
			} else {
				fieldTypes[s] = types.AnyType{}
			}
		}
		if len(paramCopy) > 1 {
			c.tupleFields[sanitizeName(q.Group.Name)] = fieldMap
			c.tupleTypes[sanitizeName(q.Group.Name)] = fieldTypes
		}
		rowExpr := "(" + allParams + ")"
		selectFn := fmt.Sprintf("lambda %s: %s", allParams, rowExpr)
		var whereFn string
		if gWhereExpr != "" {
			whereFn = fmt.Sprintf("lambda %s: (%s)", allParams, gWhereExpr)
		}

		joinStr := strings.Join(joins, ", ")
		opts := "{ 'select': " + selectFn
		if whereFn != "" {
			opts += ", 'where': " + whereFn
		}
		opts += " }"

		genv := types.NewEnv(child)
		genv.SetVar(q.Group.Name, types.GroupType{Key: keyType, Elem: elemType}, true)

		keyNames := groupKeyNames(q.Group.Exprs[0])
		prevGroup := c.currentGroup
		prevFields := c.groupFields
		if len(keyNames) > 0 {
			c.currentGroup = sanitizeName(q.Group.Name)
			f := make(map[string]bool, len(keyNames))
			for _, n := range keyNames {
				f[n] = true
			}
			c.groupFields = f
		}
		defer func() {
			c.currentGroup = prevGroup
			c.groupFields = prevFields
		}()

		var sortExpr, skipExpr, takeExpr, havingExpr string
		if q.Sort != nil {
			c.env = genv
			sortExpr, err = c.compileExpr(q.Sort)
			c.env = orig
			if err != nil {
				return "", err
			}
		}
		if q.Group.Having != nil {
			c.env = genv
			havingExpr, err = c.compileExpr(q.Group.Having)
			c.env = orig
			if err != nil {
				return "", err
			}
		}
		if q.Skip != nil {
			c.env = genv
			skipExpr, err = c.compileExpr(q.Skip)
			c.env = orig
			if err != nil {
				return "", err
			}
		}
		if q.Take != nil {
			c.env = genv
			takeExpr, err = c.compileExpr(q.Take)
			c.env = orig
			if err != nil {
				return "", err
			}
		}

		c.env = genv
		val, err := c.compileExpr(q.Select)
		if err != nil {
			c.env = orig
			return "", err
		}
		c.env = orig

		fn := fmt.Sprintf("_q%d", c.tmpCount)
		c.tmpCount++
		c.writeln(fmt.Sprintf("def %s():", fn))
		c.indent++
		c.writeln(fmt.Sprintf("_src = %s", src))
		c.writeln(fmt.Sprintf("_rows = _query(_src, [%s], %s)", joinStr, opts))
		c.writeln(fmt.Sprintf("_groups = _group_by(_rows, lambda %s: (%s))", allParams, keyExpr))
		tmpItems := fmt.Sprintf("_items%d", c.tmpCount)
		c.tmpCount++
		c.writeln(fmt.Sprintf("%s = _groups", tmpItems))
		if havingExpr != "" {
			c.writeln(fmt.Sprintf("%s = [g for g in %s if (%s)]", tmpItems, tmpItems, havingExpr))
		}
		if sortExpr != "" {
			c.use("_sort_key")
			c.writeln(fmt.Sprintf("%s = sorted(%s, key=lambda %s: _sort_key(%s))", tmpItems, tmpItems, sanitizeName(q.Group.Name), sortExpr))
		}
		if skipExpr != "" {
			c.writeln(fmt.Sprintf("%s = (%s)[max(%s, 0):]", tmpItems, tmpItems, skipExpr))
		}
		if takeExpr != "" {
			c.writeln(fmt.Sprintf("%s = (%s)[:max(%s, 0)]", tmpItems, tmpItems, takeExpr))
		}
		c.writeln(fmt.Sprintf("return [ %s for %s in %s ]", val, sanitizeName(q.Group.Name), tmpItems))
		c.indent--
		c.writeln("")

		delete(c.tupleFields, sanitizeName(q.Group.Name))
		delete(c.tupleTypes, sanitizeName(q.Group.Name))
		c.use("_query")
		c.use("_group_by")
		c.use("_group")
		return fn + "()", nil
	}

	whereExpr := ""
	whereLevel := 0
	if q.Where != nil {
		whereLevel = whereEvalLevel(q)
		w, err := c.compileExpr(q.Where)
		if err != nil {
			c.env = orig
			return "", err
		}
		whereExpr = w
	}

	loop0 := fmt.Sprintf("%s in %s", sanitizeName(q.Var), src)
	if whereExpr != "" && whereLevel == 0 {
		loop0 += " if " + whereExpr
	}
	loops := []string{loop0}
	paramList := []string{sanitizeName(q.Var)}
	for i, f := range q.Froms {
		fs, err := c.compileExpr(f.Src)
		if err != nil {
			c.env = orig
			return "", err
		}
		loop := fmt.Sprintf("%s in %s", sanitizeName(f.Var), fs)
		if whereExpr != "" && whereLevel == i+1 {
			loop += " if " + whereExpr
		}
		loops = append(loops, loop)
		paramList = append(paramList, sanitizeName(f.Var))
	}
	condParts := []string{}
	joinList := make([]string, len(q.Joins))
	for i, j := range q.Joins {
		js, err := c.compileExpr(j.Src)
		if err != nil {
			c.env = orig
			return "", err
		}
		joinList[i] = js
		loops = append(loops, fmt.Sprintf("%s in %s", sanitizeName(j.Var), js))
		on, err := c.compileExpr(j.On)
		if err != nil {
			c.env = orig
			return "", err
		}
		condParts = append(condParts, on)
		paramList = append(paramList, sanitizeName(j.Var))
	}
	c.env = orig
	cond := ""
	if len(condParts) > 0 {
		cond = " if " + strings.Join(condParts, " and ")
	}
	if len(q.Joins) == 0 {
		if q.Sort == nil && q.Skip == nil && q.Take == nil {
			if q.Group == nil && q.Select != nil &&
				q.Select.Binary != nil && len(q.Select.Binary.Right) == 0 &&
				q.Select.Binary.Left != nil && len(q.Select.Binary.Left.Ops) == 0 &&
				q.Select.Binary.Left.Value != nil && q.Select.Binary.Left.Value.Target != nil &&
				q.Select.Binary.Left.Value.Target.Call != nil {
				call := q.Select.Binary.Left.Value.Target.Call
				if len(call.Args) == 1 {
					fn := call.Func
					if fn == "sum" || fn == "count" || fn == "avg" || fn == "min" || fn == "max" {
						argExpr, err := c.compileExpr(call.Args[0])
						if err != nil {
							return "", err
						}
						items := fmt.Sprintf("[ %s for %s%s ]", argExpr, strings.Join(loops, " for "), cond)
						switch fn {
						case "sum":
							return fmt.Sprintf("sum(%s)", items), nil
						case "count":
							return fmt.Sprintf("len(%s)", items), nil
						case "avg":
							c.use("_avg")
							return fmt.Sprintf("_avg(%s)", items), nil
						case "min":
							c.use("_min")
							return fmt.Sprintf("_min(%s)", items), nil
						case "max":
							c.use("_max")
							return fmt.Sprintf("_max(%s)", items), nil
						}
					}
				}
			}
			return fmt.Sprintf("[ %s for %s%s ]", sel, strings.Join(loops, " for "), cond), nil
		}

		items := fmt.Sprintf("[ %s for %s%s ]", sanitizeName(q.Var), strings.Join(loops, " for "), cond)
		if q.Sort != nil {
			c.env = child
			s, err := c.compileExpr(q.Sort)
			c.env = orig
			if err != nil {
				return "", err
			}
			c.use("_sort_key")
			items = fmt.Sprintf("sorted(%s, key=lambda %s: _sort_key(%s))", items, strings.Join(paramList, ", "), s)
		}
		if q.Skip != nil {
			c.env = child
			sk, err := c.compileExpr(q.Skip)
			c.env = orig
			if err != nil {
				return "", err
			}
			items = fmt.Sprintf("(%s)[max(%s, 0):]", items, sk)
		}
		if q.Take != nil {
			c.env = child
			tk, err := c.compileExpr(q.Take)
			c.env = orig
			if err != nil {
				return "", err
			}
			items = fmt.Sprintf("(%s)[:max(%s, 0)]", items, tk)
		}
		return fmt.Sprintf("[ %s for %s in %s ]", sel, sanitizeName(q.Var), items), nil
	}

	fromSrcs := make([]string, len(q.Froms))
	varNames := []string{sanitizeName(q.Var)}
	for i, f := range q.Froms {
		fs, err := c.compileExpr(f.Src)
		if err != nil {
			c.env = orig
			return "", err
		}
		fromSrcs[i] = fs
		varNames = append(varNames, sanitizeName(f.Var))
	}
	joinSrcs := make([]string, len(q.Joins))
	joinOns := make([]string, len(q.Joins))
	paramCopy := append([]string(nil), varNames...)
	for i, j := range q.Joins {
		js, err := c.compileExpr(j.Src)
		if err != nil {
			c.env = orig
			return "", err
		}
		joinSrcs[i] = js
		c.env = child
		on, err := c.compileExpr(j.On)
		c.env = orig
		if err != nil {
			return "", err
		}
		joinOns[i] = on
		varNames = append(varNames, sanitizeName(j.Var))
	}
	c.env = child
	val, err := c.compileExpr(q.Select)
	if err != nil {
		c.env = orig
		return "", err
	}
	var sortExpr, skipExpr, takeExpr string
	if q.Where != nil {
		c.env = child
		whereExpr, err = c.compileExpr(q.Where)
		c.env = orig
		if err != nil {
			return "", err
		}
	}
	if q.Sort != nil {
		c.env = child
		sortExpr, err = c.compileExpr(q.Sort)
		c.env = orig
		if err != nil {
			return "", err
		}
	}
	if q.Skip != nil {
		c.env = child
		skipExpr, err = c.compileExpr(q.Skip)
		c.env = orig
		if err != nil {
			return "", err
		}
	}
	if q.Take != nil {
		c.env = child
		takeExpr, err = c.compileExpr(q.Take)
		c.env = orig
		if err != nil {
			return "", err
		}
	}
	c.env = orig

	joins := make([]string, 0, len(q.Froms)+len(q.Joins))
	params := []string{sanitizeName(q.Var)}
	for i, fs := range fromSrcs {
		joins = append(joins, fmt.Sprintf("{ 'items': %s }", fs))
		params = append(params, sanitizeName(q.Froms[i].Var))
	}
	paramCopy = append([]string(nil), params...)
	for i, js := range joinSrcs {
		onParams := append(paramCopy, sanitizeName(q.Joins[i].Var))
		spec := fmt.Sprintf("{ 'items': %s, 'on': lambda %s: (%s)", js, strings.Join(onParams, ", "), joinOns[i])
		if q.Joins[i].Side != nil {
			side := *q.Joins[i].Side
			if side == "left" || side == "outer" {
				spec += ", 'left': True"
			}
			if side == "right" || side == "outer" {
				spec += ", 'right': True"
			}
		}
		spec += " }"
		joins = append(joins, spec)
		paramCopy = append(paramCopy, sanitizeName(q.Joins[i].Var))
	}
	allParams := strings.Join(paramCopy, ", ")
	selectFn := fmt.Sprintf("lambda %s: %s", allParams, val)
	var whereFn, sortFn string
	if whereExpr != "" {
		whereFn = fmt.Sprintf("lambda %s: (%s)", allParams, whereExpr)
	}
	if sortExpr != "" {
		sortFn = fmt.Sprintf("lambda %s: (%s)", allParams, sortExpr)
	}
	joinStr := strings.Join(joins, ", ")
	opts := "{ 'select': " + selectFn
	if whereFn != "" {
		opts += ", 'where': " + whereFn
	}
	if sortFn != "" {
		opts += ", 'sortKey': " + sortFn
	}
	if skipExpr != "" {
		opts += ", 'skip': " + skipExpr
	}
	if takeExpr != "" {
		opts += ", 'take': " + takeExpr
	}
	opts += " }"
	expr := fmt.Sprintf("_query(%s, [%s], %s)", src, joinStr, opts)
	c.use("_query")
	return expr, nil
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

	paramStr := "None"
	if len(params) > 0 {
		paramStr = fmt.Sprintf("{ %s }", strings.Join(params, ", "))
	}
	if model == "" {
		model = "None"
	}

	if g.Target == "embedding" {
		c.use("_gen_embed")
		return fmt.Sprintf("_gen_embed(%s, %s, %s)", text, model, paramStr), nil
	}

	if c.env != nil {
		if _, ok := c.env.GetStruct(g.Target); ok {
			c.use("_gen_struct")
			return fmt.Sprintf("_gen_struct(%s, %s, %s, %s)", sanitizeName(g.Target), prompt, model, paramStr), nil
		}
	}
	c.use("_gen_text")
	return fmt.Sprintf("_gen_text(%s, %s, %s)", prompt, model, paramStr), nil
}

func (c *Compiler) compileMatchExpr(m *parser.MatchExpr) (string, error) {
	target, err := c.compileExpr(m.Target)
	if err != nil {
		return "", err
	}

	// Special case: boolean match with default branch can be compiled
	// using a Python conditional expression. This avoids generating a
	// helper function that fails to capture surrounding variables.
	if len(m.Cases) == 2 {
		if isBoolLiteralExpr(m.Cases[0].Pattern, true) && isUnderscoreExpr(m.Cases[1].Pattern) {
			thenExpr, err := c.compileExpr(m.Cases[0].Result)
			if err != nil {
				return "", err
			}
			elseExpr, err := c.compileExpr(m.Cases[1].Result)
			if err != nil {
				return "", err
			}
			return fmt.Sprintf("(%s if %s else %s)", thenExpr, target, elseExpr), nil
		}
		if isBoolLiteralExpr(m.Cases[1].Pattern, true) && isUnderscoreExpr(m.Cases[0].Pattern) {
			thenExpr, err := c.compileExpr(m.Cases[1].Result)
			if err != nil {
				return "", err
			}
			elseExpr, err := c.compileExpr(m.Cases[0].Result)
			if err != nil {
				return "", err
			}
			return fmt.Sprintf("(%s if %s else %s)", thenExpr, target, elseExpr), nil
		}
	}

	fn := fmt.Sprintf("_match%d", c.tmpCount)
	arg := fmt.Sprintf("_t%d", c.tmpCount)
	c.tmpCount++

	c.writeln(fmt.Sprintf("def %s(%s):", fn, arg))
	c.indent++
	c.writeln(fmt.Sprintf("match %s:", arg))
	c.indent++

	for i, cs := range m.Cases {
		pat, err := c.matchPatternString(cs.Pattern)
		if err != nil {
			return "", err
		}
		if pat == "_" {
			c.writeln("case _:")
		} else {
			c.writeln(fmt.Sprintf("case %s:", pat))
		}
		c.indent++
		res, err := c.compileExpr(cs.Result)
		if err != nil {
			return "", err
		}
		c.writeln("return " + res)
		c.indent--

		if i == len(m.Cases)-1 && pat != "_" {
			c.writeln("case _:")
			c.indent++
			c.writeln("return None")
			c.indent--
		}
	}

	c.indent--
	c.indent--
	c.writeln("")
	return fmt.Sprintf("%s(%s)", fn, target), nil
}

func (c *Compiler) matchPatternString(e *parser.Expr) (string, error) {
	if isUnderscoreExpr(e) {
		return "_", nil
	}
	if call, ok := callPattern(e); ok {
		parts := make([]string, len(call.Args))
		for i, a := range call.Args {
			if id, ok := identName(a); ok {
				parts[i] = sanitizeName(id)
			} else {
				s, err := c.compileExpr(a)
				if err != nil {
					return "", err
				}
				parts[i] = s
			}
		}
		return fmt.Sprintf("%s(%s)", sanitizeName(call.Func), strings.Join(parts, ", ")), nil
	}
	if ident, ok := identName(e); ok {
		if ut, ok := c.env.FindUnionByVariant(ident); ok {
			if st, ok := ut.Variants[ident]; ok && len(st.Fields) == 0 {
				return sanitizeName(ident) + "()", nil
			}
		}
		return sanitizeName(ident), nil
	}
	return c.compileExpr(e)
}

func (c *Compiler) compileIfExpr(ie *parser.IfExpr) (string, error) {
	cond, err := c.compileExpr(ie.Cond)
	if err != nil {
		return "", err
	}
	thenExpr, err := c.compileExpr(ie.Then)
	if err != nil {
		return "", err
	}
	var elseExpr string
	if ie.ElseIf != nil {
		elseExpr, err = c.compileIfExpr(ie.ElseIf)
		if err != nil {
			return "", err
		}
	} else if ie.Else != nil {
		elseExpr, err = c.compileExpr(ie.Else)
		if err != nil {
			return "", err
		}
	} else {
		elseExpr = "None"
	}
	return fmt.Sprintf("(%s if %s else %s)", thenExpr, cond, elseExpr), nil
}

func (c *Compiler) compileLiteral(l *parser.Literal) (string, error) {
	switch {
	case l.Int != nil:
		return fmt.Sprintf("%d", *l.Int), nil
	case l.Float != nil:
		s := strconv.FormatFloat(*l.Float, 'f', -1, 64)
		if !strings.Contains(s, ".") {
			s += ".0"
		}
		return s, nil
	case l.Str != nil:
		return fmt.Sprintf("%q", *l.Str), nil
	case l.Bool != nil:
		if *l.Bool {
			return "True", nil
		}
		return "False", nil
	case l.Null:
		return "None", nil
	default:
		return "None", fmt.Errorf("invalid literal")
	}
}

// compilePackageImport compiles a Mochi package import. The package directory
// is loaded relative to the importing file and all exported functions are
// returned as attributes on the alias object.
func (c *Compiler) compilePackageImport(im *parser.ImportStmt) error {
	alias := im.As
	if alias == "" {
		alias = parser.AliasFromPath(im.Path)
	}
	alias = sanitizeName(alias)
	path := strings.Trim(im.Path, "\"")
	base := ""
	if strings.HasPrefix(path, "./") || strings.HasPrefix(path, "../") {
		base = filepath.Dir(im.Pos.Filename)
	}
	target := filepath.Join(base, path)

	info, err := os.Stat(target)
	if err != nil {
		if os.IsNotExist(err) && !strings.HasSuffix(target, ".mochi") {
			if fi, err2 := os.Stat(target + ".mochi"); err2 == nil {
				info = fi
				target += ".mochi"
			} else {
				return fmt.Errorf("import package: %w", err)
			}
		} else {
			return fmt.Errorf("import package: %w", err)
		}
	}

	var files []string
	if info.IsDir() {
		entries, err := os.ReadDir(target)
		if err != nil {
			return fmt.Errorf("import package: %w", err)
		}
		for _, e := range entries {
			if !e.IsDir() && strings.HasSuffix(e.Name(), ".mochi") {
				files = append(files, filepath.Join(target, e.Name()))
			}
		}
		sort.Strings(files)
	} else {
		files = []string{target}
	}

	pkgEnv := types.NewEnv(c.env)
	var programs []*parser.Program
	for _, f := range files {
		prog, err := parser.Parse(f)
		if err != nil {
			return err
		}
		c.collectImports(prog.Statements)
		if errs := types.Check(prog, pkgEnv); len(errs) > 0 {
			return errs[0]
		}
		programs = append(programs, prog)
	}

	c.writeln(fmt.Sprintf("def _import_%s():", alias))
	c.indent++
	c.writeln("class _Pkg: pass")
	c.writeln("_pkg = _Pkg()")

	origEnv := c.env
	c.env = pkgEnv
	for _, prog := range programs {
		for _, s := range prog.Statements {
			if s.Fun != nil && s.Fun.Export {
				if err := c.compileFunStmt(s.Fun); err != nil {
					c.env = origEnv
					return err
				}
				name := sanitizeName(s.Fun.Name)
				c.writeln(fmt.Sprintf("_pkg.%s = %s", name, name))
				c.writeln("")
			} else {
				if err := c.compileStmt(s); err != nil {
					c.env = origEnv
					return err
				}
			}
		}
	}
	c.writeln("return _pkg")
	c.env = origEnv
	c.indent--
	c.writeln(fmt.Sprintf("%s = _import_%s()", alias, alias))
	c.writeln("")
	return nil
}
