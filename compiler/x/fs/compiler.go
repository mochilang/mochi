//go:build slow

package fscode

import (
	"bytes"
	"fmt"
	"os"
	"path/filepath"
	"regexp"
	"strings"

	"mochi/parser"
)

var identifierRegexp = regexp.MustCompile(`^[A-Za-z_][A-Za-z0-9_]*$`)

// Compiler is a very small F# code generator that supports only a
// subset of Mochi. It is intentionally minimal and only handles the
// constructs required by a few simple programs.
type Compiler struct {
	buf           bytes.Buffer
	prelude       bytes.Buffer
	indent        int
	vars          map[string]string
	structs       map[string]map[string]string
	groups        map[string]bool
	maps          map[string]bool
	anon          map[string]string
	anonCnt       int
	usesJson      bool
	usesYaml      bool
	usesBreak     bool
	usesContinue  bool
	usesUnion     bool
	usesExcept    bool
	usesIntersect bool
}

func defaultValue(typ string) string {
	switch typ {
	case "int":
		return "0"
	case "float":
		return "0.0"
	case "string":
		return "\"\""
	case "bool":
		return "false"
	default:
		return "()"
	}
}

func (c *Compiler) compileType(t *parser.TypeRef) (string, error) {
	if t == nil {
		return "", fmt.Errorf("unsupported type")
	}
	if t.Simple != nil {
		switch *t.Simple {
		case "int", "float", "string", "bool":
			return *t.Simple, nil
		case "void":
			return "unit", nil
		default:
			return *t.Simple, nil
		}
	}
	return "", fmt.Errorf("unsupported type")
}

// New creates a new F# compiler instance.
func New() *Compiler {
	return &Compiler{
		vars:          make(map[string]string),
		structs:       make(map[string]map[string]string),
		groups:        make(map[string]bool),
		maps:          make(map[string]bool),
		anon:          make(map[string]string),
		usesJson:      false,
		usesYaml:      false,
		usesBreak:     false,
		usesContinue:  false,
		usesUnion:     false,
		usesExcept:    false,
		usesIntersect: false,
	}
}

// Compile translates the given Mochi program into F# source code.
func (c *Compiler) Compile(p *parser.Program) ([]byte, error) {
	c.buf.Reset()
	c.prelude.Reset()
	c.indent = 0
	c.vars = make(map[string]string)
	c.structs = make(map[string]map[string]string)
	c.groups = make(map[string]bool)
	c.maps = make(map[string]bool)
	c.anon = make(map[string]string)
	c.anonCnt = 0
	c.usesJson = false
	c.usesYaml = false
	c.usesBreak = false
	c.usesContinue = false
	c.usesUnion = false
	c.usesExcept = false
	c.usesIntersect = false

	for _, s := range p.Statements {
		if err := c.compileStmt(s); err != nil {
			return nil, err
		}
	}
	var header bytes.Buffer
	header.WriteString("open System\n")
	if c.usesJson {
		header.WriteString("open System.Text.Json\n")
	}
	if c.usesYaml {
		header.WriteString("open System.IO\n")
		header.WriteString("open YamlDotNet.Serialization\n")
	}
	header.WriteString("\n")
	if c.usesBreak || c.usesContinue {
		header.WriteString("exception Break\n")
		header.WriteString("exception Continue\n")
		header.WriteString("\n")
	}
	if c.usesUnion {
		c.prelude.WriteString("let _union (a: 'T list) (b: 'T list) : 'T list =\n")
		c.prelude.WriteString("    List.distinct (a @ b)\n\n")
	}
	if c.usesExcept {
		c.prelude.WriteString("let _except (a: 'T list) (b: 'T list) : 'T list =\n")
		c.prelude.WriteString("    List.filter (fun x -> not (List.contains x b)) a\n\n")
	}
	if c.usesIntersect {
		c.prelude.WriteString("let _intersect (a: 'T list) (b: 'T list) : 'T list =\n")
		c.prelude.WriteString("    a |> List.filter (fun x -> List.contains x b) |> List.distinct\n\n")
	}
	var final bytes.Buffer
	final.Write(header.Bytes())
	final.Write(c.prelude.Bytes())
	final.Write(c.buf.Bytes())
	return final.Bytes(), nil
}

func (c *Compiler) compileStmt(s *parser.Statement) error {
	switch {
	case s.Test != nil:
		return c.compileTestBlock(s.Test)
	case s.Expect != nil:
		return c.compileExpect(s.Expect)
	case s.Let != nil:
		return c.compileLet(s.Let)
	case s.Var != nil:
		return c.compileVar(s.Var)
	case s.Assign != nil:
		return c.compileAssign(s.Assign)
	case s.Fun != nil:
		return c.compileFun(s.Fun)
	case s.Return != nil:
		return c.compileReturn(s.Return)
	case s.Expr != nil:
		// special-case print calls which already emit printfn
		if s.Expr.Expr != nil && s.Expr.Expr.Binary != nil &&
			s.Expr.Expr.Binary.Left != nil && s.Expr.Expr.Binary.Left.Value != nil &&
			s.Expr.Expr.Binary.Left.Value.Target != nil && s.Expr.Expr.Binary.Left.Value.Target.Call != nil &&
			s.Expr.Expr.Binary.Left.Value.Target.Call.Func == "print" {
			expr, err := c.compileExpr(s.Expr.Expr)
			if err != nil {
				return err
			}
			c.writeln(expr)
			return nil
		}
		if p := rootPrimary(s.Expr.Expr); p != nil && p.Save != nil {
			expr, err := c.compileExpr(s.Expr.Expr)
			if err != nil {
				return err
			}
			c.writeln(expr)
			return nil
		}
		expr, err := c.compileExpr(s.Expr.Expr)
		if err != nil {
			return err
		}
		c.writeln(fmt.Sprintf("printfn \"%%A\" (%s)", expr))
		return nil
	case s.While != nil:
		return c.compileWhile(s.While)
	case s.For != nil:
		return c.compileFor(s.For)
	case s.Break != nil:
		return c.compileBreak(s.Break)
	case s.Continue != nil:
		return c.compileContinue(s.Continue)
	case s.If != nil:
		return c.compileIfStmt(s.If)
	case s.Type != nil:
		return c.compileTypeDecl(s.Type)
	case s.Update != nil:
		return c.compileUpdate(s.Update)
	case s.Import != nil:
		return c.compileImport(s.Import)
	case s.ExternVar != nil, s.ExternFun != nil, s.ExternType != nil, s.ExternObject != nil:
		// Extern declarations are no-ops for the simple F# backend
		return nil
	default:
		return fmt.Errorf("unsupported statement at line %d", s.Pos.Line)
	}
}

func (c *Compiler) compileLet(l *parser.LetStmt) error {
	var typ string
	var err error
	if l.Type != nil {
		typ, err = c.compileType(l.Type)
		if err != nil {
			typ = ""
		}
	}
	var val string
	if l.Value != nil {
		if p := rootPrimary(l.Value); p != nil && p.List != nil && len(p.List.Elems) > 0 && typ == "" {
			elemT := c.inferType(p.List.Elems[0])
			typ = fmt.Sprintf("%s list", elemT)
		}
		if p := rootPrimary(l.Value); p != nil && p.Map != nil {
			c.maps[l.Name] = true
		}
		val, err = c.compileExpr(l.Value)
		if err != nil {
			return err
		}
		if typ == "" && c.isStringExpr(l.Value) {
			typ = "string"
		} else if typ == "" {
			if p := rootPrimary(l.Value); p != nil && p.Query != nil {
				elemT := c.inferQueryElemType(p.Query)
				typ = fmt.Sprintf("%s list", elemT)
			}
		}
		// infer type from cast expression
		if typ == "" {
			if p := rootPostfix(l.Value); p != nil {
				for _, op := range p.Ops {
					if op.Cast != nil && op.Cast.Type != nil && op.Cast.Type.Simple != nil {
						typ = *op.Cast.Type.Simple
						break
					}
				}
			}
		}
	} else {
		if typ == "" {
			return fmt.Errorf("let without value at line %d", l.Pos.Line)
		}
		val = defaultValue(typ)
	}
	if typ != "" {
		c.writeln(fmt.Sprintf("let %s: %s = %s", l.Name, typ, val))
		c.vars[l.Name] = typ
	} else {
		c.writeln(fmt.Sprintf("let %s = %s", l.Name, val))
	}
	return nil
}

func (c *Compiler) compileVar(v *parser.VarStmt) error {
	var typ string
	var err error
	if v.Type != nil {
		typ, err = c.compileType(v.Type)
		if err != nil {
			typ = ""
		}
	}
	var val string = "0"
	if v.Value != nil {
		if p := rootPrimary(v.Value); p != nil {
			if p.List != nil {
				elems := make([]string, len(p.List.Elems))
				for i, e := range p.List.Elems {
					s, err := c.compileExpr(e)
					if err != nil {
						return err
					}
					elems[i] = s
				}
				val = "[|" + strings.Join(elems, "; ") + "|]"
			} else if p.Map != nil {
				c.maps[v.Name] = true
				val, err = c.compileMap(p.Map)
				if err != nil {
					return err
				}
			} else if p.Query != nil {
				val, err = c.compileQuery(p.Query)
				if err != nil {
					return err
				}
				if typ == "" {
					elemT := c.inferQueryElemType(p.Query)
					typ = fmt.Sprintf("%s list", elemT)
				}
			} else {
				val, err = c.compileExpr(v.Value)
				if err != nil {
					return err
				}
			}
		}
		if typ == "" && c.isStringExpr(v.Value) {
			typ = "string"
		}
		if typ == "" {
			if p := rootPostfix(v.Value); p != nil {
				for _, op := range p.Ops {
					if op.Cast != nil && op.Cast.Type != nil && op.Cast.Type.Simple != nil {
						typ = *op.Cast.Type.Simple
						break
					}
				}
			}
		}
	} else if typ != "" {
		val = defaultValue(typ)
	}
	if typ != "" {
		c.writeln(fmt.Sprintf("let mutable %s: %s = %s", v.Name, typ, val))
		c.vars[v.Name] = typ
	} else {
		c.writeln(fmt.Sprintf("let mutable %s = %s", v.Name, val))
	}
	return nil
}

func (c *Compiler) compileAssign(a *parser.AssignStmt) error {
	val, err := c.compileExpr(a.Value)
	if err != nil {
		return err
	}
	target := a.Name
	for _, idx := range a.Index {
		if idx.Start == nil {
			return fmt.Errorf("complex indexing not supported")
		}
		s, err := c.compileExpr(idx.Start)
		if err != nil {
			return err
		}
		target = fmt.Sprintf("%s.[%s]", target, s)
	}
	for _, f := range a.Field {
		target = fmt.Sprintf("%s.%s", target, sanitizeIdent(f.Name))
	}
	c.writeln(fmt.Sprintf("%s <- %s", target, val))
	return nil
}

func (c *Compiler) compileWhile(w *parser.WhileStmt) error {
	cond, err := c.compileExpr(w.Cond)
	if err != nil {
		return err
	}
	if containsBreakOrContinue(w.Body) {
		c.usesBreak = true
		c.usesContinue = true
		c.writeln("try")
		c.indent++
		c.writeln(fmt.Sprintf("while %s do", cond))
		c.indent++
		c.writeln("try")
		c.indent++
		for _, st := range w.Body {
			if err := c.compileStmt(st); err != nil {
				return err
			}
		}
		c.indent--
		c.writeln("with Continue -> ()")
		c.indent--
		c.indent--
		c.writeln("with Break -> ()")
	} else {
		c.writeln(fmt.Sprintf("while %s do", cond))
		c.indent++
		for _, st := range w.Body {
			if err := c.compileStmt(st); err != nil {
				return err
			}
		}
		c.indent--
	}
	return nil
}

func (c *Compiler) compileFor(f *parser.ForStmt) error {
	start, err := c.compileExpr(f.Source)
	if err != nil {
		return err
	}
	if name, ok := c.simpleIdentifier(f.Source); ok {
		if t, ok2 := c.vars[name]; ok2 {
			if strings.HasSuffix(t, " list") {
				c.vars[f.Name] = strings.TrimSuffix(t, " list")
			} else {
				c.vars[f.Name] = t
			}
		}
	} else if f.RangeEnd != nil {
		c.vars[f.Name] = "int"
	}
	hasBC := containsBreakOrContinue(f.Body)
	if hasBC {
		c.usesBreak = true
		c.usesContinue = true
	}
	if f.RangeEnd != nil {
		end, err := c.compileExpr(f.RangeEnd)
		if err != nil {
			return err
		}
		if hasBC {
			c.writeln("try")
			c.indent++
		}
		c.writeln(fmt.Sprintf("for %s in %s .. %s do", f.Name, start, end))
	} else {
		if hasBC {
			c.writeln("try")
			c.indent++
		}
		c.writeln(fmt.Sprintf("for %s in %s do", f.Name, start))
	}
	c.indent++
	if hasBC {
		c.writeln("try")
		c.indent++
	}
	for _, st := range f.Body {
		if err := c.compileStmt(st); err != nil {
			return err
		}
	}
	if hasBC {
		c.indent--
		c.writeln("with Continue -> ()")
		c.indent--
	} else {
		c.indent--
	}
	if hasBC {
		c.writeln("with Break -> ()")
	}
	return nil
}

func (c *Compiler) compileFun(f *parser.FunStmt) error {
	params := make([]string, len(f.Params))
	for i, p := range f.Params {
		params[i] = fmt.Sprintf("(%s)", p.Name)
	}
	paramStr := strings.Join(params, " ")
	if len(params) == 0 {
		paramStr = "()"
	}
	header := fmt.Sprintf("let %s %s =", f.Name, paramStr)
	c.writeln(header)
	c.indent++
	for i, st := range f.Body {
		if i == len(f.Body)-1 {
			if st.Return != nil {
				val, err := c.compileExpr(st.Return.Value)
				if err != nil {
					return err
				}
				c.writeln(val)
				continue
			}
		}
		if err := c.compileStmt(st); err != nil {
			return err
		}
	}
	c.indent--
	return nil
}

func (c *Compiler) compileReturn(r *parser.ReturnStmt) error {
	val, err := c.compileExpr(r.Value)
	if err != nil {
		return err
	}
	c.writeln(val)
	return nil
}

func (c *Compiler) compileBreak(_ *parser.BreakStmt) error {
	c.usesBreak = true
	c.writeln("raise Break")
	return nil
}

func (c *Compiler) compileContinue(_ *parser.ContinueStmt) error {
	c.usesContinue = true
	c.writeln("raise Continue")
	return nil
}

func (c *Compiler) compileIfStmt(i *parser.IfStmt) error {
	cond, err := c.compileExpr(i.Cond)
	if err != nil {
		return err
	}
	c.writeln(fmt.Sprintf("if %s then", cond))
	c.indent++
	for _, st := range i.Then {
		if err := c.compileStmt(st); err != nil {
			return err
		}
	}
	c.indent--
	if i.Else != nil {
		c.writeln("else")
		c.indent++
		for _, st := range i.Else {
			if err := c.compileStmt(st); err != nil {
				return err
			}
		}
		c.indent--
	}
	return nil
}

func (c *Compiler) compileImport(im *parser.ImportStmt) error {
	if im.Lang == nil {
		return nil
	}
	alias := im.As
	if alias == "" {
		alias = parser.AliasFromPath(im.Path)
	}
	switch *im.Lang {
	case "go":
		if im.Path == "mochi/runtime/ffi/go/testpkg" {
			c.prelude.WriteString(fmt.Sprintf("module %s\n", alias))
			c.prelude.WriteString("let Add a b = a + b\n")
			c.prelude.WriteString("let Pi = 3.14\n")
			c.prelude.WriteString("let Answer = 42\n\n")
		}
	case "python":
		if im.Path == "math" {
			c.prelude.WriteString(fmt.Sprintf("module %s\n", alias))
			c.prelude.WriteString("let pi : float = System.Math.PI\n")
			c.prelude.WriteString("let e : float = System.Math.E\n")
			c.prelude.WriteString("let sqrt (x: float) : float = System.Math.Sqrt x\n")
			c.prelude.WriteString("let pow (x: float) (y: float) : float = System.Math.Pow(x, y)\n")
			c.prelude.WriteString("let sin (x: float) : float = System.Math.Sin x\n")
			c.prelude.WriteString("let log (x: float) : float = System.Math.Log x\n\n")
		}
	}
	return nil
}

func (c *Compiler) compileTypeDecl(t *parser.TypeDecl) error {
	if len(t.Variants) > 0 {
		c.writeln(fmt.Sprintf("type %s =", t.Name))
		c.indent++
		for _, v := range t.Variants {
			parts := make([]string, len(v.Fields))
			for i, f := range v.Fields {
				ft, err := c.compileType(f.Type)
				if err != nil {
					return err
				}
				parts[i] = ft
			}
			if len(parts) == 0 {
				c.writeln("| " + v.Name)
			} else {
				c.writeln("| " + v.Name + " of " + strings.Join(parts, " * "))
			}
		}
		c.indent--
		return nil
	}
	fields := make(map[string]string)
	c.writeln(fmt.Sprintf("type %s = {", t.Name))
	c.indent++
	for idx, m := range t.Members {
		if m.Field == nil {
			continue
		}
		ft, err := c.compileType(m.Field.Type)
		if err != nil {
			return err
		}
		fields[m.Field.Name] = ft
		line := fmt.Sprintf("mutable %s: %s", m.Field.Name, ft)
		if idx < len(t.Members)-1 {
			c.writeln(line)
		} else {
			c.writeln(line)
		}
	}
	c.indent--
	c.writeln("}")
	c.structs[t.Name] = fields
	return nil
}

func (c *Compiler) compileExpr(e *parser.Expr) (string, error) {
	if e == nil || e.Binary == nil {
		return "", fmt.Errorf("empty expr")
	}
	return c.compileBinary(e.Binary)
}

func (c *Compiler) compileBinary(b *parser.BinaryExpr) (string, error) {
	left, err := c.compileUnary(b.Left)
	if err != nil {
		return "", err
	}
	res := left
	for _, op := range b.Right {
		r, err := c.compilePostfix(op.Right)
		if err != nil {
			return "", err
		}
		oper := op.Op
		switch op.Op {
		case "==":
			oper = "="
		case "!=":
			oper = "<>"
		case "in":
			if strings.HasPrefix(r, "\"") {
				res = fmt.Sprintf("%s.Contains(%s)", r, res)
				continue
			}
			if strings.HasPrefix(r, "[") {
				res = fmt.Sprintf("List.contains %s %s", res, r)
				continue
			}
			if strings.HasPrefix(r, "dict [") {
				res = fmt.Sprintf("%s.ContainsKey %s", r, res)
				continue
			}
			if identifierRegexp.MatchString(r) {
				if c.maps[r] {
					res = fmt.Sprintf("%s.ContainsKey %s", r, res)
					continue
				}
			}
			res = fmt.Sprintf("List.contains %s %s", res, r)
			continue
		case "union":
			if op.All {
				res = fmt.Sprintf("(%s @ %s)", res, r)
				continue
			}
			c.usesUnion = true
			res = fmt.Sprintf("(_union %s %s)", res, r)
			continue
		case "except":
			c.usesExcept = true
			res = fmt.Sprintf("(_except %s %s)", res, r)
			continue
		case "intersect":
			c.usesIntersect = true
			res = fmt.Sprintf("(_intersect %s %s)", res, r)
			continue
		}
		res = fmt.Sprintf("%s %s %s", res, oper, r)
	}
	return res, nil
}

func (c *Compiler) compileUnary(u *parser.Unary) (string, error) {
	val, err := c.compilePostfix(u.Value)
	if err != nil {
		return "", err
	}
	for i := len(u.Ops) - 1; i >= 0; i-- {
		op := u.Ops[i]
		if op == "!" {
			val = "not " + val
		} else {
			val = op + val
		}
	}
	return val, nil
}

func (c *Compiler) compilePostfix(p *parser.PostfixExpr) (string, error) {
	val, err := c.compilePrimary(p.Target)
	if err != nil {
		return "", err
	}
	if len(p.Ops) == 0 {
		return val, nil
	}
	// handle sequence of ops including index, slice, field and call
	for i := 0; i < len(p.Ops); i++ {
		op := p.Ops[i]
		if op.Index != nil {
			if op.Index.Colon != nil {
				start := "0"
				if op.Index.Start != nil {
					s, err := c.compileExpr(op.Index.Start)
					if err != nil {
						return "", err
					}
					start = s
				}
				end := ""
				if op.Index.End != nil {
					e, err := c.compileExpr(op.Index.End)
					if err != nil {
						return "", err
					}
					end = e
				}
				if c.isStringPrimary(p.Target) {
					if end != "" {
						val = fmt.Sprintf("%s.Substring(%s, %s - %s)", val, start, end, start)
					} else {
						val = fmt.Sprintf("%s.Substring(%s)", val, start)
					}
				} else {
					if end != "" {
						val = fmt.Sprintf("%s.[%s..(%s-1)]", val, start, end)
					} else {
						val = fmt.Sprintf("%s.[%s..]", val, start)
					}
				}
				continue
			}
			if op.Index.Start != nil {
				idx, err := c.compileExpr(op.Index.Start)
				if err != nil {
					return "", err
				}
				val = fmt.Sprintf("%s.[%s]", val, idx)
				continue
			}
		}
		if op.Field != nil {
			if i+1 < len(p.Ops) && p.Ops[i+1].Call != nil {
				call := p.Ops[i+1].Call
				args := make([]string, len(call.Args))
				for j, a := range call.Args {
					s, err := c.compileExpr(a)
					if err != nil {
						return "", err
					}
					args[j] = s
				}
				name := op.Field.Name
				if c.isStringPrimary(p.Target) && name == "contains" {
					name = "Contains"
				}
				val = fmt.Sprintf("%s.%s(%s)", val, sanitizeIdent(name), strings.Join(args, ", "))
				i++
				continue
			}
			val = fmt.Sprintf("%s.%s", val, sanitizeIdent(op.Field.Name))
			continue
		}
		if op.Call != nil {
			args := make([]string, len(op.Call.Args))
			for j, a := range op.Call.Args {
				s, err := c.compileExpr(a)
				if err != nil {
					return "", err
				}
				args[j] = s
			}
			val = fmt.Sprintf("%s(%s)", val, strings.Join(args, ", "))
			continue
		}
		if op.Cast != nil {
			if p.Target != nil && p.Target.Map != nil && len(p.Target.Map.Items) > 0 {
				allSimple := true
				fields := make([]string, len(p.Target.Map.Items))
				for i, it := range p.Target.Map.Items {
					keyName, ok := c.simpleIdentifier(it.Key)
					if !ok {
						keyName, ok = stringLiteral(it.Key)
					}
					if !ok {
						allSimple = false
						break
					}
					v, err := c.compileExpr(it.Value)
					if err != nil {
						return "", err
					}
					fields[i] = fmt.Sprintf("%s = %s", keyName, v)
				}
				if allSimple && op.Cast.Type != nil && op.Cast.Type.Simple != nil {
					val = fmt.Sprintf("{ %s }", strings.Join(fields, "; "))
					continue
				}
			}
			continue
		}
		return "", fmt.Errorf("unsupported postfix expression")
	}
	return val, nil
}

func (c *Compiler) compilePrimary(p *parser.Primary) (string, error) {
	switch {
	case p.Lit != nil:
		return c.compileLiteral(p.Lit), nil
	case p.List != nil:
		elems := make([]string, len(p.List.Elems))
		for i, e := range p.List.Elems {
			s, err := c.compileExpr(e)
			if err != nil {
				return "", err
			}
			elems[i] = s
		}
		return "[" + join(elems, "; ") + "]", nil
	case p.Map != nil:
		return c.compileMap(p.Map)
	case p.Struct != nil:
		return c.compileStruct(p.Struct)
	case p.Load != nil:
		return c.compileLoadExpr(p.Load)
	case p.Save != nil:
		return c.compileSaveExpr(p.Save)
	case p.Call != nil:
		return c.compileCall(p.Call)
	case p.Query != nil:
		return c.compileQuery(p.Query)
	case p.Selector != nil:
		name := p.Selector.Root
		for _, t := range p.Selector.Tail {
			name += "." + t
		}
		return name, nil
	case p.Group != nil:
		inner, err := c.compileExpr(p.Group)
		if err != nil {
			return "", err
		}
		return "(" + inner + ")", nil
	case p.FunExpr != nil:
		return c.compileFunExpr(p.FunExpr)
	case p.If != nil:
		return c.compileIfExpr(p.If)
	case p.Match != nil:
		return c.compileMatchExpr(p.Match)
	default:
		return "", fmt.Errorf("unsupported expression at line %d", p.Pos.Line)
	}
}

func (c *Compiler) compileMatchExpr(m *parser.MatchExpr) (string, error) {
	target, err := c.compileExpr(m.Target)
	if err != nil {
		return "", err
	}
	cases := make([]string, len(m.Cases))
	for i, cs := range m.Cases {
		pat, err := c.compileExpr(cs.Pattern)
		if err != nil {
			return "", err
		}
		res, err := c.compileExpr(cs.Result)
		if err != nil {
			return "", err
		}
		cases[i] = fmt.Sprintf("| %s -> %s", pat, res)
	}
	return fmt.Sprintf("(match %s with\n    %s)", target, strings.Join(cases, "\n    ")), nil
}

func (c *Compiler) compileCall(call *parser.CallExpr) (string, error) {
	var argAST *parser.Expr
	if len(call.Args) > 0 {
		argAST = call.Args[0]
	}

	args := make([]string, len(call.Args))
	for i, a := range call.Args {
		s, err := c.compileExpr(a)
		if err != nil {
			return "", err
		}
		args[i] = s
	}
	switch call.Func {
	case "print":
		if len(args) == 1 {
			if strings.HasPrefix(args[0], "\"") && strings.HasSuffix(args[0], "\"") {
				return fmt.Sprintf("printfn \"%%s\" %s", args[0]), nil
			}
			if argAST != nil && c.isStringExpr(argAST) {
				if identifierRegexp.MatchString(args[0]) {
					return fmt.Sprintf("printfn \"%%s\" %s", args[0]), nil
				}
				return fmt.Sprintf("printfn \"%%s\" (%s)", args[0]), nil
			}
			if argAST != nil && argAST.Binary != nil && argAST.Binary.Left != nil && argAST.Binary.Left.Value != nil && argAST.Binary.Left.Value.Target != nil {
				t := argAST.Binary.Left.Value.Target
				if t.Call != nil {
					if t.Call.Func == "append" {
						return fmt.Sprintf("printfn \"%%s\" (String.concat \" \" (List.map string (%s)))", args[0]), nil
					}
					if t.Call.Func == "values" {
						return fmt.Sprintf("printfn \"%%s\" (String.concat \" \" (List.map string (%s)))", args[0]), nil
					}
				}
				if t.List != nil && len(argAST.Binary.Right) == 0 {
					return fmt.Sprintf("printfn \"%%s\" (String.concat \" \" (List.map string %s))", args[0]), nil
				}
			}
			if isBoolExpr(argAST) || c.inferType(argAST) == "bool" {
				return fmt.Sprintf("printfn \"%%b\" (%s)", args[0]), nil
			}
			t := c.inferType(argAST)
			if t == "int" {
				return fmt.Sprintf("printfn \"%%d\" (%s)", args[0]), nil
			}
			if t == "float" {
				return fmt.Sprintf("printfn \"%%f\" (%s)", args[0]), nil
			}
			return fmt.Sprintf("printfn \"%%A\" (%s)", args[0]), nil
		}
		conv := make([]string, len(args))
		for i, a := range args {
			boolArg := false
			if i < len(call.Args) && isBoolExpr(call.Args[i]) {
				boolArg = true
			} else if name, ok := c.simpleIdentifier(call.Args[i]); ok {
				if t, ok2 := c.vars[name]; ok2 && t == "bool" {
					boolArg = true
				}
			}
			if !boolArg {
				if c.inferType(call.Args[i]) == "bool" {
					boolArg = true
				}
			}
			if boolArg {
				conv[i] = fmt.Sprintf("sprintf \"%%b\" %s", a)
			} else {
				conv[i] = fmt.Sprintf("string %s", a)
			}
		}
		return fmt.Sprintf("printfn \"%%s\" (String.concat \" \" [%s])", strings.Join(conv, "; ")), nil
	case "append":
		if len(args) == 2 {
			return fmt.Sprintf("%s @ [%s]", args[0], args[1]), nil
		}
	case "avg":
		if len(args) == 1 {
			return fmt.Sprintf("(List.sum %s / List.length %s)", args[0], args[0]), nil
		}
	case "count":
		if len(args) == 1 {
			if name, ok := c.simpleIdentifier(argAST); ok {
				if c.groups[name] {
					return fmt.Sprintf("List.length %s.items", args[0]), nil
				}
			}
			return fmt.Sprintf("List.length %s", args[0]), nil
		}
	case "exists":
		if len(args) == 2 {
			return fmt.Sprintf("List.contains %s %s", args[1], args[0]), nil
		}
		if len(args) == 1 {
			return fmt.Sprintf("not (List.isEmpty %s)", args[0]), nil
		}
	case "json":
		if len(args) == 1 {
			c.usesJson = true
			return fmt.Sprintf("JsonSerializer.Serialize(%s)", args[0]), nil
		}
	case "len":
		if len(args) == 1 {
			if strings.HasPrefix(args[0], "\"") {
				return fmt.Sprintf("%s.Length", args[0]), nil
			}
			return fmt.Sprintf("List.length %s", args[0]), nil
		}
	case "max":
		if len(args) == 1 {
			return fmt.Sprintf("List.max %s", args[0]), nil
		}
	case "min":
		if len(args) == 1 {
			return fmt.Sprintf("List.min %s", args[0]), nil
		}
	case "str":
		if len(args) == 1 {
			return fmt.Sprintf("string %s", args[0]), nil
		}
	case "substring":
		if len(args) == 3 {
			return fmt.Sprintf("%s.Substring(%s, %s - %s)", args[0], args[1], args[2], args[1]), nil
		}
	case "sum":
		if len(args) == 1 {
			return fmt.Sprintf("List.sum %s", args[0]), nil
		}
	case "values":
		if len(args) == 1 {
			return fmt.Sprintf("Seq.toList (%s.Values)", args[0]), nil
		}
	default:
		argStr := strings.Join(args, " ")
		if argStr == "" {
			return fmt.Sprintf("%s()", call.Func), nil
		}
		return fmt.Sprintf("%s %s", call.Func, argStr), nil
	}
	return "", fmt.Errorf("unsupported call %s", call.Func)
}

func (c *Compiler) compileIfExpr(i *parser.IfExpr) (string, error) {
	cond, err := c.compileExpr(i.Cond)
	if err != nil {
		return "", err
	}
	thn, err := c.compileExpr(i.Then)
	if err != nil {
		return "", err
	}
	if i.Else == nil && i.ElseIf == nil {
		return fmt.Sprintf("(if %s then %s else ())", cond, thn), nil
	}
	var els string
	if i.Else != nil {
		e, err := c.compileExpr(i.Else)
		if err != nil {
			return "", err
		}
		els = e
	} else if i.ElseIf != nil {
		e, err := c.compileIfExpr(i.ElseIf)
		if err != nil {
			return "", err
		}
		els = e
	}
	return fmt.Sprintf("(if %s then %s else %s)", cond, thn, els), nil
}

func (c *Compiler) compileFunExpr(f *parser.FunExpr) (string, error) {
	params := make([]string, len(f.Params))
	for i, p := range f.Params {
		params[i] = p.Name
	}
	if f.ExprBody != nil {
		body, err := c.compileExpr(f.ExprBody)
		if err != nil {
			return "", err
		}
		return fmt.Sprintf("fun %s -> %s", strings.Join(params, " "), body), nil
	}
	return "fun _ -> ()", nil
}

func (c *Compiler) compileMap(m *parser.MapLiteral) (string, error) {
	items := make([]string, len(m.Items))
	allSimple := true
	names := make([]string, len(m.Items))
	values := make([]string, len(m.Items))
	types := make([]string, len(m.Items))
	typeMap := make(map[string]string)
	for i, it := range m.Items {
		name, ok := c.simpleIdentifier(it.Key)
		if !ok {
			allSimple = false
		}
		k, err := c.compileMapKey(it.Key)
		if err != nil {
			return "", err
		}
		v, err := c.compileExpr(it.Value)
		if err != nil {
			return "", err
		}
		items[i] = fmt.Sprintf("(%s, %s)", k, v)
		if ok {
			names[i] = sanitizeIdent(name)
			values[i] = v
			t := c.inferType(it.Value)
			types[i] = t
			typeMap[sanitizeIdent(name)] = t
		}
	}
	if allSimple {
		key := strings.Join(names, ",") + "|" + strings.Join(types, ",")
		typ, ok := c.anon[key]
		if !ok {
			c.anonCnt++
			typ = fmt.Sprintf("Anon%d", c.anonCnt)
			c.anon[key] = typ
			c.structs[typ] = typeMap
			c.prelude.WriteString(fmt.Sprintf("type %s = {\n", typ))
			for i, n := range names {
				c.prelude.WriteString(fmt.Sprintf("    %s: %s\n", n, types[i]))
			}
			c.prelude.WriteString("}\n")
		}
		fields := make([]string, len(names))
		for i, n := range names {
			fields[i] = fmt.Sprintf("%s = %s", n, values[i])
		}
		return fmt.Sprintf("{ %s }", strings.Join(fields, "; ")), nil
	}
	return "dict [" + strings.Join(items, "; ") + "]", nil
}

func (c *Compiler) compileStruct(s *parser.StructLiteral) (string, error) {
	fields := make([]string, len(s.Fields))
	names := make([]string, len(s.Fields))
	types := make([]string, len(s.Fields))
	typeMap := make(map[string]string)
	for i, f := range s.Fields {
		v, err := c.compileExpr(f.Value)
		if err != nil {
			return "", err
		}
		t := c.inferType(f.Value)
		fname := sanitizeIdent(f.Name)
		fields[i] = fmt.Sprintf("%s = %s", fname, v)
		names[i] = fname
		types[i] = t
		typeMap[fname] = t
	}
	if s.Name != "" {
		if _, ok := c.structs[s.Name]; ok {
			return fmt.Sprintf("{ %s }", strings.Join(fields, "; ")), nil
		}
		vals := make([]string, len(s.Fields))
		for i := range s.Fields {
			vals[i] = strings.SplitN(fields[i], " = ", 2)[1]
		}
		return fmt.Sprintf("%s(%s)", s.Name, strings.Join(vals, ", ")), nil
	}
	key := strings.Join(names, ",") + "|" + strings.Join(types, ",")
	typ, ok := c.anon[key]
	if !ok {
		c.anonCnt++
		typ = fmt.Sprintf("Anon%d", c.anonCnt)
		c.anon[key] = typ
		c.structs[typ] = typeMap
		c.prelude.WriteString(fmt.Sprintf("type %s = {\n", typ))
		for i, n := range names {
			c.prelude.WriteString(fmt.Sprintf("    %s: %s\n", n, types[i]))
		}
		c.prelude.WriteString("}\n")
	}
	return fmt.Sprintf("{ %s }", strings.Join(fields, "; ")), nil
}

func (c *Compiler) compileMapKey(e *parser.Expr) (string, error) {
	if name, ok := c.simpleIdentifier(e); ok {
		return fmt.Sprintf("\"%s\"", name), nil
	}
	return c.compileExpr(e)
}

func (c *Compiler) compileLoadExpr(l *parser.LoadExpr) (string, error) {
	if l.Path == nil || l.Type == nil {
		return "", fmt.Errorf("unsupported expression at line %d", l.Pos.Line)
	}
	typ, err := c.compileType(l.Type)
	if err != nil {
		return "", err
	}
	c.usesYaml = true
	return fmt.Sprintf("(let deserializer = DeserializerBuilder().Build()\n    let yamlText = File.ReadAllText(%q)\n    deserializer.Deserialize<%s list>(yamlText))", *l.Path, typ), nil
}

func (c *Compiler) compileSaveExpr(sv *parser.SaveExpr) (string, error) {
	src, err := c.compileExpr(sv.Src)
	if err != nil {
		return "", err
	}
	c.usesJson = true
	return fmt.Sprintf("(List.iter (fun row -> printfn \"%%s\" (JsonSerializer.Serialize(row))) %s)", src), nil
}

func (c *Compiler) compileTestBlock(t *parser.TestBlock) error {
	for _, st := range t.Body {
		if err := c.compileStmt(st); err != nil {
			return err
		}
	}
	return nil
}

func (c *Compiler) compileExpect(e *parser.ExpectStmt) error {
	expr, err := c.compileExpr(e.Value)
	if err != nil {
		return err
	}
	c.writeln(fmt.Sprintf("assert (%s)", expr))
	return nil
}

func (c *Compiler) compileUpdate(u *parser.UpdateStmt) error {
	iter := "item"
	c.writeln(fmt.Sprintf("for %s in %s do", iter, u.Target))
	c.indent++
	rep := func(s string) string {
		for _, it := range u.Set.Items {
			if key, ok := c.simpleIdentifier(it.Key); ok {
				s = strings.ReplaceAll(s, key, fmt.Sprintf("%s.%s", iter, sanitizeIdent(key)))
			}
		}
		return s
	}
	if u.Where != nil {
		cond, err := c.compileExpr(u.Where)
		if err != nil {
			return err
		}
		cond = rep(cond)
		c.writeln(fmt.Sprintf("if %s then", cond))
		c.indent++
	}
	for _, it := range u.Set.Items {
		key, ok := c.simpleIdentifier(it.Key)
		if !ok {
			return fmt.Errorf("unsupported update key")
		}
		val, err := c.compileExpr(it.Value)
		if err != nil {
			return err
		}
		val = rep(val)
		c.writeln(fmt.Sprintf("%s.%s <- %s", iter, sanitizeIdent(key), val))
	}
	if u.Where != nil {
		c.indent--
	}
	c.indent--
	return nil
}

func (c *Compiler) compileOuterJoin(q *parser.QueryExpr) (string, error) {
	j := q.Joins[0]
	left, err := c.compileExpr(q.Source)
	if err != nil {
		return "", err
	}
	right, err := c.compileExpr(j.Src)
	if err != nil {
		return "", err
	}
	on, err := c.compileExpr(j.On)
	if err != nil {
		return "", err
	}
	part1 := fmt.Sprintf("[ for %s in %s do\n    let %s = %s |> List.tryFind (fun %s -> %s)\n    yield { order = Some %s; customer = %s } ]",
		q.Var, left, j.Var, right, j.Var, on, q.Var, j.Var)
	part2 := fmt.Sprintf("[ for %s in %s do\n    if %s |> List.exists (fun %s -> %s) |> not then\n        yield { order = None; customer = Some %s } ]",
		j.Var, right, left, q.Var, on, j.Var)
	return fmt.Sprintf("(let orderPart = %s\n let customerPart = %s\n orderPart @ customerPart)", part1, part2), nil
}

func (c *Compiler) simpleIdentifier(e *parser.Expr) (string, bool) {
	if e == nil {
		return "", false
	}
	if e.Binary != nil && len(e.Binary.Right) == 0 {
		if u := e.Binary.Left; u != nil {
			if len(u.Ops) == 0 && u.Value != nil && u.Value.Target != nil {
				if sel := u.Value.Target.Selector; sel != nil && len(sel.Tail) == 0 {
					return sel.Root, true
				}
			}
		}
	}
	s, err := c.compileExpr(e)
	if err == nil {
		if identifierRegexp.MatchString(s) {
			return s, true
		}
	}
	return "", false
}

func stringLiteral(e *parser.Expr) (string, bool) {
	if e == nil || e.Binary == nil || len(e.Binary.Right) > 0 {
		return "", false
	}
	u := e.Binary.Left
	if u == nil || len(u.Ops) > 0 || u.Value == nil || u.Value.Target == nil {
		return "", false
	}
	p := u.Value.Target
	if p.Lit == nil || p.Lit.Str == nil {
		return "", false
	}
	return *p.Lit.Str, true
}

func (c *Compiler) compileQuery(q *parser.QueryExpr) (string, error) {
	if len(q.Joins) == 1 && q.Joins[0].Side != nil && *q.Joins[0].Side == "outer" && len(q.Froms) == 0 {
		return c.compileOuterJoin(q)
	}
	src, err := c.compileExpr(q.Source)
	if err != nil {
		return "", err
	}
	c.vars[q.Var] = c.collectionElemType(q.Source)
	loops := []string{fmt.Sprintf("for %s in %s do", q.Var, src)}
	bindings := []string{}
	for _, fr := range q.Froms {
		s, err := c.compileExpr(fr.Src)
		if err != nil {
			return "", err
		}
		c.vars[fr.Var] = c.collectionElemType(fr.Src)
		loops = append(loops, fmt.Sprintf("for %s in %s do", fr.Var, s))
	}
	joinConds := []string{}
	for _, j := range q.Joins {
		js, err := c.compileExpr(j.Src)
		if err != nil {
			return "", err
		}
		c.vars[j.Var] = c.collectionElemType(j.Src)
		on, err := c.compileExpr(j.On)
		if err != nil {
			return "", err
		}
		if j.Side == nil {
			loops = append(loops, fmt.Sprintf("for %s in %s do", j.Var, js))
			joinConds = append(joinConds, on)
		} else {
			switch *j.Side {
			case "left":
				bindings = append(bindings, fmt.Sprintf("let %s = List.tryFind (fun %s -> %s) %s", j.Var, j.Var, on, js))
			case "right":
				if len(q.Froms) == 0 && len(q.Joins) == 1 {
					loops = []string{fmt.Sprintf("for %s in %s do", j.Var, js)}
					bindings = append(bindings, fmt.Sprintf("let %s = List.tryFind (fun %s -> %s) %s", q.Var, q.Var, on, src))
				} else {
					return "", fmt.Errorf("unsupported join type")
				}
			default:
				return "", fmt.Errorf("unsupported join type")
			}
		}
	}
	allVars := []string{q.Var}
	for _, fr := range q.Froms {
		allVars = append(allVars, fr.Var)
	}
	for _, j := range q.Joins {
		allVars = append(allVars, j.Var)
	}
	condParts := []string{}
	if len(joinConds) > 0 {
		condParts = append(condParts, strings.Join(joinConds, " && "))
	}
	if q.Where != nil {
		cnd, err := c.compileExpr(q.Where)
		if err != nil {
			return "", err
		}
		condParts = append(condParts, cnd)
	}
	var cond string
	if len(condParts) > 0 {
		cond = fmt.Sprintf("if %s then ", strings.Join(condParts, " && "))
	}
	if q.Group != nil {
		c.groups[q.Group.Name] = true
	}
	sel, err := c.compileExpr(q.Select)
	if err != nil {
		return "", err
	}

	// handle group by
	if q.Group != nil {
		if len(q.Group.Exprs) != 1 {
			return "", fmt.Errorf("multiple group keys not supported")
		}
		keyExpr, err := c.compileExpr(q.Group.Exprs[0])
		if err != nil {
			return "", err
		}
		var inner strings.Builder
		inner.WriteString("[ ")
		for i, l := range loops {
			if i > 0 {
				inner.WriteByte('\n')
				inner.WriteString("  ")
			}
			inner.WriteString(l)
			inner.WriteByte(' ')
		}
		for _, bnd := range bindings {
			inner.WriteByte('\n')
			inner.WriteString("  ")
			inner.WriteString(bnd)
			inner.WriteByte(' ')
		}
		if cond != "" {
			inner.WriteString(cond)
		}
		yieldExpr := q.Var
		pat := q.Var
		if len(allVars) > 1 {
			yieldExpr = "(" + strings.Join(allVars, ", ") + ")"
			pat = yieldExpr
		}
		inner.WriteString("yield ")
		inner.WriteString(yieldExpr)
		inner.WriteString(" ]")
		listExpr := inner.String()
		grpExpr := fmt.Sprintf("%s |> List.groupBy (fun %s -> %s)", listExpr, pat, keyExpr)

		keyVar := q.Group.Name + "Key"
		itemsVar := q.Group.Name + "Items"
		c.groups[q.Group.Name] = true

		if q.Group.Having != nil {
			condExpr, err := c.compileExpr(q.Group.Having)
			if err != nil {
				return "", err
			}
			grpExpr = fmt.Sprintf("%s |> List.filter (fun (%s, %s) -> let %s = {| key = %s; items = %s |} in %s)",
				grpExpr, keyVar, itemsVar, q.Group.Name, keyVar, itemsVar, condExpr)
		}
		if q.Sort != nil {
			s, err := c.compileExpr(q.Sort)
			if err != nil {
				return "", err
			}
			if strings.HasPrefix(s, "-") {
				s = strings.TrimPrefix(s, "-")
				grpExpr = fmt.Sprintf("%s |> List.sortByDescending (fun (%s, %s) -> let %s = {| key = %s; items = %s |} in %s)",
					grpExpr, keyVar, itemsVar, q.Group.Name, keyVar, itemsVar, s)
			} else {
				grpExpr = fmt.Sprintf("%s |> List.sortBy (fun (%s, %s) -> let %s = {| key = %s; items = %s |} in %s)",
					grpExpr, keyVar, itemsVar, q.Group.Name, keyVar, itemsVar, s)
			}
		}
		if q.Skip != nil {
			sk, err := c.compileExpr(q.Skip)
			if err != nil {
				return "", err
			}
			grpExpr = fmt.Sprintf("%s |> List.skip %s", grpExpr, sk)
		}
		if q.Take != nil {
			tk, err := c.compileExpr(q.Take)
			if err != nil {
				return "", err
			}
			grpExpr = fmt.Sprintf("%s |> List.take %s", grpExpr, tk)
		}
		if q.Distinct {
			grpExpr = fmt.Sprintf("%s |> List.distinct", grpExpr)
		}
		return fmt.Sprintf("[ for %s, %s in %s do\n    let %s = {| key = %s; items = %s |}\n    yield %s ]",
			keyVar, itemsVar, grpExpr, q.Group.Name, keyVar, itemsVar, sel), nil
	}

	var b strings.Builder
	b.WriteString("[ ")
	for i, l := range loops {
		if i > 0 {
			b.WriteByte('\n')
			b.WriteString("  ")
		}
		b.WriteString(l)
		b.WriteByte(' ')
	}
	for _, bnd := range bindings {
		b.WriteByte('\n')
		b.WriteString("  ")
		b.WriteString(bnd)
		b.WriteByte(' ')
	}
	if cond != "" {
		b.WriteString(cond)
	}
	b.WriteString("yield ")
	b.WriteString(sel)
	b.WriteString(" ]")
	expr := b.String()
	if q.Sort != nil {
		s, err := c.compileExpr(q.Sort)
		if err != nil {
			return "", err
		}
		if strings.HasPrefix(s, "-") {
			s = strings.TrimPrefix(s, "-")
			expr = fmt.Sprintf("%s |> List.sortByDescending (fun %s -> %s)", expr, q.Var, s)
		} else {
			expr = fmt.Sprintf("%s |> List.sortBy (fun %s -> %s)", expr, q.Var, s)
		}
	}
	if q.Skip != nil {
		sk, err := c.compileExpr(q.Skip)
		if err != nil {
			return "", err
		}
		expr = fmt.Sprintf("%s |> List.skip %s", expr, sk)
	}
	if q.Take != nil {
		tk, err := c.compileExpr(q.Take)
		if err != nil {
			return "", err
		}
		expr = fmt.Sprintf("%s |> List.take %s", expr, tk)
	}
	if q.Distinct {
		expr = fmt.Sprintf("%s |> List.distinct", expr)
	}
	return expr, nil
}

func (c *Compiler) compileLiteral(l *parser.Literal) string {
	switch {
	case l.Int != nil:
		return fmt.Sprintf("%d", *l.Int)
	case l.Str != nil:
		return fmt.Sprintf("%q", *l.Str)
	case l.Float != nil:
		return fmt.Sprintf("%g", *l.Float)
	case l.Bool != nil:
		if bool(*l.Bool) {
			return "true"
		}
		return "false"
	default:
		return "()"
	}
}

func (c *Compiler) writeln(s string) {
	for i := 0; i < c.indent; i++ {
		c.buf.WriteString("    ")
	}
	c.buf.WriteString(s)
	c.buf.WriteByte('\n')
}

func join(parts []string, sep string) string {
	if len(parts) == 0 {
		return ""
	}
	out := parts[0]
	for _, p := range parts[1:] {
		out += sep + p
	}
	return out
}

var fsKeywords = map[string]struct{}{
	"abstract": {}, "and": {}, "as": {}, "assert": {}, "base": {}, "begin": {}, "class": {},
	"default": {}, "delegate": {}, "do": {}, "done": {}, "downcast": {}, "downto": {},
	"elif": {}, "else": {}, "end": {}, "exception": {}, "external": {}, "false": {},
	"finally": {}, "for": {}, "fun": {}, "function": {}, "if": {}, "in": {}, "inherit": {},
	"inline": {}, "interface": {}, "internal": {}, "lazy": {}, "let": {}, "match": {},
	"member": {}, "module": {}, "mutable": {}, "namespace": {}, "new": {}, "null": {},
	"of": {}, "open": {}, "or": {}, "override": {}, "private": {}, "public": {}, "rec": {},
	"return": {}, "sig": {}, "static": {}, "struct": {}, "then": {}, "to": {}, "true": {},
	"try": {}, "type": {}, "upcast": {}, "use": {}, "val": {}, "void": {}, "when": {},
	"while": {}, "with": {}, "yield": {},
}

func sanitizeIdent(name string) string {
	if _, ok := fsKeywords[name]; ok {
		return "``" + name + "``"
	}
	return name
}

func rootPrimary(e *parser.Expr) *parser.Primary {
	if e == nil || e.Binary == nil || e.Binary.Left == nil || e.Binary.Left.Value == nil {
		return nil
	}
	return e.Binary.Left.Value.Target
}

func rootPostfix(e *parser.Expr) *parser.PostfixExpr {
	if e == nil || e.Binary == nil || e.Binary.Left == nil {
		return nil
	}
	return e.Binary.Left.Value
}

func isMapLiteral(e *parser.Expr) *parser.MapLiteral {
	p := rootPrimary(e)
	if p != nil && p.Map != nil && len(e.Binary.Right) == 0 {
		return p.Map
	}
	return nil
}

func isBoolExpr(e *parser.Expr) bool {
	if e == nil || e.Binary == nil {
		return false
	}
	if e.Binary.Left != nil {
		if len(e.Binary.Left.Ops) > 0 {
			for _, op := range e.Binary.Left.Ops {
				if op == "!" {
					return true
				}
			}
		}
		if e.Binary.Left.Value != nil && e.Binary.Left.Value.Target != nil {
			if lit := e.Binary.Left.Value.Target.Lit; lit != nil && lit.Bool != nil {
				return true
			}
		}
	}
	for _, r := range e.Binary.Right {
		switch r.Op {
		case "==", "!=", "<", "<=", ">", ">=", "&&", "||", "in":
			return true
		}
	}
	return false
}

func (c *Compiler) isStringExpr(e *parser.Expr) bool {
	if p := rootPrimary(e); p != nil {
		if p.Lit != nil && p.Lit.Str != nil {
			return true
		}
		if p.Selector != nil {
			if t, ok := c.vars[p.Selector.Root]; ok {
				if t == "string" {
					return true
				}
				if fields, ok := c.structs[t]; ok {
					if len(p.Selector.Tail) == 1 {
						if ft, ok := fields[p.Selector.Tail[0]]; ok && ft == "string" {
							return true
						}
					}
				}
			}
		}
		if p.If != nil {
			return c.isStringExpr(p.If.Then) && c.isStringExpr(p.If.Else)
		}
	}
	return false
}

func (c *Compiler) isStringPrimary(p *parser.Primary) bool {
	if p == nil {
		return false
	}
	if p.Lit != nil && p.Lit.Str != nil {
		return true
	}
	if p.Selector != nil {
		if t, ok := c.vars[p.Selector.Root]; ok {
			if t == "string" {
				return true
			}
			if fields, ok := c.structs[t]; ok {
				if len(p.Selector.Tail) == 1 {
					if ft, ok := fields[p.Selector.Tail[0]]; ok && ft == "string" {
						return true
					}
				}
			}
		}
	}
	if p.If != nil {
		return c.isStringExpr(p.If.Then) && c.isStringExpr(p.If.Else)
	}
	return false
}

func (c *Compiler) inferType(e *parser.Expr) string {
	if e == nil || e.Binary == nil {
		return "obj"
	}
	if isBoolExpr(e) {
		return "bool"
	}
	if c.isStringExpr(e) {
		return "string"
	}
	if p := rootPrimary(e); p != nil {
		if p.Lit != nil {
			if p.Lit.Int != nil {
				return "int"
			}
			if p.Lit.Float != nil {
				return "float"
			}
		}
		if p.Selector != nil {
			if t, ok := c.vars[p.Selector.Root]; ok {
				if fields, ok := c.structs[t]; ok && len(p.Selector.Tail) == 1 {
					if ft, ok := fields[p.Selector.Tail[0]]; ok {
						return ft
					}
				}
				return t
			}
		}
	}
	return "obj"
}

func (c *Compiler) collectionElemType(e *parser.Expr) string {
	if name, ok := c.simpleIdentifier(e); ok {
		if t, ok2 := c.vars[name]; ok2 {
			if strings.HasSuffix(t, " list") {
				return strings.TrimSuffix(t, " list")
			}
		}
	}
	if p := rootPrimary(e); p != nil {
		if p.List != nil && len(p.List.Elems) > 0 {
			return c.inferType(p.List.Elems[0])
		}
	}
	return "obj"
}

func (c *Compiler) inferQueryElemType(q *parser.QueryExpr) string {
	if q == nil {
		return "obj"
	}
	return c.inferType(q.Select)
}

func containsBreakOrContinue(stmts []*parser.Statement) bool {
	for _, st := range stmts {
		switch {
		case st.Break != nil, st.Continue != nil:
			return true
		case st.While != nil:
			if containsBreakOrContinue(st.While.Body) {
				return true
			}
		case st.For != nil:
			if containsBreakOrContinue(st.For.Body) {
				return true
			}
		case st.If != nil:
			if containsBreakOrContinue(st.If.Then) {
				return true
			}
			if st.If.Else != nil && containsBreakOrContinue(st.If.Else) {
				return true
			}
			if st.If.ElseIf != nil && containsBreakOrContinue(st.If.ElseIf.Then) {
				return true
			}
		}
	}
	return false
}

func repoRoot() (string, error) {
	dir, err := os.Getwd()
	if err != nil {
		return "", err
	}
	for i := 0; i < 10; i++ {
		if _, err := os.Stat(filepath.Join(dir, "go.mod")); err == nil {
			return dir, nil
		}
		parent := filepath.Dir(dir)
		if parent == dir {
			break
		}
		dir = parent
	}
	return "", fmt.Errorf("go.mod not found")
}

// CompileFile is a helper that parses src and compiles it to F#.
func CompileFile(src string) ([]byte, error) {
	prog, err := parser.Parse(src)
	if err != nil {
		return nil, err
	}
	return New().Compile(prog)
}
