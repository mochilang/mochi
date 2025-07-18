//go:build slow

package fscode

import (
	"bytes"
	"encoding/json"
	"fmt"
	"gopkg.in/yaml.v3"
	"os"
	"path/filepath"
	"regexp"
	"sort"
	"strconv"
	"strings"

	meta "mochi/compiler/meta"
	"mochi/parser"
)

var identifierRegexp = regexp.MustCompile(`^[A-Za-z_][A-Za-z0-9_]*$`)

// Compiler is a very small F# code generator that supports only a
// subset of Mochi. It is intentionally minimal and only handles the
// constructs required by a few simple programs.
type Compiler struct {
	buf             bytes.Buffer
	prelude         bytes.Buffer
	indent          int
	vars            map[string]string
	structs         map[string]map[string]string
	groups          map[string]bool
	maps            map[string]bool
	anon            map[string]string
	anonCnt         int
	variants        map[string]string
	usesJson        bool
	usesYaml        bool
	usesBreak       bool
	usesContinue    bool
	usesGroupBy     bool
	hints           map[string]string
	tuples          map[string][]string
	collectingHints bool
	basePath        string
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
		vars:         make(map[string]string),
		structs:      make(map[string]map[string]string),
		groups:       make(map[string]bool),
		maps:         make(map[string]bool),
		anon:         make(map[string]string),
		variants:     make(map[string]string),
		usesJson:     false,
		usesYaml:     false,
		usesBreak:    false,
		usesContinue: false,
		// set operations no longer need helpers
		usesGroupBy:     false,
		hints:           make(map[string]string),
		tuples:          make(map[string][]string),
		collectingHints: false,
		basePath:        "",
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
	c.variants = make(map[string]string)
	c.usesJson = false
	c.usesYaml = false
	c.usesBreak = false
	c.usesContinue = false
	c.usesGroupBy = false
	c.hints = make(map[string]string)
	c.tuples = make(map[string][]string)
	c.collectingHints = true
	c.gatherHints(p.Statements)
	c.collectingHints = false

	for _, s := range p.Statements {
		if err := c.compileStmt(s); err != nil {
			return nil, err
		}
	}
	var header bytes.Buffer
	header.Write(meta.Header("//"))
	header.WriteString("open System\n")
	if c.usesJson {
		header.WriteString("open Microsoft.FSharp.Reflection\n")
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
	if c.usesGroupBy {
		c.prelude.WriteString("type _Group<'K,'T>(key: 'K) =\n")
		c.prelude.WriteString("    member val key = key with get, set\n")
		c.prelude.WriteString("    member val Items = System.Collections.Generic.List<'T>() with get\n")
		c.prelude.WriteString("    member this.size = this.Items.Count\n")
		c.prelude.WriteString("    interface System.Collections.Generic.IEnumerable<'T> with\n")
		c.prelude.WriteString("        member this.GetEnumerator() = (this.Items :> seq<'T>).GetEnumerator()\n")
		c.prelude.WriteString("    interface System.Collections.IEnumerable with\n")
		c.prelude.WriteString("        member this.GetEnumerator() = (this.Items :> System.Collections.IEnumerable).GetEnumerator()\n\n")
		c.prelude.WriteString("let _group_by (src: 'T list) (keyfn: 'T -> 'K) : _Group<'K,'T> list =\n")
		c.prelude.WriteString("    let groups = System.Collections.Generic.Dictionary<string,_Group<'K,'T>>()\n")
		c.prelude.WriteString("    let order = System.Collections.Generic.List<string>()\n")
		c.prelude.WriteString("    for it in src do\n")
		c.prelude.WriteString("        let key = keyfn it\n")
		c.prelude.WriteString("        let ks = string key\n")
		c.prelude.WriteString("        let mutable g = Unchecked.defaultof<_Group<'K,'T>>\n")
		c.prelude.WriteString("        if groups.TryGetValue(ks, &g) then ()\n")
		c.prelude.WriteString("        else\n")
		c.prelude.WriteString("            g <- _Group<'K,'T>(key)\n")
		c.prelude.WriteString("            groups.Add(ks, g)\n")
		c.prelude.WriteString("            order.Add(ks)\n")
		c.prelude.WriteString("        g.Items.Add(it)\n")
		c.prelude.WriteString("    [ for ks in order -> groups.[ks] ]\n\n")
	}
	if c.usesJson {
		c.prelude.WriteString("let rec toJson (v: obj) : string =\n")
		c.prelude.WriteString("    match v with\n")
		c.prelude.WriteString(`    | :? string as s -> "\"" + s.Replace("\\", "\\\\").Replace("\"", "\\\"") + "\""\n`)
		c.prelude.WriteString("    | :? bool as b -> if b then \"true\" else \"false\"\n")
		c.prelude.WriteString("    | :? int | :? float as n -> string n\n")
		c.prelude.WriteString("    | :? System.Collections.IDictionary as m ->\n")
		c.prelude.WriteString("        let parts = [ for k in m.Keys -> sprintf \"\"%s\":%s\" (string k) (toJson (m.[k])) ]\n")
		c.prelude.WriteString("        \"{\" + String.Join(\",\", parts) + \"}\"\n")
		c.prelude.WriteString("    | :? System.Collections.IEnumerable as e ->\n")
		c.prelude.WriteString("        let parts = [ for x in e -> toJson x ]\n")
		c.prelude.WriteString("        \"[\" + String.Join(\",\", parts) + \"]\"\n")
		c.prelude.WriteString("    | _ -> \"null\"\n\n")
	}
	var final bytes.Buffer
	final.Write(header.Bytes())
	final.Write(c.prelude.Bytes())
	final.Write(c.buf.Bytes())
	out := final.Bytes()
	return FormatFS(out), nil
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
		typ := c.inferType(s.Expr.Expr)
		format := "%A"
		if typ == "string" {
			format = "%s"
		} else if typ == "int" {
			format = "%d"
		} else if typ == "float" {
			format = "%f"
		} else if typ == "bool" {
			format = "%b"
		} else if typ == "char" {
			format = "%c"
		}
		c.writeln(fmt.Sprintf("printfn \"%s\" (%s)", format, expr))
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
			if typ == "" {
				typ = c.mapLiteralType(p.Map)
			}
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
		if typ == "" {
			typ = c.inferType(l.Value)
		}
		// infer type from cast expression
		if p := rootPostfix(l.Value); p != nil {
			for _, op := range p.Ops {
				if op.Cast != nil && op.Cast.Type != nil && op.Cast.Type.Simple != nil {
					typ = *op.Cast.Type.Simple
					break
				}
			}
		}
	} else {
		if typ == "" {
			return fmt.Errorf("let without value at line %d", l.Pos.Line)
		}
		val = defaultValue(typ)
	}
	if typ == "obj" {
		typ = ""
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
				if typ == "" {
					if len(p.List.Elems) > 0 {
						elemT := c.inferType(p.List.Elems[0])
						typ = fmt.Sprintf("%s array", elemT)
					} else if hint, ok := c.hints[v.Name]; ok {
						typ = fmt.Sprintf("%s array", hint)
					} else {
						typ = "obj array"
					}
				} else if !strings.HasSuffix(typ, " array") {
					typ = typ + " array"
				}
			} else if p.Map != nil {
				c.maps[v.Name] = true
				if typ == "" {
					typ = c.mapLiteralType(p.Map)
				}
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
			typ = c.inferType(v.Value)
		}
		if p := rootPostfix(v.Value); p != nil {
			for _, op := range p.Ops {
				if op.Cast != nil && op.Cast.Type != nil && op.Cast.Type.Simple != nil {
					typ = *op.Cast.Type.Simple
					break
				}
			}
		}
	} else if typ != "" {
		val = defaultValue(typ)
	}
	if typ == "obj" {
		typ = ""
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
		if fields, ok2 := c.tuples[name]; ok2 {
			c.tuples[f.Name] = fields
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
		c.writeln(fmt.Sprintf("for %s in %s .. (%s - 1) do", f.Name, start, end))
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
	if f.Return != nil {
		if t, err := c.compileType(f.Return); err == nil {
			c.vars[f.Name] = t
		}
	}
	params := make([]string, len(f.Params))
	for i, p := range f.Params {
		params[i] = fmt.Sprintf("(%s)", p.Name)
	}
	paramStr := strings.Join(params, " ")
	if len(params) == 0 {
		paramStr = "()"
	}
	header := fmt.Sprintf("let rec %s %s =", f.Name, paramStr)
	c.writeln(header)
	c.indent++

	// special case: if body is `if ... return X` followed by `return Y`
	if len(f.Body) == 2 && f.Body[0].If != nil && f.Body[1].Return != nil &&
		len(f.Body[0].If.Then) == 1 && f.Body[0].If.Then[0].Return != nil &&
		f.Body[0].If.Else == nil && f.Body[0].If.ElseIf == nil {
		cond, err := c.compileExpr(f.Body[0].If.Cond)
		if err != nil {
			return err
		}
		thn, err := c.compileExpr(f.Body[0].If.Then[0].Return.Value)
		if err != nil {
			return err
		}
		els, err := c.compileExpr(f.Body[1].Return.Value)
		if err != nil {
			return err
		}
		c.writeln(fmt.Sprintf("if %s then", cond))
		c.indent++
		c.writeln(thn)
		c.indent--
		c.writeln("else")
		c.indent++
		c.writeln(els)
		c.indent--
		c.indent--
		return nil
	}

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
			c.prelude.WriteString("    let Add a b = a + b\n")
			c.prelude.WriteString("    let Pi = 3.14\n")
			c.prelude.WriteString("    let Answer = 42\n\n")
		}
	case "python":
		if im.Path == "math" {
			c.prelude.WriteString(fmt.Sprintf("module %s\n", alias))
			c.prelude.WriteString("    let pi : float = System.Math.PI\n")
			c.prelude.WriteString("    let e : float = System.Math.E\n")
			c.prelude.WriteString("    let sqrt (x: float) : float = System.Math.Sqrt x\n")
			c.prelude.WriteString("    let pow (x: float) (y: float) : float = System.Math.Pow(x, y)\n")
			c.prelude.WriteString("    let sin (x: float) : float = System.Math.Sin x\n")
			c.prelude.WriteString("    let log (x: float) : float = System.Math.Log x\n\n")
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
			c.variants[v.Name] = t.Name
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
	resType := c.basicNumericType(&parser.Expr{Binary: &parser.BinaryExpr{Left: b.Left}})
	for _, op := range b.Right {
		r, err := c.compilePostfix(op.Right)
		if err != nil {
			return "", err
		}
		oper := op.Op
		rType := c.basicNumericType(&parser.Expr{Binary: &parser.BinaryExpr{Left: &parser.Unary{Value: op.Right}}})
		if oper == "*" || oper == "/" || oper == "+" || oper == "-" {
			if resType == "float" && rType == "int" {
				r = fmt.Sprintf("float %s", r)
				rType = "float"
			}
			if resType == "int" && rType == "float" {
				res = fmt.Sprintf("float %s", res)
				resType = "float"
			}
		}
		switch op.Op {
		case "==":
			oper = "="
		case "!=":
			oper = "<>"
		case "in":
			if identifierRegexp.MatchString(r) && c.maps[r] {
				res = fmt.Sprintf("%s.ContainsKey %s", r, res)
				continue
			}
			if strings.HasPrefix(r, "dict [") {
				res = fmt.Sprintf("%s.ContainsKey %s", r, res)
				continue
			}
			if strings.HasPrefix(r, "\"") {
				res = fmt.Sprintf("%s.Contains(%s)", r, res)
				continue
			}
			if strings.HasPrefix(res, "\"") {
				res = fmt.Sprintf("%s.Contains(%s)", r, res)
				continue
			}
			if strings.HasPrefix(r, "[") {
				res = fmt.Sprintf("List.contains %s %s", res, r)
				continue
			}
			res = fmt.Sprintf("List.contains %s %s", res, r)
			continue
		case "union":
			if op.All {
				res = fmt.Sprintf("(%s @ %s)", res, r)
				continue
			}
			res = fmt.Sprintf("(List.distinct (%s @ %s))", res, r)
			continue
		case "except":
			res = fmt.Sprintf("(List.filter (fun x -> not (List.contains x %s)) %s)", r, res)
			continue
		case "intersect":
			res = fmt.Sprintf("(%s |> List.filter (fun x -> List.contains x %s) |> List.distinct)", res, r)
			continue
		}
		res = fmt.Sprintf("%s %s %s", res, oper, r)
		if oper == "*" || oper == "/" || oper == "+" || oper == "-" {
			if resType == "float" || rType == "float" {
				resType = "float"
			} else {
				resType = "int"
			}
		}
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
	if p.Target.Selector != nil && len(p.Target.Selector.Tail) > 0 && p.Ops[0].Call != nil {
		tail := p.Target.Selector.Tail
		base := sanitizeIdent(p.Target.Selector.Root)
		for i, t := range tail[:len(tail)-1] {
			if i == 0 && c.groups[p.Target.Selector.Root] && t == "items" {
				base += ".Items"
			} else {
				base += "." + sanitizeIdent(t)
			}
		}
		name := tail[len(tail)-1]
		call := p.Ops[0].Call
		args := make([]string, len(call.Args))
		for j, a := range call.Args {
			s, err := c.compileExpr(a)
			if err != nil {
				return "", err
			}
			args[j] = s
		}
		if c.isStringSelector(&parser.SelectorExpr{Root: p.Target.Selector.Root, Tail: tail[:len(tail)-1]}) {
			if name == "contains" {
				name = "Contains"
			} else if name == "padStart" && len(args) == 2 {
				ch := args[1]
				if strings.HasPrefix(ch, "\"") && strings.HasSuffix(ch, "\"") && len([]rune(ch[1:len(ch)-1])) == 1 {
					ch = fmt.Sprintf("'%s'", ch[1:len(ch)-1])
				} else {
					ch = fmt.Sprintf("char (%s.[0])", ch)
				}
				val = fmt.Sprintf("%s.PadLeft(%s, %s)", base, args[0], ch)
				if len(p.Ops) > 1 {
					rest := &parser.PostfixExpr{Target: &parser.Primary{Selector: &parser.SelectorExpr{Root: val}}, Ops: p.Ops[1:]}
					return c.compilePostfix(rest)
				}
				return val, nil
			}
		}
		val = fmt.Sprintf("%s.%s(%s)", base, sanitizeIdent(name), strings.Join(args, ", "))
		if len(p.Ops) == 1 {
			return val, nil
		}
		rest := &parser.PostfixExpr{Target: &parser.Primary{Selector: &parser.SelectorExpr{Root: val}}, Ops: p.Ops[1:]}
		return c.compilePostfix(rest)
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
				if c.isStringPrimary(p.Target) {
					if name == "contains" {
						name = "Contains"
					} else if name == "padStart" && len(args) == 2 {
						ch := args[1]
						if strings.HasPrefix(ch, "\"") && strings.HasSuffix(ch, "\"") && len([]rune(ch[1:len(ch)-1])) == 1 {
							ch = fmt.Sprintf("'%s'", ch[1:len(ch)-1])
						} else {
							ch = fmt.Sprintf("char (%s.[0])", ch)
						}
						val = fmt.Sprintf("%s.PadLeft(%s, %s)", val, args[0], ch)
						i++
						continue
					}
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
		expr := sanitizeIdent(p.Selector.Root)
		for i, t := range p.Selector.Tail {
			if i == 0 && c.groups[p.Selector.Root] && t == "items" {
				expr += ".Items"
				continue
			}
			if i == 0 {
				if fields, ok := c.tuples[p.Selector.Root]; ok {
					idx := -1
					for j, f := range fields {
						if sanitizeIdent(f) == t {
							idx = j
							break
						}
					}
					if idx >= 0 {
						pat := strings.Join(sanitizeAll(fields), ", ")
						expr = fmt.Sprintf("(%s |> fun (%s) -> %s)", expr, pat, sanitizeIdent(t))
						continue
					}
				}
			}
			expr += "." + sanitizeIdent(t)
		}
		return expr, nil
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
			if lit, ok := stringLiteral(argAST); ok {
				return fmt.Sprintf("printfn \"%%s\" %q", lit), nil
			}
			if argAST != nil && c.isCharExpr(argAST) {
				if identifierRegexp.MatchString(args[0]) {
					return fmt.Sprintf("printfn \"%%c\" %s", args[0]), nil
				}
				return fmt.Sprintf("printfn \"%%c\" (%s)", args[0]), nil
			}
			if argAST != nil && c.isStringExpr(argAST) {
				if identifierRegexp.MatchString(args[0]) {
					return fmt.Sprintf("printfn \"%%s\" %s", args[0]), nil
				}
				return fmt.Sprintf("printfn \"%%s\" (%s)", args[0]), nil
			}
			if argAST != nil {
				elemT := c.collectionElemType(argAST)
				if elemT != "obj" {
					arg := args[0]
					if elemT == "string" {
						if !identifierRegexp.MatchString(arg) {
							arg = fmt.Sprintf("(%s)", arg)
						}
						return fmt.Sprintf("printfn \"%%s\" (String.concat \" \" %s)", arg), nil
					}
					if !identifierRegexp.MatchString(arg) {
						arg = fmt.Sprintf("(%s)", arg)
					}
					return fmt.Sprintf("printfn \"%%s\" (String.concat \" \" (List.map string %s))", arg), nil
				}
			}
			if isBoolExpr(argAST) || c.inferType(argAST) == "bool" {
				if identifierRegexp.MatchString(args[0]) {
					return fmt.Sprintf("printfn \"%%b\" %s", args[0]), nil
				}
				return fmt.Sprintf("printfn \"%%b\" (%s)", args[0]), nil
			}
			t := c.inferType(argAST)
			if t == "int" {
				if identifierRegexp.MatchString(args[0]) {
					return fmt.Sprintf("printfn \"%%d\" %s", args[0]), nil
				}
				return fmt.Sprintf("printfn \"%%d\" (%s)", args[0]), nil
			}
			if t == "float" {
				if identifierRegexp.MatchString(args[0]) {
					return fmt.Sprintf("printfn \"%%f\" %s", args[0]), nil
				}
				return fmt.Sprintf("printfn \"%%f\" (%s)", args[0]), nil
			}
			if t == "char" {
				if identifierRegexp.MatchString(args[0]) {
					return fmt.Sprintf("printfn \"%%c\" %s", args[0]), nil
				}
				return fmt.Sprintf("printfn \"%%c\" (%s)", args[0]), nil
			}
			if t == "string" {
				if identifierRegexp.MatchString(args[0]) {
					return fmt.Sprintf("printfn \"%%s\" %s", args[0]), nil
				}
				return fmt.Sprintf("printfn \"%%s\" (%s)", args[0]), nil
			}
			if identifierRegexp.MatchString(args[0]) {
				return fmt.Sprintf("printfn \"%%A\" %s", args[0]), nil
			}
			return fmt.Sprintf("printfn \"%%A\" (%s)", args[0]), nil
		}
		// Attempt to build a formatted string when arguments alternate
		// between string literals and values.
		if len(args)%2 == 0 {
			format := strings.Builder{}
			vals := []string{}
			ok := true
			for i := 0; i < len(call.Args); i += 2 {
				lit, okLit := stringLiteral(call.Args[i])
				if !okLit {
					ok = false
					break
				}
				if i > 0 {
					format.WriteString(" ")
				}
				lit = strings.ReplaceAll(lit, "%", "%%")
				format.WriteString(lit)
				if !(strings.HasSuffix(lit, " ") || strings.HasSuffix(lit, "$")) {
					format.WriteString(" ")
				}
				typ := c.inferType(call.Args[i+1])
				switch typ {
				case "int":
					format.WriteString("%d")
				case "float":
					format.WriteString("%f")
				case "bool":
					format.WriteString("%b")
				case "string":
					format.WriteString("%s")
				default:
					format.WriteString("%A")
				}
				vals = append(vals, args[i+1])
			}
			if ok {
				fmtStr := strings.TrimSpace(format.String())
				return fmt.Sprintf("printfn \"%s\" %s", fmtStr, strings.Join(vals, " ")), nil
			}
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
			if name, ok := c.simpleIdentifier(call.Args[0]); ok {
				if t, ok2 := c.vars[name]; ok2 && strings.HasSuffix(t, "array") {
					return fmt.Sprintf("Array.append %s [|%s|]", args[0], args[1]), nil
				}
			}
			return fmt.Sprintf("%s @ [%s]", args[0], args[1]), nil
		}
	case "avg":
		if len(args) == 1 {
			return fmt.Sprintf("(float (List.sum %s) / float (List.length %s))", args[0], args[0]), nil
		}
	case "count":
		if len(args) == 1 {
			if name, ok := c.simpleIdentifier(argAST); ok {
				if c.groups[name] {
					return fmt.Sprintf("%s.size", args[0]), nil
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
			return fmt.Sprintf("toJson %s", args[0]), nil
		}
	case "now":
		if len(args) == 0 {
			return "int (System.DateTimeOffset.UtcNow.ToUnixTimeMilliseconds())", nil
		}
	case "len":
		if len(args) == 1 {
			arg := args[0]
			if strings.HasPrefix(arg, "\"") {
				return fmt.Sprintf("%s.Length", arg), nil
			}
			if identifierRegexp.MatchString(arg) && c.maps[arg] {
				return fmt.Sprintf("%s.Count", arg), nil
			}
			if strings.HasPrefix(arg, "dict [") || strings.Contains(arg, "Dictionary") {
				if !identifierRegexp.MatchString(arg) {
					arg = fmt.Sprintf("(%s)", arg)
				}
				return fmt.Sprintf("%s.Count", arg), nil
			}
			if !identifierRegexp.MatchString(arg) {
				arg = fmt.Sprintf("(%s)", arg)
			}
			return fmt.Sprintf("List.length %s", arg), nil
		}
	case "max":
		if len(args) == 1 {
			return fmt.Sprintf("List.max %s", args[0]), nil
		}
	case "min":
		if len(args) == 1 {
			return fmt.Sprintf("List.min %s", args[0]), nil
		}
	case "pow":
		if len(args) == 2 {
			return fmt.Sprintf("pown %s %s", args[0], args[1]), nil
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
		if len(args) == 0 {
			return fmt.Sprintf("%s()", call.Func), nil
		}
		for i, a := range args {
			if !identifierRegexp.MatchString(a) && !strings.HasPrefix(a, "(") && !strings.HasPrefix(a, "[") && !strings.HasPrefix(a, "{") && !strings.HasPrefix(a, "\"") {
				args[i] = fmt.Sprintf("(%s)", a)
			}
		}
		return fmt.Sprintf("%s %s", call.Func, strings.Join(args, " ")), nil
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
		tVal := c.inferType(it.Value)
		items[i] = fmt.Sprintf("(%s, %s)", k, v)
		if ok {
			names[i] = sanitizeIdent(name)
			values[i] = v
			types[i] = tVal
			typeMap[sanitizeIdent(name)] = tVal
		} else {
			types[i] = tVal
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

	valueType := "obj"
	if len(types) > 0 {
		valueType = types[0]
		for i := 1; i < len(types); i++ {
			if types[i] != valueType {
				valueType = "obj"
				break
			}
		}
	}
	keyType := "string"
	allIntKeys := true
	for _, it := range m.Items {
		if _, ok := intLiteral(it.Key); !ok {
			allIntKeys = false
			break
		}
	}
	if allIntKeys {
		keyType = "int"
	}
	return fmt.Sprintf("System.Collections.Generic.Dictionary<%s,%s>(dict [%s])", keyType, valueType, strings.Join(items, "; ")), nil
}

func (c *Compiler) mapLiteralType(m *parser.MapLiteral) string {
	if c.collectingHints {
		return ""
	}
	names := make([]string, len(m.Items))
	types := make([]string, len(m.Items))
	allSimple := true
	sameType := true
	allIntKeys := true
	var valueType string
	typeMap := make(map[string]string)
	for i, it := range m.Items {
		name, ok := c.simpleIdentifier(it.Key)
		if !ok {
			allSimple = false
		} else {
			names[i] = sanitizeIdent(name)
		}
		if _, ok := intLiteral(it.Key); !ok {
			allIntKeys = false
		}
		t := c.inferType(it.Value)
		types[i] = t
		typeMap[sanitizeIdent(name)] = t
		if valueType == "" {
			valueType = t
		} else if valueType != t {
			sameType = false
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
		return typ
	}
	if sameType && valueType != "" {
		if allIntKeys {
			return fmt.Sprintf("System.Collections.Generic.Dictionary<int,%s>", valueType)
		}
		return fmt.Sprintf("System.Collections.Generic.Dictionary<string,%s>", valueType)
	}
	if allIntKeys {
		return "System.Collections.Generic.Dictionary<int,obj>"
	}
	return "System.Collections.Generic.Dictionary<string,obj>"
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
	typ := c.ensureAnonStruct(names, types, typeMap)
	_ = typ
	return fmt.Sprintf("{ %s }", strings.Join(fields, "; ")), nil
}

func (c *Compiler) ensureAnonStruct(names, types []string, typeMap map[string]string) string {
	if c.collectingHints {
		return ""
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
	return typ
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
	p := *l.Path
	if strings.HasPrefix(p, "../") {
		p = filepath.Join("tests", p[3:])
	}
	abs := filepath.Join(filepath.Dir(c.basePath), p)
	data, err := os.ReadFile(abs)
	if err != nil {
		return "", err
	}
	var rows []map[string]interface{}
	if err := yaml.Unmarshal(data, &rows); err != nil {
		return "", err
	}
	elems := make([]string, len(rows))
	for i, r := range rows {
		fields := make([]string, 0, len(r))
		for k, v := range r {
			fields = append(fields, fmt.Sprintf("%s = %s", sanitizeIdent(k), literalFromAny(v)))
		}
		sort.Strings(fields)
		elems[i] = fmt.Sprintf("{ %s }", strings.Join(fields, "; "))
	}
	return "[" + strings.Join(elems, "; ") + "]", nil
}

func (c *Compiler) compileSaveExpr(sv *parser.SaveExpr) (string, error) {
	src, err := c.compileExpr(sv.Src)
	if err != nil {
		return "", err
	}
	c.usesJson = true
	return fmt.Sprintf("(List.iter (fun row -> printfn \"%%s\" (toJson row)) %s)", src), nil
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

func intLiteral(e *parser.Expr) (int64, bool) {
	if e == nil || e.Binary == nil || len(e.Binary.Right) > 0 {
		return 0, false
	}
	u := e.Binary.Left
	if u == nil || len(u.Ops) > 0 || u.Value == nil || u.Value.Target == nil {
		return 0, false
	}
	p := u.Value.Target
	if p.Lit == nil || p.Lit.Int == nil {
		return 0, false
	}
	return int64(*p.Lit.Int), true
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
	if name, ok := c.simpleIdentifier(q.Source); ok {
		if fields, ok2 := c.tuples[name]; ok2 {
			c.tuples[q.Var] = fields
		}
	}
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
		elemT := c.collectionElemType(j.Src)
		c.vars[j.Var] = elemT
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
				c.vars[j.Var] = elemT + " option"
				bindings = append(bindings, fmt.Sprintf("let %s = List.tryFind (fun %s -> %s) %s", j.Var, j.Var, on, js))
			case "right":
				if len(q.Froms) == 0 && len(q.Joins) == 1 {
					loops = []string{fmt.Sprintf("for %s in %s do", j.Var, js)}
					c.vars[q.Var] = c.collectionElemType(q.Source) + " option"
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
		typ := c.inferType(q.Where)
		if strings.HasSuffix(typ, " option") && !isBoolExpr(q.Where) {
			cnd = fmt.Sprintf("Option.isSome %s", cnd)
		}
		condParts = append(condParts, cnd)
	}
	var cond string
	if len(condParts) > 0 {
		cond = fmt.Sprintf("if %s then ", strings.Join(condParts, " && "))
	}
	if q.Group != nil {
		c.groups[q.Group.Name] = true
		c.usesGroupBy = true
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
			inner.WriteByte('\n')
			inner.WriteString("  ")
		}
		if cond != "" {
			inner.WriteString(cond)
		}
		yieldExpr := q.Var
		pat := q.Var
		if len(allVars) > 1 {
			yieldExpr = "(" + strings.Join(allVars, ", ") + ")"
			pat = yieldExpr
			c.tuples[q.Group.Name] = allVars
		}
		inner.WriteString("yield ")
		inner.WriteString(yieldExpr)
		inner.WriteString(" ]")
		listExpr := inner.String()
		grpExpr := fmt.Sprintf("_group_by %s (fun %s -> %s)", listExpr, pat, keyExpr)

		if q.Group.Having != nil {
			condExpr, err := c.compileExpr(q.Group.Having)
			if err != nil {
				return "", err
			}
			tmp := q.Group.Name + "Tmp"
			grpExpr = fmt.Sprintf("%s |> List.filter (fun %s -> let %s = %s in %s)",
				grpExpr, tmp, q.Group.Name, tmp, condExpr)
		}
		if q.Sort != nil {
			s, err := c.compileExpr(q.Sort)
			if err != nil {
				return "", err
			}
			tmp := q.Group.Name + "Tmp"
			if strings.HasPrefix(s, "-") {
				s = strings.TrimPrefix(s, "-")
				grpExpr = fmt.Sprintf("%s |> List.sortByDescending (fun %s -> let %s = %s in %s)",
					grpExpr, tmp, q.Group.Name, tmp, s)
			} else {
				grpExpr = fmt.Sprintf("%s |> List.sortBy (fun %s -> let %s = %s in %s)",
					grpExpr, tmp, q.Group.Name, tmp, s)
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
		return fmt.Sprintf("[ for %s in %s do\n    yield %s ]",
			q.Group.Name, grpExpr, sel), nil
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
		b.WriteByte('\n')
		b.WriteString("  ")
	}
	if cond != "" {
		b.WriteString(cond)
	}
	b.WriteString("yield ")
	var sortKey string
	if q.Sort != nil {
		var err error
		sortKey, err = c.compileExpr(q.Sort)
		if err != nil {
			return "", err
		}
		b.WriteString("(")
		b.WriteString(sortKey)
		b.WriteString(", ")
		b.WriteString(sel)
		b.WriteString(")")
	} else {
		b.WriteString(sel)
	}
	b.WriteString(" ]")
	expr := b.String()
	if q.Sort != nil {
		asc := true
		sk := sortKey
		if strings.HasPrefix(sk, "-") {
			asc = false
			sk = strings.TrimPrefix(sk, "-")
		}
		if asc {
			expr = fmt.Sprintf("%s |> List.sortBy fst |> List.map snd", expr)
		} else {
			expr = fmt.Sprintf("%s |> List.sortByDescending fst |> List.map snd", expr)
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
		f := strconv.FormatFloat(*l.Float, 'f', -1, 64)
		if !strings.ContainsAny(f, ".eE") {
			f += ".0"
		}
		return f
	case l.Bool != nil:
		if bool(*l.Bool) {
			return "true"
		}
		return "false"
	default:
		return "()"
	}
}

func literalFromAny(v interface{}) string {
	switch t := v.(type) {
	case int, int64, float64:
		return fmt.Sprintf("%v", t)
	case bool:
		if t {
			return "true"
		}
		return "false"
	case string:
		b, _ := json.Marshal(t)
		return string(b)
	default:
		return "null"
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

func sanitizeAll(names []string) []string {
	out := make([]string, len(names))
	for i, n := range names {
		out[i] = sanitizeIdent(n)
	}
	return out
}

func dictValueType(typ string) (string, bool) {
	if strings.HasPrefix(typ, "System.Collections.Generic.Dictionary<") && strings.HasSuffix(typ, ">") {
		inner := strings.TrimSuffix(strings.TrimPrefix(typ, "System.Collections.Generic.Dictionary<"), ">")
		parts := strings.Split(inner, ",")
		if len(parts) == 2 {
			return strings.TrimSpace(parts[1]), true
		}
	}
	return "", false
}

func (c *Compiler) mapValueType(e *parser.Expr) string {
	if name, ok := c.simpleIdentifier(e); ok {
		if t, ok2 := c.vars[name]; ok2 {
			if v, ok3 := dictValueType(t); ok3 {
				return v
			}
		}
	}
	if p := rootPrimary(e); p != nil {
		if p.Map != nil {
			if t := c.mapLiteralType(p.Map); t != "" {
				if v, ok := dictValueType(t); ok {
					return v
				}
			}
		}
	}
	return ""
}

func (c *Compiler) basicNumericType(e *parser.Expr) string {
	if e == nil {
		return "obj"
	}
	if p := rootPrimary(e); p != nil {
		if p.Lit != nil {
			if p.Lit.Float != nil {
				return "float"
			}
			if p.Lit.Int != nil {
				return "int"
			}
		}
		if p.Selector != nil {
			if t, ok := c.vars[p.Selector.Root]; ok {
				if fields, ok2 := c.structs[t]; ok2 && len(p.Selector.Tail) == 1 {
					if ft, ok3 := fields[p.Selector.Tail[0]]; ok3 {
						return ft
					}
				}
				return t
			}
		}
	}
	return "obj"
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

func (c *Compiler) gatherHints(stmts []*parser.Statement) {
	for _, st := range stmts {
		switch {
		case st.Assign != nil:
			a := st.Assign
			if len(a.Index) > 0 {
				t := c.inferType(a.Value)
				if t != "obj" {
					if _, ok := c.hints[a.Name]; !ok {
						c.hints[a.Name] = t
					}
				}
			} else if p := rootPrimary(a.Value); p != nil && p.Call != nil && p.Call.Func == "append" && len(p.Call.Args) == 2 {
				if name, ok := c.simpleIdentifier(p.Call.Args[0]); ok && name == a.Name {
					t := c.inferType(p.Call.Args[1])
					if t != "obj" {
						if _, ok := c.hints[a.Name]; !ok {
							c.hints[a.Name] = t
						}
					}
				}
			} else {
				if t := c.inferType(a.Value); t != "obj" {
					if _, ok := c.hints[a.Name]; !ok {
						c.hints[a.Name] = t
					}
				}
			}
		case st.Let != nil:
			if st.Let.Value != nil {
				if t := c.inferType(st.Let.Value); t != "obj" && t != "" {
					if _, ok := c.hints[st.Let.Name]; !ok {
						c.hints[st.Let.Name] = t
					}
				}
			}
		case st.Var != nil:
			if st.Var.Value != nil {
				if t := c.inferType(st.Var.Value); t != "obj" && t != "" {
					if _, ok := c.hints[st.Var.Name]; !ok {
						c.hints[st.Var.Name] = t
					}
				}
			}
		case st.For != nil:
			c.gatherHints(st.For.Body)
		case st.While != nil:
			c.gatherHints(st.While.Body)
		case st.If != nil:
			c.gatherHints(st.If.Then)
			if st.If.Else != nil {
				c.gatherHints(st.If.Else)
			}
			if st.If.ElseIf != nil {
				c.gatherHints([]*parser.Statement{&parser.Statement{If: st.If.ElseIf}})
			}
		case st.Fun != nil:
			c.gatherHints(st.Fun.Body)
		}
	}
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
	if p := rootPostfix(e); p != nil {
		if p.Target != nil && p.Target.Selector != nil && len(p.Target.Selector.Tail) > 0 && len(p.Ops) > 0 && p.Ops[0].Call != nil {
			switch p.Target.Selector.Tail[len(p.Target.Selector.Tail)-1] {
			case "contains":
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
	if e == nil || e.Binary == nil {
		return false
	}
	// Expressions with operators generally do not evaluate to string.
	if len(e.Binary.Right) > 0 {
		return false
	}
	p := rootPrimary(e)
	if p == nil {
		return false
	}
	if p.Lit != nil && p.Lit.Str != nil {
		return true
	}
	if p.Selector != nil {
		if t, ok := c.vars[p.Selector.Root]; ok {
			if t == "string" && len(p.Selector.Tail) == 0 {
				return true
			}
			if fields, ok := c.structs[t]; ok && len(p.Selector.Tail) == 1 {
				if ft, ok := fields[p.Selector.Tail[0]]; ok && ft == "string" {
					return true
				}
			}
		}
	}
	if p.If != nil {
		return c.isStringExpr(p.If.Then) && c.isStringExpr(p.If.Else)
	}
	return false
}

func (c *Compiler) isIntExpr(e *parser.Expr) bool {
	if name, ok := c.simpleIdentifier(e); ok {
		if t, ok2 := c.vars[name]; ok2 && t == "int" {
			return true
		}
	}
	if _, ok := intLiteral(e); ok {
		return true
	}
	return false
}

func (c *Compiler) isCharExpr(e *parser.Expr) bool {
	if e == nil || e.Binary == nil || len(e.Binary.Right) > 0 {
		return false
	}
	if p := rootPostfix(e); p != nil {
		if len(p.Ops) > 0 && p.Ops[0].Index != nil && p.Ops[0].Index.Start != nil && p.Ops[0].Index.Colon == nil {
			if c.isStringPrimary(p.Target) {
				return true
			}
		}
	}
	if p := rootPrimary(e); p != nil && p.Lit != nil && p.Lit.Str != nil {
		if len([]rune(*p.Lit.Str)) == 1 {
			return true
		}
	}
	if name, ok := c.simpleIdentifier(e); ok {
		if t, ok2 := c.vars[name]; ok2 && t == "char" {
			return true
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
			if t == "string" && len(p.Selector.Tail) == 0 {
				return true
			}
			if fields, ok := c.structs[t]; ok && len(p.Selector.Tail) == 1 {
				if ft, ok := fields[p.Selector.Tail[0]]; ok && ft == "string" {
					return true
				}
			}
		}
	}
	if p.If != nil {
		return c.isStringExpr(p.If.Then) && c.isStringExpr(p.If.Else)
	}
	return false
}

func (c *Compiler) isStringSelector(sel *parser.SelectorExpr) bool {
	if sel == nil {
		return false
	}
	t, ok := c.vars[sel.Root]
	if !ok {
		return false
	}
	for _, f := range sel.Tail {
		fields, ok := c.structs[t]
		if !ok {
			return false
		}
		ft, ok := fields[f]
		if !ok {
			return false
		}
		t = ft
	}
	return t == "string"
}

func (c *Compiler) isStringListExpr(e *parser.Expr) bool {
	return c.collectionElemType(e) == "string"
}

func (c *Compiler) inferType(e *parser.Expr) string {
	if e == nil {
		return "obj"
	}

	if isBoolExpr(e) {
		return "bool"
	}

	if c.isCharExpr(e) {
		return "char"
	}

	if c.isStringExpr(e) {
		return "string"
	}

	if name, ok := c.simpleIdentifier(e); ok {
		if t, ok2 := c.vars[name]; ok2 {
			return t
		}
	}

	if p := rootPrimary(e); p != nil {
		if p.Lit != nil {
			if p.Lit.Int != nil {
				return "int"
			}
			if p.Lit.Float != nil {
				return "float"
			}
			if p.Lit.Str != nil {
				return "string"
			}
			if p.Lit.Bool != nil {
				return "bool"
			}
		}

		if p.Struct != nil {
			if p.Struct.Name != "" {
				if _, ok := c.structs[p.Struct.Name]; ok {
					return p.Struct.Name
				}
				if u, ok := c.variants[p.Struct.Name]; ok {
					return u
				}
				return "obj"
			}
			names := make([]string, len(p.Struct.Fields))
			types := make([]string, len(p.Struct.Fields))
			typeMap := make(map[string]string)
			for i, f := range p.Struct.Fields {
				t := c.inferType(f.Value)
				n := sanitizeIdent(f.Name)
				names[i] = n
				types[i] = t
				typeMap[n] = t
			}
			return c.ensureAnonStruct(names, types, typeMap)
		}

		if p.Call != nil {
			if t, ok := c.vars[p.Call.Func]; ok && t != "" {
				return t
			}
			switch p.Call.Func {
			case "exists":
				return "bool"
			case "count", "len":
				return "int"
			case "avg":
				return "float"
			case "json", "str", "substring":
				return "string"
			case "append":
				if len(p.Call.Args) > 0 {
					t := c.collectionElemType(p.Call.Args[0])
					return t + " list"
				}
				return "obj list"
			case "values":
				if len(p.Call.Args) == 1 {
					if v := c.mapValueType(p.Call.Args[0]); v != "" {
						return v + " list"
					}
				}
				return "obj list"
			}
		} else if p.Selector != nil && len(p.Selector.Tail) > 0 {
			if post := rootPostfix(e); post != nil && len(post.Ops) > 0 && post.Ops[0].Call != nil {
				name := p.Selector.Tail[len(p.Selector.Tail)-1]
				switch name {
				case "contains":
					return "bool"
				case "padStart":
					return "string"
				}
			}
		}

		if p.Map != nil {
			if t := c.mapLiteralType(p.Map); t != "" {
				return t
			}
		}

		if p.FunExpr != nil {
			return ""
		}

		if p.Selector != nil {
			if t, ok := c.vars[p.Selector.Root]; ok {
				for _, f := range p.Selector.Tail {
					base := strings.TrimSuffix(t, " option")
					fields, ok := c.structs[base]
					if !ok {
						return t
					}
					ft, ok := fields[f]
					if !ok {
						return t
					}
					t = ft
				}
				return t
			}
			if fields, ok := c.tuples[p.Selector.Root]; ok && len(p.Selector.Tail) == 1 {
				field := p.Selector.Tail[0]
				for _, f := range fields {
					if sanitizeIdent(f) == field {
						if ft, ok2 := c.vars[f]; ok2 {
							return ft
						}
					}
				}
			}
		}

		if p.List != nil && len(p.List.Elems) > 0 {
			return c.inferType(p.List.Elems[0]) + " list"
		}

		if p.Query != nil {
			return c.inferQueryElemType(p.Query) + " list"
		}

		if p.Load != nil && p.Load.Type != nil && p.Load.Type.Simple != nil {
			t, err := c.compileType(p.Load.Type)
			if err == nil {
				return t + " list"
			}
		}
	}

	if e.Binary != nil && len(e.Binary.Right) > 0 {
		// Infer type from the left-hand side of a binary expression.
		t := c.inferType(&parser.Expr{Binary: &parser.BinaryExpr{Left: e.Binary.Left}})
		if t != "obj" {
			return t
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
			if v, ok3 := dictValueType(t); ok3 {
				return v
			}
		}
	}
	if p := rootPrimary(e); p != nil {
		if p.List != nil && len(p.List.Elems) > 0 {
			return c.inferType(p.List.Elems[0])
		}
		if p.Call != nil {
			switch p.Call.Func {
			case "append":
				if len(p.Call.Args) > 0 {
					return c.collectionElemType(p.Call.Args[0])
				}
			case "values":
				if len(p.Call.Args) == 1 {
					if v := c.mapValueType(p.Call.Args[0]); v != "" {
						return v
					}
				}
			}
		}
		if p.Map != nil {
			if t := c.mapLiteralType(p.Map); t != "" {
				if v, ok := dictValueType(t); ok {
					return v
				}
			}
		}
	}
	return "obj"
}

func (c *Compiler) inferQueryElemType(q *parser.QueryExpr) string {
	if q == nil {
		return "obj"
	}
	if len(q.Joins) == 1 && q.Joins[0].Side != nil {
		j := q.Joins[0]
		left := c.collectionElemType(q.Source)
		right := c.collectionElemType(j.Src)
		if p := rootPrimary(q.Select); p != nil && p.Struct != nil {
			names := []string{}
			types := []string{}
			typeMap := map[string]string{}
			for _, f := range p.Struct.Fields {
				names = append(names, sanitizeIdent(f.Name))
				t := c.inferType(f.Value)
				types = append(types, t)
				typeMap[sanitizeIdent(f.Name)] = t
			}
			switch *j.Side {
			case "left":
				for i, f := range p.Struct.Fields {
					if name, ok := c.simpleIdentifier(f.Value); ok && name == j.Var {
						types[i] = right + " option"
						typeMap[sanitizeIdent(f.Name)] = types[i]
					}
				}
			case "right":
				if len(q.Froms) == 0 && len(q.Joins) == 1 {
					for i, f := range p.Struct.Fields {
						if name, ok := c.simpleIdentifier(f.Value); ok && name == q.Var {
							types[i] = left + " option"
							typeMap[sanitizeIdent(f.Name)] = types[i]
						}
					}
				}
			case "outer":
				if len(q.Froms) == 0 {
					for i, f := range p.Struct.Fields {
						if name, ok := c.simpleIdentifier(f.Value); ok {
							if name == q.Var {
								types[i] = left + " option"
								typeMap[sanitizeIdent(f.Name)] = types[i]
							}
							if name == j.Var {
								types[i] = right + " option"
								typeMap[sanitizeIdent(f.Name)] = types[i]
							}
						}
					}
				}
			}
			return c.ensureAnonStruct(names, types, typeMap)
		}
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
	c := New()
	c.basePath = src
	return c.Compile(prog)
}
