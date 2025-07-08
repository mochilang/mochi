//go:build slow

package gocode

import (
	"bytes"
	"fmt"
	"strings"

	"mochi/parser"
)

type funSig struct {
	params []*parser.Param
	ret    *parser.TypeRef
}

// Compiler translates a subset of Mochi to Go source code.
type Compiler struct {
	buf           bytes.Buffer
	indent        int
	needsAvg      bool
	needsSum      bool
	needsMin      bool
	needsMax      bool
	needsExists   bool
	needsContains bool
	needsReflect  bool
	needsStrconv  bool
	needsStrings  bool
	structTypes   map[string]bool
	funSigs       map[string]*funSig
}

// New creates a new Go compiler.
func New() *Compiler {
	return &Compiler{structTypes: make(map[string]bool), funSigs: make(map[string]*funSig)}
}

// Compile translates the given program to Go.
func (c *Compiler) Compile(prog *parser.Program) ([]byte, error) {
	c.buf.Reset()
	c.indent = 0
	c.needsAvg = false
	c.needsSum = false
	c.needsMin = false
	c.needsMax = false
	c.needsExists = false
	c.needsContains = false
	c.needsReflect = false
	c.needsStrconv = false
	c.needsStrings = false
	c.structTypes = make(map[string]bool)
	c.funSigs = make(map[string]*funSig)

	var types bytes.Buffer
	var funcs bytes.Buffer
	var body bytes.Buffer

	for _, s := range prog.Statements {
		if s.Type != nil {
			c.structTypes[s.Type.Name] = true
		} else if s.Fun != nil {
			c.funSigs[s.Fun.Name] = &funSig{params: s.Fun.Params, ret: s.Fun.Return}
		}
	}

	for _, s := range prog.Statements {
		if s.Type != nil {
			if err := c.compileTypeDecl(&types, s.Type); err != nil {
				return nil, err
			}
			types.WriteByte('\n')
		} else if s.Fun != nil {
			if err := c.compileFunTo(&funcs, s.Fun); err != nil {
				return nil, err
			}
			funcs.WriteByte('\n')
		} else {
			c.indent = 1
			if err := c.compileStmtTo(&body, s); err != nil {
				return nil, err
			}
			c.indent = 0
		}
	}

	// write header and imports
	c.writeln("//go:build ignore")
	c.writeln("")
	c.writeln("package main")
	c.writeln("")
	c.writeln("import (")
	c.indent++
	c.writeln("\"fmt\"")
	if c.needsStrconv {
		c.writeln("\"strconv\"")
	}
	if c.needsStrings {
		c.writeln("\"strings\"")
	}
	if c.needsReflect {
		c.writeln("\"reflect\"")
	}
	c.indent--
	c.writeln(")")
	c.writeln("")

	c.buf.Write(types.Bytes())
	if types.Len() > 0 {
		c.writeln("")
	}

	if c.needsSum {
		c.writeln("func sum(nums []int) int {")
		c.indent++
		c.writeln("var s int")
		c.writeln("for _, n := range nums {")
		c.indent++
		c.writeln("s += n")
		c.indent--
		c.writeln("}")
		c.writeln("return s")
		c.indent--
		c.writeln("}")
		c.writeln("")
	}

	if c.needsAvg {
		c.writeln("func avg(nums []int) int {")
		c.indent++
		c.writeln("if len(nums) == 0 {")
		c.indent++
		c.writeln("return 0")
		c.indent--
		c.writeln("}")
		c.writeln("var sum int")
		c.writeln("for _, n := range nums {")
		c.indent++
		c.writeln("sum += n")
		c.indent--
		c.writeln("}")
		c.writeln("return sum / len(nums)")
		c.indent--
		c.writeln("}")
		c.writeln("")
	}

	if c.needsMin {
		c.writeln("func min(nums []int) int {")
		c.indent++
		c.writeln("if len(nums) == 0 {")
		c.indent++
		c.writeln("panic(\"empty slice\")")
		c.indent--
		c.writeln("}")
		c.writeln("m := nums[0]")
		c.writeln("for _, n := range nums[1:] {")
		c.indent++
		c.writeln("if n < m {")
		c.indent++
		c.writeln("m = n")
		c.indent--
		c.writeln("}")
		c.indent--
		c.writeln("}")
		c.writeln("return m")
		c.indent--
		c.writeln("}")
		c.writeln("")
	}

	if c.needsMax {
		c.writeln("func max(nums []int) int {")
		c.indent++
		c.writeln("if len(nums) == 0 {")
		c.indent++
		c.writeln("panic(\"empty slice\")")
		c.indent--
		c.writeln("}")
		c.writeln("m := nums[0]")
		c.writeln("for _, n := range nums[1:] {")
		c.indent++
		c.writeln("if n > m {")
		c.indent++
		c.writeln("m = n")
		c.indent--
		c.writeln("}")
		c.indent--
		c.writeln("}")
		c.writeln("return m")
		c.indent--
		c.writeln("}")
		c.writeln("")
	}

	if c.needsExists {
		c.needsReflect = true
		c.writeln("func exists(v interface{}) bool {")
		c.indent++
		c.writeln("val := reflect.ValueOf(v)")
		c.writeln("switch val.Kind() {")
		c.indent++
		c.writeln("case reflect.String, reflect.Slice, reflect.Array, reflect.Map:")
		c.indent++
		c.writeln("return val.Len() > 0")
		c.indent--
		c.writeln("case reflect.Struct:")
		c.indent++
		c.writeln("f := val.FieldByName(\"Items\")")
		c.writeln("if f.IsValid() && (f.Kind() == reflect.Slice || f.Kind() == reflect.Array) {")
		c.indent++
		c.writeln("return f.Len() > 0")
		c.indent--
		c.writeln("}")
		c.writeln("return false")
		c.indent--
		c.writeln("default:")
		c.indent++
		c.writeln("return v != nil")
		c.indent--
		c.writeln("}")
		c.indent--
		c.writeln("}")
		c.writeln("")
	}

	if c.needsContains {
		c.needsReflect = true
		c.writeln("func contains(coll interface{}, v interface{}) bool {")
		c.indent++
		c.writeln("val := reflect.ValueOf(coll)")
		c.writeln("switch val.Kind() {")
		c.indent++
		c.writeln("case reflect.Slice, reflect.Array:")
		c.indent++
		c.writeln("for i := 0; i < val.Len(); i++ {")
		c.indent++
		c.writeln("if reflect.DeepEqual(val.Index(i).Interface(), v) {")
		c.indent++
		c.writeln("return true")
		c.indent--
		c.writeln("}")
		c.indent--
		c.writeln("}")
		c.writeln("return false")
		c.indent--
		c.writeln("case reflect.Map:")
		c.indent++
		c.writeln("if val.MapIndex(reflect.ValueOf(v)).IsValid() {")
		c.indent++
		c.writeln("return true")
		c.indent--
		c.writeln("}")
		c.writeln("return false")
		c.indent--
		c.writeln("default:")
		c.indent++
		c.writeln("return false")
		c.indent--
		c.writeln("}")
		c.indent--
		c.writeln("}")
		c.writeln("")
	}

	if c.needsReflect && c.needsStrings {
		c.writeln("func getField(v interface{}, name string) interface{} {")
		c.indent++
		c.writeln("if m, ok := v.(map[interface{}]interface{}); ok {")
		c.indent++
		c.writeln("return m[name]")
		c.indent--
		c.writeln("}")
		c.writeln("val := reflect.ValueOf(v)")
		c.writeln("name = strings.Title(name)")
		c.writeln("if val.Kind() == reflect.Pointer {")
		c.indent++
		c.writeln("val = val.Elem()")
		c.indent--
		c.writeln("}")
		c.writeln("f := val.FieldByName(name)")
		c.writeln("if f.IsValid() {")
		c.indent++
		c.writeln("return f.Interface()")
		c.indent--
		c.writeln("}")
		c.writeln("return nil")
		c.indent--
		c.writeln("}")
		c.writeln("")
	}

	c.buf.Write(funcs.Bytes())
	if funcs.Len() > 0 {
		c.writeln("")
	}

	c.writeln("func main() {")
	c.indent++
	c.buf.Write(body.Bytes())
	c.indent--
	c.writeln("}")

	return c.buf.Bytes(), nil
}

func (c *Compiler) compileStmt(s *parser.Statement) error {
	return c.compileStmtTo(&c.buf, s)
}

func (c *Compiler) compileStmtTo(buf *bytes.Buffer, s *parser.Statement) error {
	switch {
	case s.Let != nil:
		return c.compileLet(buf, s.Let)
	case s.Var != nil:
		return c.compileVar(buf, s.Var)
	case s.Assign != nil:
		return c.compileAssign(buf, s.Assign)
	case s.Fun != nil:
		return c.compileFun(s.Fun)
	case s.Return != nil:
		return c.compileReturn(buf, s.Return)
	case s.If != nil:
		return c.compileIf(buf, s.If)
	case s.While != nil:
		return c.compileWhile(buf, s.While)
	case s.For != nil:
		return c.compileFor(buf, s.For)
	case s.Break != nil:
		c.writeLine(buf, "break")
		return nil
	case s.Continue != nil:
		c.writeLine(buf, "continue")
		return nil
	case s.Expr != nil:
		expr, err := c.compileExpr(s.Expr.Expr)
		if err != nil {
			return err
		}
		c.writeLine(buf, expr)
		return nil
	default:
		return fmt.Errorf("unsupported statement at line %d", s.Pos.Line)
	}
}

func (c *Compiler) compileLet(buf *bytes.Buffer, l *parser.LetStmt) error {
	if l.Value == nil {
		if l.Type == nil {
			return fmt.Errorf("let without value at line %d", l.Pos.Line)
		}
		typ, err := c.compileType(l.Type)
		if err != nil {
			return err
		}
		c.writeLine(buf, fmt.Sprintf("var %s %s", l.Name, typ))
		return nil
	}
	val, err := c.compileExpr(l.Value)
	if err != nil {
		return err
	}
	typName := ""
	ptr := false
	if l.Type != nil {
		typName, err = c.compileType(l.Type)
		if err != nil {
			return err
		}
		if c.structTypes[typName] {
			ptr = true
		}
	} else if isStructLiteralExpr(l.Value) {
		typName = structNameFromExpr(l.Value)
		if typName != "" {
			ptr = true
		}
	}
	if ptr && !strings.HasPrefix(val, "&") {
		val = "&" + val
	}
	if l.Type != nil {
		typ := typName
		if ptr {
			typ = "*" + typ
		}
		c.writeLine(buf, fmt.Sprintf("var %s %s = %s", l.Name, typ, val))
	} else {
		c.writeLine(buf, fmt.Sprintf("%s := %s", l.Name, val))
	}
	return nil
}

func (c *Compiler) compileVar(buf *bytes.Buffer, v *parser.VarStmt) error {
	if v.Value == nil {
		if v.Type == nil {
			return fmt.Errorf("var without value at line %d", v.Pos.Line)
		}
		typ, err := c.compileType(v.Type)
		if err != nil {
			return err
		}
		c.writeLine(buf, fmt.Sprintf("var %s %s", v.Name, typ))
		return nil
	}
	val, err := c.compileExpr(v.Value)
	if err != nil {
		return err
	}
	typName := ""
	ptr := false
	if v.Type != nil {
		typName, err = c.compileType(v.Type)
		if err != nil {
			return err
		}
		if c.structTypes[typName] {
			ptr = true
		}
	} else if isStructLiteralExpr(v.Value) {
		typName = structNameFromExpr(v.Value)
		if typName != "" {
			ptr = true
		}
	}
	if ptr && !strings.HasPrefix(val, "&") {
		val = "&" + val
	}
	if v.Type != nil {
		typ := typName
		if ptr {
			typ = "*" + typ
		}
		c.writeLine(buf, fmt.Sprintf("var %s %s = %s", v.Name, typ, val))
	} else {
		c.writeLine(buf, fmt.Sprintf("%s := %s", v.Name, val))
	}
	return nil
}

func (c *Compiler) compileAssign(buf *bytes.Buffer, a *parser.AssignStmt) error {
	target := a.Name
	for idxNum, idx := range a.Index {
		if idx.Colon != nil {
			start, err := c.compileExpr(idx.Start)
			if err != nil {
				return err
			}
			end, err := c.compileExpr(idx.End)
			if err != nil {
				return err
			}
			target = fmt.Sprintf("%s[%s:%s]", target, start, end)
		} else {
			idxStr, err := c.compileExpr(idx.Start)
			if err != nil {
				return err
			}
			if idxNum < len(a.Index)-1 && !isIntLiteral(idx.Start) {
				target = fmt.Sprintf("%s[%s].(map[interface{}]interface{})", target, idxStr)
			} else {
				target = fmt.Sprintf("%s[%s]", target, idxStr)
			}
		}
	}
	for _, f := range a.Field {
		target += "." + strings.Title(f.Name)
	}
	val, err := c.compileExpr(a.Value)
	if err != nil {
		return err
	}
	c.writeLine(buf, fmt.Sprintf("%s = %s", target, val))
	return nil
}

func (c *Compiler) compileFun(f *parser.FunStmt) error {
	return c.compileFunTo(&c.buf, f)
}

func (c *Compiler) compileFunTo(buf *bytes.Buffer, f *parser.FunStmt) error {
	var params []string
	for _, p := range f.Params {
		typ, err := c.compileType(p.Type)
		if err != nil {
			return err
		}
		if c.structTypes[typ] {
			typ = "*" + typ
		}
		params = append(params, fmt.Sprintf("%s %s", p.Name, typ))
	}
	ret := ""
	if f.Return != nil {
		t, err := c.compileType(f.Return)
		if err != nil {
			return err
		}
		if c.structTypes[t] {
			t = "*" + t
		}
		ret = " " + t
	}
	c.writeto(buf, fmt.Sprintf("func %s(%s)%s {", f.Name, strings.Join(params, ", "), ret))
	c.indent++
	for _, st := range f.Body {
		if err := c.compileStmtTo(buf, st); err != nil {
			return err
		}
	}
	c.indent--
	c.writeto(buf, "}")
	return nil
}

func (c *Compiler) compileTypeDecl(buf *bytes.Buffer, d *parser.TypeDecl) error {
	c.writeto(buf, fmt.Sprintf("type %s struct {", d.Name))
	c.indent++
	for _, m := range d.Members {
		if m.Field == nil {
			continue
		}
		typ, err := c.compileType(m.Field.Type)
		if err != nil {
			return err
		}
		c.writeto(buf, fmt.Sprintf("%s %s", strings.Title(m.Field.Name), typ))
	}
	c.indent--
	c.writeto(buf, "}")
	return nil
}

func (c *Compiler) compileReturn(buf *bytes.Buffer, r *parser.ReturnStmt) error {
	if r.Value != nil {
		val, err := c.compileExpr(r.Value)
		if err != nil {
			return err
		}
		c.writeLine(buf, fmt.Sprintf("return %s", val))
	} else {
		c.writeLine(buf, "return")
	}
	return nil
}

func (c *Compiler) compileIf(buf *bytes.Buffer, i *parser.IfStmt) error {
	cond, err := c.compileExpr(i.Cond)
	if err != nil {
		return err
	}
	c.writeLine(buf, fmt.Sprintf("if %s {", cond))
	c.indent++
	for _, st := range i.Then {
		if err := c.compileStmtTo(buf, st); err != nil {
			return err
		}
	}
	c.indent--
	if i.ElseIf != nil {
		c.writeLine(buf, "} else {")
		c.indent++
		if err := c.compileIf(buf, i.ElseIf); err != nil {
			return err
		}
		c.indent--
		c.writeLine(buf, "}")
	} else if len(i.Else) > 0 {
		c.writeLine(buf, "} else {")
		c.indent++
		for _, st := range i.Else {
			if err := c.compileStmtTo(buf, st); err != nil {
				return err
			}
		}
		c.indent--
		c.writeLine(buf, "}")
	} else {
		c.writeLine(buf, "}")
	}
	return nil
}

func (c *Compiler) compileWhile(buf *bytes.Buffer, w *parser.WhileStmt) error {
	cond, err := c.compileExpr(w.Cond)
	if err != nil {
		return err
	}
	c.writeLine(buf, fmt.Sprintf("for %s {", cond))
	c.indent++
	for _, st := range w.Body {
		if err := c.compileStmtTo(buf, st); err != nil {
			return err
		}
	}
	c.indent--
	c.writeLine(buf, "}")
	return nil
}

func (c *Compiler) compileFor(buf *bytes.Buffer, fr *parser.ForStmt) error {
	if fr.RangeEnd != nil {
		start, err := c.compileExpr(fr.Source)
		if err != nil {
			return err
		}
		end, err := c.compileExpr(fr.RangeEnd)
		if err != nil {
			return err
		}
		c.writeLine(buf, fmt.Sprintf("for %s := %s; %s < %s; %s++ {", fr.Name, start, fr.Name, end, fr.Name))
		c.indent++
		for _, st := range fr.Body {
			if err := c.compileStmtTo(buf, st); err != nil {
				return err
			}
		}
		c.indent--
		c.writeLine(buf, "}")
		return nil
	}
	src, err := c.compileExpr(fr.Source)
	if err != nil {
		return err
	}
	c.writeLine(buf, fmt.Sprintf("for _, %s := range %s {", fr.Name, src))
	c.indent++
	for _, st := range fr.Body {
		if err := c.compileStmtTo(buf, st); err != nil {
			return err
		}
	}
	c.indent--
	c.writeLine(buf, "}")
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
		if op.Op == "in" {
			c.needsContains = true
			c.needsReflect = true
			res = fmt.Sprintf("contains(%s, %s)", r, res)
		} else {
			res = fmt.Sprintf("%s %s %s", res, op.Op, r)
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
		val = u.Ops[i] + val
	}
	return val, nil
}

func (c *Compiler) compilePostfix(p *parser.PostfixExpr) (string, error) {
	val, err := c.compilePrimary(p.Target)
	if err != nil {
		return "", err
	}
	curr := p.Target
	for i, op := range p.Ops {
		switch {
		case op.Index != nil:
			if op.Index.Colon != nil {
				start, err := c.compileExpr(op.Index.Start)
				if err != nil {
					return "", err
				}
				end, err := c.compileExpr(op.Index.End)
				if err != nil {
					return "", err
				}
				if strings.HasPrefix(val, "\"") {
					val = fmt.Sprintf("string([]rune(%s)[%s:%s])", val, start, end)
				} else {
					val = fmt.Sprintf("%s[%s:%s]", val, start, end)
				}
			} else {
				idxStr, err := c.compileExpr(op.Index.Start)
				if err != nil {
					return "", err
				}
				if i < len(p.Ops)-1 && p.Ops[i+1].Index != nil && !isIntLiteral(op.Index.Start) {
					if strings.HasPrefix(val, "\"") {
						val = fmt.Sprintf("string([]rune(%s)[%s])", val, idxStr)
					} else {
						val = fmt.Sprintf("%s[%s].(map[interface{}]interface{})", val, idxStr)
					}
				} else {
					if strings.HasPrefix(val, "\"") {
						val = fmt.Sprintf("string([]rune(%s)[%s])", val, idxStr)
					} else {
						val = fmt.Sprintf("%s[%s]", val, idxStr)
					}
				}
			}
			curr = nil
		case op.Call != nil:
			args := make([]string, len(op.Call.Args))
			for i, a := range op.Call.Args {
				s, err := c.compileExpr(a)
				if err != nil {
					return "", err
				}
				args[i] = s
			}
			switch val {
			case "len", "count":
				if len(args) != 1 {
					return "", fmt.Errorf("%s expects 1 arg", val)
				}
				val = fmt.Sprintf("len(%s)", args[0])
			case "append":
				if len(args) != 2 {
					return "", fmt.Errorf("append expects 2 args")
				}
				val = fmt.Sprintf("append(%s, %s)", args[0], args[1])
			case "avg":
				c.needsAvg = true
				val = fmt.Sprintf("avg(%s)", join(args, ", "))
			case "sum":
				c.needsSum = true
				val = fmt.Sprintf("sum(%s)", join(args, ", "))
			case "min":
				c.needsMin = true
				val = fmt.Sprintf("min(%s)", join(args, ", "))
			case "max":
				c.needsMax = true
				val = fmt.Sprintf("max(%s)", join(args, ", "))
			case "substring":
				if len(args) != 3 {
					return "", fmt.Errorf("substring expects 3 args")
				}
				val = fmt.Sprintf("string([]rune(%s)[%s:%s])", args[0], args[1], args[2])
			case "str":
				if len(args) != 1 {
					return "", fmt.Errorf("str expects 1 arg")
				}
				c.needsStrconv = true
				val = fmt.Sprintf("strconv.Itoa(%s)", args[0])
			case "contains":
				if len(args) != 2 {
					return "", fmt.Errorf("contains expects 2 args")
				}
				c.needsContains = true
				val = fmt.Sprintf("contains(%s, %s)", args[0], args[1])
			case "exists":
				if len(args) != 1 {
					return "", fmt.Errorf("exists expects 1 arg")
				}
				c.needsExists = true
				val = fmt.Sprintf("exists(%s)", args[0])
			default:
				if (strings.HasSuffix(val, ".contains") || strings.HasSuffix(val, ".Contains")) && len(args) == 1 {
					c.needsStrings = true
					base := strings.TrimSuffix(strings.TrimSuffix(val, ".contains"), ".Contains")
					val = fmt.Sprintf("strings.Contains(%s, %s)", base, args[0])
				} else if sig, ok := c.funSigs[val]; ok && len(args) < len(sig.params) {
					expr, err := c.partialFunc(val, sig, args)
					if err != nil {
						return "", err
					}
					val = expr
				} else {
					val = fmt.Sprintf("%s(%s)", val, join(args, ", "))
				}
			}
			curr = nil
		case op.Cast != nil:
			typ, err := c.compileType(op.Cast.Type)
			if err != nil {
				return "", err
			}
			if c.structTypes[typ] && curr != nil && curr.Map != nil {
				sv, err := c.mapToStructLiteral(curr.Map, typ)
				if err != nil {
					return "", err
				}
				val = sv
			} else if typ == "int" && strings.HasPrefix(val, "\"") {
				c.needsStrconv = true
				val = fmt.Sprintf("func() int { v, _ := strconv.Atoi(%s); return v }()", val)
			} else {
				val = fmt.Sprintf("(%s)(%s)", typ, val)
			}
			curr = nil
		case op.Field != nil:
			c.needsReflect = true
			c.needsStrings = true
			val = fmt.Sprintf("getField(%s, \"%s\")", val, op.Field.Name)
			curr = nil
		default:
			return "", fmt.Errorf("postfix operations not supported")
		}
	}
	return val, nil
}

func (c *Compiler) compilePrimary(p *parser.Primary) (string, error) {
	switch {
	case p.Lit != nil:
		return c.compileLiteral(p.Lit), nil
	case p.Selector != nil:
		if len(p.Selector.Tail) > 0 && len(p.Selector.Root) > 0 && strings.ToLower(p.Selector.Root[:1]) == p.Selector.Root[:1] {
			c.needsReflect = true
			c.needsStrings = true
			expr := p.Selector.Root
			for _, t := range p.Selector.Tail {
				expr = fmt.Sprintf("getField(%s, \"%s\")", expr, t)
			}
			return expr, nil
		}
		name := p.Selector.Root
		for _, t := range p.Selector.Tail {
			name += "." + strings.Title(t)
		}
		return name, nil
	case p.List != nil:
		elems := make([]string, len(p.List.Elems))
		for i, e := range p.List.Elems {
			s, err := c.compileExpr(e)
			if err != nil {
				return "", err
			}
			elems[i] = s
		}
		typ := "[]interface{}"
		allInt := true
		for _, e := range p.List.Elems {
			if !isIntLiteral(e) {
				allInt = false
				break
			}
		}
		if allInt {
			typ = "[]int"
		} else if len(p.List.Elems) > 0 && isListLiteral(p.List.Elems[0]) {
			typ = "[][]interface{}"
		}
		return typ + "{" + join(elems, ", ") + "}", nil
	case p.Call != nil:
		return c.compileCall(p.Call)
	case p.Group != nil:
		s, err := c.compileExpr(p.Group)
		if err != nil {
			return "", err
		}
		return fmt.Sprintf("(%s)", s), nil
	case p.Map != nil:
		return c.compileMapLiteral(p.Map)
	case p.Struct != nil:
		return c.compileStructLiteral(p.Struct)
	case p.Query != nil:
		return c.compileQueryExpr(p.Query)
	case p.FunExpr != nil:
		return c.compileFunExpr(p.FunExpr)
	case p.If != nil:
		return c.compileIfExpr(p.If)
	default:
		return "", fmt.Errorf("unsupported expression at line %d", p.Pos.Line)
	}
}

func (c *Compiler) compileCall(call *parser.CallExpr) (string, error) {
	if call.Func == "exists" && len(call.Args) == 1 {
		if call.Args[0] != nil && call.Args[0].Binary != nil && call.Args[0].Binary.Left != nil && call.Args[0].Binary.Left.Value != nil && call.Args[0].Binary.Left.Value.Target != nil && call.Args[0].Binary.Left.Value.Target.Query != nil {
			return c.compileExistsQuery(call.Args[0].Binary.Left.Value.Target.Query)
		}
	}
	args := make([]string, len(call.Args))
	for i, a := range call.Args {
		s, err := c.compileExpr(a)
		if err != nil {
			return "", err
		}
		args[i] = s
	}
	argStr := join(args, ", ")
	switch call.Func {
	case "print":
		return fmt.Sprintf("fmt.Println(%s)", argStr), nil
	case "append":
		if len(args) != 2 {
			return "", fmt.Errorf("append expects 2 args")
		}
		return fmt.Sprintf("append(%s, %s)", args[0], args[1]), nil
	case "avg":
		c.needsAvg = true
		return fmt.Sprintf("avg(%s)", argStr), nil
	case "count":
		if len(args) != 1 {
			return "", fmt.Errorf("count expects 1 arg")
		}
		return fmt.Sprintf("len(%s)", args[0]), nil
	case "sum":
		c.needsSum = true
		return fmt.Sprintf("sum(%s)", argStr), nil
	case "min":
		c.needsMin = true
		return fmt.Sprintf("min(%s)", argStr), nil
	case "max":
		c.needsMax = true
		return fmt.Sprintf("max(%s)", argStr), nil
	case "substring":
		if len(args) != 3 {
			return "", fmt.Errorf("substring expects 3 args")
		}
		return fmt.Sprintf("string([]rune(%s)[%s:%s])", args[0], args[1], args[2]), nil
	case "str":
		if len(args) != 1 {
			return "", fmt.Errorf("str expects 1 arg")
		}
		c.needsStrconv = true
		return fmt.Sprintf("strconv.Itoa(%s)", args[0]), nil
	case "exists":
		if len(args) != 1 {
			return "", fmt.Errorf("exists expects 1 arg")
		}
		c.needsExists = true
		return fmt.Sprintf("exists(%s)", args[0]), nil
	default:
		if sig, ok := c.funSigs[call.Func]; ok && len(args) < len(sig.params) {
			return c.partialFunc(call.Func, sig, args)
		}
		return fmt.Sprintf("%s(%s)", call.Func, argStr), nil
	}
}

func (c *Compiler) compileMapLiteral(m *parser.MapLiteral) (string, error) {
	elems := make([]string, len(m.Items))
	for i, it := range m.Items {
		k, err := c.compileMapKey(it.Key)
		if err != nil {
			return "", err
		}
		v, err := c.compileExpr(it.Value)
		if err != nil {
			return "", err
		}
		elems[i] = fmt.Sprintf("%s: %s", k, v)
	}
	return "map[interface{}]interface{}{" + join(elems, ", ") + "}", nil
}

func (c *Compiler) compileMapKey(e *parser.Expr) (string, error) {
	if e != nil && e.Binary != nil && e.Binary.Left != nil && e.Binary.Left.Value != nil && e.Binary.Left.Value.Target != nil {
		tgt := e.Binary.Left.Value.Target
		if tgt.Lit != nil && tgt.Lit.Str != nil {
			return fmt.Sprintf("\"%s\"", *tgt.Lit.Str), nil
		}
		if tgt.Selector != nil && len(tgt.Selector.Tail) == 0 {
			return fmt.Sprintf("\"%s\"", tgt.Selector.Root), nil
		}
	}
	return c.compileExpr(e)
}

func (c *Compiler) compileStructLiteral(s *parser.StructLiteral) (string, error) {
	fields := make([]string, len(s.Fields))
	for i, f := range s.Fields {
		val, err := c.compileExpr(f.Value)
		if err != nil {
			return "", err
		}
		fields[i] = fmt.Sprintf("%s: %s", strings.Title(f.Name), val)
	}
	return fmt.Sprintf("%s{%s}", s.Name, join(fields, ", ")), nil
}

func (c *Compiler) mapToStructLiteral(m *parser.MapLiteral, typ string) (string, error) {
	fields := make([]string, len(m.Items))
	for i, it := range m.Items {
		if it.Key == nil || it.Key.Binary == nil || it.Key.Binary.Left == nil || it.Key.Binary.Left.Value == nil || it.Key.Binary.Left.Value.Target == nil || it.Key.Binary.Left.Value.Target.Lit == nil || it.Key.Binary.Left.Value.Target.Lit.Str == nil {
			return "", fmt.Errorf("unsupported struct field key")
		}
		name := strings.Title(*it.Key.Binary.Left.Value.Target.Lit.Str)
		val, err := c.compileExpr(it.Value)
		if err != nil {
			return "", err
		}
		fields[i] = fmt.Sprintf("%s: %s", name, val)
	}
	return fmt.Sprintf("%s{%s}", typ, join(fields, ", ")), nil
}

func (c *Compiler) compileFunExpr(f *parser.FunExpr) (string, error) {
	params := make([]string, len(f.Params))
	for i, p := range f.Params {
		t, err := c.compileType(p.Type)
		if err != nil {
			return "", err
		}
		if c.structTypes[t] {
			t = "*" + t
		}
		params[i] = fmt.Sprintf("%s %s", p.Name, t)
	}
	ret := ""
	if f.Return != nil {
		t, err := c.compileType(f.Return)
		if err != nil {
			return "", err
		}
		if c.structTypes[t] {
			t = "*" + t
		}
		ret = " " + t
	}
	var buf bytes.Buffer
	buf.WriteString("func(" + strings.Join(params, ", ") + ")" + ret + " {")
	c.indent++
	if f.ExprBody != nil {
		expr, err := c.compileExpr(f.ExprBody)
		if err != nil {
			return "", err
		}
		c.writeLine(&buf, "return "+expr)
	} else {
		for _, st := range f.BlockBody {
			if err := c.compileStmtTo(&buf, st); err != nil {
				return "", err
			}
		}
	}
	c.indent--
	c.writeto(&buf, "}")
	return strings.TrimSuffix(buf.String(), "\n"), nil
}

func (c *Compiler) partialFunc(name string, sig *funSig, args []string) (string, error) {
	remain := sig.params[len(args):]
	params := make([]string, len(remain))
	for i, p := range remain {
		typ, err := c.compileType(p.Type)
		if err != nil {
			return "", err
		}
		if c.structTypes[typ] {
			typ = "*" + typ
		}
		params[i] = fmt.Sprintf("%s %s", p.Name, typ)
	}
	callArgs := append(append([]string{}, args...), paramNames(remain)...)
	ret := ""
	if sig.ret != nil {
		t, err := c.compileType(sig.ret)
		if err != nil {
			return "", err
		}
		if c.structTypes[t] {
			t = "*" + t
		}
		ret = " " + t
	}
	return fmt.Sprintf("func(%s)%s { return %s(%s) }", strings.Join(params, ", "), ret, name, strings.Join(callArgs, ", ")), nil
}

func paramNames(ps []*parser.Param) []string {
	out := make([]string, len(ps))
	for i, p := range ps {
		out[i] = p.Name
	}
	return out
}

func (c *Compiler) compileIfExpr(i *parser.IfExpr) (string, error) {
	cond, err := c.compileExpr(i.Cond)
	if err != nil {
		return "", err
	}
	thenVal, err := c.compileExpr(i.Then)
	if err != nil {
		return "", err
	}
	elseVal := "nil"
	if i.ElseIf != nil {
		elseVal, err = c.compileIfExpr(i.ElseIf)
		if err != nil {
			return "", err
		}
	} else if i.Else != nil {
		elseVal, err = c.compileExpr(i.Else)
		if err != nil {
			return "", err
		}
	}
	return fmt.Sprintf("func() interface{} { if %s { return %s } else { return %s } }()", cond, thenVal, elseVal), nil
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
		return fmt.Sprintf("%t", bool(*l.Bool))
	default:
		return "nil"
	}
}

func (c *Compiler) writeln(s string) {
	for i := 0; i < c.indent; i++ {
		c.buf.WriteString("    ")
	}
	c.buf.WriteString(s)
	c.buf.WriteByte('\n')
}

func (c *Compiler) writeto(buf *bytes.Buffer, s string) {
	for i := 0; i < c.indent; i++ {
		buf.WriteString("    ")
	}
	buf.WriteString(s)
	buf.WriteByte('\n')
}

func (c *Compiler) writeLine(buf *bytes.Buffer, s string) {
	for i := 0; i < c.indent; i++ {
		buf.WriteString("    ")
	}
	buf.WriteString(s)
	buf.WriteByte('\n')
}

func (c *Compiler) compileType(t *parser.TypeRef) (string, error) {
	if t == nil {
		return "", fmt.Errorf("unsupported type")
	}
	if t.Simple != nil {
		return *t.Simple, nil
	}
	if t.Fun != nil {
		params := make([]string, len(t.Fun.Params))
		for i, p := range t.Fun.Params {
			s, err := c.compileType(p)
			if err != nil {
				return "", err
			}
			params[i] = s
		}
		ret := ""
		if t.Fun.Return != nil {
			r, err := c.compileType(t.Fun.Return)
			if err != nil {
				return "", err
			}
			ret = " " + r
		}
		return fmt.Sprintf("func(%s)%s", strings.Join(params, ", "), ret), nil
	}
	if t.Struct != nil {
		fields := make([]string, len(t.Struct.Fields))
		for i, f := range t.Struct.Fields {
			typ, err := c.compileType(f.Type)
			if err != nil {
				return "", err
			}
			fields[i] = fmt.Sprintf("%s %s", strings.Title(f.Name), typ)
		}
		return fmt.Sprintf("struct{ %s }", strings.Join(fields, "; ")), nil
	}
	return "", fmt.Errorf("unsupported type")
}

func (c *Compiler) compileExistsQuery(q *parser.QueryExpr) (string, error) {
	if len(q.Froms) > 0 || len(q.Joins) > 0 || q.Group != nil || q.Sort != nil || q.Skip != nil || q.Take != nil || q.Distinct {
		return "", fmt.Errorf("unsupported query")
	}
	src, err := c.compileExpr(q.Source)
	if err != nil {
		return "", err
	}
	var cond string
	if q.Where != nil {
		cond, err = c.compileExpr(q.Where)
		if err != nil {
			return "", err
		}
	}
	var buf bytes.Buffer
	buf.WriteString("func() bool {\n")
	c.indent++
	c.writeto(&buf, fmt.Sprintf("for _, %s := range %s {", q.Var, src))
	c.indent++
	if cond != "" {
		c.writeto(&buf, fmt.Sprintf("if %s {", cond))
		c.indent++
	}
	c.writeto(&buf, "return true")
	if cond != "" {
		c.indent--
		c.writeto(&buf, "}")
	}
	c.indent--
	c.writeto(&buf, "}")
	c.writeto(&buf, "return false")
	c.indent--
	c.writeto(&buf, "}()")
	return strings.TrimSuffix(buf.String(), "\n"), nil
}

func (c *Compiler) compileQueryExpr(q *parser.QueryExpr) (string, error) {
	if len(q.Joins) > 0 || q.Group != nil || q.Sort != nil || q.Skip != nil || q.Take != nil || q.Distinct {
		return "", fmt.Errorf("unsupported query")
	}
	src, err := c.compileExpr(q.Source)
	if err != nil {
		return "", err
	}
	fromSrcs := make([]string, len(q.Froms))
	for i, f := range q.Froms {
		fs, err := c.compileExpr(f.Src)
		if err != nil {
			return "", err
		}
		fromSrcs[i] = fs
	}
	var selType string
	var selExpr string
	if q.Select != nil && q.Select.Binary != nil && q.Select.Binary.Left != nil && q.Select.Binary.Left.Value != nil && q.Select.Binary.Left.Value.Target != nil && q.Select.Binary.Left.Value.Target.Map != nil {
		typ, lit, err := c.mapToAnonStruct(q.Select.Binary.Left.Value.Target.Map)
		if err != nil {
			return "", err
		}
		selType = typ
		selExpr = lit
	} else {
		se, err := c.compileExpr(q.Select)
		if err != nil {
			return "", err
		}
		selType = "interface{}"
		selExpr = se
	}
	cond := ""
	if q.Where != nil {
		cnd, err := c.compileExpr(q.Where)
		if err != nil {
			return "", err
		}
		cond = cnd
	}

	var buf bytes.Buffer
	buf.WriteString("func() []" + selType + " {\n")
	c.indent++
	c.writeto(&buf, "var _res []"+selType)
	c.writeto(&buf, fmt.Sprintf("for _, %s := range %s {", q.Var, src))
	c.indent++
	for i, f := range q.Froms {
		c.writeto(&buf, fmt.Sprintf("for _, %s := range %s {", f.Var, fromSrcs[i]))
		c.indent++
	}
	if cond != "" {
		c.writeto(&buf, fmt.Sprintf("if %s {", cond))
		c.indent++
	}
	c.writeto(&buf, fmt.Sprintf("_res = append(_res, %s)", selExpr))
	if cond != "" {
		c.indent--
		c.writeto(&buf, "}")
	}
	for range q.Froms {
		c.indent--
		c.writeto(&buf, "}")
	}
	c.indent--
	c.writeto(&buf, "}")
	c.writeto(&buf, "return _res")
	c.indent--
	c.writeto(&buf, "}()")
	return strings.TrimSuffix(buf.String(), "\n"), nil
}

func (c *Compiler) mapToAnonStruct(m *parser.MapLiteral) (string, string, error) {
	fields := make([]string, len(m.Items))
	vals := make([]string, len(m.Items))
	for i, it := range m.Items {
		if it.Key == nil || it.Key.Binary == nil || it.Key.Binary.Left == nil || it.Key.Binary.Left.Value == nil || it.Key.Binary.Left.Value.Target == nil {
			return "", "", fmt.Errorf("unsupported struct field key")
		}
		var keyName string
		tgt := it.Key.Binary.Left.Value.Target
		if tgt.Lit != nil && tgt.Lit.Str != nil {
			keyName = *tgt.Lit.Str
		} else if tgt.Selector != nil && len(tgt.Selector.Tail) == 0 {
			keyName = tgt.Selector.Root
		} else {
			return "", "", fmt.Errorf("unsupported struct field key")
		}
		name := strings.Title(keyName)
		v, err := c.compileExpr(it.Value)
		if err != nil {
			return "", "", err
		}
		fields[i] = fmt.Sprintf("%s interface{}", name)
		vals[i] = fmt.Sprintf("%s: %s", name, v)
	}
	typ := fmt.Sprintf("struct{ %s }", strings.Join(fields, "; "))
	lit := fmt.Sprintf("%s{%s}", typ, strings.Join(vals, ", "))
	return typ, lit, nil
}

func isListLiteral(e *parser.Expr) bool {
	if e == nil || e.Binary == nil || e.Binary.Left == nil || e.Binary.Left.Value == nil {
		return false
	}
	if e.Binary.Left.Value.Target != nil && e.Binary.Left.Value.Target.List != nil {
		return true
	}
	return false
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

func isIntLiteral(e *parser.Expr) bool {
	if e == nil || e.Binary == nil || e.Binary.Left == nil || e.Binary.Left.Value == nil {
		return false
	}
	tgt := e.Binary.Left.Value.Target
	if tgt != nil && tgt.Lit != nil && tgt.Lit.Int != nil {
		return true
	}
	return false
}

func isStructLiteralExpr(e *parser.Expr) bool {
	if e == nil || e.Binary == nil || e.Binary.Left == nil || e.Binary.Left.Value == nil {
		return false
	}
	tgt := e.Binary.Left.Value.Target
	if tgt != nil && tgt.Struct != nil {
		return true
	}
	if e.Binary.Left.Value.Ops != nil && len(e.Binary.Left.Value.Ops) > 0 {
		op := e.Binary.Left.Value.Ops[len(e.Binary.Left.Value.Ops)-1]
		if op.Cast != nil && op.Cast.Type != nil && op.Cast.Type.Simple != nil {
			return true
		}
	}
	return false
}

func structNameFromExpr(e *parser.Expr) string {
	if e == nil || e.Binary == nil || e.Binary.Left == nil || e.Binary.Left.Value == nil {
		return ""
	}
	tgt := e.Binary.Left.Value.Target
	if tgt != nil && tgt.Struct != nil {
		return tgt.Struct.Name
	}
	if e.Binary.Left.Value.Ops != nil && len(e.Binary.Left.Value.Ops) > 0 {
		op := e.Binary.Left.Value.Ops[len(e.Binary.Left.Value.Ops)-1]
		if op.Cast != nil && op.Cast.Type != nil && op.Cast.Type.Simple != nil {
			return *op.Cast.Type.Simple
		}
	}
	return ""
}
