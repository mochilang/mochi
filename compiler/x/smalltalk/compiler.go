//go:build slow

package smalltalk

import (
	"bytes"
	"fmt"
	"sort"
	"strings"

	"mochi/parser"
)

// Compiler translates a limited subset of Mochi into GNU Smalltalk code.
type Compiler struct {
	buf          bytes.Buffer
	indent       int
	vars         map[string]bool
	needBreak    bool
	needContinue bool
}

// New returns a new Smalltalk compiler.
func New() *Compiler {
	return &Compiler{vars: make(map[string]bool)}
}

// Compile converts the given Mochi program into Smalltalk source code.
func (c *Compiler) Compile(p *parser.Program) ([]byte, error) {
	c.buf.Reset()
	c.indent = 0
	c.vars = make(map[string]bool)
	c.needBreak = false
	c.needContinue = false

	// compile program body into a temporary buffer first so helper
	// definitions can be emitted before any statements
	var body bytes.Buffer
	c.buf = body
	for _, st := range p.Statements {
		if err := c.compileStmt(st); err != nil {
			return nil, err
		}
	}
	bodyBytes := c.buf.Bytes()
	c.buf = bytes.Buffer{}
	c.indent = 0

	vars := collectVars(p.Statements)
	if len(vars) > 0 {
		c.writeln("| " + strings.Join(vars, " ") + " |")
		for _, v := range vars {
			c.vars[v] = true
		}
	}
	if c.needBreak {
		c.writeln("Object subclass: #BreakSignal instanceVariableNames: '' classVariableNames: '' poolDictionaries: '' category: nil!")
	}
	if c.needContinue {
		c.writeln("Object subclass: #ContinueSignal instanceVariableNames: '' classVariableNames: '' poolDictionaries: '' category: nil!")
	}

	c.buf.Write(bodyBytes)
	return c.buf.Bytes(), nil
}

func collectVars(st []*parser.Statement) []string {
	m := map[string]bool{}
	var walk func([]*parser.Statement)
	walk = func(ss []*parser.Statement) {
		for _, s := range ss {
			switch {
			case s.Let != nil:
				m[s.Let.Name] = true
			case s.Var != nil:
				m[s.Var.Name] = true
			case s.Fun != nil:
				m[s.Fun.Name] = true
				walk(s.Fun.Body)
			case s.If != nil:
				walk(s.If.Then)
				if s.If.Else != nil {
					walk(s.If.Else)
				}
				if s.If.ElseIf != nil {
					walk(s.If.ElseIf.Then)
					if s.If.ElseIf.Else != nil {
						walk(s.If.ElseIf.Else)
					}
				}
			case s.While != nil:
				walk(s.While.Body)
			case s.For != nil:
				walk(s.For.Body)
			}
		}
	}
	walk(st)
	names := make([]string, 0, len(m))
	for k := range m {
		names = append(names, k)
	}
	sort.Strings(names)
	return names
}

func (c *Compiler) compileStmt(s *parser.Statement) error {
	switch {
	case s.Let != nil:
		return c.compileLet(s.Let)
	case s.Var != nil:
		return c.compileVar(s.Var)
	case s.Assign != nil:
		return c.compileAssign(s.Assign)
	case s.Fun != nil:
		return c.compileFun(s.Fun)
	case s.Expr != nil:
		expr, err := c.compileExpr(s.Expr.Expr)
		if err != nil {
			return err
		}
		c.writeln(expr + ".")
		return nil
	case s.If != nil:
		return c.compileIf(s.If)
	case s.While != nil:
		return c.compileWhile(s.While)
	case s.For != nil:
		return c.compileFor(s.For)
	case s.Break != nil:
		return c.compileBreak()
	case s.Continue != nil:
		return c.compileContinue()
	case s.Return != nil:
		expr, err := c.compileExpr(s.Return.Value)
		if err != nil {
			return err
		}
		c.writeln(expr)
		return nil
	case s.Type != nil:
		// Struct and enum definitions have no runtime effect in the
		// generated Smalltalk code, so they are ignored.
		return nil
	default:
		return fmt.Errorf("unsupported statement at line %d", s.Pos.Line)
	}
}

func (c *Compiler) compileLet(l *parser.LetStmt) error {
	if l.Value == nil {
		return nil
	}
	val, err := c.compileExpr(l.Value)
	if err != nil {
		return err
	}
	c.writeln(fmt.Sprintf("%s := %s.", l.Name, val))
	return nil
}

func (c *Compiler) compileVar(v *parser.VarStmt) error {
	if v.Value == nil {
		return nil
	}
	val, err := c.compileExpr(v.Value)
	if err != nil {
		return err
	}
	c.writeln(fmt.Sprintf("%s := %s.", v.Name, val))
	return nil
}

func (c *Compiler) compileAssign(a *parser.AssignStmt) error {
	val, err := c.compileExpr(a.Value)
	if err != nil {
		return err
	}

	target := a.Name
	for _, idx := range a.Index {
		ex, err := c.compileExpr(idx.Start)
		if err != nil {
			return err
		}
		target = fmt.Sprintf("%s at: %s", target, ex)
	}
	for _, f := range a.Field {
		target = fmt.Sprintf("%s at: %q", target, f.Name)
	}

	if len(a.Index) > 0 || len(a.Field) > 0 {
		c.writeln(fmt.Sprintf("%s put: %s.", target, val))
	} else {
		c.writeln(fmt.Sprintf("%s := %s.", target, val))
	}
	return nil
}

func (c *Compiler) compileFun(f *parser.FunStmt) error {
	params := make([]string, len(f.Params))
	for i, p := range f.Params {
		params[i] = ":" + p.Name
	}
	body, err := c.blockString(f.Body)
	if err != nil {
		return err
	}
	block := fmt.Sprintf("[%s | %s ]", strings.Join(params, " "), body)
	c.writeln(fmt.Sprintf("%s := %s.", f.Name, block))
	return nil
}

func (c *Compiler) compileIf(i *parser.IfStmt) error {
	cond, err := c.compileExpr(i.Cond)
	if err != nil {
		return err
	}
	c.writeln(fmt.Sprintf("(%s) ifTrue: [", cond))
	c.indent++
	for _, st := range i.Then {
		if err := c.compileStmt(st); err != nil {
			return err
		}
	}
	c.indent--
	if i.ElseIf != nil {
		c.writeln("] ifFalse: [")
		c.indent++
		if err := c.compileIf(i.ElseIf); err != nil {
			return err
		}
		c.indent--
		c.writeln("].")
		return nil
	}
	if i.Else == nil {
		c.writeln("] .")
		return nil
	}
	c.writeln("] ifFalse: [")
	c.indent++
	for _, st := range i.Else {
		if err := c.compileStmt(st); err != nil {
			return err
		}
	}
	c.indent--
	c.writeln("].")
	return nil
}

func (c *Compiler) compileWhile(w *parser.WhileStmt) error {
	cond, err := c.compileExpr(w.Cond)
	if err != nil {
		return err
	}
	brk := hasBreak(w.Body)
	cont := hasContinue(w.Body)
	if brk {
		c.needBreak = true
	}
	if cont {
		c.needContinue = true
	}
	wrap := brk || cont
	if wrap {
		c.writeln("[")
		c.indent++
	}
	c.writeln(fmt.Sprintf("[%s] whileTrue: [", cond))
	c.indent++
	if cont {
		c.writeln("[")
		c.indent++
	}
	for _, st := range w.Body {
		if err := c.compileStmt(st); err != nil {
			return err
		}
	}
	if cont {
		c.indent--
		c.writeln("] on: ContinueSignal do: [:ex | ]")
	}
	c.indent--
	c.writeln("]")
	if wrap {
		c.indent--
		c.writeln("] on: BreakSignal do: [:ex | ].")
	} else {
		c.writeln(".")
	}
	return nil
}

func (c *Compiler) compileFor(f *parser.ForStmt) error {
	brk := hasBreak(f.Body)
	cont := hasContinue(f.Body)
	if brk {
		c.needBreak = true
	}
	if cont {
		c.needContinue = true
	}
	wrap := brk || cont
	if wrap {
		c.writeln("[")
		c.indent++
	}
	if f.RangeEnd != nil {
		start, err := c.compileExpr(f.Source)
		if err != nil {
			return err
		}
		end, err := c.compileExpr(f.RangeEnd)
		if err != nil {
			return err
		}
		c.writeln(fmt.Sprintf("%s to: %s do: [:%s |", start, end, f.Name))
	} else {
		src, err := c.compileExpr(f.Source)
		if err != nil {
			return err
		}
		c.writeln(fmt.Sprintf("%s do: [:%s |", src, f.Name))
	}
	c.indent++
	if cont {
		c.writeln("[")
		c.indent++
	}
	for _, st := range f.Body {
		if err := c.compileStmt(st); err != nil {
			return err
		}
	}
	if cont {
		c.indent--
		c.writeln("] on: ContinueSignal do: [:ex | ]")
	}
	c.indent--
	c.writeln("]")
	if wrap {
		c.indent--
		c.writeln("] on: BreakSignal do: [:ex | ].")
	} else {
		c.writeln(".")
	}
	return nil
}

func (c *Compiler) compileBreak() error {
	c.needBreak = true
	c.writeln("BreakSignal signal")
	return nil
}

func (c *Compiler) compileContinue() error {
	c.needContinue = true
	c.writeln("ContinueSignal signal")
	return nil
}

func (c *Compiler) compileExpr(e *parser.Expr) (string, error) {
	if e == nil || e.Binary == nil {
		return "", fmt.Errorf("empty expression")
	}
	return c.compileBinary(e.Binary)
}

func (c *Compiler) compileBinary(b *parser.BinaryExpr) (string, error) {
	// Convert the flat list of operations into a precedence aware expression
	// using the same precedence levels as the interpreter. This ensures
	// results match Mochi's semantics when translated to Smalltalk which has
	// different operator precedence rules.

	if b == nil {
		return "", fmt.Errorf("nil binary expression")
	}

	operands := []string{}
	ops := []string{}

	first, err := c.compileUnary(b.Left)
	if err != nil {
		return "", err
	}
	operands = append(operands, first)
	for _, p := range b.Right {
		o, err := c.compilePostfix(p.Right)
		if err != nil {
			return "", err
		}
		op := p.Op
		if p.All {
			op = op + "_all"
		}
		ops = append(ops, op)
		operands = append(operands, o)
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

	apply := func(left string, op string, right string) string {
		switch op {
		case "&&":
			return fmt.Sprintf("(%s and: [%s])", left, right)
		case "||":
			return fmt.Sprintf("(%s or: [%s])", left, right)
		case "==":
			return fmt.Sprintf("(%s = %s)", left, right)
		case "!=":
			return fmt.Sprintf("(%s ~= %s)", left, right)
		default:
			return fmt.Sprintf("(%s %s %s)", left, op, right)
		}
	}

	for _, level := range levels {
		for i := 0; i < len(ops); {
			matched := false
			for _, t := range level {
				if ops[i] == t {
					res := apply(operands[i], ops[i], operands[i+1])
					operands[i] = res
					operands = append(operands[:i+1], operands[i+2:]...)
					ops = append(ops[:i], ops[i+1:]...)
					matched = true
					break
				}
			}
			if !matched {
				i++
			}
		}
	}

	if len(operands) != 1 {
		return "", fmt.Errorf("expression reduction failed")
	}
	return operands[0], nil
}

func (c *Compiler) compileUnary(u *parser.Unary) (string, error) {
	val, err := c.compilePostfix(u.Value)
	if err != nil {
		return "", err
	}
	for i := len(u.Ops) - 1; i >= 0; i-- {
		switch u.Ops[i] {
		case "-":
			val = "-" + val
		case "!":
			val = val + " not"
		}
	}
	return val, nil
}

func (c *Compiler) compilePostfix(p *parser.PostfixExpr) (string, error) {
	val, err := c.compilePrimary(p.Target)
	if err != nil {
		return "", err
	}
	for _, op := range p.Ops {
		switch {
		case op.Call != nil:
			args := make([]string, len(op.Call.Args))
			for i, a := range op.Call.Args {
				s, err := c.compileExpr(a)
				if err != nil {
					return "", err
				}
				args[i] = s
			}
			if val == "print" {
				if len(args) == 0 {
					return "", fmt.Errorf("print expects at least 1 arg")
				}
				stmt := "Transcript show: "
				if isStringLiteral(op.Call.Args[0]) {
					stmt += args[0]
				} else {
					stmt += fmt.Sprintf("(%s) printString", args[0])
				}
				for i, a := range args[1:] {
					stmt += "; show: ' '; show: "
					if isStringLiteral(op.Call.Args[i+1]) {
						stmt += a
					} else {
						stmt += fmt.Sprintf("(%s) printString", a)
					}
				}
				val = stmt + "; cr"
			} else {
				call := val
				if len(args) == 0 {
					call += " value"
				} else {
					for _, a := range args {
						call += " value: " + a
					}
				}
				val = call
			}
		case op.Index != nil:
			idx, err := c.compileExpr(op.Index.Start)
			if err != nil {
				return "", err
			}
			val = fmt.Sprintf("%s at: %s", val, idx)
		case op.Field != nil:
			// treat field access like dictionary lookup
			val = fmt.Sprintf("%s at: %q", val, op.Field.Name)
		case op.Cast != nil:
			var tname string
			if op.Cast.Type.Simple != nil {
				tname = *op.Cast.Type.Simple
			}
			switch tname {
			case "int":
				val = fmt.Sprintf("%s asInteger", val)
			case "float":
				val = fmt.Sprintf("%s asFloat", val)
			case "string":
				val = fmt.Sprintf("%s asString", val)
			default:
				// unsupported cast, keep value as is
			}
		default:
			return "", fmt.Errorf("unsupported postfix expression")
		}
	}
	return val, nil
}

func (c *Compiler) compilePrimary(p *parser.Primary) (string, error) {
	switch {
	case p.Selector != nil:
		name := p.Selector.Root
		for _, s := range p.Selector.Tail {
			name += "." + s
		}
		return name, nil
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
		return "{" + strings.Join(elems, ". ") + "}", nil
	case p.Group != nil:
		return c.compileExpr(p.Group)
	case p.Map != nil:
		pairs := make([]string, len(p.Map.Items))
		for i, it := range p.Map.Items {
			var k string
			if id, ok := simpleIdent(it.Key); ok {
				k = fmt.Sprintf("'%s'", id)
			} else {
				var err error
				k, err = c.compileExpr(it.Key)
				if err != nil {
					return "", err
				}
			}
			v, err := c.compileExpr(it.Value)
			if err != nil {
				return "", err
			}
			pairs[i] = fmt.Sprintf("%s -> %s", k, v)
		}
		return "Dictionary from: {" + strings.Join(pairs, ". ") + "}", nil
	case p.Load != nil:
		return c.compileLoadExpr(p.Load)
	case p.Save != nil:
		return c.compileSaveExpr(p.Save)
	case p.Fetch != nil:
		return c.compileFetchExpr(p.Fetch)
	case p.FunExpr != nil:
		params := make([]string, len(p.FunExpr.Params))
		for i, pa := range p.FunExpr.Params {
			params[i] = ":" + pa.Name
		}
		var body string
		if p.FunExpr.ExprBody != nil {
			b, err := c.compileExpr(p.FunExpr.ExprBody)
			if err != nil {
				return "", err
			}
			body = b
		} else {
			b, err := c.blockString(p.FunExpr.BlockBody)
			if err != nil {
				return "", err
			}
			body = b
		}
		return "[" + strings.Join(params, " ") + " | " + body + " ]", nil
	case p.If != nil:
		cond, err := c.compileExpr(p.If.Cond)
		if err != nil {
			return "", err
		}
		th, err := c.compileExpr(p.If.Then)
		if err != nil {
			return "", err
		}
		el := ""
		if p.If.Else != nil {
			el, err = c.compileExpr(p.If.Else)
			if err != nil {
				return "", err
			}
		}
		if el == "" {
			return fmt.Sprintf("(%s) ifTrue: [%s]", cond, th), nil
		}
		return fmt.Sprintf("(%s) ifTrue: [%s] ifFalse: [%s]", cond, th, el), nil
	case p.Call != nil:
		args := make([]string, len(p.Call.Args))
		for i, a := range p.Call.Args {
			s, err := c.compileExpr(a)
			if err != nil {
				return "", err
			}
			args[i] = s
		}
		switch p.Call.Func {
		case "print":
			if len(args) == 0 {
				return "", fmt.Errorf("print expects at least 1 arg")
			}
			stmt := "Transcript show: "
			if isStringLiteral(p.Call.Args[0]) {
				stmt += args[0]
			} else {
				stmt += fmt.Sprintf("(%s) printString", args[0])
			}
			for i, a := range args[1:] {
				stmt += "; show: ' '; show: "
				if isStringLiteral(p.Call.Args[i+1]) {
					stmt += a
				} else {
					stmt += fmt.Sprintf("(%s) printString", a)
				}
			}
			return stmt + "; cr", nil
		case "append":
			if len(args) != 2 {
				return "", fmt.Errorf("append expects 2 args")
			}
			return fmt.Sprintf("%s copyWith: %s", args[0], args[1]), nil
		case "count", "len":
			if len(args) != 1 {
				return "", fmt.Errorf("%s expects 1 arg", p.Call.Func)
			}
			return fmt.Sprintf("(%s size)", args[0]), nil
		case "values":
			if len(args) != 1 {
				return "", fmt.Errorf("values expects 1 arg")
			}
			return fmt.Sprintf("(%s values)", args[0]), nil
		case "sum":
			if len(args) != 1 {
				return "", fmt.Errorf("sum expects 1 arg")
			}
			return fmt.Sprintf("(%s inject: 0 into: [:s :x | s + x])", args[0]), nil
		case "avg":
			if len(args) != 1 {
				return "", fmt.Errorf("avg expects 1 arg")
			}
			return fmt.Sprintf("((%s inject: 0 into: [:s :x | s + x]) / %s size)", args[0], args[0]), nil
		case "min":
			if len(args) != 1 {
				return "", fmt.Errorf("min expects 1 arg")
			}
			return fmt.Sprintf("(%s min)", args[0]), nil
		case "max":
			if len(args) != 1 {
				return "", fmt.Errorf("max expects 1 arg")
			}
			return fmt.Sprintf("(%s max)", args[0]), nil
		case "str":
			if len(args) != 1 {
				return "", fmt.Errorf("str expects 1 arg")
			}
			return fmt.Sprintf("(%s asString)", args[0]), nil
		case "substring", "substr":
			if len(args) != 3 {
				return "", fmt.Errorf("substring expects 3 args")
			}
			return fmt.Sprintf("(%s copyFrom: %s to: %s)", args[0], args[1], args[2]), nil
		default:
			call := p.Call.Func
			if len(args) == 0 {
				call += " value"
			} else {
				for _, a := range args {
					call += " value: " + a
				}
			}
			return call, nil
		}
	case p.Match != nil:
		return c.compileMatchExpr(p.Match)
	case p.Query != nil:
		return c.compileQueryExpr(p.Query)
	default:
		return "", fmt.Errorf("unsupported expression at line %d", p.Pos.Line)
	}
}

func (c *Compiler) compileMatchExpr(m *parser.MatchExpr) (string, error) {
	target, err := c.compileExpr(m.Target)
	if err != nil {
		return "", err
	}
	expr := "nil"
	for i := len(m.Cases) - 1; i >= 0; i-- {
		cs := m.Cases[i]
		res, err := c.compileExpr(cs.Result)
		if err != nil {
			return "", err
		}
		if isUnderscoreExpr(cs.Pattern) {
			expr = res
			continue
		}
		pat, err := c.compileExpr(cs.Pattern)
		if err != nil {
			return "", err
		}
		expr = fmt.Sprintf("(%s = %s) ifTrue: [%s] ifFalse: [%s]", target, pat, res, expr)
	}
	return expr, nil
}

func (c *Compiler) compileQueryExpr(q *parser.QueryExpr) (string, error) {
	if len(q.Joins) == 1 && len(q.Froms) == 0 && q.Group == nil {
		if q.Joins[0].Side != nil && *q.Joins[0].Side == "left" {
			return c.compileLeftJoinSimple(q)
		}
	}
	if q.Group != nil {
		srcs := make([]string, 1+len(q.Froms)+len(q.Joins))
		vars := make([]string, 1+len(q.Froms)+len(q.Joins))

		s, err := c.compileExpr(q.Source)
		if err != nil {
			return "", err
		}
		srcs[0] = s
		vars[0] = q.Var

		for i, f := range q.Froms {
			fs, err := c.compileExpr(f.Src)
			if err != nil {
				return "", err
			}
			srcs[i+1] = fs
			vars[i+1] = f.Var
		}

		joinConds := make([]string, len(q.Joins))
		for i, j := range q.Joins {
			js, err := c.compileExpr(j.Src)
			if err != nil {
				return "", err
			}
			srcs[len(q.Froms)+1+i] = js
			vars[len(q.Froms)+1+i] = j.Var
			jc, err := c.compileExpr(j.On)
			if err != nil {
				return "", err
			}
			joinConds[i] = jc
		}

		cond := ""
		if q.Where != nil {
			cond, err = c.compileExpr(q.Where)
			if err != nil {
				return "", err
			}
		}
		for _, jc := range joinConds {
			if jc == "" {
				continue
			}
			if cond == "" {
				cond = jc
			} else {
				cond = fmt.Sprintf("(%s and: [%s])", cond, jc)
			}
		}

		keyExpr, err := c.compileExpr(q.Group.Exprs[0])
		if err != nil {
			return "", err
		}

		sub := &Compiler{vars: make(map[string]bool)}
		for k := range c.vars {
			sub.vars[k] = true
		}
		sub.vars[q.Group.Name] = true
		valExpr, err := sub.compileExpr(q.Select)
		if err != nil {
			return "", err
		}

		having := ""
		if q.Group.Having != nil {
			havExpr, err := sub.compileExpr(q.Group.Having)
			if err != nil {
				return "", err
			}
			having = havExpr
		}

		sortExpr := ""
		if q.Sort != nil {
			srt, err := sub.compileExpr(q.Sort)
			if err != nil {
				return "", err
			}
			sortExpr = srt
		}

		skipExpr := ""
		if q.Skip != nil {
			sk, err := sub.compileExpr(q.Skip)
			if err != nil {
				return "", err
			}
			skipExpr = sk
		}

		takeExpr := ""
		if q.Take != nil {
			tk, err := sub.compileExpr(q.Take)
			if err != nil {
				return "", err
			}
			takeExpr = tk
		}

		var b strings.Builder
		b.WriteString("[ | groups res |\n")
		b.WriteString("  groups := Dictionary new.\n")

		var loop func(int, string)
		loop = func(i int, indent string) {
			b.WriteString(indent + srcs[i] + " do: [:" + vars[i] + " |\n")
			next := indent + "  "
			if i+1 < len(srcs) {
				loop(i+1, next)
			} else {
				if cond != "" {
					b.WriteString(next + "(" + cond + ") ifTrue: [\n")
					next += "  "
				}
				b.WriteString(next + "| g k |\n")
				b.WriteString(next + "k := " + keyExpr + ".\n")
				b.WriteString(next + "g := groups at: k ifAbsentPut: [OrderedCollection new].\n")
				b.WriteString(next + "g add: " + vars[0] + ".\n")
				if cond != "" {
					next = next[:len(next)-2]
					b.WriteString(next + "]\n")
				}
			}
			b.WriteString(indent + "]\n")
		}
		loop(0, "  ")

		b.WriteString("  res := OrderedCollection new.\n")
		b.WriteString("  groups keysAndValuesDo: [:k :items |\n")
		b.WriteString("    | " + q.Group.Name + " |\n")
		b.WriteString("    " + q.Group.Name + " := Dictionary from: {#key->k. #items->items}.\n")
		if having != "" {
			b.WriteString("    (" + having + ") ifTrue: [\n")
			b.WriteString("      res add: " + valExpr + ".\n")
			b.WriteString("    ].\n")
		} else {
			b.WriteString("    res add: " + valExpr + ".\n")
		}
		b.WriteString("  ].\n")

		if sortExpr != "" {
			keyA := strings.ReplaceAll(sortExpr, q.Group.Name, "a")
			keyB := strings.ReplaceAll(sortExpr, q.Group.Name, "b")
			b.WriteString("  res := res asSortedCollection: [:a :b | " + keyA + " < " + keyB + "].\n")
		}

		if skipExpr != "" || takeExpr != "" {
			start := "1"
			if skipExpr != "" {
				start = "(" + skipExpr + ") + 1"
			}
			end := "res size"
			if takeExpr != "" {
				if skipExpr != "" {
					end = "(" + start + " - 1 + " + takeExpr + ")"
				} else {
					end = takeExpr
				}
			}
			b.WriteString("  res := res copyFrom: " + start + " to: " + end + ".\n")
		}

		if q.Distinct {
			b.WriteString("  res := res asSet asOrderedCollection.\n")
		}

		b.WriteString("  res\n")
		b.WriteString("] value")
		return b.String(), nil
	}

	srcs := make([]string, 1+len(q.Froms)+len(q.Joins))
	vars := make([]string, 1+len(q.Froms)+len(q.Joins))

	s, err := c.compileExpr(q.Source)
	if err != nil {
		return "", err
	}
	srcs[0] = s
	vars[0] = q.Var

	for i, f := range q.Froms {
		fs, err := c.compileExpr(f.Src)
		if err != nil {
			return "", err
		}
		srcs[i+1] = fs
		vars[i+1] = f.Var
	}

	joinConds := make([]string, len(q.Joins))
	for i, j := range q.Joins {
		js, err := c.compileExpr(j.Src)
		if err != nil {
			return "", err
		}
		srcs[len(q.Froms)+1+i] = js
		vars[len(q.Froms)+1+i] = j.Var
		jc, err := c.compileExpr(j.On)
		if err != nil {
			return "", err
		}
		joinConds[i] = jc
	}

	cond := ""
	if q.Where != nil {
		cond, err = c.compileExpr(q.Where)
		if err != nil {
			return "", err
		}
	}
	for _, jc := range joinConds {
		if jc == "" {
			continue
		}
		if cond == "" {
			cond = jc
		} else {
			cond = fmt.Sprintf("(%s and: [%s])", cond, jc)
		}
	}

	sel, err := c.compileExpr(q.Select)
	if err != nil {
		return "", err
	}

	var b strings.Builder
	b.WriteString("[ | tmp |\n")
	b.WriteString("  tmp := OrderedCollection new.\n")

	var loop func(int, string)
	loop = func(i int, indent string) {
		b.WriteString(indent + srcs[i] + " do: [:" + vars[i] + " |\n")
		next := indent + "  "
		if i+1 < len(srcs) {
			loop(i+1, next)
		} else {
			if cond != "" {
				b.WriteString(next + "(" + cond + ") ifTrue: [\n")
				b.WriteString(next + "  tmp add: " + sel + ".\n")
				b.WriteString(next + "].\n")
			} else {
				b.WriteString(next + "tmp add: " + sel + ".\n")
			}
		}
		b.WriteString(indent + "].\n")
	}
	loop(0, "  ")

	if q.Sort != nil {
		key, err := c.compileExpr(q.Sort)
		if err != nil {
			return "", err
		}
		keyA := strings.ReplaceAll(key, vars[0], "a")
		keyB := strings.ReplaceAll(key, vars[0], "b")
		b.WriteString("  tmp := tmp asSortedCollection: [:a :b | " + keyA + " < " + keyB + "].\n")
	}

	if q.Skip != nil || q.Take != nil {
		start := "1"
		if q.Skip != nil {
			st, err := c.compileExpr(q.Skip)
			if err != nil {
				return "", err
			}
			start = "(" + st + ") + 1"
		}
		end := "tmp size"
		if q.Take != nil {
			tk, err := c.compileExpr(q.Take)
			if err != nil {
				return "", err
			}
			if q.Skip != nil {
				end = "(" + start + " - 1 + " + tk + ")"
			} else {
				end = tk
			}
		}
		b.WriteString("  tmp := tmp copyFrom: " + start + " to: " + end + ".\n")
	}

	if q.Distinct {
		b.WriteString("  tmp := tmp asSet asOrderedCollection.\n")
	}

	b.WriteString("  tmp\n")
	b.WriteString("] value")
	return b.String(), nil
}

func (c *Compiler) compileLeftJoinSimple(q *parser.QueryExpr) (string, error) {
	j := q.Joins[0]
	left, err := c.compileExpr(q.Source)
	if err != nil {
		return "", err
	}
	right, err := c.compileExpr(j.Src)
	if err != nil {
		return "", err
	}
	onCond, err := c.compileExpr(j.On)
	if err != nil {
		return "", err
	}
	cond := ""
	if q.Where != nil {
		cond, err = c.compileExpr(q.Where)
		if err != nil {
			return "", err
		}
	}
	sel, err := c.compileExpr(q.Select)
	if err != nil {
		return "", err
	}

	var b strings.Builder
	b.WriteString("[ | tmp |\n")
	b.WriteString("  tmp := OrderedCollection new.\n")
	b.WriteString(fmt.Sprintf("  %s do: [:%s |\n", left, q.Var))
	b.WriteString("    | matched |\n")
	b.WriteString("    matched := false.\n")
	b.WriteString(fmt.Sprintf("    %s do: [:%s |\n", right, j.Var))
	b.WriteString(fmt.Sprintf("      (%s) ifTrue: [\n", onCond))
	b.WriteString("        matched := true.\n")
	if cond != "" {
		b.WriteString(fmt.Sprintf("        (%s) ifTrue: [ tmp add: %s ].\n", cond, sel))
	} else {
		b.WriteString(fmt.Sprintf("        tmp add: %s.\n", sel))
	}
	b.WriteString("      ].\n")
	b.WriteString("    ].\n")
	b.WriteString("    matched ifFalse: [\n")
	b.WriteString(fmt.Sprintf("      %s := nil.\n", j.Var))
	if cond != "" {
		b.WriteString(fmt.Sprintf("      (%s) ifTrue: [ tmp add: %s ].\n", cond, sel))
	} else {
		b.WriteString(fmt.Sprintf("      tmp add: %s.\n", sel))
	}
	b.WriteString("    ].\n")
	b.WriteString("  ].\n")

	if q.Sort != nil {
		key, err := c.compileExpr(q.Sort)
		if err != nil {
			return "", err
		}
		keyA := strings.ReplaceAll(key, q.Var, "a")
		keyB := strings.ReplaceAll(key, q.Var, "b")
		b.WriteString("  tmp := tmp asSortedCollection: [:a :b | " + keyA + " < " + keyB + "].\n")
	}

	if q.Skip != nil || q.Take != nil {
		start := "1"
		if q.Skip != nil {
			sk, err := c.compileExpr(q.Skip)
			if err != nil {
				return "", err
			}
			start = "(" + sk + ") + 1"
		}
		end := "tmp size"
		if q.Take != nil {
			tk, err := c.compileExpr(q.Take)
			if err != nil {
				return "", err
			}
			if q.Skip != nil {
				end = "(" + start + " - 1 + " + tk + ")"
			} else {
				end = tk
			}
		}
		b.WriteString("  tmp := tmp copyFrom: " + start + " to: " + end + ".\n")
	}

	if q.Distinct {
		b.WriteString("  tmp := tmp asSet asOrderedCollection.\n")
	}

	b.WriteString("  tmp\n")
	b.WriteString("] value")
	return b.String(), nil
}

func (c *Compiler) compileLoadExpr(l *parser.LoadExpr) (string, error) {
	path := "nil"
	if l.Path != nil {
		s := strings.ReplaceAll(*l.Path, "'", "''")
		path = "'" + s + "'"
	}
	opts := "nil"
	if l.With != nil {
		v, err := c.compileExpr(l.With)
		if err != nil {
			return "", err
		}
		opts = v
	}
	expr := fmt.Sprintf("(Main _load: %s opts: %s)", path, opts)
	if l.Type != nil && l.Type.Simple != nil {
		expr = fmt.Sprintf("(%s collect: [:it | %s newFrom: it])", expr, *l.Type.Simple)
	}
	return expr, nil
}

func (c *Compiler) compileSaveExpr(s *parser.SaveExpr) (string, error) {
	src, err := c.compileExpr(s.Src)
	if err != nil {
		return "", err
	}
	path := "nil"
	if s.Path != nil {
		p := strings.ReplaceAll(*s.Path, "'", "''")
		path = "'" + p + "'"
	}
	opts := "nil"
	if s.With != nil {
		v, err := c.compileExpr(s.With)
		if err != nil {
			return "", err
		}
		opts = v
	}
	return fmt.Sprintf("(Main _save: %s path: %s opts: %s)", src, path, opts), nil
}

func (c *Compiler) compileFetchExpr(f *parser.FetchExpr) (string, error) {
	url, err := c.compileExpr(f.URL)
	if err != nil {
		return "", err
	}
	opts := "nil"
	if f.With != nil {
		v, err := c.compileExpr(f.With)
		if err != nil {
			return "", err
		}
		opts = v
	}
	return fmt.Sprintf("(Main _fetch: %s opts: %s)", url, opts), nil
}

func (c *Compiler) compileLiteral(l *parser.Literal) string {
	switch {
	case l.Int != nil:
		return fmt.Sprintf("%d", *l.Int)
	case l.Str != nil:
		s := strings.ReplaceAll(*l.Str, "'", "''")
		return fmt.Sprintf("'%s'", s)
	case l.Float != nil:
		return fmt.Sprintf("%g", *l.Float)
	case l.Bool != nil:
		if bool(*l.Bool) {
			return "true"
		}
		return "false"
	default:
		return "nil"
	}
}

func isStringLiteral(e *parser.Expr) bool {
	if e == nil || e.Binary == nil || len(e.Binary.Right) > 0 {
		return false
	}
	u := e.Binary.Left
	if len(u.Ops) > 0 || u.Value == nil || len(u.Value.Ops) > 0 {
		return false
	}
	return u.Value.Target != nil && u.Value.Target.Lit != nil && u.Value.Target.Lit.Str != nil
}

func (c *Compiler) blockString(stmts []*parser.Statement) (string, error) {
	sub := &Compiler{}
	for _, st := range stmts {
		if err := sub.compileStmt(st); err != nil {
			return "", err
		}
	}
	if sub.needBreak {
		c.needBreak = true
	}
	if sub.needContinue {
		c.needContinue = true
	}
	return strings.TrimSpace(sub.buf.String()), nil
}

func hasBreak(st []*parser.Statement) bool {
	for _, s := range st {
		switch {
		case s.Break != nil:
			return true
		case s.For != nil:
			if hasBreak(s.For.Body) {
				return true
			}
		case s.While != nil:
			if hasBreak(s.While.Body) {
				return true
			}
		case s.If != nil:
			if hasBreak(s.If.Then) || hasBreakIf(s.If.ElseIf) || hasBreak(s.If.Else) {
				return true
			}
		}
	}
	return false
}

func hasBreakIf(i *parser.IfStmt) bool {
	if i == nil {
		return false
	}
	if hasBreak(i.Then) || hasBreak(i.Else) {
		return true
	}
	return hasBreakIf(i.ElseIf)
}

func hasContinue(st []*parser.Statement) bool {
	for _, s := range st {
		switch {
		case s.Continue != nil:
			return true
		case s.For != nil:
			if hasContinue(s.For.Body) {
				return true
			}
		case s.While != nil:
			if hasContinue(s.While.Body) {
				return true
			}
		case s.If != nil:
			if hasContinue(s.If.Then) || hasContinueIf(s.If.ElseIf) || hasContinue(s.If.Else) {
				return true
			}
		}
	}
	return false
}

func hasContinueIf(i *parser.IfStmt) bool {
	if i == nil {
		return false
	}
	if hasContinue(i.Then) || hasContinue(i.Else) {
		return true
	}
	return hasContinueIf(i.ElseIf)
}

// simpleIdent checks if e is a bare identifier expression and returns its name.
func simpleIdent(e *parser.Expr) (string, bool) {
	if e == nil || e.Binary == nil || len(e.Binary.Right) > 0 {
		return "", false
	}
	u := e.Binary.Left
	if len(u.Ops) > 0 || u.Value == nil || len(u.Value.Ops) > 0 {
		return "", false
	}
	if sel := u.Value.Target; sel != nil && sel.Selector != nil && len(sel.Selector.Tail) == 0 {
		return sel.Selector.Root, true
	}
	return "", false
}

func isUnderscoreExpr(e *parser.Expr) bool {
	if e == nil || e.Binary == nil || len(e.Binary.Right) != 0 {
		return false
	}
	u := e.Binary.Left
	if len(u.Ops) != 0 || u.Value == nil || len(u.Value.Ops) != 0 {
		return false
	}
	return u.Value.Target != nil && u.Value.Target.Selector != nil &&
		u.Value.Target.Selector.Root == "_" && len(u.Value.Target.Selector.Tail) == 0
}

func (c *Compiler) writeln(s string) {
	for i := 0; i < c.indent; i++ {
		c.buf.WriteString("  ")
	}
	c.buf.WriteString(s)
	c.buf.WriteByte('\n')
}
