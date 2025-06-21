package stcode

import (
	"bytes"
	"fmt"
	"sort"
	"strings"

	"mochi/parser"
	"mochi/types"
)

// Compiler translates a Mochi AST into GNU Smalltalk source code.
type Compiler struct {
	buf          bytes.Buffer
	indent       int
	env          *types.Env
	funParams    map[string][]string
	needCount    bool
	needAvg      bool
	needInput    bool
	needReduce   bool
	needBreak    bool
	needContinue bool
}

// New creates a new Smalltalk compiler instance.
func New(env *types.Env) *Compiler {
	return &Compiler{env: env, funParams: make(map[string][]string)}
}

func (c *Compiler) writeln(s string)         { c.writeIndent(); c.buf.WriteString(s); c.buf.WriteByte('\n') }
func (c *Compiler) writelnNoIndent(s string) { c.buf.WriteString(s); c.buf.WriteByte('\n') }
func (c *Compiler) writeIndent() {
	for i := 0; i < c.indent; i++ {
		c.buf.WriteByte('\t')
	}
}

// Compile generates Smalltalk code for prog.
func (c *Compiler) Compile(prog *parser.Program) ([]byte, error) {
	c.buf.Reset()
	c.needCount = false
	c.needAvg = false
	c.needInput = false
	c.needReduce = false
	c.needBreak = false
	c.needContinue = false

	orig := c.buf

	// compile type declarations
	c.buf = bytes.Buffer{}
	for _, s := range prog.Statements {
		if s.Type != nil {
			if err := c.compileTypeDecl(s.Type); err != nil {
				return nil, err
			}
		}
	}
	typeCode := append([]byte(nil), c.buf.Bytes()...)

	// compile functions
	c.buf = bytes.Buffer{}
	for _, s := range prog.Statements {
		if s.Fun != nil {
			if err := c.compileFun(s.Fun); err != nil {
				return nil, err
			}
			c.writeln("")
		}
	}
	funCode := append([]byte(nil), c.buf.Bytes()...)

	// compile test blocks
	c.buf = bytes.Buffer{}
	testNames := []string{}
	for _, s := range prog.Statements {
		if s.Test != nil {
			if err := c.compileTestBlock(s.Test); err != nil {
				return nil, err
			}
			testNames = append(testNames, "test_"+sanitizeName(s.Test.Name))
			c.writeln("")
		}
	}
	testCode := append([]byte(nil), c.buf.Bytes()...)

	// compile main statements
	c.buf = bytes.Buffer{}
	for _, s := range prog.Statements {
		if s.Fun != nil || s.Type != nil || s.Test != nil {
			continue
		}
		if err := c.compileStmt(s); err != nil {
			return nil, err
		}
	}
	for _, name := range testNames {
		c.writeln(fmt.Sprintf("Main %s.", name))
	}
	mainCode := append([]byte(nil), c.buf.Bytes()...)

	// assemble
	c.buf = orig
	c.writeln("Object subclass: #Main instanceVariableNames: '' classVariableNames: '' poolDictionaries: '' category: nil!")
	c.writeln("")
	c.buf.Write(typeCode)
	c.buf.Write(funCode)
	c.buf.Write(testCode)
	if c.needCount || c.needAvg || c.needInput || c.needReduce || c.needBreak || c.needContinue {
		c.emitHelpers()
	}
	c.writelnNoIndent("!!")
	c.buf.Write(mainCode)

	return c.buf.Bytes(), nil
}

func (c *Compiler) compileFun(fn *parser.FunStmt) error {
	header := fn.Name + ": " + fn.Params[0].Name
	names := []string{fn.Params[0].Name}
	for _, p := range fn.Params[1:] {
		header += " " + p.Name + ": " + p.Name
		names = append(names, p.Name)
	}
	c.funParams[fn.Name] = names
	vars := collectVars(fn.Body)
	if c.indent > 0 {
		vars = append(vars, fn.Name)
	}

	if c.indent > 0 {
		blockHeader := "[:" + fn.Params[0].Name
		for _, p := range fn.Params[1:] {
			blockHeader += " :" + p.Name
		}
		blockHeader += " |"
		if len(vars) > 0 {
			blockHeader += " | " + strings.Join(vars, " ") + " |"
		}
		c.writeln(fmt.Sprintf("%s := %s", fn.Name, blockHeader))
		c.indent++
		for _, st := range fn.Body {
			if err := c.compileStmt(st); err != nil {
				return err
			}
		}
		c.indent--
		c.writeln("].")
		return nil
	}

	c.writeln("!Main class methodsFor: 'mochi'!")
	if len(vars) > 0 {
		c.writeln(header + " | " + strings.Join(vars, " ") + " |")
	} else {
		c.writeln(header)
	}
	c.indent++
	for _, st := range fn.Body {
		if err := c.compileStmt(st); err != nil {
			return err
		}
	}
	c.indent--
	c.writelnNoIndent("!")
	return nil
}

func (c *Compiler) compileTypeDecl(t *parser.TypeDecl) error {
	if len(t.Variants) > 0 {
		// union types not yet supported
		return nil
	}
	fields := make([]string, 0, len(t.Members))
	for _, m := range t.Members {
		if m.Field != nil {
			fields = append(fields, m.Field.Name)
		}
	}
	if len(fields) == 0 {
		return nil
	}

	c.writeln("!Main class methodsFor: 'types'!")
	header := "new" + t.Name + ": " + fields[0]
	for _, f := range fields[1:] {
		header += " " + f + ": " + f
	}
	c.writeln(header + " | dict |")
	c.indent++
	c.writeln("dict := Dictionary new.")
	for _, f := range fields {
		c.writeln(fmt.Sprintf("dict at: '%s' put: %s.", f, f))
	}
	c.writeln("^ dict")
	c.indent--
	c.writelnNoIndent("!")
	return nil
}

func collectVars(stmts []*parser.Statement) []string {
	set := map[string]bool{}
	var visit func([]*parser.Statement)
	visit = func(list []*parser.Statement) {
		for _, s := range list {
			switch {
			case s.Let != nil:
				set[s.Let.Name] = true
			case s.Var != nil:
				set[s.Var.Name] = true
			case s.For != nil:
				set[s.For.Name] = true
				visit(s.For.Body)
			case s.While != nil:
				visit(s.While.Body)
			case s.If != nil:
				visit(s.If.Then)
				if s.If.ElseIf != nil {
					visit(s.If.ElseIf.Then)
				}
				visit(s.If.Else)
			case s.Fun != nil:
				set[s.Fun.Name] = true
				visit(s.Fun.Body)
			}
		}
	}
	visit(stmts)
	vars := make([]string, 0, len(set))
	for v := range set {
		vars = append(vars, v)
	}
	sort.Strings(vars)
	return vars
}

func (c *Compiler) compileStmt(s *parser.Statement) error {
	switch {
	case s.Let != nil:
		val, err := c.compileExpr(s.Let.Value)
		if err != nil {
			return err
		}
		c.writeln(fmt.Sprintf("%s := %s.", s.Let.Name, val))
	case s.Var != nil:
		if err := c.compileVar(s.Var); err != nil {
			return err
		}
	case s.Return != nil:
		val, err := c.compileExpr(s.Return.Value)
		if err != nil {
			return err
		}
		c.writeln("^ " + val)
	case s.For != nil:
		return c.compileFor(s.For)
	case s.While != nil:
		return c.compileWhile(s.While)
	case s.Break != nil:
		c.needBreak = true
		c.writeln("BreakSignal signal")
	case s.Continue != nil:
		c.needContinue = true
		c.writeln("ContinueSignal signal")
	case s.If != nil:
		return c.compileIf(s.If)
	case s.Test != nil:
		return c.compileTestBlock(s.Test)
	case s.Expect != nil:
		return c.compileExpect(s.Expect)
	case s.Assign != nil:
		return c.compileAssign(s.Assign)
	case s.Fun != nil:
		return c.compileFun(s.Fun)
	case s.Expr != nil:
		expr, err := c.compileExpr(s.Expr.Expr)
		if err != nil {
			return err
		}
		if expr != "" {
			c.writeln(expr + ".")
		}
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

	if f.RangeEnd == nil {
		src, err := c.compileExpr(f.Source)
		if err != nil {
			return err
		}
		c.writeln(fmt.Sprintf("(%s) do: [:%s |", src, f.Name))
	} else {
		start, err := c.compileExpr(f.Source)
		if err != nil {
			return err
		}
		end, err := c.compileExpr(f.RangeEnd)
		if err != nil {
			return err
		}
		c.writeln(fmt.Sprintf("%s to: %s - 1 do: [:%s |", start, end, f.Name))
	}
	c.indent++
	if wrap {
		c.writeln("[")
		c.indent++
	}
	for _, st := range f.Body {
		if err := c.compileStmt(st); err != nil {
			return err
		}
	}
	if wrap {
		c.indent--
		c.writeln("] on: ContinueSignal do: [:ex | ]")
	}
	c.indent--
	c.writeln("]")
	if wrap {
		c.indent--
		c.writeln("] on: BreakSignal do: [:ex | ]")
	}
	c.writeln(".")
	return nil
}

func (c *Compiler) compileWhile(w *parser.WhileStmt) error {
	brk := hasBreak(w.Body)
	cont := hasContinue(w.Body)
	if brk {
		c.needBreak = true
	}
	if cont {
		c.needContinue = true
	}

	cond, err := c.compileExpr(w.Cond)
	if err != nil {
		return err
	}

	wrap := brk || cont
	if wrap {
		c.writeln("[")
		c.indent++
	}

	c.writeln("[" + cond + "] whileTrue: [")
	c.indent++
	if wrap {
		c.writeln("[")
		c.indent++
	}
	for _, st := range w.Body {
		if err := c.compileStmt(st); err != nil {
			return err
		}
	}
	if wrap {
		c.indent--
		c.writeln("] on: ContinueSignal do: [:ex | ]")
	}
	c.indent--
	c.writeln("]")
	if wrap {
		c.indent--
		c.writeln("] on: BreakSignal do: [:ex | ]")
	}
	c.writeln(".")
	return nil
}

func (c *Compiler) compileTestBlock(t *parser.TestBlock) error {
	name := "test_" + sanitizeName(t.Name)
	c.writeln("!Main class methodsFor: 'tests'!")
	c.writeln(name)
	c.indent++
	for _, st := range t.Body {
		if err := c.compileStmt(st); err != nil {
			return err
		}
	}
	c.indent--
	c.writelnNoIndent("!")
	return nil
}

func (c *Compiler) compileExpect(e *parser.ExpectStmt) error {
	expr, err := c.compileExpr(e.Value)
	if err != nil {
		return err
	}
	c.writeln(fmt.Sprintf("(%s) ifFalse: [ self error: 'expect failed' ]", expr))
	return nil
}

func (c *Compiler) compileAssign(a *parser.AssignStmt) error {
	val, err := c.compileExpr(a.Value)
	if err != nil {
		return err
	}
	if len(a.Index) > 0 {
		idx, err := c.compileExpr(a.Index[0].Start)
		if err != nil {
			return err
		}
		if t, err := c.env.GetVar(a.Name); err == nil {
			if _, ok := t.(types.MapType); ok {
				c.writeln(fmt.Sprintf("%s at: %s put: %s.", a.Name, idx, val))
				return nil
			}
		}
		c.writeln(fmt.Sprintf("%s at: %s + 1 put: %s.", a.Name, idx, val))
		return nil
	}
	c.writeln(fmt.Sprintf("%s := %s.", a.Name, val))
	return nil
}

func (c *Compiler) compileVar(v *parser.VarStmt) error {
	val := "nil"
	if v.Value != nil {
		expr, err := c.compileExpr(v.Value)
		if err != nil {
			return err
		}
		val = expr
	}
	c.writeln(fmt.Sprintf("%s := %s.", v.Name, val))
	return nil
}

func (c *Compiler) compileIf(stmt *parser.IfStmt) error {
	cond, err := c.compileExpr(stmt.Cond)
	if err != nil {
		return err
	}
	c.writeln("(" + cond + ") ifTrue: [")
	c.indent++
	for _, st := range stmt.Then {
		if err := c.compileStmt(st); err != nil {
			return err
		}
	}
	c.indent--
	if len(stmt.Else) > 0 {
		c.writeln("] ifFalse: [")
		c.indent++
		for _, st := range stmt.Else {
			if err := c.compileStmt(st); err != nil {
				return err
			}
		}
		c.indent--
		c.writeln("]")
		c.writeln(".")
	} else {
		c.writeln("]")
		c.writeln(".")
	}
	return nil
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
	if ie.ElseIf != nil {
		elseExpr, err := c.compileIfExpr(ie.ElseIf)
		if err != nil {
			return "", err
		}
		return fmt.Sprintf("((%s) ifTrue: [%s] ifFalse: [%s])", cond, thenExpr, elseExpr), nil
	}
	if ie.Else != nil {
		elseExpr, err := c.compileExpr(ie.Else)
		if err != nil {
			return "", err
		}
		return fmt.Sprintf("((%s) ifTrue: [%s] ifFalse: [%s])", cond, thenExpr, elseExpr), nil
	}
	return fmt.Sprintf("((%s) ifTrue: [%s])", cond, thenExpr), nil
}

func (c *Compiler) compileExpr(e *parser.Expr) (string, error) {
	if e == nil {
		return "", nil
	}
	return c.compileBinary(e.Binary)
}

func (c *Compiler) compileBinary(b *parser.BinaryExpr) (string, error) {
	left, err := c.compileUnary(b.Left)
	if err != nil {
		return "", err
	}
	expr := left
	leftList := isListUnary(b.Left, c.env)
	leftStr := isStringUnary(b.Left, c.env)
	for _, op := range b.Right {
		right, err := c.compilePostfix(op.Right)
		if err != nil {
			return "", err
		}
		rlist := isListPostfix(op.Right, c.env)
		rstr := isStringPostfix(op.Right, c.env)
		switch op.Op {
		case "+":
			if leftList || rlist {
				expr = fmt.Sprintf("((%s) , (%s))", expr, right)
				leftList = true
				leftStr = false
			} else if leftStr || rstr {
				expr = fmt.Sprintf("((%s) , (%s))", expr, right)
				leftStr = true
				leftList = false
			} else {
				expr = fmt.Sprintf("(%s + %s)", expr, right)
				leftList = false
				leftStr = false
			}
		case "in":
			if rlist || rstr {
				expr = fmt.Sprintf("(%s includes: %s)", right, left)
			} else if isMapPostfix(op.Right, c.env) {
				expr = fmt.Sprintf("(%s includesKey: %s)", right, left)
			} else {
				expr = fmt.Sprintf("(%s includes: %s)", right, left)
			}
			leftList = false
			leftStr = false
		case "&&":
			expr = fmt.Sprintf("(%s and: [%s])", expr, right)
			leftList = false
			leftStr = false
		case "||":
			expr = fmt.Sprintf("(%s or: [%s])", expr, right)
			leftList = false
			leftStr = false
		default:
			expr = fmt.Sprintf("(%s %s %s)", expr, mapOp(op.Op), right)
			leftList = false
			leftStr = false
		}
	}
	return expr, nil
}

func (c *Compiler) compileUnary(u *parser.Unary) (string, error) {
	if u == nil {
		return "", nil
	}
	val, err := c.compilePostfix(u.Value)
	if err != nil {
		return "", err
	}
	for i := len(u.Ops) - 1; i >= 0; i-- {
		switch u.Ops[i] {
		case "!":
			val = fmt.Sprintf("(%s) not", val)
		case "-":
			val = fmt.Sprintf("(%s negated)", val)
		default:
			val = fmt.Sprintf("%s%s", u.Ops[i], val)
		}
	}
	return val, nil
}

func (c *Compiler) compilePostfix(p *parser.PostfixExpr) (string, error) {
	expr, err := c.compilePrimary(p.Target)
	if err != nil {
		return "", err
	}
	for _, op := range p.Ops {
		if op.Index != nil {
			if op.Index.Colon != nil {
				start := "1"
				if op.Index.Start != nil {
					s, err := c.compileExpr(op.Index.Start)
					if err != nil {
						return "", err
					}
					start = fmt.Sprintf("(%s + 1)", s)
				}
				end := fmt.Sprintf("%s size", expr)
				if op.Index.End != nil {
					e, err := c.compileExpr(op.Index.End)
					if err != nil {
						return "", err
					}
					end = e
				}
				expr = fmt.Sprintf("(%s copyFrom: %s to: %s)", expr, start, end)
			} else {
				idx, err := c.compileExpr(op.Index.Start)
				if err != nil {
					return "", err
				}
				targetOnly := &parser.PostfixExpr{Target: p.Target}
				if isMapPostfix(targetOnly, c.env) {
					expr = fmt.Sprintf("(%s at: %s)", expr, idx)
				} else {
					expr = fmt.Sprintf("(%s at: %s + 1)", expr, idx)
				}
			}
		}
	}
	return expr, nil
}

func (c *Compiler) compilePrimary(p *parser.Primary) (string, error) {
	switch {
	case p.Lit != nil:
		return c.compileLiteral(p.Lit)
	case p.FunExpr != nil:
		return c.compileFunExpr(p.FunExpr)
	case p.List != nil:
		if len(p.List.Elems) == 0 {
			return "Array new", nil
		}
		elems := make([]string, len(p.List.Elems))
		for i, e := range p.List.Elems {
			v, err := c.compileExpr(e)
			if err != nil {
				return "", err
			}
			elems[i] = v
		}
		return "Array with: " + strings.Join(elems, " with: "), nil
	case p.Map != nil:
		return c.compileMapLiteral(p.Map)
	case p.Struct != nil:
		return c.compileStructLiteral(p.Struct)
	case p.Call != nil:
		return c.compileCallExpr(p.Call)
	case p.If != nil:
		return c.compileIfExpr(p.If)
	case p.Group != nil:
		inner, err := c.compileExpr(p.Group)
		if err != nil {
			return "", err
		}
		return "(" + inner + ")", nil
	case p.Selector != nil:
		expr := p.Selector.Root
		for _, t := range p.Selector.Tail {
			expr = fmt.Sprintf("%s at: '%s'", expr, t)
		}
		return expr, nil
	default:
		return "", fmt.Errorf("unsupported expression")
	}
}

func (c *Compiler) compileCallExpr(call *parser.CallExpr) (string, error) {
	name := call.Func
	args := make([]string, len(call.Args))
	for i, a := range call.Args {
		v, err := c.compileExpr(a)
		if err != nil {
			return "", err
		}
		args[i] = v
	}
	switch name {
	case "print":
		if len(args) != 1 {
			return "", fmt.Errorf("print expects 1 arg")
		}
		return fmt.Sprintf("(%s) displayOn: Transcript. Transcript cr", args[0]), nil
	case "len":
		if len(args) != 1 {
			return "", fmt.Errorf("len expects 1 arg")
		}
		return fmt.Sprintf("(%s) size", args[0]), nil
	case "now":
		if len(args) != 0 {
			return "", fmt.Errorf("now expects no args")
		}
		return "Time nanosecondClock", nil
	case "json":
		if len(args) != 1 {
			return "", fmt.Errorf("json expects 1 arg")
		}
		return fmt.Sprintf("(%s toJSON) displayOn: Transcript. Transcript cr", args[0]), nil
	case "str":
		if len(args) != 1 {
			return "", fmt.Errorf("str expects 1 arg")
		}
		return fmt.Sprintf("(%s printString)", args[0]), nil
	case "count":
		if len(args) != 1 {
			return "", fmt.Errorf("count expects 1 arg")
		}
		c.needCount = true
		return fmt.Sprintf("(Main __count: %s)", args[0]), nil
	case "avg":
		if len(args) != 1 {
			return "", fmt.Errorf("avg expects 1 arg")
		}
		c.needAvg = true
		return fmt.Sprintf("(Main __avg: %s)", args[0]), nil
	case "append":
		if len(args) != 2 {
			return "", fmt.Errorf("append expects 2 args")
		}
		return fmt.Sprintf("(%s copyWith: %s)", args[0], args[1]), nil
	case "eval":
		if len(args) != 1 {
			return "", fmt.Errorf("eval expects 1 arg")
		}
		return fmt.Sprintf("(Compiler evaluate: %s)", args[0]), nil
	case "reduce":
		if len(args) != 3 {
			return "", fmt.Errorf("reduce expects 3 args")
		}
		c.needReduce = true
		return fmt.Sprintf("(Main __reduce: %s fn: %s init: %s)", args[0], args[1], args[2]), nil
	case "input":
		if len(args) != 0 {
			return "", fmt.Errorf("input expects no args")
		}
		c.needInput = true
		return "(Main __input)", nil
	default:
		params, ok := c.funParams[name]
		if !ok {
			call := name
			for _, a := range args {
				call += " value: " + a
			}
			return call, nil
		}
		parts := []string{"Main", name + ":"}
		for i, p := range params {
			arg := fmt.Sprintf("(%s)", args[i])
			if i == 0 {
				parts = append(parts, arg)
			} else {
				parts = append(parts, fmt.Sprintf("%s: %s", p, arg))
			}
		}
		return "(" + strings.Join(parts, " ") + ")", nil
	}
}

func mapOp(op string) string {
	switch op {
	case "==":
		return "="
	case "!=":
		return "~="
	case "%":
		return "\\\\"
	}
	return op
}

func (c *Compiler) compileLiteral(l *parser.Literal) (string, error) {
	switch {
	case l.Int != nil:
		return fmt.Sprintf("%d", *l.Int), nil
	case l.Float != nil:
		return fmt.Sprintf("%f", *l.Float), nil
	case l.Bool != nil:
		if *l.Bool {
			return "true", nil
		}
		return "false", nil
	case l.Str != nil:
		s := strings.ReplaceAll(*l.Str, "'", "''")
		return "'" + s + "'", nil
	}
	return "", fmt.Errorf("unknown literal")
}

func (c *Compiler) compileFunExpr(fn *parser.FunExpr) (string, error) {
	var b strings.Builder
	if len(fn.Params) > 0 {
		b.WriteString("[:" + fn.Params[0].Name)
		for _, p := range fn.Params[1:] {
			b.WriteString(" :" + p.Name)
		}
		b.WriteString(" |")
	} else {
		b.WriteString("[|")
	}
	vars := []string{}
	if len(fn.BlockBody) > 0 {
		vars = collectVars(fn.BlockBody)
	}
	if len(vars) > 0 {
		b.WriteString(" | " + strings.Join(vars, " ") + " |")
	}
	if fn.ExprBody != nil {
		expr, err := c.compileExpr(fn.ExprBody)
		if err != nil {
			return "", err
		}
		b.WriteString(" " + expr + " ]")
		return b.String(), nil
	}
	b.WriteByte('\n')
	sub := &Compiler{env: c.env, funParams: c.funParams}
	sub.indent = 1
	for _, s := range fn.BlockBody {
		if err := sub.compileStmt(s); err != nil {
			return "", err
		}
	}
	b.WriteString(sub.buf.String())
	b.WriteString("]")
	return b.String(), nil
}

func (c *Compiler) compileMapLiteral(m *parser.MapLiteral) (string, error) {
	if len(m.Items) == 0 {
		return "Dictionary new", nil
	}
	pairs := make([]string, len(m.Items))
	for i, it := range m.Items {
		k, err := c.compileExpr(it.Key)
		if err != nil {
			return "", err
		}
		v, err := c.compileExpr(it.Value)
		if err != nil {
			return "", err
		}
		pairs[i] = fmt.Sprintf("%s -> %s", k, v)
	}
	return "Dictionary from: {" + strings.Join(pairs, ". ") + "}", nil
}

func (c *Compiler) compileStructLiteral(s *parser.StructLiteral) (string, error) {
	if len(s.Fields) == 0 {
		return "Dictionary new", nil
	}
	pairs := make([]string, len(s.Fields))
	for i, f := range s.Fields {
		v, err := c.compileExpr(f.Value)
		if err != nil {
			return "", err
		}
		key := fmt.Sprintf("'%s'", f.Name)
		pairs[i] = fmt.Sprintf("%s -> %s", key, v)
	}
	return "Dictionary from: {" + strings.Join(pairs, ". ") + "}", nil
}

func isListUnary(u *parser.Unary, env *types.Env) bool {
	if u == nil || len(u.Ops) != 0 {
		return false
	}
	return isListPostfix(u.Value, env)
}

func isListPostfix(p *parser.PostfixExpr, env *types.Env) bool {
	if p == nil || len(p.Ops) != 0 {
		return false
	}
	if p.Target.List != nil {
		return true
	}
	if p.Target.Selector != nil && len(p.Target.Selector.Tail) == 0 && env != nil {
		if t, err := env.GetVar(p.Target.Selector.Root); err == nil {
			if _, ok := t.(types.ListType); ok {
				return true
			}
		}
	}
	return false
}

func isStringUnary(u *parser.Unary, env *types.Env) bool {
	if u == nil || len(u.Ops) != 0 {
		return false
	}
	return isStringPostfix(u.Value, env)
}

func isStringPostfix(p *parser.PostfixExpr, env *types.Env) bool {
	if p == nil || len(p.Ops) != 0 {
		return false
	}
	if p.Target.Lit != nil && p.Target.Lit.Str != nil {
		return true
	}
	if p.Target.Selector != nil && len(p.Target.Selector.Tail) == 0 && env != nil {
		if t, err := env.GetVar(p.Target.Selector.Root); err == nil {
			if _, ok := t.(types.StringType); ok {
				return true
			}
		}
	}
	return false
}

func isMapUnary(u *parser.Unary, env *types.Env) bool {
	if u == nil || len(u.Ops) != 0 {
		return false
	}
	return isMapPostfix(u.Value, env)
}

func isMapPostfix(p *parser.PostfixExpr, env *types.Env) bool {
	if p == nil || len(p.Ops) != 0 {
		return false
	}
	if p.Target.Map != nil {
		return true
	}
	if p.Target.Selector != nil && len(p.Target.Selector.Tail) == 0 && env != nil {
		if t, err := env.GetVar(p.Target.Selector.Root); err == nil {
			if _, ok := t.(types.MapType); ok {
				return true
			}
		}
	}
	return false
}

func hasBreak(stmts []*parser.Statement) bool {
	for _, s := range stmts {
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

func hasBreakIf(ifst *parser.IfStmt) bool {
	if ifst == nil {
		return false
	}
	if hasBreak(ifst.Then) || hasBreak(ifst.Else) {
		return true
	}
	return hasBreakIf(ifst.ElseIf)
}

func hasContinue(stmts []*parser.Statement) bool {
	for _, s := range stmts {
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

func hasContinueIf(ifst *parser.IfStmt) bool {
	if ifst == nil {
		return false
	}
	if hasContinue(ifst.Then) || hasContinue(ifst.Else) {
		return true
	}
	return hasContinueIf(ifst.ElseIf)
}

func (c *Compiler) emitHelpers() {
	if c.needBreak {
		c.writeln("Object subclass: #BreakSignal instanceVariableNames: '' classVariableNames: '' poolDictionaries: '' category: nil!")
	}
	if c.needContinue {
		c.writeln("Object subclass: #ContinueSignal instanceVariableNames: '' classVariableNames: '' poolDictionaries: '' category: nil!")
	}
	if c.needBreak || c.needContinue {
		c.writeln("")
	}
	c.writeln("!Main class methodsFor: 'runtime'!")
	if c.needCount {
		c.writeln("__count: v")
		c.indent++
		c.writeln("(v respondsTo: #size) ifTrue: [ ^ v size ]")
		c.writeln("^ self error: 'count() expects collection'")
		c.indent--
		c.writelnNoIndent("!")
	}
	if c.needAvg {
		c.writeln("__avg: v")
		c.indent++
		c.writeln("(v respondsTo: #do:) ifFalse: [ ^ self error: 'avg() expects collection' ]")
		c.writeln("v size = 0 ifTrue: [ ^ 0 ]")
		c.writeln("| sum |")
		c.writeln("sum := 0.")
		c.writeln("v do: [:it | sum := sum + it].")
		c.writeln("^ sum / v size")
		c.indent--
		c.writelnNoIndent("!")
	}
	if c.needInput {
		c.writeln("__input")
		c.indent++
		c.writeln("^ stdin nextLine")
		c.indent--
		c.writelnNoIndent("!")
	}
	if c.needReduce {
		c.writeln("__reduce: v fn: blk init: acc")
		c.indent++
		c.writeln("| res |")
		c.writeln("res := acc.")
		c.writeln("v do: [:it | res := blk value: res value: it].")
		c.writeln("^ res")
		c.indent--
		c.writelnNoIndent("!")
	}
}
