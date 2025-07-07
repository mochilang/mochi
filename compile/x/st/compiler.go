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
	buf             bytes.Buffer
	indent          int
	env             *types.Env
	funParams       map[string][]string
	needCount       bool
	needAvg         bool
	needInput       bool
	needReduce      bool
	needBreak       bool
	needContinue    bool
	needUnionAll    bool
	needUnion       bool
	needExcept      bool
	needIntersect   bool
	needIndexStr    bool
	needSliceStr    bool
	needContainsStr bool
	needDataset     bool
	needFetch       bool
	needPaginate    bool
	needSum         bool
	needMin         bool
	needMax         bool
	needReverse     bool
	needGroupBy     bool
	needGroup       bool
	needCast        bool
	globalVars      []string
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

// reset clears the compiler state before a new compilation run.
func (c *Compiler) reset() {
	c.buf.Reset()
	c.needCount = false
	c.needAvg = false
	c.needInput = false
	c.needReduce = false
	c.needBreak = false
	c.needContinue = false
	c.needUnionAll = false
	c.needUnion = false
	c.needExcept = false
	c.needIntersect = false
	c.needIndexStr = false
	c.needSliceStr = false
	c.needContainsStr = false
	c.needDataset = false
	c.needFetch = false
	c.needPaginate = false
	c.needSum = false
	c.needMin = false
	c.needMax = false
	c.needReverse = false
	c.needGroupBy = false
	c.needGroup = false
	c.globalVars = nil
}

// Compile generates Smalltalk code for prog.
func (c *Compiler) Compile(prog *parser.Program) ([]byte, error) {
	c.reset()
	c.globalVars = collectGlobalVars(prog.Statements)

	orig := c.buf

	typeCode, err := c.compileTypeDecls(prog)
	if err != nil {
		return nil, err
	}

	funCode, err := c.compileFunctions(prog)
	if err != nil {
		return nil, err
	}

	testCode, testNames, err := c.compileTestBlocks(prog)
	if err != nil {
		return nil, err
	}

	mainCode, err := c.compileMainStmts(prog, testNames)
	if err != nil {
		return nil, err
	}

	// assemble
	c.buf = orig
	for _, g := range c.globalVars {
		c.writeln(fmt.Sprintf("Smalltalk at: #%s put: nil.", sanitizeName(g)))
	}
	if len(c.globalVars) > 0 {
		c.writeln("")
	}
	c.writeln("Object subclass: #Main instanceVariableNames: '' classVariableNames: '' poolDictionaries: '' category: nil!")
	c.writeln("")
	c.buf.Write(typeCode)
	c.buf.Write(funCode)
	c.buf.Write(testCode)
	if c.needCount || c.needAvg || c.needInput || c.needReduce || c.needBreak ||
		c.needContinue || c.needUnionAll || c.needUnion || c.needExcept ||
		c.needIntersect || c.needIndexStr || c.needSliceStr ||
		c.needContainsStr || c.needDataset || c.needFetch || c.needPaginate ||
		c.needSum || c.needMin || c.needMax || c.needReverse || c.needGroupBy || c.needGroup {
		c.emitHelpers()
	}
	c.writelnNoIndent("!!")
	c.buf.Write(mainCode)

	return format(c.buf.Bytes()), nil
}

func (c *Compiler) compileTypeDecls(prog *parser.Program) ([]byte, error) {
	c.buf = bytes.Buffer{}
	for _, s := range prog.Statements {
		if s.Type != nil {
			if err := c.compileTypeDecl(s.Type); err != nil {
				return nil, err
			}
		}
	}
	return append([]byte(nil), c.buf.Bytes()...), nil
}

func (c *Compiler) compileFunctions(prog *parser.Program) ([]byte, error) {
	c.buf = bytes.Buffer{}
	for _, s := range prog.Statements {
		if s.Fun != nil {
			if err := c.compileFun(s.Fun); err != nil {
				return nil, err
			}
			c.writeln("")
		}
	}
	return append([]byte(nil), c.buf.Bytes()...), nil
}

func (c *Compiler) compileTestBlocks(prog *parser.Program) ([]byte, []string, error) {
	c.buf = bytes.Buffer{}
	testNames := []string{}
	for _, s := range prog.Statements {
		if s.Test != nil {
			if err := c.compileTestBlock(s.Test); err != nil {
				return nil, nil, err
			}
			testNames = append(testNames, "test_"+sanitizeName(s.Test.Name))
			c.writeln("")
		}
	}
	return append([]byte(nil), c.buf.Bytes()...), testNames, nil
}

func (c *Compiler) compileMainStmts(prog *parser.Program, tests []string) ([]byte, error) {
	c.buf = bytes.Buffer{}
	for _, s := range prog.Statements {
		if s.Fun != nil || s.Type != nil || s.Test != nil {
			continue
		}
		if err := c.compileStmt(s); err != nil {
			return nil, err
		}
	}
	for _, name := range tests {
		c.writeln(fmt.Sprintf("Main %s.", name))
	}
	return append([]byte(nil), c.buf.Bytes()...), nil
}

func (c *Compiler) compileFun(fn *parser.FunStmt) error {
	header := fn.Name
	names := make([]string, len(fn.Params))
	typeParts := make([]string, len(fn.Params))
	if len(fn.Params) > 0 {
		header += ": " + fn.Params[0].Name
		if t, err := c.env.GetVar(fn.Params[0].Name); err == nil {
			typeParts[0] = fmt.Sprintf("%s: %s", fn.Params[0].Name, typeString(t))
		} else {
			typeParts[0] = fn.Params[0].Name
		}
		names[0] = fn.Params[0].Name
		for i, p := range fn.Params[1:] {
			header += " " + p.Name + ": " + p.Name
			if t, err := c.env.GetVar(p.Name); err == nil {
				typeParts[i+1] = fmt.Sprintf("%s: %s", p.Name, typeString(t))
			} else {
				typeParts[i+1] = p.Name
			}
			names[i+1] = p.Name
		}
	}
	c.funParams[fn.Name] = names
	vars := collectVars(fn.Body)
	if c.indent > 0 {
		vars = append(vars, fn.Name)
	}

	comment := strings.Join(filterEmpty(typeParts), ", ")

	if c.indent > 0 {
		blockHeader := "["
		if len(fn.Params) > 0 {
			blockHeader += ":" + fn.Params[0].Name
			for _, p := range fn.Params[1:] {
				blockHeader += " :" + p.Name
			}
			blockHeader += " |"
		}
		if len(vars) > 0 {
			if len(fn.Params) == 0 {
				blockHeader += "| " + strings.Join(vars, " ") + " |"
			} else {
				blockHeader += " | " + strings.Join(vars, " ") + " |"
			}
		}
		if comment != "" {
			c.writeln(fmt.Sprintf("%s := %s \"%s\"", fn.Name, blockHeader, comment))
		} else {
			c.writeln(fmt.Sprintf("%s := %s", fn.Name, blockHeader))
		}
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
	line := header
	if len(vars) > 0 {
		line += " | " + strings.Join(vars, " ") + " |"
	}
	if comment != "" {
		line += " \"" + comment + "\""
	}
	c.writeln(line)
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
		c.writeln("!Main class methodsFor: 'types'!")
		for _, v := range t.Variants {
			fields := make([]string, len(v.Fields))
			for i, f := range v.Fields {
				fields[i] = f.Name
			}
			header := "new" + v.Name
			if len(fields) > 0 {
				header += ": " + fields[0]
				for _, f := range fields[1:] {
					header += " " + f + ": " + f
				}
			}
			c.writeln(header + " | dict |")
			c.indent++
			c.writeln("dict := Dictionary new.")
			c.writeln(fmt.Sprintf("dict at: '__name' put: '%s'.", v.Name))
			for _, f := range fields {
				c.writeln(fmt.Sprintf("dict at: '%s' put: %s.", f, f))
			}
			c.writeln("^ dict")
			c.indent--
			c.writelnNoIndent("!")
		}
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

func collectGlobalVars(stmts []*parser.Statement) []string {
	set := map[string]bool{}
	for _, s := range stmts {
		switch {
		case s.Let != nil:
			set[s.Let.Name] = true
		case s.Var != nil:
			set[s.Var.Name] = true
		}
	}
	vars := make([]string, 0, len(set))
	for v := range set {
		vars = append(vars, v)
	}
	sort.Strings(vars)
	return vars
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

func (c *Compiler) compileStmt(s *parser.Statement) error {
	switch {
	case s.Let != nil:
		val, err := c.compileExpr(s.Let.Value)
		if err != nil {
			return err
		}
		typeStr := ""
		if t, err := c.env.GetVar(s.Let.Name); err == nil {
			typeStr = typeString(t)
		}
		if typeStr != "" {
			c.writeln(fmt.Sprintf("%s := %s. \"%s\"", s.Let.Name, val, typeStr))
		} else {
			c.writeln(fmt.Sprintf("%s := %s.", s.Let.Name, val))
		}
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
	case s.Update != nil:
		return c.compileUpdate(s.Update)
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
		if types.IsMapExpr(f.Source, c.env) {
			c.writeln(fmt.Sprintf("(%s) keysDo: [:%s |", src, f.Name))
		} else {
			c.writeln(fmt.Sprintf("(%s) do: [:%s |", src, f.Name))
		}
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

func (c *Compiler) compileUpdate(u *parser.UpdateStmt) error {
	var fields []string
	if t, err := c.env.GetVar(u.Target); err == nil {
		if lt, ok := t.(types.ListType); ok {
			if st, ok := lt.Elem.(types.StructType); ok {
				fields = st.Order
			}
		}
	}

	c.writeln(fmt.Sprintf("0 to: (%s size) - 1 do: [:idx |", u.Target))
	c.indent++
	c.writeln("| item |")
	c.writeln(fmt.Sprintf("item := %s at: idx + 1.", u.Target))

	orig := c.env
	if len(fields) > 0 {
		child := types.NewEnv(c.env)
		for _, f := range fields {
			c.writeln(fmt.Sprintf("%s := item at: '%s'.", f, f))
			child.SetVar(f, types.AnyType{}, true)
		}
		c.env = child
	}

	if u.Where != nil {
		cond, err := c.compileExpr(u.Where)
		if err != nil {
			c.env = orig
			return err
		}
		c.writeln("(" + cond + ") ifTrue: [")
		c.indent++
	}

	for _, it := range u.Set.Items {
		keyStr, ok := simpleIdent(it.Key)
		if ok {
			val, err := c.compileExpr(it.Value)
			if err != nil {
				c.env = orig
				return err
			}
			c.writeln(fmt.Sprintf("item at: '%s' put: %s.", keyStr, val))
		} else {
			k, err := c.compileExpr(it.Key)
			if err != nil {
				c.env = orig
				return err
			}
			v, err := c.compileExpr(it.Value)
			if err != nil {
				c.env = orig
				return err
			}
			c.writeln(fmt.Sprintf("item at: %s put: %s.", k, v))
		}
	}

	if u.Where != nil {
		c.indent--
		c.writeln("]")
	}

	c.writeln(fmt.Sprintf("%s at: idx + 1 put: item.", u.Target))
	c.indent--
	c.writeln("]")
	c.writeln(".")
	c.env = orig
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
	typeStr := ""
	if t, err := c.env.GetVar(v.Name); err == nil {
		typeStr = typeString(t)
	}
	if typeStr != "" {
		c.writeln(fmt.Sprintf("%s := %s. \"%s\"", v.Name, val, typeStr))
	} else {
		c.writeln(fmt.Sprintf("%s := %s.", v.Name, val))
	}
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
	leftList := types.IsListUnary(b.Left, c.env)
	leftStr := types.IsStringUnary(b.Left, c.env)
	for _, op := range b.Right {
		right, err := c.compilePostfix(op.Right)
		if err != nil {
			return "", err
		}
		rlist := types.IsListPostfix(op.Right, c.env)
		rstr := types.IsStringPostfix(op.Right, c.env)
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
			if rstr && leftStr {
				c.needContainsStr = true
				expr = fmt.Sprintf("(Main __contains_string: %s sub: %s)", right, left)
			} else if rlist || rstr {
				expr = fmt.Sprintf("(%s includes: %s)", right, left)
			} else if types.IsMapPostfix(op.Right, c.env) {
				expr = fmt.Sprintf("(%s includesKey: %s)", right, left)
			} else {
				expr = fmt.Sprintf("(%s includes: %s)", right, left)
			}
			leftList = false
			leftStr = false
		case "union":
			if op.All {
				c.needUnionAll = true
				expr = fmt.Sprintf("(Main __union_all: (%s) with: (%s))", expr, right)
			} else {
				c.needUnion = true
				expr = fmt.Sprintf("(Main __union: (%s) with: (%s))", expr, right)
			}
			leftList = true
			leftStr = false
		case "except":
			c.needExcept = true
			expr = fmt.Sprintf("(Main __except: (%s) with: (%s))", expr, right)
			leftList = true
			leftStr = false
		case "intersect":
			c.needIntersect = true
			expr = fmt.Sprintf("(Main __intersect: (%s) with: (%s))", expr, right)
			leftList = true
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
	for i := 0; i < len(p.Ops); i++ {
		op := p.Ops[i]
		if op.Index != nil {
			if op.Index.Colon != nil {
				start0 := "0"
				start1 := "1"
				if op.Index.Start != nil {
					s, err := c.compileExpr(op.Index.Start)
					if err != nil {
						return "", err
					}
					start0 = s
					start1 = fmt.Sprintf("(%s + 1)", s)
				}
				end0 := fmt.Sprintf("%s size", expr)
				end1 := fmt.Sprintf("%s size", expr)
				if op.Index.End != nil {
					e, err := c.compileExpr(op.Index.End)
					if err != nil {
						return "", err
					}
					end0 = e
					end1 = e
				}
				targetOnly := &parser.PostfixExpr{Target: p.Target}
				if types.IsStringPostfix(targetOnly, c.env) {
					c.needSliceStr = true
					expr = fmt.Sprintf("(Main __slice_string: %s start: %s end: %s)", expr, start0, end0)
				} else {
					expr = fmt.Sprintf("(%s copyFrom: %s to: %s)", expr, start1, end1)
				}
			} else {
				idx, err := c.compileExpr(op.Index.Start)
				if err != nil {
					return "", err
				}
				targetOnly := &parser.PostfixExpr{Target: p.Target}
				if types.IsMapPostfix(targetOnly, c.env) {
					expr = fmt.Sprintf("(%s at: %s)", expr, idx)
				} else if types.IsStringPostfix(targetOnly, c.env) {
					c.needIndexStr = true
					expr = fmt.Sprintf("(Main __index_string: %s idx: %s)", expr, idx)
				} else {
					expr = fmt.Sprintf("(%s at: %s + 1)", expr, idx)
				}
			}
		} else if op.Cast != nil {
			t := typeName(op.Cast.Type)
			c.needCast = true
			expr = fmt.Sprintf("(Main _cast: '%s' value: %s)", t, expr)
		} else if op.Field != nil {
			// method call like .contains(arg)
			if op.Field.Name == "contains" && i+1 < len(p.Ops) && p.Ops[i+1].Call != nil {
				argExpr, err := c.compileExpr(p.Ops[i+1].Call.Args[0])
				if err != nil {
					return "", err
				}
				c.needContainsStr = true
				expr = fmt.Sprintf("(Main __contains_string: %s sub: %s)", expr, argExpr)
				i++
				continue
			}
			expr = fmt.Sprintf("%s at: '%s'", expr, op.Field.Name)
		} else if op.Call != nil {
			// direct call on value
			params := make([]string, len(op.Call.Args))
			for j, a := range op.Call.Args {
				v, err := c.compileExpr(a)
				if err != nil {
					return "", err
				}
				params[j] = v
			}
			if len(params) == 0 {
				expr = fmt.Sprintf("(%s value)", expr)
			} else {
				expr = fmt.Sprintf("(%s value: %s)", expr, strings.Join(params, " value: "))
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
			if strings.ContainsAny(v, " \t\n") {
				elems[i] = "(" + v + ")"
			} else {
				elems[i] = v
			}
		}
		return "Array with: " + strings.Join(elems, " with: "), nil
	case p.Map != nil:
		return c.compileMapLiteral(p.Map)
	case p.Struct != nil:
		return c.compileStructLiteral(p.Struct)
	case p.Query != nil:
		return c.compileQueryExpr(p.Query)
	case p.Load != nil:
		return c.compileLoadExpr(p.Load)
	case p.Save != nil:
		return c.compileSaveExpr(p.Save)
	case p.Fetch != nil:
		return c.compileFetchExpr(p.Fetch)
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
	case "sum":
		if len(args) != 1 {
			return "", fmt.Errorf("sum expects 1 arg")
		}
		c.needSum = true
		return fmt.Sprintf("(Main __sum: %s)", args[0]), nil
	case "min":
		if len(args) != 1 {
			return "", fmt.Errorf("min expects 1 arg")
		}
		c.needMin = true
		return fmt.Sprintf("(Main __min: %s)", args[0]), nil
	case "max":
		if len(args) != 1 {
			return "", fmt.Errorf("max expects 1 arg")
		}
		c.needMax = true
		return fmt.Sprintf("(Main __max: %s)", args[0]), nil
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
	case "substr", "substring":
		if len(args) != 3 {
			return "", fmt.Errorf("substr expects 3 args")
		}
		c.needSliceStr = true
		end := fmt.Sprintf("%s + %s", args[1], args[2])
		return fmt.Sprintf("(Main __slice_string: %s start: %s end: %s)", args[0], args[1], end), nil
	case "reverse":
		if len(args) != 1 {
			return "", fmt.Errorf("reverse expects 1 arg")
		}
		c.needReverse = true
		return fmt.Sprintf("(Main __reverse: %s)", args[0]), nil
	default:
		params, ok := c.funParams[name]
		if !ok {
			call := name
			if len(args) == 0 {
				call += " value"
			}
			for _, a := range args {
				call += " value: " + a
			}
			return call, nil
		}
		parts := []string{"Main"}
		if len(params) == 0 {
			parts = append(parts, name)
			return "(" + strings.Join(parts, " ") + ")", nil
		}
		parts = append(parts, name+":")
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
	case l.Null:
		return "nil", nil
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
		b.WriteString("[")
	}
	vars := []string{}
	if len(fn.BlockBody) > 0 {
		vars = collectVars(fn.BlockBody)
	}
	if len(vars) > 0 {
		if len(fn.Params) == 0 {
			b.WriteString("| " + strings.Join(vars, " ") + " |")
		} else {
			b.WriteString(" | " + strings.Join(vars, " ") + " |")
		}
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
}

func (c *Compiler) compileStructLiteral(s *parser.StructLiteral) (string, error) {
	if c.env != nil {
		if st, ok := c.env.GetStruct(s.Name); ok && len(st.Order) > 0 {
			vals := map[string]string{}
			for _, f := range s.Fields {
				v, err := c.compileExpr(f.Value)
				if err != nil {
					return "", err
				}
				vals[f.Name] = v
			}
			parts := []string{"Main", "new" + st.Name + ":"}
			for i, fn := range st.Order {
				v := "nil"
				if val, ok := vals[fn]; ok {
					v = val
				}
				if i == 0 {
					parts = append(parts, v)
				} else {
					parts = append(parts, fmt.Sprintf("%s: %s", fn, v))
				}
			}
			return "(" + strings.Join(parts, " ") + ")", nil
		}
	}
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

func (c *Compiler) compileQueryExpr(q *parser.QueryExpr) (string, error) {
	src, err := c.compileExpr(q.Source)
	if err != nil {
		return "", err
	}
	orig := c.env

	if q.Group != nil && len(q.Froms) == 0 && q.Sort == nil && q.Skip == nil && q.Take == nil {
		child := types.NewEnv(c.env)
		child.SetVar(q.Var, types.AnyType{}, true)
		c.env = child
		var cond string
		if q.Where != nil {
			cond, err = c.compileExpr(q.Where)
			if err != nil {
				c.env = orig
				return "", err
			}
		}
		keyExpr, err := c.compileExpr(q.Group.Exprs[0])
		if err != nil {
			c.env = orig
			return "", err
		}
		genv := types.NewEnv(orig)
		genv.SetVar(q.Group.Name, types.AnyType{}, true)
		c.env = genv
		sel, err := c.compileExpr(q.Select)
		if err != nil {
			c.env = orig
			return "", err
		}
		var having string
		if q.Group.Having != nil {
			having, err = c.compileExpr(q.Group.Having)
			if err != nil {
				c.env = orig
				return "", err
			}
		}
		c.env = orig
		c.needGroupBy = true
		c.needGroup = true
		var b strings.Builder
		b.WriteString("((| rows groups |\n")
		b.WriteString("rows := OrderedCollection new.\n")
		b.WriteString(fmt.Sprintf("(%s) do: [:%s |\n", src, sanitizeName(q.Var)))
		if cond != "" {
			b.WriteString(fmt.Sprintf("\t(%s) ifTrue: [ rows add: %s ].\n", cond, sanitizeName(q.Var)))
		} else {
			b.WriteString(fmt.Sprintf("\trows add: %s.\n", sanitizeName(q.Var)))
		}
		b.WriteString("]\n")
		b.WriteString(fmt.Sprintf("groups := (Main _group_by: rows keyFn: [:%s | %s]).\n", sanitizeName(q.Var), keyExpr))
		b.WriteString("rows := OrderedCollection new.\n")
		b.WriteString(fmt.Sprintf("(groups) do: [:%s |\n", sanitizeName(q.Group.Name)))
		if having != "" {
			b.WriteString(fmt.Sprintf("\t(%s) ifTrue: [ rows add: %s ].\n", having, sel))
		} else {
			b.WriteString(fmt.Sprintf("\trows add: %s.\n", sel))
		}
		b.WriteString("]\n")
		b.WriteString("rows := rows asArray.\n")
		b.WriteString("rows))")
		return b.String(), nil
	}
	child := types.NewEnv(c.env)
	child.SetVar(q.Var, types.AnyType{}, true)
	fromSrcs := make([]string, len(q.Froms))
	for i, f := range q.Froms {
		fs, err := c.compileExpr(f.Src)
		if err != nil {
			return "", err
		}
		fromSrcs[i] = fs
		child.SetVar(f.Var, types.AnyType{}, true)
	}
	joinSrcs := make([]string, len(q.Joins))
	for i, j := range q.Joins {
		js, err := c.compileExpr(j.Src)
		if err != nil {
			return "", err
		}
		joinSrcs[i] = js
		child.SetVar(j.Var, types.AnyType{}, true)
	}
	c.env = child
	sel, err := c.compileExpr(q.Select)
	if err != nil {
		c.env = orig
		return "", err
	}
	var cond, sortExpr, skipExpr, takeExpr string
	if q.Where != nil {
		cond, err = c.compileExpr(q.Where)
		if err != nil {
			c.env = orig
			return "", err
		}
	}
	joinConds := make([]string, len(q.Joins))
	for i, j := range q.Joins {
		jc, err := c.compileExpr(j.On)
		if err != nil {
			c.env = orig
			return "", err
		}
		joinConds[i] = jc
	}
	if q.Sort != nil {
		sortExpr, err = c.compileExpr(q.Sort)
		if err != nil {
			c.env = orig
			return "", err
		}
	}
	if q.Skip != nil {
		skipExpr, err = c.compileExpr(q.Skip)
		if err != nil {
			c.env = orig
			return "", err
		}
	}
	if q.Take != nil {
		takeExpr, err = c.compileExpr(q.Take)
		if err != nil {
			c.env = orig
			return "", err
		}
	}
	c.env = orig

	fullCond := cond
	for _, jc := range joinConds {
		if jc == "" {
			continue
		}
		if fullCond == "" {
			fullCond = jc
		} else {
			fullCond = fmt.Sprintf("(%s and: [%s])", fullCond, jc)
		}
	}

	var b strings.Builder
	b.WriteString("((| res |\n")
	b.WriteString("res := OrderedCollection new.\n")

	loopSrc := src
	if fullCond != "" && len(fromSrcs) == 0 {
		push := true
		for _, f := range q.Froms {
			if strings.Contains(fullCond, sanitizeName(f.Var)) {
				push = false
				break
			}
		}
		if push {
			loopSrc = fmt.Sprintf("(%s) select: [:%s | %s]", src, sanitizeName(q.Var), fullCond)
			fullCond = ""
		}
	}

	b.WriteString(fmt.Sprintf("(%s) do: [:%s |\n", loopSrc, sanitizeName(q.Var)))
	indent := "\t"
	for i, fs := range fromSrcs {
		b.WriteString(indent + fmt.Sprintf("(%s) do: [:%s |\n", fs, sanitizeName(q.Froms[i].Var)))
		indent += "\t"
	}
	for i, js := range joinSrcs {
		b.WriteString(indent + fmt.Sprintf("(%s) do: [:%s |\n", js, sanitizeName(q.Joins[i].Var)))
		indent += "\t"
	}
	if fullCond != "" {
		b.WriteString(indent + fmt.Sprintf("(%s) ifTrue: [\n", fullCond))
		indent += "\t"
	}
	if sortExpr != "" {
		b.WriteString(indent + fmt.Sprintf("res add: { %s . %s }.\n", sortExpr, sel))
	} else {
		b.WriteString(indent + fmt.Sprintf("res add: %s.\n", sel))
	}
	if fullCond != "" {
		indent = indent[:len(indent)-1]
		b.WriteString(indent + "]\n")
	}
	for range joinSrcs {
		indent = indent[:len(indent)-1]
		b.WriteString(indent + "]\n")
	}
	for range fromSrcs {
		indent = indent[:len(indent)-1]
		b.WriteString(indent + "]\n")
	}
	b.WriteString("]\n")
	b.WriteString("res := res asArray.\n")
	if sortExpr != "" {
		b.WriteString("res := (SortedCollection sortBlock: [:a :b | a first <= b first ]) withAll: res; asArray.\n")
		b.WriteString("res := res collect: [:p | p second].\n")
	}
	if skipExpr != "" || takeExpr != "" {
		sk := "0"
		tk := "nil"
		if skipExpr != "" {
			sk = skipExpr
		}
		if takeExpr != "" {
			tk = takeExpr
		}
		c.needPaginate = true
		b.WriteString(fmt.Sprintf("res := (Main _paginate: res skip: %s take: %s).\n", sk, tk))
	}
	b.WriteString("res))")
	return b.String(), nil
}

func (c *Compiler) compileLoadExpr(l *parser.LoadExpr) (string, error) {
	c.needDataset = true
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
	return fmt.Sprintf("(Main _load: %s opts: %s)", path, opts), nil
}

func (c *Compiler) compileSaveExpr(s *parser.SaveExpr) (string, error) {
	c.needDataset = true
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
	c.needFetch = true
	return fmt.Sprintf("(Main _fetch: %s opts: %s)", url, opts), nil
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
	if c.needGroup {
		c.writeln("Object subclass: #_Group instanceVariableNames: 'key items' classVariableNames: '' poolDictionaries: '' category: nil!")
		c.writeln("")
		c.writeln("!_Group class methodsFor: 'instance creation'!")
		c.writeln("key: k | g |")
		c.indent++
		c.writeln("g := self new.")
		c.writeln("g key: k.")
		c.writeln("g initialize.")
		c.writeln("^ g")
		c.indent--
		c.writelnNoIndent("!")
		c.writeln("!_Group methodsFor: 'initialization'!")
		c.writeln("initialize")
		c.indent++
		c.writeln("items := OrderedCollection new.")
		c.writeln("^ self")
		c.indent--
		c.writelnNoIndent("!")
		c.writeln("!_Group methodsFor: 'accessing'!")
		c.writeln("key")
		c.indent++
		c.writeln("^ key")
		c.indent--
		c.writelnNoIndent("!")
		c.writeln("key: k")
		c.indent++
		c.writeln("key := k")
		c.indent--
		c.writelnNoIndent("!")
		c.writeln("add: it")
		c.indent++
		c.writeln("items add: it")
		c.indent--
		c.writelnNoIndent("!")
		c.writeln("do: blk")
		c.indent++
		c.writeln("items do: blk")
		c.indent--
		c.writelnNoIndent("!")
		c.writeln("size")
		c.indent++
		c.writeln("^ items size")
		c.indent--
		c.writelnNoIndent("!")
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
	if c.needUnionAll {
		c.writeln("__union_all: a with: b")
		c.indent++
		c.writeln("| out |")
		c.writeln("out := OrderedCollection new.")
		c.writeln("a ifNotNil: [ a do: [:v | out add: v ] ].")
		c.writeln("b ifNotNil: [ b do: [:v | out add: v ] ].")
		c.writeln("^ out asArray")
		c.indent--
		c.writelnNoIndent("!")
	}
	if c.needUnion {
		c.writeln("__union: a with: b")
		c.indent++
		c.writeln("| out |")
		c.writeln("out := OrderedCollection new.")
		c.writeln("a ifNotNil: [ a do: [:v | (out includes: v) ifFalse: [ out add: v ] ] ].")
		c.writeln("b ifNotNil: [ b do: [:v | (out includes: v) ifFalse: [ out add: v ] ] ].")
		c.writeln("^ out asArray")
		c.indent--
		c.writelnNoIndent("!")
	}
	if c.needExcept {
		c.writeln("__except: a with: b")
		c.indent++
		c.writeln("| out |")
		c.writeln("out := OrderedCollection new.")
		c.writeln("a ifNotNil: [ a do: [:v | (b isNil or: [ (b includes: v) not ]) ifTrue: [ out add: v ] ] ].")
		c.writeln("^ out asArray")
		c.indent--
		c.writelnNoIndent("!")
	}
	if c.needIntersect {
		c.writeln("__intersect: a with: b")
		c.indent++
		c.writeln("| out |")
		c.writeln("out := OrderedCollection new.")
		c.writeln("(a notNil and: [ b notNil ]) ifTrue: [")
		c.indent++
		c.writeln("a do: [:v | (b includes: v) ifTrue: [ (out includes: v) ifFalse: [ out add: v ] ] ].")
		c.indent--
		c.writeln("]")
		c.writeln("^ out asArray")
		c.indent--
		c.writelnNoIndent("!")
	}
	if c.needIndexStr {
		c.writeln("__index_string: s idx: i")
		c.indent++
		c.writeln("| idx n |")
		c.writeln("idx := i.")
		c.writeln("n := s size.")
		c.writeln("idx < 0 ifTrue: [ idx := idx + n ].")
		c.writeln("(idx < 0 or: [ idx >= n ]) ifTrue: [ self error: 'index out of range' ].")
		c.writeln("^ (s at: idx + 1) asString")
		c.indent--
		c.writelnNoIndent("!")
	}
	if c.needSliceStr {
		c.writeln("__slice_string: s start: i end: j")
		c.indent++
		c.writeln("| start end n |")
		c.writeln("start := i.")
		c.writeln("end := j.")
		c.writeln("n := s size.")
		c.writeln("start < 0 ifTrue: [ start := start + n ].")
		c.writeln("end < 0 ifTrue: [ end := end + n ].")
		c.writeln("start < 0 ifTrue: [ start := 0 ].")
		c.writeln("end > n ifTrue: [ end := n ].")
		c.writeln("end < start ifTrue: [ end := start ].")
		c.writeln("^ (s copyFrom: start + 1 to: end)")
		c.indent--
		c.writelnNoIndent("!")
	}
	if c.needContainsStr {
		c.writeln("__contains_string: s sub: t")
		c.indent++
		c.writeln("^ (s findString: t startingAt: 1) ~= 0")
		c.indent--
		c.writelnNoIndent("!")
	}
	if c.needReverse {
		c.writeln("__reverse: obj")
		c.indent++
		c.writeln("(obj isKindOf: Array) ifTrue: [ ^ obj reverse ]")
		c.writeln("(obj isString) ifTrue: [ ^ obj reverse ]")
		c.writeln("^ self error: 'reverse expects list or string'")
		c.indent--
		c.writelnNoIndent("!")
	}
	if c.needSum {
		c.writeln("__sum: v")
		c.indent++
		c.writeln("(v respondsTo: #do:) ifFalse: [ ^ self error: 'sum() expects collection' ]")
		c.writeln("| s |")
		c.writeln("s := 0.")
		c.writeln("v do: [:it | s := s + it].")
		c.writeln("^ s")
		c.indent--
		c.writelnNoIndent("!")
	}
	if c.needMin {
		c.writeln("__min: v")
		c.indent++
		c.writeln("(v respondsTo: #do:) ifFalse: [ ^ self error: 'min() expects collection' ]")
		c.writeln("| m |")
		c.writeln("m := nil.")
		c.writeln("v do: [:it | m isNil ifTrue: [ m := it ] ifFalse: [ (it < m) ifTrue: [ m := it ] ] ].")
		c.writeln("^ m")
		c.indent--
		c.writelnNoIndent("!")
	}
	if c.needMax {
		c.writeln("__max: v")
		c.indent++
		c.writeln("(v respondsTo: #do:) ifFalse: [ ^ self error: 'max() expects collection' ]")
		c.writeln("| m first |")
		c.writeln("first := true.")
		c.writeln("v do: [:it | first ifTrue: [ m := it. first := false ] ifFalse: [ (it > m) ifTrue: [ m := it ] ] ].")
		c.writeln("^ first ifTrue: [ 0 ] ifFalse: [ m ]")
		c.indent--
		c.writelnNoIndent("!")
	}
	if c.needDataset {
		c.writeln("_load: path opts: o")
		c.indent++
		c.writeln("| fmt stream text data |")
		c.writeln("fmt := (o notNil and: [ o includesKey: 'format' ]) ifTrue: [ o at: 'format' ] ifFalse: [ 'json' ].")
		c.writeln("stream := (path isNil or: [ path = '' or: [ path = '-' ] ])")
		c.indent++
		c.writeln("ifTrue: [ stdin ]")
		c.writeln("ifFalse: [ FileStream open: path mode: FileStream read ].")
		c.indent--
		c.writeln("text := stream contents.")
		c.writeln("stream ~= stdin ifTrue: [ stream close ].")
		c.writeln("fmt = 'jsonl' ifTrue: [")
		c.indent++
		c.writeln("| out |")
		c.writeln("out := OrderedCollection new.")
		c.writeln("text linesDo: [:l | l isEmpty ifFalse: [ out add: (JSONReader fromJSON: l) ] ].")
		c.writeln("^ out asArray")
		c.indent--
		c.writeln("] .")
		c.writeln("data := JSONReader fromJSON: text.")
		c.writeln("(data isKindOf: OrderedCollection) ifTrue: [ ^ data asArray ].")
		c.writeln("^ Array with: data")
		c.indent--
		c.writelnNoIndent("!")

		c.writeln("_save: rows path: p opts: o")
		c.indent++
		c.writeln("| fmt stream |")
		c.writeln("fmt := (o notNil and: [ o includesKey: 'format' ]) ifTrue: [ o at: 'format' ] ifFalse: [ 'json' ].")
		c.writeln("stream := (p isNil or: [ p = '' or: [ p = '-' ] ])")
		c.indent++
		c.writeln("ifTrue: [ stdout ]")
		c.writeln("ifFalse: [ FileStream open: p mode: FileStream write ].")
		c.indent--
		c.writeln("fmt = 'jsonl' ifTrue: [")
		c.indent++
		c.writeln("rows do: [:r | stream nextPutAll: (JSONReader toJSON: r); nextPut: Character nl ].")
		c.writeln("] ifFalse: [")
		c.indent++
		c.writeln("stream nextPutAll: (JSONReader toJSON: rows)")
		c.indent--
		c.writeln("] .")
		c.writeln("stream ~= stdout ifTrue: [ stream close ].")
		c.writeln("^ self")
		c.indent--
		c.writelnNoIndent("!")

	}
	if c.needFetch {
		c.writeln("_fetch: url opts: o")
		c.indent++
		c.writeln("| args cmd stream text |")
		c.writeln("args := OrderedCollection new.")
		c.writeln("args add: '-s'.")
		c.writeln("| method |")
		c.writeln("method := (o notNil and: [ o includesKey: 'method' ]) ifTrue: [ o at: 'method' ] ifFalse: [ 'GET' ].")
		c.writeln("args add: '-X'; add: method.")
		c.writeln("(o notNil and: [ o includesKey: 'headers' ]) ifTrue: [ (o at: 'headers') keysAndValuesDo: [:k :v | args add: '-H'; add: (k , ': ' , v printString) ] ].")
		c.writeln("(o notNil and: [ o includesKey: 'query' ]) ifTrue: [ | qs sep |")
		c.indent++
		c.writeln("qs := String streamContents: [:s | (o at: 'query') keysAndValuesDo: [:k :v | s nextPutAll: k; nextPut: '='; nextPutAll: v printString; nextPut: '&' ] ].")
		c.writeln("qs := qs copyFrom: 1 to: qs size - 1.")
		c.writeln("sep := (url includes: '?') ifTrue: [ '&' ] ifFalse: [ '?' ].")
		c.writeln("url := url , sep , qs.")
		c.indent--
		c.writeln("] .")
		c.writeln("(o notNil and: [ o includesKey: 'body' ]) ifTrue: [ args add: '-d'; add: (JSONReader toJSON: (o at: 'body')) ].")
		c.writeln("(o notNil and: [ o includesKey: 'timeout' ]) ifTrue: [ args add: '--max-time'; add: (o at: 'timeout') printString ].")
		c.writeln("args add: url.")
		c.writeln("cmd := 'curl ' , (String streamContents: [:s | args doWithIndex: [:a :i | s nextPutAll: a. i < args size ifTrue: [ s nextPut: Character space ] ] ]).")
		c.writeln("stream := PipeStream open: cmd.")
		c.writeln("text := stream contents.")
		c.writeln("stream close.")
		c.writeln("^ JSONReader fromJSON: text")
		c.indent--
		c.writelnNoIndent("!")
	}
	if c.needCast {
		c.writeln("_cast: type value: v")
		c.indent++
		c.writeln("^ v")
		c.indent--
		c.writelnNoIndent("!")
	}
	if c.needPaginate {
		c.writeln("_paginate: items skip: s take: t")
		c.indent++
		c.writeln("| out start |")
		c.writeln("out := items asArray.")
		c.writeln("start := s ifNil: [ 0 ] ifNotNil: [ s ].")
		c.writeln("start > 0 ifTrue: [ out := out copyFrom: start + 1 to: out size ].")
		c.writeln("t notNil ifTrue: [ out := out copyFrom: 1 to: (t min: out size) ].")
		c.writeln("^ out")
		c.indent--
		c.writelnNoIndent("!")
	}
	if c.needGroupBy {
		c.writeln("_group_by: src keyFn: blk")
		c.indent++
		c.writeln("| groups order |")
		c.writeln("groups := Dictionary new.")
		c.writeln("order := OrderedCollection new.")
		c.writeln("src do: [:it |")
		c.indent++
		c.writeln("| key ks g |")
		c.writeln("key := blk value: it.")
		c.writeln("ks := key printString.")
		c.writeln("g := groups at: ks ifAbsentPut: [ |_g | _g := _Group key: key. order add: ks. groups at: ks put: _g. _g ].")
		c.writeln("g add: it.")
		c.indent--
		c.writeln("]")
		c.writeln("^ order collect: [:k | groups at: k ]")
		c.indent--
		c.writelnNoIndent("!")
	}
}
