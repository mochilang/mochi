package scalacode

import (
	"bytes"
	"fmt"
	"strconv"
	"strings"

	"mochi/parser"
	"mochi/types"
)

// Compiler translates a Mochi AST into Scala source code (limited subset).
type Compiler struct {
	buf       bytes.Buffer
	indent    int
	env       *types.Env
	mainStmts []*parser.Statement
	retType   types.Type

	tempVarCount int
	loopStack    []loopLabels

	needCompare bool

	paramAlias map[string]string
}

type loopLabels struct {
	brk  string
	cont string
}

func (c *Compiler) newTemp(prefix string) string {
	c.tempVarCount++
	return fmt.Sprintf("%s%d", prefix, c.tempVarCount)
}

func containsBreak(stmts []*parser.Statement) bool {
	for _, s := range stmts {
		if s.Break != nil {
			return true
		}
		switch {
		case s.If != nil:
			if containsBreak(s.If.Then) || containsBreak(s.If.Else) || (s.If.ElseIf != nil && containsBreak([]*parser.Statement{{If: s.If.ElseIf}})) {
				return true
			}
		case s.For != nil:
			if containsBreak(s.For.Body) {
				return true
			}
		case s.While != nil:
			if containsBreak(s.While.Body) {
				return true
			}
		case s.Fun != nil:
			if containsBreak(s.Fun.Body) {
				return true
			}
		case s.Test != nil:
			if containsBreak(s.Test.Body) {
				return true
			}
		}
	}
	return false
}

func containsContinue(stmts []*parser.Statement) bool {
	for _, s := range stmts {
		if s.Continue != nil {
			return true
		}
		switch {
		case s.If != nil:
			if containsContinue(s.If.Then) || containsContinue(s.If.Else) || (s.If.ElseIf != nil && containsContinue([]*parser.Statement{{If: s.If.ElseIf}})) {
				return true
			}
		case s.For != nil:
			if containsContinue(s.For.Body) {
				return true
			}
		case s.While != nil:
			if containsContinue(s.While.Body) {
				return true
			}
		case s.Fun != nil:
			if containsContinue(s.Fun.Body) {
				return true
			}
		case s.Test != nil:
			if containsContinue(s.Test.Body) {
				return true
			}
		}
	}
	return false
}

func (c *Compiler) compileBreak() {
	if len(c.loopStack) == 0 {
		return
	}
	lbl := c.loopStack[len(c.loopStack)-1]
	if lbl.brk != "" {
		c.writeln(lbl.brk + ".break()")
	}
}

func (c *Compiler) compileContinue() {
	if len(c.loopStack) == 0 {
		return
	}
	lbl := c.loopStack[len(c.loopStack)-1]
	if lbl.cont != "" {
		c.writeln(lbl.cont + ".break()")
	}
}

// New creates a new Scala compiler instance.
func New(env *types.Env) *Compiler {
	return &Compiler{env: env, paramAlias: make(map[string]string)}
}

// Compile generates Scala code for prog.
func (c *Compiler) Compile(prog *parser.Program) ([]byte, error) {
	for _, s := range prog.Statements {
		if s.Type != nil {
			if err := c.compileTypeDecl(s.Type); err != nil {
				return nil, err
			}
			c.writeln("")
		}
	}

	c.writeln("object Main {")
	c.indent++
	for _, s := range prog.Statements {
		if s.Fun != nil {
			if err := c.compileFun(s.Fun); err != nil {
				return nil, err
			}
			c.writeln("")
		} else if s.Type == nil {
			c.mainStmts = append(c.mainStmts, s)
		}
	}
	c.writeln("def main(args: Array[String]): Unit = {")
	c.indent++
	for _, s := range c.mainStmts {
		if err := c.compileStmt(s); err != nil {
			return nil, err
		}
	}
	c.indent--
	c.writeln("}")
	if c.needCompare {
		c.writeln("")
		c.writeln("def _compare(a: Any, b: Any): Int = (a, b) match {")
		c.indent++
		c.writeln("case (x: Int, y: Int) => x.compare(y)")
		c.writeln("case (x: Double, y: Double) => java.lang.Double.compare(x, y)")
		c.writeln("case (x: String, y: String) => x.compareTo(y)")
		c.writeln("case _ => a.toString.compareTo(b.toString)")
		c.indent--
		c.writeln("}")
		c.writeln("implicit val _anyOrdering: Ordering[Any] = new Ordering[Any] { def compare(x: Any, y: Any): Int = _compare(x, y) }")
	}
	c.indent--
	c.writeln("}")
	return c.buf.Bytes(), nil
}

func (c *Compiler) compileFun(fn *parser.FunStmt) error {
	origRet := c.retType
	if fn.Return != nil {
		c.retType = c.resolveTypeRef(fn.Return)
	} else {
		c.retType = types.VoidType{}
	}
	origEnv := c.env
	child := types.NewEnv(c.env)
	for _, p := range fn.Params {
		child.SetVar(p.Name, c.resolveTypeRef(p.Type), true)
	}
	c.env = child
	c.writeIndent()
	c.buf.WriteString("def " + sanitizeName(fn.Name) + "(")
	for i, p := range fn.Params {
		if i > 0 {
			c.buf.WriteString(", ")
		}
		c.buf.WriteString(fmt.Sprintf("%s: %s", sanitizeName(p.Name), scalaType(c.resolveTypeRef(p.Type))))
	}
	c.buf.WriteString(")")
	if fn.Return != nil {
		c.buf.WriteString(": " + scalaType(c.resolveTypeRef(fn.Return)))
	}
	c.buf.WriteString(" = {\n")
	c.indent++
	origAlias := c.paramAlias
	c.paramAlias = make(map[string]string)
	for _, p := range fn.Params {
		if paramMutated(fn.Body, p.Name) {
			alias := sanitizeName(p.Name) + "_var"
			c.paramAlias[p.Name] = alias
			typ := scalaType(c.resolveTypeRef(p.Type))
			c.writeln(fmt.Sprintf("var %s: %s = %s", alias, typ, sanitizeName(p.Name)))
		}
	}
	for _, st := range fn.Body {
		if err := c.compileStmt(st); err != nil {
			c.paramAlias = origAlias
			return err
		}
	}
	c.paramAlias = origAlias
	c.env = origEnv
	c.indent--
	c.writeln("}")
	c.retType = origRet
	return nil
}

func paramMutated(body []*parser.Statement, name string) bool {
	for _, st := range body {
		if st.Assign != nil && st.Assign.Name == name && len(st.Assign.Index) == 0 {
			return true
		}
		if st.If != nil {
			if paramMutated(st.If.Then, name) || paramMutated(st.If.Else, name) || (st.If.ElseIf != nil && paramMutated([]*parser.Statement{&parser.Statement{If: st.If.ElseIf}}, name)) {
				return true
			}
		}
		if st.For != nil {
			if paramMutated(st.For.Body, name) {
				return true
			}
		}
		if st.While != nil {
			if paramMutated(st.While.Body, name) {
				return true
			}
		}
	}
	return false
}

func (c *Compiler) compileStmt(s *parser.Statement) error {
	switch {
	case s.Let != nil:
		return c.compileLet(s.Let)
	case s.Var != nil:
		return c.compileVar(s.Var)
	case s.Fun != nil:
		return c.compileFun(s.Fun)
	case s.Assign != nil:
		return c.compileAssign(s.Assign)
	case s.Return != nil:
		return c.compileReturn(s.Return)
	case s.For != nil:
		return c.compileFor(s.For)
	case s.While != nil:
		return c.compileWhile(s.While)
	case s.If != nil:
		return c.compileIf(s.If)
	case s.Break != nil:
		c.compileBreak()
		return nil
	case s.Continue != nil:
		c.compileContinue()
		return nil
	case s.Expr != nil:
		expr, err := c.compileExpr(s.Expr.Expr)
		if err != nil {
			return err
		}
		if expr != "" {
			c.writeln(expr)
		}
		return nil
	default:
		return nil
	}
}

func (c *Compiler) compileLet(st *parser.LetStmt) error {
	expr, err := c.compileExpr(st.Value)
	if err != nil {
		return err
	}
	if expr == "" {
		c.writeln(fmt.Sprintf("val %s", sanitizeName(st.Name)))
	} else {
		c.writeln(fmt.Sprintf("val %s = %s", sanitizeName(st.Name), expr))
	}
	return nil
}

func (c *Compiler) compileVar(st *parser.VarStmt) error {
	value := ""
	if st.Value != nil {
		v, err := c.compileExpr(st.Value)
		if err != nil {
			return err
		}
		value = " = " + v
	}

	typ := ""
	if st.Type != nil {
		typ = ": " + scalaType(c.resolveTypeRef(st.Type))
	}

	if st.Value != nil && emptyListExpr(st.Value) {
		if typ == "" {
			if lt, ok := c.retType.(types.ListType); ok {
				typ = ": " + scalaType(lt)
			} else {
				typ = ": scala.collection.mutable.ArrayBuffer[Any]"
			}
		}
		value = " = scala.collection.mutable.ArrayBuffer()"
	}

	c.writeln(fmt.Sprintf("var %s%s%s", sanitizeName(st.Name), typ, value))
	return nil
}

func (c *Compiler) compileAssign(st *parser.AssignStmt) error {
	lhs := sanitizeName(st.Name)
	if alias, ok := c.paramAlias[st.Name]; ok {
		lhs = alias
	}
	if len(st.Index) == 0 {
		rhs, err := c.compileExpr(st.Value)
		if err != nil {
			return err
		}
		c.writeln(fmt.Sprintf("%s = %s", lhs, rhs))
		return nil
	}

	idx, err := c.compileExpr(st.Index[0].Start)
	if err != nil {
		return err
	}
	rhs, err := c.compileExpr(st.Value)
	if err != nil {
		return err
	}
	c.writeln(fmt.Sprintf("%s.update(%s, %s)", lhs, idx, rhs))
	return nil
}

func (c *Compiler) compileReturn(st *parser.ReturnStmt) error {
	expr, err := c.compileExpr(st.Value)
	if err != nil {
		return err
	}
	c.writeln("return " + expr)
	return nil
}

func (c *Compiler) compileWhile(st *parser.WhileStmt) error {
	cond, err := c.compileExpr(st.Cond)
	if err != nil {
		return err
	}
	needBreak := containsBreak(st.Body)
	needCont := containsContinue(st.Body)
	lbl := loopLabels{}
	if needBreak || needCont {
		lbl.brk = c.newTemp("brk")
		c.writeln(fmt.Sprintf("val %s = new scala.util.control.Breaks", lbl.brk))
		c.writeln(fmt.Sprintf("%s.breakable {", lbl.brk))
		c.indent++
	}
	c.loopStack = append(c.loopStack, lbl)
	c.writeln(fmt.Sprintf("while (%s) {", cond))
	c.indent++
	if needCont {
		lbl.cont = c.newTemp("cont")
		c.writeln(fmt.Sprintf("val %s = new scala.util.control.Breaks", lbl.cont))
		c.writeln(fmt.Sprintf("%s.breakable {", lbl.cont))
		c.indent++
		c.loopStack[len(c.loopStack)-1] = lbl
	}
	for _, s := range st.Body {
		if err := c.compileStmt(s); err != nil {
			c.loopStack = c.loopStack[:len(c.loopStack)-1]
			return err
		}
	}
	if needCont {
		c.indent--
		c.writeln("}")
	}
	c.indent--
	c.writeln("}")
	c.loopStack = c.loopStack[:len(c.loopStack)-1]
	if needBreak || needCont {
		c.indent--
		c.writeln("}")
	}
	return nil
}

func (c *Compiler) compileFor(st *parser.ForStmt) error {
	name := sanitizeName(st.Name)
	needBreak := containsBreak(st.Body)
	needCont := containsContinue(st.Body)
	lbl := loopLabels{}
	if needBreak || needCont {
		lbl.brk = c.newTemp("brk")
		c.writeln(fmt.Sprintf("val %s = new scala.util.control.Breaks", lbl.brk))
		c.writeln(fmt.Sprintf("%s.breakable {", lbl.brk))
		c.indent++
	}

	c.loopStack = append(c.loopStack, lbl)
	if st.RangeEnd != nil {
		start, err := c.compileExpr(st.Source)
		if err != nil {
			c.loopStack = c.loopStack[:len(c.loopStack)-1]
			return err
		}
		end, err := c.compileExpr(st.RangeEnd)
		if err != nil {
			c.loopStack = c.loopStack[:len(c.loopStack)-1]
			return err
		}
		idx := c.newTemp("i")
		c.writeln(fmt.Sprintf("var %s = %s", idx, start))
		c.writeln(fmt.Sprintf("while (%s < %s) {", idx, end))
		c.indent++
		c.writeln(fmt.Sprintf("val %s = %s", name, idx))
		c.writeln(fmt.Sprintf("%s = %s + 1", idx, idx))
	} else {
		src, err := c.compileExpr(st.Source)
		if err != nil {
			c.loopStack = c.loopStack[:len(c.loopStack)-1]
			return err
		}
		it := c.newTemp("it")
		c.writeln(fmt.Sprintf("val %s = %s.iterator", it, src))
		c.writeln(fmt.Sprintf("while (%s.hasNext) {", it))
		c.indent++
		c.writeln(fmt.Sprintf("val %s = %s.next()", name, it))
	}

	if needCont {
		lbl.cont = c.newTemp("cont")
		c.writeln(fmt.Sprintf("val %s = new scala.util.control.Breaks", lbl.cont))
		c.writeln(fmt.Sprintf("%s.breakable {", lbl.cont))
		c.indent++
		c.loopStack[len(c.loopStack)-1] = lbl
	}

	for _, s := range st.Body {
		if err := c.compileStmt(s); err != nil {
			c.loopStack = c.loopStack[:len(c.loopStack)-1]
			return err
		}
	}

	if needCont {
		c.indent--
		c.writeln("}")
	}
	c.indent--
	c.writeln("}")
	c.loopStack = c.loopStack[:len(c.loopStack)-1]
	if needBreak || needCont {
		c.indent--
		c.writeln("}")
	}
	return nil
}

func (c *Compiler) compileIf(st *parser.IfStmt) error {
	cond, err := c.compileExpr(st.Cond)
	if err != nil {
		return err
	}
	c.writeIndent()
	c.buf.WriteString("if (" + cond + ") {\n")
	c.indent++
	for _, s := range st.Then {
		if err := c.compileStmt(s); err != nil {
			return err
		}
	}
	c.indent--
	c.writeIndent()
	c.buf.WriteString("}")
	if st.ElseIf != nil {
		c.buf.WriteString(" else ")
		return c.compileIf(st.ElseIf)
	}
	if len(st.Else) > 0 {
		c.buf.WriteString(" else {\n")
		c.indent++
		for _, s := range st.Else {
			if err := c.compileStmt(s); err != nil {
				return err
			}
		}
		c.indent--
		c.writeIndent()
		c.buf.WriteString("}")
	}
	c.buf.WriteByte('\n')
	return nil
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

	ops := []string{}
	operands := []string{left}
	nodes := []*parser.PostfixExpr{b.Left.Value}
	for _, part := range b.Right {
		r, err := c.compilePostfix(part.Right)
		if err != nil {
			return "", err
		}
		operands = append(operands, r)
		nodes = append(nodes, part.Right)
		ops = append(ops, part.Op)
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

			l := operands[i]
			r := operands[i+1]
			op := ops[i]

			var expr string
			if op == "in" {
				expr = fmt.Sprintf("%s.contains(%s)", r, l)
			} else if op == "+" && nodes[i+1].Target.List != nil {
				expr = fmt.Sprintf("(%s ++ %s)", l, r)
			} else {
				expr = fmt.Sprintf("(%s %s %s)", l, op, r)
			}

			operands[i] = expr
			operands = append(operands[:i+1], operands[i+2:]...)
			nodes = append(nodes[:i+1], nodes[i+2:]...)
			ops = append(ops[:i], ops[i+1:]...)
		}
	}

	if len(operands) != 1 {
		return "", fmt.Errorf("unexpected state after binary compilation")
	}
	return operands[0], nil
}

func (c *Compiler) compileUnary(u *parser.Unary) (string, error) {
	expr, err := c.compilePostfix(u.Value)
	if err != nil {
		return "", err
	}
	for i := len(u.Ops) - 1; i >= 0; i-- {
		expr = fmt.Sprintf("(%s%s)", u.Ops[i], expr)
	}
	return expr, nil
}

func (c *Compiler) compilePostfix(p *parser.PostfixExpr) (string, error) {
	expr, err := c.compilePrimary(p.Target)
	if err != nil {
		return "", err
	}
	for _, op := range p.Ops {
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
				end := fmt.Sprintf("%s.length", expr)
				if op.Index.End != nil {
					e, err := c.compileExpr(op.Index.End)
					if err != nil {
						return "", err
					}
					end = e
				}
				expr = fmt.Sprintf("%s.slice(%s, %s)", expr, start, end)
			} else {
				idx, err := c.compileExpr(op.Index.Start)
				if err != nil {
					return "", err
				}
				expr = fmt.Sprintf("%s(%s)", expr, idx)
			}
			continue
		}
		if op.Call != nil {
			args := make([]string, len(op.Call.Args))
			for i, a := range op.Call.Args {
				v, err := c.compileExpr(a)
				if err != nil {
					return "", err
				}
				args[i] = v
			}
			expr = fmt.Sprintf("%s(%s)", expr, strings.Join(args, ", "))
			continue
		}
		if op.Cast != nil {
			typ := scalaType(c.resolveTypeRef(op.Cast.Type))
			expr = fmt.Sprintf("%s.asInstanceOf[%s]", expr, typ)
			continue
		}
	}
	return expr, nil
}

func (c *Compiler) compilePrimary(p *parser.Primary) (string, error) {
	switch {
	case p.Struct != nil:
		parts := make([]string, len(p.Struct.Fields))
		for i, f := range p.Struct.Fields {
			v, err := c.compileExpr(f.Value)
			if err != nil {
				return "", err
			}
			parts[i] = fmt.Sprintf("%s = %s", sanitizeName(f.Name), v)
		}
		return fmt.Sprintf("%s(%s)", sanitizeName(p.Struct.Name), strings.Join(parts, ", ")), nil
	case p.Lit != nil:
		if p.Lit.Int != nil {
			return strconv.Itoa(*p.Lit.Int), nil
		}
		if p.Lit.Float != nil {
			return strconv.FormatFloat(*p.Lit.Float, 'f', -1, 64), nil
		}
		if p.Lit.Bool != nil {
			if bool(*p.Lit.Bool) {
				return "true", nil
			}
			return "false", nil
		}
		if p.Lit.Str != nil {
			return strconv.Quote(*p.Lit.Str), nil
		}
	case p.List != nil:
		elems := make([]string, len(p.List.Elems))
		for i, e := range p.List.Elems {
			v, err := c.compileExpr(e)
			if err != nil {
				return "", err
			}
			elems[i] = v
		}
		return "scala.collection.mutable.ArrayBuffer(" + strings.Join(elems, ", ") + ")", nil
	case p.Map != nil:
		items := make([]string, len(p.Map.Items))
		for i, it := range p.Map.Items {
			k, err := c.compileExpr(it.Key)
			if err != nil {
				return "", err
			}
			// keep keys as regular strings
			v, err := c.compileExpr(it.Value)
			if err != nil {
				return "", err
			}
			items[i] = fmt.Sprintf("%s -> %s", k, v)
		}
		return "scala.collection.mutable.Map(" + strings.Join(items, ", ") + ")", nil
	case p.Selector != nil:
		name := p.Selector.Root
		if alias, ok := c.paramAlias[name]; ok {
			name = alias
		} else {
			name = sanitizeName(name)
		}
		for _, part := range p.Selector.Tail {
			name += "." + sanitizeName(part)
		}
		return name, nil
	case p.Group != nil:
		expr, err := c.compileExpr(p.Group)
		if err != nil {
			return "", err
		}
		return "(" + expr + ")", nil
	case p.Call != nil:
		return c.compileCall(p.Call, "")
	case p.FunExpr != nil:
		return c.compileFunExpr(p.FunExpr)
	case p.Match != nil:
		return c.compileMatchExpr(p.Match)
	case p.Query != nil:
		return c.compileQueryExpr(p.Query)
	}
	return "", fmt.Errorf("unsupported expression")
}

func (c *Compiler) compileCall(call *parser.CallExpr, recv string) (string, error) {
	args := make([]string, len(call.Args))
	for i, a := range call.Args {
		v, err := c.compileExpr(a)
		if err != nil {
			return "", err
		}
		args[i] = v
	}
	argStr := strings.Join(args, ", ")
	if recv != "" {
		// method call is not used yet
	}
	switch call.Func {
	case "print":
		return fmt.Sprintf("println(%s)", argStr), nil
	case "len":
		if len(args) != 1 {
			return "", fmt.Errorf("len expects 1 arg")
		}
		if isMapExpr(call.Args[0], c.env) {
			return fmt.Sprintf("%s.size", args[0]), nil
		}
		return fmt.Sprintf("%s.length", args[0]), nil
	case "count":
		if len(args) != 1 {
			return "", fmt.Errorf("count expects 1 arg")
		}
		return fmt.Sprintf("%s.size", args[0]), nil
	case "avg":
		if len(args) != 1 {
			return "", fmt.Errorf("avg expects 1 arg")
		}
		a := args[0]
		return fmt.Sprintf("(%s.sum / %s.size)", a, a), nil
	case "input":
		if len(args) != 0 {
			return "", fmt.Errorf("input expects no args")
		}
		return "scala.io.StdIn.readLine()", nil
	case "str":
		if len(args) == 1 {
			return args[0] + ".toString()", nil
		}
	}
	return fmt.Sprintf("%s(%s)", sanitizeName(call.Func), argStr), nil
}

func (c *Compiler) compileFunExpr(fn *parser.FunExpr) (string, error) {
	params := make([]string, len(fn.Params))
	for i, p := range fn.Params {
		typ := scalaType(c.resolveTypeRef(p.Type))
		params[i] = fmt.Sprintf("%s: %s", sanitizeName(p.Name), typ)
	}

	origEnv := c.env
	child := types.NewEnv(c.env)
	for _, p := range fn.Params {
		if p.Type != nil {
			child.SetVar(p.Name, c.resolveTypeRef(p.Type), true)
		}
	}
	c.env = child

	var body string
	if fn.ExprBody != nil {
		expr, err := c.compileExpr(fn.ExprBody)
		if err != nil {
			c.env = origEnv
			return "", err
		}
		body = expr
	} else {
		var buf bytes.Buffer
		savedBuf := c.buf
		savedIndent := c.indent
		c.buf = buf
		c.indent = 0
		for _, st := range fn.BlockBody {
			if err := c.compileStmt(st); err != nil {
				c.env = origEnv
				c.buf = savedBuf
				c.indent = savedIndent
				return "", err
			}
		}
		body = "{\n" + indentBlock(c.buf.String(), 1) + "}"
		c.buf = savedBuf
		c.indent = savedIndent
	}
	c.env = origEnv

	return fmt.Sprintf("(%s) => %s", strings.Join(params, ", "), body), nil
}

func (c *Compiler) compileQueryExpr(q *parser.QueryExpr) (string, error) {
	if len(q.Joins) > 0 || q.Group != nil {
		return "", fmt.Errorf("unsupported query expression")
	}
	src, err := c.compileExpr(q.Source)
	if err != nil {
		return "", err
	}
	orig := c.env
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
	c.env = child
	sel, err := c.compileExpr(q.Select)
	if err != nil {
		c.env = orig
		return "", err
	}
	var cond, sortExpr string
	if q.Where != nil {
		cond, err = c.compileExpr(q.Where)
		if err != nil {
			c.env = orig
			return "", err
		}
	}
	if q.Sort != nil {
		sortExpr, err = c.compileExpr(q.Sort)
		if err != nil {
			c.env = orig
			return "", err
		}
	}
	c.env = orig
	res := c.newTemp("res")
	var b strings.Builder
	b.WriteString("(() => {\n")
	if sortExpr != "" {
		b.WriteString("\tval " + res + " = scala.collection.mutable.ArrayBuffer[(Any, Any)]()\n")
	} else {
		b.WriteString("\tval " + res + " = scala.collection.mutable.ArrayBuffer[Any]()\n")
	}
	indent := "\t"
	b.WriteString(fmt.Sprintf(indent+"for (%s <- %s) {\n", sanitizeName(q.Var), src))
	indent += "\t"
	for i, f := range q.Froms {
		b.WriteString(fmt.Sprintf(indent+"for (%s <- %s) {\n", sanitizeName(f.Var), fromSrcs[i]))
		indent += "\t"
	}
	if cond != "" {
		b.WriteString(fmt.Sprintf(indent+"if (%s) {\n", cond))
		indent += "\t"
	}
	if sortExpr != "" {
		b.WriteString(fmt.Sprintf(indent+"%s.append((%s, %s))\n", res, sel, sortExpr))
	} else {
		b.WriteString(fmt.Sprintf(indent+"%s.append(%s)\n", res, sel))
	}
	if cond != "" {
		indent = indent[:len(indent)-1]
		b.WriteString(indent + "}\n")
	}
	for i := len(q.Froms) - 1; i >= 0; i-- {
		indent = indent[:len(indent)-1]
		b.WriteString(indent + "}\n")
	}
	indent = indent[:len(indent)-1]
	b.WriteString(indent + "}\n")
	seq := c.newTemp("seq")
	if sortExpr != "" {
		c.needCompare = true
		b.WriteString(fmt.Sprintf("\tvar %s = %s.sortBy(_._2)(_anyOrdering).map(_._1).toSeq\n", seq, res))
	} else {
		b.WriteString(fmt.Sprintf("\tvar %s = %s.toSeq\n", seq, res))
	}
	if q.Skip != nil {
		skipExpr, err := c.compileExpr(q.Skip)
		if err != nil {
			return "", err
		}
		b.WriteString(fmt.Sprintf("\t%s = %s.drop(%s)\n", seq, seq, skipExpr))
	}
	if q.Take != nil {
		takeExpr, err := c.compileExpr(q.Take)
		if err != nil {
			return "", err
		}
		b.WriteString(fmt.Sprintf("\t%s = %s.take(%s)\n", seq, seq, takeExpr))
	}
	b.WriteString(fmt.Sprintf("\t%s\n", seq))
	b.WriteString("})()")
	return b.String(), nil
}

func (c *Compiler) resolveTypeRef(t *parser.TypeRef) types.Type {
	if t == nil {
		return types.AnyType{}
	}
	if t.Simple != nil {
		switch *t.Simple {
		case "int":
			return types.IntType{}
		case "float":
			return types.FloatType{}
		case "bool":
			return types.BoolType{}
		case "string":
			return types.StringType{}
		}
	}
	if t.Generic != nil {
		if t.Generic.Name == "list" && len(t.Generic.Args) == 1 {
			return types.ListType{Elem: c.resolveTypeRef(t.Generic.Args[0])}
		}
		if t.Generic.Name == "map" && len(t.Generic.Args) == 2 {
			return types.MapType{Key: c.resolveTypeRef(t.Generic.Args[0]), Value: c.resolveTypeRef(t.Generic.Args[1])}
		}
	}
	if t.Fun != nil {
		params := make([]types.Type, len(t.Fun.Params))
		for i, p := range t.Fun.Params {
			params[i] = c.resolveTypeRef(p)
		}
		var ret types.Type = types.VoidType{}
		if t.Fun.Return != nil {
			ret = c.resolveTypeRef(t.Fun.Return)
		}
		return types.FuncType{Params: params, Return: ret}
	}
	return types.AnyType{}
}

func scalaType(t types.Type) string {
	switch tt := t.(type) {
	case types.IntType, types.Int64Type:
		return "Int"
	case types.FloatType:
		return "Double"
	case types.BoolType:
		return "Boolean"
	case types.StringType:
		return "String"
	case types.ListType:
		return "scala.collection.mutable.ArrayBuffer[" + scalaType(tt.Elem) + "]"
	case types.MapType:
		return "scala.collection.mutable.Map[" + scalaType(tt.Key) + ", " + scalaType(tt.Value) + "]"
	case types.FuncType:
		params := make([]string, len(tt.Params))
		for i, p := range tt.Params {
			params[i] = scalaType(p)
		}
		ret := "Unit"
		if tt.Return != nil {
			ret = scalaType(tt.Return)
		}
		if len(params) == 1 {
			return fmt.Sprintf("(%s => %s)", params[0], ret)
		}
		return fmt.Sprintf("(%s) => %s", strings.Join(params, ", "), ret)
	default:
		return "Any"
	}
}

var scalaKeywords = map[string]struct{}{
	"abstract": {}, "case": {}, "catch": {}, "class": {}, "def": {}, "do": {},
	"else": {}, "extends": {}, "false": {}, "final": {}, "finally": {},
	"for": {}, "if": {}, "implicit": {}, "import": {}, "lazy": {}, "match": {},
	"new": {}, "null": {}, "object": {}, "override": {}, "package": {},
	"private": {}, "protected": {}, "return": {}, "sealed": {}, "super": {},
	"this": {}, "throw": {}, "trait": {}, "try": {}, "true": {}, "type": {},
	"val": {}, "var": {}, "while": {}, "with": {}, "yield": {},
}

func (c *Compiler) writeln(s string) {
	c.writeIndent()
	c.buf.WriteString(s)
	c.buf.WriteByte('\n')
}

func (c *Compiler) writeIndent() {
	for i := 0; i < c.indent; i++ {
		c.buf.WriteByte('\t')
	}
}

func sanitizeName(name string) string {
	if name == "" {
		return ""
	}
	var b strings.Builder
	for i, r := range name {
		if r == '_' || ('a' <= r && r <= 'z') || ('A' <= r && r <= 'Z') || ('0' <= r && r <= '9' && i > 0) {
			b.WriteRune(r)
		} else {
			b.WriteRune('_')
		}
	}
	s := b.String()
	if s == "" {
		return "_"
	}
	if !(s[0] >= 'a' && s[0] <= 'z' || s[0] >= 'A' && s[0] <= 'Z' || s[0] == '_') {
		s = "_" + s
	}
	if _, ok := scalaKeywords[s]; ok {
		s = "_" + s
	}
	return s
}

func joinArgs(args []string) string {
	if len(args) == 0 {
		return ""
	}
	res := args[0]
	for i := 1; i < len(args); i++ {
		res += ", " + args[i]
	}
	return res
}

func (c *Compiler) compileTypeDecl(t *parser.TypeDecl) error {
	name := sanitizeName(t.Name)
	if len(t.Variants) > 0 {
		c.writeln("sealed trait " + name)
		for _, v := range t.Variants {
			vname := sanitizeName(v.Name)
			fields := make([]string, len(v.Fields))
			for i, f := range v.Fields {
				typ := scalaType(c.resolveTypeRef(f.Type))
				fields[i] = fmt.Sprintf("%s: %s", sanitizeName(f.Name), typ)
			}
			params := strings.Join(fields, ", ")
			c.writeln(fmt.Sprintf("case class %s(%s) extends %s", vname, params, name))
		}
		return nil
	}
	fields := []string{}
	for _, m := range t.Members {
		if m.Field != nil {
			typ := scalaType(c.resolveTypeRef(m.Field.Type))
			fields = append(fields, fmt.Sprintf("%s: %s", sanitizeName(m.Field.Name), typ))
		}
	}
	c.writeln(fmt.Sprintf("case class %s(%s)", name, strings.Join(fields, ", ")))
	return nil
}

func (c *Compiler) compileMatchExpr(m *parser.MatchExpr) (string, error) {
	target, err := c.compileExpr(m.Target)
	if err != nil {
		return "", err
	}
	var b strings.Builder
	b.WriteString("(" + target + " match {\n")
	for _, cs := range m.Cases {
		pat, err := c.compileMatchPattern(cs.Pattern)
		if err != nil {
			return "", err
		}
		res, err := c.compileExpr(cs.Result)
		if err != nil {
			return "", err
		}
		b.WriteString("\tcase " + pat + " => " + res + "\n")
	}
	b.WriteString("})")
	return b.String(), nil
}

func (c *Compiler) compileMatchPattern(pat *parser.Expr) (string, error) {
	if isUnderscoreExpr(pat) {
		return "_", nil
	}
	if call, ok := callPattern(pat); ok {
		args := make([]string, len(call.Args))
		for i, a := range call.Args {
			if name, ok := identName(a); ok {
				args[i] = sanitizeName(name)
			} else {
				return "", fmt.Errorf("invalid pattern")
			}
		}
		return fmt.Sprintf("%s(%s)", sanitizeName(call.Func), strings.Join(args, ", ")), nil
	}
	if id, ok := identName(pat); ok {
		return sanitizeName(id), nil
	}
	return c.compileExpr(pat)
}
