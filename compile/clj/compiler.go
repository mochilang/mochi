package cljcode

import (
	"bytes"
	"fmt"
	"strconv"
	"strings"

	"mochi/parser"
	"mochi/types"
)

// Compiler translates a Mochi AST into Clojure source code (limited subset).
type Compiler struct {
	buf          bytes.Buffer
	indent       int
	env          *types.Env
	mainStmts    []*parser.Statement
	tempVarCount int
}

// New creates a new Clojure compiler instance.
func New(env *types.Env) *Compiler {
	return &Compiler{env: env, tempVarCount: 0}
}

// Compile generates Clojure code for prog.
func (c *Compiler) Compile(prog *parser.Program) ([]byte, error) {
	for _, s := range prog.Statements {
		switch {
		case s.Fun != nil:
			if err := c.compileFun(s.Fun); err != nil {
				return nil, err
			}
			c.writeln("")
		case s.Test != nil:
			if err := c.compileTestBlock(s.Test); err != nil {
				return nil, err
			}
			c.writeln("")
		default:
			c.mainStmts = append(c.mainStmts, s)
		}
	}
	for _, s := range c.mainStmts {
		if err := c.compileStmt(s); err != nil {
			return nil, err
		}
	}
	for _, s := range prog.Statements {
		if s.Test != nil {
			c.writeln("(" + "test_" + sanitizeName(s.Test.Name) + ")")
		}
	}
	return c.buf.Bytes(), nil
}

func (c *Compiler) compileFun(fn *parser.FunStmt) error {
	origEnv := c.env
	if origEnv != nil {
		child := types.NewEnv(origEnv)
		if t, err := origEnv.GetVar(fn.Name); err == nil {
			if ft, ok := t.(types.FuncType); ok {
				for i, p := range fn.Params {
					if i < len(ft.Params) {
						child.SetVar(p.Name, ft.Params[i], true)
					} else {
						child.SetVar(p.Name, types.AnyType{}, true)
					}
				}
			}
		}
		c.env = child
		defer func() { c.env = origEnv }()
	}

	c.writeIndent()
	c.buf.WriteString("(defn " + sanitizeName(fn.Name) + " [")
	for i, p := range fn.Params {
		if i > 0 {
			c.buf.WriteString(" ")
		}
		c.buf.WriteString(sanitizeName(p.Name))
	}
	c.buf.WriteString("]\n")
	c.indent++
	c.writeln("(try")
	c.indent++
	for _, st := range fn.Body {
		if err := c.compileStmt(st); err != nil {
			return err
		}
	}
	c.indent--
	c.writeln("(catch clojure.lang.ExceptionInfo e")
	c.indent++
	c.writeln("(if (= (.getMessage e) \"return\")")
	c.indent++
	c.writeln("(:value (ex-data e))")
	c.indent--
	c.writeln("(throw e)))")
	c.indent--
	c.writeln(")")
	c.indent--
	c.writeln(")")
	return nil
}

func (c *Compiler) compileTestBlock(t *parser.TestBlock) error {
	name := "test_" + sanitizeName(t.Name)
	c.writeIndent()
	c.buf.WriteString("(defn " + name + " []\n")
	c.indent++
	for _, s := range t.Body {
		if err := c.compileStmt(s); err != nil {
			return err
		}
	}
	c.indent--
	c.writeln(")")
	return nil
}

func (c *Compiler) compileExpect(e *parser.ExpectStmt) error {
	expr, err := c.compileExpr(e.Value)
	if err != nil {
		return err
	}
	c.writeln(fmt.Sprintf("(assert %s)", expr))
	return nil
}

func (c *Compiler) compileStmt(s *parser.Statement) error {
	switch {
	case s.Let != nil:
		return c.compileLet(s.Let)
	case s.Var != nil:
		return c.compileVar(s.Var)
	case s.Assign != nil:
		return c.compileAssign(s.Assign)
	case s.Return != nil:
		return c.compileReturn(s.Return)
	case s.For != nil:
		return c.compileFor(s.For)
	case s.While != nil:
		return c.compileWhile(s.While)
	case s.Break != nil:
		c.writeln("(throw (ex-info \"break\" {}))")
		return nil
	case s.Continue != nil:
		c.writeln("(throw (ex-info \"continue\" {}))")
		return nil
	case s.If != nil:
		return c.compileIf(s.If)
	case s.Expect != nil:
		return c.compileExpect(s.Expect)
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
		return fmt.Errorf("unsupported statement")
	}
}

func (c *Compiler) compileLet(st *parser.LetStmt) error {
	expr, err := c.compileExpr(st.Value)
	if err != nil {
		return err
	}
	c.writeln(fmt.Sprintf("(def %s %s)", sanitizeName(st.Name), expr))
	if c.env != nil {
		var typ types.Type = types.AnyType{}
		if st.Type != nil {
			typ = c.resolveTypeRef(st.Type)
		} else {
			typ = c.inferExprType(st.Value)
		}
		c.env.SetVar(st.Name, typ, true)
	}
	return nil
}

func (c *Compiler) compileVar(st *parser.VarStmt) error {
	expr := "nil"
	if st.Value != nil {
		v, err := c.compileExpr(st.Value)
		if err != nil {
			return err
		}
		expr = v
	}
	c.writeln(fmt.Sprintf("(def %s %s)", sanitizeName(st.Name), expr))
	if c.env != nil {
		var typ types.Type = types.AnyType{}
		if st.Type != nil {
			typ = c.resolveTypeRef(st.Type)
		} else if st.Value != nil {
			typ = c.inferExprType(st.Value)
		}
		c.env.SetVar(st.Name, typ, true)
	}
	return nil
}

func (c *Compiler) compileAssign(st *parser.AssignStmt) error {
	name := sanitizeName(st.Name)
	rhs, err := c.compileExpr(st.Value)
	if err != nil {
		return err
	}
	if len(st.Index) == 0 {
		c.writeln(fmt.Sprintf("(def %s %s)", name, rhs))
		if c.env != nil {
			typ := c.inferExprType(st.Value)
			c.env.SetVar(st.Name, typ, true)
		}
		return nil
	}
	idxs := make([]string, len(st.Index))
	for i, idx := range st.Index {
		v, err := c.compileExpr(idx.Start)
		if err != nil {
			return err
		}
		idxs[i] = v
	}
	if len(idxs) == 1 {
		c.writeln(fmt.Sprintf("(def %s (assoc %s %s %s))", name, name, idxs[0], rhs))
	} else {
		c.writeln(fmt.Sprintf("(def %s (assoc-in %s [%s] %s))", name, name, strings.Join(idxs, " "), rhs))
	}
	if c.env != nil {
		if t, err := c.env.GetVar(st.Name); err == nil {
			c.env.SetVar(st.Name, t, true)
		}
	}
	return nil
}

func (c *Compiler) compileReturn(st *parser.ReturnStmt) error {
	expr, err := c.compileExpr(st.Value)
	if err != nil {
		return err
	}
	if expr == "" {
		expr = "nil"
	}
	c.writeln(fmt.Sprintf("(throw (ex-info \"return\" {:value %s}))", expr))
	return nil
}

func (c *Compiler) compileFor(st *parser.ForStmt) error {
	name := sanitizeName(st.Name)
	if st.RangeEnd != nil {
		start, err := c.compileExpr(st.Source)
		if err != nil {
			return err
		}
		end, err := c.compileExpr(st.RangeEnd)
		if err != nil {
			return err
		}
		c.writeln(fmt.Sprintf("(loop [%s %s]", name, start))
		c.indent++
		c.writeln(fmt.Sprintf("(when (< %s %s)", name, end))
		c.indent++
		c.writeln("(let [r (try")
		c.indent++
		for _, s := range st.Body {
			if err := c.compileStmt(s); err != nil {
				return err
			}
		}
		c.writeln(":next")
		c.indent--
		c.writeln("(catch clojure.lang.ExceptionInfo e")
		c.indent++
		c.writeln("(cond")
		c.indent++
		c.writeln("(= (.getMessage e) \"continue\") :next")
		c.writeln("(= (.getMessage e) \"break\") :break")
		c.writeln(":else (throw e))")
		c.indent--
		c.writeln(")")
		c.indent--
		c.writeln(")]")
		c.indent--
		c.writeln("(cond")
		c.indent++
		c.writeln("(= r :break) nil")
		c.writeln(fmt.Sprintf(":else (recur (inc %s))", name))
		c.indent--
		c.writeln(")")
		c.indent--
		c.writeln(")")
		c.indent--
		c.writeln(")")
		c.indent--
		c.writeln(")")
		return nil
	}
	src, err := c.compileExpr(st.Source)
	if err != nil {
		return err
	}
	seqVar := c.newTemp()
	c.writeln(fmt.Sprintf("(loop [%s (seq %s)]", seqVar, src))
	c.indent++
	c.writeln(fmt.Sprintf("(when %s", seqVar))
	c.indent++
	c.writeln(fmt.Sprintf("(let [%s (first %s)]", name, seqVar))
	c.indent++
	c.writeln("(let [r (try")
	c.indent++
	for _, s := range st.Body {
		if err := c.compileStmt(s); err != nil {
			return err
		}
	}
	c.writeln(":next")
	c.indent--
	c.writeln("(catch clojure.lang.ExceptionInfo e")
	c.indent++
	c.writeln("(cond")
	c.indent++
	c.writeln("(= (.getMessage e) \"continue\") :next")
	c.writeln("(= (.getMessage e) \"break\") :break")
	c.writeln(":else (throw e))")
	c.indent--
	c.writeln(")")
	c.indent--
	c.writeln(")]")
	c.indent--
	c.writeln("(cond")
	c.indent++
	c.writeln("(= r :break) nil")
	c.writeln(fmt.Sprintf(":else (recur (next %s))", seqVar))
	c.indent--
	c.writeln(")")
	c.indent--
	c.writeln(")")
	c.indent--
	c.writeln(")")
	c.indent--
	c.writeln(")")
	c.indent--
	c.writeln(")")
	return nil
}

func (c *Compiler) compileWhile(st *parser.WhileStmt) error {
	cond, err := c.compileExpr(st.Cond)
	if err != nil {
		return err
	}
	c.writeln("(loop []")
	c.indent++
	c.writeln("(when " + cond)
	c.indent++
	c.writeln("(let [r (try")
	c.indent++
	for _, s := range st.Body {
		if err := c.compileStmt(s); err != nil {
			return err
		}
	}
	c.writeln(":next")
	c.indent--
	c.writeln("(catch clojure.lang.ExceptionInfo e")
	c.indent++
	c.writeln("(cond")
	c.indent++
	c.writeln("(= (.getMessage e) \"continue\") :next")
	c.writeln("(= (.getMessage e) \"break\") :break")
	c.writeln(":else (throw e))")
	c.indent--
	c.writeln(")")
	c.indent--
	c.writeln(")]")
	c.indent--
	c.writeln("(cond")
	c.indent++
	c.writeln("(= r :break) nil")
	c.writeln("(= r :next) (recur)")
	c.indent--
	c.writeln(")")
	c.indent--
	c.writeln(")")
	c.indent--
	c.writeln(")")
	c.indent--
	c.writeln(")")
	return nil
}

func (c *Compiler) compileIf(st *parser.IfStmt) error {
	cond, err := c.compileExpr(st.Cond)
	if err != nil {
		return err
	}

	// No else or else-if branches -> use when
	if st.ElseIf == nil && len(st.Else) == 0 {
		c.writeln("(when " + cond)
		c.indent++
		for _, s := range st.Then {
			if err := c.compileStmt(s); err != nil {
				return err
			}
		}
		c.indent--
		c.writeln(")")
		return nil
	}

	// if with else or else-if
	c.writeln("(if " + cond)
	c.indent++
	c.writeln("(do")
	c.indent++
	for _, s := range st.Then {
		if err := c.compileStmt(s); err != nil {
			return err
		}
	}
	c.indent--
	c.writeln(")")
	c.indent--
	c.writeIndent()
	c.buf.WriteByte('\n')
	if st.ElseIf != nil {
		if err := c.compileIf(st.ElseIf); err != nil {
			return err
		}
	} else {
		c.writeln("(do")
		c.indent++
		for _, s := range st.Else {
			if err := c.compileStmt(s); err != nil {
				return err
			}
		}
		c.indent--
		c.writeln(")")
	}
	c.writeln(")")
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
	operands := []string{left}
	typesList := []types.Type{c.inferUnaryType(b.Left)}
	ops := []string{}
	for _, part := range b.Right {
		r, err := c.compilePostfix(part.Right)
		if err != nil {
			return "", err
		}
		operands = append(operands, r)
		typesList = append(typesList, c.inferPostfixType(part.Right))
		ops = append(ops, part.Op)
	}

	levels := [][]string{
		{"*", "/", "%"},
		{"+", "-"},
		{"<", "<=", ">", ">="},
		{"==", "!="},
		{"&&"},
		{"||"},
	}

	for _, lvl := range levels {
		for i := 0; i < len(ops); {
			if contains(lvl, ops[i]) {
				expr, typ := c.compileBinaryOp(operands[i], typesList[i], ops[i], operands[i+1], typesList[i+1])
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
		return "", fmt.Errorf("unexpected binary expr")
	}
	return operands[0], nil
}

func (c *Compiler) compileBinaryOp(left string, leftType types.Type, op string, right string, rightType types.Type) (string, types.Type) {
	opName := op
	switch opName {
	case "%":
		opName = "mod"
	case "/":
		if isInt(leftType) && isInt(rightType) {
			opName = "quot"
		}
	case "&&":
		opName = "and"
	case "||":
		opName = "or"
	case "==":
		opName = "="
	case "!=":
		opName = "not="
	}

	switch op {
	case "+":
		if isList(leftType) && isList(rightType) {
			return fmt.Sprintf("(vec (concat %s %s))", left, right), leftType
		}
		if isString(leftType) || isString(rightType) {
			return fmt.Sprintf("(str %s %s)", left, right), types.StringType{}
		}
		return fmt.Sprintf("(+ %s %s)", left, right), leftType
	case "-", "*":
		return fmt.Sprintf("(%s %s %s)", opName, left, right), leftType
	case "/":
		return fmt.Sprintf("(%s %s %s)", opName, left, right), leftType
	case "%":
		return fmt.Sprintf("(%s %s %s)", opName, left, right), leftType
	case "==", "!=", "<", "<=", ">", ">=":
		return fmt.Sprintf("(%s %s %s)", opName, left, right), types.BoolType{}
	case "&&", "||":
		return fmt.Sprintf("(%s %s %s)", opName, left, right), types.BoolType{}
	default:
		return fmt.Sprintf("(%s %s %s)", opName, left, right), types.AnyType{}
	}
}

func (c *Compiler) compileUnary(u *parser.Unary) (string, error) {
	expr, err := c.compilePostfix(u.Value)
	if err != nil {
		return "", err
	}
	for i := len(u.Ops) - 1; i >= 0; i-- {
		op := u.Ops[i]
		name := op
		if name == "!" {
			name = "not"
		}
		expr = fmt.Sprintf("(%s %s)", name, expr)
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
			start, err := c.compileExpr(op.Index.Start)
			if err != nil {
				return "", err
			}
			if op.Index.Colon != nil {
				end, err := c.compileExpr(op.Index.End)
				if err != nil {
					return "", err
				}
				expr = fmt.Sprintf("(subs %s %s %s)", expr, start, end)
			} else {
				expr = fmt.Sprintf("(nth %s %s)", expr, start)
			}
			continue
		}
		if op.Call != nil {
			args := []string{}
			for _, a := range op.Call.Args {
				v, err := c.compileExpr(a)
				if err != nil {
					return "", err
				}
				args = append(args, v)
			}
			name := expr
			if name == "print" {
				expr = fmt.Sprintf("(println %s)", strings.Join(args, " "))
			} else if name == "len" {
				expr = fmt.Sprintf("(count %s)", args[0])
			} else {
				expr = fmt.Sprintf("(%s %s)", name, strings.Join(args, " "))
			}
			continue
		}
		if op.Cast != nil {
			if op.Cast.Type != nil && op.Cast.Type.Simple != nil {
				switch *op.Cast.Type.Simple {
				case "float":
					expr = fmt.Sprintf("(double %s)", expr)
				case "int":
					expr = fmt.Sprintf("(int %s)", expr)
				default:
					// ignore other casts as they have no runtime effect
				}
			}
			continue
		}
	}
	return expr, nil
}

func (c *Compiler) compilePrimary(p *parser.Primary) (string, error) {
	switch {
	case p.Lit != nil:
		if p.Lit.Int != nil {
			return strconv.Itoa(*p.Lit.Int), nil
		}
		if p.Lit.Float != nil {
			s := strconv.FormatFloat(*p.Lit.Float, 'f', -1, 64)
			if !strings.Contains(s, ".") {
				s += ".0"
			}
			return s, nil
		}
		if p.Lit.Bool != nil {
			if *p.Lit.Bool {
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
		return "[" + strings.Join(elems, " ") + "]", nil
	case p.Selector != nil:
		if len(p.Selector.Tail) == 0 {
			return sanitizeName(p.Selector.Root), nil
		}
	case p.Group != nil:
		expr, err := c.compileExpr(p.Group)
		if err != nil {
			return "", err
		}
		return expr, nil
	case p.Call != nil:
		args := []string{}
		for _, a := range p.Call.Args {
			v, err := c.compileExpr(a)
			if err != nil {
				return "", err
			}
			args = append(args, v)
		}
		name := sanitizeName(p.Call.Func)
		if name == "print" {
			return "(println " + strings.Join(args, " ") + ")", nil
		}
		if name == "len" && len(args) == 1 {
			return "(count " + args[0] + ")", nil
		}
		return "(" + name + " " + strings.Join(args, " ") + ")", nil
	}
	return "", fmt.Errorf("unsupported expression")
}

func (c *Compiler) isStringExpr(expr string) bool {
	expr = strings.TrimSpace(expr)
	if len(expr) >= 2 && strings.HasPrefix(expr, "\"") && strings.HasSuffix(expr, "\"") {
		return true
	}
	if strings.HasPrefix(expr, "(nth ") {
		v := strings.Fields(expr[5:])
		if len(v) > 0 {
			if t, err := c.env.GetVar(v[0]); err == nil {
				if _, ok := t.(types.StringType); ok {
					return true
				}
			}
		}
	}
	if strings.HasPrefix(expr, "(subs ") {
		v := strings.Fields(expr[6:])
		if len(v) > 0 {
			if t, err := c.env.GetVar(v[0]); err == nil {
				if _, ok := t.(types.StringType); ok {
					return true
				}
			}
		}
	}
	if strings.HasPrefix(expr, "(str ") {
		return true
	}
	if t, err := c.env.GetVar(expr); err == nil {
		if _, ok := t.(types.StringType); ok {
			return true
		}
	}
	return false
}

func (c *Compiler) writeln(s string) {
	c.writeIndent()
	c.buf.WriteString(s)
	c.buf.WriteByte('\n')
}

func (c *Compiler) writeIndent() {
	for i := 0; i < c.indent; i++ {
		c.buf.WriteString("  ")
	}
}

func (c *Compiler) newTemp() string {
	name := fmt.Sprintf("_tmp%d", c.tempVarCount)
	c.tempVarCount++
	return name
}
