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
	buf             bytes.Buffer
	indent          int
	env             *types.Env
	mainStmts       []*parser.Statement
	tempVarCount    int
	needIndexString bool
	needIndexList   bool
	needInput       bool
	needCount       bool
	needAvg         bool
	needGroup       bool
	needGroupBy     bool
	needLoad        bool
	needSave        bool
}

// New creates a new Clojure compiler instance.
func New(env *types.Env) *Compiler {
	return &Compiler{env: env, tempVarCount: 0}
}

// Compile generates Clojure code for prog.
func (c *Compiler) Compile(prog *parser.Program) ([]byte, error) {
	c.buf.Reset()
	c.indent = 0
	c.mainStmts = nil
	c.tempVarCount = 0
	c.needIndexString = false
	c.needIndexList = false
	c.needInput = false
	c.needCount = false
	c.needAvg = false
	c.needGroup = false
	c.needGroupBy = false
	c.needLoad = false
	c.needSave = false

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
	c.writeln("(defn -main []")
	c.indent++
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
	c.indent--
	c.writeln(")")
	c.writeln("")
	c.writeln("(-main)")

	finalBuf := bytes.Buffer{}
	finalBuf.WriteString("(ns main)\n\n")
	c.writeRuntime(&finalBuf)
	finalBuf.Write(c.buf.Bytes())

	return finalBuf.Bytes(), nil
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
	c.writeln(fmt.Sprintf("(let [%s (clojure.core/first %s)]", name, seqVar))
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
	t := c.inferPrimaryType(p.Target)
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
				if _, ok := t.(types.StringType); ok {
					expr = fmt.Sprintf("(subs %s %s %s)", expr, start, end)
					t = types.StringType{}
				} else if lt, ok := t.(types.ListType); ok {
					expr = fmt.Sprintf("(subvec %s %s %s)", expr, start, end)
					t = lt
				} else {
					expr = fmt.Sprintf("(subs %s %s %s)", expr, start, end)
					t = types.AnyType{}
				}
			} else {
				if _, ok := t.(types.StringType); ok {
					c.needIndexString = true
					expr = fmt.Sprintf("(_indexString %s %s)", expr, start)
					t = types.StringType{}
				} else if lt, ok := t.(types.ListType); ok {
					c.needIndexList = true
					expr = fmt.Sprintf("(_indexList %s %s)", expr, start)
					t = lt.Elem
				} else if mt, ok := t.(types.MapType); ok {
					expr = fmt.Sprintf("(get %s %s)", expr, start)
					t = mt.Value
				} else {
					c.needIndexList = true
					expr = fmt.Sprintf("(_indexList %s %s)", expr, start)
					t = types.AnyType{}
				}
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
			switch name {
			case "print":
				expr = fmt.Sprintf("(println %s)", strings.Join(args, " "))
			case "len":
				expr = fmt.Sprintf("(count %s)", args[0])
			case "count":
				c.needCount = true
				expr = fmt.Sprintf("(_count %s)", args[0])
			case "avg":
				c.needAvg = true
				expr = fmt.Sprintf("(_avg %s)", args[0])
			case "input":
				c.needInput = true
				expr = "(_input)"
			case "str":
				expr = fmt.Sprintf("(str %s)", strings.Join(args, " "))
			default:
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
	case p.Map != nil:
		parts := make([]string, 0, len(p.Map.Items)*2)
		for _, it := range p.Map.Items {
			k, err := c.compileExpr(it.Key)
			if err != nil {
				return "", err
			}
			if name, ok := isIdentExpr(it.Key); ok {
				k = ":" + sanitizeName(name)
			}
			v, err := c.compileExpr(it.Value)
			if err != nil {
				return "", err
			}
			parts = append(parts, k, v)
		}
		return "{" + strings.Join(parts, " ") + "}", nil
	case p.Struct != nil:
		parts := make([]string, 0, len(p.Struct.Fields)+1)
		parts = append(parts, fmt.Sprintf(":__name \"%s\"", p.Struct.Name))
		for _, f := range p.Struct.Fields {
			v, err := c.compileExpr(f.Value)
			if err != nil {
				return "", err
			}
			parts = append(parts, fmt.Sprintf(":%s %s", sanitizeName(f.Name), v))
		}
		return "{" + strings.Join(parts, " ") + "}", nil
	case p.Query != nil:
		expr, err := c.compileQuery(p.Query)
		if err != nil {
			return "", err
		}
		return expr, nil
	case p.Match != nil:
		expr, err := c.compileMatch(p.Match)
		if err != nil {
			return "", err
		}
		return expr, nil
	case p.Selector != nil:
		expr := sanitizeName(p.Selector.Root)
		for _, s := range p.Selector.Tail {
			expr = fmt.Sprintf("(:%s %s)", sanitizeName(s), expr)
		}
		return expr, nil
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
		switch name {
		case "print":
			return "(println " + strings.Join(args, " ") + ")", nil
		case "len":
			if len(args) == 1 {
				return "(count " + args[0] + ")", nil
			}
		case "count":
			if len(args) == 1 {
				c.needCount = true
				return "(_count " + args[0] + ")", nil
			}
		case "avg":
			if len(args) == 1 {
				c.needAvg = true
				return "(_avg " + args[0] + ")", nil
			}
		case "input":
			if len(args) == 0 {
				c.needInput = true
				return "(_input)", nil
			}
		case "str":
			return "(str " + strings.Join(args, " ") + ")", nil
		}
		return "(" + name + " " + strings.Join(args, " ") + ")", nil
	}
	return "", fmt.Errorf("unsupported expression")
}

func (c *Compiler) compileQuery(q *parser.QueryExpr) (string, error) {
	if len(q.Joins) != 0 {
		return "", fmt.Errorf("unsupported query expression")
	}
	src, err := c.compileExpr(q.Source)
	if err != nil {
		return "", err
	}
	origEnv := c.env
	child := types.NewEnv(c.env)
	child.SetVar(q.Var, types.AnyType{}, true)
	for _, f := range q.Froms {
		child.SetVar(f.Var, types.AnyType{}, true)
	}
	c.env = child

	if q.Group != nil && len(q.Froms) == 0 && q.Where == nil && q.Sort == nil && q.Skip == nil && q.Take == nil {
		keyExpr, err := c.compileExpr(q.Group.Expr)
		if err != nil {
			c.env = origEnv
			return "", err
		}
		genv := types.NewEnv(child)
		genv.SetVar(q.Group.Name, types.GroupType{Elem: types.AnyType{}}, true)
		c.env = genv
		valExpr, err := c.compileExpr(q.Select)
		if err != nil {
			c.env = origEnv
			return "", err
		}
		c.env = origEnv
		c.needGroup = true
		c.needGroupBy = true
		expr := fmt.Sprintf("(map (fn [%s] %s) (_group_by %s (fn [%s] %s)))",
			sanitizeName(q.Group.Name), valExpr, src, sanitizeName(q.Var), keyExpr)
		return expr, nil
	}
	fromSrcs := make([]string, len(q.Froms))
	for i, f := range q.Froms {
		fs, err := c.compileExpr(f.Src)
		if err != nil {
			return "", err
		}
		fromSrcs[i] = fs
	}
	var whereExpr string
	if q.Where != nil {
		whereExpr, err = c.compileExpr(q.Where)
		if err != nil {
			return "", err
		}
	}
	var sortExpr, skipExpr, takeExpr string
	if q.Sort != nil {
		sortExpr, err = c.compileExpr(q.Sort)
		if err != nil {
			return "", err
		}
	}
	if q.Skip != nil {
		skipExpr, err = c.compileExpr(q.Skip)
		if err != nil {
			return "", err
		}
	}
	if q.Take != nil {
		takeExpr, err = c.compileExpr(q.Take)
		if err != nil {
			return "", err
		}
	}
	selExpr, err := c.compileExpr(q.Select)
	if err != nil {
		return "", err
	}
	v := sanitizeName(q.Var)
	var b strings.Builder
	b.WriteString("(vec (->>")
	b.WriteString(" (for [" + v + " " + src)
	for i, f := range q.Froms {
		b.WriteString(" " + sanitizeName(f.Var) + " " + fromSrcs[i])
	}
	if whereExpr != "" {
		b.WriteString(" :when " + whereExpr)
	}
	b.WriteString("] " + selExpr + ")")
	if sortExpr != "" {
		b.WriteString(" (sort-by (fn [" + v + "] " + sortExpr + "))")
	}
	if skipExpr != "" {
		b.WriteString(" (drop " + skipExpr + ")")
	}
	if takeExpr != "" {
		b.WriteString(" (take " + takeExpr + ")")
	}
	b.WriteString("))")
	c.env = origEnv
	return b.String(), nil
}

func (c *Compiler) compileMatch(m *parser.MatchExpr) (string, error) {
	target, err := c.compileExpr(m.Target)
	if err != nil {
		return "", err
	}
	var b strings.Builder
	b.WriteString("(let [t " + target + "]\n")
	b.WriteString("  (cond\n")
	hasDefault := false
	for _, cs := range m.Cases {
		if isUnderscoreExpr(cs.Pattern) {
			res, err := c.compileExpr(cs.Result)
			if err != nil {
				return "", err
			}
			b.WriteString("    :else " + res + "\n")
			hasDefault = true
			continue
		}
		pat, err := c.compileExpr(cs.Pattern)
		if err != nil {
			return "", err
		}
		res, err := c.compileExpr(cs.Result)
		if err != nil {
			return "", err
		}
		b.WriteString("    (= t " + pat + ") " + res + "\n")
	}
	if !hasDefault {
		b.WriteString("    :else nil\n")
	}
	b.WriteString("  ))")
	return b.String(), nil
}

func (c *Compiler) compileLoadExpr(l *parser.LoadExpr) (string, error) {
	path := "\"\""
	if l.Path != nil {
		path = strconv.Quote(*l.Path)
	}
	opts := "nil"
	if l.With != nil {
		v, err := c.compileExpr(l.With)
		if err != nil {
			return "", err
		}
		opts = v
	}
	c.needLoad = true
	return fmt.Sprintf("(_load %s %s)", path, opts), nil
}

func (c *Compiler) compileSaveExpr(s *parser.SaveExpr) (string, error) {
	src, err := c.compileExpr(s.Src)
	if err != nil {
		return "", err
	}
	path := "\"\""
	if s.Path != nil {
		path = strconv.Quote(*s.Path)
	}
	opts := "nil"
	if s.With != nil {
		v, err := c.compileExpr(s.With)
		if err != nil {
			return "", err
		}
		opts = v
	}
	c.needSave = true
	return fmt.Sprintf("(_save %s %s %s)", src, path, opts), nil
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

func (c *Compiler) writeRuntime(buf *bytes.Buffer) {
	if c.needIndexString {
		buf.WriteString(`(defn _indexString [s i]
  (let [r (vec (seq s))
        i (if (neg? i) (+ i (count r)) i)]
    (if (or (< i 0) (>= i (count r)))
      (throw (ex-info "index out of range" {}))
      (str (nth r i)))))

`)
	}
	if c.needIndexList {
		buf.WriteString(`(defn _indexList [xs i]
  (let [idx (if (neg? i) (+ i (count xs)) i)]
    (if (or (< idx 0) (>= idx (count xs)))
      (throw (ex-info "index out of range" {}))
      (nth xs idx))))

`)
	}
	if c.needInput {
		buf.WriteString(`(defn _input []
  (clojure.string/trim (read-line)))

`)
	}
	if c.needCount {
		buf.WriteString(`(defn _count [v]
  (cond
    (sequential? v) (count v)
    (and (map? v) (contains? v :Items)) (count (:Items v))
    :else (throw (ex-info "count() expects list or group" {}))))

`)
	}
	if c.needAvg {
		buf.WriteString(`(defn _avg [v]
  (let [lst (cond
              (and (map? v) (contains? v :Items)) (:Items v)
              (sequential? v) v
              :else (throw (ex-info "avg() expects list or group" {})))]
    (if (empty? lst)
      0
      (/ (reduce + lst) (double (count lst)))))

`)
	}
	if c.needGroup {
		buf.WriteString(`(defrecord _Group [key Items])

`)
	}
	if c.needGroupBy {
		buf.WriteString(`(defn _group_by [src keyfn]
  (let [groups (atom {})
        order (atom [])]
    (doseq [it src]
      (let [k (keyfn it)
            ks (str k)]
        (when-not (contains? @groups ks)
          (swap! groups assoc ks (_Group. k []))
          (swap! order conj ks))
        (swap! groups update ks (fn [g] (assoc g :Items (conj (:Items g) it)))))
    )
    (map (fn [k] (@groups k)) @order)))

`)
	}
	if c.needLoad {
		buf.WriteString(`(defn _parse_csv [text header delim]
  (let [lines (->> (clojure.string/split-lines text)
                   (remove clojure.string/blank?))
        headers (if header
                    (clojure.string/split (first lines) (re-pattern (str delim)))
                    (map #(str "c" %) (range (count (clojure.string/split (first lines) (re-pattern (str delim)))))))]
    (mapv (fn [line]
            (let [parts (clojure.string/split line (re-pattern (str delim)))]
              (zipmap headers parts)))
          (drop (if header 1 0) lines))) )

`)
		buf.WriteString(`(defn _load [path opts]
  (let [fmt (get opts :format "csv")
        header (get opts :header true)
        delim (first (or (get opts :delimiter ",") ","))
        text (if (or (nil? path) (= path "") (= path "-"))
               (slurp *in*)
               (slurp path))]
    (cond
      (= fmt "csv") (_parse_csv text header delim)
      :else [])) )

`)
	}
	if c.needSave {
		buf.WriteString(`(defn _save [rows path opts]
  (let [fmt (get opts :format "csv")
        header (get opts :header false)
        delim (first (or (get opts :delimiter ",") ","))
        headers (if (seq rows) (sort (keys (first rows))) [])
        lines (concat
                (when header [(clojure.string/join delim headers)])
                (map (fn [r]
                       (clojure.string/join delim (map #(str (get r % "")) headers)))
                     rows))
        out (str (clojure.string/join "\n" lines) "\n")]
    (if (or (nil? path) (= path "") (= path "-"))
      (print out)
      (spit path out))) )

`)
	}
}
