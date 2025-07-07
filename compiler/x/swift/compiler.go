//go:build slow

package swift

import (
	"fmt"
	"strings"

	"mochi/parser"
	"mochi/types"
)

// Compile parses Mochi source and generates Swift code. Only a subset of
// Mochi is supported. Unsupported features result in an error.
func Compile(src string) ([]byte, error) {
	prog, err := parser.ParseString(src)
	if err != nil {
		return nil, err
	}
	c := &Compiler{}
	return c.Compile(prog)
}

// Compiler converts a subset of Mochi into Swift source code.
type Compiler struct {
	compiler
}

// New creates a new Compiler instance.
func New(env *types.Env) *Compiler { return &Compiler{} }

// Compile generates Swift code for the given program.
func (c *Compiler) Compile(p *parser.Program) ([]byte, error) {
	c.buf.Reset()
	c.indent = 0
	if err := c.program(p); err != nil {
		return nil, err
	}
	return []byte(c.buf.String()), nil
}

type compiler struct {
	buf    strings.Builder
	indent int
}

func (c *compiler) program(p *parser.Program) error {
	for _, s := range p.Statements {
		if err := c.stmt(s); err != nil {
			return err
		}
	}
	return nil
}

func (c *compiler) stmt(s *parser.Statement) error {
	switch {
	case s.Let != nil:
		return c.letStmt(s.Let)
	case s.Var != nil:
		return c.varStmt(s.Var)
	case s.Assign != nil:
		return c.assignStmt(s.Assign)
	case s.Fun != nil:
		return c.funStmt(s.Fun)
	case s.Return != nil:
		return c.returnStmt(s.Return)
	case s.If != nil:
		return c.ifStmt(s.If)
	case s.While != nil:
		return c.whileStmt(s.While)
	case s.Expr != nil:
		expr, err := c.expr(s.Expr.Expr)
		if err != nil {
			return err
		}
		c.writeln(expr)
		return nil
	default:
		return fmt.Errorf("unsupported statement at line %d", s.Pos.Line)
	}
}

func (c *compiler) letStmt(l *parser.LetStmt) error {
	typ, err := c.typeRef(l.Type)
	if err != nil {
		return err
	}
	if l.Value != nil {
		val, err := c.expr(l.Value)
		if err != nil {
			return err
		}
		if typ != "" {
			c.writeln(fmt.Sprintf("let %s: %s = %s", l.Name, typ, val))
		} else {
			c.writeln(fmt.Sprintf("let %s = %s", l.Name, val))
		}
		return nil
	}
	if typ == "" {
		return fmt.Errorf("let without value or type at line %d", l.Pos.Line)
	}
	c.writeln(fmt.Sprintf("let %s: %s = %s", l.Name, typ, defaultValue(typ)))
	return nil
}

func (c *compiler) varStmt(v *parser.VarStmt) error {
	typ, err := c.typeRef(v.Type)
	if err != nil {
		return err
	}
	if v.Value != nil {
		val, err := c.expr(v.Value)
		if err != nil {
			return err
		}
		if typ != "" {
			c.writeln(fmt.Sprintf("var %s: %s = %s", v.Name, typ, val))
		} else {
			c.writeln(fmt.Sprintf("var %s = %s", v.Name, val))
		}
		return nil
	}
	if typ == "" {
		return fmt.Errorf("var without value or type at line %d", v.Pos.Line)
	}
	c.writeln(fmt.Sprintf("var %s: %s = %s", v.Name, typ, defaultValue(typ)))
	return nil
}

func (c *compiler) assignStmt(a *parser.AssignStmt) error {
	if len(a.Index) > 0 || len(a.Field) > 0 {
		return fmt.Errorf("complex assignment not supported")
	}
	val, err := c.expr(a.Value)
	if err != nil {
		return err
	}
	c.writeln(fmt.Sprintf("%s = %s", a.Name, val))
	return nil
}

func (c *compiler) funStmt(f *parser.FunStmt) error {
	c.writeIndent()
	c.buf.WriteString("func ")
	c.buf.WriteString(f.Name)
	c.buf.WriteString("(")
	for i, p := range f.Params {
		if i > 0 {
			c.buf.WriteString(", ")
		}
		typ, err := c.typeRef(p.Type)
		if err != nil {
			return err
		}
		if typ == "" {
			return fmt.Errorf("parameter %s missing type", p.Name)
		}
		c.buf.WriteString(p.Name)
		c.buf.WriteString(": ")
		c.buf.WriteString(typ)
	}
	c.buf.WriteString(")")
	if f.Return != nil {
		rtyp, err := c.typeRef(f.Return)
		if err != nil {
			return err
		}
		if rtyp != "" {
			c.buf.WriteString(" -> ")
			c.buf.WriteString(rtyp)
		}
	}
	c.buf.WriteString(" {\n")
	c.indent++
	for _, st := range f.Body {
		if err := c.stmt(st); err != nil {
			return err
		}
	}
	c.indent--
	c.writeln("}")
	return nil
}

func (c *compiler) returnStmt(r *parser.ReturnStmt) error {
	val, err := c.expr(r.Value)
	if err != nil {
		return err
	}
	c.writeln("return " + val)
	return nil
}

func (c *compiler) ifStmt(i *parser.IfStmt) error {
	cond, err := c.expr(i.Cond)
	if err != nil {
		return err
	}
	c.writeln("if " + cond + " {")
	c.indent++
	for _, st := range i.Then {
		if err := c.stmt(st); err != nil {
			return err
		}
	}
	c.indent--
	if i.ElseIf != nil {
		c.writeIndent()
		c.buf.WriteString("else ")
		if err := c.ifStmt(i.ElseIf); err != nil {
			return err
		}
		return nil
	}
	if len(i.Else) > 0 {
		c.writeln("else {")
		c.indent++
		for _, st := range i.Else {
			if err := c.stmt(st); err != nil {
				return err
			}
		}
		c.indent--
		c.writeln("}")
		return nil
	}
	c.writeln("}")
	return nil
}

func (c *compiler) whileStmt(w *parser.WhileStmt) error {
	cond, err := c.expr(w.Cond)
	if err != nil {
		return err
	}
	c.writeln("while " + cond + " {")
	c.indent++
	for _, st := range w.Body {
		if err := c.stmt(st); err != nil {
			return err
		}
	}
	c.indent--
	c.writeln("}")
	return nil
}

func (c *compiler) expr(e *parser.Expr) (string, error) {
	if e == nil || e.Binary == nil {
		return "", fmt.Errorf("empty expression")
	}
	return c.binary(e.Binary)
}

func (c *compiler) binary(b *parser.BinaryExpr) (string, error) {
	left, err := c.unary(b.Left)
	if err != nil {
		return "", err
	}
	res := left
	for _, op := range b.Right {
		if !supportedOp(op.Op) {
			return "", fmt.Errorf("unsupported operator %s at line %d", op.Op, op.Pos.Line)
		}
		r, err := c.postfix(op.Right)
		if err != nil {
			return "", err
		}
		res = fmt.Sprintf("%s %s %s", res, op.Op, r)
	}
	return res, nil
}

func supportedOp(op string) bool {
	switch op {
	case "+", "-", "*", "/", "%", "<", "<=", ">", ">=", "==", "!=", "&&", "||":
		return true
	}
	return false
}

func (c *compiler) unary(u *parser.Unary) (string, error) {
	val, err := c.postfix(u.Value)
	if err != nil {
		return "", err
	}
	for i := len(u.Ops) - 1; i >= 0; i-- {
		op := u.Ops[i]
		if op != "-" && op != "!" {
			return "", fmt.Errorf("unsupported unary operator %s at line %d", op, u.Pos.Line)
		}
		val = op + val
	}
	return val, nil
}

func (c *compiler) postfix(p *parser.PostfixExpr) (string, error) {
	val, err := c.primary(p.Target)
	if err != nil {
		return "", err
	}
	for _, op := range p.Ops {
		switch {
		case op.Cast != nil:
			typ, err := c.typeRef(op.Cast.Type)
			if err != nil {
				return "", err
			}
			switch typ {
			case "Int", "Double", "Bool", "String":
				val = fmt.Sprintf("%s(%s)", typ, val)
			default:
				return "", fmt.Errorf("unsupported cast to %s", typ)
			}
		default:
			return "", fmt.Errorf("postfix operations not supported")
		}
	}
	return val, nil
}

func (c *compiler) primary(p *parser.Primary) (string, error) {
	switch {
	case p.Lit != nil:
		return literal(p.Lit), nil
	case p.Call != nil:
		return c.callExpr(p.Call)
	case p.Selector != nil:
		return selector(p.Selector), nil
	case p.Group != nil:
		e, err := c.expr(p.Group)
		if err != nil {
			return "", err
		}
		return "(" + e + ")", nil
	default:
		return "", fmt.Errorf("unsupported expression at line %d", p.Pos.Line)
	}
}

func (c *compiler) callExpr(call *parser.CallExpr) (string, error) {
	args := make([]string, len(call.Args))
	for i, a := range call.Args {
		s, err := c.expr(a)
		if err != nil {
			return "", err
		}
		args[i] = s
	}
	joined := strings.Join(args, ", ")
	switch call.Func {
	case "print":
		return fmt.Sprintf("print(%s)", joined), nil
	case "str":
		if len(args) != 1 {
			return "", fmt.Errorf("str expects 1 argument at line %d", call.Pos.Line)
		}
		return fmt.Sprintf("String(%s)", joined), nil
	default:
		return fmt.Sprintf("%s(%s)", call.Func, joined), nil
	}
}

func selector(s *parser.SelectorExpr) string {
	if len(s.Tail) == 0 {
		return s.Root
	}
	return s.Root + "." + strings.Join(s.Tail, ".")
}

func literal(l *parser.Literal) string {
	switch {
	case l.Int != nil:
		return fmt.Sprintf("%d", *l.Int)
	case l.Float != nil:
		return fmt.Sprintf("%g", *l.Float)
	case l.Bool != nil:
		if *l.Bool {
			return "true"
		}
		return "false"
	case l.Str != nil:
		return fmt.Sprintf("%q", *l.Str)
	default:
		return "nil"
	}
}

func (c *compiler) typeRef(t *parser.TypeRef) (string, error) {
	if t == nil {
		return "", nil
	}
	if t.Simple == nil {
		return "", fmt.Errorf("unsupported type reference")
	}
	switch strings.ToLower(*t.Simple) {
	case "int":
		return "Int", nil
	case "string":
		return "String", nil
	case "float":
		return "Double", nil
	case "bool":
		return "Bool", nil
	default:
		return "", fmt.Errorf("unknown type %s", *t.Simple)
	}
}

func defaultValue(typ string) string {
	switch typ {
	case "Int":
		return "0"
	case "Double":
		return "0.0"
	case "Bool":
		return "false"
	case "String":
		return "\"\""
	default:
		return "nil"
	}
}

func (c *compiler) writeln(s string) {
	for i := 0; i < c.indent; i++ {
		c.buf.WriteString("    ")
	}
	c.buf.WriteString(s)
	c.buf.WriteByte('\n')
}

func (c *compiler) writeIndent() {
	for i := 0; i < c.indent; i++ {
		c.buf.WriteString("    ")
	}
}
