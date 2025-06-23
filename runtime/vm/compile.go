package vm

import (
	"fmt"

	"github.com/alecthomas/participle/v2/lexer"

	"mochi/parser"
)

// Compiled represents a program ready to run on the VM.
type Compiled struct {
	Ops     []Instruction
	Consts  []Value
	NumRegs int
}

type compiler struct {
	consts   []Value
	ops      []Instruction
	regs     map[string]int
	builtins map[string]int
	nextReg  int
	last     int
}

// Compile converts a Mochi program to VM instructions.
func Compile(prog *parser.Program, builtins map[string]int) (*Compiled, error) {
	c := &compiler{regs: map[string]int{}, builtins: builtins}
	for _, stmt := range prog.Statements {
		if err := c.stmt(stmt); err != nil {
			return nil, err
		}
	}
	// Halt with last register if any instructions emitted.
	haltReg := 0
	if c.nextReg > 0 {
		haltReg = c.last
	}
	c.ops = append(c.ops, Instruction{Op: OpHalt, A: haltReg})
	return &Compiled{Ops: c.ops, Consts: c.consts, NumRegs: c.nextReg}, nil
}

func (c *compiler) newReg() int {
	r := c.nextReg
	c.nextReg++
	return r
}

func (c *compiler) constIndex(v Value) int {
	for i, cv := range c.consts {
		if cv == v {
			return i
		}
	}
	c.consts = append(c.consts, v)
	return len(c.consts) - 1
}

func (c *compiler) stmt(s *parser.Statement) error {
	switch {
	case s.Let != nil:
		r, err := c.expr(s.Let.Value)
		if err != nil {
			return err
		}
		c.regs[s.Let.Name] = r
		c.last = r
	case s.Var != nil:
		r, err := c.expr(s.Var.Value)
		if err != nil {
			return err
		}
		c.regs[s.Var.Name] = r
		c.last = r
	case s.Assign != nil:
		r, err := c.expr(s.Assign.Value)
		if err != nil {
			return err
		}
		dst, ok := c.regs[s.Assign.Name]
		if !ok {
			return fmt.Errorf("undefined variable %s", s.Assign.Name)
		}
		c.ops = append(c.ops, Instruction{Op: OpMove, A: dst, B: r})
		c.last = dst
	case s.Expr != nil:
		r, err := c.expr(s.Expr.Expr)
		if err != nil {
			return err
		}
		c.last = r
	}
	return nil
}

func (c *compiler) expr(e *parser.Expr) (int, error) {
	return c.binary(e.Binary)
}

func (c *compiler) binary(b *parser.BinaryExpr) (int, error) {
	left, err := c.unary(b.Left)
	if err != nil {
		return 0, err
	}

	type token struct {
		pos lexer.Position
		op  string
	}

	operands := []int{left}
	operators := []token{}

	for _, part := range b.Right {
		operators = append(operators, token{part.Pos, part.Op})
		right, err := c.postfix(part.Right)
		if err != nil {
			return 0, err
		}
		operands = append(operands, right)
	}

	precedence := [][]string{
		{"*", "/", "%"},
		{"+", "-"},
		{"<", "<=", ">", ">="},
		{"==", "!=", "in"},
		{"&&"},
		{"||"},
		{"union", "union_all", "except", "intersect"},
	}

	for _, level := range precedence {
		for i := 0; i < len(operators); {
			if contains(level, operators[i].op) {
				l := operands[i]
				r := operands[i+1]
				dst := c.newReg()
				switch operators[i].op {
				case "+":
					c.ops = append(c.ops, Instruction{Op: OpAdd, A: dst, B: l, C: r})
				case "-":
					c.ops = append(c.ops, Instruction{Op: OpSub, A: dst, B: l, C: r})
				case "*":
					c.ops = append(c.ops, Instruction{Op: OpMul, A: dst, B: l, C: r})
				case "/":
					c.ops = append(c.ops, Instruction{Op: OpDiv, A: dst, B: l, C: r})
				case "==":
					c.ops = append(c.ops, Instruction{Op: OpEqual, A: dst, B: l, C: r})
				case "<":
					c.ops = append(c.ops, Instruction{Op: OpLess, A: dst, B: l, C: r})
				case ">":
					c.ops = append(c.ops, Instruction{Op: OpGreater, A: dst, B: l, C: r})
				default:
					return 0, fmt.Errorf("unsupported op %s", operators[i].op)
				}
				operands[i] = dst
				operands = append(operands[:i+1], operands[i+2:]...)
				operators = append(operators[:i], operators[i+1:]...)
			} else {
				i++
			}
		}
	}

	if len(operands) != 1 {
		return 0, fmt.Errorf("unexpected expression state")
	}
	return operands[0], nil
}

func (c *compiler) unary(u *parser.Unary) (int, error) {
	r, err := c.postfix(u.Value)
	if err != nil {
		return 0, err
	}
	for i := len(u.Ops) - 1; i >= 0; i-- {
		op := u.Ops[i]
		dst := c.newReg()
		switch op {
		case "-":
			c.ops = append(c.ops, Instruction{Op: OpNeg, A: dst, B: r})
		default:
			return 0, fmt.Errorf("unsupported unary %s", op)
		}
		r = dst
	}
	return r, nil
}

func (c *compiler) postfix(p *parser.PostfixExpr) (int, error) {
	r, err := c.primary(p.Target)
	if err != nil {
		return 0, err
	}
	for _, op := range p.Ops {
		if op.Call != nil {
			r, err = c.callExpr(op.Call, r)
			if err != nil {
				return 0, err
			}
		} else {
			return 0, fmt.Errorf("unsupported postfix")
		}
	}
	return r, nil
}

func (c *compiler) callExpr(call *parser.CallOp, recv int) (int, error) {
	// not used in our subset
	return 0, fmt.Errorf("unsupported call")
}

func (c *compiler) primary(p *parser.Primary) (int, error) {
	switch {
	case p.Lit != nil:
		if p.Lit.Int != nil {
			idx := c.constIndex(*p.Lit.Int)
			r := c.newReg()
			c.ops = append(c.ops, Instruction{Op: OpLoadConst, A: r, B: idx})
			return r, nil
		}
		if p.Lit.Str != nil {
			idx := c.constIndex(*p.Lit.Str)
			r := c.newReg()
			c.ops = append(c.ops, Instruction{Op: OpLoadConst, A: r, B: idx})
			return r, nil
		}
		if p.Lit.Bool != nil {
			b := bool(*p.Lit.Bool)
			idx := c.constIndex(b)
			r := c.newReg()
			c.ops = append(c.ops, Instruction{Op: OpLoadConst, A: r, B: idx})
			return r, nil
		}
	case p.Call != nil:
		return c.compileCall(p.Call)
	case p.Selector != nil && len(p.Selector.Tail) == 0:
		reg, ok := c.regs[p.Selector.Root]
		if !ok {
			return 0, fmt.Errorf("undefined variable %s", p.Selector.Root)
		}
		r := c.newReg()
		c.ops = append(c.ops, Instruction{Op: OpMove, A: r, B: reg})
		return r, nil
	case p.Group != nil:
		return c.expr(p.Group)
	}
	return 0, fmt.Errorf("unsupported expression")
}

func (c *compiler) compileCall(call *parser.CallExpr) (int, error) {
	idx, ok := c.builtins[call.Func]
	if !ok {
		return 0, fmt.Errorf("unknown builtin %s", call.Func)
	}
	if len(call.Args) != 1 {
		return 0, fmt.Errorf("builtin %s expects 1 arg", call.Func)
	}
	arg, err := c.expr(call.Args[0])
	if err != nil {
		return 0, err
	}
	dst := c.newReg()
	c.ops = append(c.ops, Instruction{Op: OpCall, A: dst, B: idx, C: arg})
	return dst, nil
}

func contains(ops []string, op string) bool {
	for _, o := range ops {
		if o == op {
			return true
		}
	}
	return false
}
