package ir

import (
	"fmt"

	"mochi/parser"
	"mochi/runtime/vm"
)

// Lower converts a parsed program into VM instructions.
// It supports a minimal subset of the language: integer literals,
// basic arithmetic, let-bindings, and print statements.
func Lower(prog *parser.Program) ([]vm.Instruction, error) {
	b := builder{}
	if err := b.program(prog); err != nil {
		return nil, err
	}
	b.emit(vm.OpStop)
	return b.ins, nil
}

type builder struct{ ins []vm.Instruction }

func (b *builder) emit(op vm.Opcode) { b.ins = append(b.ins, vm.Instruction{Op: op}) }

func (b *builder) emitConst(v any) { b.ins = append(b.ins, vm.Instruction{Op: vm.OpConst, Value: v}) }

func (b *builder) emitVar(op vm.Opcode, name string) {
	b.ins = append(b.ins, vm.Instruction{Op: op, Str: name})
}

func (b *builder) program(p *parser.Program) error {
	for _, s := range p.Statements {
		if s.Let != nil {
			if err := b.expr(s.Let.Value); err != nil {
				return err
			}
			b.emitVar(vm.OpStoreVar, s.Let.Name)
		} else if s.Expr != nil {
			if err := b.expr(s.Expr.Expr); err != nil {
				return err
			}
			// Expression results are popped via print or return
		}
	}
	return nil
}

func (b *builder) expr(e *parser.Expr) error {
	if e == nil || e.Binary == nil {
		return nil
	}
	if err := b.unary(e.Binary.Left); err != nil {
		return err
	}
	for _, op := range e.Binary.Right {
		if err := b.postfix(op.Right); err != nil {
			return err
		}
		if err := b.binaryOp(op.Op); err != nil {
			return err
		}
	}
	return nil
}

func (b *builder) unary(u *parser.Unary) error {
	if err := b.postfix(u.Value); err != nil {
		return err
	}
	for i := len(u.Ops) - 1; i >= 0; i-- {
		switch u.Ops[i] {
		case "-":
			b.emit(vm.OpNeg)
		default:
			return fmt.Errorf("unary op %s not supported", u.Ops[i])
		}
	}
	return nil
}

func (b *builder) postfix(p *parser.PostfixExpr) error {
	if err := b.primary(p.Target); err != nil {
		return err
	}
	if len(p.Ops) != 0 {
		return fmt.Errorf("postfix operations not supported")
	}
	return nil
}

func (b *builder) primary(p *parser.Primary) error {
	switch {
	case p == nil:
		return nil
	case p.Lit != nil:
		if p.Lit.Int != nil {
			b.emitConst(*p.Lit.Int)
			return nil
		}
		return fmt.Errorf("literal type not supported")
	case p.Selector != nil && len(p.Selector.Tail) == 0:
		b.emitVar(vm.OpLoadVar, p.Selector.Root)
		return nil
	case p.Group != nil:
		return b.expr(p.Group)
	case p.Call != nil && p.Call.Func == "print" && len(p.Call.Args) == 1:
		if err := b.expr(p.Call.Args[0]); err != nil {
			return err
		}
		b.emit(vm.OpPrint)
		return nil
	default:
		return fmt.Errorf("expression not supported")
	}
}

func (b *builder) binaryOp(op string) error {
	switch op {
	case "+":
		b.emit(vm.OpAdd)
	case "-":
		b.emit(vm.OpSub)
	case "*":
		b.emit(vm.OpMul)
	case "/":
		b.emit(vm.OpDiv)
	default:
		return fmt.Errorf("binary op %s not supported", op)
	}
	return nil
}
