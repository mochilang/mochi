package asm

import (
	"fmt"
	"runtime"
	"strings"

	"mochi/parser"
	"mochi/types"
)

// Compiler translates a Mochi AST into x86-64 assembly. Only a very small
// subset of the language is currently supported.
type Compiler struct {
	env *types.Env
}

// New creates a new assembly compiler.
func New(env *types.Env) *Compiler { return &Compiler{env: env} }

// Compile emits AT&T style x86-64 assembly that prints the results of simple
// integer expressions using printf from libc.
func (c *Compiler) Compile(prog *parser.Program) ([]byte, error) {
	switch runtime.GOARCH {
	case "amd64":
		return c.compileAMD64(prog)
	case "arm64":
		return c.compileARM64(prog)
	default:
		return nil, fmt.Errorf("assembly backend unsupported on %s/%s", runtime.GOOS, runtime.GOARCH)
	}
}

func (c *Compiler) compileAMD64(prog *parser.Program) ([]byte, error) {
	switch runtime.GOOS {
	case "linux":
		return c.compileAMD64Linux(prog)
	case "darwin":
		return c.compileAMD64Darwin(prog)
	case "windows":
		return c.compileAMD64Windows(prog)
	default:
		return nil, fmt.Errorf("unsupported OS %s for amd64", runtime.GOOS)
	}
}

func (c *Compiler) compileAMD64Linux(prog *parser.Program) ([]byte, error) {
	var out []string
	out = append(out, "\t.section .rodata")
	out = append(out, "fmt: .string \"%d\\n\"")
	out = append(out, "\t.text")
	out = append(out, "\t.globl main")
	out = append(out, "main:")

	for _, s := range prog.Statements {
		call, ok := printCall(s)
		if !ok {
			return nil, fmt.Errorf("unsupported statement")
		}
		if len(call.Args) != 1 {
			return nil, fmt.Errorf("print with %d args unsupported", len(call.Args))
		}
		val, err := evalInt(call.Args[0])
		if err != nil {
			return nil, err
		}
		out = append(out,
			fmt.Sprintf("\tmovl $%d, %%esi", val),
			"\tmovl $fmt, %edi",
			"\txorl %eax, %eax",
			"\tcall printf@PLT",
		)
	}

	out = append(out,
		"\tmovl $0, %eax",
		"\tret",
	)

	return []byte(strings.Join(out, "\n") + "\n"), nil
}

func (c *Compiler) compileAMD64Darwin(prog *parser.Program) ([]byte, error) {
	var out []string
	out = append(out, "\t.section __TEXT,__cstring")
	out = append(out, "_fmt: .asciz \"%d\\n\"")
	out = append(out, "\t.text")
	out = append(out, "\t.globl _main")
	out = append(out, "_main:")

	for _, s := range prog.Statements {
		call, ok := printCall(s)
		if !ok {
			return nil, fmt.Errorf("unsupported statement")
		}
		if len(call.Args) != 1 {
			return nil, fmt.Errorf("print with %d args unsupported", len(call.Args))
		}
		val, err := evalInt(call.Args[0])
		if err != nil {
			return nil, err
		}
		out = append(out,
			fmt.Sprintf("\tmovl $%d, %%esi", val),
			"\tleaq _fmt(%rip), %rdi",
			"\txorl %eax, %eax",
			"\tcall _printf",
		)
	}

	out = append(out,
		"\tmovl $0, %eax",
		"\tret",
	)

	return []byte(strings.Join(out, "\n") + "\n"), nil
}

func (c *Compiler) compileAMD64Windows(prog *parser.Program) ([]byte, error) {
	var out []string
	out = append(out, "\t.section .rdata")
	out = append(out, "fmt: .asciz \"%d\\n\"")
	out = append(out, "\t.text")
	out = append(out, "\t.globl main")
	out = append(out, "main:")

	for _, s := range prog.Statements {
		call, ok := printCall(s)
		if !ok {
			return nil, fmt.Errorf("unsupported statement")
		}
		if len(call.Args) != 1 {
			return nil, fmt.Errorf("print with %d args unsupported", len(call.Args))
		}
		val, err := evalInt(call.Args[0])
		if err != nil {
			return nil, err
		}
		out = append(out,
			"\tsubq $40, %rsp",
			fmt.Sprintf("\tmovl $%d, %%edx", val),
			"\tleaq fmt(%rip), %rcx",
			"\tcall printf",
			"\taddq $40, %rsp",
		)
	}

	out = append(out,
		"\tmovl $0, %eax",
		"\tret",
	)

	return []byte(strings.Join(out, "\n") + "\n"), nil
}

func (c *Compiler) compileARM64(prog *parser.Program) ([]byte, error) {
	switch runtime.GOOS {
	case "linux":
		return c.compileARM64Linux(prog)
	case "darwin":
		return c.compileARM64Darwin(prog)
	default:
		return nil, fmt.Errorf("unsupported OS %s for arm64", runtime.GOOS)
	}
}

func (c *Compiler) compileARM64Linux(prog *parser.Program) ([]byte, error) {
	var out []string
	out = append(out, "\t.section .rodata")
	out = append(out, "fmt: .string \"%d\\n\"")
	out = append(out, "\t.text")
	out = append(out, "\t.globl main")
	out = append(out, "main:")
	out = append(out, "\tstp x29, x30, [sp, -16]!")
	out = append(out, "\tmov x29, sp")

	for _, s := range prog.Statements {
		call, ok := printCall(s)
		if !ok {
			return nil, fmt.Errorf("unsupported statement")
		}
		if len(call.Args) != 1 {
			return nil, fmt.Errorf("print with %d args unsupported", len(call.Args))
		}
		val, err := evalInt(call.Args[0])
		if err != nil {
			return nil, err
		}
		out = append(out,
			fmt.Sprintf("\tmov x1, #%d", val),
			"\tadrp x0, fmt@PAGE",
			"\tadd x0, x0, :lo12:fmt",
			"\tbl printf",
		)
	}

	out = append(out,
		"\tmov w0, #0",
		"\tldp x29, x30, [sp], #16",
		"\tret",
	)

	return []byte(strings.Join(out, "\n") + "\n"), nil
}

func (c *Compiler) compileARM64Darwin(prog *parser.Program) ([]byte, error) {
	var out []string
	out = append(out, "\t.section __TEXT,__cstring")
	out = append(out, "_fmt: .asciz \"%d\\n\"")
	out = append(out, "\t.text")
	out = append(out, "\t.globl _main")
	out = append(out, "_main:")
	out = append(out, "\tstp x29, x30, [sp, -16]!")
	out = append(out, "\tmov x29, sp")

	for _, s := range prog.Statements {
		call, ok := printCall(s)
		if !ok {
			return nil, fmt.Errorf("unsupported statement")
		}
		if len(call.Args) != 1 {
			return nil, fmt.Errorf("print with %d args unsupported", len(call.Args))
		}
		val, err := evalInt(call.Args[0])
		if err != nil {
			return nil, err
		}
		out = append(out,
			fmt.Sprintf("\tmov x1, #%d", val),
			"\tadrp x0, _fmt@PAGE",
			"\tadd x0, x0, :lo12:_fmt",
			"\tbl _printf",
		)
	}

	out = append(out,
		"\tmov w0, #0",
		"\tldp x29, x30, [sp], #16",
		"\tret",
	)

	return []byte(strings.Join(out, "\n") + "\n"), nil
}

// printCall checks if s is an expression statement calling print.
func printCall(s *parser.Statement) (*parser.CallExpr, bool) {
	if s == nil || s.Expr == nil {
		return nil, false
	}
	e := s.Expr.Expr
	if e == nil || len(e.Binary.Right) != 0 {
		return nil, false
	}
	u := e.Binary.Left
	if len(u.Ops) != 0 {
		return nil, false
	}
	p := u.Value
	if len(p.Ops) != 0 || p.Target.Call == nil {
		return nil, false
	}
	if p.Target.Call.Func != "print" {
		return nil, false
	}
	return p.Target.Call, true
}

// evalInt evaluates a simple integer expression consisting only of integer
// literals and the operators +, -, *, / and %.
func evalInt(e *parser.Expr) (int, error) {
	if e == nil {
		return 0, fmt.Errorf("nil expr")
	}
	v, err := evalUnary(e.Binary.Left)
	if err != nil {
		return 0, err
	}
	for _, op := range e.Binary.Right {
		r, err := evalPostfix(op.Right)
		if err != nil {
			return 0, err
		}
		switch op.Op {
		case "+":
			v += r
		case "-":
			v -= r
		case "*":
			v *= r
		case "/":
			v /= r
		case "%":
			v %= r
		default:
			return 0, fmt.Errorf("unsupported op %s", op.Op)
		}
	}
	return v, nil
}

func evalUnary(u *parser.Unary) (int, error) {
	val, err := evalPostfix(u.Value)
	if err != nil {
		return 0, err
	}
	for i := len(u.Ops) - 1; i >= 0; i-- {
		switch u.Ops[i] {
		case "-":
			val = -val
		case "!":
			if val == 0 {
				val = 1
			} else {
				val = 0
			}
		default:
			return 0, fmt.Errorf("unsupported unary %s", u.Ops[i])
		}
	}
	return val, nil
}

func evalPostfix(p *parser.PostfixExpr) (int, error) {
	if len(p.Ops) != 0 {
		return 0, fmt.Errorf("postfix ops unsupported")
	}
	t := p.Target
	switch {
	case t.Lit != nil && t.Lit.Int != nil:
		return *t.Lit.Int, nil
	case t.Group != nil:
		return evalInt(t.Group)
	default:
		return 0, fmt.Errorf("unsupported expr")
	}
}
