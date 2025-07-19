package st

import (
	"fmt"
	"io"
	"os"
	"path/filepath"
	"strings"
	"time"

	"mochi/ast"
	"mochi/parser"
	"mochi/types"
)

// Program is a simple Smalltalk AST representing a sequence of statements.
type Program struct {
	Stmts []Stmt
}

type Stmt interface{ emit(io.Writer) }

// ExprStmt is a statement consisting solely of an expression.
type ExprStmt struct{ Expr Expr }

func (s *ExprStmt) emit(w io.Writer) { s.Expr.emit(w) }

type Expr interface{ emit(io.Writer) }

// PrintString represents a Transcript show statement.
type PrintString struct{ Value string }

func (p *PrintString) emit(w io.Writer) {
	fmt.Fprintf(w, "Transcript show: '%s'; cr", escape(p.Value))
}

type valKind int

const (
	valUnknown valKind = iota
	valInt
	valBool
	valString
)

type value struct {
	kind valKind
	i    int
	b    bool
	s    string
}

func (v value) String() string {
	switch v.kind {
	case valInt:
		return fmt.Sprintf("%d", v.i)
	case valBool:
		if v.b {
			return "1"
		}
		return "0"
	case valString:
		return v.s
	default:
		return ""
	}
}

// Emit writes the Smalltalk source code to w with a generated header.
func Emit(w io.Writer, prog *Program) error {
	if prog == nil {
		return nil
	}
	if _, err := io.WriteString(w, header()); err != nil {
		return err
	}
	for i, s := range prog.Stmts {
		s.emit(w)
		if i < len(prog.Stmts)-1 {
			if _, err := io.WriteString(w, "\n"); err != nil {
				return err
			}
		}
	}
	if len(prog.Stmts) > 0 {
		if _, err := io.WriteString(w, "\n"); err != nil {
			return err
		}
	}
	return nil
}

func escape(s string) string { return strings.ReplaceAll(s, "'", "''") }

func zeroValue(t *parser.TypeRef) value {
	if t == nil || t.Simple == nil {
		return value{}
	}
	switch *t.Simple {
	case "int":
		return value{kind: valInt}
	case "bool":
		return value{kind: valBool}
	case "string":
		return value{kind: valString}
	default:
		return value{}
	}
}

func evalExpr(e *parser.Expr, vars map[string]value) (value, error) {
	if e == nil {
		return value{}, fmt.Errorf("nil expr")
	}
	return evalBinary(e.Binary, vars)
}

func evalBinary(b *parser.BinaryExpr, vars map[string]value) (value, error) {
	values := []value{}
	ops := []string{}
	left, err := evalUnary(b.Left, vars)
	if err != nil {
		return value{}, err
	}
	values = append(values, left)
	for _, op := range b.Right {
		right, err := evalPostfix(op.Right, vars)
		if err != nil {
			return value{}, err
		}
		ops = append(ops, op.Op)
		values = append(values, right)
	}
	prec := [][]string{
		{"*", "/", "%"},
		{"+", "-"},
		{"<", "<=", ">", ">=", "==", "!="},
		{"&&"},
		{"||"},
	}
	for _, level := range prec {
		i := 0
		for i < len(ops) {
			if contains(level, ops[i]) {
				v, err := applyOp(values[i], ops[i], values[i+1])
				if err != nil {
					return value{}, err
				}
				values[i] = v
				values = append(values[:i+1], values[i+2:]...)
				ops = append(ops[:i], ops[i+1:]...)
			} else {
				i++
			}
		}
	}
	if len(values) != 1 {
		return value{}, fmt.Errorf("eval error")
	}
	return values[0], nil
}

func contains(list []string, s string) bool {
	for _, v := range list {
		if v == s {
			return true
		}
	}
	return false
}

func applyOp(a value, op string, b value) (value, error) {
	switch op {
	case "+":
		if a.kind == valString && b.kind == valString {
			return value{kind: valString, s: a.s + b.s}, nil
		}
		if a.kind == valInt && b.kind == valInt {
			return value{kind: valInt, i: a.i + b.i}, nil
		}
	case "-":
		if a.kind == valInt && b.kind == valInt {
			return value{kind: valInt, i: a.i - b.i}, nil
		}
	case "*":
		if a.kind == valInt && b.kind == valInt {
			return value{kind: valInt, i: a.i * b.i}, nil
		}
	case "/":
		if a.kind == valInt && b.kind == valInt && b.i != 0 {
			return value{kind: valInt, i: a.i / b.i}, nil
		}
	case "%":
		if a.kind == valInt && b.kind == valInt && b.i != 0 {
			return value{kind: valInt, i: a.i % b.i}, nil
		}
	case "<":
		if a.kind == valInt && b.kind == valInt {
			return value{kind: valBool, b: a.i < b.i}, nil
		}
	case "<=":
		if a.kind == valInt && b.kind == valInt {
			return value{kind: valBool, b: a.i <= b.i}, nil
		}
	case ">":
		if a.kind == valInt && b.kind == valInt {
			return value{kind: valBool, b: a.i > b.i}, nil
		}
	case ">=":
		if a.kind == valInt && b.kind == valInt {
			return value{kind: valBool, b: a.i >= b.i}, nil
		}
	case "==":
		if a.kind == valInt && b.kind == valInt {
			return value{kind: valBool, b: a.i == b.i}, nil
		}
		if a.kind == valBool && b.kind == valBool {
			return value{kind: valBool, b: a.b == b.b}, nil
		}
		if a.kind == valString && b.kind == valString {
			return value{kind: valBool, b: a.s == b.s}, nil
		}
	case "!=":
		res, err := applyOp(a, "==", b)
		if err != nil {
			return value{}, err
		}
		return value{kind: valBool, b: !res.b}, nil
	case "&&":
		if a.kind == valBool && b.kind == valBool {
			return value{kind: valBool, b: a.b && b.b}, nil
		}
	case "||":
		if a.kind == valBool && b.kind == valBool {
			return value{kind: valBool, b: a.b || b.b}, nil
		}
	}
	return value{}, fmt.Errorf("unsupported op")
}

func evalUnary(u *parser.Unary, vars map[string]value) (value, error) {
	v, err := evalPostfix(u.Value, vars)
	if err != nil {
		return value{}, err
	}
	for i := len(u.Ops) - 1; i >= 0; i-- {
		switch u.Ops[i] {
		case "-":
			if v.kind == valInt {
				v.i = -v.i
			} else {
				return value{}, fmt.Errorf("bad unary")
			}
		case "!":
			if v.kind == valBool {
				v.b = !v.b
			} else {
				return value{}, fmt.Errorf("bad unary")
			}
		}
	}
	return v, nil
}

func evalPostfix(p *parser.PostfixExpr, vars map[string]value) (value, error) {
	v, err := evalPrimary(p.Target, vars)
	if err != nil {
		return value{}, err
	}
	if len(p.Ops) > 0 {
		return value{}, fmt.Errorf("postfix not supported")
	}
	return v, nil
}

func evalPrimary(p *parser.Primary, vars map[string]value) (value, error) {
	switch {
	case p.Lit != nil:
		if p.Lit.Int != nil {
			return value{kind: valInt, i: int(*p.Lit.Int)}, nil
		}
		if p.Lit.Bool != nil {
			return value{kind: valBool, b: bool(*p.Lit.Bool)}, nil
		}
		if p.Lit.Str != nil {
			return value{kind: valString, s: *p.Lit.Str}, nil
		}
	case p.Selector != nil:
		if len(p.Selector.Tail) == 0 {
			if v, ok := vars[p.Selector.Root]; ok {
				return v, nil
			}
		}
	case p.Group != nil:
		return evalExpr(p.Group, vars)
	}
	return value{}, fmt.Errorf("unsupported primary")
}

// Transpile converts a Mochi program into our Smalltalk AST.
func Transpile(prog *parser.Program, env *types.Env) (*Program, error) {
	vars := map[string]value{}
	p := &Program{}
	for _, st := range prog.Statements {
		switch {
		case st.Let != nil:
			v := value{}
			if st.Let.Value != nil {
				var err error
				v, err = evalExpr(st.Let.Value, vars)
				if err != nil {
					return nil, err
				}
			} else if st.Let.Type != nil {
				v = zeroValue(st.Let.Type)
			}
			vars[st.Let.Name] = v
		case st.Var != nil:
			v := value{}
			if st.Var.Value != nil {
				var err error
				v, err = evalExpr(st.Var.Value, vars)
				if err != nil {
					return nil, err
				}
			} else if st.Var.Type != nil {
				v = zeroValue(st.Var.Type)
			}
			vars[st.Var.Name] = v
		case st.Assign != nil:
			if len(st.Assign.Index) > 0 || len(st.Assign.Field) > 0 {
				return nil, fmt.Errorf("unsupported assign")
			}
			v, err := evalExpr(st.Assign.Value, vars)
			if err != nil {
				return nil, err
			}
			if _, ok := vars[st.Assign.Name]; !ok {
				return nil, fmt.Errorf("assign to unknown var")
			}
			vars[st.Assign.Name] = v
		case st.Expr != nil:
			call := st.Expr.Expr.Binary.Left.Value.Target.Call
			if call == nil || call.Func != "print" || len(call.Args) != 1 {
				return nil, fmt.Errorf("unsupported expression")
			}
			arg, err := evalExpr(call.Args[0], vars)
			if err != nil {
				return nil, err
			}
			p.Stmts = append(p.Stmts, &ExprStmt{Expr: &PrintString{Value: arg.String()}})
		default:
			return nil, fmt.Errorf("unsupported statement")
		}
	}
	_ = env
	return p, nil
}

func repoRoot() string {
	dir, err := os.Getwd()
	if err != nil {
		return ""
	}
	for i := 0; i < 10; i++ {
		if _, err := os.Stat(filepath.Join(dir, "go.mod")); err == nil {
			return dir
		}
		parent := filepath.Dir(dir)
		if parent == dir {
			break
		}
		dir = parent
	}
	return ""
}

func version() string {
	root := repoRoot()
	if root == "" {
		return "dev"
	}
	if data, err := os.ReadFile(filepath.Join(root, "VERSION")); err == nil {
		return strings.TrimSpace(string(data))
	}
	return "dev"
}

func header() string {
	loc := time.FixedZone("GMT+7", 7*3600)
	t := time.Now().In(loc)
	return fmt.Sprintf("\" Generated by Mochi transpiler v%s on %s\n", version(), t.Format("2006-01-02 15:04:05 MST"))
}

// Print converts the AST to a generic ast.Node and prints it.
func Print(p *Program) {
	toNode(p).Print("")
}

func toNode(p *Program) *ast.Node {
	n := &ast.Node{Kind: "program"}
	for _, s := range p.Stmts {
		n.Children = append(n.Children, stmtNode(s))
	}
	return n
}

func stmtNode(s Stmt) *ast.Node {
	switch st := s.(type) {
	case *ExprStmt:
		return &ast.Node{Kind: "print", Children: []*ast.Node{{Kind: "string", Value: st.Expr.(*PrintString).Value}}}
	default:
		return &ast.Node{Kind: "unknown"}
	}
}
