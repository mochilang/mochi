//go:build slow

package st

import (
	"fmt"
	"io"
	"os/exec"
	"strings"
	"time"

	"mochi/parser"
	"mochi/types"
)

// Program is a simple Smalltalk AST representing a sequence of statements.
// Program holds generated Smalltalk source lines.
type Program struct {
	Lines []string
}

type valKind int

const (
	valUnknown valKind = iota
	valInt
	valBool
	valString
	valList
	valMap
)

type value struct {
	kind valKind
	i    int
	b    bool
	s    string
	list []value
	kv   map[string]value
}

func (v value) String() string {
	switch v.kind {
	case valInt:
		return fmt.Sprintf("%d", v.i)
	case valBool:
		if v.b {
			return "true"
		}
		return "false"
	case valString:
		return fmt.Sprintf("'%s'", escape(v.s))
	case valList:
		parts := make([]string, len(v.list))
		for i, elem := range v.list {
			parts[i] = elem.String()
		}
		return "#(" + strings.Join(parts, " ") + ")"
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
	for i, line := range prog.Lines {
		if _, err := io.WriteString(w, line); err != nil {
			return err
		}
		if i < len(prog.Lines)-1 {
			if _, err := io.WriteString(w, "\n"); err != nil {
				return err
			}
		}
	}
	if len(prog.Lines) > 0 {
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
	case "list":
		return value{kind: valList}
	case "map":
		return value{kind: valMap}
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
		if a.kind == valString && b.kind == valString {
			return value{kind: valBool, b: a.s < b.s}, nil
		}
	case "<=":
		if a.kind == valInt && b.kind == valInt {
			return value{kind: valBool, b: a.i <= b.i}, nil
		}
		if a.kind == valString && b.kind == valString {
			return value{kind: valBool, b: a.s <= b.s}, nil
		}
	case ">":
		if a.kind == valInt && b.kind == valInt {
			return value{kind: valBool, b: a.i > b.i}, nil
		}
		if a.kind == valString && b.kind == valString {
			return value{kind: valBool, b: a.s > b.s}, nil
		}
	case ">=":
		if a.kind == valInt && b.kind == valInt {
			return value{kind: valBool, b: a.i >= b.i}, nil
		}
		if a.kind == valString && b.kind == valString {
			return value{kind: valBool, b: a.s >= b.s}, nil
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
	case p.List != nil:
		elems := make([]value, len(p.List.Elems))
		for i, e := range p.List.Elems {
			v, err := evalExpr(e, vars)
			if err != nil {
				return value{}, err
			}
			elems[i] = v
		}
		return value{kind: valList, list: elems}, nil
	case p.Map != nil:
		m := make(map[string]value)
		for _, it := range p.Map.Items {
			k, err := evalExpr(it.Key, vars)
			if err != nil {
				return value{}, err
			}
			if k.kind != valString {
				return value{}, fmt.Errorf("map key must be string")
			}
			v, err := evalExpr(it.Value, vars)
			if err != nil {
				return value{}, err
			}
			m[k.s] = v
		}
		return value{kind: valMap, kv: m}, nil
	case p.Call != nil:
		if p.Call.Func == "len" && len(p.Call.Args) == 1 {
			v, err := evalExpr(p.Call.Args[0], vars)
			if err != nil {
				return value{}, err
			}
			switch v.kind {
			case valString:
				return value{kind: valInt, i: len([]rune(v.s))}, nil
			case valList:
				return value{kind: valInt, i: len(v.list)}, nil
			case valMap:
				return value{kind: valInt, i: len(v.kv)}, nil
			}
		}
		return value{}, fmt.Errorf("unsupported call")
	case p.If != nil:
		return evalIfExpr(p.If, vars)
	case p.Group != nil:
		return evalExpr(p.Group, vars)
	}
	return value{}, fmt.Errorf("unsupported primary")
}

func evalIfExpr(ie *parser.IfExpr, vars map[string]value) (value, error) {
	cond, err := evalExpr(ie.Cond, vars)
	if err != nil {
		return value{}, err
	}
	if cond.kind != valBool {
		return value{}, fmt.Errorf("non-bool condition")
	}
	if cond.b {
		return evalExpr(ie.Then, vars)
	}
	if ie.ElseIf != nil {
		return evalIfExpr(ie.ElseIf, vars)
	}
	if ie.Else != nil {
		return evalExpr(ie.Else, vars)
	}
	return value{}, nil
}

// Transpile converts a Mochi program into our Smalltalk AST.
func Transpile(prog *parser.Program, env *types.Env) (*Program, error) {
	vars := map[string]value{}
	p := &Program{}

	var processStmt func(*parser.Statement) error
	processStmt = func(st *parser.Statement) error {
		switch {
		case st.Let != nil:
			v := value{}
			if st.Let.Value != nil {
				var err error
				v, err = evalExpr(st.Let.Value, vars)
				if err != nil {
					return err
				}
			} else if st.Let.Type != nil {
				v = zeroValue(st.Let.Type)
			}
			vars[st.Let.Name] = v
			p.Lines = append(p.Lines, fmt.Sprintf("%s := %s.", st.Let.Name, v))
		case st.Var != nil:
			v := value{}
			if st.Var.Value != nil {
				var err error
				v, err = evalExpr(st.Var.Value, vars)
				if err != nil {
					return err
				}
			} else if st.Var.Type != nil {
				v = zeroValue(st.Var.Type)
			}
			vars[st.Var.Name] = v
			p.Lines = append(p.Lines, fmt.Sprintf("%s := %s.", st.Var.Name, v))
		case st.Assign != nil:
			if len(st.Assign.Index) > 0 || len(st.Assign.Field) > 0 {
				return fmt.Errorf("unsupported assign")
			}
			v, err := evalExpr(st.Assign.Value, vars)
			if err != nil {
				return err
			}
			if _, ok := vars[st.Assign.Name]; !ok {
				return fmt.Errorf("assign to unknown var")
			}
			vars[st.Assign.Name] = v
			p.Lines = append(p.Lines, fmt.Sprintf("%s := %s.", st.Assign.Name, v))
		case st.If != nil:
			cond, err := evalExpr(st.If.Cond, vars)
			if err != nil {
				return err
			}
			if cond.kind != valBool {
				return fmt.Errorf("non-bool condition")
			}
			if cond.b {
				for _, s := range st.If.Then {
					if err := processStmt(s); err != nil {
						return err
					}
				}
				return nil
			}
			if st.If.ElseIf != nil {
				return processStmt(&parser.Statement{If: st.If.ElseIf})
			}
			for _, s := range st.If.Else {
				if err := processStmt(s); err != nil {
					return err
				}
			}
			return nil
		case st.Expr != nil:
			call := st.Expr.Expr.Binary.Left.Value.Target.Call
			if call == nil || call.Func != "print" || len(call.Args) != 1 {
				return fmt.Errorf("unsupported expression")
			}
			arg, err := evalExpr(call.Args[0], vars)
			if err != nil {
				return err
			}
			p.Lines = append(p.Lines, fmt.Sprintf("Transcript show: %s; cr", arg))
		default:
			return fmt.Errorf("unsupported statement")
		}
		return nil
	}

	for _, st := range prog.Statements {
		if err := processStmt(st); err != nil {
			return nil, err
		}
	}

	_ = env
	return p, nil
}

func header() string {
	out, err := exec.Command("git", "log", "-1", "--format=%cI").Output()
	t := time.Now()
	if err == nil {
		if ts, perr := time.Parse(time.RFC3339, strings.TrimSpace(string(out))); perr == nil {
			t = ts
		}
	}
	return fmt.Sprintf("\" Code generated by Mochi transpiler on %s\n", t.Format("2006-01-02 15:04 MST"))
}
