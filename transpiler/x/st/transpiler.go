//go:build slow

package st

import (
	"fmt"
	"io"
	"os/exec"
	"sort"
	"strconv"
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
	valFloat
	valList
	valMap
)

type value struct {
	kind valKind
	i    int
	b    bool
	s    string
	f    float64
	list []value
	kv   map[string]value
}

func keyString(v value) (string, error) {
	switch v.kind {
	case valString:
		return v.s, nil
	case valInt:
		return fmt.Sprintf("%d", v.i), nil
	default:
		return "", fmt.Errorf("invalid key type")
	}
}

func (v value) String() string { return formatValue(v, 0) }

func formatValue(v value, indent int) string {
	pad := func(i int) string { return strings.Repeat(" ", i) }
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
	case valFloat:
		return fmt.Sprintf("%.1f", v.f)
	case valList:
		if len(v.list) == 0 {
			return "#()"
		}
		lines := []string{"#("}
		for i, elem := range v.list {
			line := pad(indent+2) + formatValue(elem, indent+2)
			if i < len(v.list)-1 {
				line += "."
			}
			lines = append(lines, line)
		}
		lines = append(lines, pad(indent)+")")
		return strings.Join(lines, "\n")
	case valMap:
		if len(v.kv) == 0 {
			return "Dictionary newFrom: {}"
		}
		keys := make([]string, 0, len(v.kv))
		for k := range v.kv {
			keys = append(keys, k)
		}
		sort.Strings(keys)
		lines := []string{"Dictionary newFrom: {"}
		for i, k := range keys {
			val := v.kv[k]
			line := pad(indent+2) + fmt.Sprintf("'%s'->%s", escape(k), formatValue(val, indent+2))
			if i < len(keys)-1 {
				line += "."
			}
			lines = append(lines, line)
		}
		lines = append(lines, pad(indent)+"}")
		return strings.Join(lines, "\n")
	default:
		return ""
	}
}

func isTruthy(v value) bool {
	switch v.kind {
	case valBool:
		return v.b
	case valInt:
		return v.i != 0
	case valFloat:
		return v.f != 0
	case valString:
		return v.s != ""
	case valList:
		return len(v.list) > 0
	case valMap:
		return len(v.kv) > 0
	default:
		return false
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

func appendAssign(lines *[]string, name string, v value) {
	sv := formatValue(v, 0)
	parts := strings.Split(sv, "\n")
	if len(parts) == 1 {
		*lines = append(*lines, fmt.Sprintf("%s := %s.", name, parts[0]))
		return
	}
	*lines = append(*lines, fmt.Sprintf("%s := %s", name, parts[0]))
	for _, line := range parts[1:] {
		*lines = append(*lines, line)
	}
	(*lines)[len(*lines)-1] += "."
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
		{"<", "<=", ">", ">=", "==", "!=", "in"},
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
	case "in":
		if b.kind == valList {
			for _, it := range b.list {
				res, err := applyOp(a, "==", it)
				if err == nil && res.kind == valBool && res.b {
					return value{kind: valBool, b: true}, nil
				}
			}
			return value{kind: valBool, b: false}, nil
		}
		if b.kind == valMap {
			key, err := keyString(a)
			if err != nil {
				return value{}, err
			}
			_, ok := b.kv[key]
			return value{kind: valBool, b: ok}, nil
		}
		if a.kind == valString && b.kind == valString {
			return value{kind: valBool, b: strings.Contains(b.s, a.s)}, nil
		}
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
	// special case string method calls like s.contains("cat")
	if p.Target != nil && p.Target.Selector != nil && len(p.Target.Selector.Tail) == 1 && len(p.Ops) > 0 && p.Ops[0].Call != nil {
		root, ok := vars[p.Target.Selector.Root]
		if ok && root.kind == valString && p.Target.Selector.Tail[0] == "contains" {
			call := p.Ops[0].Call
			if len(call.Args) != 1 {
				return value{}, fmt.Errorf("bad args")
			}
			arg, err := evalExpr(call.Args[0], vars)
			if err != nil {
				return value{}, err
			}
			if arg.kind != valString {
				return value{}, fmt.Errorf("bad args")
			}
			return value{kind: valBool, b: strings.Contains(root.s, arg.s)}, nil
		}
	}

	v, err := evalPrimary(p.Target, vars)
	if err != nil {
		return value{}, err
	}
	for i := 0; i < len(p.Ops); i++ {
		op := p.Ops[i]
		switch {
		case op.Index != nil:
			idxOp := op.Index
			if idxOp.Colon != nil || idxOp.End != nil || idxOp.Colon2 != nil || idxOp.Step != nil {
				return value{}, fmt.Errorf("slicing not supported")
			}
			idxVal, err := evalExpr(idxOp.Start, vars)
			if err != nil {
				return value{}, err
			}
			switch v.kind {
			case valList:
				if idxVal.kind != valInt {
					return value{}, fmt.Errorf("index must be int")
				}
				if idxVal.i < 0 || idxVal.i >= len(v.list) {
					return value{}, fmt.Errorf("index out of range")
				}
				v = v.list[idxVal.i]
			case valString:
				if idxVal.kind != valInt {
					return value{}, fmt.Errorf("index must be int")
				}
				r := []rune(v.s)
				if idxVal.i < 0 || idxVal.i >= len(r) {
					return value{}, fmt.Errorf("index out of range")
				}
				v = value{kind: valString, s: string(r[idxVal.i])}
			case valMap:
				key, err := keyString(idxVal)
				if err != nil {
					return value{}, err
				}
				mv, ok := v.kv[key]
				if !ok {
					return value{}, fmt.Errorf("missing key")
				}
				v = mv
			default:
				return value{}, fmt.Errorf("indexing non-container")
			}
		case op.Cast != nil:
			if op.Cast.Type == nil || op.Cast.Type.Simple == nil {
				return value{}, fmt.Errorf("unsupported cast")
			}
			t := *op.Cast.Type.Simple
			switch t {
			case "int":
				switch v.kind {
				case valInt:
					// no-op
				case valString:
					n, err := strconv.Atoi(v.s)
					if err != nil {
						return value{}, fmt.Errorf("invalid int")
					}
					v = value{kind: valInt, i: n}
				default:
					return value{}, fmt.Errorf("bad cast")
				}
			default:
				return value{}, fmt.Errorf("unsupported cast")
			}
		case op.Field != nil:
			// support simple string methods like contains
			if i+1 < len(p.Ops) && p.Ops[i+1].Call != nil && op.Field.Name == "contains" && v.kind == valString {
				call := p.Ops[i+1].Call
				if len(call.Args) != 1 {
					return value{}, fmt.Errorf("bad args")
				}
				arg, err := evalExpr(call.Args[0], vars)
				if err != nil {
					return value{}, err
				}
				if arg.kind != valString {
					return value{}, fmt.Errorf("bad args")
				}
				v = value{kind: valBool, b: strings.Contains(v.s, arg.s)}
				i++
			} else {
				return value{}, fmt.Errorf("postfix not supported")
			}
		default:
			return value{}, fmt.Errorf("postfix not supported")
		}
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
		if v, ok := vars[p.Selector.Root]; ok {
			for _, f := range p.Selector.Tail {
				if v.kind != valMap {
					return value{}, fmt.Errorf("field access on non-map")
				}
				fv, ok := v.kv[f]
				if !ok {
					return value{}, fmt.Errorf("missing field")
				}
				v = fv
			}
			return v, nil
		}
		if len(p.Selector.Tail) == 0 {
			return value{kind: valString, s: p.Selector.Root}, nil
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
			if name, ok := identName(it.Key); ok {
				v, err := evalExpr(it.Value, vars)
				if err != nil {
					return value{}, err
				}
				m[name] = v
				continue
			}
			k, err := evalExpr(it.Key, vars)
			if err != nil {
				return value{}, err
			}
			key, err := keyString(k)
			if err != nil {
				return value{}, err
			}
			v, err := evalExpr(it.Value, vars)
			if err != nil {
				return value{}, err
			}
			m[key] = v
		}
		return value{kind: valMap, kv: m}, nil
	case p.Query != nil:
		return evalQueryExpr(p.Query, vars)
	case p.Call != nil:
		switch p.Call.Func {
		case "len", "count":
			if len(p.Call.Args) != 1 {
				return value{}, fmt.Errorf("bad args")
			}
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
		case "values":
			if len(p.Call.Args) != 1 {
				return value{}, fmt.Errorf("bad args")
			}
			v, err := evalExpr(p.Call.Args[0], vars)
			if err != nil {
				return value{}, err
			}
			if v.kind == valMap {
				keys := make([]string, 0, len(v.kv))
				for k := range v.kv {
					keys = append(keys, k)
				}
				sort.Strings(keys)
				vals := make([]value, 0, len(keys))
				for _, k := range keys {
					vals = append(vals, v.kv[k])
				}
				return value{kind: valList, list: vals}, nil
			}
		case "str":
			if len(p.Call.Args) != 1 {
				return value{}, fmt.Errorf("bad args")
			}
			v, err := evalExpr(p.Call.Args[0], vars)
			if err != nil {
				return value{}, err
			}
			switch v.kind {
			case valInt:
				return value{kind: valString, s: fmt.Sprintf("%d", v.i)}, nil
			case valBool:
				if v.b {
					return value{kind: valString, s: "true"}, nil
				}
				return value{kind: valString, s: "false"}, nil
			case valString:
				return value{kind: valString, s: v.s}, nil
			}
		case "append":
			if len(p.Call.Args) != 2 {
				return value{}, fmt.Errorf("bad args")
			}
			listv, err := evalExpr(p.Call.Args[0], vars)
			if err != nil {
				return value{}, err
			}
			elem, err := evalExpr(p.Call.Args[1], vars)
			if err != nil {
				return value{}, err
			}
			if listv.kind == valList {
				nl := append(append([]value{}, listv.list...), elem)
				return value{kind: valList, list: nl}, nil
			}
		case "sum":
			if len(p.Call.Args) != 1 {
				return value{}, fmt.Errorf("bad args")
			}
			listv, err := evalExpr(p.Call.Args[0], vars)
			if err != nil {
				return value{}, err
			}
			if listv.kind == valList {
				total := 0
				for _, it := range listv.list {
					if it.kind != valInt {
						return value{}, fmt.Errorf("sum supports int list")
					}
					total += it.i
				}
				return value{kind: valInt, i: total}, nil
			}
		case "avg":
			if len(p.Call.Args) != 1 {
				return value{}, fmt.Errorf("bad args")
			}
			listv, err := evalExpr(p.Call.Args[0], vars)
			if err != nil {
				return value{}, err
			}
			if listv.kind == valList {
				if len(listv.list) == 0 {
					return value{kind: valFloat, f: 0}, nil
				}
				total := 0
				for _, it := range listv.list {
					if it.kind != valInt {
						return value{}, fmt.Errorf("avg supports int list")
					}
					total += it.i
				}
				return value{kind: valFloat, f: float64(total) / float64(len(listv.list))}, nil
			}
		case "substring":
			if len(p.Call.Args) != 3 {
				return value{}, fmt.Errorf("bad args")
			}
			s, err := evalExpr(p.Call.Args[0], vars)
			if err != nil {
				return value{}, err
			}
			start, err := evalExpr(p.Call.Args[1], vars)
			if err != nil {
				return value{}, err
			}
			end, err := evalExpr(p.Call.Args[2], vars)
			if err != nil {
				return value{}, err
			}
			if s.kind == valString && start.kind == valInt && end.kind == valInt {
				r := []rune(s.s)
				if start.i < 0 || end.i > len(r) || start.i > end.i {
					return value{}, fmt.Errorf("bad substring range")
				}
				return value{kind: valString, s: string(r[start.i:end.i])}, nil
			}
		case "min":
			if len(p.Call.Args) != 1 {
				return value{}, fmt.Errorf("bad args")
			}
			listv, err := evalExpr(p.Call.Args[0], vars)
			if err != nil {
				return value{}, err
			}
			if listv.kind == valList && len(listv.list) > 0 {
				m := listv.list[0].i
				for _, it := range listv.list {
					if it.kind != valInt {
						return value{}, fmt.Errorf("min supports int list")
					}
					if it.i < m {
						m = it.i
					}
				}
				return value{kind: valInt, i: m}, nil
			}
		case "max":
			if len(p.Call.Args) != 1 {
				return value{}, fmt.Errorf("bad args")
			}
			listv, err := evalExpr(p.Call.Args[0], vars)
			if err != nil {
				return value{}, err
			}
			if listv.kind == valList && len(listv.list) > 0 {
				m := listv.list[0].i
				for _, it := range listv.list {
					if it.kind != valInt {
						return value{}, fmt.Errorf("max supports int list")
					}
					if it.i > m {
						m = it.i
					}
				}
				return value{kind: valInt, i: m}, nil
			}
		case "exists":
			if len(p.Call.Args) != 1 {
				return value{}, fmt.Errorf("bad args")
			}
			v, err := evalExpr(p.Call.Args[0], vars)
			if err != nil {
				return value{}, err
			}
			if v.kind == valList {
				return value{kind: valBool, b: len(v.list) > 0}, nil
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
	if isTruthy(cond) {
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

func copyVars(vars map[string]value) map[string]value {
	m := make(map[string]value, len(vars))
	for k, v := range vars {
		m[k] = v
	}
	return m
}

func identName(e *parser.Expr) (string, bool) {
	if e == nil || e.Binary == nil || len(e.Binary.Right) != 0 {
		return "", false
	}
	u := e.Binary.Left
	if len(u.Ops) != 0 || u.Value == nil {
		return "", false
	}
	p := u.Value.Target
	if p == nil || p.Selector == nil || len(p.Selector.Tail) != 0 {
		return "", false
	}
	return p.Selector.Root, true
}

func evalQueryExpr(q *parser.QueryExpr, vars map[string]value) (value, error) {
	// handle simple join form: FROM <src> [JOIN ...] SELECT ...
	if len(q.Joins) == 1 && len(q.Froms) == 0 && q.Group == nil && q.Sort == nil && q.Skip == nil && q.Take == nil && !q.Distinct {
		j := q.Joins[0]

		left, err := evalExpr(q.Source, vars)
		if err != nil {
			return value{}, err
		}
		right, err := evalExpr(j.Src, vars)
		if err != nil {
			return value{}, err
		}
		if left.kind != valList || right.kind != valList {
			return value{}, fmt.Errorf("join sources must be list")
		}

		leftMatched := make([]bool, len(left.list))
		var results []value
		for _, r := range right.list {
			match := false
			for li, l := range left.list {
				local := copyVars(vars)
				local[q.Var] = l
				local[j.Var] = r
				cond, err := evalExpr(j.On, local)
				if err != nil {
					return value{}, err
				}
				if cond.kind != valBool {
					return value{}, fmt.Errorf("non-bool join condition")
				}
				if cond.b {
					match = true
					leftMatched[li] = true
					if q.Where != nil {
						wc, err := evalExpr(q.Where, local)
						if err != nil {
							return value{}, err
						}
						if wc.kind != valBool || !wc.b {
							continue
						}
					}
					v, err := evalExpr(q.Select, local)
					if err != nil {
						return value{}, err
					}
					results = append(results, v)
				}
			}
			if !match && j.Side != nil && (*j.Side == "right" || *j.Side == "outer") {
				local := copyVars(vars)
				local[q.Var] = value{}
				local[j.Var] = r
				if q.Where != nil {
					wc, err := evalExpr(q.Where, local)
					if err != nil {
						return value{}, err
					}
					if wc.kind != valBool || !wc.b {
						continue
					}
				}
				v, err := evalExpr(q.Select, local)
				if err != nil {
					return value{}, err
				}
				results = append(results, v)
			}
		}
		if j.Side != nil && (*j.Side == "left" || *j.Side == "outer") {
			for li, l := range left.list {
				if leftMatched[li] {
					continue
				}
				local := copyVars(vars)
				local[q.Var] = l
				local[j.Var] = value{}
				if q.Where != nil {
					wc, err := evalExpr(q.Where, local)
					if err != nil {
						return value{}, err
					}
					if wc.kind != valBool || !wc.b {
						continue
					}
				}
				v, err := evalExpr(q.Select, local)
				if err != nil {
					return value{}, err
				}
				results = append(results, v)
			}
		}
		return value{kind: valList, list: results}, nil
	}

	// fallback: simple cross join for multiple FROM clauses
	type clause struct {
		name string
		src  *parser.Expr
	}
	clauses := []clause{{q.Var, q.Source}}
	for _, f := range q.Froms {
		clauses = append(clauses, clause{f.Var, f.Src})
	}

	var results []value
	var iter func(int, map[string]value) error
	iter = func(i int, local map[string]value) error {
		if i == len(clauses) {
			if q.Where != nil {
				cond, err := evalExpr(q.Where, local)
				if err != nil {
					return err
				}
				if cond.kind != valBool || !cond.b {
					return nil
				}
			}
			v, err := evalExpr(q.Select, local)
			if err != nil {
				return err
			}
			results = append(results, v)
			return nil
		}
		cl := clauses[i]
		listv, err := evalExpr(cl.src, local)
		if err != nil {
			return err
		}
		if listv.kind != valList {
			return fmt.Errorf("query source must be list")
		}
		for _, elem := range listv.list {
			next := copyVars(local)
			next[cl.name] = elem
			if err := iter(i+1, next); err != nil {
				return err
			}
		}
		return nil
	}
	if err := iter(0, copyVars(vars)); err != nil {
		return value{}, err
	}
	return value{kind: valList, list: results}, nil
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
			appendAssign(&p.Lines, st.Let.Name, v)
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
			appendAssign(&p.Lines, st.Var.Name, v)
		case st.Assign != nil:
			if len(st.Assign.Field) > 0 {
				return fmt.Errorf("unsupported assign")
			}
			if len(st.Assign.Index) > 0 {
				if len(st.Assign.Index) != 1 {
					return fmt.Errorf("multi index not supported")
				}
				idx := st.Assign.Index[0]
				if idx.Colon != nil || idx.End != nil || idx.Colon2 != nil || idx.Step != nil {
					return fmt.Errorf("slicing not supported")
				}
				target, ok := vars[st.Assign.Name]
				if !ok {
					return fmt.Errorf("assign to unknown var")
				}
				idxVal, err := evalExpr(idx.Start, vars)
				if err != nil {
					return err
				}
				v, err := evalExpr(st.Assign.Value, vars)
				if err != nil {
					return err
				}
				switch target.kind {
				case valList:
					if idxVal.kind != valInt {
						return fmt.Errorf("index must be int")
					}
					if idxVal.i < 0 || idxVal.i >= len(target.list) {
						return fmt.Errorf("index out of range")
					}
					target.list[idxVal.i] = v
					vars[st.Assign.Name] = target
					p.Lines = append(p.Lines, fmt.Sprintf("%s at:%d put:%s.", st.Assign.Name, idxVal.i+1, v))
				case valMap:
					key, err := keyString(idxVal)
					if err != nil {
						return err
					}
					if target.kv == nil {
						target.kv = map[string]value{}
					}
					target.kv[key] = v
					vars[st.Assign.Name] = target
					keyStr := key
					if idxVal.kind == valString {
						keyStr = fmt.Sprintf("'%s'", escape(idxVal.s))
					}
					p.Lines = append(p.Lines, fmt.Sprintf("%s at:%s put:%s.", st.Assign.Name, keyStr, v))
				default:
					return fmt.Errorf("index assign only for list or map")
				}
			} else {
				v, err := evalExpr(st.Assign.Value, vars)
				if err != nil {
					return err
				}
				if _, ok := vars[st.Assign.Name]; !ok {
					return fmt.Errorf("assign to unknown var")
				}
				vars[st.Assign.Name] = v
				appendAssign(&p.Lines, st.Assign.Name, v)
			}
		case st.If != nil:
			cond, err := evalExpr(st.If.Cond, vars)
			if err != nil {
				return err
			}
			if isTruthy(cond) {
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
		case st.While != nil:
			for {
				cond, err := evalExpr(st.While.Cond, vars)
				if err != nil {
					return err
				}
				if !isTruthy(cond) {
					break
				}
				for _, b := range st.While.Body {
					if err := processStmt(b); err != nil {
						return err
					}
				}
			}
			return nil
		case st.For != nil:
			src, err := evalExpr(st.For.Source, vars)
			if err != nil {
				return err
			}
			var items []value
			if st.For.RangeEnd != nil {
				endv, err := evalExpr(st.For.RangeEnd, vars)
				if err != nil {
					return err
				}
				if src.kind != valInt || endv.kind != valInt {
					return fmt.Errorf("range must be int")
				}
				for i := src.i; i < endv.i; i++ {
					items = append(items, value{kind: valInt, i: i})
				}
			} else {
				switch src.kind {
				case valList:
					items = src.list
				case valMap:
					keys := make([]string, 0, len(src.kv))
					for k := range src.kv {
						keys = append(keys, k)
					}
					sort.Strings(keys)
					for _, k := range keys {
						items = append(items, value{kind: valString, s: k})
					}
				default:
					return fmt.Errorf("for over unsupported type")
				}
			}
			for _, it := range items {
				vars[st.For.Name] = it
				for _, b := range st.For.Body {
					if err := processStmt(b); err != nil {
						return err
					}
				}
			}
			delete(vars, st.For.Name)
			return nil
		case st.Expr != nil:
			call := st.Expr.Expr.Binary.Left.Value.Target.Call
			if call == nil || call.Func != "print" {
				return fmt.Errorf("unsupported expression")
			}
			if len(call.Args) == 0 {
				return fmt.Errorf("print with no args")
			}
			parts := make([]string, len(call.Args))
			for i, a := range call.Args {
				v, err := evalExpr(a, vars)
				if err != nil {
					return err
				}
				parts[i] = v.String()
			}
			line := "Transcript show:" + parts[0]
			for _, part := range parts[1:] {
				line += "; show:' '; show:" + part
			}
			line += "; cr"
			p.Lines = append(p.Lines, line)
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
