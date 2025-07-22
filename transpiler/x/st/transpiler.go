//go:build slow

package st

import (
	"bytes"
	"encoding/json"
	"fmt"
	"io"
	"math"
	"os"
	"os/exec"
	"path/filepath"
	"reflect"
	"sort"
	"strconv"
	"strings"
	"time"

	"gopkg.in/yaml.v3"

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
	valFunc
)

type value struct {
	kind valKind
	typ  string
	i    int
	b    bool
	s    string
	f    float64
	list []value
	kv   map[string]value
	fun  *parser.FunExpr
	part *partial
	env  map[string]value
}

type partial struct {
	fn   *parser.FunStmt
	args []value
}

// currentFuncs holds function declarations available during constant
// evaluation. It is set by Transpile for the duration of a single
// transpilation run.
var (
	currentFuncs      map[string]*parser.FunStmt
	currentEnv        *types.Env
	pythonMathAliases map[string]bool
)

type breakErr struct{}

func (breakErr) Error() string { return "break" }

type continueErr struct{}

func (continueErr) Error() string { return "continue" }

func toFloat(v value) float64 {
	switch v.kind {
	case valFloat:
		return v.f
	case valInt:
		return float64(v.i)
	default:
		return 0
	}
}

func keyString(v value) (string, error) {
	switch v.kind {
	case valString:
		return v.s, nil
	case valInt:
		return fmt.Sprintf("%d", v.i), nil
	case valFloat:
		return fmt.Sprintf("%g", v.f), nil
	case valBool:
		if v.b {
			return "true", nil
		}
		return "false", nil
	case valList, valMap:
		return jsonString(v), nil
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
		return fmt.Sprintf("%.2f", v.f)
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
			if v.typ != "" {
				return fmt.Sprintf("%s new", v.typ)
			}
			return "Dictionary newFrom: {}"
		}
		keys := make([]string, 0, len(v.kv))
		for k := range v.kv {
			keys = append(keys, k)
		}
		sort.Strings(keys)
		prefix := "Dictionary newFrom: {"
		if v.typ != "" {
			prefix = fmt.Sprintf("%s newFrom: {", v.typ)
		}
		lines := []string{prefix}
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
	case valFunc:
		return "[closure]"
	default:
		return ""
	}
}

func jsonString(v value) string {
	switch v.kind {
	case valInt:
		return fmt.Sprintf("%d", v.i)
	case valBool:
		if v.b {
			return "true"
		}
		return "false"
	case valString:
		return fmt.Sprintf("\"%s\"", escape(v.s))
	case valFloat:
		return fmt.Sprintf("%.2f", v.f)
	case valList:
		parts := make([]string, len(v.list))
		for i, it := range v.list {
			parts[i] = jsonString(it)
		}
		return "[" + strings.Join(parts, ", ") + "]"
	case valMap:
		keys := make([]string, 0, len(v.kv))
		for k := range v.kv {
			keys = append(keys, k)
		}
		sort.Strings(keys)
		parts := make([]string, len(keys))
		for i, k := range keys {
			parts[i] = fmt.Sprintf("\"%s\": %s", escape(k), jsonString(v.kv[k]))
		}
		return "{" + strings.Join(parts, ", ") + "}"
	case valFunc:
		return "\"<fun>\""
	default:
		return "null"
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
	case valFunc:
		return true
	default:
		return false
	}
}

func compareValues(a, b value) int {
	switch {
	case a.kind == valInt && b.kind == valInt:
		if a.i < b.i {
			return -1
		}
		if a.i > b.i {
			return 1
		}
		return 0
	case a.kind == valFloat && b.kind == valFloat:
		if a.f < b.f {
			return -1
		}
		if a.f > b.f {
			return 1
		}
		return 0
	case a.kind == valString && b.kind == valString:
		if a.s < b.s {
			return -1
		}
		if a.s > b.s {
			return 1
		}
		return 0
	case a.kind == valBool && b.kind == valBool:
		if !a.b && b.b {
			return -1
		}
		if a.b && !b.b {
			return 1
		}
		return 0
	default:
		sa := a.String()
		sb := b.String()
		if sa < sb {
			return -1
		}
		if sa > sb {
			return 1
		}
		return 0
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

func setIndexedValue(target value, idxVals []value, val value) (value, error) {
	if len(idxVals) == 0 {
		return val, nil
	}
	idx := idxVals[0]
	switch target.kind {
	case valList:
		if idx.kind != valInt {
			return value{}, fmt.Errorf("index must be int")
		}
		if idx.i < 0 || idx.i >= len(target.list) {
			return value{}, fmt.Errorf("index out of range")
		}
		child := target.list[idx.i]
		updated, err := setIndexedValue(child, idxVals[1:], val)
		if err != nil {
			return value{}, err
		}
		target.list[idx.i] = updated
		return target, nil
	case valMap:
		key, err := keyString(idx)
		if err != nil {
			return value{}, err
		}
		child := target.kv[key]
		updated, err := setIndexedValue(child, idxVals[1:], val)
		if err != nil {
			return value{}, err
		}
		if target.kv == nil {
			target.kv = map[string]value{}
		}
		target.kv[key] = updated
		return target, nil
	default:
		return value{}, fmt.Errorf("index assign only for list or map")
	}
}

func setFieldValue(target value, fields []string, val value) (value, error) {
	if len(fields) == 0 {
		return val, nil
	}
	if target.kind != valMap {
		return value{}, fmt.Errorf("field assign only for map")
	}
	if target.kv == nil {
		target.kv = map[string]value{}
	}
	child := target.kv[fields[0]]
	updated, err := setFieldValue(child, fields[1:], val)
	if err != nil {
		return value{}, err
	}
	target.kv[fields[0]] = updated
	return target, nil
}

func evalExpr(e *parser.Expr, vars map[string]value) (value, error) {
	if e == nil {
		return value{}, fmt.Errorf("nil expr")
	}
	return evalBinary(e.Binary, vars)
}

func evalBinary(b *parser.BinaryExpr, vars map[string]value) (value, error) {
	ops := make([]string, len(b.Right))
	exprs := make([]*parser.PostfixExpr, len(b.Right)+1)
	values := make([]value, len(b.Right)+1)
	done := make([]bool, len(b.Right)+1)

	left, err := evalUnary(b.Left, vars)
	if err != nil {
		return value{}, err
	}
	values[0] = left
	done[0] = true
	for i, op := range b.Right {
		ops[i] = op.Op
		exprs[i+1] = op.Right
	}

	getVal := func(i int) (value, error) {
		if !done[i] {
			v, err := evalPostfix(exprs[i], vars)
			if err != nil {
				return value{}, err
			}
			values[i] = v
			done[i] = true
		}
		return values[i], nil
	}

	prec := [][]string{
		{"*", "/", "%"},
		{"+", "-"},
		{"<", "<=", ">", ">=", "==", "!=", "in"},
		{"&&"},
		{"||"},
		{"union", "union_all", "except", "intersect"},
	}
	for _, level := range prec {
		i := 0
		for i < len(ops) {
			if contains(level, ops[i]) {
				leftVal, err := getVal(i)
				if err != nil {
					return value{}, err
				}
				if ops[i] == "&&" {
					if leftVal.kind == valBool && !leftVal.b {
						values[i] = value{kind: valBool, b: false}
						done[i] = true
						exprs = append(exprs[:i+1], exprs[i+2:]...)
						values = append(values[:i+1], values[i+2:]...)
						done = append(done[:i+1], done[i+2:]...)
						ops = append(ops[:i], ops[i+1:]...)
						continue
					}
				}
				if ops[i] == "||" {
					if leftVal.kind == valBool && leftVal.b {
						values[i] = value{kind: valBool, b: true}
						done[i] = true
						exprs = append(exprs[:i+1], exprs[i+2:]...)
						values = append(values[:i+1], values[i+2:]...)
						done = append(done[:i+1], done[i+2:]...)
						ops = append(ops[:i], ops[i+1:]...)
						continue
					}
				}
				rightVal, err := getVal(i + 1)
				if err != nil {
					return value{}, err
				}
				v, err := applyOp(leftVal, ops[i], rightVal)
				if err != nil {
					return value{}, err
				}
				values[i] = v
				done[i] = true
				exprs = append(exprs[:i+1], exprs[i+2:]...)
				values = append(values[:i+1], values[i+2:]...)
				done = append(done[:i+1], done[i+2:]...)
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
		if (a.kind == valFloat || a.kind == valInt) && (b.kind == valFloat || b.kind == valInt) {
			return value{kind: valFloat, f: toFloat(a) + toFloat(b)}, nil
		}
	case "-":
		if a.kind == valInt && b.kind == valInt {
			return value{kind: valInt, i: a.i - b.i}, nil
		}
		if (a.kind == valFloat || a.kind == valInt) && (b.kind == valFloat || b.kind == valInt) {
			return value{kind: valFloat, f: toFloat(a) - toFloat(b)}, nil
		}
	case "*":
		if a.kind == valInt && b.kind == valInt {
			return value{kind: valInt, i: a.i * b.i}, nil
		}
		if (a.kind == valFloat || a.kind == valInt) && (b.kind == valFloat || b.kind == valInt) {
			return value{kind: valFloat, f: toFloat(a) * toFloat(b)}, nil
		}
	case "/":
		if a.kind == valInt && b.kind == valInt && b.i != 0 {
			return value{kind: valInt, i: a.i / b.i}, nil
		}
		if (a.kind == valFloat || a.kind == valInt) && (b.kind == valFloat || b.kind == valInt) && toFloat(b) != 0 {
			return value{kind: valFloat, f: toFloat(a) / toFloat(b)}, nil
		}
	case "%":
		if a.kind == valInt && b.kind == valInt && b.i != 0 {
			return value{kind: valInt, i: a.i % b.i}, nil
		}
	case "<":
		if (a.kind == valInt || a.kind == valFloat) && (b.kind == valInt || b.kind == valFloat) {
			return value{kind: valBool, b: toFloat(a) < toFloat(b)}, nil
		}
		if a.kind == valString && b.kind == valString {
			return value{kind: valBool, b: a.s < b.s}, nil
		}
	case "<=":
		if (a.kind == valInt || a.kind == valFloat) && (b.kind == valInt || b.kind == valFloat) {
			return value{kind: valBool, b: toFloat(a) <= toFloat(b)}, nil
		}
		if a.kind == valString && b.kind == valString {
			return value{kind: valBool, b: a.s <= b.s}, nil
		}
	case ">":
		if (a.kind == valInt || a.kind == valFloat) && (b.kind == valInt || b.kind == valFloat) {
			return value{kind: valBool, b: toFloat(a) > toFloat(b)}, nil
		}
		if a.kind == valString && b.kind == valString {
			return value{kind: valBool, b: a.s > b.s}, nil
		}
	case ">=":
		if (a.kind == valInt || a.kind == valFloat) && (b.kind == valInt || b.kind == valFloat) {
			return value{kind: valBool, b: toFloat(a) >= toFloat(b)}, nil
		}
		if a.kind == valString && b.kind == valString {
			return value{kind: valBool, b: a.s >= b.s}, nil
		}
	case "==":
		if (a.kind == valInt || a.kind == valFloat) && (b.kind == valInt || b.kind == valFloat) {
			return value{kind: valBool, b: toFloat(a) == toFloat(b)}, nil
		}
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
	case "union":
		if a.kind == valList && b.kind == valList {
			seen := map[string]bool{}
			out := make([]value, 0, len(a.list)+len(b.list))
			for _, it := range a.list {
				key := it.String()
				if !seen[key] {
					seen[key] = true
					out = append(out, it)
				}
			}
			for _, it := range b.list {
				key := it.String()
				if !seen[key] {
					seen[key] = true
					out = append(out, it)
				}
			}
			return value{kind: valList, list: out}, nil
		}
	case "union_all":
		if a.kind == valList && b.kind == valList {
			out := append(append([]value{}, a.list...), b.list...)
			return value{kind: valList, list: out}, nil
		}
	case "except":
		if a.kind == valList && b.kind == valList {
			out := []value{}
			for _, it := range a.list {
				keep := true
				for _, jb := range b.list {
					res, err := applyOp(it, "==", jb)
					if err == nil && res.kind == valBool && res.b {
						keep = false
						break
					}
				}
				if keep {
					out = append(out, it)
				}
			}
			return value{kind: valList, list: out}, nil
		}
	case "intersect":
		if a.kind == valList && b.kind == valList {
			out := []value{}
			seen := map[string]bool{}
			for _, it := range a.list {
				for _, jb := range b.list {
					res, err := applyOp(it, "==", jb)
					if err == nil && res.kind == valBool && res.b {
						key := it.String()
						if !seen[key] {
							seen[key] = true
							out = append(out, it)
						}
						break
					}
				}
			}
			return value{kind: valList, list: out}, nil
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
			switch v.kind {
			case valInt:
				v.i = -v.i
			case valFloat:
				v.f = -v.f
			default:
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
		if p.Target.Selector.Root == "testpkg" && p.Target.Selector.Tail[0] == "Add" {
			call := p.Ops[0].Call
			if len(call.Args) != 2 {
				return value{}, fmt.Errorf("bad args")
			}
			a, err := evalExpr(call.Args[0], vars)
			if err != nil {
				return value{}, err
			}
			b, err := evalExpr(call.Args[1], vars)
			if err != nil {
				return value{}, err
			}
			if a.kind == valInt && b.kind == valInt {
				return value{kind: valInt, i: a.i + b.i}, nil
			}
			return value{}, fmt.Errorf("bad args")
		}
		if pythonMathAliases[p.Target.Selector.Root] {
			call := p.Ops[0].Call
			name := p.Target.Selector.Tail[0]
			switch name {
			case "sqrt":
				if len(call.Args) != 1 {
					return value{}, fmt.Errorf("bad args")
				}
				x, err := evalExpr(call.Args[0], vars)
				if err != nil {
					return value{}, err
				}
				return value{kind: valFloat, f: math.Sqrt(toFloat(x))}, nil
			case "pow":
				if len(call.Args) != 2 {
					return value{}, fmt.Errorf("bad args")
				}
				x, err := evalExpr(call.Args[0], vars)
				if err != nil {
					return value{}, err
				}
				y, err := evalExpr(call.Args[1], vars)
				if err != nil {
					return value{}, err
				}
				return value{kind: valFloat, f: math.Pow(toFloat(x), toFloat(y))}, nil
			case "sin":
				if len(call.Args) != 1 {
					return value{}, fmt.Errorf("bad args")
				}
				x, err := evalExpr(call.Args[0], vars)
				if err != nil {
					return value{}, err
				}
				return value{kind: valFloat, f: math.Sin(toFloat(x))}, nil
			case "log":
				if len(call.Args) != 1 {
					return value{}, fmt.Errorf("bad args")
				}
				x, err := evalExpr(call.Args[0], vars)
				if err != nil {
					return value{}, err
				}
				return value{kind: valFloat, f: math.Log(toFloat(x))}, nil
			}
		}
		if p.Target.Selector.Root == "strings" {
			call := p.Ops[0].Call
			name := p.Target.Selector.Tail[0]
			switch name {
			case "TrimSpace":
				if len(call.Args) != 1 {
					return value{}, fmt.Errorf("bad args")
				}
				x, err := evalExpr(call.Args[0], vars)
				if err != nil {
					return value{}, err
				}
				if x.kind != valString {
					return value{}, fmt.Errorf("bad args")
				}
				return value{kind: valString, s: strings.TrimSpace(x.s)}, nil
			case "ToUpper":
				if len(call.Args) != 1 {
					return value{}, fmt.Errorf("bad args")
				}
				x, err := evalExpr(call.Args[0], vars)
				if err != nil {
					return value{}, err
				}
				if x.kind != valString {
					return value{}, fmt.Errorf("bad args")
				}
				return value{kind: valString, s: strings.ToUpper(x.s)}, nil
			}
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
			if idxOp.Colon != nil || idxOp.End != nil {
				if idxOp.Colon2 != nil || idxOp.Step != nil {
					return value{}, fmt.Errorf("slicing not supported")
				}
				start := 0
				if idxOp.Start != nil {
					sv, err := evalExpr(idxOp.Start, vars)
					if err != nil {
						return value{}, err
					}
					if sv.kind != valInt {
						return value{}, fmt.Errorf("index must be int")
					}
					start = sv.i
				}
				end := 0
				switch v.kind {
				case valList:
					end = len(v.list)
				case valString:
					end = len([]rune(v.s))
				default:
					return value{}, fmt.Errorf("slicing non-container")
				}
				if idxOp.End != nil {
					ev, err := evalExpr(idxOp.End, vars)
					if err != nil {
						return value{}, err
					}
					if ev.kind != valInt {
						return value{}, fmt.Errorf("index must be int")
					}
					end = ev.i
				}
				if start < 0 {
					start = 0
				}
				if end < start {
					end = start
				}
				switch v.kind {
				case valList:
					if start > len(v.list) {
						start = len(v.list)
					}
					if end > len(v.list) {
						end = len(v.list)
					}
					slice := append([]value{}, v.list[start:end]...)
					v = value{kind: valList, list: slice}
				case valString:
					r := []rune(v.s)
					if start > len(r) {
						start = len(r)
					}
					if end > len(r) {
						end = len(r)
					}
					v = value{kind: valString, s: string(r[start:end])}
				}
			} else {
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
			case "float":
				switch v.kind {
				case valInt:
					v = value{kind: valFloat, f: float64(v.i)}
				case valFloat:
					// no-op
				default:
					return value{}, fmt.Errorf("bad cast")
				}
			default:
				if v.kind != valMap {
					return value{}, fmt.Errorf("bad cast")
				}
				v.typ = t
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
			} else if i+1 < len(p.Ops) && p.Ops[i+1].Call != nil {
				return value{}, fmt.Errorf("postfix not supported")
			} else {
				return value{}, fmt.Errorf("postfix not supported")
			}
		case op.Call != nil:
			args := make([]value, len(op.Call.Args))
			for i2, a := range op.Call.Args {
				av, err := evalExpr(a, vars)
				if err != nil {
					return value{}, err
				}
				args[i2] = av
			}
			if v.kind != valFunc {
				return value{}, fmt.Errorf("postfix call on non-function")
			}
			res, _, err := evalFunExpr(v.fun, args, v.env)
			if err != nil {
				return value{}, err
			}
			v = res
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
		if p.Lit.Float != nil {
			return value{kind: valFloat, f: *p.Lit.Float}, nil
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
		if p.Selector.Root == "testpkg" && len(p.Selector.Tail) == 1 {
			switch p.Selector.Tail[0] {
			case "Pi":
				return value{kind: valFloat, f: 3.14}, nil
			case "Answer":
				return value{kind: valInt, i: 42}, nil
			}
		}
		if pythonMathAliases[p.Selector.Root] && len(p.Selector.Tail) == 1 {
			switch p.Selector.Tail[0] {
			case "pi":
				return value{kind: valFloat, f: math.Pi}, nil
			case "e":
				return value{kind: valFloat, f: math.E}, nil
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
	case p.Struct != nil:
		m := make(map[string]value)
		for _, f := range p.Struct.Fields {
			v, err := evalExpr(f.Value, vars)
			if err != nil {
				return value{}, err
			}
			m[f.Name] = v
		}
		return value{kind: valMap, kv: m, typ: p.Struct.Name}, nil
	case p.Load != nil:
		return evalLoadExpr(p.Load)
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
	case p.FunExpr != nil:
		return value{kind: valFunc, fun: p.FunExpr, env: copyVars(vars)}, nil
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
				if items, ok := v.kv["items"]; ok && items.kind == valList {
					return value{kind: valInt, i: len(items.list)}, nil
				}
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
				intTotal := 0
				floatTotal := 0.0
				hasFloat := false
				for _, it := range listv.list {
					switch it.kind {
					case valInt:
						intTotal += it.i
					case valFloat:
						hasFloat = true
						floatTotal += it.f
					default:
						return value{}, fmt.Errorf("sum supports numeric list")
					}
				}
				if hasFloat {
					return value{kind: valFloat, f: floatTotal + float64(intTotal)}, nil
				}
				return value{kind: valInt, i: intTotal}, nil
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
		case "now":
			if len(p.Call.Args) != 0 {
				return value{}, fmt.Errorf("bad args")
			}
			return value{kind: valInt, i: int(time.Now().UnixNano())}, nil
		}
		if fv, ok := vars[p.Call.Func]; ok && fv.kind == valFunc {
			args := make([]value, len(p.Call.Args))
			for i, a := range p.Call.Args {
				av, err := evalExpr(a, vars)
				if err != nil {
					return value{}, err
				}
				args[i] = av
			}
			if fv.part != nil {
				all := append(append([]value{}, fv.part.args...), args...)
				res, _, err := evalFunction(fv.part.fn, all, fv.env)
				return res, err
			}
			res, _, err := evalFunExpr(fv.fun, args, fv.env)
			return res, err
		}
		if fn, ok := currentFuncs[p.Call.Func]; ok {
			args := make([]value, len(p.Call.Args))
			for i, a := range p.Call.Args {
				av, err := evalExpr(a, vars)
				if err != nil {
					return value{}, err
				}
				args[i] = av
			}
			res, _, err := evalFunction(fn, args, vars)
			return res, err
		}
		if currentEnv != nil {
			if st, ok := currentEnv.GetStruct(p.Call.Func); ok {
				if len(p.Call.Args) != len(st.Order) {
					return value{}, fmt.Errorf("bad args")
				}
				kv := make(map[string]value, len(st.Order))
				for i, arg := range p.Call.Args {
					v, err := evalExpr(arg, vars)
					if err != nil {
						return value{}, err
					}
					kv[st.Order[i]] = v
				}
				return value{kind: valMap, kv: kv, typ: p.Call.Func}, nil
			}
			if ut, ok := currentEnv.FindUnionByVariant(p.Call.Func); ok {
				st := ut.Variants[p.Call.Func]
				if len(p.Call.Args) != len(st.Order) {
					return value{}, fmt.Errorf("bad args")
				}
				kv := make(map[string]value, len(st.Order))
				for i, arg := range p.Call.Args {
					v, err := evalExpr(arg, vars)
					if err != nil {
						return value{}, err
					}
					kv[st.Order[i]] = v
				}
				return value{kind: valMap, kv: kv, typ: p.Call.Func}, nil
			}
		}
		return value{}, fmt.Errorf("unsupported call")
	case p.If != nil:
		return evalIfExpr(p.If, vars)
	case p.Match != nil:
		return evalMatchExpr(p.Match, vars)
	case p.Group != nil:
		return evalExpr(p.Group, vars)
	}
	return value{}, fmt.Errorf("unsupported primary")
}

func evalMatchExpr(m *parser.MatchExpr, vars map[string]value) (value, error) {
	target, err := evalExpr(m.Target, vars)
	if err != nil {
		return value{}, err
	}
	for _, c := range m.Cases {
		if wild, ok := identName(c.Pattern); ok {
			if wild == "_" {
				return evalExpr(c.Result, vars)
			}
			if currentEnv != nil {
				if st, ok := currentEnv.GetStruct(wild); ok {
					if target.typ == wild {
						if len(st.Order) == 0 {
							return evalExpr(c.Result, vars)
						}
					}
				}
				if ut, ok := currentEnv.FindUnionByVariant(wild); ok {
					st := ut.Variants[wild]
					if target.typ == wild && len(st.Order) == 0 {
						return evalExpr(c.Result, vars)
					}
				}
			}
		}
		if call, ok := callPattern(c.Pattern); ok && currentEnv != nil {
			if st, ok := currentEnv.GetStruct(call.Func); ok {
				if target.typ == call.Func && len(call.Args) == len(st.Order) {
					local := copyVars(vars)
					for i, arg := range call.Args {
						if name, ok := identName(arg); ok {
							local[name] = target.kv[st.Order[i]]
						}
					}
					return evalExpr(c.Result, local)
				}
			}
			if ut, ok := currentEnv.FindUnionByVariant(call.Func); ok {
				st := ut.Variants[call.Func]
				if target.typ == call.Func && len(call.Args) == len(st.Order) {
					local := copyVars(vars)
					for i, arg := range call.Args {
						if name, ok := identName(arg); ok {
							local[name] = target.kv[st.Order[i]]
						}
					}
					return evalExpr(c.Result, local)
				}
			}
		}
		pv, err := evalExpr(c.Pattern, vars)
		if err != nil {
			return value{}, err
		}
		if compareValues(target, pv) == 0 {
			return evalExpr(c.Result, vars)
		}
	}
	return value{}, fmt.Errorf("no match")
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

// evalFunction evaluates a user-defined function with the provided argument
// values. Only very simple functions containing a single return statement are
// supported as this transpiler performs aggressive constant folding.
// evalFunction evaluates a user-defined function using constant folding. It
// now supports a mix of local function declarations, simple variable
// assignments and a final return statement.
func evalFunction(fn *parser.FunStmt, args []value, captured map[string]value) (value, []value, error) {
	if fn == nil {
		return value{}, nil, fmt.Errorf("undefined function")
	}
	if len(args) < len(fn.Params) {
		cp := append([]value(nil), args...)
		return value{kind: valFunc, part: &partial{fn: fn, args: cp}, env: copyVars(captured)}, nil, nil
	}
	if len(args) > len(fn.Params) {
		return value{}, nil, fmt.Errorf("argument count mismatch")
	}

	// copy current functions so that nested functions are visible during
	// evaluation
	saved := currentFuncs
	locals := make(map[string]*parser.FunStmt, len(saved))
	for k, v := range saved {
		locals[k] = v
	}
	currentFuncs = locals
	defer func() { currentFuncs = saved }()

	vars := copyVars(captured)
	for i, p := range fn.Params {
		vars[p.Name] = args[i]
	}

	var ret *parser.Expr

	var process func(*parser.Statement) error
	process = func(st *parser.Statement) error {
		switch {
		case st.Fun != nil:
			locals[st.Fun.Name] = st.Fun
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
		case st.Assign != nil:
			if len(st.Assign.Index) > 0 {
				target, ok := vars[st.Assign.Name]
				if !ok {
					return fmt.Errorf("assign to unknown var")
				}

				idxVals := make([]value, len(st.Assign.Index))
				for i, idx := range st.Assign.Index {
					if idx.Colon != nil || idx.End != nil || idx.Colon2 != nil || idx.Step != nil {
						return fmt.Errorf("slicing not supported")
					}
					iv, err := evalExpr(idx.Start, vars)
					if err != nil {
						return err
					}
					idxVals[i] = iv
				}

				val, err := evalExpr(st.Assign.Value, vars)
				if err != nil {
					return err
				}

				newTarget, err := setIndexedValue(target, idxVals, val)
				if err != nil {
					return err
				}
				vars[st.Assign.Name] = newTarget
				break
			}

			v, err := evalExpr(st.Assign.Value, vars)
			if err != nil {
				return err
			}
			if len(st.Assign.Field) > 0 {
				target, ok := vars[st.Assign.Name]
				if !ok {
					return fmt.Errorf("assign to unknown var")
				}
				names := make([]string, len(st.Assign.Field))
				for i, f := range st.Assign.Field {
					names[i] = f.Name
				}
				nt, err := setFieldValue(target, names, v)
				if err != nil {
					return err
				}
				vars[st.Assign.Name] = nt
			} else {
				vars[st.Assign.Name] = v
			}
		case st.If != nil:
			cond, err := evalExpr(st.If.Cond, vars)
			if err != nil {
				return err
			}
			if isTruthy(cond) {
				for _, s := range st.If.Then {
					if err := process(s); err != nil {
						return err
					}
				}
				return nil
			}
			if st.If.ElseIf != nil {
				return process(&parser.Statement{If: st.If.ElseIf})
			}
			for _, s := range st.If.Else {
				if err := process(s); err != nil {
					return err
				}
			}
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
					if err := process(b); err != nil {
						switch err.(type) {
						case breakErr:
							return nil
						case continueErr:
							goto contWhile
						default:
							return err
						}
					}
				}
			contWhile:
				continue
			}
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
					if err := process(b); err != nil {
						switch err.(type) {
						case breakErr:
							goto endFor
						case continueErr:
							goto contFor
						default:
							delete(vars, st.For.Name)
							return err
						}
					}
				}
			contFor:
				continue
			}
		endFor:
			delete(vars, st.For.Name)
		case st.Break != nil:
			return breakErr{}
		case st.Continue != nil:
			return continueErr{}
		case st.Expr != nil:
			call := st.Expr.Expr.Binary.Left.Value.Target.Call
			if call == nil {
				return fmt.Errorf("unsupported expression")
			}
			if fn, ok := locals[call.Func]; ok {
				args := make([]value, len(call.Args))
				for i, a := range call.Args {
					av, err := evalExpr(a, vars)
					if err != nil {
						return err
					}
					args[i] = av
				}
				res, newArgs, err := evalFunction(fn, args, vars)
				if err != nil {
					return err
				}
				// update by-value params
				for i, p := range fn.Params {
					vars[p.Name] = newArgs[i]
				}
				_ = res
				return nil
			}
			switch call.Func {
			case "print":
				if len(call.Args) == 0 {
					return fmt.Errorf("print with no args")
				}
				// ignore prints inside functions
				return nil
			default:
				return fmt.Errorf("unsupported expression")
			}
		case st.Return != nil:
			ret = st.Return.Value
			return nil
		case st.Test != nil:
			for _, b := range st.Test.Body {
				if err := process(b); err != nil {
					return err
				}
			}
		case st.Expect != nil:
			v, err := evalExpr(st.Expect.Value, vars)
			if err != nil {
				return err
			}
			if v.kind != valBool || !v.b {
				return fmt.Errorf("expect failed")
			}
		default:
			return fmt.Errorf("unsupported function body")
		}
		return nil
	}

	for _, st := range fn.Body {
		if err := process(st); err != nil {
			return value{}, nil, err
		}
		if ret != nil {
			break
		}
	}
	var out value
	var err error
	if ret != nil {
		out, err = evalExpr(ret, vars)
		if err != nil {
			return value{}, nil, err
		}
	}
	newArgs := make([]value, len(fn.Params))
	for i, p := range fn.Params {
		newArgs[i] = vars[p.Name]
	}
	return out, newArgs, nil
}

func evalFunExpr(fe *parser.FunExpr, args []value, captured map[string]value) (value, []value, error) {
	stmt := &parser.FunStmt{Params: fe.Params, Return: fe.Return}
	if fe.ExprBody != nil {
		stmt.Body = []*parser.Statement{{Return: &parser.ReturnStmt{Value: fe.ExprBody}}}
	} else {
		stmt.Body = fe.BlockBody
	}
	return evalFunction(stmt, args, captured)
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

func literalString(e *parser.Expr) (string, bool) {
	if e == nil || e.Binary == nil || len(e.Binary.Right) > 0 {
		return "", false
	}
	u := e.Binary.Left
	if len(u.Ops) > 0 || u.Value == nil {
		return "", false
	}
	p := u.Value
	if len(p.Ops) > 0 || p.Target == nil {
		return "", false
	}
	if p.Target.Lit != nil && p.Target.Lit.Str != nil {
		return *p.Target.Lit.Str, true
	}
	if p.Target.Selector != nil && len(p.Target.Selector.Tail) == 0 {
		return p.Target.Selector.Root, true
	}
	return "", false
}

func callPattern(e *parser.Expr) (*parser.CallExpr, bool) {
	if e == nil || e.Binary == nil || len(e.Binary.Right) != 0 {
		return nil, false
	}
	u := e.Binary.Left
	if len(u.Ops) != 0 || u.Value == nil {
		return nil, false
	}
	p := u.Value
	if len(p.Ops) != 0 || p.Target.Call == nil {
		return nil, false
	}
	return p.Target.Call, true
}

func callName(e *parser.Expr) (string, []*parser.Expr, bool) {
	if e == nil || e.Binary == nil || len(e.Binary.Right) != 0 {
		return "", nil, false
	}
	u := e.Binary.Left
	if len(u.Ops) != 0 || u.Value == nil || u.Value.Target == nil || u.Value.Target.Call == nil {
		return "", nil, false
	}
	c := u.Value.Target.Call
	return c.Func, c.Args, true
}

func evalQueryExpr(q *parser.QueryExpr, vars map[string]value) (value, error) {
	// handle simple aggregation without grouping
	if q.Group == nil && len(q.Joins) == 0 && len(q.Froms) == 0 && q.Sort == nil && q.Skip == nil && q.Take == nil && !q.Distinct {
		if name, args, ok := callName(q.Select); ok && len(args) == 1 {
			src, err := evalExpr(q.Source, vars)
			if err != nil {
				return value{}, err
			}
			if src.kind != valList {
				return value{}, fmt.Errorf("query source must be list")
			}
			var vals []value
			for _, it := range src.list {
				local := copyVars(vars)
				local[q.Var] = it
				if q.Where != nil {
					cond, err := evalExpr(q.Where, local)
					if err != nil {
						return value{}, err
					}
					if cond.kind != valBool || !cond.b {
						continue
					}
				}
				v, err := evalExpr(args[0], local)
				if err != nil {
					return value{}, err
				}
				vals = append(vals, v)
			}
			switch name {
			case "sum":
				intTotal := 0
				floatTotal := 0.0
				hasFloat := false
				for _, v := range vals {
					switch v.kind {
					case valInt:
						intTotal += v.i
					case valFloat:
						hasFloat = true
						floatTotal += v.f
					default:
						return value{}, fmt.Errorf("sum supports numeric list")
					}
				}
				if hasFloat {
					return value{kind: valFloat, f: floatTotal + float64(intTotal)}, nil
				}
				return value{kind: valInt, i: intTotal}, nil
			case "count":
				return value{kind: valInt, i: len(vals)}, nil
			case "avg":
				if len(vals) == 0 {
					return value{kind: valFloat, f: 0}, nil
				}
				total := 0
				for _, v := range vals {
					if v.kind != valInt {
						return value{}, fmt.Errorf("avg supports int list")
					}
					total += v.i
				}
				return value{kind: valFloat, f: float64(total) / float64(len(vals))}, nil
			}
		}
	}

	// handle group by form: FROM <src> GROUP BY <expr> INTO g SELECT ...
	if q.Group != nil && len(q.Group.Exprs) == 1 && len(q.Froms) == 0 && len(q.Joins) == 0 && q.Skip == nil && q.Take == nil && !q.Distinct {
		src, err := evalExpr(q.Source, vars)
		if err != nil {
			return value{}, err
		}
		if src.kind != valList {
			return value{}, fmt.Errorf("group-by source must be list")
		}

		type grp struct {
			key   value
			items []value
		}
		groups := map[string]*grp{}
		for _, it := range src.list {
			local := copyVars(vars)
			local[q.Var] = it
			kVal, err := evalExpr(q.Group.Exprs[0], local)
			if err != nil {
				return value{}, err
			}
			kStr, err := keyString(kVal)
			if err != nil {
				return value{}, err
			}
			g, ok := groups[kStr]
			if !ok {
				g = &grp{key: kVal}
				groups[kStr] = g
			}
			g.items = append(g.items, it)
		}

		keys := make([]string, 0, len(groups))
		for k := range groups {
			keys = append(keys, k)
		}
		sort.Strings(keys)

		type pair struct{ key, val value }
		var pairs []pair
		for _, k := range keys {
			g := groups[k]
			gv := value{kind: valMap, kv: map[string]value{
				"key":   g.key,
				"items": {kind: valList, list: g.items},
			}}
			local := copyVars(vars)
			local[q.Group.Name] = gv
			if q.Group.Having != nil {
				cond, err := evalExpr(q.Group.Having, local)
				if err != nil {
					return value{}, err
				}
				if cond.kind != valBool || !cond.b {
					continue
				}
			}
			v, err := evalExpr(q.Select, local)
			if err != nil {
				return value{}, err
			}
			k2 := value{}
			if q.Sort != nil {
				k2, err = evalExpr(q.Sort, local)
				if err != nil {
					return value{}, err
				}
			}
			pairs = append(pairs, pair{key: k2, val: v})
		}
		if q.Sort != nil {
			sort.Slice(pairs, func(i, j int) bool {
				return compareValues(pairs[i].key, pairs[j].key) < 0
			})
		}
		results := make([]value, len(pairs))
		for i, p := range pairs {
			results[i] = p.val
		}
		return value{kind: valList, list: results}, nil
	}

	// handle join with group by: FROM <src> JOIN ... GROUP BY <expr>
	if q.Group != nil && len(q.Group.Exprs) == 1 && len(q.Joins) == 1 && len(q.Froms) == 0 && q.Skip == nil && q.Take == nil && !q.Distinct {
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

		type grp struct {
			key   value
			items []value
		}
		groups := map[string]*grp{}
		for _, l := range left.list {
			for _, r := range right.list {
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
				if !cond.b {
					continue
				}
				kVal, err := evalExpr(q.Group.Exprs[0], local)
				if err != nil {
					return value{}, err
				}
				kStr, err := keyString(kVal)
				if err != nil {
					return value{}, err
				}
				g, ok := groups[kStr]
				if !ok {
					g = &grp{key: kVal}
					groups[kStr] = g
				}
				item := value{kind: valMap, kv: map[string]value{
					q.Var: l,
					j.Var: r,
				}}
				g.items = append(g.items, item)
			}
		}

		keys := make([]string, 0, len(groups))
		for k := range groups {
			keys = append(keys, k)
		}
		sort.Strings(keys)

		type pair struct{ key, val value }
		var pairs []pair
		for _, k := range keys {
			g := groups[k]
			gv := value{kind: valMap, kv: map[string]value{
				"key":   g.key,
				"items": {kind: valList, list: g.items},
			}}
			local := copyVars(vars)
			local[q.Group.Name] = gv
			if q.Group.Having != nil {
				cond, err := evalExpr(q.Group.Having, local)
				if err != nil {
					return value{}, err
				}
				if cond.kind != valBool || !cond.b {
					continue
				}
			}
			v, err := evalExpr(q.Select, local)
			if err != nil {
				return value{}, err
			}
			k2 := value{}
			if q.Sort != nil {
				k2, err = evalExpr(q.Sort, local)
				if err != nil {
					return value{}, err
				}
			}
			pairs = append(pairs, pair{key: k2, val: v})
		}
		if q.Sort != nil {
			sort.Slice(pairs, func(i, j int) bool {
				return compareValues(pairs[i].key, pairs[j].key) < 0
			})
		}
		results := make([]value, len(pairs))
		for i, p := range pairs {
			results[i] = p.val
		}
		return value{kind: valList, list: results}, nil
	}

	// handle queries with multiple joins (inner join only)
	if len(q.Joins) > 1 && len(q.Froms) == 0 && q.Skip == nil && q.Take == nil && !q.Distinct {
		type clause struct {
			name string
			src  *parser.Expr
			on   *parser.Expr
		}
		clauses := []clause{{q.Var, q.Source, nil}}
		for _, j := range q.Joins {
			clauses = append(clauses, clause{j.Var, j.Src, j.On})
		}

		type pair struct{ key, val value }

		if q.Group != nil && len(q.Group.Exprs) == 1 {
			type grp struct {
				key   value
				items []value
			}
			groups := map[string]*grp{}
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
					item := value{kind: valMap, kv: map[string]value{}}
					for k, v := range local {
						item.kv[k] = v
					}
					kVal, err := evalExpr(q.Group.Exprs[0], local)
					if err != nil {
						return err
					}
					kStr, err := keyString(kVal)
					if err != nil {
						return err
					}
					g, ok := groups[kStr]
					if !ok {
						g = &grp{key: kVal}
						groups[kStr] = g
					}
					g.items = append(g.items, item)
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
					if cl.on != nil {
						cond, err := evalExpr(cl.on, next)
						if err != nil {
							return err
						}
						if cond.kind != valBool {
							return fmt.Errorf("non-bool join condition")
						}
						if !cond.b {
							continue
						}
					}
					if err := iter(i+1, next); err != nil {
						return err
					}
				}
				return nil
			}
			if err := iter(0, copyVars(vars)); err != nil {
				return value{}, err
			}

			keys := make([]string, 0, len(groups))
			for k := range groups {
				keys = append(keys, k)
			}
			sort.Strings(keys)

			var pairs []pair
			for _, k := range keys {
				g := groups[k]
				gv := value{kind: valMap, kv: map[string]value{
					"key":   g.key,
					"items": {kind: valList, list: g.items},
				}}
				local := copyVars(vars)
				local[q.Group.Name] = gv
				if q.Group.Having != nil {
					cond, err := evalExpr(q.Group.Having, local)
					if err != nil {
						return value{}, err
					}
					if cond.kind != valBool || !cond.b {
						continue
					}
				}
				v, err := evalExpr(q.Select, local)
				if err != nil {
					return value{}, err
				}
				k2 := value{}
				if q.Sort != nil {
					k2, err = evalExpr(q.Sort, local)
					if err != nil {
						return value{}, err
					}
				}
				pairs = append(pairs, pair{key: k2, val: v})
			}
			if q.Sort != nil {
				sort.Slice(pairs, func(i, j int) bool {
					return compareValues(pairs[i].key, pairs[j].key) < 0
				})
			}
			results := make([]value, len(pairs))
			for i, p := range pairs {
				results[i] = p.val
			}
			return value{kind: valList, list: results}, nil
		}

		var pairs []pair
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
				k := value{}
				if q.Sort != nil {
					k, err = evalExpr(q.Sort, local)
					if err != nil {
						return err
					}
				}
				pairs = append(pairs, pair{key: k, val: v})
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
				if cl.on != nil {
					cond, err := evalExpr(cl.on, next)
					if err != nil {
						return err
					}
					if cond.kind != valBool {
						return fmt.Errorf("non-bool join condition")
					}
					if !cond.b {
						continue
					}
				}
				if err := iter(i+1, next); err != nil {
					return err
				}
			}
			return nil
		}
		if err := iter(0, copyVars(vars)); err != nil {
			return value{}, err
		}
		if q.Sort != nil {
			sort.Slice(pairs, func(i, j int) bool {
				return compareValues(pairs[i].key, pairs[j].key) < 0
			})
		}
		results := make([]value, len(pairs))
		for i, p := range pairs {
			results[i] = p.val
		}
		return value{kind: valList, list: results}, nil
	}

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

	type pair struct{ key, val value }
	var pairs []pair
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
			k := value{}
			if q.Sort != nil {
				k, err = evalExpr(q.Sort, local)
				if err != nil {
					return err
				}
			}
			pairs = append(pairs, pair{key: k, val: v})
			return nil
		}
		cl := clauses[i]
		listv, err := evalExpr(cl.src, local)
		if err != nil {
			return err
		}
		if listv.kind == valMap {
			if it, ok := listv.kv["items"]; ok && it.kind == valList {
				listv = it
			}
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
	if q.Sort != nil {
		sort.Slice(pairs, func(i, j int) bool {
			return compareValues(pairs[i].key, pairs[j].key) < 0
		})
	}
	results := make([]value, len(pairs))
	for i, p := range pairs {
		results[i] = p.val
	}
	return value{kind: valList, list: results}, nil
}

// Transpile converts a Mochi program into our Smalltalk AST.
func Transpile(prog *parser.Program, env *types.Env) (*Program, error) {
	vars := map[string]value{}
	funcs := map[string]*parser.FunStmt{}
	currentFuncs = funcs
	currentEnv = env
	pythonMathAliases = map[string]bool{}
	defer func() {
		currentFuncs = nil
		currentEnv = nil
		pythonMathAliases = nil
	}()
	p := &Program{}

	var processStmt func(*parser.Statement) error
	processStmt = func(st *parser.Statement) error {
		switch {
		case st.Import != nil:
			if st.Import.Lang != nil && *st.Import.Lang == "python" && strings.Trim(st.Import.Path, "\"") == "math" {
				alias := st.Import.As
				if alias == "" {
					alias = parser.AliasFromPath(st.Import.Path)
				}
				pythonMathAliases[alias] = true
				return nil
			}
			// other imports are ignored
			return nil
		case st.ExternVar != nil, st.ExternFun != nil, st.ExternType != nil, st.ExternObject != nil:
			// extern declarations are ignored
			return nil
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
		case st.Fun != nil:
			funcs[st.Fun.Name] = st.Fun
			return nil
		case st.Type != nil:
			return nil
		case st.Test != nil:
			// tests are ignored when transpiling
			return nil
		case st.Expect != nil:
			// expects are ignored when transpiling
			return nil
		case st.Assign != nil:
			if len(st.Assign.Index) > 0 {
				target, ok := vars[st.Assign.Name]
				if !ok {
					return fmt.Errorf("assign to unknown var")
				}

				idxVals := make([]value, len(st.Assign.Index))
				for i, idx := range st.Assign.Index {
					if idx.Colon != nil || idx.End != nil || idx.Colon2 != nil || idx.Step != nil {
						return fmt.Errorf("slicing not supported")
					}
					iv, err := evalExpr(idx.Start, vars)
					if err != nil {
						return err
					}
					idxVals[i] = iv
				}

				val, err := evalExpr(st.Assign.Value, vars)
				if err != nil {
					return err
				}

				newTarget, err := setIndexedValue(target, idxVals, val)
				if err != nil {
					return err
				}
				vars[st.Assign.Name] = newTarget

				line := st.Assign.Name
				cur := target
				for i, iv := range idxVals {
					switch cur.kind {
					case valList:
						if iv.kind != valInt {
							return fmt.Errorf("index must be int")
						}
						line += fmt.Sprintf(" at:%d", iv.i+1)
						if i < len(idxVals)-1 && iv.i >= 0 && iv.i < len(cur.list) {
							cur = cur.list[iv.i]
						}
					case valMap:
						key, err := keyString(iv)
						if err != nil {
							return err
						}
						if iv.kind == valString {
							line += fmt.Sprintf(" at:'%s'", escape(iv.s))
						} else {
							line += fmt.Sprintf(" at:%s", key)
						}
						if i < len(idxVals)-1 {
							if child, ok := cur.kv[key]; ok {
								cur = child
							}
						}
					default:
						return fmt.Errorf("index assign only for list or map")
					}
				}
				line += fmt.Sprintf(" put:%s.", val)
				p.Lines = append(p.Lines, line)
			} else {
				v, err := evalExpr(st.Assign.Value, vars)
				if err != nil {
					return err
				}
				if _, ok := vars[st.Assign.Name]; !ok {
					return fmt.Errorf("assign to unknown var")
				}
				if len(st.Assign.Field) > 0 {
					target := vars[st.Assign.Name]
					names := make([]string, len(st.Assign.Field))
					for i, f := range st.Assign.Field {
						names[i] = f.Name
					}
					nt, err := setFieldValue(target, names, v)
					if err != nil {
						return err
					}
					vars[st.Assign.Name] = nt
				} else {
					vars[st.Assign.Name] = v
				}
				appendAssign(&p.Lines, st.Assign.Name, vars[st.Assign.Name])
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
				cont := false
				for _, b := range st.While.Body {
					if err := processStmt(b); err != nil {
						switch err.(type) {
						case breakErr:
							return nil
						case continueErr:
							cont = true
							break
						default:
							return err
						}
					}
				}
				if cont {
					continue
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
				cont := false
				for _, b := range st.For.Body {
					if err := processStmt(b); err != nil {
						switch err.(type) {
						case breakErr:
							delete(vars, st.For.Name)
							return nil
						case continueErr:
							cont = true
							break
						default:
							delete(vars, st.For.Name)
							return err
						}
					}
				}
				if cont {
					continue
				}
			}
			delete(vars, st.For.Name)
			return nil
		case st.Break != nil:
			return breakErr{}
		case st.Continue != nil:
			return continueErr{}
		case st.Update != nil:
			target, ok := vars[st.Update.Target]
			if !ok {
				return fmt.Errorf("unknown update target")
			}
			if target.kind != valList {
				return fmt.Errorf("update target not list")
			}
			newList := make([]value, len(target.list))
			for i, elem := range target.list {
				if elem.kind != valMap {
					return fmt.Errorf("update element not struct")
				}
				local := copyVars(vars)
				for k, v := range elem.kv {
					local[k] = v
				}
				apply := true
				if st.Update.Where != nil {
					cond, err := evalExpr(st.Update.Where, local)
					if err != nil {
						return err
					}
					apply = isTruthy(cond)
				}
				newElem := elem
				if apply {
					if newElem.kv == nil {
						newElem.kv = map[string]value{}
					}
					for _, it := range st.Update.Set.Items {
						name, ok := identName(it.Key)
						if !ok {
							name, ok = literalString(it.Key)
							if !ok {
								return fmt.Errorf("unsupported update key")
							}
						}
						val, err := evalExpr(it.Value, local)
						if err != nil {
							return err
						}
						newElem.kv[name] = val
						local[name] = val
					}
				}
				newList[i] = newElem
			}
			target.list = newList
			vars[st.Update.Target] = target
			appendAssign(&p.Lines, st.Update.Target, target)
			return nil
		case st.Expr != nil:
			call := st.Expr.Expr.Binary.Left.Value.Target.Call
			if call == nil {
				return fmt.Errorf("unsupported expression")
			}
			if fn, ok := funcs[call.Func]; ok {
				args := make([]value, len(call.Args))
				argNames := make([]string, len(call.Args))
				for i, a := range call.Args {
					av, err := evalExpr(a, vars)
					if err != nil {
						return err
					}
					args[i] = av
					if n, ok := identName(a); ok {
						argNames[i] = n
					}
				}
				_, newArgs, err := evalFunction(fn, args, vars)
				if err != nil {
					return err
				}
				for i, n := range argNames {
					if n != "" {
						vars[n] = newArgs[i]
					}
				}
				break
			}
			switch call.Func {
			case "print":
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
			case "json":
				if len(call.Args) != 1 {
					return fmt.Errorf("json expects one arg")
				}
				v, err := evalExpr(call.Args[0], vars)
				if err != nil {
					return err
				}
				line := "Transcript show:" + fmt.Sprintf("'%s'", escape(jsonString(v))) + "; cr"
				p.Lines = append(p.Lines, line)
			default:
				return fmt.Errorf("unsupported expression")
			}
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

func parseFormatExpr(e *parser.Expr) string {
	if e == nil {
		return ""
	}
	v, err := evalExpr(e, map[string]value{})
	if err != nil || v.kind != valMap {
		return ""
	}
	if f, ok := v.kv["format"]; ok && f.kind == valString {
		return f.s
	}
	return ""
}

func anyToValue(x interface{}, typ string) value {
	switch v := x.(type) {
	case map[string]interface{}:
		kv := make(map[string]value, len(v))
		for k, val := range v {
			kv[k] = anyToValue(val, "")
		}
		return value{kind: valMap, kv: kv, typ: typ}
	case []interface{}:
		list := make([]value, len(v))
		for i, it := range v {
			list[i] = anyToValue(it, "")
		}
		return value{kind: valList, list: list}
	case string:
		return value{kind: valString, s: v}
	case int, int64:
		return value{kind: valInt, i: int(reflect.ValueOf(v).Int())}
	case float64, float32:
		return value{kind: valFloat, f: reflect.ValueOf(v).Float()}
	case bool:
		return value{kind: valBool, b: v}
	default:
		return value{}
	}
}

func evalLoadExpr(l *parser.LoadExpr) (value, error) {
	path := ""
	if l.Path != nil {
		path = *l.Path
	}
	if strings.HasPrefix(path, "../") {
		clean := strings.TrimPrefix(path, "../")
		root := repoRoot()
		if root != "" {
			path = filepath.Join(root, "tests", clean)
		}
	}
	format := parseFormatExpr(l.With)
	if format == "" {
		if strings.HasSuffix(path, ".yaml") {
			format = "yaml"
		} else if strings.HasSuffix(path, ".jsonl") {
			format = "jsonl"
		} else if strings.HasSuffix(path, ".json") {
			format = "json"
		}
	}
	if path == "" {
		return value{kind: valList}, nil
	}
	data, err := os.ReadFile(path)
	if err != nil {
		return value{}, err
	}
	switch format {
	case "jsonl":
		lines := bytes.Split(data, []byte{'\n'})
		var list []value
		for _, line := range lines {
			line = bytes.TrimSpace(line)
			if len(line) == 0 {
				continue
			}
			var m map[string]interface{}
			if err := json.Unmarshal(line, &m); err != nil {
				return value{}, err
			}
			list = append(list, anyToValue(m, ""))
		}
		if l.Type != nil && l.Type.Simple != nil {
			for i := range list {
				list[i].typ = *l.Type.Simple
			}
		}
		return value{kind: valList, list: list}, nil
	case "yaml":
		var arr []map[string]interface{}
		if err := yaml.Unmarshal(data, &arr); err != nil {
			return value{}, err
		}
		list := make([]value, len(arr))
		for i, obj := range arr {
			list[i] = anyToValue(obj, "")
			if l.Type != nil && l.Type.Simple != nil {
				list[i].typ = *l.Type.Simple
			}
		}
		return value{kind: valList, list: list}, nil
	case "json":
		var arr []map[string]interface{}
		if err := json.Unmarshal(data, &arr); err != nil {
			return value{}, err
		}
		list := make([]value, len(arr))
		for i, obj := range arr {
			list[i] = anyToValue(obj, "")
			if l.Type != nil && l.Type.Simple != nil {
				list[i].typ = *l.Type.Simple
			}
		}
		return value{kind: valList, list: list}, nil
	default:
		return value{}, fmt.Errorf("unsupported load format")
	}
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
