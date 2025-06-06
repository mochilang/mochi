package interpreter

import (
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"github.com/alecthomas/participle/v2/lexer"
	"github.com/fatih/color"
	"mochi/parser"
	"mochi/runtime/llm"
	"mochi/types"
	"os"
	"strings"
	"time"
)

// ValueTag represents the type of a runtime value.
type ValueTag uint8

const (
	TagNull ValueTag = iota
	TagInt
	TagFloat
	TagStr
	TagBool
	TagList
	TagMap
	TagFunc
)

func (t ValueTag) String() string {
	switch t {
	case TagNull:
		return "null"
	case TagInt:
		return "int"
	case TagFloat:
		return "float"
	case TagStr:
		return "string"
	case TagBool:
		return "bool"
	case TagList:
		return "list"
	case TagMap:
		return "map"
	case TagFunc:
		return "func"
	default:
		return "unknown"
	}
}

// Value is a tagged union used at runtime to avoid reflection.
type Value struct {
	Tag   ValueTag
	Int   int
	Float float64
	Str   string
	Bool  bool
	List  []Value
	Map   map[string]Value
	Func  *closure
}

// Truthy returns the boolean interpretation of v.
func (v Value) Truthy() bool {
	switch v.Tag {
	case TagBool:
		return v.Bool
	case TagInt:
		return v.Int != 0
	case TagFloat:
		return v.Float != 0
	case TagStr:
		return v.Str != ""
	case TagList:
		return len(v.List) > 0
	case TagMap:
		return len(v.Map) > 0
	default:
		return false
	}
}

func anyToValue(v any) Value {
	switch val := v.(type) {
	case nil:
		return Value{Tag: TagNull}
	case int:
		return Value{Tag: TagInt, Int: val}
	case int64:
		return Value{Tag: TagInt, Int: int(val)}
	case float64:
		return Value{Tag: TagFloat, Float: val}
	case string:
		return Value{Tag: TagStr, Str: val}
	case bool:
		return Value{Tag: TagBool, Bool: val}
	case []Value:
		return Value{Tag: TagList, List: val}
	case []any:
		list := make([]Value, len(val))
		for i, x := range val {
			list[i] = anyToValue(x)
		}
		return Value{Tag: TagList, List: list}
	case map[string]Value:
		return Value{Tag: TagMap, Map: val}
	case map[string]any:
		m := make(map[string]Value, len(val))
		for k, x := range val {
			m[k] = anyToValue(x)
		}
		return Value{Tag: TagMap, Map: m}
	case closure:
		return Value{Tag: TagFunc, Func: &val}
	case *closure:
		return Value{Tag: TagFunc, Func: val}
	default:
		return Value{Tag: TagNull}
	}
}

func valueToAny(v Value) any {
	switch v.Tag {
	case TagInt:
		return v.Int
	case TagFloat:
		return v.Float
	case TagStr:
		return v.Str
	case TagBool:
		return v.Bool
	case TagList:
		list := make([]any, len(v.List))
		for i, x := range v.List {
			list[i] = valueToAny(x)
		}
		return list
	case TagMap:
		m := make(map[string]any, len(v.Map))
		for k, x := range v.Map {
			m[k] = valueToAny(x)
		}
		return m
	case TagFunc:
		return v.Func
	default:
		return nil
	}
}

// Interpreter executes Mochi programs using a shared runtime and type environment.
type Interpreter struct {
	prog  *parser.Program
	env   *types.Env
	types *types.Env
}

func New(prog *parser.Program, typesEnv *types.Env) *Interpreter {
	return &Interpreter{
		prog: prog,
		// env:   types.NewEnv(nil),
		env:   typesEnv,
		types: typesEnv,
	}
}

func (i *Interpreter) SetProgram(prog *parser.Program) {
	i.prog = prog
}

func (i *Interpreter) Env() *types.Env { return i.env }

var (
	cTest = color.New(color.FgYellow).SprintFunc()
	cOK   = color.New(color.FgGreen).SprintFunc()
	cFail = color.New(color.FgRed).SprintFunc()
)

func printTestStart(name string) {
	fmt.Printf("   %s %-30s ...", cTest("test"), name)
}

func printTestPass(duration time.Duration) {
	fmt.Printf(" %s (%s)\n", cOK("ok"), formatDuration(duration))
}

func printTestFail(err error, duration time.Duration) {
	fmt.Printf(" %s %v (%s)\n", cFail("fail"), err, formatDuration(duration))
}

func formatDuration(d time.Duration) string {
	switch {
	case d < time.Microsecond:
		return fmt.Sprintf("%dns", d.Nanoseconds())
	case d < time.Millisecond:
		return fmt.Sprintf("%.1fµs", float64(d.Microseconds()))
	case d < time.Second:
		return fmt.Sprintf("%.1fms", float64(d.Milliseconds()))
	default:
		return fmt.Sprintf("%.2fs", d.Seconds())
	}
}

func (i *Interpreter) Run() error {
	/*
		// Load all shared definitions (let, fun) first
		for _, stmt := range i.prog.Statements {
			if stmt.Let != nil || stmt.Fun != nil {
				if err := i.evalStmt(stmt); err != nil {
					return err
				}
			}
		}
	*/

	// Run only non-test, non-decl statements (Expr, If, For, Return)
	for _, stmt := range i.prog.Statements {
		if stmt.Test != nil {
			continue
		}
		if err := i.evalStmt(stmt); err != nil {
			return err
		}
	}

	return nil
}

func (i *Interpreter) Test() error {
	var failures []error

	// Preload all shared declarations (let, fun)
	for _, stmt := range i.prog.Statements {
		if stmt.Let != nil || stmt.Fun != nil {
			if err := i.evalStmt(stmt); err != nil {
				return err
			}
		}
	}

	// Run each test block independently
	for _, stmt := range i.prog.Statements {
		if stmt.Test == nil {
			continue
		}

		name := stmt.Test.Name
		printTestStart(name)

		child := types.NewEnv(i.env)
		interp := &Interpreter{
			prog:  i.prog,
			env:   child,
			types: i.types,
		}

		start := time.Now()
		var failed error
		for _, s := range stmt.Test.Body {
			if err := interp.evalStmt(s); err != nil {
				failed = err
				break
			}
		}
		duration := time.Since(start)

		if failed != nil {
			printTestFail(failed, duration)
			failures = append(failures, failed)
		} else {
			printTestPass(duration)
		}
	}

	if len(failures) > 0 {
		fmt.Fprintf(os.Stderr, "\n%s %d test(s) failed.\n", cFail("[FAIL]"), len(failures))
		return fmt.Errorf("test failed: %d test(s) failed", len(failures))
	}

	return nil
}

type closure struct {
	Name       string
	Fn         *parser.FunExpr
	Env        *types.Env
	Args       []Value
	FullParams []*parser.Param
}

func (i *Interpreter) evalSelector(sel *parser.SelectorExpr) (any, error) {
	val, err := i.env.GetValue(sel.Root)
	if err != nil {
		if fn, ok := i.env.GetFunc(sel.Root); ok {
			cl := closure{Name: sel.Root, Fn: &parser.FunExpr{Params: fn.Params, Return: fn.Return, BlockBody: fn.Body}, Env: i.env.Copy(), FullParams: fn.Params}
			val = cl
		} else {
			return nil, errUndefinedVariable(lexer.Position{}, sel.Root)
		}
	}
	for _, field := range sel.Tail {
		obj, ok := val.(map[string]any)
		if !ok {
			return nil, errFieldAccessOnNonObject(lexer.Position{}, field, fmt.Sprintf("%T", val))
		}
		val = obj[field]
	}
	return val, nil
}

func (c closure) String() string {
	return fmt.Sprintf("<closure %d/%d args>", len(c.Args), len(c.Fn.Params)+len(c.Args))
}

// --- Statement Evaluation ---

func (i *Interpreter) evalStmt(s *parser.Statement) error {
	switch {
	case s.Let != nil:
		val := any(nil)
		var err error
		if s.Let.Value != nil {
			val, err = i.evalExpr(s.Let.Value)
			if err != nil {
				return err
			}
		}
		i.env.SetValue(s.Let.Name, val, false)
		return nil

	case s.Var != nil:
		val := any(nil)
		var err error
		if s.Var.Value != nil {
			val, err = i.evalExpr(s.Var.Value)
			if err != nil {
				return err
			}
		}
		i.env.SetValue(s.Var.Name, val, true)
		return nil

	case s.Assign != nil:
		val, err := i.evalExpr(s.Assign.Value)
		if err != nil {
			return err
		}
		return i.env.UpdateValue(s.Assign.Name, val)

	case s.Expr != nil:
		_, err := i.evalExpr(s.Expr.Expr)
		return err

	case s.Fun != nil:
		i.env.SetFunc(s.Fun.Name, s.Fun)
		return nil

	case s.Return != nil:
		val, err := i.evalExpr(s.Return.Value)
		if err != nil {
			return err
		}
		return returnSignal{val}

	case s.If != nil:
		return i.evalIf(s.If)

	case s.For != nil:
		return i.evalFor(s.For)

	case s.Type != nil:
		// type declarations have no runtime effect
		return nil

	case s.Model != nil:
		spec := types.ModelSpec{Params: map[string]any{}}
		for _, f := range s.Model.Fields {
			v, err := i.evalExpr(f.Value)
			if err != nil {
				return err
			}
			switch f.Name {
			case "provider":
				if s, ok := v.(string); ok {
					spec.Provider = s
				}
			case "name":
				if s, ok := v.(string); ok {
					spec.Name = s
				}
			default:
				spec.Params[f.Name] = v
			}
		}
		i.env.SetModel(s.Model.Name, spec)
		return nil

	case s.Test != nil:
		fmt.Printf("🔍 Test %s\n", s.Test.Name)
		child := types.NewEnv(i.env)
		interp := &Interpreter{prog: i.prog, env: child, types: i.types}
		for _, stmt := range s.Test.Body {
			if err := interp.evalStmt(stmt); err != nil {
				return fmt.Errorf("❌ %s: %w", s.Test.Name, err)
			}
		}
		fmt.Printf("✅ %s passed\n", s.Test.Name)
		return nil

	case s.Expect != nil:
		val, err := i.evalExpr(s.Expect.Value)
		if err != nil {
			return err
		}
		if b, ok := val.(bool); !ok || !b {
			return errExpectFailed(s.Expect.Pos)
		}
		return nil

	default:
		return fmt.Errorf("unsupported statement: %#v", s)
	}
}

func (i *Interpreter) evalIf(stmt *parser.IfStmt) error {
	condVal, err := i.evalExpr(stmt.Cond)
	if err != nil {
		return err
	}
	if truthy(condVal) {
		child := types.NewEnv(i.env)
		old := i.env
		i.env = child
		for _, s := range stmt.Then {
			if err := i.evalStmt(s); err != nil {
				i.env = old
				return err
			}
		}
		i.env = old
		return nil
	}
	if stmt.ElseIf != nil {
		return i.evalIf(stmt.ElseIf)
	}
	child := types.NewEnv(i.env)
	old := i.env
	i.env = child
	for _, s := range stmt.Else {
		if err := i.evalStmt(s); err != nil {
			i.env = old
			return err
		}
	}
	i.env = old
	return nil
}

func (i *Interpreter) evalFor(stmt *parser.ForStmt) error {
	fromVal, err := i.evalExpr(stmt.Source)
	if err != nil {
		return err
	}

	// Create loop scope
	child := types.NewEnv(i.env)
	oldEnv := i.env
	i.env = child
	defer func() { i.env = oldEnv }()

	// --- Range loop: `for x in a..b` ---
	if stmt.RangeEnd != nil {
		toVal, err := i.evalExpr(stmt.RangeEnd)
		if err != nil {
			return err
		}
		fromInt, ok1 := fromVal.(int)
		toInt, ok2 := toVal.(int)
		if !ok1 || !ok2 {
			return errInvalidRangeBounds(stmt.Pos, fmt.Sprintf("%T", fromVal), fmt.Sprintf("%T", toVal))
		}
		for x := fromInt; x < toInt; x++ {
			child.SetValue(stmt.Name, x, true)
			for _, s := range stmt.Body {
				if err := i.evalStmt(s); err != nil {
					return err
				}
			}
		}
		return nil
	}

	// --- Collection loop: `for x in list/map/string` ---
	switch coll := fromVal.(type) {
	case []any:
		for _, item := range coll {
			child.SetValue(stmt.Name, item, true)
			for _, s := range stmt.Body {
				if err := i.evalStmt(s); err != nil {
					return err
				}
			}
		}
	case map[any]any:
		for k := range coll {
			child.SetValue(stmt.Name, k, true)
			for _, s := range stmt.Body {
				if err := i.evalStmt(s); err != nil {
					return err
				}
			}
		}
	case map[string]any:
		for k := range coll {
			child.SetValue(stmt.Name, k, true)
			for _, s := range stmt.Body {
				if err := i.evalStmt(s); err != nil {
					return err
				}
			}
		}
	case map[int]any:
		for k := range coll {
			child.SetValue(stmt.Name, k, true)
			for _, s := range stmt.Body {
				if err := i.evalStmt(s); err != nil {
					return err
				}
			}
		}
	case string:
		for _, r := range coll {
			child.SetValue(stmt.Name, string(r), true)
			for _, s := range stmt.Body {
				if err := i.evalStmt(s); err != nil {
					return err
				}
			}
		}
	default:
		return errInvalidIterator(stmt.Pos, fmt.Sprintf("%T", fromVal))
	}

	return nil
}

// --- Expression Evaluation ---
func (i *Interpreter) evalExpr(e *parser.Expr) (any, error) {
	if e == nil {
		return nil, errors.New("nil expression")
	}
	return i.evalBinaryExpr(e.Binary)
}

func (i *Interpreter) evalBinaryExpr(b *parser.BinaryExpr) (any, error) {
	if b == nil {
		return nil, errors.New("nil binary expression")
	}

	// Step 1: Build a list of operands and operators
	type token struct {
		pos lexer.Position
		op  string
	}
	var operands []any
	var operators []token

	// Initial left expression
	left, err := i.evalUnary(b.Left)
	if err != nil {
		return nil, err
	}
	operands = append(operands, left)

	for _, part := range b.Right {
		operators = append(operators, token{part.Pos, part.Op})
		right, err := i.evalPostfixExpr(part.Right)
		if err != nil {
			return nil, err
		}
		operands = append(operands, right)
	}

	// Step 2: Apply precedence rules (high to low)
	for _, level := range [][]string{
		{"*", "/", "%"},        // highest
		{"+", "-"},             // addition
		{"<", "<=", ">", ">="}, // comparison
		{"==", "!="},           // equality
		{"&&"},                 // logical AND
		{"||"},                 // logical OR (lowest)
	} {
		for i := 0; i < len(operators); {
			op := operators[i].op
			if contains(level, op) {
				left := operands[i]
				right := operands[i+1]
				result, err := applyBinary(operators[i].pos, left, op, right)
				if err != nil {
					return nil, err
				}
				// Replace left and right with result
				operands[i] = result
				operands = append(operands[:i+1], operands[i+2:]...)  // remove i+1
				operators = append(operators[:i], operators[i+1:]...) // remove i
			} else {
				i++
			}
		}
	}

	if len(operands) != 1 {
		return nil, fmt.Errorf("unexpected state after binary eval")
	}
	return operands[0], nil
}

func contains(ops []string, op string) bool {
	for _, o := range ops {
		if o == op {
			return true
		}
	}
	return false
}

/*

func (i *Interpreter) evalExpr(e *parser.Expr) (any, error) {
	if e == nil {
		return nil, errors.New("nil expression")
	}
	return i.evalEquality(e.Equality)
}

func (i *Interpreter) evalEquality(e *parser.Equality) (any, error) {
	left, err := i.evalComparison(e.Left)
	if err != nil {
		return nil, err
	}
	for _, op := range e.Right {
		right, err := i.evalComparison(op.Right)
		if err != nil {
			return nil, err
		}
		left, err = applyBinary(op.Pos, left, op.Op, right)
		if err != nil {
			return nil, err
		}
	}
	return left, nil
}

func (i *Interpreter) evalComparison(c *parser.Comparison) (any, error) {
	left, err := i.evalTerm(c.Left)
	if err != nil {
		return nil, err
	}
	for _, op := range c.Right {
		right, err := i.evalTerm(op.Right)
		if err != nil {
			return nil, err
		}
		left, err = applyBinary(op.Pos, left, op.Op, right)
		if err != nil {
			return nil, err
		}
	}
	return left, nil
}

func (i *Interpreter) evalTerm(t *parser.Term) (any, error) {
	left, err := i.evalFactor(t.Left)
	if err != nil {
		return nil, err
	}
	for _, op := range t.Right {
		right, err := i.evalFactor(op.Right)
		if err != nil {
			return nil, err
		}
		left, err = applyBinary(op.Pos, left, op.Op, right)
		if err != nil {
			return nil, err
		}
	}
	return left, nil
}

func (i *Interpreter) evalFactor(f *parser.Factor) (any, error) {
	left, err := i.evalUnary(f.Left)
	if err != nil {
		return nil, err
	}
	for _, op := range f.Right {
		right, err := i.evalUnary(op.Right)
		if err != nil {
			return nil, err
		}
		left, err = applyBinary(op.Pos, left, op.Op, right)
		if err != nil {
			return nil, err
		}
	}
	return left, nil
}

*/

func (i *Interpreter) evalUnary(u *parser.Unary) (any, error) {
	val, err := i.evalPostfixExpr(u.Value)
	if err != nil {
		return nil, err
	}
	for j := len(u.Ops) - 1; j >= 0; j-- {
		val, err = applyUnary(u.Pos, u.Ops[j], val)
		if err != nil {
			return nil, err
		}
	}
	return val, nil
}

func (i *Interpreter) evalPostfixExpr(p *parser.PostfixExpr) (any, error) {
	val, err := i.evalPrimary(p.Target)
	if err != nil {
		return nil, err
	}

	for _, op := range p.Index {
		switch src := val.(type) {
		case []any:
			// Index
			if op.Colon == nil {
				index, err := i.evalExpr(op.Start)
				if err != nil {
					return nil, err
				}
				n, ok := index.(int)
				if !ok {
					return nil, errInvalidIndex(op.Pos, index)
				}
				if n < 0 {
					n += len(src)
				}
				if n < 0 || n >= len(src) {
					return nil, errIndexOutOfBounds(op.Pos, n, len(src))
				}
				val = src[n]

			} else {
				// Slice
				start, end := 0, len(src)
				if op.Start != nil {
					s, err := i.evalExpr(op.Start)
					if err != nil {
						return nil, err
					}
					if n, ok := s.(int); ok {
						if n < 0 {
							n += len(src)
						}
						start = n
					} else {
						return nil, errInvalidIndex(op.Pos, s)
					}
				}
				if op.End != nil {
					e, err := i.evalExpr(op.End)
					if err != nil {
						return nil, err
					}
					if n, ok := e.(int); ok {
						if n < 0 {
							n += len(src)
						}
						end = n
					} else {
						return nil, errInvalidIndex(op.Pos, e)
					}
				}
				if start < 0 || end > len(src) || start > end {
					return nil, errSliceOutOfBounds(op.Pos, start, end, len(src))
				}
				val = src[start:end]
			}

		case string:
			runes := []rune(src)
			if op.Colon == nil {
				index, err := i.evalExpr(op.Start)
				if err != nil {
					return nil, err
				}
				n, ok := index.(int)
				if !ok {
					return nil, errInvalidIndex(op.Pos, index)
				}
				if n < 0 {
					n += len(runes)
				}
				if n < 0 || n >= len(runes) {
					return nil, errIndexOutOfBounds(op.Pos, n, len(runes))
				}
				val = string(runes[n])
			} else {
				start, end := 0, len(runes)
				if op.Start != nil {
					s, err := i.evalExpr(op.Start)
					if err != nil {
						return nil, err
					}
					if n, ok := s.(int); ok {
						if n < 0 {
							n += len(runes)
						}
						start = n
					} else {
						return nil, errInvalidIndex(op.Pos, s)
					}
				}
				if op.End != nil {
					e, err := i.evalExpr(op.End)
					if err != nil {
						return nil, err
					}
					if n, ok := e.(int); ok {
						if n < 0 {
							n += len(runes)
						}
						end = n
					} else {
						return nil, errInvalidIndex(op.Pos, e)
					}
				}
				if start < 0 || end > len(runes) || start > end {
					return nil, errSliceOutOfBounds(op.Pos, start, end, len(runes))
				}
				val = string(runes[start:end])
			}
		case map[string]any:
			if op.Colon != nil {
				return nil, errInvalidIndexTarget(op.Pos, "map")
			}
			if op.Start == nil {
				return nil, errInvalidIndex(op.Pos, nil)
			}
			key, err := i.evalExpr(op.Start)
			if err != nil {
				return nil, err
			}
			keyStr, ok := key.(string)
			if !ok {
				return nil, errInvalidMapKey(op.Pos, key)
			}
			val = src[keyStr]

		default:
			return nil, errInvalidIndexTarget(op.Pos, fmt.Sprintf("%T", src))
		}
	}
	return val, nil
}

func (i *Interpreter) evalPrimary(p *parser.Primary) (any, error) {
	switch {
	case p.Lit != nil:
		return i.evalLiteral(p.Lit)

	case p.Call != nil:
		return i.evalCall(p.Call)

	case p.Group != nil:
		return i.evalExpr(p.Group)

	case p.FunExpr != nil:
		return closure{Name: "", Fn: p.FunExpr, Env: i.env.Copy(), FullParams: p.FunExpr.Params}, nil

	case p.Selector != nil:
		return i.evalSelector(p.Selector)

	case p.Struct != nil:
		obj := map[string]any{}
		for _, field := range p.Struct.Fields {
			v, err := i.evalExpr(field.Value)
			if err != nil {
				return nil, err
			}
			obj[field.Name] = v
		}
		if st, ok := i.types.GetStruct(p.Struct.Name); ok {
			for name, fn := range st.Methods {
				env := i.env.Copy()
				for k, v := range obj {
					env.SetValue(k, v, true)
				}
				cl := closure{Name: name, Fn: &parser.FunExpr{Params: fn.Params, Return: fn.Return, BlockBody: fn.Body}, Env: env, FullParams: fn.Params}
				obj[name] = cl
			}
		} else {
			obj["__name"] = p.Struct.Name
		}
		return obj, nil

	case p.List != nil:
		var elems []any
		for _, expr := range p.List.Elems {
			val, err := i.evalExpr(expr)
			if err != nil {
				return nil, err
			}
			elems = append(elems, val)
		}
		return elems, nil
	case p.Map != nil:
		obj := map[string]any{}
		for _, item := range p.Map.Items {
			keyVal, err := i.evalExpr(item.Key)
			if err != nil {
				return nil, err
			}
			strKey, ok := keyVal.(string)
			if !ok {
				return nil, errInvalidMapKey(item.Pos, keyVal)
			}
			val, err := i.evalExpr(item.Value)
			if err != nil {
				return nil, err
			}
			obj[strKey] = val
		}
		return obj, nil

	case p.Match != nil:
		return i.evalMatch(p.Match)

	case p.Generate != nil:
		reqParams := map[string]any{}
		var (
			prompt     string
			text       string
			modelStr   string
			provider   string
			normalize  bool
			haveNorm   bool
			toolsList  []llm.Tool
			toolFuncs  = map[string]closure{}
			toolChoice string
		)
		for _, f := range p.Generate.Fields {
			v, err := i.evalExpr(f.Value)
			if err != nil {
				return nil, err
			}
			switch f.Name {
			case "prompt":
				if s, ok := v.(string); ok {
					prompt = s
				}
			case "text":
				if s, ok := v.(string); ok {
					text = s
				}
			case "temperature", "top_p", "max_tokens", "stop":
				reqParams[f.Name] = v
			case "model":
				if s, ok := v.(string); ok {
					modelStr = s
				}
			case "normalize":
				if b, ok := v.(bool); ok {
					normalize = b
					haveNorm = true
				}
			case "tools":
				if list, ok := v.([]any); ok {
					for _, item := range list {
						switch t := item.(type) {
						case closure:
							if t.Name == "" {
								continue
							}
							toolFuncs[t.Name] = t
							if fn, ok := i.env.GetFunc(t.Name); ok {
								typ, _ := i.types.GetVar(t.Name)
								if ft, ok2 := typ.(types.FuncType); ok2 {
									toolsList = append(toolsList, funcToTool(t.Name, fn, ft))
								}
							}
						case *closure:
							cl := *t
							if cl.Name == "" {
								continue
							}
							toolFuncs[cl.Name] = cl
							if fn, ok := i.env.GetFunc(cl.Name); ok {
								typ, _ := i.types.GetVar(cl.Name)
								if ft, ok2 := typ.(types.FuncType); ok2 {
									toolsList = append(toolsList, funcToTool(cl.Name, fn, ft))
								}
							}
						case map[string]any:
							nameAny, has := t["__name"]
							name, ok1 := nameAny.(string)
							if !has || !ok1 {
								continue
							}
							desc, _ := t["description"].(string)
							if fn, ok := i.env.GetFunc(name); ok {
								typ, _ := i.types.GetVar(name)
								if ft, ok2 := typ.(types.FuncType); ok2 {
									tool := funcToTool(name, fn, ft)
									if desc != "" {
										tool.Description = desc
									}
									toolsList = append(toolsList, tool)
									cl := closure{Name: name, Fn: &parser.FunExpr{Params: fn.Params, Return: fn.Return, BlockBody: fn.Body}, Env: i.env.Copy(), FullParams: fn.Params}
									toolFuncs[name] = cl
								}
							}
						}
					}
				}
			case "tool_choice":
				if s, ok := v.(string); ok {
					toolChoice = s
				}
			default:
				reqParams[f.Name] = v
			}
		}

		if modelStr != "" {
			if parts := strings.SplitN(modelStr, ":", 2); len(parts) == 2 {
				provider = parts[0]
				modelStr = parts[1]
			} else if spec, ok := i.env.GetModel(modelStr); ok {
				provider = spec.Provider
				if spec.Name != "" {
					modelStr = spec.Name
				}
				for k, v := range spec.Params {
					if _, ok := reqParams[k]; !ok {
						reqParams[k] = v
					}
				}
			}
		}

		opts := []llm.Option{}
		if modelStr != "" {
			opts = append(opts, llm.WithModel(modelStr))
		}
		for k, v := range reqParams {
			opts = append(opts, llm.WithParam(k, v))
		}
		if len(toolsList) > 0 {
			opts = append(opts, llm.WithTools(toolsList))
		}
		if toolChoice != "" {
			opts = append(opts, llm.WithToolChoice(toolChoice))
		}

		if p.Generate.Target != "text" && p.Generate.Target != "embedding" {
			format := llm.ResponseFormat{Type: "json_object"}
			if st, ok := i.types.GetStruct(p.Generate.Target); ok {
				format.Schema = structToSchema(st)
			}
			opts = append(opts, llm.WithResponseFormat(format))
		}

		if p.Generate.Target == "embedding" {
			embedOpts := []llm.EmbedOption{}
			if modelStr != "" {
				embedOpts = append(embedOpts, llm.WithEmbedModel(modelStr))
			}
			for k, v := range reqParams {
				embedOpts = append(embedOpts, llm.WithEmbedParam(k, v))
			}
			if haveNorm {
				embedOpts = append(embedOpts, llm.WithEmbedNormalize(normalize))
			}
			var resp *llm.EmbedResponse
			var err error
			if provider != "" {
				client, errOpen := llm.Open(provider, "", llm.Options{Model: modelStr})
				if errOpen != nil {
					return nil, errOpen
				}
				defer client.Close()
				resp, err = client.Embed(context.Background(), text, embedOpts...)
			} else {
				resp, err = llm.Embed(context.Background(), text, embedOpts...)
			}
			if err != nil {
				return nil, err
			}
			out := make([]any, len(resp.Vector))
			for i, v := range resp.Vector {
				out[i] = v
			}
			return out, nil
		}

		var resp *llm.ChatResponse
		msgs := []llm.Message{{Role: "user", Content: prompt}}
		for {
			var err error
			if provider != "" {
				client, errOpen := llm.Open(provider, "", llm.Options{Model: modelStr})
				if errOpen != nil {
					return nil, errOpen
				}
				defer client.Close()
				resp, err = client.Chat(context.Background(), msgs, opts...)
			} else {
				resp, err = llm.Chat(context.Background(), msgs, opts...)
			}
			if err != nil {
				return nil, err
			}
			if resp.Message.ToolCall == nil {
				break
			}
			tc := resp.Message.ToolCall
			tool, ok := toolFuncs[tc.Name]
			if !ok {
				return nil, fmt.Errorf("unknown tool %s", tc.Name)
			}
			result, err := i.invokeTool(tool, tc.Args)
			if err != nil {
				return nil, err
			}
			msgs = append(msgs, resp.Message)
			msgs = append(msgs, llm.Message{Role: "tool", Content: fmt.Sprint(result), ToolCall: &llm.ToolCall{ID: tc.ID, Name: tc.Name}})
		}
		if p.Generate.Target == "text" {
			return resp.Message.Content, nil
		}
		var obj map[string]any
		if err := json.Unmarshal([]byte(resp.Message.Content), &obj); err != nil {
			return nil, err
		}
		return obj, nil

	default:
		return nil, errInvalidPrimaryExpression(p.Pos)
	}
}

func (i *Interpreter) evalLiteral(l *parser.Literal) (any, error) {
	switch {
	case l.Int != nil:
		return *l.Int, nil
	case l.Float != nil:
		return *l.Float, nil
	case l.Str != nil:
		return *l.Str, nil
	case l.Bool != nil:
		return bool(*l.Bool), nil
	default:
		return nil, errInvalidLiteral(l.Pos)
	}
}

func builtinPrint(i *Interpreter, c *parser.CallExpr) (any, error) {
	var sb strings.Builder
	for _, arg := range c.Args {
		val, err := i.evalExpr(arg)
		if err != nil {
			return nil, err
		}
		fmt.Fprintf(&sb, "%v ", val)
	}
	_, err := fmt.Fprintln(i.env.Writer(), strings.TrimSpace(sb.String()))
	return nil, err
}

func builtinLen(i *Interpreter, c *parser.CallExpr) (any, error) {
	if len(c.Args) != 1 {
		return nil, errTooManyFunctionArgs(c.Pos, "len", 1, len(c.Args))
	}
	val, err := i.evalExpr(c.Args[0])
	if err != nil {
		return nil, err
	}

	switch v := val.(type) {
	case []any:
		return len(v), nil
	case string:
		return len([]rune(v)), nil
	case map[string]any:
		return len(v), nil
	default:
		return nil, errInvalidLenOperand(c.Pos, fmt.Sprintf("%T", val))
	}
}

func builtinNow(i *Interpreter, c *parser.CallExpr) (any, error) {
	if len(c.Args) != 0 {
		return nil, fmt.Errorf("now() takes no arguments")
	}
	return time.Now().UnixNano(), nil
}

func builtinJSON(i *Interpreter, c *parser.CallExpr) (any, error) {
	if len(c.Args) != 1 {
		return nil, fmt.Errorf("json(x) takes exactly one argument")
	}
	val, err := i.evalExpr(c.Args[0])
	if err != nil {
		return nil, err
	}
	data, err := json.MarshalIndent(val, "", "  ")
	if err != nil {
		return nil, err
	}
	_, err = fmt.Fprintln(i.env.Writer(), string(data))
	return nil, err
}

func (i *Interpreter) invokeTool(cl closure, args map[string]any) (any, error) {
	child := types.NewEnv(cl.Env)
	for _, param := range cl.FullParams {
		child.SetValue(param.Name, args[param.Name], true)
	}
	old := i.env
	i.env = child
	defer func() { i.env = old }()
	if cl.Fn.ExprBody != nil {
		return i.evalExpr(cl.Fn.ExprBody)
	}
	for _, stmt := range cl.Fn.BlockBody {
		if err := i.evalStmt(stmt); err != nil {
			if r, ok := err.(returnSignal); ok {
				return r.value, nil
			}
			return nil, err
		}
	}
	return nil, nil
}

func isUnderscoreExpr(e *parser.Expr) bool {
	if e == nil {
		return false
	}
	if len(e.Binary.Right) != 0 {
		return false
	}
	u := e.Binary.Left
	if len(u.Ops) != 0 {
		return false
	}
	p := u.Value
	if len(p.Index) != 0 {
		return false
	}
	if p.Target.Selector != nil && p.Target.Selector.Root == "_" && len(p.Target.Selector.Tail) == 0 {
		return true
	}
	return false
}

func (i *Interpreter) evalMatch(m *parser.MatchExpr) (any, error) {
	val, err := i.evalExpr(m.Target)
	if err != nil {
		return nil, err
	}
	for _, c := range m.Cases {
		if isUnderscoreExpr(c.Pattern) {
			return i.evalExpr(c.Result)
		}
		pv, err := i.evalExpr(c.Pattern)
		if err != nil {
			return nil, err
		}
		eq, err := applyBinary(c.Pos, val, "==", pv)
		if err != nil {
			return nil, err
		}
		if b, ok := eq.(bool); ok && b {
			return i.evalExpr(c.Result)
		}
	}
	return nil, nil
}

func (i *Interpreter) builtinFuncs() map[string]func(*Interpreter, *parser.CallExpr) (any, error) {
	return map[string]func(*Interpreter, *parser.CallExpr) (any, error){
		"print": builtinPrint,
		"len":   builtinLen,
		"now":   builtinNow,
		"json":  builtinJSON,
	}
}

func (i *Interpreter) evalCall(c *parser.CallExpr) (any, error) {
	name := selectorString(c.Func)
	// Built-in functions only apply to simple identifiers
	if fn, ok := i.builtinFuncs()[name]; ok && len(c.Func.Tail) == 0 {
		return fn(i, c)
	}

	if fn, ok := i.env.GetFunc(name); ok && len(c.Func.Tail) == 0 {
		argCount := len(c.Args)
		paramCount := len(fn.Params)

		if argCount > paramCount {
			return nil, errTooManyFunctionArgs(c.Pos, name, paramCount, argCount)
		}

		if argCount < paramCount {
			applied := make([]Value, argCount)
			for idx := range c.Args {
				v, err := i.evalExpr(c.Args[idx])
				if err != nil {
					return nil, err
				}
				applied[idx] = anyToValue(v)
			}
			remainingParams := fn.Params[argCount:]
			return closure{
				Name: name,
				Fn: &parser.FunExpr{
					Params:    remainingParams,
					BlockBody: fn.Body,
				},
				Env:        i.env.Copy(),
				Args:       applied,
				FullParams: fn.Params,
			}, nil
		}

		child := types.NewEnv(i.env)
		for idx, param := range fn.Params {
			v, err := i.evalExpr(c.Args[idx])
			if err != nil {
				return nil, err
			}
			child.SetValue(param.Name, v, true)
		}
		old := i.env
		i.env = child
		defer func() { i.env = old }()
		for _, stmt := range fn.Body {
			if err := i.evalStmt(stmt); err != nil {
				if r, ok := err.(returnSignal); ok {
					return r.value, nil
				}
				return nil, err
			}
		}
		return nil, nil
	}

	val, err := i.evalSelector(c.Func)
	if err == nil {
		if cl, ok := val.(closure); ok {
			totalArgs := len(cl.Args) + len(c.Args)
			fullParamCount := len(cl.FullParams)

			if totalArgs > fullParamCount {
				return nil, errTooManyFunctionArgs(c.Pos, name, fullParamCount, totalArgs)
			}

			allArgs := append([]Value{}, cl.Args...)
			for _, arg := range c.Args {
				v, err := i.evalExpr(arg)
				if err != nil {
					return nil, err
				}
				allArgs = append(allArgs, anyToValue(v))
			}

			if totalArgs < fullParamCount {
				return closure{
					Name: cl.Name,
					Fn: &parser.FunExpr{
						Params:    cl.Fn.Params[totalArgs-len(cl.Args):],
						BlockBody: cl.Fn.BlockBody,
						ExprBody:  cl.Fn.ExprBody,
					},
					Env:        cl.Env,
					Args:       allArgs,
					FullParams: cl.FullParams,
				}, nil
			}

			if len(cl.FullParams) != len(allArgs) {
				return nil, errInternalClosureArgMismatch(c.Pos)
			}

			child := types.NewEnv(cl.Env)
			for idx, param := range cl.FullParams {
				child.SetValue(param.Name, valueToAny(allArgs[idx]), true)
			}

			old := i.env
			i.env = child
			defer func() { i.env = old }()
			if cl.Fn.ExprBody != nil {
				return i.evalExpr(cl.Fn.ExprBody)
			}
			for _, stmt := range cl.Fn.BlockBody {
				if err := i.evalStmt(stmt); err != nil {
					if r, ok := err.(returnSignal); ok {
						return r.value, nil
					}
					return nil, err
				}
			}
			return nil, nil
		}
	}

	return nil, errUndefinedFunctionOrClosure(c.Pos, name)
}

// --- Return ---

type returnSignal struct{ value any }

func (r returnSignal) Error() string { return "return" }

func applyBinary(pos lexer.Position, left any, op string, right any) (any, error) {
	lv := anyToValue(left)
	rv := anyToValue(right)
	res, err := applyBinaryValue(pos, lv, op, rv)
	if err != nil {
		return nil, err
	}
	return valueToAny(res), nil
}

func applyBinaryValue(pos lexer.Position, left Value, op string, right Value) (Value, error) {
	if left.Tag == TagList && right.Tag == TagList {
		switch op {
		case "+":
			return Value{Tag: TagList, List: append(append([]Value{}, left.List...), right.List...)}, nil
		case "==":
			if len(left.List) != len(right.List) {
				return Value{Tag: TagBool, Bool: false}, nil
			}
			for i := range left.List {
				eq, err := applyBinaryValue(pos, left.List[i], "==", right.List[i])
				if err != nil {
					return Value{}, err
				}
				if !eq.Bool {
					return Value{Tag: TagBool, Bool: false}, nil
				}
			}
			return Value{Tag: TagBool, Bool: true}, nil
		case "!=":
			eq, err := applyBinaryValue(pos, left, "==", right)
			if err != nil {
				return Value{}, err
			}
			return Value{Tag: TagBool, Bool: !eq.Bool}, nil
		}
	}

	switch left.Tag {
	case TagBool:
		if right.Tag == TagBool {
			switch op {
			case "==":
				return Value{Tag: TagBool, Bool: left.Bool == right.Bool}, nil
			case "!=":
				return Value{Tag: TagBool, Bool: left.Bool != right.Bool}, nil
			case "&&":
				return Value{Tag: TagBool, Bool: left.Bool && right.Bool}, nil
			case "||":
				return Value{Tag: TagBool, Bool: left.Bool || right.Bool}, nil
			}
		}
	case TagInt:
		if right.Tag == TagInt {
			return applyIntBinaryValue(pos, left.Int, op, right.Int)
		}
	case TagFloat:
		if right.Tag == TagFloat {
			return applyFloatBinaryValue(pos, left.Float, op, right.Float)
		}
	case TagStr:
		if right.Tag == TagStr {
			switch op {
			case "+":
				return Value{Tag: TagStr, Str: left.Str + right.Str}, nil
			case "==":
				return Value{Tag: TagBool, Bool: left.Str == right.Str}, nil
			case "!=":
				return Value{Tag: TagBool, Bool: left.Str != right.Str}, nil
			}
		}
	}
	return Value{}, errInvalidOperator(pos, op, left.Tag.String(), right.Tag.String())
}

func applyIntBinaryValue(pos lexer.Position, l int, op string, r int) (Value, error) {
	switch op {
	case "/":
		if r == 0 {
			return Value{}, errDivisionByZero(pos)
		}
		return Value{Tag: TagInt, Int: l / r}, nil
	case "%":
		if r == 0 {
			return Value{}, errDivisionByZero(pos)
		}
		return Value{Tag: TagInt, Int: l % r}, nil
	case "+":
		return Value{Tag: TagInt, Int: l + r}, nil
	case "-":
		return Value{Tag: TagInt, Int: l - r}, nil
	case "*":
		return Value{Tag: TagInt, Int: l * r}, nil
	case "==":
		return Value{Tag: TagBool, Bool: l == r}, nil
	case "!=":
		return Value{Tag: TagBool, Bool: l != r}, nil
	case "<":
		return Value{Tag: TagBool, Bool: l < r}, nil
	case "<=":
		return Value{Tag: TagBool, Bool: l <= r}, nil
	case ">":
		return Value{Tag: TagBool, Bool: l > r}, nil
	case ">=":
		return Value{Tag: TagBool, Bool: l >= r}, nil
	default:
		return Value{}, errInvalidOperator(pos, op, "int", "int")
	}
}

func applyInt64Binary(pos lexer.Position, l int64, op string, r int64) (Value, error) {
	switch op {
	case "/":
		if r == 0 {
			return Value{}, errDivisionByZero(pos)
		}
		return Value{Tag: TagInt, Int: int(l / r)}, nil
	case "%":
		if r == 0 {
			return Value{}, errDivisionByZero(pos)
		}
		return Value{Tag: TagInt, Int: int(l % r)}, nil
	case "+":
		return Value{Tag: TagInt, Int: int(l + r)}, nil
	case "-":
		return Value{Tag: TagInt, Int: int(l - r)}, nil
	case "*":
		return Value{Tag: TagInt, Int: int(l * r)}, nil
	case "==":
		return Value{Tag: TagBool, Bool: l == r}, nil
	case "!=":
		return Value{Tag: TagBool, Bool: l != r}, nil
	case "<":
		return Value{Tag: TagBool, Bool: l < r}, nil
	case "<=":
		return Value{Tag: TagBool, Bool: l <= r}, nil
	case ">":
		return Value{Tag: TagBool, Bool: l > r}, nil
	case ">=":
		return Value{Tag: TagBool, Bool: l >= r}, nil
	default:
		return Value{}, errInvalidOperator(pos, op, "int", "int")
	}
}

func applyFloatBinaryValue(pos lexer.Position, l float64, op string, r float64) (Value, error) {
	switch op {
	case "/":
		if r == 0 {
			return Value{}, errDivisionByZero(pos)
		}
		return Value{Tag: TagFloat, Float: l / r}, nil
	case "+":
		return Value{Tag: TagFloat, Float: l + r}, nil
	case "-":
		return Value{Tag: TagFloat, Float: l - r}, nil
	case "*":
		return Value{Tag: TagFloat, Float: l * r}, nil
	case "==":
		return Value{Tag: TagBool, Bool: l == r}, nil
	case "!=":
		return Value{Tag: TagBool, Bool: l != r}, nil
	case "<":
		return Value{Tag: TagBool, Bool: l < r}, nil
	case "<=":
		return Value{Tag: TagBool, Bool: l <= r}, nil
	case ">":
		return Value{Tag: TagBool, Bool: l > r}, nil
	case ">=":
		return Value{Tag: TagBool, Bool: l >= r}, nil
	default:
		return Value{}, errInvalidOperator(pos, op, "float", "float")
	}
}

func applyUnary(pos lexer.Position, op string, val any) (any, error) {
	v := anyToValue(val)
	res, err := applyUnaryValue(pos, op, v)
	if err != nil {
		return nil, err
	}
	return valueToAny(res), nil
}

func applyUnaryValue(pos lexer.Position, op string, val Value) (Value, error) {
	switch op {
	case "-":
		switch val.Tag {
		case TagInt:
			return Value{Tag: TagInt, Int: -val.Int}, nil
		case TagFloat:
			return Value{Tag: TagFloat, Float: -val.Float}, nil
		default:
			return Value{}, errInvalidUnaryOperator(pos, op, val.Tag.String())
		}
	case "!":
		if val.Tag == TagBool {
			return Value{Tag: TagBool, Bool: !val.Bool}, nil
		}
		return Value{}, errInvalidUnaryOperator(pos, op, val.Tag.String())
	default:
		return Value{}, errUnknownUnaryOperator(pos, op)
	}
}

func truthy(val any) bool {
	return anyToValue(val).Truthy()
}

func selectorString(sel *parser.SelectorExpr) string {
	if sel == nil {
		return ""
	}
	s := sel.Root
	for _, f := range sel.Tail {
		s += "." + f
	}
	return s
}
