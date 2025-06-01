package interpreter

import (
	"encoding/json"
	"errors"
	"fmt"
	"github.com/alecthomas/participle/v2/lexer"
	"github.com/fatih/color"
	"mochi/parser"
	"mochi/types"
	"os"
	"reflect"
	"strings"
	"time"
)

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
	Fn         *parser.FunExpr
	Env        *types.Env
	Args       []any
	FullParams []*parser.Param
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
		i.env.SetValue(s.Let.Name, val)
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
		for _, s := range stmt.Then {
			if err := i.evalStmt(s); err != nil {
				return err
			}
		}
		return nil
	}
	if stmt.ElseIf != nil {
		return i.evalIf(stmt.ElseIf)
	}
	for _, s := range stmt.Else {
		if err := i.evalStmt(s); err != nil {
			return err
		}
	}
	return nil
}

func (i *Interpreter) evalFor(stmt *parser.ForStmt) error {
	fromVal, err := i.evalExpr(stmt.Source)
	if err != nil {
		return err
	}

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
			i.env.SetValue(stmt.Name, x)
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
			i.env.SetValue(stmt.Name, item)
			for _, s := range stmt.Body {
				if err := i.evalStmt(s); err != nil {
					return err
				}
			}
		}
	case map[any]any:
		for k := range coll {
			i.env.SetValue(stmt.Name, k)
			for _, s := range stmt.Body {
				if err := i.evalStmt(s); err != nil {
					return err
				}
			}
		}
	case map[string]any:
		for k := range coll {
			i.env.SetValue(stmt.Name, k)
			for _, s := range stmt.Body {
				if err := i.evalStmt(s); err != nil {
					return err
				}
			}
		}
	case map[int]any:
		for k := range coll {
			i.env.SetValue(stmt.Name, k)
			for _, s := range stmt.Body {
				if err := i.evalStmt(s); err != nil {
					return err
				}
			}
		}
	case string:
		for _, r := range coll {
			i.env.SetValue(stmt.Name, string(r))
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
		{"+", "-"},             // middle
		{"<", "<=", ">", ">="}, // comparison
		{"==", "!="},           // equality
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
		return closure{Fn: p.FunExpr, Env: i.env.Copy(), FullParams: p.FunExpr.Params}, nil

	case p.Selector != nil:
		val, err := i.env.GetValue(p.Selector.Root)
		if err != nil {
			return nil, errUndefinedVariable(p.Pos, p.Selector.Root)
		}
		for _, field := range p.Selector.Tail {
			obj, ok := val.(map[string]any)
			if !ok {
				return nil, errFieldAccessOnNonObject(p.Pos, field, fmt.Sprintf("%T", val))
			}
			val = obj[field]
		}
		return val, nil

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
		return *l.Bool, nil
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

func (i *Interpreter) builtinFuncs() map[string]func(*Interpreter, *parser.CallExpr) (any, error) {
	return map[string]func(*Interpreter, *parser.CallExpr) (any, error){
		"print": builtinPrint,
		"len":   builtinLen,
		"now":   builtinNow,
		"json":  builtinJSON,
	}
}

func (i *Interpreter) evalCall(c *parser.CallExpr) (any, error) {
	// Built-in function dispatch
	if fn, ok := i.builtinFuncs()[c.Func]; ok {
		return fn(i, c)
	}

	if fn, ok := i.env.GetFunc(c.Func); ok {
		argCount := len(c.Args)
		paramCount := len(fn.Params)

		if argCount > paramCount {
			return nil, errTooManyFunctionArgs(c.Pos, c.Func, paramCount, argCount)
		}

		if argCount < paramCount {
			applied := make([]any, argCount)
			for idx := range c.Args {
				val, err := i.evalExpr(c.Args[idx])
				if err != nil {
					return nil, err
				}
				applied[idx] = val
			}
			remainingParams := fn.Params[argCount:]
			return closure{
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
			val, err := i.evalExpr(c.Args[idx])
			if err != nil {
				return nil, err
			}
			child.SetValue(param.Name, val)
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

	val, err := i.env.GetValue(c.Func)
	if err == nil {
		if cl, ok := val.(closure); ok {
			totalArgs := len(cl.Args) + len(c.Args)
			fullParamCount := len(cl.FullParams)

			if totalArgs > fullParamCount {
				return nil, errTooManyFunctionArgs(c.Pos, c.Func, fullParamCount, totalArgs)
			}

			allArgs := append([]any{}, cl.Args...)
			for _, arg := range c.Args {
				val, err := i.evalExpr(arg)
				if err != nil {
					return nil, err
				}
				allArgs = append(allArgs, val)
			}

			if totalArgs < fullParamCount {
				return closure{
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
				child.SetValue(param.Name, allArgs[idx])
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

	return nil, errUndefinedFunctionOrClosure(c.Pos, c.Func)
}

// --- Return ---

type returnSignal struct{ value any }

func (r returnSignal) Error() string { return "return" }

func applyBinary(pos lexer.Position, left any, op string, right any) (any, error) {
	// Deep compare slices
	if lSlice, ok := left.([]any); ok {
		if rSlice, ok := right.([]any); ok {
			switch op {
			case "+":
				return append(lSlice, rSlice...), nil
			case "==":
				return reflect.DeepEqual(lSlice, rSlice), nil
			case "!=":
				return !reflect.DeepEqual(lSlice, rSlice), nil
			default:
				return nil, errInvalidOperator(pos, op, "[]any", "[]any")
			}
		}
	}

	switch l := left.(type) {
	case bool:
		if r, ok := right.(bool); ok {
			switch op {
			case "==":
				return l == r, nil
			case "!=":
				return l != r, nil
			}
		}

	case int64:
		if r, ok := right.(int64); ok {
			return applyInt64Binary(pos, l, op, r)
		}
		if r, ok := right.(int); ok {
			return applyInt64Binary(pos, l, op, int64(r))
		}
	case int:
		if r, ok := right.(int); ok {
			return applyIntBinary(pos, l, op, r)
		}
	case float64:
		if r, ok := right.(float64); ok {
			return applyFloatBinary(pos, l, op, r)
		}
	case string:
		if r, ok := right.(string); ok {
			switch op {
			case "+":
				return l + r, nil
			case "==":
				return l == r, nil
			case "!=":
				return l != r, nil
			}
		}
	}
	return nil, errInvalidOperator(pos, op, fmt.Sprintf("%T", left), fmt.Sprintf("%T", right))
}

func applyIntBinary(pos lexer.Position, l int, op string, r int) (any, error) {
	switch op {
	case "/":
		if r == 0 {
			return nil, errDivisionByZero(pos)
		}
		return l / r, nil
	case "%":
		if r == 0 {
			return nil, errDivisionByZero(pos)
		}
		return l % r, nil
	case "+":
		return l + r, nil
	case "-":
		return l - r, nil
	case "*":
		return l * r, nil
	case "==":
		return l == r, nil
	case "!=":
		return l != r, nil
	case "<":
		return l < r, nil
	case "<=":
		return l <= r, nil
	case ">":
		return l > r, nil
	case ">=":
		return l >= r, nil
	default:
		return nil, errInvalidOperator(pos, op, "int", "int")
	}
}

func applyInt64Binary(pos lexer.Position, l int64, op string, r int64) (any, error) {
	switch op {
	case "/":
		if r == 0 {
			return nil, errDivisionByZero(pos)
		}
		return l / r, nil
	case "%":
		if r == 0 {
			return nil, errDivisionByZero(pos)
		}
		return l % r, nil
	case "+":
		return l + r, nil
	case "-":
		return l - r, nil
	case "*":
		return l * r, nil
	case "==":
		return l == r, nil
	case "!=":
		return l != r, nil
	case "<":
		return l < r, nil
	case "<=":
		return l <= r, nil
	case ">":
		return l > r, nil
	case ">=":
		return l >= r, nil
	default:
		return nil, errInvalidOperator(pos, op, "int", "int")
	}
}

func applyFloatBinary(pos lexer.Position, l float64, op string, r float64) (any, error) {
	switch op {
	case "/":
		if r == 0 {
			return nil, errDivisionByZero(pos)
		}
		return l / r, nil
	case "+":
		return l + r, nil
	case "-":
		return l - r, nil
	case "*":
		return l * r, nil
	case "==":
		return l == r, nil
	case "!=":
		return l != r, nil
	case "<":
		return l < r, nil
	case "<=":
		return l <= r, nil
	case ">":
		return l > r, nil
	case ">=":
		return l >= r, nil
	default:
		return nil, errInvalidOperator(pos, op, "float", "float")
	}
}

func applyUnary(pos lexer.Position, op string, val any) (any, error) {
	switch op {
	case "-":
		switch v := val.(type) {
		case int:
			return -v, nil
		case float64:
			return -v, nil
		default:
			return nil, errInvalidUnaryOperator(pos, op, fmt.Sprintf("%T", val))
		}
	case "!":
		if b, ok := val.(bool); ok {
			return !b, nil
		}
		return nil, errInvalidUnaryOperator(pos, op, fmt.Sprintf("%T", val))
	default:
		return nil, errUnknownUnaryOperator(pos, op)
	}
}

func truthy(val any) bool {
	switch v := val.(type) {
	case bool:
		return v
	case int:
		return v != 0
	case float64:
		return v != 0
	case string:
		return v != ""
	default:
		return val != nil
	}
}
