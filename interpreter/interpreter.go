package interpreter

import (
	"errors"
	"fmt"
	"github.com/alecthomas/participle/v2/lexer"
	"mochi/ast"
	"mochi/parser"
	"mochi/types"
	"strings"
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

func (i *Interpreter) Run() error {
	var testFailures []error

	for _, stmt := range i.prog.Statements {
		if stmt.Test != nil {
			name := stmt.Test.Name
			fmt.Printf("🔍 Test %s\n", name)

			child := types.NewEnv(i.env)
			interp := &Interpreter{prog: i.prog, env: child, types: i.types}

			var failed error
			for _, s := range stmt.Test.Body {
				if err := interp.evalStmt(s); err != nil {
					failed = fmt.Errorf("❌ %s: %w", name, err)
					break // stop this test, but continue others
				}
			}

			if failed != nil {
				fmt.Println(failed)
				testFailures = append(testFailures, failed)
			} else {
				fmt.Printf("✅ %s passed\n", name)
			}

		} else {
			if err := i.evalStmt(stmt); err != nil {
				return err // still stop on non-test failure
			}
		}
	}

	if len(testFailures) > 0 {
		var sb strings.Builder
		fmt.Fprintf(&sb, "\n💥 %d test(s) failed:\n", len(testFailures))
		for _, err := range testFailures {
			fmt.Fprintf(&sb, "  - %v\n", err)
		}
		return errors.New(sb.String())
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
			return fmt.Errorf("expect failed: %s", ast.PrettyStatement(s))
		}
		return nil

	default:
		return fmt.Errorf("unsupported statement: %s", ast.PrettyStatement(s))
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
	fromVal, err := i.evalExpr(stmt.Start)
	if err != nil {
		return err
	}
	toVal, err := i.evalExpr(stmt.End)
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

// --- Expression Evaluation ---

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

func (i *Interpreter) evalUnary(u *parser.Unary) (any, error) {
	val, err := i.evalPrimary(u.Value)
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

	default:
		return nil, errInvalidPrimaryExpression(p.Pos)
	}
}

/*
func (i *Interpreter) evalLiteral(l *parser.Literal) (any, error) {
	switch {
	case l.Int != nil:
		var n int
		if _, err := fmt.Sscanf(*l.Int, "%d", &n); err != nil {
			return nil, errInvalidIntLiteral(l.Pos, *l.Int)
		}
		return n, nil

	case l.Float != nil:
		var f float64
		if _, err := fmt.Sscanf(*l.Float, "%f", &f); err != nil {
			return nil, errInvalidFloatLiteral(l.Pos, *l.Float)
		}
		return f, nil

	case l.Str != nil:
		return *l.Str, nil

	case l.Bool != nil:
		switch *l.Bool {
		case "true":
			return true, nil
		case "false":
			return false, nil
		default:
			return nil, errInvalidBoolLiteral(l.Pos, *l.Bool)
		}

	default:
		return nil, errInvalidLiteral(l.Pos)
	}
}
*/

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

func (i *Interpreter) evalCall(c *parser.CallExpr) (any, error) {
	if c.Func == "print" {
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
