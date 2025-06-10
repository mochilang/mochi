package interpreter

import (
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"github.com/alecthomas/participle/v2/lexer"
	"github.com/fatih/color"
	"reflect"
	"strings"
	"sync"

	"mochi/parser"
	"mochi/runtime/agent"
	"mochi/runtime/data"
	"mochi/runtime/extern"
	mhttp "mochi/runtime/http"
	"mochi/runtime/llm"
	"mochi/runtime/stream"
	"mochi/types"
	"os"
	"time"
)

// Interpreter executes Mochi programs using a shared runtime and type environment.
type Interpreter struct {
	prog          *parser.Program
	env           *types.Env
	types         *types.Env
	streams       map[string]stream.Stream
	handlers      map[string][]onHandler
	subs          []stream.Subscriber
	cancels       []context.CancelFunc
	wg            *sync.WaitGroup
	agents        []*agent.Agent
	externTypes   map[string]*parser.ExternTypeDecl
	externVars    map[string]*parser.ExternVarDecl
	externFuncs   map[string]*parser.ExternFunDecl
	externObjects map[string]*parser.ExternObjectDecl
	memoize       bool
	memo          map[string]map[string]any
	dataPlan      string
}

func New(prog *parser.Program, typesEnv *types.Env) *Interpreter {
	return &Interpreter{
		prog: prog,
		// env:   types.NewEnv(nil),
		env:           typesEnv,
		types:         typesEnv,
		streams:       map[string]stream.Stream{},
		handlers:      map[string][]onHandler{},
		subs:          []stream.Subscriber{},
		cancels:       []context.CancelFunc{},
		wg:            &sync.WaitGroup{},
		agents:        []*agent.Agent{},
		externTypes:   map[string]*parser.ExternTypeDecl{},
		externVars:    map[string]*parser.ExternVarDecl{},
		externFuncs:   map[string]*parser.ExternFunDecl{},
		externObjects: map[string]*parser.ExternObjectDecl{},
		memoize:       false,
		memo:          map[string]map[string]any{},
		dataPlan:      "memory",
	}
}

func (i *Interpreter) SetProgram(prog *parser.Program) {
	i.prog = prog
}

// SetDataPlan chooses how dataset queries are executed. Valid options are
// "memory" and "duckdb".
func (i *Interpreter) SetDataPlan(plan string) {
	switch plan {
	case "duckdb":
		i.dataPlan = "duckdb"
	default:
		i.dataPlan = "memory"
	}
}

func (i *Interpreter) checkExternObjects() error {
	for name, decl := range i.externObjects {
		if _, ok := extern.Get(name); !ok {
			return errExternObject(decl.Pos, name)
		}
	}
	return nil
}

// SetMemoization enables or disables memoization of pure function calls.
func (i *Interpreter) SetMemoization(enable bool) {
	i.memoize = enable
	if !enable {
		i.memo = map[string]map[string]any{}
	}
}

func (i *Interpreter) Env() *types.Env { return i.env }

// EvalExpr evaluates a Mochi expression using the interpreter.
// This is exported for compile-time evaluation of pure expressions.
func (i *Interpreter) EvalExpr(e *parser.Expr) (any, error) {
	return i.evalExpr(e)
}

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
	defer i.Close()
	if err := i.checkExternObjects(); err != nil {
		return err
	}
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
	defer i.Close()
	if err := i.checkExternObjects(); err != nil {
		return err
	}
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

type agentInstance struct {
	decl  *parser.AgentDecl
	agent *agent.Agent
	env   *types.Env
}

type agentIntent struct {
	inst *agentInstance
	decl *parser.IntentDecl
}

type stringMethod struct {
	recv string
	name string
}

func (i *Interpreter) newAgentInstance(decl *parser.AgentDecl) (*agentInstance, error) {
	inst := &agentInstance{
		decl:  decl,
		agent: agent.New(agent.Config{Name: decl.Name, BufSize: 16}),
		env:   types.NewEnv(i.env),
	}

	for _, blk := range decl.Body {
		switch {
		case blk.Let != nil:
			val := any(nil)
			var err error
			if blk.Let.Value != nil {
				val, err = i.evalExpr(blk.Let.Value)
				if err != nil {
					return nil, err
				}
			}
			inst.env.SetValue(blk.Let.Name, val, false)

		case blk.Var != nil:
			val := any(nil)
			var err error
			if blk.Var.Value != nil {
				val, err = i.evalExpr(blk.Var.Value)
				if err != nil {
					return nil, err
				}
			}
			inst.env.SetValue(blk.Var.Name, val, true)
		}
	}

	for _, blk := range decl.Body {
		switch {
		case blk.On != nil:
			strm, ok := i.streams[blk.On.Stream]
			if !ok {
				return nil, fmt.Errorf("undefined stream: %s", blk.On.Stream)
			}
			alias := blk.On.Alias
			body := blk.On.Body
			inst.agent.On(strm, func(ctx context.Context, ev *stream.Event) {
				child := types.NewEnv(inst.env)
				child.SetValue(alias, ev.Data, true)
				interp := &Interpreter{prog: i.prog, env: child, types: i.types, streams: i.streams, handlers: i.handlers, subs: i.subs, cancels: i.cancels, wg: i.wg, agents: i.agents}
				for _, stmt := range body {
					_ = interp.evalStmt(stmt)
				}
			})

		case blk.Intent != nil:
			intent := blk.Intent
			inst.agent.RegisterIntent(intent.Name, func(ctx context.Context, args ...agent.Value) (agent.Value, error) {
				child := types.NewEnv(inst.env)
				for idx, p := range intent.Params {
					if idx < len(args) {
						child.SetValue(p.Name, args[idx], true)
					} else {
						child.SetValue(p.Name, nil, true)
					}
				}
				old := i.env
				i.env = child
				defer func() { i.env = old }()
				for _, stmt := range intent.Body {
					if err := i.evalStmt(stmt); err != nil {
						if r, ok := err.(returnSignal); ok {
							return r.value, nil
						}
						return nil, err
					}
				}
				return nil, nil
			})
		}
	}

	return inst, nil
}

func findIntent(decl *parser.AgentDecl, name string) *parser.IntentDecl {
	for _, blk := range decl.Body {
		if blk.Intent != nil && blk.Intent.Name == name {
			return blk.Intent
		}
	}
	return nil
}

type onHandler struct {
	alias string
	body  []*parser.Statement
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

	case s.While != nil:
		return i.evalWhile(s.While)

	case s.For != nil:
		return i.evalFor(s.For)

	case s.Break != nil:
		return breakSignal{}

	case s.Continue != nil:
		return continueSignal{}

	case s.Agent != nil:
		i.env.SetAgent(s.Agent.Name, s.Agent)
		return nil

	case s.Type != nil:
		// type declarations have no runtime effect
		return nil

	case s.ExternType != nil:
		// Placeholder for registering external types later
		i.externTypes[s.ExternType.Name] = s.ExternType
		return nil

	case s.ExternVar != nil:
		// Placeholder for registering external variables later
		i.externVars[s.ExternVar.Name] = s.ExternVar
		return nil

	case s.ExternFun != nil:
		// Placeholder for registering external functions later
		i.externFuncs[s.ExternFun.Name] = s.ExternFun
		return nil

	case s.ExternObject != nil:
		i.externObjects[s.ExternObject.Name] = s.ExternObject
		return nil

	case s.Stream != nil:
		i.streams[s.Stream.Name] = stream.New(s.Stream.Name, 64)
		return nil

	case s.On != nil:
		strm, ok := i.streams[s.On.Stream]
		if !ok {
			return fmt.Errorf("undefined stream: %s", s.On.Stream)
		}
		h := onHandler{alias: s.On.Alias, body: s.On.Body}
		i.handlers[s.On.Stream] = append(i.handlers[s.On.Stream], h)
		sub := strm.Subscribe(fmt.Sprintf("handler-%d", len(i.subs)), func(ev *stream.Event) error {
			data := ev.Data
			if st, ok := i.types.GetStream(s.On.Stream); ok {
				if m, ok2 := data.(map[string]any); ok2 {
					if _, has := m["__name"]; !has {
						if cv, err := castValue(s.On.Pos, st, m); err == nil {
							if mm, ok3 := cv.(map[string]any); ok3 {
								data = mm
							}
						}
					}
				}
			}
			child := types.NewEnv(i.env)
			child.SetValue(h.alias, data, true)
			interp := &Interpreter{prog: i.prog, env: child, types: i.types, streams: i.streams, handlers: i.handlers, subs: i.subs, cancels: i.cancels, wg: i.wg}
			for _, stmt := range h.body {
				if err := interp.evalStmt(stmt); err != nil {
					return err
				}
			}
			return nil
		})
		i.subs = append(i.subs, sub)
		return nil

	case s.Emit != nil:
		ev := map[string]any{}
		for _, f := range s.Emit.Fields {
			v, err := i.evalExpr(f.Value)
			if err != nil {
				return err
			}
			ev[f.Name] = v
		}
		if st, ok := i.types.GetStream(s.Emit.Stream); ok {
			if cv, err := castValue(s.Emit.Pos, st, ev); err == nil {
				if mm, ok2 := cv.(map[string]any); ok2 {
					ev = mm
				}
			}
		}
		strm, ok := i.streams[s.Emit.Stream]
		if !ok {
			return fmt.Errorf("undefined stream: %s", s.Emit.Stream)
		}
		if _, err := strm.Emit(context.Background(), ev); err != nil {
			return err
		}
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

func (i *Interpreter) evalWhile(stmt *parser.WhileStmt) error {
	// Create loop scope
	child := types.NewEnv(i.env)
	old := i.env
	i.env = child
	defer func() { i.env = old }()

	for {
		condVal, err := i.evalExpr(stmt.Cond)
		if err != nil {
			return err
		}
		if !truthy(condVal) {
			return nil
		}

		cont, err := i.execLoopBody(stmt.Body)
		if err != nil {
			if _, ok := err.(breakSignal); ok {
				return nil
			}
			return err
		}
		if cont {
			continue
		}
	}
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
			cont, err := i.execLoopBody(stmt.Body)
			if err != nil {
				if _, ok := err.(breakSignal); ok {
					return nil
				}
				return err
			}
			if cont {
				continue
			}
		}
		return nil
	}

	// --- Collection loop: `for x in list/map/string` ---
	err = i.forEach(stmt.Pos, fromVal, func(item any) (bool, error) {
		child.SetValue(stmt.Name, item, true)
		return i.execLoopBody(stmt.Body)
	})
	if err != nil {
		if _, ok := err.(breakSignal); ok {
			return nil
		}
		return err
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
		{"==", "!=", "in"},     // equality and membership
		{"&&"},                 // logical AND
		{"||"},                 // logical OR
		{"union", "union_all", "except", "intersect"}, // set unions, intersection, and difference lowest
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

	for _, op := range p.Ops {
		switch {
		case op.Index != nil:
			val, err = i.applyIndex(val, op.Index)
		case op.Call != nil:
			val, err = i.applyCallOp(val, op.Call)
		case op.Cast != nil:
			val, err = i.applyCast(val, op.Cast)
		}
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
		return closure{Name: "", Fn: p.FunExpr, Env: i.env.Copy(), FullParams: p.FunExpr.Params}, nil

	case p.Selector != nil:
		val, err := i.env.GetValue(p.Selector.Root)
		if err != nil {
			if v, ok := extern.Get(p.Selector.Root); ok {
				val = v
			} else if _, ok := i.externObjects[p.Selector.Root]; ok {
				return nil, errExternObject(p.Pos, p.Selector.Root)
			} else if fn, ok := i.env.GetFunc(p.Selector.Root); ok {
				cl := closure{Name: p.Selector.Root, Fn: &parser.FunExpr{Params: fn.Params, Return: fn.Return, BlockBody: fn.Body}, Env: i.env.Copy(), FullParams: fn.Params}
				return cl, nil
			} else if _, ok := i.types.FindUnionByVariant(p.Selector.Root); ok {
				val = map[string]any{"__name": p.Selector.Root}
			} else {
				return nil, errUndefinedVariable(p.Pos, p.Selector.Root)
			}
		}
		for _, field := range p.Selector.Tail {
			if g, ok := val.(*data.Group); ok {
				switch field {
				case "key":
					val = g.Key
					continue
				case "values":
					val = g.Items
					continue
				}
			}
			if inst, ok := val.(*agentInstance); ok {
				if v, err := inst.env.GetValue(field); err == nil {
					val = v
					continue
				}
				if intent := findIntent(inst.decl, field); intent != nil {
					val = agentIntent{inst: inst, decl: intent}
					continue
				}
				val = nil
				continue
			}
			if str, ok := val.(string); ok {
				switch field {
				case "contains":
					val = stringMethod{recv: str, name: "contains"}
					continue
				default:
					val = nil
					continue
				}
			}
			obj, ok := val.(map[string]any)
			if !ok {
				return nil, errFieldAccessOnNonObject(p.Pos, field, fmt.Sprintf("%T", val))
			}
			if v, ok := obj[field]; ok {
				val = v
				continue
			}
			name, _ := obj["__name"].(string)
			if st, ok := i.types.GetStruct(name); ok {
				if m, ok := st.Methods[field]; ok {
					env := i.env.Copy()
					for k, v := range obj {
						if k == "__name" {
							continue
						}
						env.SetValue(k, v, true)
					}
					cl := closure{Name: field, Fn: &parser.FunExpr{Params: m.Decl.Params, Return: m.Decl.Return, BlockBody: m.Decl.Body}, Env: env, FullParams: m.Decl.Params}
					val = cl
					continue
				}
			}
			val = nil
		}
		return val, nil

	case p.Struct != nil:
		if decl, ok := i.env.GetAgent(p.Struct.Name); ok {
			inst, err := i.newAgentInstance(decl)
			if err != nil {
				return nil, err
			}
			i.agents = append(i.agents, inst.agent)
			inst.agent.Start(context.Background())
			return inst, nil
		}
		obj := map[string]any{"__name": p.Struct.Name}
		for _, field := range p.Struct.Fields {
			v, err := i.evalExpr(field.Value)
			if err != nil {
				return nil, err
			}
			obj[field.Name] = v
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
			var strKey string
			if k, ok := simpleStringKey(item.Key); ok {
				strKey = k
			} else {
				keyVal, err := i.evalExpr(item.Key)
				if err != nil {
					return nil, err
				}
				s, ok := keyVal.(string)
				if !ok {
					return nil, errInvalidMapKey(item.Pos, keyVal)
				}
				strKey = s
			}
			val, err := i.evalExpr(item.Value)
			if err != nil {
				return nil, err
			}
			obj[strKey] = val
		}
		return obj, nil

	case p.Query != nil:
		eng := data.EngineByName(i.dataPlan)
		return data.RunQuery(p.Query, i.env, eng, func(env *types.Env, e *parser.Expr) (any, error) {
			old := i.env
			i.env = env
			val, err := i.evalExpr(e)
			i.env = old
			return val, err
		})

	case p.Match != nil:
		return i.evalMatch(p.Match)

	case p.Fetch != nil:
		urlVal, err := i.evalExpr(p.Fetch.URL)
		if err != nil {
			return nil, err
		}
		urlStr, ok := urlVal.(string)
		if !ok {
			return nil, fmt.Errorf("fetch URL must be a string")
		}
		var opts map[string]any
		if p.Fetch.With != nil {
			v, err := i.evalExpr(p.Fetch.With)
			if err != nil {
				return nil, err
			}
			opts = toAnyMap(v)
		}
		return mhttp.FetchWith(urlStr, opts)

	case p.Load != nil:
		format := "csv"
		if p.Load.With != nil {
			v, err := i.evalExpr(p.Load.With)
			if err != nil {
				return nil, err
			}
			opts := toAnyMap(v)
			if f, ok := opts["format"].(string); ok {
				format = f
			}
		}

		var (
			rows []map[string]any
			err  error
		)
		switch format {
		case "jsonl":
			if p.Load.Path == nil {
				rows, err = data.LoadJSONLReader(os.Stdin)
			} else {
				rows, err = data.LoadJSONL(*p.Load.Path)
			}
		default:
			if p.Load.Path == nil {
				rows, err = data.LoadCSVReader(os.Stdin)
			} else {
				rows, err = data.LoadCSV(*p.Load.Path)
			}
		}
		if err != nil {
			return nil, err
		}
		items := make([]any, len(rows))
		var typ types.Type
		if p.Load.Type != nil {
			typ = resolveTypeRef(p.Load.Type, i.types)
		}
		for idx, row := range rows {
			v := any(row)
			if p.Load.Type != nil {
				cv, err := castValue(p.Load.Pos, typ, row)
				if err != nil {
					return nil, err
				}
				v = cv
			}
			items[idx] = v
		}
		return items, nil

	case p.Save != nil:
		srcVal, err := i.evalExpr(p.Save.Src)
		if err != nil {
			return nil, err
		}
		format := "csv"
		header := false
		delim := ','
		if p.Save.With != nil {
			v, err := i.evalExpr(p.Save.With)
			if err != nil {
				return nil, err
			}
			opts := toAnyMap(v)
			if f, ok := opts["format"].(string); ok {
				format = f
			}
			if h, ok := opts["header"].(bool); ok {
				header = h
			}
			if d, ok := opts["delimiter"].(string); ok && len(d) > 0 {
				delim = rune(d[0])
			}
		}
		rows, ok := toMapSlice(srcVal)
		if !ok {
			return nil, fmt.Errorf("save source must be list of maps")
		}
		switch format {
		case "jsonl":
			if p.Save.Path == nil {
				err = data.SaveJSONLWriter(rows, os.Stdout)
			} else {
				err = data.SaveJSONL(rows, *p.Save.Path)
			}
		default:
			if p.Save.Path == nil {
				err = data.SaveCSVWriter(rows, os.Stdout, header, delim)
			} else {
				err = data.SaveCSV(rows, *p.Save.Path, header, delim)
			}
		}
		if err != nil {
			return nil, err
		}
		return nil, nil

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

		argVals := make([]any, argCount)
		for idx := range c.Args {
			v, err := i.evalExpr(c.Args[idx])
			if err != nil {
				return nil, err
			}
			argVals[idx] = v
		}

		if argCount < paramCount {
			applied := make([]Value, argCount)
			for idx := range argVals {
				applied[idx] = anyToValue(argVals[idx])
			}
			remainingParams := fn.Params[argCount:]
			return closure{
				Name: c.Func,
				Fn: &parser.FunExpr{
					Params:    remainingParams,
					BlockBody: fn.Body,
				},
				Env:        i.env.Copy(),
				Args:       applied,
				FullParams: fn.Params,
			}, nil
		}

		var pure bool
		var key string
		if t, err := i.types.GetVar(c.Func); err == nil {
			if ft, ok := t.(types.FuncType); ok && ft.Pure {
				pure = true
				key = argsKey(argVals)
				if i.memoize {
					if fnCache, ok := i.memo[c.Func]; ok {
						if res, ok := fnCache[key]; ok {
							return res, nil
						}
					}
				}
			}
		}

		child := types.NewEnv(i.env)
		for idx, param := range fn.Params {
			child.SetValue(param.Name, argVals[idx], true)
		}
		old := i.env
		i.env = child
		defer func() { i.env = old }()
		var ret any
		for _, stmt := range fn.Body {
			if err := i.evalStmt(stmt); err != nil {
				if r, ok := err.(returnSignal); ok {
					ret = r.value
					err = nil
					break
				}
				return nil, err
			}
		}
		if pure && i.memoize {
			if _, ok := i.memo[c.Func]; !ok {
				i.memo[c.Func] = map[string]any{}
			}
			i.memo[c.Func][key] = ret
		}
		return ret, nil
	}

	val, err := i.env.GetValue(c.Func)
	if err == nil {
		switch fn := val.(type) {
		case closure:
			cl := fn
			totalArgs := len(cl.Args) + len(c.Args)
			fullParamCount := len(cl.FullParams)

			if totalArgs > fullParamCount {
				return nil, errTooManyFunctionArgs(c.Pos, c.Func, fullParamCount, totalArgs)
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

		case agentIntent:
			args := make([]agent.Value, len(c.Args))
			for idx, arg := range c.Args {
				v, err := i.evalExpr(arg)
				if err != nil {
					return nil, err
				}
				args[idx] = v
			}
			return fn.inst.agent.Call(context.Background(), fn.decl.Name, args...)
		}
	}

	return nil, errUndefinedFunctionOrClosure(c.Pos, c.Func)
}

// --- Return ---

type returnSignal struct{ value any }

func (r returnSignal) Error() string { return "return" }

type breakSignal struct{}

func (b breakSignal) Error() string { return "break" }

type continueSignal struct{}

func (c continueSignal) Error() string { return "continue" }

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
		case "union_all":
			return Value{Tag: TagList, List: append(append([]Value{}, left.List...), right.List...)}, nil
		case "union":
			merged := append([]Value{}, left.List...)
			for _, rv := range right.List {
				found := false
				for _, lv := range merged {
					eq, err := applyBinaryValue(pos, lv, "==", rv)
					if err == nil && eq.Tag == TagBool && eq.Bool {
						found = true
						break
					}
					if err != nil && reflect.DeepEqual(valueToAny(lv), valueToAny(rv)) {
						found = true
						break
					}
				}
				if !found {
					merged = append(merged, rv)
				}
			}
			return Value{Tag: TagList, List: merged}, nil
		case "except":
			diff := []Value{}
			for _, lv := range left.List {
				found := false
				for _, rv := range right.List {
					eq, err := applyBinaryValue(pos, lv, "==", rv)
					if err == nil && eq.Tag == TagBool && eq.Bool {
						found = true
						break
					}
					if err != nil && reflect.DeepEqual(valueToAny(lv), valueToAny(rv)) {
						found = true
						break
					}
				}
				if !found {
					diff = append(diff, lv)
				}
			}
			return Value{Tag: TagList, List: diff}, nil
		case "intersect":
			inter := []Value{}
			for _, lv := range left.List {
				match := false
				for _, rv := range right.List {
					eq, err := applyBinaryValue(pos, lv, "==", rv)
					if err == nil && eq.Tag == TagBool && eq.Bool {
						match = true
						break
					}
					if err != nil && reflect.DeepEqual(valueToAny(lv), valueToAny(rv)) {
						match = true
						break
					}
				}
				if match {
					// avoid duplicates in result
					exists := false
					for _, iv := range inter {
						eq, err := applyBinaryValue(pos, iv, "==", lv)
						if err == nil && eq.Tag == TagBool && eq.Bool {
							exists = true
							break
						}
						if err != nil && reflect.DeepEqual(valueToAny(iv), valueToAny(lv)) {
							exists = true
							break
						}
					}
					if !exists {
						inter = append(inter, lv)
					}
				}
			}
			return Value{Tag: TagList, List: inter}, nil
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
			case "in":
				return Value{Tag: TagBool, Bool: strings.Contains(right.Str, left.Str)}, nil
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

// execLoopBody executes a sequence of statements that form the body of a loop.
// It returns true if a `continue` statement was encountered. Any `break` or
// `return` signals are passed back to the caller as errors.
func (i *Interpreter) execLoopBody(body []*parser.Statement) (bool, error) {
	for _, s := range body {
		if err := i.evalStmt(s); err != nil {
			switch err.(type) {
			case continueSignal:
				return true, nil
			case breakSignal, returnSignal:
				return false, err
			default:
				return false, err
			}
		}
	}
	return false, nil
}

// forEach iterates over any supported collection type and invokes fn for each
// item. The callback may return a `continue` flag to skip to the next iteration.
// Errors, including break and return signals, are propagated to the caller.
func (i *Interpreter) forEach(pos lexer.Position, src any, fn func(any) (bool, error)) error {
	switch coll := src.(type) {
	case []any:
		for _, item := range coll {
			cont, err := fn(item)
			if err != nil {
				return err
			}
			if cont {
				continue
			}
		}
	case *data.Group:
		for _, item := range coll.Items {
			cont, err := fn(item)
			if err != nil {
				return err
			}
			if cont {
				continue
			}
		}
	case map[any]any:
		for k := range coll {
			cont, err := fn(k)
			if err != nil {
				return err
			}
			if cont {
				continue
			}
		}
	case map[string]any:
		for k := range coll {
			cont, err := fn(k)
			if err != nil {
				return err
			}
			if cont {
				continue
			}
		}
	case map[int]any:
		for k := range coll {
			cont, err := fn(k)
			if err != nil {
				return err
			}
			if cont {
				continue
			}
		}
	case string:
		for _, r := range coll {
			cont, err := fn(string(r))
			if err != nil {
				return err
			}
			if cont {
				continue
			}
		}
	default:
		return errInvalidIterator(pos, fmt.Sprintf("%T", src))
	}
	return nil
}

func castValue(pos lexer.Position, t types.Type, v any) (any, error) {
	switch tt := t.(type) {
	case types.IntType, types.Int64Type:
		switch x := v.(type) {
		case int:
			return x, nil
		case float64:
			return int(x), nil
		default:
			return nil, errCastType(pos, v, t)
		}
	case types.FloatType:
		switch x := v.(type) {
		case float64:
			return x, nil
		case int:
			return float64(x), nil
		default:
			return nil, errCastType(pos, v, t)
		}
	case types.StringType:
		if s, ok := v.(string); ok {
			return s, nil
		}
		return nil, errCastType(pos, v, t)
	case types.BoolType:
		if b, ok := v.(bool); ok {
			return b, nil
		}
		return nil, errCastType(pos, v, t)
	case types.ListType:
		list, ok := v.([]any)
		if !ok {
			return nil, errCastType(pos, v, t)
		}
		out := make([]any, len(list))
		for i, item := range list {
			cv, err := castValue(pos, tt.Elem, item)
			if err != nil {
				return nil, err
			}
			out[i] = cv
		}
		return out, nil
	case types.MapType:
		m, ok := v.(map[string]any)
		if !ok {
			return nil, errCastType(pos, v, t)
		}
		out := map[string]any{}
		for k, val := range m {
			cv, err := castValue(pos, tt.Value, val)
			if err != nil {
				return nil, err
			}
			out[k] = cv
		}
		return out, nil
	case types.StructType:
		m, ok := v.(map[string]any)
		if !ok {
			return nil, errCastType(pos, v, t)
		}
		out := map[string]any{"__name": tt.Name}
		for name, ft := range tt.Fields {
			fv, ok := m[name]
			if !ok {
				return nil, errCastMissingField(pos, name, tt.Name)
			}
			cv, err := castValue(pos, ft, fv)
			if err != nil {
				return nil, err
			}
			out[name] = cv
		}
		return out, nil
	default:
		return v, nil
	}
}

func resolveTypeRef(t *parser.TypeRef, env *types.Env) types.Type {
	if t.Fun != nil {
		params := make([]types.Type, len(t.Fun.Params))
		for i, p := range t.Fun.Params {
			params[i] = resolveTypeRef(p, env)
		}
		var ret types.Type = types.VoidType{}
		if t.Fun.Return != nil {
			ret = resolveTypeRef(t.Fun.Return, env)
		}
		return types.FuncType{Params: params, Return: ret}
	}
	if t.Generic != nil {
		switch t.Generic.Name {
		case "list":
			if len(t.Generic.Args) == 1 {
				return types.ListType{Elem: resolveTypeRef(t.Generic.Args[0], env)}
			}
		case "map":
			if len(t.Generic.Args) == 2 {
				return types.MapType{Key: resolveTypeRef(t.Generic.Args[0], env), Value: resolveTypeRef(t.Generic.Args[1], env)}
			}
		}
		return types.AnyType{}
	}
	if t.Simple != nil {
		switch *t.Simple {
		case "int":
			return types.IntType{}
		case "float":
			return types.FloatType{}
		case "string":
			return types.StringType{}
		case "bool":
			return types.BoolType{}
		default:
			if st, ok := env.GetStruct(*t.Simple); ok {
				return st
			}
			if ut, ok := env.GetUnion(*t.Simple); ok {
				return ut
			}
		}
	}
	return types.AnyType{}
}

func toAnyMap(m any) map[string]any {
	switch v := m.(type) {
	case map[string]any:
		return v
	case map[string]string:
		out := make(map[string]any, len(v))
		for k, vv := range v {
			out[k] = vv
		}
		return out
	default:
		return nil
	}
}

func toMapSlice(v any) ([]map[string]any, bool) {
	switch rows := v.(type) {
	case []map[string]any:
		return rows, true
	case []any:
		out := make([]map[string]any, len(rows))
		for i, item := range rows {
			m, ok := item.(map[string]any)
			if !ok {
				return nil, false
			}
			out[i] = m
		}
		return out, true
	default:
		return nil, false
	}
}

func simpleStringKey(e *parser.Expr) (string, bool) {
	if e == nil {
		return "", false
	}
	if len(e.Binary.Right) != 0 {
		return "", false
	}
	u := e.Binary.Left
	if len(u.Ops) != 0 {
		return "", false
	}
	p := u.Value
	if len(p.Ops) != 0 {
		return "", false
	}
	if p.Target.Selector != nil && len(p.Target.Selector.Tail) == 0 {
		return p.Target.Selector.Root, true
	}
	if p.Target.Lit != nil && p.Target.Lit.Str != nil {
		return *p.Target.Lit.Str, true
	}
	return "", false
}

func (i *Interpreter) Close() {
	for _, cancel := range i.cancels {
		cancel()
	}
	i.wg.Wait()
	for _, s := range i.streams {
		s.Close()
	}
	for _, a := range i.agents {
		a.Stop()
	}
}
