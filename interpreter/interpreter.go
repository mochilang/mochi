package interpreter

import (
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"github.com/alecthomas/participle/v2/lexer"
	"github.com/fatih/color"
	"path/filepath"
	"sort"
	"strings"
	"sync"
	"sync/atomic"

	"mochi/parser"
	"mochi/runtime/agent"
	"mochi/runtime/data"
	ffi "mochi/runtime/ffi"
	"mochi/runtime/ffi/extern"
	mhttp "mochi/runtime/http"
	"mochi/runtime/llm"
	"mochi/runtime/logic"
	"mochi/runtime/stream"
	"mochi/types"
	"os"
	"time"
)

// Interpreter executes Mochi programs using a shared runtime and type environment.
type Interpreter struct {
	prog             *parser.Program
	env              *types.Env
	types            *types.Env
	streams          map[string]stream.Stream
	handlers         map[string][]onHandler
	subs             []stream.Subscriber
	cancels          []context.CancelFunc
	wg               *sync.WaitGroup
	agents           []*agent.Agent
	ffi              *ffi.Manager
	root             string
	memoize          bool
	memo             map[string]map[string]any
	dataPlan         string
	tx               *atomic.Int64
	logic            *logic.Engine
	inlineCandidates map[string]*parser.FunStmt
	callCounts       map[string]int
}

func New(prog *parser.Program, typesEnv *types.Env, root string) *Interpreter {
	return &Interpreter{
		prog:             prog,
		env:              typesEnv,
		types:            typesEnv,
		streams:          map[string]stream.Stream{},
		handlers:         map[string][]onHandler{},
		subs:             []stream.Subscriber{},
		cancels:          []context.CancelFunc{},
		wg:               &sync.WaitGroup{},
		agents:           []*agent.Agent{},
		ffi:              ffi.NewManager(),
		root:             root,
		memoize:          false,
		memo:             map[string]map[string]any{},
		dataPlan:         "memory",
		tx:               &atomic.Int64{},
		logic:            logic.NewEngine(),
		inlineCandidates: map[string]*parser.FunStmt{},
		callCounts:       map[string]int{},
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
	if err := i.ffi.CheckExternObjects(); err != nil {
		// Error positions are stored in the manager, so just forward.
		return err
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
	return d.String()
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

// RunResult executes the program like Run but returns the value of the last
// expression statement, if any. Declarations and other statements behave the
// same as in Run.
func (i *Interpreter) RunResult() (any, error) {
	defer i.Close()
	if err := i.checkExternObjects(); err != nil {
		return nil, err
	}

	var result any
	for idx, stmt := range i.prog.Statements {
		if stmt.Test != nil {
			continue
		}
		if stmt.Expr != nil && idx == len(i.prog.Statements)-1 {
			v, err := i.evalExpr(stmt.Expr.Expr)
			if err != nil {
				return nil, err
			}
			result = v
			continue
		}
		if err := i.evalStmt(stmt); err != nil {
			return nil, err
		}
	}

	return result, nil
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
			prog:             i.prog,
			env:              child,
			types:            i.types,
			tx:               i.tx,
			inlineCandidates: map[string]*parser.FunStmt{},
			callCounts:       map[string]int{},
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

type Closure struct {
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

// importPackage loads a Mochi package from a directory and binds it to alias.
func (i *Interpreter) importPackage(alias, path, filename string) error {
	p := strings.Trim(path, "\"")
	base := i.root
	if strings.HasPrefix(p, "./") || strings.HasPrefix(p, "../") {
		base = filepath.Dir(filename)
	}
	target := filepath.Join(base, p)

	info, err := os.Stat(target)
	if err != nil {
		if os.IsNotExist(err) && !strings.HasSuffix(target, ".mochi") {
			if fi, err2 := os.Stat(target + ".mochi"); err2 == nil {
				info = fi
				target += ".mochi"
			} else {
				return fmt.Errorf("import package: %w", err)
			}
		} else {
			return fmt.Errorf("import package: %w", err)
		}
	}

	var files []string
	if info.IsDir() {
		entries, err := os.ReadDir(target)
		if err != nil {
			return fmt.Errorf("import package: %w", err)
		}
		for _, e := range entries {
			if !e.IsDir() && strings.HasSuffix(e.Name(), ".mochi") {
				files = append(files, filepath.Join(target, e.Name()))
			}
		}
		sort.Strings(files)
	} else {
		files = []string{target}
	}
	pkgEnv := types.NewEnv(i.env)
	var stmts []*parser.Statement
	pkgName := alias
	for _, f := range files {
		prog, err := parser.Parse(f)
		if err != nil {
			return err
		}
		if prog.Package != "" {
			pkgName = prog.Package
		}
		stmts = append(stmts, prog.Statements...)
	}

	oldProg := i.prog
	oldEnv := i.env
	i.env = pkgEnv
	for _, s := range stmts {
		if err := i.evalStmt(s); err != nil {
			i.prog = oldProg
			i.env = oldEnv
			return err
		}
	}
	i.env = oldEnv
	i.prog = oldProg

	obj := map[string]any{"__name": pkgName}
	for _, s := range stmts {
		if s.Fun != nil && s.Fun.Export {
			cl := Closure{Name: s.Fun.Name, Fn: &parser.FunExpr{Params: s.Fun.Params, Return: s.Fun.Return, BlockBody: s.Fun.Body}, Env: pkgEnv.Copy(), FullParams: s.Fun.Params}
			obj[s.Fun.Name] = cl
		}
	}
	i.env.SetValue(alias, obj, false)
	return nil
}

func (i *Interpreter) newAgentInstance(decl *parser.AgentDecl) (*agentInstance, error) {
	inst := &agentInstance{
		decl:  decl,
		agent: agent.New(agent.Config{Name: decl.Name, BufSize: 16, WG: i.wg}),
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
				interp := &Interpreter{prog: i.prog, env: child, types: i.types, streams: i.streams, handlers: i.handlers, subs: i.subs, cancels: i.cancels, wg: i.wg, agents: i.agents, tx: i.tx, inlineCandidates: map[string]*parser.FunStmt{}, callCounts: map[string]int{}}
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

func (c Closure) String() string {
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
			if n, ok := toInt(val); ok {
				val = n
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
			if n, ok := toInt(val); ok {
				val = n
			}
		}
		i.env.SetValue(s.Var.Name, val, true)
		return nil

	case s.Assign != nil:
		val, err := i.evalExpr(s.Assign.Value)
		if err != nil {
			return err
		}
		if len(s.Assign.Index) == 0 {
			return i.env.UpdateValue(s.Assign.Name, val)
		}
		mutable, err := i.env.IsMutable(s.Assign.Name)
		if err != nil {
			return err
		}
		if !mutable {
			return fmt.Errorf("cannot assign to immutable variable: %s", s.Assign.Name)
		}
		target, err := i.env.GetValue(s.Assign.Name)
		if err != nil {
			return err
		}
		container := target
		for idx, op := range s.Assign.Index {
			idxVal, err := i.evalExpr(op.Start)
			if err != nil {
				return err
			}
			last := idx == len(s.Assign.Index)-1

			switch cur := container.(type) {
			case map[string]any:
				key, ok := idxVal.(string)
				if !ok {
					return fmt.Errorf("map key must be string")
				}
				if last {
					cur[key] = val
					return nil
				}
				container = cur[key]
			case map[int]any:
				var k int
				switch v := idxVal.(type) {
				case int:
					k = v
				case int64:
					k = int(v)
				default:
					return fmt.Errorf("map key must be int")
				}
				if last {
					cur[k] = val
					return nil
				}
				container = cur[k]
			case map[any]any:
				if last {
					cur[idxVal] = val
					return nil
				}
				container = cur[idxVal]
			case []any:
				n, ok := idxVal.(int)
				if !ok {
					return fmt.Errorf("list index must be int")
				}
				if n < 0 || n >= len(cur) {
					return fmt.Errorf("index out of range")
				}
				if last {
					cur[n] = val
					return nil
				}
				container = cur[n]
			default:
				return fmt.Errorf("%s is not indexable", s.Assign.Name)
			}
		}
		return nil

	case s.Expr != nil:
		_, err := i.evalExpr(s.Expr.Expr)
		return err

	case s.Fun != nil:
		i.env.SetFunc(s.Fun.Name, s.Fun)
		if len(s.Fun.Body) == 1 {
			i.inlineCandidates[s.Fun.Name] = s.Fun
		}
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

	case s.Fetch != nil:
		urlVal, err := i.evalExpr(s.Fetch.URL)
		if err != nil {
			return err
		}
		urlStr, ok := urlVal.(string)
		if !ok {
			return fmt.Errorf("fetch URL must be a string")
		}
		var opts map[string]any
		if s.Fetch.With != nil {
			v, err := i.evalExpr(s.Fetch.With)
			if err != nil {
				return err
			}
			opts = toAnyMap(v)
		}
		val, err := mhttp.FetchWith(urlStr, opts)
		if err != nil {
			return err
		}
		i.env.SetValue(s.Fetch.Target, val, false)
		return nil

	case s.Agent != nil:
		i.env.SetAgent(s.Agent.Name, s.Agent)
		return nil

	case s.Type != nil:
		// type declarations have no runtime effect
		return nil

	case s.Import != nil:
		alias := s.Import.As
		if alias == "" {
			alias = parser.AliasFromPath(s.Import.Path)
		}
		if s.Import.Lang == nil {
			return i.importPackage(alias, s.Import.Path, s.Pos.Filename)
		}
		return i.ffi.Import(*s.Import.Lang, alias, s.Import.Path, i.root, s.Import.Auto)

	case s.ExternType != nil:
		// type declarations have no runtime effect
		return nil

	case s.ExternVar != nil:
		return nil

	case s.ExternFun != nil:
		return nil

	case s.ExternObject != nil:
		i.ffi.DeclareExternObject(s.ExternObject)
		return nil

	case s.Fact != nil:
		pred := convertLogicPredicate(s.Fact.Pred)
		i.logic.AddFact(pred)
		return nil

	case s.Rule != nil:
		rule := convertLogicRule(s.Rule)
		i.logic.AddRule(rule)
		return nil

	case s.Stream != nil:
		i.streams[s.Stream.Name] = stream.NewWithCounter(s.Stream.Name, 64, i.tx, i.wg)
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
			interp := &Interpreter{prog: i.prog, env: child, types: i.types, streams: i.streams, handlers: i.handlers, subs: i.subs, cancels: i.cancels, wg: i.wg, tx: i.tx, inlineCandidates: map[string]*parser.FunStmt{}, callCounts: map[string]int{}}
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
		// Wait until all subscribers have processed the event to ensure
		// deterministic ordering for tests.
		if w, ok := strm.(interface{ Wait() }); ok {
			w.Wait()
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
		interp := &Interpreter{prog: i.prog, env: child, types: i.types, tx: i.tx, inlineCandidates: map[string]*parser.FunStmt{}, callCounts: map[string]int{}}
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
		fromInt, ok1 := toInt(fromVal)
		toIntVal, ok2 := toInt(toVal)
		if !ok1 || !ok2 {
			return errInvalidRangeBounds(stmt.Pos, fmt.Sprintf("%T", fromVal), fmt.Sprintf("%T", toVal))
		}
		for x := fromInt; x < toIntVal; x++ {
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

	// Lazy operand evaluation to support short-circuit logic
	type operand struct {
		eval func() (any, error)
		val  any
		done bool
	}
	valueOf := func(o *operand) (any, error) {
		if o.done {
			return o.val, nil
		}
		v, err := o.eval()
		if err != nil {
			return nil, err
		}
		o.val = v
		o.done = true
		return v, nil
	}

	type token struct {
		pos lexer.Position
		op  string
	}

	var operands []operand
	var operators []token

	// Initial left expression (eager)
	left, err := i.evalUnary(b.Left)
	if err != nil {
		return nil, err
	}
	operands = append(operands, operand{val: left, done: true})

	for _, part := range b.Right {
		p := part
		operators = append(operators, token{p.Pos, p.Op})
		// Each binary operation's right side is a postfix expression.
		operands = append(operands, operand{eval: func() (any, error) { return i.evalPostfixExpr(p.Right) }})
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
				leftVal, err := valueOf(&operands[i])
				if err != nil {
					return nil, err
				}

				// Short-circuit evaluation for logical operators
				if op == "&&" {
					if lb, ok := leftVal.(bool); ok && !lb {
						operands[i] = operand{val: false, done: true}
						operands = append(operands[:i+1], operands[i+2:]...)
						operators = append(operators[:i], operators[i+1:]...)
						continue
					}
				} else if op == "||" {
					if lb, ok := leftVal.(bool); ok && lb {
						operands[i] = operand{val: true, done: true}
						operands = append(operands[:i+1], operands[i+2:]...)
						operators = append(operators[:i], operators[i+1:]...)
						continue
					}
				}

				rightVal, err := valueOf(&operands[i+1])
				if err != nil {
					return nil, err
				}
				result, err := applyBinary(operators[i].pos, leftVal, op, rightVal)
				if err != nil {
					return nil, err
				}
				operands[i] = operand{val: result, done: true}
				operands = append(operands[:i+1], operands[i+2:]...)
				operators = append(operators[:i], operators[i+1:]...)
			} else {
				i++
			}
		}
	}

	if len(operands) != 1 {
		return nil, fmt.Errorf("unexpected state after binary eval")
	}
	return valueOf(&operands[0])
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
	return i.ffi.Final(val)
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
		return Closure{Name: "", Fn: p.FunExpr, Env: i.env.Copy(), FullParams: p.FunExpr.Params}, nil

	case p.If != nil:
		return i.evalIfExpr(p.If)

	case p.Selector != nil:
		val, err := i.env.GetValue(p.Selector.Root)
		if err != nil {
			if v, ok := extern.Get(p.Selector.Root); ok {
				val = v
			} else if mval, ok := i.ffi.Lookup(p.Selector.Root); ok {
				val = mval
			} else if i.ffi.IsExternObjectDeclared(p.Selector.Root) {
				return nil, errExternObject(p.Pos, p.Selector.Root)
			} else if fn, ok := i.env.GetFunc(p.Selector.Root); ok {
				cl := Closure{Name: p.Selector.Root, Fn: &parser.FunExpr{Params: fn.Params, Return: fn.Return, BlockBody: fn.Body}, Env: i.env.Copy(), FullParams: fn.Params}
				return cl, nil
			} else if _, ok := i.types.FindUnionByVariant(p.Selector.Root); ok {
				val = map[string]any{"__name": p.Selector.Root}
			} else {
				return nil, errUndefinedVariable(p.Pos, p.Selector.Root)
			}
		}
		for _, field := range p.Selector.Tail {
			if nv, ok := i.ffi.Append(val, field); ok {
				val = nv
				continue
			}
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
					cl := Closure{Name: field, Fn: &parser.FunExpr{Params: m.Decl.Params, Return: m.Decl.Return, BlockBody: m.Decl.Body}, Env: env, FullParams: m.Decl.Params}
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
		var (
			kind   string
			objStr map[string]any
			objInt map[int]any
			objAny map[any]any
		)
		for _, item := range p.Map.Items {
			var key any
			if k, ok := simpleStringKey(item.Key); ok {
				key = k
			} else {
				kv, err := i.evalExpr(item.Key)
				if err != nil {
					return nil, err
				}
				switch v := kv.(type) {
				case string:
					key = v
				case int:
					key = v
				case int64:
					key = int(v)
				default:
					return nil, errInvalidMapKey(item.Pos, kv)
				}
			}

			val, err := i.evalExpr(item.Value)
			if err != nil {
				return nil, err
			}

			switch k := key.(type) {
			case string:
				switch kind {
				case "":
					kind = "string"
					objStr = map[string]any{}
				case "int":
					if objAny == nil {
						objAny = make(map[any]any, len(objInt))
						for kk, vv := range objInt {
							objAny[kk] = vv
						}
					}
					kind = "any"
				}
				if kind == "string" {
					objStr[k] = val
				} else {
					objAny[k] = val
				}
			case int:
				switch kind {
				case "":
					kind = "int"
					objInt = map[int]any{}
				case "string":
					if objAny == nil {
						objAny = make(map[any]any, len(objStr))
						for kk, vv := range objStr {
							objAny[kk] = vv
						}
					}
					kind = "any"
				}
				if kind == "int" {
					objInt[k] = val
				} else {
					objAny[k] = val
				}
			default:
				if objAny == nil {
					switch kind {
					case "string":
						objAny = make(map[any]any, len(objStr))
						for kk, vv := range objStr {
							objAny[kk] = vv
						}
					case "int":
						objAny = make(map[any]any, len(objInt))
						for kk, vv := range objInt {
							objAny[kk] = vv
						}
					default:
						objAny = map[any]any{}
					}
					kind = "any"
				}
				objAny[k] = val
			}
		}
		switch kind {
		case "string":
			return objStr, nil
		case "int":
			return objInt, nil
		default:
			if objAny == nil {
				objAny = map[any]any{}
			}
			return objAny, nil
		}

	case p.Query != nil:
		eng := data.EngineByName(i.dataPlan)
		return data.RunQuery(p.Query, i.env, eng, func(env *types.Env, e *parser.Expr) (any, error) {
			old := i.env
			i.env = env
			val, err := i.evalExpr(e)
			i.env = old
			return val, err
		})

	case p.LogicQuery != nil:
		pred := convertLogicPredicate(p.LogicQuery.Pred)
		results := i.logic.Query(pred)
		out := make([]any, len(results))
		for idx, r := range results {
			out[idx] = r
		}
		return out, nil

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
		header := true
		delim := ','
		if p.Load.With != nil {
			v, err := i.evalExpr(p.Load.With)
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
		case "json":
			if p.Load.Path == nil {
				rows, err = data.LoadJSONReader(os.Stdin)
			} else {
				rows, err = data.LoadJSON(*p.Load.Path)
			}
		case "yaml":
			if p.Load.Path == nil {
				rows, err = data.LoadYAMLReader(os.Stdin)
			} else {
				rows, err = data.LoadYAML(*p.Load.Path)
			}
		case "tsv":
			delim = '\t'
			fallthrough
		default:
			if p.Load.Path == nil {
				rows, err = data.LoadCSVReader(os.Stdin, header, delim)
			} else {
				rows, err = data.LoadCSV(*p.Load.Path, header, delim)
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
		case "json":
			if p.Save.Path == nil {
				err = data.SaveJSONWriter(rows, os.Stdout)
			} else {
				err = data.SaveJSON(rows, *p.Save.Path)
			}
		case "yaml":
			if p.Save.Path == nil {
				err = data.SaveYAMLWriter(rows, os.Stdout)
			} else {
				err = data.SaveYAML(rows, *p.Save.Path)
			}
		case "tsv":
			delim = '\t'
			fallthrough
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
			toolFuncs  = map[string]Closure{}
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
						case Closure:
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
						case *Closure:
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
									cl := Closure{Name: name, Fn: &parser.FunExpr{Params: fn.Params, Return: fn.Return, BlockBody: fn.Body}, Env: i.env.Copy(), FullParams: fn.Params}
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
		i.callCounts[c.Func]++
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

		if argCount == paramCount {
			if cand, ok := i.inlineCandidates[c.Func]; ok {
				if i.shouldInline(c.Func, cand) {
					return i.inlineFunction(cand, argVals)
				}
			}
		}

		if argCount < paramCount {
			applied := make([]Value, argCount)
			for idx := range argVals {
				applied[idx] = anyToValue(argVals[idx])
			}
			remainingParams := fn.Params[argCount:]
			return Closure{
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

		if i.memoize {
			if t, err := i.types.GetVar(c.Func); err == nil {
				if ft, ok := t.(types.FuncType); ok && ft.Pure {
					key := argsKey(argVals)
					if fnCache, ok := i.memo[c.Func]; ok {
						if res, ok := fnCache[key]; ok {
							return res, nil
						}
					}
					ret, err := i.invokeFunction(fn, argVals)
					if err != nil {
						return nil, err
					}
					if _, ok := i.memo[c.Func]; !ok {
						i.memo[c.Func] = map[string]any{}
					}
					i.memo[c.Func][key] = ret
					return ret, nil
				}
			}
		}

		return i.invokeFunction(fn, argVals)
	}

	val, err := i.env.GetValue(c.Func)
	if err == nil {
		switch fn := val.(type) {
		case Closure:
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
				return Closure{
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

func (i *Interpreter) invokeFunction(fn *parser.FunStmt, args []any) (any, error) {
	child := types.NewEnv(i.env)
	for idx, param := range fn.Params {
		child.SetValue(param.Name, args[idx], true)
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
	return ret, nil
}

// shouldInline returns true if fn is a small pure function and call count
// suggests potential performance gain from inlining.
func (i *Interpreter) shouldInline(name string, fn *parser.FunStmt) bool {
	if fn == nil {
		return false
	}
	bodyLen := len(fn.Body)
	if bodyLen != 1 {
		return false
	}
	stmt := fn.Body[0]
	if stmt.Return == nil && stmt.Expr == nil {
		return false
	}
	if t, err := i.types.GetVar(name); err == nil {
		if ft, ok := t.(types.FuncType); ok && ft.Pure {
			if i.callCounts[name] > 10 {
				return true
			}
		}
	}
	return false
}

// inlineFunction executes a simple function body directly without full call
// setup. Arguments must already be evaluated.
func (i *Interpreter) inlineFunction(fn *parser.FunStmt, args []any) (any, error) {
	child := types.NewEnv(i.env)
	for idx, param := range fn.Params {
		if idx < len(args) {
			child.SetValue(param.Name, args[idx], true)
		} else {
			child.SetValue(param.Name, nil, true)
		}
	}
	old := i.env
	i.env = child
	defer func() { i.env = old }()
	stmt := fn.Body[0]
	if stmt.Return != nil {
		return i.evalExpr(stmt.Return.Value)
	}
	if stmt.Expr != nil {
		return i.evalExpr(stmt.Expr.Expr)
	}
	return nil, nil
}

// --- Return ---

type returnSignal struct{ value any }

func (r returnSignal) Error() string { return "return" }

type breakSignal struct{}

func (b breakSignal) Error() string { return "break" }

type continueSignal struct{}

func (c continueSignal) Error() string { return "continue" }

func convertLogicTerm(t *parser.LogicTerm) logic.Term {
	if t.Var != nil {
		return logic.Term{Var: *t.Var, IsVar: true}
	}
	if t.Str != nil {
		return logic.Term{Val: *t.Str}
	}
	if t.Int != nil {
		return logic.Term{Val: *t.Int}
	}
	return logic.Term{}
}

func convertLogicPredicate(p *parser.LogicPredicate) logic.Predicate {
	args := make([]logic.Term, len(p.Args))
	for i, a := range p.Args {
		args[i] = convertLogicTerm(a)
	}
	return logic.Predicate{Name: p.Name, Args: args}
}

func convertLogicRule(r *parser.RuleStmt) logic.Rule {
	head := convertLogicPredicate(r.Head)
	body := make([]logic.Condition, len(r.Body))
	for i, c := range r.Body {
		if c.Pred != nil {
			p := convertLogicPredicate(c.Pred)
			body[i] = logic.Condition{Pred: &p}
		} else if c.Neq != nil {
			body[i] = logic.Condition{Neq: &logic.NotEqual{A: c.Neq.A, B: c.Neq.B}}
		}
	}
	return logic.Rule{Head: head, Body: body}
}

func (i *Interpreter) Close() {
	for _, cancel := range i.cancels {
		cancel()
	}
	// Wait for all stream handlers to finish processing events before
	// shutting everything down.
	for _, s := range i.streams {
		if w, ok := s.(interface{ Wait() }); ok {
			w.Wait()
		}
	}
	i.wg.Wait()
	for _, s := range i.streams {
		s.Close()
	}
	for _, a := range i.agents {
		a.Stop()
	}
}
