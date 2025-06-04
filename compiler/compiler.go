package compiler

import (
	"fmt"
	"github.com/alecthomas/participle/v2/lexer"
	"log"
	"mochi/parser"
)

// --- Bytecode Definitions ---

type OpCode byte

const (
	OpConst OpCode = iota
	OpAdd
	OpSub
	OpMul
	OpDiv
	OpMod
	OpNeg
	OpPop
	OpStore
	OpLoad
	OpReturn
	OpJump
	OpJumpIfFalse
	OpDefineFun
	OpCall
	OpMakeList
	OpMakeMap
	OpIndex
	OpGetAttr
	OpExpect
	OpNoop

	OpEq
	OpNeq
	OpLt
	OpLe
	OpGt
	OpGe
)

func (op OpCode) String() string {
	switch op {
	case OpConst:
		return "OpConst"
	case OpAdd:
		return "OpAdd"
	case OpSub:
		return "OpSub"
	case OpMul:
		return "OpMul"
	case OpDiv:
		return "OpDiv"
	case OpMod:
		return "OpMod"
	case OpNeg:
		return "OpNeg"
	case OpPop:
		return "OpPop"
	case OpStore:
		return "OpStore"
	case OpLoad:
		return "OpLoad"
	case OpReturn:
		return "OpReturn"
	case OpJump:
		return "OpJump"
	case OpJumpIfFalse:
		return "OpJumpIfFalse"
	case OpDefineFun:
		return "OpDefineFun"
	case OpCall:
		return "OpCall"
	case OpMakeList:
		return "OpMakeList"
	case OpMakeMap:
		return "OpMakeMap"
	case OpIndex:
		return "OpIndex"
	case OpGetAttr:
		return "OpGetAttr"
	case OpExpect:
		return "OpExpect"
	case OpNoop:
		return "OpNoop"
	case OpEq:
		return "OpEq"
	case OpNeq:
		return "OpNeq"
	case OpLt:
		return "OpLt"
	case OpLe:
		return "OpLe"
	case OpGt:
		return "OpGt"
	case OpGe:
		return "OpGe"
	default:
		return fmt.Sprintf("OpCode(%d)", op)
	}
}

type Instruction struct {
	Op  OpCode
	Arg any
}

type Chunk struct {
	Code []Instruction
}

// --- Function & Closure ---

type Function struct {
	Name   string
	Params []string
	Body   []Instruction
}

type Closure struct {
	Fn       *Function
	Captured map[string]any
}

type Frame struct {
	Fn      *Function
	Locals  map[string]any
	IP      int
	BasePtr int
}

// --- Compiler ---

type Compiler struct {
	chunk      *Chunk
	locals     map[string]bool
	stackDepth int  // 🔐 simulated stack depth
	Debug      bool // ← NEW
}

func New() *Compiler {
	return &Compiler{
		chunk:  &Chunk{},
		locals: make(map[string]bool),
	}
}

func (c *Compiler) logf(format string, args ...any) {
	if c.Debug {
		log.Printf("[compile] "+format, args...)
	}
}

func (c *Compiler) Compile(prog *parser.Program) (*Chunk, error) {
	for _, stmt := range prog.Statements {
		if err := c.compileStmt(stmt); err != nil {
			return nil, err
		}
	}
	c.emit(OpReturn, nil)
	return c.chunk, nil
}

func (c *Compiler) emit(op OpCode, arg any) int {
	c.updateStack(op, arg)
	inst := Instruction{Op: op, Arg: arg}
	c.chunk.Code = append(c.chunk.Code, inst)
	return len(c.chunk.Code) - 1
}

func (c *Compiler) updateStack(op OpCode, arg any) {
	switch op {
	case OpConst, OpLoad, OpDefineFun, OpMakeList, OpMakeMap:
		c.stackDepth++

	case OpPop, OpStore:
		c.stackDepth--
		if c.stackDepth < 0 {
			log.Fatalf("compiler error: stack underflow at %s", op)
		}

	case OpAdd, OpSub, OpMul, OpDiv, OpMod, OpEq, OpNeq, OpLt, OpLe, OpGt, OpGe, OpIndex:
		c.stackDepth-- // 2 in, 1 out
		if c.stackDepth < 0 {
			log.Fatalf("compiler error: stack underflow at %s", op)
		}

	case OpCall:
		argc, ok := argAsInt(arg)
		if !ok {
			log.Fatalf("compiler error: OpCall expects int arg, got %T", arg)
		}
		c.stackDepth -= argc // pop arguments
		c.stackDepth--       // pop function
		if c.stackDepth < 0 {
			log.Fatalf("compiler error: stack underflow at OpCall")
		}
		c.stackDepth++ // push return value

	case OpReturn:
		// Leave depth unchanged: return value already popped, stack discarded

	case OpJump, OpJumpIfFalse, OpGetAttr, OpNoop, OpExpect:
		// These don't affect stack height directly

	default:
		log.Fatalf("compiler error: unknown stack effect for op %s", op)
	}
}

func argAsInt(arg any) (int, bool) {
	switch v := arg.(type) {
	case int:
		return v, true
	case int64:
		return int(v), true
	default:
		return 0, false
	}
}

func (c *Compiler) patchJump(pos int, target int) {
	c.chunk.Code[pos].Arg = target
}

// --- Statement Compilation ---

func (c *Compiler) compileStmt(stmt *parser.Statement) error {
	switch {
	case stmt.If != nil:
		return c.compileIf(stmt.If)
	case stmt.For != nil:
		return c.compileFor(stmt.For)
	case stmt.Test != nil:
		for _, s := range stmt.Test.Body {
			if err := c.compileStmt(s); err != nil {
				return err
			}
		}
		return nil
	case stmt.Expect != nil:
		if err := c.compileExpr(stmt.Expect.Value); err != nil {
			return err
		}
		c.emit(OpExpect, nil)
		return nil
	default:
		return c.compileBaseStmt(stmt)
	}
}

func (c *Compiler) compileBaseStmt(stmt *parser.Statement) error {
	switch {
	case stmt.Fun != nil:
		return c.compileFunStmt(stmt.Fun)
	case stmt.Let != nil || stmt.Var != nil || stmt.Assign != nil || stmt.Return != nil || stmt.Expr != nil:
		return c.compileSimpleStmt(stmt)
	default:
		return fmt.Errorf("unsupported base statement")
	}
}

func (c *Compiler) compileSimpleStmt(stmt *parser.Statement) error {
	switch {
	case stmt.Let != nil:
		// fmt.Printf("[compile] let %s (has value: %v)\n", stmt.Let.Name, stmt.Let.Value != nil)
		name := stmt.Let.Name
		if stmt.Let.Value != nil {
			if err := c.compileExpr(stmt.Let.Value); err != nil {
				return err
			}
		} else {
			c.emit(OpConst, nil)
		}
		c.emit(OpStore, name)
		c.locals[name] = true
		return nil
	case stmt.Var != nil:
		name := stmt.Var.Name
		if stmt.Var.Value != nil {
			if err := c.compileExpr(stmt.Var.Value); err != nil {
				return err
			}
		} else {
			c.emit(OpConst, nil)
		}
		c.emit(OpStore, name)
		c.locals[name] = true
		return nil
	case stmt.Assign != nil:
		name := stmt.Assign.Name
		if !c.locals[name] {
			return fmt.Errorf("assign to undefined variable: %s", name)
		}
		if err := c.compileExpr(stmt.Assign.Value); err != nil {
			return err
		}
		c.emit(OpStore, name)
		return nil
	case stmt.Return != nil:
		if err := c.compileExpr(stmt.Return.Value); err != nil {
			return err
		}
		c.emit(OpReturn, nil)
		return nil
	case stmt.Expr != nil:
		if err := c.compileExpr(stmt.Expr.Expr); err != nil {
			return err
		}
		c.emit(OpPop, nil)
		return nil
	default:
		return fmt.Errorf("unsupported statement")
	}
}

func (c *Compiler) compileIf(stmt *parser.IfStmt) error {
	// Step 1: Compile condition expression
	if err := c.compileExpr(stmt.Cond); err != nil {
		return err
	}

	// Emit JumpIfFalse to jump to else (we patch it later)
	jumpToElse := c.emit(OpJumpIfFalse, -1)

	// --- THEN BLOCK ---
	thenStart := len(c.chunk.Code)

	for _, s := range stmt.Then {
		if err := c.compileStmt(s); err != nil {
			return err
		}
	}

	// Determine if the then-block ends in return
	// If not, emit a jump to skip over the else block
	endJump := -1
	if !endsWithReturn(c.chunk.Code[thenStart:]) {
		endJump = c.emit(OpJump, -1)
	}

	// Patch JumpIfFalse to point here (start of else)
	c.patchJump(jumpToElse, len(c.chunk.Code))

	// --- ELSE BLOCK ---
	if stmt.ElseIf != nil {
		// Recursively compile nested else-if
		if err := c.compileIf(stmt.ElseIf); err != nil {
			return err
		}
	} else if len(stmt.Else) > 0 {
		for _, s := range stmt.Else {
			if err := c.compileStmt(s); err != nil {
				return err
			}
		}
	}

	// Patch jump over else block (if we emitted one)
	if endJump != -1 {
		c.patchJump(endJump, len(c.chunk.Code))
	}

	return nil
}

func (c *Compiler) compileFor(stmt *parser.ForStmt) error {
	name := stmt.Name

	if stmt.RangeEnd != nil {
		// --- Range loop: for i in start..end ---
		if err := c.compileExpr(stmt.Source); err != nil {
			return err
		}
		c.emit(OpStore, name+"__start")

		if err := c.compileExpr(stmt.RangeEnd); err != nil {
			return err
		}
		c.emit(OpStore, name+"__end")

		loopStart := len(c.chunk.Code)

		c.emit(OpLoad, name+"__start")
		c.emit(OpLoad, name+"__end")
		c.emit(OpLt, nil) // while i < end

		exit := c.emit(OpJumpIfFalse, -1)

		c.emit(OpLoad, name+"__start")
		c.emit(OpStore, name)

		for _, s := range stmt.Body {
			if err := c.compileStmt(s); err != nil {
				return err
			}
		}

		c.emit(OpLoad, name+"__start")
		c.emit(OpConst, 1)
		c.emit(OpAdd, nil)
		c.emit(OpStore, name+"__start")

		c.emit(OpJump, loopStart)
		c.patchJump(exit, len(c.chunk.Code))
		return nil
	}

	// --- Collection loop: for x in expr ---
	// Evaluate collection → store in temp
	if err := c.compileExpr(stmt.Source); err != nil {
		return err
	}
	c.emit(OpStore, name+"__coll")

	// i = 0
	c.emit(OpConst, 0)
	c.emit(OpStore, name+"__i")

	loopStart := len(c.chunk.Code)

	// if i < len(coll)
	c.emit(OpLoad, name+"__i")
	c.emit(OpLoad, name+"__coll")
	c.emit(OpConst, "len")
	c.emit(OpCall, 1) // len(coll)
	c.emit(OpLt, nil)

	exit := c.emit(OpJumpIfFalse, -1)

	// x = coll[i]
	c.emit(OpLoad, name+"__coll")
	c.emit(OpLoad, name+"__i")
	c.emit(OpIndex, nil) // Runtime must handle: list[i] or map keys()[i]
	c.emit(OpStore, name)

	for _, s := range stmt.Body {
		if err := c.compileStmt(s); err != nil {
			return err
		}
	}

	c.emit(OpLoad, name+"__i")
	c.emit(OpConst, 1)
	c.emit(OpAdd, nil)
	c.emit(OpStore, name+"__i")

	c.emit(OpJump, loopStart)
	c.patchJump(exit, len(c.chunk.Code))
	return nil
}

// --- Expressions ---

func (c *Compiler) compileExpr(expr *parser.Expr) error {
	if expr == nil || expr.Binary == nil {
		return fmt.Errorf("invalid expression: nil")
	}
	return c.compileBinary(expr.Binary)
}

type binOp struct {
	op    string
	pos   lexer.Position
	left  int // operand index
	right int // operand index
}

func (c *Compiler) compileBinary(bin *parser.BinaryExpr) error {
	if bin == nil {
		return fmt.Errorf("nil binary")
	}

	var operands []*parser.Unary
	var operators []binOp

	operands = append(operands, bin.Left)
	for i, part := range bin.Right {
		operands = append(operands, &parser.Unary{
			Pos:   part.Right.Target.Pos,
			Ops:   nil,
			Value: part.Right,
		})
		operators = append(operators, binOp{
			op:    part.Op,
			pos:   part.Pos,
			left:  i,
			right: i + 1,
		})
	}

	if len(operators) == 0 && len(operands) == 1 {
		return c.compileUnary(operands[0])
	}

	emitted := make([]bool, len(operands))

	for _, level := range [][]string{
		{"*", "/", "%"},
		{"+", "-"},
		{"<", "<=", ">", ">="},
		{"==", "!="},
	} {
		for i := 0; i < len(operators); {
			op := operators[i]
			if !contains(level, op.op) {
				i++
				continue
			}

			left := operands[op.left]
			right := operands[op.right]

			// Attempt constant folding
			lval, lok := extractLiteral(left)
			rval, rok := extractLiteral(right)

			if lok && rok {
				// Fold at compile-time
				var folded any
				var err error
				if isCompareOp(op.op) {
					folded, err = compareFold(op.op, lval, rval)
				} else {
					folded, err = arithFold(op.op, lval, rval)
				}
				if err != nil {
					return err
				}
				c.emit(OpConst, folded)
				emitted[op.left] = true
				emitted[op.right] = true
				operators = append(operators[:i], operators[i+1:]...)
				continue
			}

			// Normal evaluation path
			if !emitted[op.left] {
				if err := c.compileUnary(left); err != nil {
					return err
				}
				emitted[op.left] = true
			}
			if !emitted[op.right] {
				if err := c.compileUnary(right); err != nil {
					return err
				}
				emitted[op.right] = true
			}

			switch op.op {
			case "*":
				c.emit(OpMul, nil)
			case "/":
				c.emit(OpDiv, nil)
			case "%":
				c.emit(OpMod, nil)
			case "+":
				c.emit(OpAdd, nil)
			case "-":
				c.emit(OpSub, nil)
			case "==":
				c.emit(OpEq, nil)
			case "!=":
				c.emit(OpNeq, nil)
			case "<":
				c.emit(OpLt, nil)
			case "<=":
				c.emit(OpLe, nil)
			case ">":
				c.emit(OpGt, nil)
			case ">=":
				c.emit(OpGe, nil)
			default:
				return fmt.Errorf("unsupported binary op: %s", op.op)
			}
			operators = append(operators[:i], operators[i+1:]...)
		}
	}

	return nil
}

func contains(list []string, x string) bool {
	for _, s := range list {
		if s == x {
			return true
		}
	}
	return false
}

func (c *Compiler) compileUnary(un *parser.Unary) error {
	if err := c.compilePostfix(un.Value); err != nil {
		return err
	}
	for i := len(un.Ops) - 1; i >= 0; i-- {
		switch un.Ops[i] {
		case "-":
			c.emit(OpNeg, nil)
		default:
			return fmt.Errorf("unsupported unary op: %s", un.Ops[i])
		}
	}
	return nil
}

func (c *Compiler) compilePostfix(p *parser.PostfixExpr) error {
	return c.compilePrimary(p.Target)
}

func (c *Compiler) compileLiteral(l *parser.Literal) error {
	switch {
	case l.Int != nil:
		// log.Printf("[compileLiteral] int: %d", *l.Int)
		c.emit(OpConst, *l.Int)
	case l.Float != nil:
		// log.Printf("[compileLiteral] float: %f", *l.Float)
		c.emit(OpConst, *l.Float)
	case l.Str != nil:
		// log.Printf("[compileLiteral] str: %q", *l.Str)
		c.emit(OpConst, *l.Str)
	case l.Bool != nil:
		// log.Printf("[compileLiteral] bool: %v", *l.Bool)
		c.emit(OpConst, *l.Bool)
	default:
		return fmt.Errorf("unsupported literal")
	}
	return nil
}

func (c *Compiler) compileFunStmt(fun *parser.FunStmt) error {
	paramNames := make([]string, len(fun.Params))
	for i, p := range fun.Params {
		paramNames[i] = p.Name
	}
	sub := New()

	for _, s := range fun.Body {
		if err := sub.compileStmt(s); err != nil {
			return err
		}
	}

	// ✅ Emit OpReturn only if last statement wasn't an explicit return
	if !endsWithReturn(sub.chunk.Code) {
		sub.emit(OpReturn, nil)
	}

	f := &Function{Name: fun.Name, Params: paramNames, Body: sub.chunk.Code}
	c.emit(OpDefineFun, f)
	c.emit(OpStore, fun.Name)
	c.locals[fun.Name] = true
	return nil
}

func endsWithReturn(code []Instruction) bool {
	for i := len(code) - 1; i >= 0; i-- {
		switch code[i].Op {
		case OpReturn:
			return true
		case OpPop, OpNoop:
			continue
		default:
			return false
		}
	}
	return false
}

func (c *Compiler) compileFunExpr(fn *parser.FunExpr) error {
	paramNames := make([]string, len(fn.Params))
	for i, p := range fn.Params {
		paramNames[i] = p.Name
	}
	sub := New()

	if fn.ExprBody != nil {
		ret := &parser.Statement{Return: &parser.ReturnStmt{Value: fn.ExprBody}}
		if err := sub.compileStmt(ret); err != nil {
			return err
		}
	} else {
		for _, s := range fn.BlockBody {
			if err := sub.compileStmt(s); err != nil {
				return err
			}
		}
		if !endsWithReturn(sub.chunk.Code) {
			sub.emit(OpReturn, nil)
		}
	}

	f := &Function{Name: "", Params: paramNames, Body: sub.chunk.Code}
	c.emit(OpDefineFun, f)
	return nil
}

var builtinNames = map[string]bool{
	"print": true,
	"len":   true,
	"now":   true,
	"json":  true,
}

func (c *Compiler) compileCallExpr(call *parser.CallExpr) error {
	fnName := call.Func

	// ✅ Push args first
	for _, arg := range call.Args {
		if err := c.compileExpr(arg); err != nil {
			return err
		}
	}

	// ✅ Then push the function (it will be popped last)
	if builtinNames[fnName] {
		c.emit(OpConst, fnName)
	} else {
		c.emit(OpLoad, fnName)
	}

	// ✅ Emit OpCall (argument count)
	c.emit(OpCall, len(call.Args))
	return nil
}

func (c *Compiler) compilePrimary(p *parser.Primary) error {
	switch {
	case p.Lit != nil:
		return c.compileLiteral(p.Lit)
	case p.Group != nil:
		return c.compileExpr(p.Group)
	case p.FunExpr != nil:
		return c.compileFunExpr(p.FunExpr)
	case p.Selector != nil:
		c.emit(OpLoad, p.Selector.Root)
		for _, sel := range p.Selector.Tail {
			c.emit(OpConst, sel)
			c.emit(OpGetAttr, nil)
		}
		return nil
	case p.List != nil:
		for _, elem := range p.List.Elems {
			if err := c.compileExpr(elem); err != nil {
				return err
			}
		}
		c.emit(OpMakeList, len(p.List.Elems))
		return nil
	case p.Map != nil:
		for _, entry := range p.Map.Items {
			if err := c.compileExpr(entry.Key); err != nil {
				return err
			}
			if err := c.compileExpr(entry.Value); err != nil {
				return err
			}
		}
		c.emit(OpMakeMap, len(p.Map.Items))
		return nil
	case p.Call != nil:
		return c.compileCallExpr(p.Call)
	default:
		return fmt.Errorf("unsupported primary expression")
	}
}

func extractLiteral(un *parser.Unary) (any, bool) {
	if len(un.Ops) > 0 {
		return nil, false
	}
	if un.Value == nil || un.Value.Target == nil {
		return nil, false
	}

	target := un.Value.Target

	if target.Lit != nil {
		lit := target.Lit
		switch {
		case lit.Int != nil:
			return *lit.Int, true
		case lit.Float != nil:
			return *lit.Float, true
		case lit.Str != nil:
			return *lit.Str, true
		case lit.Bool != nil:
			return *lit.Bool, true
		default:
			return nil, false
		}
	}

	if target.List != nil {
		list := target.List
		constants := make([]any, 0, len(list.Elems))
		for _, elem := range list.Elems {
			if elem == nil || elem.Binary == nil {
				return nil, false
			}
			expr := &parser.Unary{
				Pos:   elem.Binary.Left.Pos,
				Value: &parser.PostfixExpr{Target: elem.Binary.Left.Value.Target},
			}
			val, ok := extractLiteral(expr)
			if !ok {
				return nil, false
			}
			constants = append(constants, val)
		}
		return constants, true
	}

	return nil, false
}

func arithFold(op string, a, b any) (any, error) {
	switch a := a.(type) {
	case int:
		bi, ok := b.(int)
		if !ok {
			return nil, fmt.Errorf("type mismatch: %T and %T", a, b)
		}
		switch op {
		case "+":
			return a + bi, nil
		case "-":
			return a - bi, nil
		case "*":
			return a * bi, nil
		case "/":
			if bi == 0 {
				return nil, fmt.Errorf("division by zero")
			}
			return a / bi, nil
		case "%":
			if bi == 0 {
				return nil, fmt.Errorf("modulo by zero")
			}
			return a % bi, nil
		}
	case float64:
		bf, ok := b.(float64)
		if !ok {
			return nil, fmt.Errorf("type mismatch: %T and %T", a, b)
		}
		switch op {
		case "+":
			return a + bf, nil
		case "-":
			return a - bf, nil
		case "*":
			return a * bf, nil
		case "/":
			if bf == 0.0 {
				return nil, fmt.Errorf("division by zero")
			}
			return a / bf, nil
		}
	case []any:
		bList, ok := b.([]any)
		if !ok {
			return nil, fmt.Errorf("type mismatch: %T and %T", a, b)
		}
		switch op {
		case "+":
			return append(a, bList...), nil
		default:
			return nil, fmt.Errorf("unsupported list op: %s", op)
		}
	}
	return nil, fmt.Errorf("cannot fold: %v %s %v", a, op, b)
}

func compareFold(op string, a, b any) (bool, error) {
	switch a := a.(type) {
	case int:
		bi, ok := b.(int)
		if !ok {
			return false, fmt.Errorf("type mismatch: %T and %T", a, b)
		}
		switch op {
		case "==":
			return a == bi, nil
		case "!=":
			return a != bi, nil
		case "<":
			return a < bi, nil
		case "<=":
			return a <= bi, nil
		case ">":
			return a > bi, nil
		case ">=":
			return a >= bi, nil
		}
	case float64:
		bf, ok := b.(float64)
		if !ok {
			return false, fmt.Errorf("type mismatch: %T and %T", a, b)
		}
		switch op {
		case "==":
			return a == bf, nil
		case "!=":
			return a != bf, nil
		case "<":
			return a < bf, nil
		case "<=":
			return a <= bf, nil
		case ">":
			return a > bf, nil
		case ">=":
			return a >= bf, nil
		}
	case string:
		bs, ok := b.(string)
		if !ok {
			return false, fmt.Errorf("type mismatch: %T and %T", a, b)
		}
		switch op {
		case "==":
			return a == bs, nil
		case "!=":
			return a != bs, nil
		case "<":
			return a < bs, nil
		case "<=":
			return a <= bs, nil
		case ">":
			return a > bs, nil
		case ">=":
			return a >= bs, nil
		}
	case bool:
		bb, ok := b.(bool)
		if !ok {
			return false, fmt.Errorf("type mismatch: %T and %T", a, b)
		}
		switch op {
		case "==":
			return a == bb, nil
		case "!=":
			return a != bb, nil
		}
	}
	return false, fmt.Errorf("unsupported constant comparison: %v %s %v", a, op, b)
}

func isCompareOp(op string) bool {
	switch op {
	case "==", "!=", "<", "<=", ">", ">=":
		return true
	default:
		return false
	}
}
