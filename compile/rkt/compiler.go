package rktcode

import (
	"bytes"
	"fmt"
	"strconv"
	"strings"

	"mochi/parser"
	"mochi/types"
)

// Compiler translates Mochi AST into Racket source code.
type Compiler struct {
	buf    bytes.Buffer
	indent int
	env    *types.Env
	loops  []loopCtx
}

type loopCtx struct {
	brk     string
	cont    string
	contArg string
}

func (c *Compiler) pushLoop(brk, cont, arg string) {
	c.loops = append(c.loops, loopCtx{brk: brk, cont: cont, contArg: arg})
}

func (c *Compiler) popLoop() {
	if len(c.loops) > 0 {
		c.loops = c.loops[:len(c.loops)-1]
	}
}

func hasLoopCtrl(stmts []*parser.Statement) bool {
	for _, s := range stmts {
		switch {
		case s.Break != nil || s.Continue != nil:
			return true
		case s.For != nil:
			if hasLoopCtrl(s.For.Body) {
				return true
			}
		case s.While != nil:
			if hasLoopCtrl(s.While.Body) {
				return true
			}
		case s.If != nil:
			if hasLoopCtrl(s.If.Then) || hasLoopCtrlIf(s.If.ElseIf) || hasLoopCtrl(s.If.Else) {
				return true
			}
		}
	}
	return false
}

func hasLoopCtrlIf(ifst *parser.IfStmt) bool {
	if ifst == nil {
		return false
	}
	if hasLoopCtrl(ifst.Then) || hasLoopCtrl(ifst.Else) {
		return true
	}
	return hasLoopCtrlIf(ifst.ElseIf)
}

func New(env *types.Env) *Compiler {
	return &Compiler{env: env}
}

func (c *Compiler) Compile(prog *parser.Program) ([]byte, error) {
	c.writeln("#lang racket")
	c.writeln("(require racket/list racket/match)")
	c.writeln("")
	// helpers for indexing and slicing
	c.writeln("(define (idx x i)")
	c.writeln("  (cond [(string? x) (string-ref x i)]")
	c.writeln("        [(hash? x) (hash-ref x i)]")
	c.writeln("        [else (list-ref x i)]))")
	c.writeln("(define (slice x s e) (if (string? x) (substring x s e) (take (drop x s) (- e s))))")
	c.writeln("(define (count x)")
	c.writeln("  (cond [(string? x) (string-length x)]")
	c.writeln("        [(hash? x) (hash-count x)]")
	c.writeln("        [else (length x)]))")
	c.writeln("(define (avg x)")
	c.writeln("  (let ([n (count x)])")
	c.writeln("    (if (= n 0) 0")
	c.writeln("        (/ (for/fold ([s 0.0]) ([v x]) (+ s (real->double-flonum v))) n))))")
	c.writeln("(define (union-all a b) (append (list->list a) (list->list b)))")
	c.writeln("(define (union a b)")
	c.writeln("  (let loop ([res (list->list a)] [xs (list->list b)])")
	c.writeln("    (if (null? xs) res")
	c.writeln("        (let ([x (car xs)])")
	c.writeln("          (if (member x res)")
	c.writeln("              (loop res (cdr xs))")
	c.writeln("              (loop (append res (list x)) (cdr xs))))))")
	c.writeln("(define (except a b)")
	c.writeln("  (for/list ([x (list->list a)] #:unless (member x (list->list b))) x))")
	c.writeln("(define (intersect a b)")
	c.writeln("  (for/fold ([res '()]) ([x (list->list a)])")
	c.writeln("    (if (and (member x (list->list b)) (not (member x res)))")
	c.writeln("        (append res (list x))")
	c.writeln("        res)))")
	c.writeln("")
	// function declarations first
	for _, s := range prog.Statements {
		if s.Fun != nil {
			if err := c.compileFun(s.Fun); err != nil {
				return nil, err
			}
			c.writeln("")
		}
	}
	// main body
	for _, s := range prog.Statements {
		if s.Fun == nil && s.Type == nil && s.Test == nil {
			if err := c.compileStmt(s); err != nil {
				return nil, err
			}
		}
	}
	return c.buf.Bytes(), nil
}

func (c *Compiler) compileFun(fn *parser.FunStmt) error {
	name := sanitizeName(fn.Name)
	c.writeIndent()
	c.buf.WriteString("(define (" + name)
	for _, p := range fn.Params {
		c.buf.WriteString(" " + sanitizeName(p.Name))
	}
	c.buf.WriteString(")\n")
	c.indent++
	c.writeln("(let/ec return")
	c.indent++
	for _, st := range fn.Body {
		if err := c.compileStmt(st); err != nil {
			return err
		}
	}
	c.writeln("(return (void))") // default return when none hit
	c.indent--
	c.writeln(")")
	c.indent--
	c.writeln(")")
	return nil
}

func (c *Compiler) compileStmt(s *parser.Statement) error {
	switch {
	case s.Let != nil:
		return c.compileLet(s.Let)
	case s.Var != nil:
		return c.compileVar(s.Var)
	case s.Return != nil:
		val, err := c.compileExpr(s.Return.Value)
		if err != nil {
			return err
		}
		c.writeln(fmt.Sprintf("(return %s)", val))
		return nil
	case s.Assign != nil:
		return c.compileAssign(s.Assign)
	case s.For != nil:
		return c.compileFor(s.For)
	case s.While != nil:
		return c.compileWhile(s.While)
	case s.Break != nil:
		return c.compileBreak(s.Break)
	case s.Continue != nil:
		return c.compileContinue(s.Continue)
	case s.If != nil:
		return c.compileIf(s.If)
	case s.Expr != nil:
		expr, err := c.compileExpr(s.Expr.Expr)
		if err != nil {
			return err
		}
		c.writeln(expr)
		return nil
	case s.Fun != nil:
		return c.compileFun(s.Fun)
	default:
		return fmt.Errorf("unsupported statement")
	}
}

func (c *Compiler) compileLet(l *parser.LetStmt) error {
	val := "(void)"
	if l.Value != nil {
		expr, err := c.compileExpr(l.Value)
		if err != nil {
			return err
		}
		val = expr
	}
	c.writeln(fmt.Sprintf("(define %s %s)", sanitizeName(l.Name), val))
	return nil
}

func (c *Compiler) compileVar(v *parser.VarStmt) error {
	val := "(void)"
	if v.Value != nil {
		expr, err := c.compileExpr(v.Value)
		if err != nil {
			return err
		}
		val = expr
	}
	c.writeln(fmt.Sprintf("(define %s %s)", sanitizeName(v.Name), val))
	return nil
}

func (c *Compiler) compileAssign(a *parser.AssignStmt) error {
	val, err := c.compileExpr(a.Value)
	if err != nil {
		return err
	}
	name := sanitizeName(a.Name)
	if len(a.Index) == 0 {
		c.writeln(fmt.Sprintf("(set! %s %s)", name, val))
		return nil
	}
	if len(a.Index) == 1 && a.Index[0].Colon == nil {
		idx, err := c.compileExpr(a.Index[0].Start)
		if err != nil {
			return err
		}
		c.writeln(fmt.Sprintf("(set! %s (if (hash? %s) (hash-set %s %s %s) (list-set %s %s %s)))", name, name, name, idx, val, name, idx, val))
		return nil
	}
	if len(a.Index) == 2 && a.Index[0].Colon == nil && a.Index[1].Colon == nil {
		idx1, err := c.compileExpr(a.Index[0].Start)
		if err != nil {
			return err
		}
		idx2, err := c.compileExpr(a.Index[1].Start)
		if err != nil {
			return err
		}
		c.writeln(fmt.Sprintf("(set! %s (list-set %s %s (list-set (idx %s %s) %s %s)))", name, name, idx1, name, idx1, idx2, val))
		return nil
	}
	return fmt.Errorf("indexed assignment unsupported")
}

func (c *Compiler) compileBreak(b *parser.BreakStmt) error {
	if len(c.loops) == 0 {
		return fmt.Errorf("break not in loop")
	}
	lbl := c.loops[len(c.loops)-1].brk
	c.writeln("(" + lbl + " (void))")
	return nil
}

func (c *Compiler) compileContinue(cs *parser.ContinueStmt) error {
	if len(c.loops) == 0 {
		return fmt.Errorf("continue not in loop")
	}
	ctx := c.loops[len(c.loops)-1]
	if ctx.cont == "" {
		return fmt.Errorf("continue unsupported in this loop")
	}
	if ctx.contArg != "" {
		c.writeln(fmt.Sprintf("(%s %s)", ctx.cont, ctx.contArg))
	} else {
		c.writeln("(" + ctx.cont + ")")
	}
	return nil
}

func (c *Compiler) compileFor(f *parser.ForStmt) error {
	name := sanitizeName(f.Name)
	ctrl := hasLoopCtrl(f.Body)

	// Optimised path when no break/continue statements are present.
	if !ctrl {
		if f.RangeEnd != nil {
			start, err := c.compileExpr(f.Source)
			if err != nil {
				return err
			}
			end, err := c.compileExpr(f.RangeEnd)
			if err != nil {
				return err
			}
			c.writeln(fmt.Sprintf("(for ([%s (in-range %s %s)])", name, start, end))
		} else {
			src, err := c.compileExpr(f.Source)
			if err != nil {
				return err
			}
			c.writeln(fmt.Sprintf("(for ([%s %s])", name, src))
		}
		c.indent++
		for _, st := range f.Body {
			if err := c.compileStmt(st); err != nil {
				return err
			}
		}
		c.indent--
		c.writeln(")")
		return nil
	}

	// Fallback using explicit recursion to support break and continue.
	brk := fmt.Sprintf("brk%d", len(c.loops))
	loop := fmt.Sprintf("loop%d", len(c.loops))
	c.writeln("(let/ec " + brk)
	c.indent++

	if f.RangeEnd != nil {
		start, err := c.compileExpr(f.Source)
		if err != nil {
			return err
		}
		end, err := c.compileExpr(f.RangeEnd)
		if err != nil {
			return err
		}
		c.writeln("(let " + loop + " ([i " + start + "])")
		c.indent++
		c.writeln("(when (< i " + end + ")")
		c.indent++
		c.writeln(fmt.Sprintf("(define %s i)", name))
		c.pushLoop(brk, loop, "(+ i 1)")
		for _, st := range f.Body {
			if err := c.compileStmt(st); err != nil {
				c.popLoop()
				return err
			}
		}
		c.writeln("(" + loop + " (+ i 1)))")
		c.popLoop()
		c.indent--
		c.writeln(")")
		c.indent--
		c.writeln(")")
	} else {
		src, err := c.compileExpr(f.Source)
		if err != nil {
			return err
		}
		c.writeln("(let " + loop + " ([it " + src + "])")
		c.indent++
		c.writeln("(when (pair? it)")
		c.indent++
		c.writeln(fmt.Sprintf("(define %s (car it))", name))
		c.pushLoop(brk, loop, "(cdr it)")
		for _, st := range f.Body {
			if err := c.compileStmt(st); err != nil {
				c.popLoop()
				return err
			}
		}
		c.writeln("(" + loop + " (cdr it)))")
		c.popLoop()
		c.indent--
		c.writeln(")")
		c.indent--
		c.writeln(")")
	}
	return nil
}

func (c *Compiler) compileWhile(w *parser.WhileStmt) error {
	cond, err := c.compileExpr(w.Cond)
	if err != nil {
		return err
	}
	ctrl := hasLoopCtrl(w.Body)
	if ctrl {
		brk := fmt.Sprintf("brk%d", len(c.loops))
		loop := fmt.Sprintf("loop%d", len(c.loops))
		c.writeln("(let/ec " + brk)
		c.indent++
		c.writeln("(let " + loop + " ()")
		c.indent++
		c.writeln(fmt.Sprintf("(when %s", cond))
		c.indent++
		c.pushLoop(brk, loop, "")
		for _, st := range w.Body {
			if err := c.compileStmt(st); err != nil {
				c.popLoop()
				return err
			}
		}
		c.writeln("(" + loop + "))")
		c.popLoop()
		c.indent--
		c.writeln(")")
		c.indent--
		c.writeln(")")
		return nil
	}
	c.writeln("(let loop ()")
	c.indent++
	c.writeln(fmt.Sprintf("(when %s", cond))
	c.indent++
	for _, st := range w.Body {
		if err := c.compileStmt(st); err != nil {
			return err
		}
	}
	c.writeln("(loop))")
	c.indent--
	c.indent--
	c.writeln(")")
	return nil
}

func (c *Compiler) compileIf(s *parser.IfStmt) error {
	cond, err := c.compileExpr(s.Cond)
	if err != nil {
		return err
	}
	c.writeln(fmt.Sprintf("(if %s", cond))
	c.indent++
	// then branch
	c.writeln("(begin")
	c.indent++
	for _, st := range s.Then {
		if err := c.compileStmt(st); err != nil {
			return err
		}
	}
	c.indent--
	c.writeln(")")
	// else branch
	if s.ElseIf != nil {
		if err := c.compileIf(s.ElseIf); err != nil {
			return err
		}
	} else if len(s.Else) > 0 {
		c.writeln("(begin")
		c.indent++
		for _, st := range s.Else {
			if err := c.compileStmt(st); err != nil {
				return err
			}
		}
		c.indent--
		c.writeln(")")
	} else {
		c.writeln("(void)")
	}
	c.indent--
	c.writeln(")")
	return nil
}

// --- Expressions ---

func (c *Compiler) compileExpr(e *parser.Expr) (string, error) {
	if e == nil {
		return "", nil
	}
	return c.compileBinary(e.Binary)
}

func (c *Compiler) compileBinary(b *parser.BinaryExpr) (string, error) {
	if b == nil {
		return "", nil
	}
	first, err := c.compileUnary(b.Left)
	if err != nil {
		return "", err
	}

	operands := []string{first}
	ops := []string{}

	for _, part := range b.Right {
		r, err := c.compilePostfix(part.Right)
		if err != nil {
			return "", err
		}
		operands = append(operands, r)
		ops = append(ops, part.Op)
	}

	prec := [][]string{{"*", "/", "%"}, {"+", "-"}, {"<", "<=", ">", ">=", "in"}, {"==", "!="}, {"&&"}, {"||"}, {"union", "union_all", "except", "intersect"}}

	contains := func(list []string, s string) bool {
		for _, v := range list {
			if v == s {
				return true
			}
		}
		return false
	}

	for _, level := range prec {
		for i := 0; i < len(ops); {
			if !contains(level, ops[i]) {
				i++
				continue
			}
			l := operands[i]
			r := operands[i+1]
			op := ops[i]
			var expr string
			switch op {
			case "+":
				expr = fmt.Sprintf("(+ %s %s)", l, r)
			case "-":
				expr = fmt.Sprintf("(- %s %s)", l, r)
			case "*":
				expr = fmt.Sprintf("(* %s %s)", l, r)
			case "%":
				expr = fmt.Sprintf("(modulo %s %s)", l, r)
			case "/":
				expr = fmt.Sprintf("(/ %s %s)", l, r)
			case "==":
				expr = fmt.Sprintf("(= %s %s)", l, r)
			case "!=":
				expr = fmt.Sprintf("(not (= %s %s))", l, r)
			case "<", "<=", ">", ">=":
				expr = fmt.Sprintf("(%s %s %s)", op, l, r)
			case "&&":
				expr = fmt.Sprintf("(and %s %s)", l, r)
			case "||":
				expr = fmt.Sprintf("(or %s %s)", l, r)
			case "in":
				expr = fmt.Sprintf("(hash-has-key? %s %s)", r, l)
			case "union":
				expr = fmt.Sprintf("(union %s %s)", l, r)
			case "union_all":
				expr = fmt.Sprintf("(union-all %s %s)", l, r)
			case "except":
				expr = fmt.Sprintf("(except %s %s)", l, r)
			case "intersect":
				expr = fmt.Sprintf("(intersect %s %s)", l, r)
			default:
				return "", fmt.Errorf("unsupported op %s", op)
			}
			operands[i] = expr
			operands = append(operands[:i+1], operands[i+2:]...)
			ops = append(ops[:i], ops[i+1:]...)
		}
	}

	if len(operands) != 1 {
		return "", fmt.Errorf("invalid expression")
	}
	return operands[0], nil
}

func (c *Compiler) compileUnary(u *parser.Unary) (string, error) {
	val, err := c.compilePostfix(u.Value)
	if err != nil {
		return "", err
	}
	for i := len(u.Ops) - 1; i >= 0; i-- {
		switch u.Ops[i] {
		case "-":
			val = fmt.Sprintf("(- %s)", val)
		case "!":
			val = fmt.Sprintf("(not %s)", val)
		default:
			return "", fmt.Errorf("unsupported unary op %s", u.Ops[i])
		}
	}
	return val, nil
}

func (c *Compiler) compilePostfix(p *parser.PostfixExpr) (string, error) {
	val, err := c.compilePrimary(p.Target)
	if err != nil {
		return "", err
	}
	for _, op := range p.Ops {
		if op.Call != nil {
			args := make([]string, len(op.Call.Args))
			for i, a := range op.Call.Args {
				v, err := c.compileExpr(a)
				if err != nil {
					return "", err
				}
				args[i] = v
			}
			val = fmt.Sprintf("(%s %s)", val, strings.Join(args, " "))
		} else if op.Index != nil {
			if op.Index.Colon == nil {
				idx, err := c.compileExpr(op.Index.Start)
				if err != nil {
					return "", err
				}
				val = fmt.Sprintf("(idx %s %s)", val, idx)
			} else {
				start := "0"
				if op.Index.Start != nil {
					s, err := c.compileExpr(op.Index.Start)
					if err != nil {
						return "", err
					}
					start = s
				}
				end := fmt.Sprintf("(if (string? %s) (string-length %s) (length %s))", val, val, val)
				if op.Index.End != nil {
					e, err := c.compileExpr(op.Index.End)
					if err != nil {
						return "", err
					}
					end = e
				}
				val = fmt.Sprintf("(slice %s %s %s)", val, start, end)
			}
		} else if op.Cast != nil {
			// Racket is dynamically typed; casts are no-ops
		}
	}
	return val, nil
}

func (c *Compiler) compilePrimary(p *parser.Primary) (string, error) {
	switch {
	case p.Lit != nil:
		return c.compileLiteral(p.Lit)
	case p.List != nil:
		elems := make([]string, len(p.List.Elems))
		for i, e := range p.List.Elems {
			v, err := c.compileExpr(e)
			if err != nil {
				return "", err
			}
			elems[i] = v
		}
		return "(list " + strings.Join(elems, " ") + ")", nil
	case p.Map != nil:
		pairs := make([]string, len(p.Map.Items))
		for i, it := range p.Map.Items {
			k, err := c.compileExpr(it.Key)
			if err != nil {
				return "", err
			}
			v, err := c.compileExpr(it.Value)
			if err != nil {
				return "", err
			}
			pairs[i] = fmt.Sprintf("(%s . %s)", k, v)
		}
		return "'#hash(" + strings.Join(pairs, " ") + ")", nil
	case p.Call != nil:
		return c.compileCallExpr(p.Call)
	case p.Match != nil:
		return c.compileMatchExpr(p.Match)
	case p.Selector != nil:
		return sanitizeName(p.Selector.Root), nil
	case p.Group != nil:
		expr, err := c.compileExpr(p.Group)
		if err != nil {
			return "", err
		}
		return "(" + expr + ")", nil
	default:
		return "", fmt.Errorf("unsupported primary")
	}
}

func (c *Compiler) compileCallExpr(call *parser.CallExpr) (string, error) {
	name := sanitizeName(call.Func)
	switch call.Func {
	case "len":
		name = "count"
	case "print":
		name = "displayln"
	case "str":
		name = "format"
	case "count":
		name = "count"
	case "avg":
		name = "avg"
	case "input":
		name = "read-line"
	}
	args := make([]string, len(call.Args))
	for i, a := range call.Args {
		v, err := c.compileExpr(a)
		if err != nil {
			return "", err
		}
		args[i] = v
	}
	if call.Func == "print" && len(args) > 1 {
		fmtStr := strings.TrimSpace(strings.Repeat("~a ", len(args)))
		return fmt.Sprintf("(displayln (format \"%s\" %s))", fmtStr, strings.Join(args, " ")), nil
	}
	if call.Func == "str" {
		args = append([]string{"\"~a\""}, args...)
	}
	return fmt.Sprintf("(%s %s)", name, strings.Join(args, " ")), nil
}

func (c *Compiler) compileLiteral(l *parser.Literal) (string, error) {
	switch {
	case l.Int != nil:
		return strconv.Itoa(*l.Int), nil
	case l.Float != nil:
		return strconv.FormatFloat(*l.Float, 'f', -1, 64), nil
	case l.Bool != nil:
		if bool(*l.Bool) {
			return "true", nil
		}
		return "false", nil
	case l.Str != nil:
		return strconv.Quote(*l.Str), nil
	default:
		return "", fmt.Errorf("empty literal")
	}
}

func (c *Compiler) compileMatchExpr(m *parser.MatchExpr) (string, error) {
	target, err := c.compileExpr(m.Target)
	if err != nil {
		return "", err
	}
	var b strings.Builder
	b.WriteString("(match " + target + "\n")
	for _, cs := range m.Cases {
		pat, err := c.compileMatchPattern(cs.Pattern)
		if err != nil {
			return "", err
		}
		res, err := c.compileExpr(cs.Result)
		if err != nil {
			return "", err
		}
		b.WriteString("\t[" + pat + " " + res + "]\n")
	}
	b.WriteString(")")
	return b.String(), nil
}

func (c *Compiler) compileMatchPattern(pat *parser.Expr) (string, error) {
	if isUnderscoreExpr(pat) {
		return "_", nil
	}
	return c.compileExpr(pat)
}
