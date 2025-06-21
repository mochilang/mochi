package schemecode

import (
	"bytes"
	"fmt"
	"sort"
	"strconv"
	"strings"

	"mochi/parser"
	"mochi/types"
)

const datasetHelpers = `(define (_fetch url opts)
  (let* ((method (if (and opts (assq 'method opts)) (cdr (assq 'method opts)) "GET"))
         (args (list "curl" "-s" "-X" method)))
    (when (and opts (assq 'headers opts))
      (for-each (lambda (p)
                  (set! args (append args (list "-H" (format "~a: ~a" (car p) (cdr p))))))
                (cdr (assq 'headers opts))))
    (when (and opts (assq 'query opts))
      (let* ((q (cdr (assq 'query opts)))
             (qs (string-join (map (lambda (p) (format "~a=~a" (car p) (cdr p))) q) "&")))
        (set! url (string-append url (if (string-contains url "?") "&" "?") qs))))
    (when (and opts (assq 'body opts))
      (set! args (append args (list "-d" (json->string (cdr (assq 'body opts)))))))
    (when (and opts (assq 'timeout opts))
      (set! args (append args (list "--max-time" (format "~a" (cdr (assq 'timeout opts)))))))
    (set! args (append args (list url)))
    (let* ((p (open-input-pipe (string-join args " ")))
           (txt (port->string p)))
      (close-input-port p)
      (string->json txt)))

(define (_load path opts)
  (let* ((fmt (if (and opts (assq 'format opts)) (cdr (assq 'format opts)) "json"))
         (in (if (or (not path) (string=? path "") (string=? path "-"))
                 (current-input-port)
                 (open-input-file path)))
         (text (port->string in)))
    (when (not (eq? in (current-input-port)))
      (close-input-port in))
    (cond ((string=? fmt "jsonl")
           (map string->json
                (filter (lambda (l) (not (string=? l "")))
                        (string-split text #\newline))))
          (else
           (let ((d (string->json text)))
             (if (list? d) d (list d))))))

(define (_save rows path opts)
  (let* ((fmt (if (and opts (assq 'format opts)) (cdr (assq 'format opts)) "json"))
         (out (if (or (not path) (string=? path "") (string=? path "-"))
                  (current-output-port)
                  (open-output-file path))))
    (cond ((string=? fmt "jsonl")
           (for-each (lambda (r) (write-string (json->string r) out) (newline out)) rows))
          (else
           (write-string (json->string rows) out)))
    (when (not (eq? out (current-output-port)))
      (close-output-port out)))`

const listOpHelpers = `(define (_union_all a b)
  (append a b))

(define (_union a b)
  (let ((res a))
    (for-each (lambda (it)
                (when (not (member it res))
                  (set! res (append res (list it)))))
              b)
    res))

(define (_except a b)
  (let ((res '()))
    (for-each (lambda (it)
                (when (not (member it b))
                  (set! res (append res (list it)))))
              a)
    res))

(define (_intersect a b)
  (let ((res '()))
    (for-each (lambda (it)
                (when (and (member it b) (not (member it res)))
                  (set! res (append res (list it)))))
              a)
    res))`

// Compiler translates a Mochi AST into Scheme source code (minimal subset).
type Compiler struct {
	buf            bytes.Buffer
	indent         int
	env            *types.Env
	inFun          bool
	vars           map[string]string // local variable types within a function
	needListSet    bool
	needStringSet  bool
	needMapHelpers bool
	needDataset    bool
	needListOps    bool
	loops          []loopCtx
	mainStmts      []*parser.Statement
	tests          []string
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

// New creates a new Scheme compiler instance.
func New(env *types.Env) *Compiler {
	return &Compiler{env: env, vars: map[string]string{}, loops: []loopCtx{}, needDataset: false, needListOps: false, mainStmts: nil, tests: nil}
}

func (c *Compiler) writeIndent() {
	for i := 0; i < c.indent; i++ {
		c.buf.WriteByte('\t')
	}
}

func (c *Compiler) writeln(s string) {
	c.writeIndent()
	c.buf.WriteString(s)
	c.buf.WriteByte('\n')
}

// Compile converts the given program to Scheme source code.
func (c *Compiler) Compile(prog *parser.Program) ([]byte, error) {
	c.buf.Reset()
	c.needListSet = false
	c.needStringSet = false
	c.needMapHelpers = false
	c.needDataset = false
	c.needListOps = false
	c.mainStmts = nil
	c.tests = nil
	// Declarations and tests
	for _, s := range prog.Statements {
		switch {
		case s.Fun != nil:
			if err := c.compileFun(s.Fun); err != nil {
				return nil, err
			}
			c.writeln("")
		case s.Test != nil:
			if err := c.compileTestBlock(s.Test); err != nil {
				return nil, err
			}
			c.writeln("")
		default:
			c.mainStmts = append(c.mainStmts, s)
		}
	}

	// Main body
	for _, s := range c.mainStmts {
		if err := c.compileStmt(s); err != nil {
			return nil, err
		}
	}

	for _, name := range c.tests {
		c.writeln("(" + name + ")")
	}
	code := c.buf.Bytes()
	if c.needListSet || c.needStringSet || c.needMapHelpers || c.needDataset || c.needListOps {
		var pre bytes.Buffer
		if c.needListSet {
			pre.WriteString("(define (list-set lst idx val)\n")
			pre.WriteString("    (let loop ((i idx) (l lst))\n")
			pre.WriteString("        (if (null? l)\n")
			pre.WriteString("            '()\n")
			pre.WriteString("            (if (= i 0)\n")
			pre.WriteString("                (cons val (cdr l))\n")
			pre.WriteString("                (cons (car l) (loop (- i 1) (cdr l))))))\n")
			pre.WriteString(")\n")
		}
		if c.needStringSet {
			pre.WriteString("(define (string-set str idx ch)\n")
			pre.WriteString("    (list->string (list-set (string->list str) idx ch))\n")
			pre.WriteString(")\n")
		}
		if c.needMapHelpers {
			pre.WriteString("(define (map-get m k)\n")
			pre.WriteString("    (let ((p (assoc k m)))\n")
			pre.WriteString("        (if p (cdr p) '())))\n")
			pre.WriteString(")\n")
			pre.WriteString("(define (map-set m k v)\n")
			pre.WriteString("    (let ((p (assoc k m)))\n")
			pre.WriteString("        (if p\n")
			pre.WriteString("            (begin (set-cdr! p v) m)\n")
			pre.WriteString("            (cons (cons k v) m))))\n")
			pre.WriteString(")\n")
		}
		if c.needDataset {
			pre.WriteString(datasetHelpers)
			pre.WriteByte('\n')
		}
		if c.needListOps {
			pre.WriteString(listOpHelpers)
			pre.WriteByte('\n')
		}
		if pre.Len() > 0 {
			pre.WriteByte('\n')
		}
		pre.Write(code)
		code = pre.Bytes()
	}
	return code, nil
}

func (c *Compiler) compileFun(fn *parser.FunStmt) error {
	params := make([]string, len(fn.Params))
	for i, p := range fn.Params {
		params[i] = sanitizeName(p.Name)
	}
	prevVars := c.vars
	c.vars = map[string]string{}
	for _, p := range fn.Params {
		if p.Type != nil {
			if p.Type.Simple != nil && *p.Type.Simple == "string" {
				c.vars[p.Name] = "string"
			}
			if p.Type.Generic != nil && p.Type.Generic.Name == "map" {
				c.vars[p.Name] = "map"
			}
		}
	}
	c.writeln(fmt.Sprintf("(define (%s %s)", sanitizeName(fn.Name), strings.Join(params, " ")))
	c.indent++
	c.writeln("(call/cc (lambda (return)")
	c.indent++

	vars := map[string]bool{}
	collectVars(fn.Body, vars)
	names := make([]string, 0, len(vars))
	for v := range vars {
		names = append(names, sanitizeName(v))
	}
	sort.Strings(names)
	for _, n := range names {
		c.writeln(fmt.Sprintf("(define %s '())", n))
	}

	prev := c.inFun
	c.inFun = true
	for _, st := range fn.Body {
		if err := c.compileStmt(st); err != nil {
			return err
		}
	}
	c.inFun = prev
	c.vars = prevVars
	c.indent--
	c.writeln("))")
	c.indent--
	c.writeln(")")
	return nil
}

func collectVars(stmts []*parser.Statement, vars map[string]bool) {
	for _, s := range stmts {
		switch {
		case s.Let != nil:
			vars[s.Let.Name] = true
		case s.Var != nil:
			vars[s.Var.Name] = true
		case s.For != nil:
			collectVars(s.For.Body, vars)
		case s.If != nil:
			collectVars(s.If.Then, vars)
			if s.If.ElseIf != nil {
				collectVars([]*parser.Statement{{If: s.If.ElseIf}}, vars)
			}
			collectVars(s.If.Else, vars)
		case s.While != nil:
			collectVars(s.While.Body, vars)
		}
	}
}

func (c *Compiler) compileTestBlock(t *parser.TestBlock) error {
	name := "test_" + sanitizeName(t.Name)
	c.tests = append(c.tests, name)
	c.writeln(fmt.Sprintf("(define (%s)", name))
	c.indent++
	for _, st := range t.Body {
		if err := c.compileStmt(st); err != nil {
			return err
		}
	}
	c.indent--
	c.writeln(")")
	return nil
}

func (c *Compiler) compileExpect(e *parser.ExpectStmt) error {
	expr, err := c.compileExpr(e.Value)
	if err != nil {
		return err
	}
	c.writeln(fmt.Sprintf("(when (not %s) (error \"expect failed\"))", expr))
	return nil
}

func (c *Compiler) compileStmt(s *parser.Statement) error {
	switch {
	case s.Let != nil:
		expr, err := c.compileExpr(s.Let.Value)
		if err != nil {
			return err
		}
		name := sanitizeName(s.Let.Name)
		if c.inFun {
			c.writeln(fmt.Sprintf("(set! %s %s)", name, expr))
		} else {
			c.writeln(fmt.Sprintf("(define %s %s)", name, expr))
		}
		if s.Let.Type != nil {
			if s.Let.Type.Simple != nil && *s.Let.Type.Simple == "string" {
				c.vars[s.Let.Name] = "string"
			}
			if s.Let.Type.Generic != nil && s.Let.Type.Generic.Name == "map" {
				c.vars[s.Let.Name] = "map"
			}
		} else {
			if c.isStringExpr(s.Let.Value) {
				c.vars[s.Let.Name] = "string"
			}
			if c.isMapExpr(s.Let.Value) {
				c.vars[s.Let.Name] = "map"
			}
		}
	case s.Var != nil:
		expr := "()"
		if s.Var.Value != nil {
			v, err := c.compileExpr(s.Var.Value)
			if err != nil {
				return err
			}
			expr = v
		}
		name := sanitizeName(s.Var.Name)
		if c.inFun {
			c.writeln(fmt.Sprintf("(set! %s %s)", name, expr))
		} else {
			c.writeln(fmt.Sprintf("(define %s %s)", name, expr))
		}
		if s.Var.Type != nil {
			if s.Var.Type.Simple != nil && *s.Var.Type.Simple == "string" {
				c.vars[s.Var.Name] = "string"
			}
			if s.Var.Type.Generic != nil && s.Var.Type.Generic.Name == "map" {
				c.vars[s.Var.Name] = "map"
			}
		} else {
			if c.isStringExpr(s.Var.Value) {
				c.vars[s.Var.Name] = "string"
			}
			if c.isMapExpr(s.Var.Value) {
				c.vars[s.Var.Name] = "map"
			}
		}
	case s.Assign != nil:
		rhs, err := c.compileExpr(s.Assign.Value)
		if err != nil {
			return err
		}
		lhs := sanitizeName(s.Assign.Name)
		if len(s.Assign.Index) == 0 {
			c.writeln(fmt.Sprintf("(set! %s %s)", lhs, rhs))
			break
		}
		expr, err := c.compileIndexedSet(lhs, s.Assign.Index, rhs, c.varType(s.Assign.Name) == "string", c.varType(s.Assign.Name) == "map")
		if err != nil {
			return err
		}
		c.writeln(fmt.Sprintf("(set! %s %s)", lhs, expr))
	case s.Return != nil:
		expr, err := c.compileExpr(s.Return.Value)
		if err != nil {
			return err
		}
		c.writeln(fmt.Sprintf("(return %s)", expr))
	case s.Expr != nil:
		expr, err := c.compileExpr(s.Expr.Expr)
		if err != nil {
			return err
		}
		c.writeln(expr)
	case s.For != nil:
		return c.compileFor(s.For)
	case s.If != nil:
		return c.compileIf(s.If)
	case s.While != nil:
		return c.compileWhile(s.While)
	case s.Break != nil:
		return c.compileBreak()
	case s.Continue != nil:
		return c.compileContinue()
	case s.Expect != nil:
		return c.compileExpect(s.Expect)
	default:
		// ignore unsupported statements
	}
	return nil
}

// compileIndexedSet builds a nested list/string update expression for an indexed assignment.
func (c *Compiler) compileIndexedSet(name string, idx []*parser.IndexOp, rhs string, isString, isMap bool) (string, error) {
	if len(idx) == 0 {
		return rhs, nil
	}
	ie, err := c.compileExpr(idx[0].Start)
	if err != nil {
		return "", err
	}
	if len(idx) == 1 {
		if isString {
			c.needStringSet = true
			return fmt.Sprintf("(string-set %s %s %s)", name, ie, rhs), nil
		}
		if isMap {
			c.needMapHelpers = true
			return fmt.Sprintf("(map-set %s %s %s)", name, ie, rhs), nil
		}
		c.needListSet = true
		return fmt.Sprintf("(list-set %s %s %s)", name, ie, rhs), nil
	}
	inner, err := c.compileIndexedSet(fmt.Sprintf("(list-ref %s %s)", name, ie), idx[1:], rhs, false, false)
	if err != nil {
		return "", err
	}
	c.needListSet = true
	return fmt.Sprintf("(list-set %s %s %s)", name, ie, inner), nil
}

func (c *Compiler) compileFor(st *parser.ForStmt) error {
	name := sanitizeName(st.Name)
	ctrl := hasLoopCtrl(st.Body)
	if st.RangeEnd != nil {
		start, err := c.compileExpr(st.Source)
		if err != nil {
			return err
		}
		end, err := c.compileExpr(st.RangeEnd)
		if err != nil {
			return err
		}
		if !ctrl {
			c.writeln(fmt.Sprintf("(let loop ((%s %s))", name, start))
			c.indent++
			c.writeln(fmt.Sprintf("(if (< %s %s)", name, end))
			c.indent++
			c.writeln("(begin")
			c.indent++
			for _, s := range st.Body {
				if err := c.compileStmt(s); err != nil {
					return err
				}
			}
			c.writeln(fmt.Sprintf("(loop (+ %s 1))", name))
			c.indent--
			c.writeln(")")
			c.indent--
			c.writeln("'())")
			c.indent--
			c.writeln(")")
			return nil
		}
		brk := fmt.Sprintf("brk%d", len(c.loops))
		loop := fmt.Sprintf("loop%d", len(c.loops))
		c.writeln("(call/cc (lambda (" + brk + ")")
		c.indent++
		c.writeln("(let " + loop + " ((" + name + " " + start + "))")
		c.indent++
		c.writeln("(when (< " + name + " " + end + ")")
		c.indent++
		c.pushLoop(brk, loop, "(+ "+name+" 1)")
		for _, s := range st.Body {
			if err := c.compileStmt(s); err != nil {
				c.popLoop()
				return err
			}
		}
		c.writeln("(" + loop + " (+ " + name + " 1)))")
		c.popLoop()
		c.indent--
		c.writeln(")")
		c.indent--
		c.writeln(")")
		c.indent--
		c.writeln(")")
		return nil
	}

	src, err := c.compileExpr(st.Source)
	if err != nil {
		return err
	}
	root := rootNameExpr(st.Source)
	isStr := c.varType(root) == "string" || c.isStringExpr(st.Source)
	idx := sanitizeName(name + "_idx")
	lenExpr := fmt.Sprintf("(length %s)", src)
	elemExpr := fmt.Sprintf("(list-ref %s %s)", src, idx)
	if isStr {
		lenExpr = fmt.Sprintf("(string-length %s)", src)
		elemExpr = fmt.Sprintf("(string-ref %s %s)", src, idx)
	}
	if !ctrl {
		c.writeln(fmt.Sprintf("(let loop ((%s 0))", idx))
		c.indent++
		c.writeln(fmt.Sprintf("(if (< %s %s)", idx, lenExpr))
		c.indent++
		c.writeln("(begin")
		c.indent++
		if st.Name != "_" {
			c.writeln(fmt.Sprintf("(let ((%s %s))", name, elemExpr))
			c.indent++
			for _, s := range st.Body {
				if err := c.compileStmt(s); err != nil {
					return err
				}
			}
			c.indent--
			c.writeln(")")
		} else {
			for _, s := range st.Body {
				if err := c.compileStmt(s); err != nil {
					return err
				}
			}
		}
		c.writeln(fmt.Sprintf("(loop (+ %s 1))", idx))
		c.indent--
		c.writeln(")")
		c.indent--
		c.writeln("'())")
		c.indent--
		c.writeln(")")
		return nil
	}
	brk := fmt.Sprintf("brk%d", len(c.loops))
	loop := fmt.Sprintf("loop%d", len(c.loops))
	c.writeln("(call/cc (lambda (" + brk + ")")
	c.indent++
	c.writeln("(let " + loop + " ((" + idx + " 0))")
	c.indent++
	c.writeln("(when (< " + idx + " " + lenExpr + ")")
	c.indent++
	if st.Name != "_" {
		c.writeln(fmt.Sprintf("(let ((%s %s))", name, elemExpr))
		c.indent++
	}
	c.pushLoop(brk, loop, "(+ "+idx+" 1)")
	for _, s := range st.Body {
		if err := c.compileStmt(s); err != nil {
			c.popLoop()
			return err
		}
	}
	c.popLoop()
	if st.Name != "_" {
		c.indent--
		c.writeln(")")
	}
	c.writeln("(" + loop + " (+ " + idx + " 1)))")
	c.indent--
	c.writeln(")")
	c.indent--
	c.writeln(")")
	c.indent--
	c.writeln(")")
	return nil
}

func rootNameExpr(e *parser.Expr) string {
	if e == nil || e.Binary == nil || e.Binary.Left == nil {
		return ""
	}
	return rootNameUnary(e.Binary.Left)
}

func rootNameUnary(u *parser.Unary) string {
	if u == nil {
		return ""
	}
	return rootNamePostfix(u.Value)
}

func rootNamePostfix(p *parser.PostfixExpr) string {
	if p == nil {
		return ""
	}
	if p.Target != nil && p.Target.Selector != nil {
		return p.Target.Selector.Root
	}
	return ""
}

func (c *Compiler) compileIf(st *parser.IfStmt) error {
	cond, err := c.compileExpr(st.Cond)
	if err != nil {
		return err
	}
	c.writeln(fmt.Sprintf("(if %s", cond))
	c.indent++
	c.writeln("(begin")
	c.indent++
	for _, s := range st.Then {
		if err := c.compileStmt(s); err != nil {
			return err
		}
	}
	c.indent--
	c.writeln(")")
	if st.ElseIf != nil {
		if err := c.compileIf(st.ElseIf); err != nil {
			return err
		}
	} else if len(st.Else) > 0 {
		c.writeln("(begin")
		c.indent++
		for _, s := range st.Else {
			if err := c.compileStmt(s); err != nil {
				return err
			}
		}
		c.indent--
		c.writeln(")")
	} else {
		c.writeln("'()")
	}
	c.indent--
	c.writeln(")")
	return nil
}

func (c *Compiler) compileWhile(st *parser.WhileStmt) error {
	cond, err := c.compileExpr(st.Cond)
	if err != nil {
		return err
	}
	ctrl := hasLoopCtrl(st.Body)
	if !ctrl {
		c.writeln("(let loop ()")
		c.indent++
		c.writeln(fmt.Sprintf("(if %s", cond))
		c.indent++
		c.writeln("(begin")
		c.indent++
		for _, s := range st.Body {
			if err := c.compileStmt(s); err != nil {
				return err
			}
		}
		c.writeln("(loop)")
		c.indent--
		c.writeln(")")
		c.indent--
		c.writeln("'())")
		c.indent--
		c.writeln(")")
		return nil
	}
	brk := fmt.Sprintf("brk%d", len(c.loops))
	loop := fmt.Sprintf("loop%d", len(c.loops))
	c.writeln("(call/cc (lambda (" + brk + ")")
	c.indent++
	c.writeln("(let " + loop + " ()")
	c.indent++
	c.writeln(fmt.Sprintf("(when %s", cond))
	c.indent++
	c.pushLoop(brk, loop, "")
	for _, s := range st.Body {
		if err := c.compileStmt(s); err != nil {
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
	c.indent--
	c.writeln(")")
	return nil
}

func (c *Compiler) compileBreak() error {
	if len(c.loops) == 0 {
		return fmt.Errorf("break not in loop")
	}
	lbl := c.loops[len(c.loops)-1].brk
	c.writeln("(" + lbl + " '())")
	return nil
}

func (c *Compiler) compileContinue() error {
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

func (c *Compiler) compileExpr(e *parser.Expr) (string, error) {
	if e == nil {
		return "()", nil
	}
	return c.compileBinary(e.Binary)
}

func (c *Compiler) compileBinary(b *parser.BinaryExpr) (string, error) {
	leftAst := b.Left
	left, err := c.compileUnary(leftAst)
	if err != nil {
		return "", err
	}
	expr := left
	for _, op := range b.Right {
		rightAst := op.Right
		right, err := c.compilePostfix(rightAst)
		if err != nil {
			return "", err
		}
		switch op.Op {
		case "+":
			if isListUnary(leftAst) || isListPostfix(rightAst) {
				expr = fmt.Sprintf("(append %s %s)", expr, right)
			} else {
				expr = fmt.Sprintf("(+ %s %s)", expr, right)
			}
		case "-", "*", "/":
			expr = fmt.Sprintf("(%s %s %s)", op.Op, expr, right)
		case "%":
			expr = fmt.Sprintf("(modulo %s %s)", expr, right)
		case "==":
			expr = fmt.Sprintf("(= %s %s)", expr, right)
		case "!=":
			expr = fmt.Sprintf("(not (= %s %s))", expr, right)
		case "&&":
			expr = fmt.Sprintf("(and %s %s)", expr, right)
		case "||":
			expr = fmt.Sprintf("(or %s %s)", expr, right)
		case "in":
			root := rootNamePostfix(rightAst)
			if c.varType(root) == "string" || c.isStringPostfix(rightAst) {
				expr = fmt.Sprintf("(if (string-contains %s %s) #t #f)", right, expr)
			} else if c.varType(root) == "map" || c.isMapPostfix(rightAst) {
				expr = fmt.Sprintf("(if (assoc %s %s) #t #f)", expr, right)
			} else {
				expr = fmt.Sprintf("(if (member %s %s) #t #f)", expr, right)
			}
		case "union":
			c.needListOps = true
			if op.All {
				expr = fmt.Sprintf("(_union_all %s %s)", expr, right)
			} else {
				expr = fmt.Sprintf("(_union %s %s)", expr, right)
			}
		case "except":
			c.needListOps = true
			expr = fmt.Sprintf("(_except %s %s)", expr, right)
		case "intersect":
			c.needListOps = true
			expr = fmt.Sprintf("(_intersect %s %s)", expr, right)
		case "<", "<=", ">", ">=":
			expr = fmt.Sprintf("(%s %s %s)", op.Op, expr, right)
		default:
			expr = fmt.Sprintf("(%s %s %s)", op.Op, expr, right)
		}
		leftAst = &parser.Unary{Value: rightAst}
	}
	return expr, nil
}

func (c *Compiler) compileUnary(u *parser.Unary) (string, error) {
	expr, err := c.compilePostfix(u.Value)
	if err != nil {
		return "", err
	}
	for i := len(u.Ops) - 1; i >= 0; i-- {
		op := u.Ops[i]
		if op == "-" {
			expr = fmt.Sprintf("(- %s)", expr)
		} else if op == "!" {
			expr = fmt.Sprintf("(not %s)", expr)
		}
	}
	return expr, nil
}

func (c *Compiler) compilePostfix(p *parser.PostfixExpr) (string, error) {
	expr, err := c.compilePrimary(p.Target)
	if err != nil {
		return "", err
	}
	for _, op := range p.Ops {
		if op.Index != nil {
			idx, err := c.compileExpr(op.Index.Start)
			if err != nil {
				return "", err
			}
			targetName := ""
			if p.Target != nil && p.Target.Selector != nil {
				targetName = p.Target.Selector.Root
			}
			if c.varType(targetName) == "string" || c.isStringPrimary(p.Target) {
				expr = fmt.Sprintf("(string-ref %s %s)", expr, idx)
			} else if c.varType(targetName) == "map" || c.isMapPrimary(p.Target) {
				c.needMapHelpers = true
				expr = fmt.Sprintf("(map-get %s %s)", expr, idx)
			} else {
				expr = fmt.Sprintf("(list-ref %s %s)", expr, idx)
			}
			continue
		}
		if op.Call != nil {
			args := make([]string, len(op.Call.Args))
			for i, a := range op.Call.Args {
				v, err := c.compileExpr(a)
				if err != nil {
					return "", err
				}
				args[i] = v
			}
			expr = fmt.Sprintf("(%s %s)", expr, strings.Join(args, " "))
			continue
		}
		if op.Cast != nil {
			if op.Cast.Type != nil && op.Cast.Type.Simple != nil {
				switch *op.Cast.Type.Simple {
				case "float":
					expr = fmt.Sprintf("(exact->inexact %s)", expr)
				case "int":
					expr = fmt.Sprintf("(inexact->exact %s)", expr)
				case "string":
					expr = fmt.Sprintf("(number->string %s)", expr)
				}
			}
			continue
		}
	}
	return expr, nil
}

func isListUnary(u *parser.Unary) bool {
	if u == nil || len(u.Ops) > 0 {
		return false
	}
	return isListPostfix(u.Value)
}

func isListPostfix(p *parser.PostfixExpr) bool {
	if p == nil || len(p.Ops) > 0 {
		return false
	}
	return p.Target != nil && p.Target.List != nil
}

func (c *Compiler) compilePrimary(p *parser.Primary) (string, error) {
	switch {
	case p.Lit != nil:
		if p.Lit.Int != nil {
			return strconv.Itoa(*p.Lit.Int), nil
		}
		if p.Lit.Float != nil {
			s := strconv.FormatFloat(*p.Lit.Float, 'f', -1, 64)
			if !strings.ContainsAny(s, ".eE") {
				s += ".0"
			}
			return s, nil
		}
		if p.Lit.Bool != nil {
			if *p.Lit.Bool {
				return "#t", nil
			}
			return "#f", nil
		}
		if p.Lit.Str != nil {
			return strconv.Quote(*p.Lit.Str), nil
		}
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
			pairs[i] = fmt.Sprintf("(cons %s %s)", k, v)
		}
		return "(list " + strings.Join(pairs, " ") + ")", nil
	case p.Struct != nil:
		parts := make([]string, len(p.Struct.Fields))
		for i, f := range p.Struct.Fields {
			v, err := c.compileExpr(f.Value)
			if err != nil {
				return "", err
			}
			parts[i] = fmt.Sprintf("(cons '%s %s)", sanitizeName(f.Name), v)
		}
		return "(list " + strings.Join(parts, " ") + ")", nil
	case p.Selector != nil:
		if len(p.Selector.Tail) == 0 {
			return sanitizeName(p.Selector.Root), nil
		}
		expr := sanitizeName(p.Selector.Root)
		for _, s := range p.Selector.Tail {
			c.needMapHelpers = true
			expr = fmt.Sprintf("(map-get %s '%s)", expr, sanitizeName(s))
		}
		return expr, nil
	case p.Group != nil:
		expr, err := c.compileExpr(p.Group)
		if err != nil {
			return "", err
		}
		return expr, nil
	case p.If != nil:
		return c.compileIfExpr(p.If)
	case p.Match != nil:
		return c.compileMatchExpr(p.Match)
	case p.Fetch != nil:
		return c.compileFetchExpr(p.Fetch)
	case p.Load != nil:
		return c.compileLoadExpr(p.Load)
	case p.Save != nil:
		return c.compileSaveExpr(p.Save)
	case p.FunExpr != nil:
		return c.compileFunExpr(p.FunExpr)
	case p.Call != nil:
		return c.compileCall(p.Call, "")
	}
	return sanitizeName(p.Selector.Root), nil
}

func (c *Compiler) compileCall(call *parser.CallExpr, recv string) (string, error) {
	args := make([]string, len(call.Args))
	for i, a := range call.Args {
		v, err := c.compileExpr(a)
		if err != nil {
			return "", err
		}
		args[i] = v
	}
	switch call.Func {
	case "len":
		if len(args) != 1 {
			return "", fmt.Errorf("len expects 1 arg")
		}
		root := rootNameExpr(call.Args[0])
		if c.varType(root) == "string" || c.isStringExpr(call.Args[0]) {
			return fmt.Sprintf("(string-length %s)", args[0]), nil
		}
		return fmt.Sprintf("(length %s)", args[0]), nil
	case "print":
		if len(args) == 0 {
			return "", fmt.Errorf("print expects at least 1 arg")
		}
		parts := make([]string, 0, len(args)*2+1)
		for i, a := range args {
			if i > 0 {
				parts = append(parts, "(display \" \")")
			}
			parts = append(parts, fmt.Sprintf("(display %s)", a))
		}
		parts = append(parts, "(newline)")
		return "(begin " + strings.Join(parts, " ") + ")", nil
	case "str":
		if len(args) != 1 {
			return "", fmt.Errorf("str expects 1 arg")
		}
		return fmt.Sprintf("(format \"~a\" %s)", args[0]), nil
	case "count":
		if len(args) != 1 {
			return "", fmt.Errorf("count expects 1 arg")
		}
		root := rootNameExpr(call.Args[0])
		if c.varType(root) == "string" || c.isStringExpr(call.Args[0]) {
			return fmt.Sprintf("(string-length %s)", args[0]), nil
		}
		return fmt.Sprintf("(length %s)", args[0]), nil
	case "avg":
		if len(args) != 1 {
			return "", fmt.Errorf("avg expects 1 arg")
		}
		return fmt.Sprintf("(let ((lst %s)) (if (null? lst) 0 (/ (apply + lst) (length lst))))", args[0]), nil
	case "push":
		if len(args) != 2 {
			return "", fmt.Errorf("push expects 2 args")
		}
		return fmt.Sprintf("(append %s (list %s))", args[0], args[1]), nil
	case "keys":
		if len(args) != 1 {
			return "", fmt.Errorf("keys expects 1 arg")
		}
		return fmt.Sprintf("(map car %s)", args[0]), nil
	case "input":
		if len(args) != 0 {
			return "", fmt.Errorf("input expects no args")
		}
		return "(read-line)", nil
	}
	if recv != "" {
		return fmt.Sprintf("(%s %s %s)", recv, call.Func, strings.Join(args, " ")), nil
	}
	return fmt.Sprintf("(%s %s)", sanitizeName(call.Func), strings.Join(args, " ")), nil
}

func (c *Compiler) compileFunExpr(fn *parser.FunExpr) (string, error) {
	params := make([]string, len(fn.Params))
	for i, p := range fn.Params {
		params[i] = sanitizeName(p.Name)
	}
	sub := &Compiler{env: c.env, vars: map[string]string{}, inFun: true}
	for _, p := range fn.Params {
		if p.Type != nil {
			if p.Type.Simple != nil && *p.Type.Simple == "string" {
				sub.vars[p.Name] = "string"
			}
			if p.Type.Generic != nil && p.Type.Generic.Name == "map" {
				sub.vars[p.Name] = "map"
			}
		}
	}
	sub.indent = 2

	vars := map[string]bool{}
	collectVars(fn.BlockBody, vars)
	names := make([]string, 0, len(vars))
	for v := range vars {
		names = append(names, sanitizeName(v))
	}
	sort.Strings(names)
	for _, n := range names {
		sub.writeln(fmt.Sprintf("(define %s '())", n))
	}
	if fn.ExprBody != nil {
		expr, err := sub.compileExpr(fn.ExprBody)
		if err != nil {
			return "", err
		}
		sub.writeln(fmt.Sprintf("(return %s)", expr))
	} else {
		for _, st := range fn.BlockBody {
			if err := sub.compileStmt(st); err != nil {
				return "", err
			}
		}
	}

	body := sub.buf.String()
	if sub.needListSet {
		c.needListSet = true
	}
	if sub.needStringSet {
		c.needStringSet = true
	}
	if sub.needMapHelpers {
		c.needMapHelpers = true
	}

	var buf bytes.Buffer
	buf.WriteString("(lambda (" + strings.Join(params, " ") + ")\n")
	buf.WriteString("\t(call/cc (lambda (return)\n")
	buf.WriteString(body)
	buf.WriteString("\t))\n")
	buf.WriteString(")")
	return buf.String(), nil
}

func (c *Compiler) compileIfExpr(ie *parser.IfExpr) (string, error) {
	cond, err := c.compileExpr(ie.Cond)
	if err != nil {
		return "", err
	}
	thenExpr, err := c.compileExpr(ie.Then)
	if err != nil {
		return "", err
	}
	var elseExpr string
	if ie.ElseIf != nil {
		elseExpr, err = c.compileIfExpr(ie.ElseIf)
		if err != nil {
			return "", err
		}
	} else if ie.Else != nil {
		elseExpr, err = c.compileExpr(ie.Else)
		if err != nil {
			return "", err
		}
	} else {
		elseExpr = "'()"
	}
	return fmt.Sprintf("(if %s %s %s)", cond, thenExpr, elseExpr), nil
}

func (c *Compiler) compileMatchExpr(m *parser.MatchExpr) (string, error) {
	target, err := c.compileExpr(m.Target)
	if err != nil {
		return "", err
	}
	var buf bytes.Buffer
	buf.WriteString("(let ((_t " + target + ")) (cond")
	hasDefault := false
	for _, cse := range m.Cases {
		res, err := c.compileExpr(cse.Result)
		if err != nil {
			return "", err
		}
		if isUnderscoreExpr(cse.Pattern) {
			buf.WriteString(" (else " + res + ")")
			hasDefault = true
			continue
		}
		pat, err := c.compileExpr(cse.Pattern)
		if err != nil {
			return "", err
		}
		buf.WriteString(" ((equal? _t " + pat + ") " + res + ")")
	}
	if !hasDefault {
		buf.WriteString(" (else '())")
	}
	buf.WriteString("))")
	return buf.String(), nil
}

func (c *Compiler) compileFetchExpr(f *parser.FetchExpr) (string, error) {
	c.needDataset = true
	url, err := c.compileExpr(f.URL)
	if err != nil {
		return "", err
	}
	opts := "'()"
	if f.With != nil {
		o, err := c.compileExpr(f.With)
		if err != nil {
			return "", err
		}
		opts = o
	}
	return fmt.Sprintf("(_fetch %s %s)", url, opts), nil
}

func (c *Compiler) compileLoadExpr(l *parser.LoadExpr) (string, error) {
	c.needDataset = true
	path := "''"
	if l.Path != nil {
		path = strconv.Quote(*l.Path)
	}
	opts := "'()"
	if l.With != nil {
		o, err := c.compileExpr(l.With)
		if err != nil {
			return "", err
		}
		opts = o
	}
	return fmt.Sprintf("(_load %s %s)", path, opts), nil
}

func (c *Compiler) compileSaveExpr(s *parser.SaveExpr) (string, error) {
	c.needDataset = true
	src, err := c.compileExpr(s.Src)
	if err != nil {
		return "", err
	}
	path := "''"
	if s.Path != nil {
		path = strconv.Quote(*s.Path)
	}
	opts := "'()"
	if s.With != nil {
		o, err := c.compileExpr(s.With)
		if err != nil {
			return "", err
		}
		opts = o
	}
	return fmt.Sprintf("(_save %s %s %s)", src, path, opts), nil
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
	if len(p.Ops) != 0 {
		return false
	}
	return p.Target != nil && p.Target.Selector != nil && p.Target.Selector.Root == "_" && len(p.Target.Selector.Tail) == 0
}

func sanitizeName(name string) string {
	if name == "" {
		return ""
	}
	var b strings.Builder
	for i, r := range name {
		if r == '_' || ('a' <= r && r <= 'z') || ('A' <= r && r <= 'Z') || ('0' <= r && r <= '9' && i > 0) {
			b.WriteRune(r)
		} else {
			b.WriteRune('_')
		}
	}
	s := b.String()
	if s == "" {
		return "_"
	}
	if !(s[0] >= 'a' && s[0] <= 'z' || s[0] >= 'A' && s[0] <= 'Z' || s[0] == '_') {
		s = "_" + s
	}
	return s
}
