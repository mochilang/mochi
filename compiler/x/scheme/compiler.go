//go:build slow

package schemecode

import (
	"bytes"
	"fmt"
	"path/filepath"
	"sort"
	"strconv"
	"strings"

	"mochi/parser"
	"mochi/types"
)

const datasetHelpers = `(import (srfi 95) (chibi json) (chibi io) (chibi) (chibi string))

(define (_to_string v)
  (call-with-output-string (lambda (p) (write v p))))

(define (_yaml_value v)
  (let ((n (string->number v)))
    (if n n v)))

(define (_parse_yaml text)
  (let ((rows '()) (cur '()))
    (for-each (lambda (ln)
                (when (string-prefix? ln "- ")
                  (when (not (null? cur))
                    (set! rows (append rows (list cur))))
                  (set! cur '())
                  (set! ln (substring ln 2 (string-length ln))))
                (when (string-contains ln ":")
                  (let* ((p (string-split ln #\:))
                         (k (string-trim (car p)))
                         (val (string-trim (string-join (cdr p) ":"))))
                    (set! cur (append cur (list (cons k (_yaml_value val))))))))
              (string-split text #\newline))
    (when (not (null? cur))
      (set! rows (append rows (list cur))))
    rows))

(define (_fetch url opts)
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
      (string->json txt))))

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
          ((string=? fmt "yaml")
           (_parse_yaml text))
          (else
           (let ((d (string->json text)))
             (if (list? d) d (list d)))))))

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
      (close-output-port out))))

(define (_lt a b)
  (cond
    ((and (number? a) (number? b)) (< a b))
    ((and (string? a) (string? b)) (string<? a b))
    ((and (pair? a) (pair? b))
      (cond
        ((null? a) (not (null? b)))
        ((null? b) #f)
        (else (let ((ka (car a)) (kb (car b)))
                (if (equal? ka kb)
                    (_lt (cdr a) (cdr b))
                    (_lt ka kb)))))
    )
    (else (string<? (_to_string a) (_to_string b)))))

(define (_sort pairs)
  (sort pairs (lambda (a b) (_lt (cdr a) (cdr b)))))`

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

const sliceHelper = `(define (_slice obj i j)
  (let* ((n (if (string? obj) (string-length obj) (length obj)))
         (start i)
         (end j))
    (when (< start 0) (set! start (+ n start)))
    (when (< end 0) (set! end (+ n end)))
    (when (< start 0) (set! start 0))
    (when (> end n) (set! end n))
    (when (< end start) (set! end start))
        (if (string? obj)
        (substring obj start end)
        (let loop ((idx 0) (xs obj) (out '()))
          (if (or (null? xs) (>= idx end))
              (reverse out)
              (loop (+ idx 1) (cdr xs)
                    (if (>= idx start)
                        (cons (car xs) out)
                        out)))))))`

const groupHelpers = `(define (_count v)
  (cond
    ((string? v) (string-length v))
    ((and (pair? v) (assq 'Items v)) (length (cdr (assq 'Items v))))
    ((list? v) (length v))
    (else 0)))

(define (_sum v)
  (let* ((lst (cond
               ((and (pair? v) (assq 'Items v)) (cdr (assq 'Items v)))
               ((list? v) v)
               (else '())))
         (s (if (null? lst) 0 (apply + lst))))
    s))

(define (_avg v)
  (let ((lst (cond
               ((and (pair? v) (assq 'Items v)) (cdr (assq 'Items v)))
               ((list? v) v)
               (else '())))
        (n 0))
    (set! n (length lst))
    (if (= n 0) 0 (/ (_sum lst) n)))
)

(define (_exists v)
  (cond
    ((and (pair? v) (assq 'Items v)) (not (null? (cdr (assq 'Items v)))))
    ((string? v) (> (string-length v) 0))
    ((list? v) (not (null? v)))
    (else #f)))

(define (_max v)
  (let ((lst (cond
               ((and (pair? v) (assq 'Items v)) (cdr (assq 'Items v)))
               ((list? v) v)
               (else '())))
        (m 0))
    (when (not (null? lst))
      (set! m (car lst))
      (for-each (lambda (n)
                  (when (> n m) (set! m n)))
                (cdr lst)))
    m))

(define (_min v)
  (let ((lst (cond
               ((and (pair? v) (assq 'Items v)) (cdr (assq 'Items v)))
               ((list? v) v)
               (else '())))
        (m 0))
    (when (not (null? lst))
      (set! m (car lst))
      (for-each (lambda (n)
                  (when (< n m) (set! m n)))
                (cdr lst)))
    m))
(define (_group_by src keyfn)
  (let ((groups '()) (order '()))
    (for-each (lambda (it)
                (let* ((key (keyfn it))
                       (ks (format "~a" key))
                       (pair (assoc ks groups)))
                  (if pair
                      (let* ((grp (cdr pair))
                             (items (cdr (assq 'Items grp))))
                        (set-cdr! (assq 'Items grp) (append items (list it))))
                      (begin
                        (set! groups (append groups (list (cons ks (list (cons 'key key) (cons 'Items (list it)))))))
                        (set! order (append order (list ks))))))
              src)
    (map (lambda (k) (cdr (assoc k groups))) order))))`

const jsonHelper = `(define (_json v)
  (write v)
  (newline))`

const testHelpers = `(define failures 0)
(define (print-test-start name)
  (display "   test ") (display name) (display " ..."))
(define (print-test-pass) (display " ok") (newline))
(define (print-test-fail err) (display " fail ") (display err) (newline))
(define (run-test name thunk)
  (print-test-start name)
  (let ((ok #t))
    (with-exception-handler
      (lambda (e)
        (set! ok #f)
        (set! failures (+ failures 1))
        (print-test-fail e))
      (lambda () (thunk)))
    (when ok (print-test-pass))))`

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
	needSlice      bool
	needGroup      bool
	needJSON       bool
	needStringLib  bool
	loops          []loopCtx
	tmpCount       int
	mainStmts      []*parser.Statement
	tests          []testInfo
}

type testInfo struct {
	name  string
	label string
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
	return &Compiler{env: env, vars: map[string]string{}, loops: []loopCtx{}, needDataset: false, needListOps: false, needSlice: false, needGroup: false, needJSON: false, needStringLib: false, tmpCount: 0, mainStmts: nil, tests: []testInfo{}}
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
	c.needSlice = false
	c.needJSON = false
	c.needStringLib = false
	c.mainStmts = nil
	c.tests = nil
	// Declarations and tests
	for _, s := range prog.Statements {
		switch {
		case s.Type != nil:
			if err := c.compileTypeDecl(s.Type); err != nil {
				return nil, err
			}
			c.writeln("")
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

	if len(c.tests) > 0 {
		for _, t := range c.tests {
			c.writeln(fmt.Sprintf("(run-test %q %s)", t.label, t.name))
		}
		c.writeln("(when (> failures 0) (display \"\\n[FAIL] \") (display failures) (display \" test(s) failed.\\n\"))")
	}
	code := c.buf.Bytes()
	if c.needListSet || c.needStringSet || c.needMapHelpers || c.needDataset || c.needListOps || c.needSlice || c.needGroup || c.needJSON || c.needStringLib || len(c.tests) > 0 {
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
			pre.WriteString("        (if p (cdr p) '()))\n")
			pre.WriteString(")\n")
			pre.WriteString("(define (map-set m k v)\n")
			pre.WriteString("    (let ((p (assoc k m)))\n")
			pre.WriteString("        (if p\n")
			pre.WriteString("            (begin (set-cdr! p v) m)\n")
			pre.WriteString("            (cons (cons k v) m)))\n")
			pre.WriteString(")\n")
		}
		if c.needDataset {
			pre.WriteString(datasetHelpers)
			pre.WriteByte('\n')
		}
		if c.needStringLib {
			pre.WriteString("(import (chibi string))\n")
		}
		if c.needListOps {
			pre.WriteString(listOpHelpers)
			pre.WriteByte('\n')
		}
		if c.needSlice {
			pre.WriteString(sliceHelper)
			pre.WriteByte('\n')
		}
		if c.needGroup {
			pre.WriteString(groupHelpers)
			pre.WriteByte('\n')
		}
		if c.needJSON {
			pre.WriteString(jsonHelper)
			pre.WriteByte('\n')
		}
		if len(c.tests) > 0 {
			pre.WriteString(testHelpers)
			pre.WriteByte('\n')
		}
		if pre.Len() > 0 {
			pre.WriteByte('\n')
		}
		pre.Write(code)
		code = pre.Bytes()
	}
	code = FormatScheme(code)
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

func (c *Compiler) compileMethod(structName string, fn *parser.FunStmt) error {
	params := make([]string, len(fn.Params)+1)
	params[0] = "Self"
	for i, p := range fn.Params {
		params[i+1] = sanitizeName(p.Name)
	}
	prevVars := c.vars
	c.vars = map[string]string{}
	c.writeln(fmt.Sprintf("(define (%s_%s %s)", sanitizeName(structName), sanitizeName(fn.Name), strings.Join(params, " ")))
	c.indent++
	c.writeln("(call/cc (lambda (return)")
	c.indent++
	if c.env != nil {
		if st, ok := c.env.GetStruct(structName); ok {
			names := st.Order
			if len(names) == 0 {
				for fname := range st.Fields {
					names = append(names, fname)
				}
				sort.Strings(names)
			}
			for _, fname := range names {
				c.needMapHelpers = true
				c.writeln(fmt.Sprintf("(define %s (map-get Self \"%s\"))", sanitizeName(fname), sanitizeName(fname)))
			}
		}
	}
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

func (c *Compiler) compileTypeDecl(t *parser.TypeDecl) error {
	if len(t.Variants) > 0 {
		for _, v := range t.Variants {
			name := sanitizeName(v.Name)
			if len(v.Fields) == 0 {
				c.writeln(fmt.Sprintf("(define %s '%s)", name, name))
				continue
			}
			params := make([]string, len(v.Fields))
			for i, f := range v.Fields {
				params[i] = sanitizeName(f.Name)
			}
			c.writeln(fmt.Sprintf("(define (%s %s)", name, strings.Join(params, " ")))
			c.indent++
			c.writeln(fmt.Sprintf("(list '%s %s)", name, strings.Join(params, " ")))
			c.indent--
			c.writeln(")")
		}
		return nil
	}
	fields := []string{}
	methods := []*parser.FunStmt{}
	for _, m := range t.Members {
		if m.Field != nil {
			fields = append(fields, m.Field.Name)
		}
		if m.Method != nil {
			methods = append(methods, m.Method)
		}
	}
	name := sanitizeName(t.Name)
	params := make([]string, len(fields))
	for i, f := range fields {
		params[i] = sanitizeName(f)
	}
	c.writeln(fmt.Sprintf("(define (new-%s %s)", name, strings.Join(params, " ")))
	c.indent++
	parts := make([]string, len(fields))
	for i, f := range fields {
		parts[i] = fmt.Sprintf("(cons '%s %s)", sanitizeName(f), sanitizeName(f))
	}
	if len(parts) > 0 {
		c.writeln("(list " + strings.Join(parts, " ") + ")")
	} else {
		c.writeln("'()")
	}
	c.indent--
	c.writeln(")")
	for _, m := range methods {
		if err := c.compileMethod(t.Name, m); err != nil {
			return err
		}
		c.writeln("")
	}
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
	c.tests = append(c.tests, testInfo{name: name, label: t.Name})
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
			if s.Let.Type.Simple != nil && c.env != nil {
				if _, ok := c.env.GetStruct(*s.Let.Type.Simple); ok {
					c.vars[s.Let.Name] = "map"
				}
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
		expr, err := c.compileExpr(s.Var.Value)
		if err != nil {
			return err
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
			if s.Var.Type.Simple != nil && c.env != nil {
				if _, ok := c.env.GetStruct(*s.Var.Type.Simple); ok {
					c.vars[s.Var.Name] = "map"
				}
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
		if len(s.Assign.Index) == 0 && len(s.Assign.Field) == 0 {
			c.writeln(fmt.Sprintf("(set! %s %s)", lhs, rhs))
			break
		}
		var expr string
		if len(s.Assign.Field) > 0 {
			expr, err = c.compileFieldSet(lhs, s.Assign.Field, rhs)
		} else {
			expr, err = c.compileIndexedSet(lhs, s.Assign.Index, rhs, c.varType(s.Assign.Name) == "string", c.varType(s.Assign.Name) == "map")
		}
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
	case s.Update != nil:
		return c.compileUpdate(s.Update)
	case s.Fun != nil:
		return c.compileFun(s.Fun)
	case s.Expect != nil:
		return c.compileExpect(s.Expect)
	default:
		// ignore unsupported statements
	}
	return nil
}

// compileIndexedSet builds a nested list/string update expression for an indexed assignment.
// compileIndexedSet builds nested update expressions for assignments using
// indexing. When assigning into maps we use `map-get`/`map-set` to handle
// association lists instead of list-ref/list-set which only work on plain
// lists. Only the outermost container type is tracked which is sufficient for
// simple nested map updates used in the tests.
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

	// Access the current element using the appropriate lookup operator so
	// nested map assignments work correctly.
	innerSrc := fmt.Sprintf("(list-ref %s %s)", name, ie)
	nextIsMap := false
	if isMap {
		c.needMapHelpers = true
		innerSrc = fmt.Sprintf("(map-get %s %s)", name, ie)
		nextIsMap = true
	}
	inner, err := c.compileIndexedSet(innerSrc, idx[1:], rhs, false, nextIsMap)
	if err != nil {
		return "", err
	}
	if isMap {
		c.needMapHelpers = true
		return fmt.Sprintf("(map-set %s %s %s)", name, ie, inner), nil
	}
	c.needListSet = true
	return fmt.Sprintf("(list-set %s %s %s)", name, ie, inner), nil
}

func (c *Compiler) compileFieldSet(name string, fields []*parser.FieldOp, rhs string) (string, error) {
	if len(fields) == 0 {
		return rhs, nil
	}
	key := fmt.Sprintf("'%s", sanitizeName(fields[0].Name))
	if len(fields) == 1 {
		c.needMapHelpers = true
		return fmt.Sprintf("(map-set %s %s %s)", name, key, rhs), nil
	}
	c.needMapHelpers = true
	inner := fmt.Sprintf("(map-get %s %s)", name, key)
	sub, err := c.compileFieldSet(inner, fields[1:], rhs)
	if err != nil {
		return "", err
	}
	return fmt.Sprintf("(map-set %s %s %s)", name, key, sub), nil
}

func (c *Compiler) compileExpr(e *parser.Expr) (string, error) {
	if e == nil {
		return "'()", nil
	}
	return c.compileBinary(e.Binary)
}

func (c *Compiler) compileBinary(b *parser.BinaryExpr) (string, error) {
	left, err := c.compileUnary(b.Left)
	if err != nil {
		return "", err
	}
	operands := []string{left}
	ops := []string{}
	rights := []*parser.PostfixExpr{}
	strFlags := []bool{c.isStringUnary(b.Left)}
	listFlags := []bool{isListUnary(b.Left)}
	for _, part := range b.Right {
		r, err := c.compilePostfix(part.Right)
		if err != nil {
			return "", err
		}
		operands = append(operands, r)
		op := part.Op
		if part.Op == "union" && part.All {
			op = "union_all"
		}
		ops = append(ops, op)
		rights = append(rights, part.Right)
		strFlags = append(strFlags, c.isStringPostfix(part.Right))
		listFlags = append(listFlags, isListPostfix(part.Right))
	}

	prec := [][]string{{"*", "/", "%"}, {"+", "-"}, {"<", "<=", ">", ">="}, {"==", "!=", "in"}, {"union", "union_all", "except", "intersect"}, {"&&"}, {"||"}}

	for _, level := range prec {
		for i := 0; i < len(ops); {
			if contains(level, ops[i]) {
				l := operands[i]
				r := operands[i+1]
				op := ops[i]
				var expr string
				switch op {
				case "&&":
					expr = fmt.Sprintf("(and %s %s)", l, r)
				case "||":
					expr = fmt.Sprintf("(or %s %s)", l, r)
				case "+":
					if strFlags[i] || strFlags[i+1] {
						expr = fmt.Sprintf("(string-append %s %s)", l, r)
					} else if listFlags[i] || listFlags[i+1] {
						expr = fmt.Sprintf("(append %s %s)", l, r)
					} else {
						expr = fmt.Sprintf("(+ %s %s)", l, r)
					}
				case "-", "*", "/":
					expr = fmt.Sprintf("(%s %s %s)", op, l, r)
				case "%":
					expr = fmt.Sprintf("(modulo %s %s)", l, r)
				case "==":
					expr = fmt.Sprintf("(equal? %s %s)", l, r)
				case "!=":
					expr = fmt.Sprintf("(not (equal? %s %s))", l, r)
				case "in":
					root := rootNamePostfix(rights[i])
					if c.varType(root) == "string" || c.isStringPostfix(rights[i]) {
						c.needStringLib = true
						expr = fmt.Sprintf("(if (string-contains %s %s) #t #f)", r, l)
					} else if c.varType(root) == "map" || c.isMapPostfix(rights[i]) {
						expr = fmt.Sprintf("(if (assoc %s %s) #t #f)", l, r)
					} else {
						expr = fmt.Sprintf("(if (member %s %s) #t #f)", l, r)
					}
				case "union":
					c.needListOps = true
					expr = fmt.Sprintf("(_union %s %s)", l, r)
				case "union_all":
					c.needListOps = true
					expr = fmt.Sprintf("(_union_all %s %s)", l, r)
				case "except":
					c.needListOps = true
					expr = fmt.Sprintf("(_except %s %s)", l, r)
				case "intersect":
					c.needListOps = true
					expr = fmt.Sprintf("(_intersect %s %s)", l, r)
				case "<", "<=", ">", ">=":
					if strFlags[i] || strFlags[i+1] {
						fn := map[string]string{"<": "string<?", "<=": "string<=?", ">": "string>?", ">=": "string>=?"}[op]
						expr = fmt.Sprintf("(%s %s %s)", fn, l, r)
					} else {
						expr = fmt.Sprintf("(%s %s %s)", op, l, r)
					}
				default:
					expr = fmt.Sprintf("(%s %s %s)", op, l, r)
				}
				operands[i] = expr
				strFlags[i] = strFlags[i] || strFlags[i+1]
				listFlags[i] = listFlags[i] || listFlags[i+1]
				operands = append(operands[:i+1], operands[i+2:]...)
				strFlags = append(strFlags[:i+1], strFlags[i+2:]...)
				listFlags = append(listFlags[:i+1], listFlags[i+2:]...)
				rights = append(rights[:i], rights[i+1:]...)
				ops = append(ops[:i], ops[i+1:]...)
			} else {
				i++
			}
		}
	}

	if len(operands) != 1 {
		return "", fmt.Errorf("invalid expression")
	}
	return operands[0], nil
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
			if op.Index.Colon != nil {
				start := "0"
				if op.Index.Start != nil {
					s, err := c.compileExpr(op.Index.Start)
					if err != nil {
						return "", err
					}
					start = s
				}
				end := ""
				targetName := ""
				if p.Target != nil && p.Target.Selector != nil {
					targetName = p.Target.Selector.Root
				}
				if c.varType(targetName) == "string" || c.isStringPrimary(p.Target) {
					end = fmt.Sprintf("(string-length %s)", expr)
				} else {
					end = fmt.Sprintf("(length %s)", expr)
				}
				if op.Index.End != nil {
					e, err := c.compileExpr(op.Index.End)
					if err != nil {
						return "", err
					}
					end = e
				}
				c.needSlice = true
				expr = fmt.Sprintf("(_slice %s %s %s)", expr, start, end)
			} else {
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
			if p.Target != nil && p.Target.Selector != nil && len(p.Target.Selector.Tail) > 0 {
				method := p.Target.Selector.Tail[len(p.Target.Selector.Tail)-1]
				recvName := p.Target.Selector.Root
				if c.env != nil {
					if vt, err := c.env.GetVar(recvName); err == nil {
						if st, ok := vt.(types.StructType); ok {
							if _, ok := st.Methods[method]; ok {
								recv := sanitizeName(recvName)
								expr = fmt.Sprintf("(%s_%s %s", sanitizeName(st.Name), sanitizeName(method), recv)
								if len(args) > 0 {
									expr += " " + strings.Join(args, " ")
								}
								expr += ")"
								continue
							}
						}
					}
				}
				if method == "contains" && len(args) == 1 {
					c.needStringLib = true
					base := *p.Target
					if base.Selector != nil && len(base.Selector.Tail) > 0 {
						dup := make([]string, len(base.Selector.Tail)-1)
						copy(dup, base.Selector.Tail[:len(base.Selector.Tail)-1])
						base.Selector = &parser.SelectorExpr{Root: base.Selector.Root, Tail: dup}
					}
					recvExpr, err := c.compilePrimary(&base)
					if err != nil {
						return "", err
					}
					expr = fmt.Sprintf("(if (string-contains %s %s) #t #f)", recvExpr, args[0])
					continue
				}
			}
			expr = fmt.Sprintf("(%s %s)", expr, strings.Join(args, " "))
			continue
		}
		if op.Cast != nil {
			if op.Cast.Type != nil {
				if op.Cast.Type.Simple != nil {
					switch *op.Cast.Type.Simple {
					case "float":
						expr = fmt.Sprintf("(exact->inexact %s)", expr)
					case "int":
						if c.isStringPrimary(p.Target) {
							expr = fmt.Sprintf("(string->number %s)", expr)
						} else {
							expr = fmt.Sprintf("(inexact->exact %s)", expr)
						}
					case "string":
						expr = fmt.Sprintf("(number->string %s)", expr)
					default:
						if c.env != nil {
							if st, ok := c.env.GetStruct(*op.Cast.Type.Simple); ok {
								names := st.Order
								if len(names) == 0 {
									for fname := range st.Fields {
										names = append(names, fname)
									}
									sort.Strings(names)
								}
								vals := make([]string, len(names))
								c.needMapHelpers = true
								for i, f := range names {
									vals[i] = fmt.Sprintf("(map-get _tmp \"%s\")", sanitizeName(f))
								}
								expr = fmt.Sprintf("(let ((_tmp %s)) (new-%s %s))", expr, sanitizeName(st.Name), strings.Join(vals, " "))
							}
						}
					}
				} else if op.Cast.Type.Generic != nil {
					if op.Cast.Type.Generic.Name == "list" && len(op.Cast.Type.Generic.Args) == 1 {
						arg := op.Cast.Type.Generic.Args[0]
						if arg != nil && arg.Simple != nil && c.env != nil {
							if st, ok := c.env.GetStruct(*arg.Simple); ok {
								names := st.Order
								if len(names) == 0 {
									for fname := range st.Fields {
										names = append(names, fname)
									}
									sort.Strings(names)
								}
								vals := make([]string, len(names))
								for i, f := range names {
									vals[i] = fmt.Sprintf("(map-get _it \"%s\")", sanitizeName(f))
								}
								body := fmt.Sprintf("(new-%s %s)", sanitizeName(st.Name), strings.Join(vals, " "))
								c.needMapHelpers = true
								expr = fmt.Sprintf("(map (lambda (_it) %s) %s)", body, expr)
							}
						}
					}
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
		if p.Lit.Null {
			return "'()", nil
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
			if id, ok := identName(it.Key); ok {
				v, err := c.compileExpr(it.Value)
				if err != nil {
					return "", err
				}
				pairs[i] = fmt.Sprintf("(cons '%s %s)", sanitizeName(id), v)
				continue
			}
			if s, ok := simpleStringKey(it.Key); ok {
				v, err := c.compileExpr(it.Value)
				if err != nil {
					return "", err
				}
				pairs[i] = fmt.Sprintf("(cons \"%s\" %s)", s, v)
				continue
			}
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
		if c.env != nil {
			if ut, ok := c.env.FindUnionByVariant(p.Struct.Name); ok {
				if st, ok2 := ut.Variants[p.Struct.Name]; ok2 {
					names := st.Order
					if len(names) == 0 {
						for fn := range st.Fields {
							names = append(names, fn)
						}
						sort.Strings(names)
					}
					vals := make([]string, len(names))
					for i, n := range names {
						for _, f := range p.Struct.Fields {
							if f.Name == n {
								v, err := c.compileExpr(f.Value)
								if err != nil {
									return "", err
								}
								vals[i] = v
								break
							}
						}
					}
					return fmt.Sprintf("(%s %s)", sanitizeName(p.Struct.Name), strings.Join(vals, " ")), nil
				}
			}
		}
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
	case p.Query != nil:
		return c.compileQueryExpr(p.Query)
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
	paramCount := 0
	if c.env != nil {
		if t, err := c.env.GetVar(call.Func); err == nil {
			if ft, ok := t.(types.FuncType); ok {
				paramCount = len(ft.Params)
			}
		}
	}
	if paramCount > 0 && len(args) < paramCount && recv == "" {
		rest := make([]string, paramCount-len(args))
		for i := range rest {
			rest[i] = fmt.Sprintf("_p%d", i)
		}
		all := append([]string{strings.Join(args, " ")}, rest...)
		callArgs := strings.TrimSpace(strings.Join(all, " "))
		if callArgs != "" {
			callArgs = " " + callArgs
		}
		return fmt.Sprintf("(lambda (%s) (%s%s))", strings.Join(rest, " "), sanitizeName(call.Func), callArgs), nil
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
		return fmt.Sprintf("(let ((s (open-output-string))) (write %s s) (get-output-string s))", args[0]), nil
	case "count":
		if len(args) != 1 {
			return "", fmt.Errorf("count expects 1 arg")
		}
		c.needGroup = true
		return fmt.Sprintf("(_count %s)", args[0]), nil
	case "exists":
		if len(args) != 1 {
			return "", fmt.Errorf("exists expects 1 arg")
		}
		root := rootNameExpr(call.Args[0])
		if c.varType(root) == "string" || c.isStringExpr(call.Args[0]) {
			return fmt.Sprintf("(> (string-length %s) 0)", args[0]), nil
		}
		if c.isMapExpr(call.Args[0]) || c.isListExpr(call.Args[0]) {
			return fmt.Sprintf("(> (length %s) 0)", args[0]), nil
		}
		c.needGroup = true
		return fmt.Sprintf("(_exists %s)", args[0]), nil
	case "avg":
		if len(args) != 1 {
			return "", fmt.Errorf("avg expects 1 arg")
		}
		c.needGroup = true
		return fmt.Sprintf("(_avg %s)", args[0]), nil
	case "max":
		if len(args) != 1 {
			return "", fmt.Errorf("max expects 1 arg")
		}
		c.needGroup = true
		return fmt.Sprintf("(_max %s)", args[0]), nil
	case "min":
		if len(args) != 1 {
			return "", fmt.Errorf("min expects 1 arg")
		}
		c.needGroup = true
		return fmt.Sprintf("(_min %s)", args[0]), nil
	case "sum":
		if len(args) != 1 {
			return "", fmt.Errorf("sum expects 1 arg")
		}
		c.needGroup = true
		return fmt.Sprintf("(_sum %s)", args[0]), nil
	case "substr":
		if len(args) != 3 {
			return "", fmt.Errorf("substr expects 3 args")
		}
		c.needSlice = true
		return fmt.Sprintf("(_slice %s %s %s)", args[0], args[1], args[2]), nil
	case "reverse":
		if len(args) != 1 {
			return "", fmt.Errorf("reverse expects 1 arg")
		}
		root := rootNameExpr(call.Args[0])
		if c.varType(root) == "string" || c.isStringExpr(call.Args[0]) {
			c.needStringLib = true
			return fmt.Sprintf("(list->string (reverse (string->list %s)))", args[0]), nil
		}
		return fmt.Sprintf("(reverse %s)", args[0]), nil
	case "append":
		if len(args) != 2 {
			return "", fmt.Errorf("append expects 2 args")
		}
		return fmt.Sprintf("(append %s (list %s))", args[0], args[1]), nil
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
	case "pow":
		if len(args) != 2 {
			return "", fmt.Errorf("pow expects 2 args")
		}
		return fmt.Sprintf("(expt %s %s)", args[0], args[1]), nil
	case "json":
		if len(args) != 1 {
			return "", fmt.Errorf("json expects 1 arg")
		}
		c.needJSON = true
		return fmt.Sprintf("(_json %s)", args[0]), nil
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
	if sub.needSlice {
		c.needSlice = true
	}
	if sub.needDataset {
		c.needDataset = true
	}
	if sub.needListOps {
		c.needListOps = true
	}
	if sub.needJSON {
		c.needJSON = true
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
	}
	if elseExpr == "" {
		// emit (when ...) form when no else branch is present
		return fmt.Sprintf("(when %s %s)", cond, thenExpr), nil
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
		if v, names, ok := isUnionVariantPattern(cse.Pattern); ok {
			buf.WriteString(" ((and (pair? _t) (eq? (car _t) '" + sanitizeName(v) + "))")
			if len(names) > 0 {
				buf.WriteString(" (let (")
				for i, n := range names {
					if i > 0 {
						buf.WriteString(" ")
					}
					buf.WriteString(fmt.Sprintf("(%s (list-ref _t %d))", n, i+1))
				}
				buf.WriteString(") " + res + "))")
			} else {
				buf.WriteString(" " + res + ")")
			}
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
	// pass an empty string when no path is provided so runtime treats it as stdin
	path := "\"\""
	if l.Path != nil {
		p := *l.Path
		if strings.HasPrefix(p, "../") {
			p = filepath.Join("tests", p[3:])
		}
		path = strconv.Quote(p)
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
	// pass an empty string when no path is provided so runtime treats it as stdout
	path := "\"\""
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

func (c *Compiler) compileQueryExpr(q *parser.QueryExpr) (string, error) {
	if q.Group != nil && len(q.Froms) == 0 && len(q.Joins) == 0 && q.Sort == nil && q.Skip == nil && q.Take == nil {
		src, err := c.compileExpr(q.Source)
		if err != nil {
			return "", err
		}
		orig := c.env
		child := types.NewEnv(c.env)
		child.SetVar(q.Var, types.AnyType{}, true)
		c.env = child
		var cond string
		var sortExpr, skipExpr, takeExpr string
		if q.Where != nil {
			cond, err = c.compileExpr(q.Where)
			if err != nil {
				c.env = orig
				return "", err
			}
		}
		keyExpr, err := c.compileExpr(q.Group.Exprs[0])
		if err != nil {
			c.env = orig
			return "", err
		}
		genv := types.NewEnv(child)
		genv.SetVar(q.Group.Name, types.GroupType{Elem: types.AnyType{}}, true)
		c.env = genv
		valExpr, err := c.compileExpr(q.Select)
		var havingExpr string
		if err == nil && q.Group.Having != nil {
			havingExpr, err = c.compileExpr(q.Group.Having)
		}
		c.env = orig
		if err != nil {
			return "", err
		}
		c.needGroup = true
		var b strings.Builder
		b.WriteString("(let ((_tmp '()))\n")
		b.WriteString(fmt.Sprintf("  (for-each (lambda (%s)\n", sanitizeName(q.Var)))
		if cond != "" {
			b.WriteString(fmt.Sprintf("    (when %s (set! _tmp (append _tmp (list %s))))\n", cond, sanitizeName(q.Var)))
		} else {
			b.WriteString(fmt.Sprintf("    (set! _tmp (append _tmp (list %s)))\n", sanitizeName(q.Var)))
		}
		b.WriteString(fmt.Sprintf("  ) (if (string? %s) (string->list %s) %s))\n", src, src, src))
		b.WriteString("  (let ((_res '()))\n")
		b.WriteString(fmt.Sprintf("    (for-each (lambda (%s)\n", sanitizeName(q.Group.Name)))
		if havingExpr != "" {
			b.WriteString(fmt.Sprintf("      (when %s (set! _res (append _res (list %s))))\n", havingExpr, valExpr))
		} else {
			b.WriteString(fmt.Sprintf("      (set! _res (append _res (list %s)))\n", valExpr))
		}
		b.WriteString(fmt.Sprintf("    ) (_group_by _tmp (lambda (%s) %s)))\n", sanitizeName(q.Var), keyExpr))
		if sortExpr != "" {
			b.WriteString("    (set! _res (_sort (map (lambda (x) (cons x " + sortExpr + ")) _res)))\n")
			b.WriteString("    (set! _res (map car _res))\n")
		}
		if skipExpr != "" {
			c.needSlice = true
			b.WriteString(fmt.Sprintf("    (set! _res (_slice _res %s (length _res)))\n", skipExpr))
		}
		if takeExpr != "" {
			c.needSlice = true
			b.WriteString(fmt.Sprintf("    (set! _res (_slice _res 0 %s))\n", takeExpr))
		}
		b.WriteString("    _res))")
		return b.String(), nil
	}
	if q.Group != nil {
		src, err := c.compileExpr(q.Source)
		if err != nil {
			return "", err
		}
		orig := c.env
		child := types.NewEnv(c.env)
		child.SetVar(q.Var, types.AnyType{}, true)
		for _, f := range q.Froms {
			child.SetVar(f.Var, types.AnyType{}, true)
		}
		for _, j := range q.Joins {
			child.SetVar(j.Var, types.AnyType{}, true)
		}
		c.env = child
		var cond string
		var sortExpr, skipExpr, takeExpr string
		if q.Where != nil {
			cond, err = c.compileExpr(q.Where)
			if err != nil {
				c.env = orig
				return "", err
			}
		}
		keyExpr, err := c.compileExpr(q.Group.Exprs[0])
		if err != nil {
			c.env = orig
			return "", err
		}
		genv := types.NewEnv(child)
		genv.SetVar(q.Group.Name, types.GroupType{Elem: types.AnyType{}}, true)
		c.env = genv
		valExpr, err := c.compileExpr(q.Select)
		var havingExpr string
		if err == nil && q.Group.Having != nil {
			havingExpr, err = c.compileExpr(q.Group.Having)
		}
		c.env = orig
		if err != nil {
			return "", err
		}
		if q.Sort != nil {
			c.env = genv
			sortExpr, err = c.compileExpr(q.Sort)
			c.env = orig
			if err != nil {
				return "", err
			}
			c.needDataset = true
		}
		if q.Skip != nil {
			c.env = genv
			skipExpr, err = c.compileExpr(q.Skip)
			c.env = orig
			if err != nil {
				return "", err
			}
		}
		if q.Take != nil {
			c.env = genv
			takeExpr, err = c.compileExpr(q.Take)
			c.env = orig
			if err != nil {
				return "", err
			}
		}
		fromSrcs := make([]string, len(q.Froms))
		for i, f := range q.Froms {
			fs, err := c.compileExpr(f.Src)
			if err != nil {
				return "", err
			}
			fromSrcs[i] = fs
		}
		joinSrcs := make([]string, len(q.Joins))
		joinOns := make([]string, len(q.Joins))
		for i, j := range q.Joins {
			js, err := c.compileExpr(j.Src)
			if err != nil {
				return "", err
			}
			joinSrcs[i] = js
			if j.On != nil {
				on, err := c.compileExpr(j.On)
				if err != nil {
					return "", err
				}
				joinOns[i] = on
			}
		}
		c.needGroup = true
		var b strings.Builder
		b.WriteString("(let ((_tmp '()))\n")
		b.WriteString(fmt.Sprintf("  (for-each (lambda (%s)\n", sanitizeName(q.Var)))
		var writeJoins func(int, string)
		writeJoins = func(j int, indent string) {
			if j == len(joinSrcs) {
				if cond != "" {
					b.WriteString(indent + "(when " + cond + "\n")
					indent += "  "
				}
				b.WriteString(indent + fmt.Sprintf("(set! _tmp (append _tmp (list %s)))\n", sanitizeName(q.Var)))
				if cond != "" {
					indent = indent[:len(indent)-2]
					b.WriteString(indent + ")\n")
				}
				return
			}
			jv := sanitizeName(q.Joins[j].Var)
			js := joinSrcs[j]
			on := joinOns[j]
			if q.Joins[j].Side != nil && *q.Joins[j].Side == "left" {
				tmp := fmt.Sprintf("_ms%d", c.tmpCount)
				flag := fmt.Sprintf("_m%d", c.tmpCount)
				c.tmpCount++
				b.WriteString(indent + fmt.Sprintf("(let ((%s '()) (%s #f))\n", tmp, flag))
				b.WriteString(indent + fmt.Sprintf("  (for-each (lambda (%s)\n", jv))
				if on != "" {
					b.WriteString(indent + "    (when " + on + "\n")
					b.WriteString(indent + fmt.Sprintf("      (set! %s (append %s (list %s)))\n", tmp, tmp, jv))
					b.WriteString(indent + fmt.Sprintf("      (set! %s #t))\n", flag))
				} else {
					b.WriteString(indent + fmt.Sprintf("    (set! %s (append %s (list %s)))\n", tmp, tmp, jv))
					b.WriteString(indent + fmt.Sprintf("    (set! %s #t)\n", flag))
				}
				b.WriteString(fmt.Sprintf(") (if (string? %s) (string->list %s) %s))\n", js, js, js))
				b.WriteString(indent + fmt.Sprintf("  (if %s\n", flag))
				b.WriteString(indent + fmt.Sprintf("      (for-each (lambda (%s)\n", jv))
				writeJoins(j+1, indent+"        ")
				b.WriteString(indent + fmt.Sprintf("      ) %s)\n", tmp))
				b.WriteString(indent + "      (let ((" + jv + " '()))\n")
				writeJoins(j+1, indent+"        ")
				b.WriteString(indent + "      ))\n")
				b.WriteString(indent + ")\n")
			} else {
				b.WriteString(indent + fmt.Sprintf("(for-each (lambda (%s)\n", jv))
				if on != "" {
					b.WriteString(indent + "  (when " + on + "\n")
					writeJoins(j+1, indent+"    ")
					b.WriteString(indent + "  )")
				} else {
					writeJoins(j+1, indent+"  ")
				}
				b.WriteString(fmt.Sprintf(") (if (string? %s) (string->list %s) %s))\n", js, js, js))
			}
		}
		var writeLoops func(int, string)
		writeLoops = func(i int, indent string) {
			if i == len(fromSrcs) {
				writeJoins(0, indent)
				return
			}
			fv := sanitizeName(q.Froms[i].Var)
			fs := fromSrcs[i]
			b.WriteString(indent + fmt.Sprintf("(for-each (lambda (%s)\n", fv))
			writeLoops(i+1, indent+"  ")
			b.WriteString(indent + fmt.Sprintf(") (if (string? %s) (string->list %s) %s))\n", fs, fs, fs))
		}
		writeLoops(0, "    ")
		b.WriteString(fmt.Sprintf("  ) (if (string? %s) (string->list %s) %s))\n", src, src, src))
		b.WriteString("  (let ((_res '()))\n")
		b.WriteString(fmt.Sprintf("    (for-each (lambda (%s)\n", sanitizeName(q.Group.Name)))
		if havingExpr != "" {
			b.WriteString(fmt.Sprintf("      (when %s (set! _res (append _res (list %s))))\n", havingExpr, valExpr))
		} else {
			b.WriteString(fmt.Sprintf("      (set! _res (append _res (list %s)))\n", valExpr))
		}
		b.WriteString(fmt.Sprintf("    ) (_group_by _tmp (lambda (%s) %s)))\n", sanitizeName(q.Var), keyExpr))
		if sortExpr != "" {
			b.WriteString("    (set! _res (_sort (map (lambda (x) (cons x " + sortExpr + ")) _res)))\n")
			b.WriteString("    (set! _res (map car _res))\n")
		}
		if skipExpr != "" {
			c.needSlice = true
			b.WriteString(fmt.Sprintf("    (set! _res (_slice _res %s (length _res)))\n", skipExpr))
		}
		if takeExpr != "" {
			c.needSlice = true
			b.WriteString(fmt.Sprintf("    (set! _res (_slice _res 0 %s))\n", takeExpr))
		}
		b.WriteString("    _res))")
		return b.String(), nil
	}
	src, err := c.compileExpr(q.Source)
	if err != nil {
		return "", err
	}

	orig := c.env
	child := types.NewEnv(c.env)
	child.SetVar(q.Var, types.AnyType{}, true)
	for _, f := range q.Froms {
		child.SetVar(f.Var, types.AnyType{}, true)
	}
	c.env = child

	sel, err := c.compileExpr(q.Select)
	if err != nil {
		c.env = orig
		return "", err
	}
	var cond, skipExpr, takeExpr, sortExpr string
	if q.Where != nil {
		cond, err = c.compileExpr(q.Where)
		if err != nil {
			c.env = orig
			return "", err
		}
	}
	if q.Sort != nil {
		sortExpr, err = c.compileExpr(q.Sort)
		if err != nil {
			c.env = orig
			return "", err
		}
		c.needDataset = true
	}
	if q.Skip != nil {
		skipExpr, err = c.compileExpr(q.Skip)
		if err != nil {
			c.env = orig
			return "", err
		}
	}
	if q.Take != nil {
		takeExpr, err = c.compileExpr(q.Take)
		if err != nil {
			c.env = orig
			return "", err
		}
	}
	fromSrcs := make([]string, len(q.Froms))
	for i, f := range q.Froms {
		fs, err := c.compileExpr(f.Src)
		if err != nil {
			c.env = orig
			return "", err
		}
		fromSrcs[i] = fs
	}
	joinSrcs := make([]string, len(q.Joins))
	joinOns := make([]string, len(q.Joins))
	for i, j := range q.Joins {
		js, err := c.compileExpr(j.Src)
		if err != nil {
			c.env = orig
			return "", err
		}
		joinSrcs[i] = js
		if j.On != nil {
			on, err := c.compileExpr(j.On)
			if err != nil {
				c.env = orig
				return "", err
			}
			joinOns[i] = on
		}
	}
	c.env = orig

	var b strings.Builder
	b.WriteString("(let ((_res '())")
	if sortExpr != "" {
		b.WriteString(" (_tmp '())")
	}
	b.WriteString(")\n")
	b.WriteString(fmt.Sprintf("  (for-each (lambda (%s)\n", sanitizeName(q.Var)))

	var writeJoins func(int, string)
	writeJoins = func(j int, indent string) {
		if j == len(joinSrcs) {
			if cond != "" {
				b.WriteString(indent + "(when " + cond + "\n")
				indent += "  "
			}
			if sortExpr != "" {
				b.WriteString(indent + fmt.Sprintf("(set! _tmp (append _tmp (list (cons %s %s))))\n", sel, sortExpr))
			} else {
				b.WriteString(indent + fmt.Sprintf("(set! _res (append _res (list %s)))\n", sel))
			}
			if cond != "" {
				indent = indent[:len(indent)-2]
				b.WriteString(indent + ")\n")
			}
			return
		}
		jv := sanitizeName(q.Joins[j].Var)
		js := joinSrcs[j]
		on := joinOns[j]
		if q.Joins[j].Side != nil && *q.Joins[j].Side == "left" {
			tmp := fmt.Sprintf("_ms%d", c.tmpCount)
			flag := fmt.Sprintf("_m%d", c.tmpCount)
			c.tmpCount++
			b.WriteString(indent + fmt.Sprintf("(let ((%s '()) (%s #f))\n", tmp, flag))
			b.WriteString(indent + fmt.Sprintf("  (for-each (lambda (%s)\n", jv))
			if on != "" {
				b.WriteString(indent + "    (when " + on + "\n")
				b.WriteString(indent + fmt.Sprintf("      (set! %s (append %s (list %s)))\n", tmp, tmp, jv))
				b.WriteString(indent + fmt.Sprintf("      (set! %s #t))\n", flag))
			} else {
				b.WriteString(indent + fmt.Sprintf("    (set! %s (append %s (list %s)))\n", tmp, tmp, jv))
				b.WriteString(indent + fmt.Sprintf("    (set! %s #t)\n", flag))
			}
			b.WriteString(fmt.Sprintf(") (if (string? %s) (string->list %s) %s))\n", js, js, js))
			b.WriteString(indent + fmt.Sprintf("  (if %s\n", flag))
			b.WriteString(indent + fmt.Sprintf("      (for-each (lambda (%s)\n", jv))
			writeJoins(j+1, indent+"        ")
			b.WriteString(indent + fmt.Sprintf("      ) %s)\n", tmp))
			b.WriteString(indent + "      (let ((" + jv + " '()))\n")
			writeJoins(j+1, indent+"        ")
			b.WriteString(indent + "      ))\n")
			b.WriteString(indent + ")\n")
		} else {
			b.WriteString(indent + fmt.Sprintf("(for-each (lambda (%s)\n", jv))
			if on != "" {
				b.WriteString(indent + "  (when " + on + "\n")
				writeJoins(j+1, indent+"    ")
				b.WriteString(indent + "  )")
			} else {
				writeJoins(j+1, indent+"  ")
			}
			b.WriteString(fmt.Sprintf(") (if (string? %s) (string->list %s) %s))\n", js, js, js))
		}
	}

	var writeLoops func(int, string)
	writeLoops = func(i int, indent string) {
		if i == len(fromSrcs) {
			writeJoins(0, indent)
			return
		}
		fv := sanitizeName(q.Froms[i].Var)
		fs := fromSrcs[i]
		b.WriteString(indent + fmt.Sprintf("(for-each (lambda (%s)\n", fv))
		writeLoops(i+1, indent+"  ")
		b.WriteString(indent + fmt.Sprintf(") (if (string? %s) (string->list %s) %s))\n", fs, fs, fs))
	}

	writeLoops(0, "    ")
	b.WriteString(fmt.Sprintf("  ) (if (string? %s) (string->list %s) %s))\n", src, src, src))
	if sortExpr != "" {
		b.WriteString("  (set! _res (_sort _tmp))\n")
		b.WriteString("  (set! _res (map car _res))\n")
	}
	if skipExpr != "" {
		c.needSlice = true
		b.WriteString(fmt.Sprintf("  (set! _res (_slice _res %s (length _res)))\n", skipExpr))
	}
	if takeExpr != "" {
		c.needSlice = true
		b.WriteString(fmt.Sprintf("  (set! _res (_slice _res 0 %s))\n", takeExpr))
	}
	b.WriteString("  _res)")
	return b.String(), nil
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

func isUnionVariantPattern(e *parser.Expr) (string, []string, bool) {
	if e == nil || len(e.Binary.Right) != 0 {
		return "", nil, false
	}
	u := e.Binary.Left
	if len(u.Ops) != 0 || u.Value == nil {
		return "", nil, false
	}
	p := u.Value
	if len(p.Ops) != 0 || p.Target == nil || p.Target.Call == nil {
		return "", nil, false
	}
	call := p.Target.Call
	names := make([]string, len(call.Args))
	for i, a := range call.Args {
		if len(a.Binary.Right) != 0 || len(a.Binary.Left.Ops) != 0 || a.Binary.Left.Value == nil {
			return "", nil, false
		}
		sel := a.Binary.Left.Value.Target
		if sel == nil || sel.Selector == nil || len(a.Binary.Left.Value.Ops) != 0 {
			return "", nil, false
		}
		names[i] = sanitizeName(sel.Selector.Root)
	}
	return call.Func, names, true
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
