package rktcode

import (
	"bytes"
	"fmt"
	"strconv"
	"strings"

	"mochi/parser"
	"mochi/types"
)

const datasetHelpers = `(define (_fetch url opts)
  (define opts (or opts (hash)))
  (define method (hash-ref opts 'method "GET"))
  (define args (list "curl" "-s" "-X" method))
  (when (hash-has-key? opts 'headers)
    (for ([k (hash-keys (hash-ref opts 'headers))])
      (set! args (append args (list "-H" (format "~a: ~a" k (hash-ref (hash-ref opts 'headers) k)))))))
  (when (hash-has-key? opts 'query)
    (define q (hash-ref opts 'query))
    (define qs (string-join (for/list ([k (hash-keys q)]) (format "~a=~a" k (hash-ref q k))) "&"))
    (set! url (string-append url (if (regexp-match? #px"\\?" url) "&" "?") qs)))
  (when (hash-has-key? opts 'body)
    (set! args (append args (list "-d" (jsexpr->string (hash-ref opts 'body))))) )
  (when (hash-has-key? opts 'timeout)
    (set! args (append args (list "--max-time" (format "~a" (hash-ref opts 'timeout))))) )
  (set! args (append args (list url)))
  (define out (with-output-to-string (lambda () (apply system* args))))
  (string->jsexpr out))

(define (_load path opts)
  (define opts (or opts (hash)))
  (define fmt (hash-ref opts 'format "json"))
  (define text (if path (call-with-input-file path port->string) (port->string (current-input-port))))
  (cond [(string=? fmt "jsonl") (for/list ([l (in-lines (open-input-string text))] #:unless (regexp-match? #px"^\\s*$" l)) (string->jsexpr l))]
        [(string=? fmt "json") (let ([d (string->jsexpr text)]) (if (list? d) d (list d)))]
        [else '()]))

(define (_save rows path opts)
  (define opts (or opts (hash)))
  (define fmt (hash-ref opts 'format "json"))
  (define out (if path (open-output-file path #:exists 'replace) (current-output-port)))
  (cond [(string=? fmt "jsonl") (for ([r rows]) (fprintf out "~a\n" (jsexpr->string r)))]
        [(string=? fmt "json") (fprintf out "~a" (jsexpr->string rows))])
  (when path (close-output-port out)))

;; grouping helpers
(struct _Group (key Items) #:mutable)

(define (_group_by src keyfn)
  (define groups (make-hash))
  (define order '())
  (for ([it src])
    (define k (keyfn it))
    (define ks (format "~a" k))
    (define g (hash-ref groups ks #f))
    (unless g
      (set! g (_Group k '()))
      (hash-set! groups ks g)
      (set! order (append order (list ks))))
    (set-_Group-Items! g (append (_Group-Items g) (list it))))
  (for/list ([ks order]) (hash-ref groups ks)))

(define (_distinct xs)
  (remove-duplicates xs))
`

const setOpsHelpers = `(define (union-all a b) (append (list->list a) (list->list b)))
(define (union a b)
  (let loop ([res (list->list a)] [xs (list->list b)])
    (if (null? xs) res
        (let ([x (car xs)])
          (if (member x res)
              (loop res (cdr xs))
              (loop (append res (list x)) (cdr xs)))))) )
(define (except a b)
  (for/list ([x (list->list a)] #:unless (member x (list->list b))) x))
(define (intersect a b)
  (for/fold ([res '()]) ([x (list->list a)])
    (if (and (member x (list->list b)) (not (member x res)))
        (append res (list x))
        res)))`

const llmHelpers = `(define (_genText prompt model params)
  ;; TODO: connect to an LLM
  prompt)

(define (_genEmbed text model params)
  ;; naive embedding as character codes
  (for/list ([c (in-string text)]) (exact->inexact (char->integer c))))

(define (_genStruct ctor fields prompt model params)
  ;; parse JSON and build struct using provided constructor
  (define data (string->jsexpr prompt))
  (apply ctor (map (lambda (f) (hash-ref data f)) fields)))`

const jsonHelpers = `(define (to-jsexpr v)
  (cond [(hash? v)
         (for/hash ([(k val) (in-hash v)])
           (values (if (string? k) (string->symbol k) k) (to-jsexpr val)))]
        [(list? v)
         (map to-jsexpr v)]
        [else v]))`

// Compiler translates Mochi AST into Racket source code.
type Compiler struct {
	buf          bytes.Buffer
	indent       int
	env          *types.Env
	loops        []loopCtx
	needsDataset bool
	needsSetOps  bool
	needsMatch   bool
	needsLLM     bool
	needsJSON    bool
	imports      []string
	exports      []string
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

func hasDefine(stmts []*parser.Statement) bool {
	for _, s := range stmts {
		switch {
		case s.Let != nil || s.Var != nil || s.Fun != nil:
			return true
		case s.If != nil:
			if hasDefine(s.If.Then) || hasDefine(s.If.Else) || hasDefineIf(s.If.ElseIf) {
				return true
			}
		case s.For != nil:
			if hasDefine(s.For.Body) {
				return true
			}
		case s.While != nil:
			if hasDefine(s.While.Body) {
				return true
			}
		}
	}
	return false
}

func hasDefineIf(ifst *parser.IfStmt) bool {
	if ifst == nil {
		return false
	}
	if hasDefine(ifst.Then) || hasDefine(ifst.Else) {
		return true
	}
	return hasDefineIf(ifst.ElseIf)
}

func New(env *types.Env) *Compiler {
	return &Compiler{env: env}
}

func (c *Compiler) Compile(prog *parser.Program) ([]byte, error) {
	c.writeln("#lang racket")
	c.writeln("; generated by Mochi")
	c.writeln(";require-placeholder")
	c.writeln(";provide-placeholder")
	c.writeln("")
	// helpers for indexing and slicing
	c.writeln("(define (idx x i)")
	c.writeln("  (cond [(string? x) (let* ([n (string-length x)] [idx (if (< i 0) (+ i n) i)]) (char->integer (string-ref x idx)))]")
	c.writeln("        [(hash? x) (hash-ref x i)]")
	c.writeln("        [else (let* ([n (length x)] [idx (if (< i 0) (+ i n) i)]) (list-ref x idx))]))")
	c.writeln("(define (slice x s e)")
	c.writeln("  (if (string? x)")
	c.writeln("      (let* ([n (string-length x)]")
	c.writeln("             [start (if (< s 0) (+ s n) s)]")
	c.writeln("             [end (if (< e 0) (+ e n) e)]")
	c.writeln("             [start (max 0 start)]")
	c.writeln("             [end (min n end)]")
	c.writeln("             [end (if (< end start) start end)])")
	c.writeln("        (substring x start end))")
	c.writeln("      (let* ([n (length x)]")
	c.writeln("             [start (if (< s 0) (+ s n) s)]")
	c.writeln("             [end (if (< e 0) (+ e n) e)]")
	c.writeln("             [start (max 0 start)]")
	c.writeln("             [end (min n end)]")
	c.writeln("             [end (if (< end start) start end)])")
	// Close the slice helper with an extra paren to finish the define
	c.writeln("        (take (drop x start) (- end start)))))")
	c.writeln("(define (count x)")
	c.writeln("  (cond [(string? x) (string-length x)]")
	c.writeln("        [(hash? x) (hash-count x)]")
	c.writeln("        [else (length x)]))")
	c.writeln("(define (avg x)")
	c.writeln("  (let ([n (count x)])")
	c.writeln("    (if (= n 0) 0")
	c.writeln("        (/ (for/fold ([s 0.0]) ([v x]) (+ s (real->double-flonum v))) n))))")

	// sum over numeric values
	c.writeln("(define (sum x)")
	c.writeln("  (for/fold ([s 0.0]) ([v x]) (+ s (real->double-flonum v))))")

	// minimum across numbers or strings
	c.writeln("(define (min-list xs)")
	c.writeln("  (cond [(null? xs) 0]")
	c.writeln("        [(for/and ([v xs]) (number? v)) (apply min xs)]")
	c.writeln("        [(for/and ([v xs]) (string? v)) (for/fold ([m (car xs)]) ([v (cdr xs)]) (if (string<? v m) v m))]")
	c.writeln("        [else (error \"unsupported min operands\")]))")

	// generic addition supporting numbers, strings and lists
	c.writeln("(define (_add a b)")
	c.writeln("  (cond [(and (number? a) (number? b)) (+ a b)]")
	c.writeln("        [(and (string? a) (string? b)) (string-append a b)]")
	c.writeln("        [(and (list? a) (list? b)) (append a b)]")
	c.writeln("        [else (error \"unsupported + operands\")]))")

	// integer division when both args are integers
	c.writeln("(define (_div a b)")
	c.writeln("  (cond [(and (integer? a) (integer? b)) (quotient a b)]")
	c.writeln("        [else (/ a b)]))")
	c.writeln("")
	c.writeln("(define (expect cond) (unless cond (error \"expect failed\")))")
	c.writeln(";dataset-placeholder")
	c.writeln(";setops-placeholder")
	c.writeln(";llm-placeholder")
	c.writeln(";json-placeholder")
	// type declarations first
	for _, s := range prog.Statements {
		if s.Type != nil {
			if err := c.compileTypeDecl(s.Type); err != nil {
				return nil, err
			}
			c.writeln("")
		}
	}
	// function declarations
	for _, s := range prog.Statements {
		if s.Fun != nil {
			if err := c.compileFun(s.Fun); err != nil {
				return nil, err
			}
			c.writeln("")
		}
	}
	// test block declarations
	for _, s := range prog.Statements {
		if s.Test != nil {
			if err := c.compileTestBlock(s.Test); err != nil {
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
	// run tests
	for _, s := range prog.Statements {
		if s.Test != nil {
			name := "test_" + sanitizeName(s.Test.Name)
			c.writeln("(" + name + ")")
		}
	}
	code := c.buf.Bytes()
	imports := []string{"racket/list"}
	if c.needsMatch {
		imports = append(imports, "racket/match")
	}
	if c.needsDataset {
		imports = append(imports, "racket/string", "racket/system", "json")
	} else if c.needsLLM {
		imports = append(imports, "json")
	}
	var extraModules []string
	var extraRequires []string
	for _, imp := range c.imports {
		if strings.HasPrefix(imp, "(require ") {
			extraRequires = append(extraRequires, imp)
		} else {
			extraModules = append(extraModules, imp)
		}
	}
	imports = append(imports, extraModules...)
	header := "(require " + strings.Join(imports, " ") + ")"
	if len(extraRequires) > 0 {
		header += "\n" + strings.Join(extraRequires, "\n")
	}
	code = bytes.Replace(code, []byte(";require-placeholder"), []byte(header), 1)
	if len(c.exports) > 0 {
		prov := "(provide " + strings.Join(c.exports, " ") + ")"
		code = bytes.Replace(code, []byte(";provide-placeholder"), []byte(prov), 1)
	} else {
		code = bytes.Replace(code, []byte(";provide-placeholder\n"), nil, 1)
	}
	if c.needsDataset {
		code = bytes.Replace(code, []byte(";dataset-placeholder\n"), []byte(datasetHelpers), 1)
	} else {
		code = bytes.Replace(code, []byte(";dataset-placeholder\n"), nil, 1)
	}
	if c.needsSetOps {
		code = bytes.Replace(code, []byte(";setops-placeholder\n"), []byte(setOpsHelpers), 1)
	} else {
		code = bytes.Replace(code, []byte(";setops-placeholder\n"), nil, 1)
	}
	if c.needsLLM {
		code = bytes.Replace(code, []byte(";llm-placeholder\n"), []byte(llmHelpers), 1)
	} else {
		code = bytes.Replace(code, []byte(";llm-placeholder\n"), nil, 1)
	}
	if c.needsJSON {
		code = bytes.Replace(code, []byte(";json-placeholder\n"), []byte(jsonHelpers), 1)
	} else {
		code = bytes.Replace(code, []byte(";json-placeholder\n"), nil, 1)
	}
	return FormatRacket(code), nil
}

func (c *Compiler) compileFun(fn *parser.FunStmt) error {
	name := sanitizeName(fn.Name)
	if fn.Export {
		c.exports = append(c.exports, name)
	}
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

func (c *Compiler) compileTestBlock(t *parser.TestBlock) error {
	name := "test_" + sanitizeName(t.Name)
	c.writeIndent()
	c.buf.WriteString("(define (" + name + ")\n")
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

func (c *Compiler) compileImport(im *parser.ImportStmt) error {
	path := strings.Trim(im.Path, "\"")
	alias := im.As
	if alias == "" {
		alias = parser.AliasFromPath(im.Path)
	}
	req := fmt.Sprintf("(require %s)", strconv.Quote(path))
	if alias != "" {
		req = fmt.Sprintf("(require (prefix-in %s: %s))", sanitizeName(alias), strconv.Quote(path))
	}
	c.imports = append(c.imports, req)
	return nil
}

func (c *Compiler) compileStmt(s *parser.Statement) error {
	switch {
	case s.Let != nil:
		return c.compileLet(s.Let)
	case s.Var != nil:
		return c.compileVar(s.Var)
	case s.Expect != nil:
		return c.compileExpect(s.Expect)
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
	case s.Import != nil:
		return c.compileImport(s.Import)
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

func (c *Compiler) compileTypeDecl(t *parser.TypeDecl) error {
	// Records are represented as hash tables, so no struct definition is needed.
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
	expr, err := c.assignNested(name, a.Index, val)
	if err != nil {
		return err
	}
	c.writeln(fmt.Sprintf("(set! %s %s)", name, expr))
	return nil
}

func (c *Compiler) assignNested(base string, idxs []*parser.IndexOp, val string) (string, error) {
	if len(idxs) == 0 {
		return val, nil
	}
	idx := idxs[0]
	if idx.Colon != nil {
		start := "0"
		if idx.Start != nil {
			s, err := c.compileExpr(idx.Start)
			if err != nil {
				return "", err
			}
			start = s
		}
		end := fmt.Sprintf("(if (string? %s) (string-length %s) (length %s))", base, base, base)
		if idx.End != nil {
			e, err := c.compileExpr(idx.End)
			if err != nil {
				return "", err
			}
			end = e
		}
		sliceBase := fmt.Sprintf("(slice %s %s %s)", base, start, end)
		innerVal, err := c.assignNested(sliceBase, idxs[1:], val)
		if err != nil {
			return "", err
		}
		expr := fmt.Sprintf("(if (string? %s) (string-append (substring %s 0 %s) %s (substring %s %s (string-length %s))) (append (take %s %s) %s (drop %s %s)))", base, base, start, innerVal, base, end, base, base, start, innerVal, base, end)
		return expr, nil
	}
	idxExpr, err := c.compileExpr(idx.Start)
	if err != nil {
		return "", err
	}
	innerBase := fmt.Sprintf("(idx %s %s)", base, idxExpr)
	innerVal, err := c.assignNested(innerBase, idxs[1:], val)
	if err != nil {
		return "", err
	}
	expr := fmt.Sprintf("(if (hash? %s) (hash-set %s %s %s) (list-set %s %s %s))", base, base, idxExpr, innerVal, base, idxExpr, innerVal)
	return expr, nil
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

func (c *Compiler) compileExpect(e *parser.ExpectStmt) error {
	expr, err := c.compileExpr(e.Value)
	if err != nil {
		return err
	}
	c.writeln(fmt.Sprintf("(unless %s (error \"expect failed\"))", expr))
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
			c.writeln(fmt.Sprintf("(for ([%s (if (hash? %s) (hash-keys %s) %s)])", name, src, src, src))
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
		c.writeln("(let ([" + name + " i])")
		c.indent++
		c.pushLoop(brk, loop, "(+ i 1)")
		for _, st := range f.Body {
			if err := c.compileStmt(st); err != nil {
				c.popLoop()
				return err
			}
		}
		c.indent--
		c.writeln(")")
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
		c.writeln("(let " + loop + " ([it (if (hash? " + src + ") (hash-keys " + src + ") " + src + ")])")
		c.indent++
		c.writeln("(when (pair? it)")
		c.indent++
		c.writeln("(let ([" + name + " (car it)])")
		c.indent++
		c.pushLoop(brk, loop, "(cdr it)")
		for _, st := range f.Body {
			if err := c.compileStmt(st); err != nil {
				c.popLoop()
				return err
			}
		}
		c.indent--
		c.writeln(")")
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
	if hasDefine(s.Then) {
		c.writeln("(let ()")
	} else {
		c.writeln("(begin")
	}
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
		if hasDefine(s.Else) {
			c.writeln("(let ()")
		} else {
			c.writeln("(begin")
		}
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
				expr = fmt.Sprintf("(_add %s %s)", l, r)
			case "-":
				expr = fmt.Sprintf("(- %s %s)", l, r)
			case "*":
				expr = fmt.Sprintf("(* %s %s)", l, r)
			case "%":
				expr = fmt.Sprintf("(modulo %s %s)", l, r)
			case "/":
				expr = fmt.Sprintf("(_div %s %s)", l, r)
			case "==":
				expr = fmt.Sprintf("(equal? %s %s)", l, r)
			case "!=":
				expr = fmt.Sprintf("(not (equal? %s %s))", l, r)
			case "<", "<=", ">", ">=":
				cmp := map[string]string{"<": "string<?", "<=": "string<=?", ">": "string>?", ">=": "string>=?"}[op]
				expr = fmt.Sprintf("(let ([la (and (string? %s) (string->number %s))] [lb (and (string? %s) (string->number %s))]) (if (and la lb) (%s la lb) (%s (format \"~a\" %s) (format \"~a\" %s))))", l, l, r, r, op, cmp, l, r)
			case "&&":
				expr = fmt.Sprintf("(and %s %s)", l, r)
			case "||":
				expr = fmt.Sprintf("(or %s %s)", l, r)
			case "in":
				expr = fmt.Sprintf("(cond [(hash? %s) (hash-has-key? %s %s)] [(string? %s) (not (false? (string-contains? %s (format \"~a\" %s))))] [else (not (false? (member %s %s)))])", r, r, l, r, r, l, l, r)
			case "union":
				c.needsSetOps = true
				expr = fmt.Sprintf("(union %s %s)", l, r)
			case "union_all":
				c.needsSetOps = true
				expr = fmt.Sprintf("(union-all %s %s)", l, r)
			case "except":
				c.needsSetOps = true
				expr = fmt.Sprintf("(except %s %s)", l, r)
			case "intersect":
				c.needsSetOps = true
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
	// Special case for `x.contains(y)` on lists, strings or maps.
	if len(p.Ops) == 1 && p.Ops[0].Call != nil && p.Target.Selector != nil &&
		len(p.Target.Selector.Tail) > 0 && p.Target.Selector.Tail[len(p.Target.Selector.Tail)-1] == "contains" {
		recvSel := &parser.Primary{Selector: &parser.SelectorExpr{Root: p.Target.Selector.Root, Tail: p.Target.Selector.Tail[:len(p.Target.Selector.Tail)-1]}}
		recv, err := c.compilePrimary(recvSel)
		if err != nil {
			return "", err
		}
		arg, err := c.compileExpr(p.Ops[0].Call.Args[0])
		if err != nil {
			return "", err
		}
		expr := fmt.Sprintf("(cond [(hash? %s) (hash-has-key? %s %s)] [(string? %s) (not (false? (string-contains? %s (format \"~a\" %s))))] [else (not (false? (member %s %s)))])", recv, recv, arg, recv, recv, arg, arg, recv)
		return expr, nil
	}

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
			if id, ok := bareIdent(it.Key); ok {
				k = strconv.Quote(sanitizeName(id))
			}
			v, err := c.compileExpr(it.Value)
			if err != nil {
				return "", err
			}
			pairs[i] = fmt.Sprintf("%s %s", k, v)
		}
		return "(hash " + strings.Join(pairs, " ") + ")", nil
	case p.Call != nil:
		return c.compileCallExpr(p.Call)
	case p.Match != nil:
		return c.compileMatchExpr(p.Match)
	case p.FunExpr != nil:
		return c.compileFunExpr(p.FunExpr)
	case p.If != nil:
		return c.compileIfExpr(p.If)
	case p.Fetch != nil:
		return c.compileFetchExpr(p.Fetch)
	case p.Generate != nil:
		return c.compileGenerateExpr(p.Generate)
	case p.Load != nil:
		return c.compileLoadExpr(p.Load)
	case p.Save != nil:
		return c.compileSaveExpr(p.Save)
	case p.Query != nil:
		return c.compileQueryExpr(p.Query)
	case p.Struct != nil:
		return c.compileStructExpr(p.Struct)
	case p.Selector != nil:
		return c.compileSelector(p.Selector)
	case p.Group != nil:
		return c.compileExpr(p.Group)
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
	case "min":
		name = "min-list"
	case "input":
		name = "read-line"
	case "now":
		return "(inexact->exact (floor (* (current-inexact-milliseconds) 1000000)))", nil
	case "json":
		c.imports = append(c.imports, "json")
		c.needsJSON = true
		if len(call.Args) == 1 {
			arg, err := c.compileExpr(call.Args[0])
			if err != nil {
				return "", err
			}
			return fmt.Sprintf("(displayln (jsexpr->string (to-jsexpr %s)))", arg), nil
		}
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
	c.needsMatch = true
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

func (c *Compiler) compileFetchExpr(f *parser.FetchExpr) (string, error) {
	c.needsDataset = true
	url, err := c.compileExpr(f.URL)
	if err != nil {
		return "", err
	}
	opts := "#f"
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
	c.needsDataset = true
	path := "#f"
	if l.Path != nil {
		path = strconv.Quote(*l.Path)
	}
	opts := "#f"
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
	c.needsDataset = true
	src, err := c.compileExpr(s.Src)
	if err != nil {
		return "", err
	}
	path := "#f"
	if s.Path != nil {
		path = strconv.Quote(*s.Path)
	}
	opts := "#f"
	if s.With != nil {
		o, err := c.compileExpr(s.With)
		if err != nil {
			return "", err
		}
		opts = o
	}
	return fmt.Sprintf("(_save %s %s %s)", src, path, opts), nil
}

func (c *Compiler) compileGenerateExpr(g *parser.GenerateExpr) (string, error) {
	var prompt, text, model string
	params := []string{}
	for _, f := range g.Fields {
		v, err := c.compileExpr(f.Value)
		if err != nil {
			return "", err
		}
		switch f.Name {
		case "prompt":
			prompt = v
		case "text":
			text = v
		case "model":
			model = v
		default:
			params = append(params, fmt.Sprintf("'%s %s", f.Name, v))
		}
	}
	if prompt == "" && g.Target != "embedding" {
		prompt = "\"\""
	}
	if text == "" && g.Target == "embedding" {
		text = "\"\""
	}
	paramStr := "#f"
	if len(params) > 0 {
		paramStr = fmt.Sprintf("(hash %s)", strings.Join(params, " "))
	}
	if model == "" {
		model = "\"\""
	}
	c.needsLLM = true
	if g.Target == "embedding" {
		return fmt.Sprintf("(_genEmbed %s %s %s)", text, model, paramStr), nil
	}
	if st, ok := c.env.GetStruct(g.Target); ok {
		fields := make([]string, len(st.Order))
		for i, f := range st.Order {
			fields[i] = strconv.Quote(f)
		}
		return fmt.Sprintf("(_genStruct %s '(%s) %s %s %s)", sanitizeName(g.Target), strings.Join(fields, " "), prompt, model, paramStr), nil
	}
	return fmt.Sprintf("(_genText %s %s %s)", prompt, model, paramStr), nil
}

func (c *Compiler) compileStructExpr(s *parser.StructLiteral) (string, error) {
	pairs := make([]string, len(s.Fields))
	for i, f := range s.Fields {
		v, err := c.compileExpr(f.Value)
		if err != nil {
			return "", err
		}
		pairs[i] = fmt.Sprintf("%q %s", sanitizeName(f.Name), v)
	}
	return "(hash " + strings.Join(pairs, " ") + ")", nil
}

func (c *Compiler) compileSelector(sel *parser.SelectorExpr) (string, error) {
	base := sanitizeName(sel.Root)
	t, err := c.env.GetVar(sel.Root)
	group := false
	if err == nil {
		if _, ok := t.(types.GroupType); ok {
			group = true
		}
	}
	for i, field := range sel.Tail {
		if group && i == 0 && field == "key" {
			base = fmt.Sprintf("(_Group-key %s)", base)
			continue
		}
		base = fmt.Sprintf("(hash-ref %s %q)", base, sanitizeName(field))
	}
	return base, nil
}

func (c *Compiler) compileQueryExpr(q *parser.QueryExpr) (string, error) {
	if q.Group != nil && len(q.Froms) == 0 && len(q.Joins) == 0 {
		src, err := c.compileExpr(q.Source)
		if err != nil {
			return "", err
		}
		if id, ok := bareIdent(q.Source); ok {
			if t, _ := c.env.GetVar(id); t != nil {
				if _, ok := t.(types.GroupType); ok {
					src = fmt.Sprintf("(_Group-Items %s)", sanitizeName(id))
				}
			}
		}
		orig := c.env
		child := types.NewEnv(c.env)
		child.SetVar(q.Var, types.AnyType{}, true)
		c.env = child
		var whereExpr string
		if q.Where != nil {
			whereExpr, err = c.compileExpr(q.Where)
			if err != nil {
				c.env = orig
				return "", err
			}
		}
		keyParts := make([]string, len(q.Group.Exprs))
		for i, e := range q.Group.Exprs {
			k, err := c.compileExpr(e)
			if err != nil {
				c.env = orig
				return "", err
			}
			keyParts[i] = k
		}
		keyExpr := strings.Join(keyParts, " ")
		if len(keyParts) > 1 {
			keyExpr = fmt.Sprintf("(list %s)", keyExpr)
		}
		genv := types.NewEnv(child)
		genv.SetVar(q.Group.Name, types.GroupType{Elem: types.AnyType{}}, true)
		c.env = genv
		valExpr, err := c.compileExpr(q.Select)
		if err != nil {
			c.env = orig
			return "", err
		}
		var sortExpr, skipExpr, takeExpr string
		distinct := q.Distinct
		if q.Sort != nil {
			sortExpr, err = c.compileExpr(q.Sort)
			if err != nil {
				c.env = orig
				return "", err
			}
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
		c.env = orig
		c.needsDataset = true
		filterSrc := src
		if whereExpr != "" {
			filterSrc = fmt.Sprintf("(filter (lambda (%s) %s) %s)", sanitizeName(q.Var), whereExpr, src)
		}
		groupExpr := fmt.Sprintf("(_group_by %s (lambda (%s) %s))", filterSrc, sanitizeName(q.Var), keyExpr)
		var b strings.Builder
		b.WriteString("(let ([groups " + groupExpr + "])\n")
		b.WriteString("  (let ([_res '()])\n")
		b.WriteString(fmt.Sprintf("    (for ([%s groups])\n", sanitizeName(q.Group.Name)))
		if sortExpr != "" {
			b.WriteString(fmt.Sprintf("      (set! _res (append _res (list (cons %s %s))))\n", sortExpr, valExpr))
		} else {
			b.WriteString(fmt.Sprintf("      (set! _res (append _res (list %s)))\n", valExpr))
		}
		b.WriteString("    )\n")
		if sortExpr != "" {
			b.WriteString("    (set! _res (map cdr (sort _res (lambda (a b)\n" +
				"      (let ([ak (car a)] [bk (car b)])\n" +
				"        (cond [(and (number? ak) (number? bk)) (< ak bk)]\n" +
				"              [(and (string? ak) (string? bk)) (string<? ak bk)]\n" +
				"              [else (string<? (format \"~a\" ak) (format \"~a\" bk))])))\n" +
				"    )))\n")
		}
		if skipExpr != "" {
			b.WriteString(fmt.Sprintf("    (set! _res (drop _res %s))\n", skipExpr))
		}
		if takeExpr != "" {
			b.WriteString(fmt.Sprintf("    (set! _res (take _res %s))\n", takeExpr))
		}
		if distinct {
			b.WriteString("    (set! _res (_distinct _res))\n")
		}
		b.WriteString("    _res))")
		return b.String(), nil
	}

	if q.Group != nil && len(q.Froms) == 0 {
		src, err := c.compileExpr(q.Source)
		if err != nil {
			return "", err
		}
		orig := c.env
		child := types.NewEnv(c.env)
		child.SetVar(q.Var, types.AnyType{}, true)
		joinSrcs := make([]string, len(q.Joins))
		joinOns := make([]string, len(q.Joins))
		for _, j := range q.Joins {
			child.SetVar(j.Var, types.AnyType{}, true)
		}
		c.env = child
		for i, j := range q.Joins {
			js, err := c.compileExpr(j.Src)
			if err != nil {
				c.env = orig
				return "", err
			}
			if id, ok := bareIdent(j.Src); ok {
				if t, _ := c.env.GetVar(id); t != nil {
					if _, ok := t.(types.GroupType); ok {
						js = fmt.Sprintf("(_Group-Items %s)", sanitizeName(id))
					}
				}
			}
			joinSrcs[i] = js
			on, err := c.compileExpr(j.On)
			if err != nil {
				c.env = orig
				return "", err
			}
			joinOns[i] = on
		}
		var whereExpr string
		if q.Where != nil {
			whereExpr, err = c.compileExpr(q.Where)
			if err != nil {
				c.env = orig
				return "", err
			}
		}
		keyParts := make([]string, len(q.Group.Exprs))
		for i, e := range q.Group.Exprs {
			k, err := c.compileExpr(e)
			if err != nil {
				c.env = orig
				return "", err
			}
			keyParts[i] = k
		}
		keyExpr := strings.Join(keyParts, " ")
		if len(keyParts) > 1 {
			keyExpr = fmt.Sprintf("(list %s)", keyExpr)
		}
		genv := types.NewEnv(child)
		genv.SetVar(q.Group.Name, types.GroupType{Elem: types.AnyType{}}, true)
		c.env = genv
		valExpr, err := c.compileExpr(q.Select)
		if err != nil {
			c.env = orig
			return "", err
		}
		var sortExpr, skipExpr, takeExpr string
		distinct := q.Distinct
		if q.Sort != nil {
			sortExpr, err = c.compileExpr(q.Sort)
			if err != nil {
				c.env = orig
				return "", err
			}
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
		c.env = orig
		c.needsDataset = true
		var b strings.Builder
		b.WriteString("(let ([groups (let ([map (make-hash)] [order '()])\n")
		b.WriteString(fmt.Sprintf("  (for ([%s %s])\n", sanitizeName(q.Var), src))
		indent := "    "
		for i := range q.Joins {
			b.WriteString(indent + fmt.Sprintf("(for ([%s %s])\n", sanitizeName(q.Joins[i].Var), joinSrcs[i]))
			indent += "  "
			b.WriteString(indent + fmt.Sprintf("(when %s\n", joinOns[i]))
			indent += "  "
		}
		if whereExpr != "" {
			b.WriteString(indent + fmt.Sprintf("(when %s\n", whereExpr))
			indent += "  "
		}
		b.WriteString(indent + "(let* ([key " + keyExpr + "]\n")
		b.WriteString(indent + "       [ks (format \"~a\" key)]\n")
		b.WriteString(indent + "       [g (hash-ref map ks #f)])\n")
		b.WriteString(indent + "  (unless g\n")
		b.WriteString(indent + "    (set! g (_Group key '()))\n")
		b.WriteString(indent + "    (hash-set! map ks g)\n")
		b.WriteString(indent + "    (set! order (append order (list ks))))\n")
		b.WriteString(indent + fmt.Sprintf("  (set-_Group-Items! g (append (_Group-Items g) (list %s)))\n", sanitizeName(q.Var)))
		b.WriteString(indent + ")\n")
		if whereExpr != "" {
			indent = indent[:len(indent)-2]
			b.WriteString(indent + ")\n")
		}
		for range q.Joins {
			indent = indent[:len(indent)-2]
			b.WriteString(indent + ")\n")
			indent = indent[:len(indent)-2]
			b.WriteString(indent + ")\n")
		}
		b.WriteString("  )\n")
		b.WriteString("  (for/list ([ks order]) (hash-ref map ks)))])\n")
		b.WriteString("  (let ([_res '()])\n")
		b.WriteString(fmt.Sprintf("    (for ([%s groups])\n", sanitizeName(q.Group.Name)))
		if sortExpr != "" {
			b.WriteString(fmt.Sprintf("      (set! _res (append _res (list (cons %s %s))))\n", sortExpr, valExpr))
		} else {
			b.WriteString(fmt.Sprintf("      (set! _res (append _res (list %s)))\n", valExpr))
		}
		b.WriteString("    )\n")
		if sortExpr != "" {
			b.WriteString("    (set! _res (map cdr (sort _res (lambda (a b)\n" +
				"      (let ([ak (car a)] [bk (car b)])\n" +
				"        (cond [(and (number? ak) (number? bk)) (< ak bk)]\n" +
				"              [(and (string? ak) (string? bk)) (string<? ak bk)]\n" +
				"              [else (string<? (format \"~a\" ak) (format \"~a\" bk))])))\n" +
				"    )))\n")
		}
		if skipExpr != "" {
			b.WriteString(fmt.Sprintf("    (set! _res (drop _res %s))\n", skipExpr))
		}
		if takeExpr != "" {
			b.WriteString(fmt.Sprintf("    (set! _res (take _res %s))\n", takeExpr))
		}
		if distinct {
			b.WriteString("    (set! _res (_distinct _res))\n")
		}
		b.WriteString("    _res))")
		return b.String(), nil
	}

	if q.Group != nil {
		return "", fmt.Errorf("group clause not supported")
	}
	for _, j := range q.Joins {
		if j.Side != nil {
			return "", fmt.Errorf("join sides not supported")
		}
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
	for _, j := range q.Joins {
		child.SetVar(j.Var, types.AnyType{}, true)
	}
	c.env = child

	fromSrcs := make([]string, len(q.Froms))
	for i, f := range q.Froms {
		fs, err := c.compileExpr(f.Src)
		if err != nil {
			c.env = orig
			return "", err
		}
		if id, ok := bareIdent(f.Src); ok {
			if t, _ := c.env.GetVar(id); t != nil {
				if _, ok := t.(types.GroupType); ok {
					fs = fmt.Sprintf("(_Group-Items %s)", sanitizeName(id))
				}
			}
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
		on, err := c.compileExpr(j.On)
		if err != nil {
			c.env = orig
			return "", err
		}
		joinOns[i] = on
	}

	var whereExpr, sortExpr, skipExpr, takeExpr string
	distinct := q.Distinct
	if q.Where != nil {
		whereExpr, err = c.compileExpr(q.Where)
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

	val, err := c.compileExpr(q.Select)
	if err != nil {
		c.env = orig
		return "", err
	}
	c.env = orig

	var b strings.Builder
	b.WriteString("(let ([_res '()])\n")
	b.WriteString(fmt.Sprintf("  (for ([%s %s])\n", sanitizeName(q.Var), src))
	indent := "    "
	for i, f := range q.Froms {
		b.WriteString(indent + fmt.Sprintf("(for ([%s %s])\n", sanitizeName(f.Var), fromSrcs[i]))
		indent += "  "
	}
	for i, j := range q.Joins {
		b.WriteString(indent + fmt.Sprintf("(for ([%s %s])\n", sanitizeName(j.Var), joinSrcs[i]))
		indent += "  "
		b.WriteString(indent + fmt.Sprintf("(when %s\n", joinOns[i]))
		indent += "  "
	}
	if whereExpr != "" {
		b.WriteString(indent + fmt.Sprintf("(when %s\n", whereExpr))
		indent += "  "
	}
	if sortExpr != "" {
		b.WriteString(indent + fmt.Sprintf("(set! _res (append _res (list (cons %s %s))))\n", sortExpr, val))
	} else {
		b.WriteString(indent + fmt.Sprintf("(set! _res (append _res (list %s)))\n", val))
	}
	if whereExpr != "" {
		indent = indent[:len(indent)-2]
		b.WriteString(indent + ")\n")
	}
	for range q.Joins {
		indent = indent[:len(indent)-2]
		b.WriteString(indent + ")\n")
		indent = indent[:len(indent)-2]
		b.WriteString(indent + ")\n")
	}
	for range q.Froms {
		indent = indent[:len(indent)-2]
		b.WriteString(indent + ")\n")
	}
	b.WriteString("  )\n")
	if sortExpr != "" {
		b.WriteString("  (set! _res (map cdr (sort _res (lambda (a b)\n" +
			"    (let ([ak (car a)] [bk (car b)])\n" +
			"      (cond [(and (number? ak) (number? bk)) (< ak bk)]\n" +
			"            [(and (string? ak) (string? bk)) (string<? ak bk)]\n" +
			"            [else (string<? (format \"~a\" ak) (format \"~a\" bk))])))\n" +
			"  )))\n")
	}
	if skipExpr != "" {
		b.WriteString(fmt.Sprintf("  (set! _res (drop _res %s))\n", skipExpr))
	}
	if takeExpr != "" {
		b.WriteString(fmt.Sprintf("  (set! _res (take _res %s))\n", takeExpr))
	}
	if distinct {
		b.WriteString("  (set! _res (_distinct _res))\n")
	}
	b.WriteString("  _res)")
	return b.String(), nil
}

func (c *Compiler) compileFunExpr(f *parser.FunExpr) (string, error) {
	params := make([]string, len(f.Params))
	for i, p := range f.Params {
		params[i] = sanitizeName(p.Name)
	}
	if f.ExprBody != nil {
		body, err := c.compileExpr(f.ExprBody)
		if err != nil {
			return "", err
		}
		return fmt.Sprintf("(lambda (%s) %s)", strings.Join(params, " "), body), nil
	}
	// Block-bodied anonymous functions mirror compileFun but return a
	// Racket lambda form. We use a temporary compiler to generate the
	// body with proper indentation.
	sub := New(c.env)
	sub.indent = 2
	sub.writeln("(let/ec return")
	sub.indent++
	for _, st := range f.BlockBody {
		if err := sub.compileStmt(st); err != nil {
			return "", err
		}
	}
	sub.writeln("(return (void))")
	sub.indent--
	sub.writeln(")")
	code := sub.buf.String()

	var b strings.Builder
	b.WriteString("(lambda (" + strings.Join(params, " ") + ")\n")
	// indent the generated body by one tab to nest inside the lambda
	for _, line := range strings.Split(strings.TrimRight(code, "\n"), "\n") {
		b.WriteString("\t" + line + "\n")
	}
	b.WriteString(")")
	return b.String(), nil
}

func (c *Compiler) compileIfExpr(e *parser.IfExpr) (string, error) {
	cond, err := c.compileExpr(e.Cond)
	if err != nil {
		return "", err
	}
	thenExpr, err := c.compileExpr(e.Then)
	if err != nil {
		return "", err
	}
	var elseExpr string
	if e.ElseIf != nil {
		elseExpr, err = c.compileIfExpr(e.ElseIf)
		if err != nil {
			return "", err
		}
	} else if e.Else != nil {
		elseExpr, err = c.compileExpr(e.Else)
		if err != nil {
			return "", err
		}
	} else {
		elseExpr = "(void)"
	}
	return fmt.Sprintf("(if %s %s %s)", cond, thenExpr, elseExpr), nil
}
