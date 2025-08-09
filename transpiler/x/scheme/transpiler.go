//go:build slow

package scheme

import (
	"bufio"
	"bytes"
	"encoding/json"
	"fmt"
	"io"
	"math"
	"os"
	"os/exec"
	"path/filepath"
	"sort"
	"strings"
	"time"

	"mochi/parser"
	testpkg "mochi/runtime/ffi/go/testpkg"
	"mochi/types"
)

var currentEnv *types.Env

// tagKey is the hash-table key used to store the variant tag for union values.
const tagKey = "__tag"

var breakStack []Symbol
var continueStack []Node
var gensymCounter int
var needBase bool
var needHash bool
var usesInput bool
var usesNow bool
var usesLookupHost bool
var usesGetEnv bool
var usesEnviron bool
var usesJSON bool
var usesBench bool
var usesSubprocess bool
var usesFetch bool
var needMD5Hex bool
var needSHA256 bool
var benchMain bool
var returnStack []Symbol
var unionConsts map[string]int
var unionConstOrder []string
var methodNames map[string]struct{}
var currentMethodFields map[string]struct{}
var externFuncs map[string]struct{}

// SetBenchMain configures whether the generated main function is wrapped
// in a benchmark block when emitting code. When enabled, the program will
// output a JSON object with duration and memory statistics on completion.
func SetBenchMain(v bool) { benchMain = v }

func pushLoop(breakSym Symbol, cont Node) {
	breakStack = append(breakStack, breakSym)
	continueStack = append(continueStack, cont)
}

func pushReturn(retSym Symbol) {
	returnStack = append(returnStack, retSym)
}

func popReturn() {
	if len(returnStack) > 0 {
		returnStack = returnStack[:len(returnStack)-1]
	}
}

func currentReturn() Symbol { return returnStack[len(returnStack)-1] }

func popLoop() {
	if len(breakStack) > 0 {
		breakStack = breakStack[:len(breakStack)-1]
	}
	if len(continueStack) > 0 {
		continueStack = continueStack[:len(continueStack)-1]
	}
}

func currentBreak() Symbol  { return breakStack[len(breakStack)-1] }
func currentContinue() Node { return continueStack[len(continueStack)-1] }

func hasBreakOrContinue(stmts []*parser.Statement) bool {
	for _, st := range stmts {
		switch {
		case st.Break != nil || st.Continue != nil:
			return true
		case st.If != nil:
			if ifHasBreakOrContinue(st.If) {
				return true
			}
		case st.While != nil:
			if hasBreakOrContinue(st.While.Body) {
				return true
			}
		case st.For != nil:
			if hasBreakOrContinue(st.For.Body) {
				return true
			}
		case st.Bench != nil:
			if hasBreakOrContinue(st.Bench.Body) {
				return true
			}
		}
	}
	return false
}

func ifHasBreakOrContinue(is *parser.IfStmt) bool {
	if hasBreakOrContinue(is.Then) {
		return true
	}
	if is.ElseIf != nil && ifHasBreakOrContinue(is.ElseIf) {
		return true
	}
	if hasBreakOrContinue(is.Else) {
		return true
	}
	return false
}

func scanReturn(stmts []*parser.Statement, tail bool) (has, early bool) {
	for i, st := range stmts {
		tailPos := tail && i == len(stmts)-1
		switch {
		case st.Return != nil:
			has = true
			if !tailPos {
				early = true
			}
		case st.If != nil:
			h, e := scanIfReturn(st.If, tailPos)
			has = has || h
			early = early || e
		case st.While != nil:
			h, e := scanReturn(st.While.Body, false)
			has = has || h
			early = early || e
		case st.For != nil:
			h, e := scanReturn(st.For.Body, false)
			has = has || h
			early = early || e
		case st.Bench != nil:
			h, e := scanReturn(st.Bench.Body, false)
			has = has || h
			early = early || e
		}
	}
	return
}

func scanIfReturn(is *parser.IfStmt, tail bool) (has, early bool) {
	h, e := scanReturn(is.Then, tail)
	has = has || h
	early = early || e
	if is.ElseIf != nil {
		h2, e2 := scanIfReturn(is.ElseIf, tail)
		has = has || h2
		early = early || e2
	}
	if len(is.Else) > 0 {
		h3, e3 := scanReturn(is.Else, tail)
		has = has || h3
		early = early || e3
	}
	return
}

func hasEarlyReturn(stmts []*parser.Statement) bool {
	_, early := scanReturn(stmts, true)
	return early
}

func hasReturn(stmts []*parser.Statement) bool {
	has, _ := scanReturn(stmts, true)
	return has
}

func gensym(prefix string) Symbol {
	gensymCounter++
	return Symbol(fmt.Sprintf("%s%d", prefix, gensymCounter))
}

func ensureUnionConst(variant string) Symbol {
	if _, ok := unionConsts[variant]; !ok {
		unionConsts[variant] = len(unionConsts)
		unionConstOrder = append(unionConstOrder, variant)
	}
	return Symbol("OP_" + strings.ToUpper(variant))
}

// Node represents a Scheme AST node.
type Node interface{ Emit(io.Writer) }

type Symbol string

func (s Symbol) Emit(w io.Writer) { io.WriteString(w, string(s)) }

type StringLit string

func (s StringLit) Emit(w io.Writer) {
	io.WriteString(w, "\"")
	for _, r := range string(s) {
		switch r {
		case '\\':
			io.WriteString(w, "\\\\")
		case '"':
			io.WriteString(w, "\\\"")
		case '\n':
			io.WriteString(w, "\\n")
		case '\r':
			io.WriteString(w, "\\r")
		case '\t':
			io.WriteString(w, "\\t")
		default:
			if r < 32 || r == 127 {
				fmt.Fprintf(w, "\\x%02X;", r)
			} else {
				io.WriteString(w, string(r))
			}
		}
	}
	io.WriteString(w, "\"")
}

type IntLit int

func (i IntLit) Emit(w io.Writer) { fmt.Fprintf(w, "%d", int(i)) }

type FloatLit float64

func (f FloatLit) Emit(w io.Writer) {
	v := float64(f)
	if math.Trunc(v) == v {
		fmt.Fprintf(w, "%.1f", v)
	} else {
		fmt.Fprintf(w, "%g", v)
	}
}

type BoolLit bool

func (b BoolLit) Emit(w io.Writer) {
	if bool(b) {
		io.WriteString(w, "#t")
	} else {
		io.WriteString(w, "#f")
	}
}

type List struct{ Elems []Node }

func (l *List) Emit(w io.Writer) {
	io.WriteString(w, "(")
	for i, e := range l.Elems {
		if i > 0 {
			io.WriteString(w, " ")
		}
		if e != nil {
			e.Emit(w)
		}
	}
	io.WriteString(w, ")")
}

type Program struct{ Forms []Node }

func (p *Program) Emit(w io.Writer) {
	for i, f := range p.Forms {
		if f != nil {
			f.Emit(w)
		}
		if i < len(p.Forms)-1 {
			io.WriteString(w, "\n")
		}
	}
}

func EmitString(p *Program) []byte {
	var buf bytes.Buffer
	if p != nil {
		p.Emit(&buf)
	}
	return buf.Bytes()
}

func Format(src []byte) []byte {
	var out bytes.Buffer
	indent := 0
	inStr := false
	for i := 0; i < len(src); i++ {
		c := src[i]
		switch c {
		case '"':
			out.WriteByte(c)
			if i == 0 || src[i-1] != '\\' {
				inStr = !inStr
			}
		case '(':
			out.WriteByte(c)
			if !inStr {
				indent++
				out.WriteByte('\n')
				writeIndent(&out, indent)
			}
		case ')':
			if !inStr {
				indent--
				out.WriteByte('\n')
				writeIndent(&out, indent)
				out.WriteByte(')')
				if i+1 < len(src) && src[i+1] != ')' && src[i+1] != '\n' {
					out.WriteByte('\n')
					writeIndent(&out, indent)
				}
			} else {
				out.WriteByte(')')
			}
		case '\n':
			out.WriteByte('\n')
			if !inStr {
				writeIndent(&out, indent)
			}
		default:
			out.WriteByte(c)
		}
	}
	if out.Len() > 0 && out.Bytes()[out.Len()-1] != '\n' {
		out.WriteByte('\n')
	}
	return append(header(), out.Bytes()...)
}

func writeIndent(buf *bytes.Buffer, n int) {
	for i := 0; i < n; i++ {
		buf.WriteString("  ")
	}
}

func header() []byte {
	ts := time.Now().UTC()
	if out, err := exec.Command("git", "log", "-1", "--format=%cI").Output(); err == nil {
		if t, perr := time.Parse(time.RFC3339, strings.TrimSpace(string(out))); perr == nil {
			ts = t
		}
	}
	loc, _ := time.LoadLocation("Asia/Bangkok")
	prelude := ""
	prelude += "(import (scheme base))\n"
	prelude += "(import (scheme time))\n"
	// Always import string helpers for functions like string-contains
	prelude += "(import (chibi string))\n"
	prelude += "(import (only (scheme char) string-upcase string-downcase))\n"
	prelude += "(import (srfi 69))\n"
	prelude += "(import (srfi 1))\n"
	prelude += "(define _list list)\n"
	if needMD5Hex {
		prelude += "(import (chibi crypto md5))\n"
	}
	if needSHA256 {
		prelude += "(import (chibi crypto sha2))\n"
		prelude += `(define (_sha256 v)
  (let* ((bv (cond ((string? v) (string->utf8 v))
                   ((list? v) (apply bytevector v))
                   (else (error "sha256 expects string or list"))))
         (hex (sha-256 bv)))
    (let loop ((i 0) (out '()))
      (if (>= i (string-length hex))
          (reverse out)
          (let ((b (string->number (substring hex i (+ i 2)) 16)))
            (loop (+ i 2) (cons b out)))))))`
	}
	if usesInput {
		prelude += "(import (chibi io))\n"
	}
	if usesNow {
		prelude += "(import (chibi time) (srfi 98))\n"
		prelude += `(define _now_seeded #f)
(define _now_seed 0)
(define (now)
  (when (not _now_seeded)
    (let ((s (get-environment-variable "MOCHI_NOW_SEED")))
      (when (and s (string->number s))
        (set! _now_seed (string->number s))
        (set! _now_seeded #t))))
  (if _now_seeded
      (begin
        (set! _now_seed (modulo (+ (* _now_seed 1664525) 1013904223) 2147483647))
        _now_seed)
      (exact (floor (* (current-second) 1000000000))))
)
`
	}
	if usesBench {
		if !usesNow {
			prelude += "(import (chibi time))\n"
		}
		prelude += "(define (_mem) (* 1024 (resource-usage-max-rss (get-resource-usage resource-usage/self))))\n"
	}
	if usesLookupHost {
		prelude += "(import (chibi net) (scheme sort) (chibi string))\n"
		prelude += `(define (_lookup_host host)
  (with-exception-handler
   (lambda (e) (list '() e))
   (lambda ()
     (let loop ((a (get-address-info host "http")) (acc '()))
       (if a
           (let ((addr (sockaddr-name (address-info-address a))))
             (loop (address-info-next a)
                   (if (string-contains addr ":") acc (cons addr acc))))
          (list (list-sort string<? acc) '()))))))
`
	}
	if usesGetEnv || usesEnviron {
		prelude += "(import (srfi 98))\n"
	}
	if usesGetEnv {
		prelude += "(define (_getenv name) (let ((v (get-environment-variable name))) (if v v \"\")))\n"
	}
	if usesEnviron {
		prelude += "(define (_environ)\n  (map (lambda (p) (string-append (car p) \"=\" (cdr p)))\n       (get-environment-variables)))\n"
	}
	if usesSubprocess {
		prelude += "(import (chibi process))\n"
		prelude += "(define (subprocess.getoutput cmd) (process->string cmd))\n"
	}
	if usesJSON {
		prelude += "(import (chibi json))\n"
	}
	if usesFetch {
		prelude += "(define (_fetch url) (let ((d (string->json (process->string (string-append \"curl -s \" url))))) (if (and (list? d) (pair? d) (pair? (car d))) (alist->hash-table (map (lambda (p) (cons (symbol->string (car p)) (cdr p))) d)) d)))\n"
	}
	prelude += `(define (to-str x)
  (cond ((pair? x)
         (string-append "[" (string-join (map to-str x) ", ") "]"))
        ((hash-table? x)
         (let* ((ks (hash-table-keys x))
                (pairs (map (lambda (k)
                              (string-append (to-str k) ": " (to-str (hash-table-ref x k))))
                            ks)))
           (string-append "{" (string-join pairs ", ") "}")))
        ((null? x) "[]")
        ((string? x) (let ((out (open-output-string))) (json-write x out) (get-output-string out)))
        ((boolean? x) (if x "true" "false"))
        ((number? x)
         (if (integer? x)
             (number->string (inexact->exact x))
             (number->string x)))
        (else (number->string x))))
(define (to-str-space x)
  (cond ((pair? x)
         (string-append "[" (string-join (map to-str-space x) " ") "]"))
        ((string? x) x)
        (else (to-str x))))`
	prelude += "\n(define (upper s) (string-upcase s))"
	prelude += "\n(define (lower s) (string-downcase s))"
	prelude += "\n(define (_floor x) (cond ((string? x) (let ((n (string->number x))) (if n (floor n) 0))) ((boolean? x) (if x 1 0)) (else (floor x))))"
	prelude += "\n(define (fmod a b) (- a (* (_floor (/ a b)) b)))"
	prelude += "\n(define (_mod a b) (if (and (integer? a) (integer? b)) (modulo a b) (fmod a b)))"
	// Perform integer division when both operands are integers to mimic
	// Mochi's semantics. Fall back to Scheme's `/` for cases requiring
	// floating-point results.
	prelude += "\n(define (_div a b) (if (and (integer? a) (integer? b) (exact? a) (exact? b)) (quotient a b) (/ a b)))"
	prelude += "\n(define (_gt a b) (cond ((and (number? a) (number? b)) (> a b)) ((and (string? a) (string? b)) (string>? a b)) (else (> a b))))"
	prelude += "\n(define (_lt a b) (cond ((and (number? a) (number? b)) (< a b)) ((and (string? a) (string? b)) (string<? a b)) (else (< a b))))"
	prelude += "\n(define (_ge a b) (cond ((and (number? a) (number? b)) (>= a b)) ((and (string? a) (string? b)) (string>=? a b)) (else (>= a b))))"
	prelude += "\n(define (_le a b) (cond ((and (number? a) (number? b)) (<= a b)) ((and (string? a) (string? b)) (string<=? a b)) (else (<= a b))))"
	prelude += `
(define (_add a b)
  (cond ((and (number? a) (number? b)) (+ a b))
        ((string? a) (string-append a (to-str b)))
        ((string? b) (string-append (to-str a) b))
        ((and (list? a) (list? b)) (append a b))
        (else (+ a b))))`
	prelude += "\n(define (indexOf s sub)" +
		" (let ((cur (string-contains s sub)))" +
		"   (if cur (string-cursor->index s cur) -1)))"
	prelude += "\n(define (_display . args) (apply display args))"
	prelude += "\n(define (panic msg) (error msg))"
	prelude += `
(define (padStart s width pad)
  (let loop ((out s))
    (if (< (string-length out) width)
        (loop (string-append pad out))
        out)))`
	prelude += `
(define (_substring s start end)
  (let* ((len (string-length s))
         (s0 (max 0 (min len start)))
         (e0 (max s0 (min len end))))
    (substring s s0 e0)))`
	prelude += `
(define (_repeat s n)
  (let loop ((i 0) (out ""))
    (if (< i n)
        (loop (+ i 1) (string-append out s))
        out)))`
	prelude += `
(define (slice seq start end)
  (let* ((len (if (string? seq) (string-length seq) (length seq)))
         (s (if (< start 0) (+ len start) start))
         (e (if (< end 0) (+ len end) end)))
    (set! s (max 0 (min len s)))
    (set! e (max 0 (min len e)))
    (when (< e s) (set! e s))
    (if (string? seq)
        (_substring seq s e)
        (take (drop seq s) (- e s)))))`
	prelude += `
(define (_parseIntStr s base)
  (let* ((b (if (number? base) base 10))
         (n (string->number (if (list? s) (list->string s) s) b)))
    (if n (inexact->exact (truncate n)) 0)))`
	prelude += `
(define (_split s sep)
  (let* ((str (if (string? s) s (list->string s)))
         (del (cond ((char? sep) sep)
                     ((string? sep) (if (= (string-length sep) 1)
                                       (string-ref sep 0)
                                       sep))
                     (else sep))))
    (cond
     ((and (string? del) (string=? del ""))
      (map string (string->list str)))
     ((char? del)
      (string-split str del))
     (else
        (let loop ((r str) (acc '()))
          (let ((cur (string-contains r del)))
            (if cur
                (let ((idx (string-cursor->index r cur)))
                  (loop (_substring r (+ idx (string-length del)) (string-length r))
                        (cons (_substring r 0 idx) acc)))
                (reverse (cons r acc)))))))))`
	prelude += `
(define (_len x)
  (cond ((string? x) (string-length x))
        ((hash-table? x) (hash-table-size x))
        (else (length x))))`
	prelude += "\n(define (list-ref-safe lst idx) (if (and (integer? idx) (>= idx 0) (< idx (length lst))) (list-ref lst idx) '()))"
	if usesInput {
		prelude += "\n(define (_input)\n  (let ((l (read-line)))\n    (if (eof-object? l) \"\" l)))"
	}
	return []byte(fmt.Sprintf(";; Generated on %s\n%s\n",
		ts.In(loc).Format("2006-01-02 15:04 -0700"), prelude))
}

func voidSym() Node { return Symbol("'()") }

func typedDefault(t *parser.TypeRef) Node {
	if t == nil || t.Simple == nil {
		return voidSym()
	}
	switch *t.Simple {
	case "int":
		return IntLit(0)
	case "float":
		return FloatLit(0)
	case "bool":
		return BoolLit(false)
	case "string":
		return StringLit("")
	case "map":
		needHash = true
		return &List{Elems: []Node{Symbol("make-hash-table")}}
	default:
		return voidSym()
	}
}

func convertStmts(stmts []*parser.Statement) ([]Node, error) {
	if len(stmts) == 0 {
		return nil, nil
	}

	st := stmts[0]
	var name string
	var val Node
	var err error
	if st.Let != nil || st.Var != nil {
		if st.Let != nil {
			name = st.Let.Name
			if st.Let.Value != nil {
				val, err = convertParserExpr(st.Let.Value)
			} else if st.Let.Type != nil {
				val = typedDefault(st.Let.Type)
			} else {
				val = voidSym()
			}
			if currentEnv != nil {
				var t types.Type = types.AnyType{}
				if st.Let.Type != nil {
					t = types.ResolveTypeRef(st.Let.Type, currentEnv)
				} else if st.Let.Value != nil {
					t = types.ExprType(st.Let.Value, currentEnv)
				}
				currentEnv.SetVar(name, t, false)
			}
		} else {
			name = st.Var.Name
			if st.Var.Value != nil {
				val, err = convertParserExpr(st.Var.Value)
			} else if st.Var.Type != nil {
				val = typedDefault(st.Var.Type)
			} else {
				val = voidSym()
			}
			if currentEnv != nil {
				var t types.Type = types.AnyType{}
				if st.Var.Type != nil {
					t = types.ResolveTypeRef(st.Var.Type, currentEnv)
				} else if st.Var.Value != nil {
					t = types.ExprType(st.Var.Value, currentEnv)
				}
				currentEnv.SetVar(name, t, true)
			}
		}
		if err != nil {
			return nil, err
		}
		rest, err := convertStmts(stmts[1:])
		if err != nil {
			return nil, err
		}
		body := &List{Elems: append([]Node{Symbol("begin")}, rest...)}
		binding := &List{Elems: []Node{Symbol(name), val}}
		return []Node{&List{Elems: []Node{Symbol("let"), &List{Elems: []Node{binding}}, body}}}, nil
	}

	first, err := convertStmt(st)
	if err != nil {
		return nil, err
	}
	rest, err := convertStmts(stmts[1:])
	if err != nil {
		return nil, err
	}
	var forms []Node
	if first != nil {
		if lst, ok := first.(*List); ok && len(lst.Elems) > 0 && lst.Elems[0] == Symbol("begin") {
			forms = append(forms, lst.Elems[1:]...)
		} else {
			forms = append(forms, first)
		}
	}
	if st.Return != nil {
		return forms, nil
	}
	return append(forms, rest...), nil
}

func convertIfStmt(is *parser.IfStmt) (Node, error) {
	cond, err := convertParserExpr(is.Cond)
	if err != nil {
		return nil, err
	}
	thenForms, err := convertStmts(is.Then)
	if err != nil {
		return nil, err
	}
	thenNode := &List{Elems: append([]Node{Symbol("begin")}, thenForms...)}
	var elseNode Node = voidSym()
	if is.ElseIf != nil {
		elseNode, err = convertIfStmt(is.ElseIf)
		if err != nil {
			return nil, err
		}
	} else if len(is.Else) > 0 {
		elseForms, err := convertStmts(is.Else)
		if err != nil {
			return nil, err
		}
		elseNode = &List{Elems: append([]Node{Symbol("begin")}, elseForms...)}
	}
	return &List{Elems: []Node{Symbol("if"), cond, thenNode, elseNode}}, nil
}

func convertWhileStmt(ws *parser.WhileStmt) (Node, error) {
	needBase = true
	cond, err := convertParserExpr(ws.Cond)
	if err != nil {
		return nil, err
	}
	if !hasBreakOrContinue(ws.Body) {
		loopSym := gensym("loop")
		body, err := convertStmts(ws.Body)
		if err != nil {
			return nil, err
		}
		body = append(body, &List{Elems: []Node{loopSym}})
		bodyNode := &List{Elems: append([]Node{Symbol("begin")}, body...)}
		return &List{Elems: []Node{
			Symbol("letrec"),
			&List{Elems: []Node{
				&List{Elems: []Node{loopSym, &List{Elems: []Node{Symbol("lambda"), &List{Elems: []Node{}},
					&List{Elems: []Node{Symbol("if"), cond, bodyNode, voidSym()}}}}}},
			}},
			&List{Elems: []Node{loopSym}},
		}}, nil
	}
	loopSym := gensym("loop")
	breakSym := gensym("break")
	pushLoop(breakSym, &List{Elems: []Node{loopSym}})
	body, err := convertStmts(ws.Body)
	if err != nil {
		popLoop()
		return nil, err
	}
	loopCall := &List{Elems: []Node{loopSym}}
	body = append(body, loopCall)
	bodyNode := &List{Elems: append([]Node{Symbol("begin")}, body...)}
	popLoop()
	return &List{Elems: []Node{
		Symbol("call/cc"),
		&List{Elems: []Node{
			Symbol("lambda"), &List{Elems: []Node{breakSym}},
			&List{Elems: []Node{
				Symbol("letrec"),
				&List{Elems: []Node{
					&List{Elems: []Node{loopSym, &List{Elems: []Node{Symbol("lambda"), &List{Elems: []Node{}},
						&List{Elems: []Node{Symbol("if"), cond, bodyNode, voidSym()}}}}}},
				}},
				&List{Elems: []Node{loopSym}},
			}},
		}},
	}}, nil
}

func convertForStmt(fs *parser.ForStmt) (Node, error) {
	needBase = true
	prevEnv := currentEnv
	currentEnv = types.NewEnv(currentEnv)
	currentEnv.SetVar(fs.Name, types.AnyType{}, true)

	var iter Node
	isString := false
	var start Node
	var end Node
	var err error
	hasRange := fs.RangeEnd != nil
	if hasRange {
		start, err = convertParserExpr(fs.Source)
		if err != nil {
			currentEnv = prevEnv
			return nil, err
		}
		end, err = convertParserExpr(fs.RangeEnd)
		if err != nil {
			currentEnv = prevEnv
			return nil, err
		}
	} else {
		iter, err = convertParserExpr(fs.Source)
		if err != nil {
			currentEnv = prevEnv
			return nil, err
		}
		switch types.ExprType(fs.Source, prevEnv).(type) {
		case types.MapType:
			needHash = true
			iter = &List{Elems: []Node{Symbol("hash-table-keys"), iter}}
		case types.StringType:
			iter = &List{Elems: []Node{Symbol("string->list"), iter}}
			isString = true
		}
	}
	loopVar := Symbol("xs")
	loopSym := gensym("loop")
	breakSym := gensym("break")
	if hasRange {
		pushLoop(breakSym, &List{Elems: []Node{loopSym, &List{Elems: []Node{Symbol("+"), Symbol(fs.Name), IntLit(1)}}}})
	} else {
		pushLoop(breakSym, &List{Elems: []Node{loopSym, &List{Elems: []Node{Symbol("cdr"), loopVar}}}})
	}

	body, err := convertStmts(fs.Body)
	currentEnv = prevEnv
	if err != nil {
		popLoop()
		return nil, err
	}
	bodyNode := &List{Elems: append([]Node{Symbol("begin")}, body...)}

	var loopBody Node
	var loopInit Node
	param := loopVar
	if hasRange {
		loopBody = &List{Elems: []Node{
			Symbol("if"), &List{Elems: []Node{Symbol("<"), Symbol(fs.Name), end}},
			&List{Elems: []Node{
				Symbol("begin"),
				bodyNode,
				&List{Elems: []Node{loopSym, &List{Elems: []Node{Symbol("+"), Symbol(fs.Name), IntLit(1)}}}},
			}},
			voidSym(),
		}}
		loopInit = start
		param = Symbol(fs.Name)
	} else {
		loopBody = &List{Elems: []Node{
			Symbol("if"), &List{Elems: []Node{Symbol("null?"), loopVar}},
			voidSym(),
			&List{Elems: []Node{
				Symbol("begin"),
				&List{Elems: []Node{
					Symbol("let"),
					&List{Elems: []Node{&List{Elems: []Node{Symbol(fs.Name), func() Node {
						if isString {
							return &List{Elems: []Node{Symbol("string"), &List{Elems: []Node{Symbol("car"), loopVar}}}}
						}
						return &List{Elems: []Node{Symbol("car"), loopVar}}
					}()}}}},
					bodyNode,
				}},
				&List{Elems: []Node{loopSym, &List{Elems: []Node{Symbol("cdr"), loopVar}}}},
			}},
		}}
		loopInit = iter
	}

	loopFn := &List{Elems: []Node{
		Symbol("call/cc"),
		&List{Elems: []Node{
			Symbol("lambda"), &List{Elems: []Node{breakSym}},
			&List{Elems: []Node{
				Symbol("letrec"),
				&List{Elems: []Node{
					&List{Elems: []Node{loopSym, &List{Elems: []Node{Symbol("lambda"), &List{Elems: []Node{param}}, loopBody}}}},
				}},
				&List{Elems: []Node{loopSym, loopInit}},
			}},
		}},
	}}
	popLoop()
	return loopFn, nil
}

func convertFuncStmt(fs *parser.FunStmt, name string, withSelf bool) (Node, error) {
	needBase = true
	params := []Node{}
	prevEnv := currentEnv
	childEnv := types.NewEnv(currentEnv)
	childEnv.SetVar(name, types.AnyType{}, false)
	currentEnv = childEnv
	if withSelf {
		params = append(params, Symbol("self"))
		childEnv.SetVar("self", types.AnyType{}, false)
	}
	for _, p := range fs.Params {
		params = append(params, Symbol(p.Name))
		var pt types.Type = types.AnyType{}
		if p.Type != nil {
			pt = types.ResolveTypeRef(p.Type, prevEnv)
		}
		childEnv.SetVar(p.Name, pt, true)
	}
	_, earlyRet := scanReturn(fs.Body, true)
	var bodyForms []Node
	var err error
	if earlyRet {
		retSym := gensym("ret")
		pushReturn(retSym)
		bodyForms, err = convertStmts(fs.Body)
		popReturn()
		currentEnv = prevEnv
		if prevEnv != nil {
			prevEnv.SetVar(name, types.AnyType{}, false)
		}
		if err != nil {
			return nil, err
		}
		var body Node
		if len(bodyForms) == 1 {
			body = bodyForms[0]
		} else {
			body = &List{Elems: append([]Node{Symbol("begin")}, bodyForms...)}
		}
		wrapped := &List{Elems: []Node{
			Symbol("call/cc"),
			&List{Elems: []Node{
				Symbol("lambda"), &List{Elems: []Node{retSym}},
				body,
			}},
		}}
		return &List{Elems: []Node{
			Symbol("define"),
			&List{Elems: append([]Node{Symbol(name)}, params...)},
			wrapped,
		}}, nil
	}
	bodyForms, err = convertStmts(fs.Body)
	currentEnv = prevEnv
	if prevEnv != nil {
		prevEnv.SetVar(name, types.AnyType{}, false)
	}
	if err != nil {
		return nil, err
	}
	var body Node
	if len(bodyForms) == 1 {
		body = bodyForms[0]
	} else {
		body = &List{Elems: append([]Node{Symbol("begin")}, bodyForms...)}
	}
	return &List{Elems: []Node{
		Symbol("define"),
		&List{Elems: append([]Node{Symbol(name)}, params...)},
		body,
	}}, nil
}

func convertStmt(st *parser.Statement) (Node, error) {
	switch {
	case st.Expr != nil:
		call := st.Expr.Expr.Binary.Left.Value.Target.Call
		if call != nil && len(st.Expr.Expr.Binary.Right) == 0 {
			args := make([]Node, len(call.Args))
			for i, a := range call.Args {
				n, err := convertParserExpr(a)
				if err != nil {
					return nil, err
				}
				args[i] = n
			}
			switch call.Func {
			case "print":
				forms := []Node{Symbol("begin")}
				for i, a := range args {
					t := types.ExprType(call.Args[i], currentEnv)
					if _, ok := t.(types.BoolType); ok {
						a = &List{Elems: []Node{Symbol("if"), a, BoolLit(true), BoolLit(false)}}
					}
					disp := &List{Elems: []Node{Symbol("if"), &List{Elems: []Node{Symbol("string?"), a}}, a, &List{Elems: []Node{Symbol("to-str"), a}}}}
					forms = append(forms, &List{Elems: []Node{Symbol("_display"), disp}})
					if i < len(args)-1 {
						forms = append(forms, &List{Elems: []Node{Symbol("_display"), StringLit(" ")}})
					}
				}
				forms = append(forms, &List{Elems: []Node{Symbol("newline")}})
				return &List{Elems: forms}, nil
			case "json":
				if len(args) != 1 {
					return nil, fmt.Errorf("json expects 1 arg")
				}
				forms := []Node{Symbol("begin"),
					&List{Elems: []Node{Symbol("_display"), args[0]}},
					&List{Elems: []Node{Symbol("newline")}},
				}
				return &List{Elems: forms}, nil
			default:
				expr, err := convertCall(Symbol(call.Func), &parser.CallOp{Args: call.Args})
				if err != nil {
					return nil, err
				}
				return expr, nil
			}
		}
		expr, err := convertParserExpr(st.Expr.Expr)
		if err != nil {
			return nil, err
		}
		return expr, nil
	case st.Let != nil:
		name := st.Let.Name
		var val Node
		if st.Let.Value != nil {
			var err error
			val, err = convertParserExpr(st.Let.Value)
			if err != nil {
				return nil, err
			}
		} else if st.Let.Type != nil {
			val = typedDefault(st.Let.Type)
		} else {
			val = voidSym()
		}
		if currentEnv != nil {
			var t types.Type = types.AnyType{}
			if st.Let.Type != nil {
				t = types.ResolveTypeRef(st.Let.Type, currentEnv)
			} else if st.Let.Value != nil {
				t = types.ExprType(st.Let.Value, currentEnv)
			}
			currentEnv.SetVar(name, t, false)
		}
		return &List{Elems: []Node{Symbol("define"), Symbol(name), val}}, nil
	case st.Var != nil:
		name := st.Var.Name
		var val Node
		if st.Var.Value != nil {
			var err error
			val, err = convertParserExpr(st.Var.Value)
			if err != nil {
				return nil, err
			}
		} else if st.Var.Type != nil {
			val = typedDefault(st.Var.Type)
		} else {
			val = voidSym()
		}
		if currentEnv != nil {
			var t types.Type = types.AnyType{}
			if st.Var.Type != nil {
				t = types.ResolveTypeRef(st.Var.Type, currentEnv)
			} else if st.Var.Value != nil {
				t = types.ExprType(st.Var.Value, currentEnv)
			}
			currentEnv.SetVar(name, t, true)
		}
		return &List{Elems: []Node{Symbol("define"), Symbol(name), val}}, nil
	case st.Assign != nil:
		val, err := convertParserExpr(st.Assign.Value)
		if err != nil {
			return nil, err
		}
		// Handle simple variable assignment.
		if len(st.Assign.Index) == 0 && len(st.Assign.Field) == 0 {
			return &List{Elems: []Node{Symbol("set!"), Symbol(st.Assign.Name), val}}, nil
		}
		// Build a postfix expression from name/index/field for complex targets.
		pf := &parser.PostfixExpr{Target: &parser.Primary{Selector: &parser.SelectorExpr{Root: st.Assign.Name}}}
		for _, idx := range st.Assign.Index {
			pf.Ops = append(pf.Ops, &parser.PostfixOp{Index: idx})
		}
		for _, f := range st.Assign.Field {
			pf.Ops = append(pf.Ops, &parser.PostfixOp{Field: f})
		}
		var target Node
		var typ types.Type = types.AnyType{}
		if pf.Target.Selector != nil && len(pf.Target.Selector.Tail) == 0 {
			name := pf.Target.Selector.Root
			target = Symbol(name)
			if currentEnv != nil {
				if t, err := currentEnv.GetVar(name); err == nil {
					typ = t
				}
			}
		} else {
			target, err = convertParserPrimary(pf.Target)
			if err != nil {
				return nil, err
			}
			if currentEnv != nil {
				typ = types.ExprType(&parser.Expr{Binary: &parser.BinaryExpr{Left: &parser.Unary{Value: &parser.PostfixExpr{Target: pf.Target}}}}, currentEnv)
			}
		}
		for i := 0; i < len(pf.Ops)-1; i++ {
			op := pf.Ops[i]
			switch {
			case op.Index != nil:
				idxNode, err := convertParserExpr(op.Index.Start)
				if err != nil {
					return nil, err
				}
				switch t := typ.(type) {
				case types.ListType:
					target = &List{Elems: []Node{Symbol("list-ref-safe"), target, idxNode}}
					typ = t.Elem
				case types.MapType:
					needHash = true
					target = &List{Elems: []Node{Symbol("hash-table-ref"), target, idxNode}}
					typ = t.Value
				default:
					if _, ok := idxNode.(StringLit); ok {
						needHash = true
						target = &List{Elems: []Node{Symbol("hash-table-ref"), target, idxNode}}
					} else {
						target = &List{Elems: []Node{Symbol("list-ref-safe"), target, idxNode}}
					}
					typ = types.AnyType{}
				}
			case op.Field != nil:
				needHash = true
				target = &List{Elems: []Node{Symbol("hash-table-ref"), target, StringLit(op.Field.Name)}}
				typ = types.AnyType{}
			default:
				return nil, fmt.Errorf("unsupported assignment target")
			}
		}
		if len(pf.Ops) == 0 {
			return nil, fmt.Errorf("unsupported assignment target")
		}
		last := pf.Ops[len(pf.Ops)-1]
		if last.Index != nil {
			idxNode, err := convertParserExpr(last.Index.Start)
			if err != nil {
				return nil, err
			}
			switch typ.(type) {
			case types.MapType:
				needHash = true
				return &List{Elems: []Node{Symbol("hash-table-set!"), target, idxNode, val}}, nil
			case types.ListType:
				return &List{Elems: []Node{Symbol("list-set!"), target, idxNode, val}}, nil
			default:
				if _, ok := idxNode.(StringLit); ok {
					needHash = true
					return &List{Elems: []Node{Symbol("hash-table-set!"), target, idxNode, val}}, nil
				}
				return &List{Elems: []Node{Symbol("list-set!"), target, idxNode, val}}, nil
			}
		}
		if last.Field != nil {
			needHash = true
			return &List{Elems: []Node{Symbol("hash-table-set!"), target, StringLit(last.Field.Name), val}}, nil
		}
		return nil, fmt.Errorf("unsupported assignment target")
	case st.Fun != nil:
		return convertFuncStmt(st.Fun, st.Fun.Name, false)
	case st.Return != nil:
		var val Node = voidSym()
		if st.Return.Value != nil {
			var err error
			val, err = convertParserExpr(st.Return.Value)
			if err != nil {
				return nil, err
			}
		}
		if len(returnStack) == 0 {
			return val, nil
		}
		return &List{Elems: []Node{currentReturn(), val}}, nil
	case st.If != nil:
		return convertIfStmt(st.If)
	case st.While != nil:
		return convertWhileStmt(st.While)
	case st.For != nil:
		return convertForStmt(st.For)
	case st.Bench != nil:
		needBase = true
		usesNow = true
		usesJSON = true
		prevEnv := currentEnv
		currentEnv = types.NewEnv(currentEnv)
		bodyForms, err := convertStmts(st.Bench.Body)
		currentEnv = prevEnv
		if err != nil {
			return nil, err
		}
		startSym := gensym("start")
		endSym := gensym("end")
		durSym := gensym("dur")
		durStr := &List{Elems: []Node{Symbol("number->string"), Symbol(durSym)}}
		jsonStr := &List{Elems: []Node{Symbol("string-append"),
			StringLit("{\n  \"duration_us\": "), durStr,
			StringLit(",\n  \"memory_bytes\": 0,\n  \"name\": \"" + st.Bench.Name + "\"\n}"),
		}}
		jsonCall := &List{Elems: []Node{
			Symbol("begin"),
			&List{Elems: []Node{Symbol("_display"), jsonStr}},
			&List{Elems: []Node{Symbol("newline")}},
		}}
		innerLet2 := &List{Elems: []Node{
			Symbol("let"),
			&List{Elems: []Node{
				&List{Elems: []Node{Symbol(durSym), &List{Elems: []Node{Symbol("quotient"), &List{Elems: []Node{Symbol("-"), Symbol(endSym), Symbol(startSym)}}, IntLit(1000)}}}},
			}},
			jsonCall,
		}}
		innerLet1 := &List{Elems: []Node{
			Symbol("let"),
			&List{Elems: []Node{
				&List{Elems: []Node{Symbol(endSym), &List{Elems: []Node{Symbol("now")}}}},
			}},
			innerLet2,
		}}
		inner := append(bodyForms, innerLet1)
		return &List{Elems: []Node{
			Symbol("let"),
			&List{Elems: []Node{&List{Elems: []Node{Symbol(startSym), &List{Elems: []Node{Symbol("now")}}}}}},
			&List{Elems: append([]Node{Symbol("begin")}, inner...)},
		}}, nil
	case st.Test != nil:
		// Ignore test blocks during transpilation
		return nil, nil
	case st.Type != nil:
		fields := map[string]struct{}{}
		for _, m := range st.Type.Members {
			if m.Field != nil {
				fields[m.Field.Name] = struct{}{}
			}
		}
		var defs []Node
		for _, m := range st.Type.Members {
			if m.Method != nil {
				methodNames[m.Method.Name] = struct{}{}
				currentMethodFields = fields
				fn, err := convertFuncStmt(m.Method, m.Method.Name, true)
				currentMethodFields = nil
				if err != nil {
					return nil, err
				}
				defs = append(defs, fn)
			}
		}
		if len(defs) == 0 {
			return nil, nil
		}
		return &List{Elems: append([]Node{Symbol("begin")}, defs...)}, nil
	case st.Import != nil:
		if st.Import.Auto && st.Import.Path == "mochi/runtime/ffi/go/testpkg" {
			alias := st.Import.As
			if alias == "" {
				alias = "testpkg"
			}
			needHash = true
			needMD5Hex = true
			obj := Symbol(alias)
			defs := []Node{
				&List{Elems: []Node{Symbol("define"), obj, &List{Elems: []Node{Symbol("make-hash-table")}}}},
				&List{Elems: []Node{
					Symbol("hash-table-set!"),
					obj,
					StringLit("MD5Hex"),
					&List{Elems: []Node{
						Symbol("lambda"),
						&List{Elems: []Node{Symbol("s")}},
						&List{Elems: []Node{Symbol("md5"), Symbol("s")}},
					}},
				}},
			}
			return &List{Elems: append([]Node{Symbol("begin")}, defs...)}, nil
		}
		// other imports have no effect in the Scheme transpiler
		return nil, nil
	case st.ExternFun != nil:
		name := st.ExternFun.Root
		if len(st.ExternFun.Tail) > 0 {
			name += "." + strings.Join(st.ExternFun.Tail, ".")
		}
		externFuncs[name] = struct{}{}
		if st.ExternFun.Root == "subprocess" && len(st.ExternFun.Tail) == 1 && st.ExternFun.Tail[0] == "getoutput" {
			usesSubprocess = true
		}
		return nil, nil
	case st.Break != nil:
		if len(breakStack) == 0 {
			return nil, fmt.Errorf("break outside loop")
		}
		return &List{Elems: []Node{currentBreak(), voidSym()}}, nil
	case st.Continue != nil:
		if len(continueStack) == 0 {
			return nil, fmt.Errorf("continue outside loop")
		}
		return currentContinue(), nil
	default:
		return nil, fmt.Errorf("unsupported statement")
	}
}

// Transpile converts a Mochi AST into a minimal Scheme AST supporting
// print statements with string literals.
func Transpile(prog *parser.Program, env *types.Env) (*Program, error) {
	currentEnv = env
	// Reset per-run state to ensure previous transpilation artifacts do not
	// leak into subsequent runs when the transpiler is reused. This mirrors
	// the fresh environment used by the tests for each Rosetta example.
	breakStack = nil
	continueStack = nil
	returnStack = nil
	gensymCounter = 0

	needBase = false
	needHash = false
	usesInput = false
	usesNow = false
	usesJSON = false
	usesLookupHost = false
	usesGetEnv = false
	usesEnviron = false
	usesBench = false
	usesSubprocess = false
	usesFetch = false
	needMD5Hex = false
	unionConsts = map[string]int{}
	unionConstOrder = nil
	methodNames = map[string]struct{}{}
	currentMethodFields = nil
	externFuncs = map[string]struct{}{}
	forms, err := convertStmts(prog.Statements)
	if err != nil {
		return nil, err
	}
	p := &Program{Forms: forms}
	if len(unionConstOrder) > 0 {
		defs := make([]Node, len(unionConstOrder))
		for i, v := range unionConstOrder {
			constSym := Symbol("OP_" + strings.ToUpper(v))
			defs[i] = &List{Elems: []Node{Symbol("define"), constSym, IntLit(unionConsts[v])}}
		}
		p.Forms = append(defs, p.Forms...)
	}
	if benchMain {
		needBase = true
		usesJSON = true
		usesBench = true
		startSym := gensym("start")
		endSym := gensym("end")
		durSym := gensym("dur")
		jpsSym := gensym("jps")
		durCalc := &List{Elems: []Node{
			Symbol("quotient"),
			&List{Elems: []Node{
				Symbol("*"),
				&List{Elems: []Node{Symbol("-"), Symbol(endSym), Symbol(startSym)}},
				IntLit(1000000),
			}},
			Symbol(jpsSym),
		}}
		durStr := &List{Elems: []Node{Symbol("number->string"), Symbol(durSym)}}
		memStr := &List{Elems: []Node{Symbol("number->string"), &List{Elems: []Node{Symbol("_mem")}}}}
		jsonStr := &List{Elems: []Node{Symbol("string-append"),
			StringLit("{\n  \"duration_us\": "), durStr,
			StringLit(",\n  \"memory_bytes\": "), memStr,
			StringLit(",\n  \"name\": \"main\"\n}"),
		}}
		jsonCall := &List{Elems: []Node{
			Symbol("begin"),
			&List{Elems: []Node{Symbol("_display"), jsonStr}},
			&List{Elems: []Node{Symbol("newline")}},
		}}
		innerLet2 := &List{Elems: []Node{
			Symbol("let"),
			&List{Elems: []Node{
				&List{Elems: []Node{Symbol(durSym), durCalc}},
			}},
			jsonCall,
		}}
		innerLet1 := &List{Elems: []Node{
			Symbol("let"),
			&List{Elems: []Node{
				&List{Elems: []Node{Symbol(endSym), &List{Elems: []Node{Symbol("current-jiffy")}}}},
			}},
			innerLet2,
		}}
		inner := append(p.Forms, innerLet1)
		p.Forms = []Node{&List{Elems: []Node{
			Symbol("let"),
			&List{Elems: []Node{
				&List{Elems: []Node{Symbol(startSym), &List{Elems: []Node{Symbol("current-jiffy")}}}},
				&List{Elems: []Node{Symbol(jpsSym), &List{Elems: []Node{Symbol("jiffies-per-second")}}}},
			}},
			&List{Elems: append([]Node{Symbol("begin")}, inner...)},
		}}}
	}
	return p, nil
}

func convertParserExpr(e *parser.Expr) (Node, error) {
	if e == nil || e.Binary == nil {
		return nil, fmt.Errorf("unsupported expression")
	}

	left, err := convertParserUnary(e.Binary.Left)
	if err != nil {
		return nil, err
	}
	leftType := types.ExprType(&parser.Expr{Binary: &parser.BinaryExpr{Left: e.Binary.Left}}, currentEnv)

	exprs := []Node{left}
	typesStack := []types.Type{leftType}
	ops := []string{}

	for _, part := range e.Binary.Right {
		right, err := convertParserPostfix(part.Right)
		if err != nil {
			return nil, err
		}
		rightType := types.ExprType(&parser.Expr{Binary: &parser.BinaryExpr{Left: &parser.Unary{Value: part.Right}}}, currentEnv)

		for len(ops) > 0 && precedence(ops[len(ops)-1]) >= precedence(part.Op) {
			r := exprs[len(exprs)-1]
			exprs = exprs[:len(exprs)-1]
			rt := typesStack[len(typesStack)-1]
			typesStack = typesStack[:len(typesStack)-1]

			l := exprs[len(exprs)-1]
			exprs = exprs[:len(exprs)-1]
			lt := typesStack[len(typesStack)-1]
			typesStack = typesStack[:len(typesStack)-1]

			o := ops[len(ops)-1]
			ops = ops[:len(ops)-1]

			exprs = append(exprs, makeBinaryTyped(o, l, r, lt, rt))
			typesStack = append(typesStack, binaryResultType(o, lt, rt))
		}

		op := part.Op
		if part.Op == "union" && part.All {
			op = "union_all"
		}
		ops = append(ops, op)
		exprs = append(exprs, right)
		typesStack = append(typesStack, rightType)
	}

	for len(ops) > 0 {
		r := exprs[len(exprs)-1]
		exprs = exprs[:len(exprs)-1]
		rt := typesStack[len(typesStack)-1]
		typesStack = typesStack[:len(typesStack)-1]

		l := exprs[len(exprs)-1]
		exprs = exprs[:len(exprs)-1]
		lt := typesStack[len(typesStack)-1]
		typesStack = typesStack[:len(typesStack)-1]

		o := ops[len(ops)-1]
		ops = ops[:len(ops)-1]

		exprs = append(exprs, makeBinaryTyped(o, l, r, lt, rt))
		typesStack = append(typesStack, binaryResultType(o, lt, rt))
	}

	if len(exprs) != 1 {
		return nil, fmt.Errorf("expr reduce error")
	}

	return exprs[0], nil
}

func convertParserUnary(u *parser.Unary) (Node, error) {
	if u == nil {
		return nil, fmt.Errorf("unsupported unary")
	}
	expr, err := convertParserPostfix(u.Value)
	if err != nil {
		return nil, err
	}
	for i := len(u.Ops) - 1; i >= 0; i-- {
		op := u.Ops[i]
		switch op {
		case "-":
			expr = &List{Elems: []Node{Symbol("-"), expr}}
		case "!":
			expr = &List{Elems: []Node{Symbol("not"), expr}}
		default:
			return nil, fmt.Errorf("unsupported unary op")
		}
	}
	return expr, nil
}

func convertParserPostfix(pf *parser.PostfixExpr) (Node, error) {
	if pf == nil {
		return nil, fmt.Errorf("unsupported postfix")
	}
	var node Node
	var err error
	// handle net.LookupHost call
	if pf.Target.Selector != nil && len(pf.Target.Selector.Tail) == 1 && pf.Target.Selector.Tail[0] == "LookupHost" && len(pf.Ops) > 0 && pf.Ops[0].Call != nil {
		if pf.Target.Selector.Root == "net" {
			call := pf.Ops[0].Call
			if len(call.Args) != 1 {
				return nil, fmt.Errorf("LookupHost expects 1 arg")
			}
			arg, err := convertParserExpr(call.Args[0])
			if err != nil {
				return nil, err
			}
			usesLookupHost = true
			return &List{Elems: []Node{Symbol("_lookup_host"), arg}}, nil
		}
	}
	// handle os.Getenv call
	if pf.Target.Selector != nil && len(pf.Target.Selector.Tail) == 1 && pf.Target.Selector.Tail[0] == "Getenv" && len(pf.Ops) > 0 && pf.Ops[0].Call != nil {
		if pf.Target.Selector.Root == "os" {
			call := pf.Ops[0].Call
			if len(call.Args) != 1 {
				return nil, fmt.Errorf("Getenv expects 1 arg")
			}
			arg, err := convertParserExpr(call.Args[0])
			if err != nil {
				return nil, err
			}
			usesGetEnv = true
			return &List{Elems: []Node{Symbol("_getenv"), arg}}, nil
		}
	}
	// handle os.Environ call
	if pf.Target.Selector != nil && len(pf.Target.Selector.Tail) == 1 && pf.Target.Selector.Tail[0] == "Environ" && len(pf.Ops) > 0 && pf.Ops[0].Call != nil {
		if pf.Target.Selector.Root == "os" {
			call := pf.Ops[0].Call
			if len(call.Args) != 0 {
				return nil, fmt.Errorf("Environ expects no args")
			}
			usesEnviron = true
			return &List{Elems: []Node{Symbol("_environ")}}, nil
		}
	}
	if pf.Target.Selector != nil && len(pf.Target.Selector.Tail) == 0 && len(pf.Ops) > 0 && pf.Ops[0].Field != nil {
		name := pf.Target.Selector.Root + "." + pf.Ops[0].Field.Name
		if _, ok := externFuncs[name]; ok {
			node = Symbol(name)
			pf.Ops = pf.Ops[1:]
		}
	}
	// handle Object.keys(map)
	if pf.Target.Selector != nil && len(pf.Target.Selector.Tail) == 1 && pf.Target.Selector.Root == "Object" && pf.Target.Selector.Tail[0] == "keys" && len(pf.Ops) > 0 && pf.Ops[0].Call != nil {
		call := pf.Ops[0].Call
		if len(call.Args) != 1 {
			return nil, fmt.Errorf("keys expects 1 arg")
		}
		arg, err := convertParserExpr(call.Args[0])
		if err != nil {
			return nil, err
		}
		needHash = true
		return &List{Elems: []Node{Symbol("hash-table-keys"), arg}}, nil
	}
	// handle selector like `x.contains()` parsed as part of target
	if pf.Target.Selector != nil && len(pf.Target.Selector.Tail) == 1 && pf.Target.Selector.Tail[0] == "get" && len(pf.Ops) > 0 && pf.Ops[0].Call != nil {
		rootPrim := &parser.Primary{Selector: &parser.SelectorExpr{Root: pf.Target.Selector.Root}}
		node, err = convertParserPrimary(rootPrim)
		if err != nil {
			return nil, err
		}
		call := pf.Ops[0].Call
		if len(call.Args) != 1 && len(call.Args) != 2 {
			return nil, fmt.Errorf("get expects 1 or 2 args")
		}
		keyArg, err := convertParserExpr(call.Args[0])
		if err != nil {
			return nil, err
		}
		var defArg Node = voidSym()
		if len(call.Args) == 2 {
			defArg, err = convertParserExpr(call.Args[1])
			if err != nil {
				return nil, err
			}
		}
		needHash = true
		node = &List{Elems: []Node{Symbol("hash-table-ref/default"), node, keyArg, defArg}}
		pf = &parser.PostfixExpr{Target: rootPrim, Ops: pf.Ops[1:]}
	} else if pf.Target.Selector != nil && len(pf.Target.Selector.Tail) == 1 && pf.Target.Selector.Tail[0] == "contains" && len(pf.Ops) > 0 && pf.Ops[0].Call != nil {
		rootPrim := &parser.Primary{Selector: &parser.SelectorExpr{Root: pf.Target.Selector.Root}}
		node, err = convertParserPrimary(rootPrim)
		if err != nil {
			return nil, err
		}
		call := pf.Ops[0].Call
		if len(call.Args) != 1 {
			return nil, fmt.Errorf("contains expects 1 arg")
		}
		arg, err := convertParserExpr(call.Args[0])
		if err != nil {
			return nil, err
		}
		node = &List{Elems: []Node{Symbol("if"), &List{Elems: []Node{Symbol("string-contains"), node, arg}}, BoolLit(true), BoolLit(false)}}
		pf = &parser.PostfixExpr{Target: rootPrim, Ops: pf.Ops[1:]}
	} else if pf.Target.Selector != nil && len(pf.Target.Selector.Tail) == 1 && pf.Target.Selector.Tail[0] == "keys" && len(pf.Ops) > 0 && pf.Ops[0].Call != nil {
		rootPrim := &parser.Primary{Selector: &parser.SelectorExpr{Root: pf.Target.Selector.Root}}
		node, err = convertParserPrimary(rootPrim)
		if err != nil {
			return nil, err
		}
		call := pf.Ops[0].Call
		if len(call.Args) != 0 {
			return nil, fmt.Errorf("keys expects no args")
		}
		needHash = true
		node = &List{Elems: []Node{Symbol("hash-table-keys"), node}}
		pf = &parser.PostfixExpr{Target: rootPrim, Ops: pf.Ops[1:]}
	} else if pf.Target.Selector != nil && pf.Target.Selector.Root == "testpkg" && len(pf.Target.Selector.Tail) == 1 {
		node, err = convertParserPrimary(pf.Target)
		if err != nil {
			return nil, err
		}
	} else if pf.Target.Selector != nil && len(pf.Target.Selector.Tail) == 1 && len(pf.Ops) > 0 && pf.Ops[0].Call != nil {
		rootPrim := &parser.Primary{Selector: &parser.SelectorExpr{Root: pf.Target.Selector.Root}}
		call := pf.Ops[0].Call
		full := pf.Target.Selector.Root + "." + pf.Target.Selector.Tail[0]
		if _, ok := externFuncs[full]; ok {
			args := make([]Node, len(call.Args))
			for j, a := range call.Args {
				n, err := convertParserExpr(a)
				if err != nil {
					return nil, err
				}
				args[j] = n
			}
			node = &List{Elems: append([]Node{Symbol(full)}, args...)}
		} else {
			objNode, err := convertParserPrimary(rootPrim)
			if err != nil {
				return nil, err
			}
			args := make([]Node, len(call.Args))
			for j, a := range call.Args {
				n, err := convertParserExpr(a)
				if err != nil {
					return nil, err
				}
				args[j] = n
			}
			name := pf.Target.Selector.Tail[0]
			if _, ok := currentEnv.GetFunc(name); ok {
				args = append([]Node{objNode}, args...)
				node = &List{Elems: append([]Node{Symbol(name)}, args...)}
			} else if _, ok := methodNames[name]; ok {
				args = append([]Node{objNode}, args...)
				node = &List{Elems: append([]Node{Symbol(name)}, args...)}
			} else {
				needHash = true
				fn := &List{Elems: []Node{Symbol("hash-table-ref"), objNode, StringLit(name)}}
				node = &List{Elems: append([]Node{fn}, args...)}
			}
		}
		pf = &parser.PostfixExpr{Target: rootPrim, Ops: pf.Ops[1:]}
	} else if node == nil {
		node, err = convertParserPrimary(pf.Target)
		if err != nil {
			return nil, err
		}
	}
	for i := 0; i < len(pf.Ops); i++ {
		op := pf.Ops[i]
		switch {
		case op.Call != nil:
			var err error
			node, err = convertCall(node, op.Call)
			if err != nil {
				return nil, err
			}
		case op.Index != nil:
			var err error
			var orig *parser.Primary
			if i == 0 {
				orig = pf.Target
			}
			node, err = convertIndex(node, orig, op.Index)
			if err != nil {
				return nil, err
			}
		case op.Cast != nil:
			t := op.Cast.Type
			if t.Simple != nil && *t.Simple == "int" {
				x := gensym("v")
				cond := &List{Elems: []Node{
					Symbol("cond"),
					&List{Elems: []Node{&List{Elems: []Node{Symbol("string?"), Symbol(x)}},
						&List{Elems: []Node{Symbol("inexact->exact"),
							&List{Elems: []Node{Symbol("_floor"),
								&List{Elems: []Node{Symbol("string->number"), Symbol(x)}}}}}}}},
					&List{Elems: []Node{&List{Elems: []Node{Symbol("boolean?"), Symbol(x)}},
						&List{Elems: []Node{Symbol("if"), Symbol(x), IntLit(1), IntLit(0)}}}},
					&List{Elems: []Node{Symbol("else"),
						&List{Elems: []Node{Symbol("inexact->exact"),
							&List{Elems: []Node{Symbol("_floor"), Symbol(x)}}}}}},
				}}
				node = &List{Elems: []Node{
					Symbol("let"),
					&List{Elems: []Node{&List{Elems: []Node{Symbol(x), node}}}},
					cond,
				}}
			} else if t.Simple != nil && *t.Simple == "string" {
				node = &List{Elems: []Node{Symbol("to-str"), node}}
			} else if t.Simple != nil && *t.Simple == "float" {
				node = &List{Elems: []Node{Symbol("+"), FloatLit(0), node}}
			} // ignore other casts
		case op.Field != nil && op.Field.Name == "contains" && i+1 < len(pf.Ops) && pf.Ops[i+1].Call != nil:
			call := pf.Ops[i+1].Call
			if len(call.Args) != 1 {
				return nil, fmt.Errorf("contains expects 1 arg")
			}
			arg, err := convertParserExpr(call.Args[0])
			if err != nil {
				return nil, err
			}
			if i == 0 && types.IsStringPrimary(pf.Target, currentEnv) {
				node = &List{Elems: []Node{Symbol("string-contains"), node, arg}}
			} else {
				node = &List{Elems: []Node{
					Symbol("if"),
					&List{Elems: []Node{Symbol("string?"), node}},
					&List{Elems: []Node{Symbol("string-contains"), node, arg}},
					&List{Elems: []Node{Symbol("if"), &List{Elems: []Node{Symbol("member"), arg, node}}, BoolLit(true), BoolLit(false)}},
				}}
			}
			i++
		case op.Field != nil && op.Field.Name == "keys" && i+1 < len(pf.Ops) && pf.Ops[i+1].Call != nil:
			call := pf.Ops[i+1].Call
			if len(call.Args) != 0 {
				return nil, fmt.Errorf("keys expects no args")
			}
			needHash = true
			node = &List{Elems: []Node{Symbol("hash-table-keys"), node}}
			i++
		case op.Field != nil && op.Field.Name == "get" && i+1 < len(pf.Ops) && pf.Ops[i+1].Call != nil:
			call := pf.Ops[i+1].Call
			if len(call.Args) != 1 && len(call.Args) != 2 {
				return nil, fmt.Errorf("get expects 1 or 2 args")
			}
			keyArg, err := convertParserExpr(call.Args[0])
			if err != nil {
				return nil, err
			}
			var defArg Node = voidSym()
			if len(call.Args) == 2 {
				defArg, err = convertParserExpr(call.Args[1])
				if err != nil {
					return nil, err
				}
			}
			needHash = true
			node = &List{Elems: []Node{Symbol("hash-table-ref/default"), node, keyArg, defArg}}
			i++
		case op.Field != nil && op.Field.Name == "padStart" && i+1 < len(pf.Ops) && pf.Ops[i+1].Call != nil:
			call := pf.Ops[i+1].Call
			if len(call.Args) != 2 {
				return nil, fmt.Errorf("padStart expects 2 args")
			}
			widthArg, err := convertParserExpr(call.Args[0])
			if err != nil {
				return nil, err
			}
			padArg, err := convertParserExpr(call.Args[1])
			if err != nil {
				return nil, err
			}
			node = &List{Elems: []Node{Symbol("padStart"), &List{Elems: []Node{Symbol("to-str"), node}}, widthArg, padArg}}
			i++
		case op.Field != nil && i+1 < len(pf.Ops) && pf.Ops[i+1].Call != nil:
			call := pf.Ops[i+1].Call
			args := make([]Node, len(call.Args))
			for j, a := range call.Args {
				n, err := convertParserExpr(a)
				if err != nil {
					return nil, err
				}
				args[j] = n
			}
			name := op.Field.Name
			if _, ok := currentEnv.GetFunc(name); ok {
				args = append([]Node{node}, args...)
				node = &List{Elems: append([]Node{Symbol(name)}, args...)}
			} else if _, ok := methodNames[name]; ok {
				args = append([]Node{node}, args...)
				node = &List{Elems: append([]Node{Symbol(name)}, args...)}
			} else {
				needHash = true
				fn := &List{Elems: []Node{Symbol("hash-table-ref"), node, StringLit(name)}}
				node = &List{Elems: append([]Node{fn}, args...)}
			}
			i++
		case op.Field != nil:
			needHash = true
			node = &List{Elems: []Node{Symbol("hash-table-ref"), node, StringLit(op.Field.Name)}}
		default:
			return nil, fmt.Errorf("unsupported postfix")
		}
	}
	return node, nil
}

func convertParserPrimary(p *parser.Primary) (Node, error) {
	switch {
	case p.Lit != nil && p.Lit.Int != nil:
		return IntLit(int(*p.Lit.Int)), nil
	case p.Lit != nil && p.Lit.Float != nil:
		return FloatLit(*p.Lit.Float), nil
	case p.Lit != nil && p.Lit.Str != nil:
		return StringLit(*p.Lit.Str), nil
	case p.Lit != nil && p.Lit.Bool != nil:
		return BoolLit(bool(*p.Lit.Bool)), nil
	case p.Lit != nil && p.Lit.Null:
		return voidSym(), nil
	case p.Selector != nil && len(p.Selector.Tail) == 0:
		if p.Selector.Root == "nil" {
			return voidSym(), nil
		}
		if currentEnv != nil {
			if _, err := currentEnv.GetVar(p.Selector.Root); err == nil {
				return Symbol(p.Selector.Root), nil
			}
			if _, ok := currentEnv.GetFunc(p.Selector.Root); ok {
				return Symbol(p.Selector.Root), nil
			}
			if _, ok := currentMethodFields[p.Selector.Root]; ok {
				needHash = true
				return &List{Elems: []Node{Symbol("hash-table-ref"), Symbol("self"), StringLit(p.Selector.Root)}}, nil
			}
		}
		return StringLit(p.Selector.Root), nil
	case p.Selector != nil:
		full := p.Selector.Root
		if len(p.Selector.Tail) > 0 {
			full += "." + strings.Join(p.Selector.Tail, ".")
		}
		if _, ok := externFuncs[full]; ok {
			return Symbol(full), nil
		}
		if p.Selector.Root == "testpkg" && len(p.Selector.Tail) == 1 {
			switch p.Selector.Tail[0] {
			case "FifteenPuzzleExample":
				return Symbol("testpkg.FifteenPuzzleExample"), nil
			case "ECDSAExample":
				return Symbol("testpkg.ECDSAExample"), nil
			}
		}
		var node Node = Symbol(p.Selector.Root)
		needHash = true
		for _, f := range p.Selector.Tail {
			node = &List{Elems: []Node{Symbol("hash-table-ref"), node, StringLit(f)}}
		}
		return node, nil
	case p.If != nil:
		return convertIfExpr(p.If)
	case p.Group != nil:
		return convertParserExpr(p.Group)
	case p.List != nil:
		needBase = true
		elems := []Node{}
		for _, e := range p.List.Elems {
			n, err := convertParserExpr(e)
			if err != nil {
				return nil, err
			}
			elems = append(elems, n)
		}
		return &List{Elems: append([]Node{Symbol("_list")}, elems...)}, nil
	case p.Map != nil:
		needBase = true
		needHash = true
		pairs := []Node{Symbol("_list")}
		for _, it := range p.Map.Items {
			var k Node
			if s, ok := types.SimpleStringKey(it.Key); ok {
				k = StringLit(s)
			} else {
				var err error
				k, err = convertParserExpr(it.Key)
				if err != nil {
					return nil, err
				}
			}
			v, err := convertParserExpr(it.Value)
			if err != nil {
				return nil, err
			}
			pair := &List{Elems: []Node{Symbol("cons"), k, v}}
			pairs = append(pairs, pair)
		}
		return &List{Elems: []Node{Symbol("alist->hash-table"), &List{Elems: pairs}}}, nil
	case p.Struct != nil:
		if ut, ok := currentEnv.FindUnionByVariant(p.Struct.Name); ok {
			needHash = true
			st := ut.Variants[p.Struct.Name]
			if len(p.Struct.Fields) != len(st.Order) {
				return nil, fmt.Errorf("invalid fields for %s", p.Struct.Name)
			}
			pairs := []Node{Symbol("_list"),
				&List{Elems: []Node{Symbol("cons"), StringLit(tagKey), ensureUnionConst(p.Struct.Name)}},
			}
			for i, f := range p.Struct.Fields {
				v, err := convertParserExpr(f.Value)
				if err != nil {
					return nil, err
				}
				pair := &List{Elems: []Node{Symbol("cons"), StringLit(st.Order[i]), v}}
				pairs = append(pairs, pair)
			}
			return &List{Elems: []Node{Symbol("alist->hash-table"), &List{Elems: pairs}}}, nil
		}
		needHash = true
		pairs := []Node{Symbol("_list")}
		for _, f := range p.Struct.Fields {
			v, err := convertParserExpr(f.Value)
			if err != nil {
				return nil, err
			}
			pair := &List{Elems: []Node{Symbol("cons"), StringLit(f.Name), v}}
			pairs = append(pairs, pair)
		}
		return &List{Elems: []Node{Symbol("alist->hash-table"), &List{Elems: pairs}}}, nil
	case p.Fetch != nil:
		url, err := convertParserExpr(p.Fetch.URL)
		if err != nil {
			return nil, err
		}
		usesJSON = true
		usesSubprocess = true
		usesFetch = true
		return &List{Elems: []Node{Symbol("_fetch"), url}}, nil
	case p.Query != nil:
		if n, err := convertGroupByJoinQuery(p.Query); err == nil {
			return n, nil
		}
		if n, err := convertGroupByQuery(p.Query); err == nil {
			return n, nil
		}
		if n, err := convertRightJoinQuery(p.Query); err == nil {
			return n, nil
		}
		return convertQueryExpr(p.Query)
	case p.Match != nil:
		return convertMatchExpr(p.Match)
	case p.FunExpr != nil:
		return convertFunExpr(p.FunExpr)
	case p.Load != nil:
		format := parseFormat(p.Load.With)
		path := ""
		if p.Load.Path != nil {
			path = strings.Trim(*p.Load.Path, "\"")
		}
		n, err := dataExprFromFile(path, format, p.Load.Type)
		if err != nil {
			return nil, err
		}
		return n, nil
	case p.Call != nil:
		return convertCall(Symbol(p.Call.Func), &parser.CallOp{Args: p.Call.Args})
	}
	return nil, fmt.Errorf("unsupported primary")
}

func convertIfExpr(ie *parser.IfExpr) (Node, error) {
	cond, err := convertParserExpr(ie.Cond)
	if err != nil {
		return nil, err
	}
	thenNode, err := convertParserExpr(ie.Then)
	if err != nil {
		return nil, err
	}
	var elseNode Node = voidSym()
	if ie.ElseIf != nil {
		elseNode, err = convertIfExpr(ie.ElseIf)
		if err != nil {
			return nil, err
		}
	} else if ie.Else != nil {
		elseNode, err = convertParserExpr(ie.Else)
		if err != nil {
			return nil, err
		}
	}
	return &List{Elems: []Node{Symbol("if"), cond, thenNode, elseNode}}, nil
}

func convertMatchExpr(me *parser.MatchExpr) (Node, error) {
	target, err := convertParserExpr(me.Target)
	if err != nil {
		return nil, err
	}
	temp := gensym("match")
	var expr Node = voidSym()
	for i := len(me.Cases) - 1; i >= 0; i-- {
		c := me.Cases[i]
		if isUnderscore(c.Pattern) {
			n, err := convertParserExpr(c.Result)
			if err != nil {
				return nil, err
			}
			expr = n
			continue
		}
		if variant, vars, fields, ok := extractVariantPattern(c.Pattern); ok {
			prevEnv := currentEnv
			var st types.StructType
			if len(vars) > 0 {
				child := types.NewEnv(currentEnv)
				ut, _ := currentEnv.FindUnionByVariant(variant)
				st = ut.Variants[variant]
				for j, nm := range vars {
					if nm == "_" {
						continue
					}
					child.SetVar(nm, st.Fields[fields[j]], true)
				}
				currentEnv = child
			}
			body, err := convertParserExpr(c.Result)
			if len(vars) > 0 {
				currentEnv = prevEnv
			}
			if err != nil {
				return nil, err
			}
			then := body
			if len(vars) > 0 {
				bindings := []Node{}
				for j, nm := range vars {
					if nm == "_" {
						continue
					}
					val := &List{Elems: []Node{Symbol("hash-table-ref"), temp, StringLit(fields[j])}}
					bindings = append(bindings, &List{Elems: []Node{Symbol(nm), val}})
				}
				if len(bindings) > 0 {
					then = &List{Elems: []Node{Symbol("let"), &List{Elems: bindings}, body}}
				}
			}
			var cond Node
			cond = &List{Elems: []Node{Symbol("equal?"),
				&List{Elems: []Node{Symbol("hash-table-ref"), temp, StringLit(tagKey)}},
				ensureUnionConst(variant),
			}}
			expr = &List{Elems: []Node{Symbol("if"), cond, then, expr}}
			continue
		}
		pat, err := convertParserExpr(c.Pattern)
		if err != nil {
			return nil, err
		}
		res, err := convertParserExpr(c.Result)
		if err != nil {
			return nil, err
		}
		cond := &List{Elems: []Node{Symbol("equal?"), temp, pat}}
		expr = &List{Elems: []Node{Symbol("if"), cond, res, expr}}
	}
	return &List{Elems: []Node{
		Symbol("let"),
		&List{Elems: []Node{&List{Elems: []Node{temp, target}}}},
		expr,
	}}, nil
}

func isUnderscore(e *parser.Expr) bool {
	if e == nil || e.Binary == nil || e.Binary.Left == nil || e.Binary.Left.Value == nil {
		return false
	}
	t := e.Binary.Left.Value.Target
	return t != nil && t.Selector != nil && t.Selector.Root == "_" && len(t.Selector.Tail) == 0
}

func isSimpleIdent(e *parser.Expr) (string, bool) {
	if e == nil || e.Binary == nil || len(e.Binary.Right) > 0 {
		return "", false
	}
	u := e.Binary.Left
	if len(u.Ops) > 0 || u.Value == nil {
		return "", false
	}
	p := u.Value
	if len(p.Ops) > 0 || p.Target == nil {
		return "", false
	}
	if p.Target.Selector != nil && len(p.Target.Selector.Tail) == 0 {
		return p.Target.Selector.Root, true
	}
	return "", false
}

func extractVariantPattern(e *parser.Expr) (string, []string, []string, bool) {
	if e == nil || e.Binary == nil || e.Binary.Left == nil || e.Binary.Left.Value == nil {
		return "", nil, nil, false
	}
	pf := e.Binary.Left.Value
	prim := pf.Target
	if prim != nil && prim.Call != nil && len(pf.Ops) == 0 {
		if ut, ok := currentEnv.FindUnionByVariant(prim.Call.Func); ok {
			st := ut.Variants[prim.Call.Func]
			if len(prim.Call.Args) != len(st.Order) {
				return "", nil, nil, false
			}
			vars := make([]string, len(prim.Call.Args))
			for i, a := range prim.Call.Args {
				if name, ok2 := isSimpleIdent(a); ok2 {
					vars[i] = name
				} else {
					return "", nil, nil, false
				}
			}
			return prim.Call.Func, vars, st.Order, true
		}
	} else if prim != nil && prim.Selector != nil && len(prim.Selector.Tail) == 0 && len(pf.Ops) == 0 {
		if ut, ok := currentEnv.FindUnionByVariant(prim.Selector.Root); ok {
			st := ut.Variants[prim.Selector.Root]
			return prim.Selector.Root, nil, st.Order, true
		}
	}
	return "", nil, nil, false
}

func literalString(e *parser.Expr) (string, bool) {
	if e == nil || e.Binary == nil || len(e.Binary.Right) > 0 {
		return "", false
	}
	u := e.Binary.Left
	if len(u.Ops) > 0 || u.Value == nil {
		return "", false
	}
	p := u.Value
	if len(p.Ops) > 0 || p.Target == nil {
		return "", false
	}
	if p.Target.Lit != nil && p.Target.Lit.Str != nil {
		return *p.Target.Lit.Str, true
	}
	if p.Target.Selector != nil && len(p.Target.Selector.Tail) == 0 {
		return p.Target.Selector.Root, true
	}
	return "", false
}

func parseFormat(e *parser.Expr) string {
	if e == nil || e.Binary == nil || len(e.Binary.Right) > 0 {
		return ""
	}
	u := e.Binary.Left
	if len(u.Ops) > 0 || u.Value == nil {
		return ""
	}
	p := u.Value
	if len(p.Ops) > 0 || p.Target == nil || p.Target.Map == nil {
		return ""
	}
	for _, it := range p.Target.Map.Items {
		key, ok := isSimpleIdent(it.Key)
		if !ok {
			key, ok = literalString(it.Key)
		}
		if key == "format" {
			if s, ok := literalString(it.Value); ok {
				return s
			}
		}
	}
	return ""
}

func valueToNode(v interface{}, typ *parser.TypeRef) Node {
	switch val := v.(type) {
	case map[string]interface{}:
		names := make([]string, 0, len(val))
		for k := range val {
			names = append(names, k)
		}
		sort.Strings(names)
		pairs := []Node{Symbol("_list")}
		for _, k := range names {
			pair := &List{Elems: []Node{Symbol("cons"), StringLit(k), valueToNode(val[k], nil)}}
			pairs = append(pairs, pair)
		}
		return &List{Elems: []Node{Symbol("alist->hash-table"), &List{Elems: pairs}}}
	case []interface{}:
		elems := make([]Node, 0, len(val)+1)
		elems = append(elems, Symbol("_list"))
		for _, it := range val {
			elems = append(elems, valueToNode(it, nil))
		}
		return &List{Elems: elems}
	case string:
		return StringLit(val)
	case bool:
		return BoolLit(val)
	case float64:
		if float64(int(val)) == val {
			return IntLit(int(val))
		}
		return FloatLit(val)
	case int:
		return IntLit(val)
	case int64:
		return IntLit(int(val))
	case nil:
		return StringLit("")
	default:
		return StringLit(fmt.Sprintf("%v", val))
	}
}

func dataExprFromFile(path, format string, typ *parser.TypeRef) (Node, error) {
	if path == "" {
		return &List{Elems: []Node{Symbol("_list")}}, nil
	}
	root := repoRoot()
	if root != "" {
		if strings.HasPrefix(path, "../") {
			clean := strings.TrimPrefix(path, "../")
			path = filepath.Join(root, "tests", clean)
		} else if !filepath.IsAbs(path) {
			path = filepath.Join(root, path)
		}
	}
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	var v interface{}
	switch format {
	case "jsonl":
		var list []interface{}
		scanner := bufio.NewScanner(bytes.NewReader(data))
		for scanner.Scan() {
			line := strings.TrimSpace(scanner.Text())
			if line == "" {
				continue
			}
			var item interface{}
			if err := json.Unmarshal([]byte(line), &item); err != nil {
				return nil, err
			}
			list = append(list, item)
		}
		if err := scanner.Err(); err != nil {
			return nil, err
		}
		v = list
	case "json":
		if err := json.Unmarshal(data, &v); err != nil {
			return nil, err
		}
	case "yaml", "csv", "":
		return nil, fmt.Errorf("unsupported load format")
	default:
		return nil, fmt.Errorf("unsupported load format")
	}
	return valueToNode(v, typ), nil
}

func repoRoot() string {
	out, err := exec.Command("git", "rev-parse", "--show-toplevel").CombinedOutput()
	if err != nil {
		return ""
	}
	return strings.TrimSpace(string(out))
}

func convertFunExpr(fe *parser.FunExpr) (Node, error) {
	params := []Node{}
	prevEnv := currentEnv
	currentEnv = types.NewEnv(currentEnv)
	for _, p := range fe.Params {
		params = append(params, Symbol(p.Name))
		currentEnv.SetVar(p.Name, types.AnyType{}, true)
	}
	var body Node
	if fe.ExprBody != nil {
		b, err := convertParserExpr(fe.ExprBody)
		if err != nil {
			currentEnv = prevEnv
			return nil, err
		}
		body = b
	} else {
		retSym := gensym("ret")
		pushReturn(retSym)
		stmts, err := convertStmts(fe.BlockBody)
		popReturn()
		if err != nil {
			currentEnv = prevEnv
			return nil, err
		}
		if len(stmts) == 1 {
			body = stmts[0]
		} else {
			body = &List{Elems: append([]Node{Symbol("begin")}, stmts...)}
		}
		body = &List{Elems: []Node{
			Symbol("call/cc"),
			&List{Elems: []Node{
				Symbol("lambda"), &List{Elems: []Node{retSym}},
				body,
			}},
		}}
	}
	currentEnv = prevEnv
	return &List{Elems: []Node{Symbol("lambda"), &List{Elems: params}, body}}, nil
}

func convertRightJoinQuery(q *parser.QueryExpr) (Node, error) {
	if q == nil || len(q.Joins) != 1 || q.Distinct || q.Group != nil || q.Sort != nil || q.Skip != nil || q.Take != nil || q.Where != nil || len(q.Froms) > 0 {
		return nil, fmt.Errorf("unsupported query")
	}
	j := q.Joins[0]
	if j.Side == nil || *j.Side != "right" {
		return nil, fmt.Errorf("unsupported query")
	}
	leftSrc, err := convertParserExpr(q.Source)
	if err != nil {
		return nil, err
	}
	rightSrc, err := convertParserExpr(j.Src)
	if err != nil {
		return nil, err
	}
	prevEnv := currentEnv
	child := types.NewEnv(currentEnv)
	child.SetVar(q.Var, types.AnyType{}, true)
	child.SetVar(j.Var, types.AnyType{}, true)
	currentEnv = child
	cond, err := convertParserExpr(j.On)
	if err != nil {
		currentEnv = prevEnv
		return nil, err
	}
	sel, err := convertParserExpr(q.Select)
	currentEnv = prevEnv
	if err != nil {
		return nil, err
	}
	resSym := gensym("res")
	matchedSym := gensym("matched")
	appendNode := &List{Elems: []Node{
		Symbol("set!"), resSym,
		&List{Elems: []Node{Symbol("append"), resSym, &List{Elems: []Node{Symbol("_list"), sel}}}},
	}}
	innerBody := &List{Elems: []Node{
		Symbol("if"), cond,
		&List{Elems: []Node{
			Symbol("begin"),
			&List{Elems: []Node{Symbol("set!"), matchedSym, BoolLit(true)}},
			appendNode,
		}},
		voidSym(),
	}}
	innerLoop := &List{Elems: []Node{
		Symbol("for-each"),
		&List{Elems: []Node{Symbol("lambda"), &List{Elems: []Node{Symbol(q.Var)}}, innerBody}},
		leftSrc,
	}}
	notMatched := &List{Elems: []Node{Symbol("not"), matchedSym}}
	unmatchedBody := &List{Elems: []Node{
		Symbol("let"), &List{Elems: []Node{&List{Elems: []Node{Symbol(q.Var), voidSym()}}}},
		appendNode,
	}}
	afterLoop := &List{Elems: []Node{Symbol("if"), notMatched, unmatchedBody, voidSym()}}
	outerBody := &List{Elems: []Node{
		Symbol("let"), &List{Elems: []Node{&List{Elems: []Node{matchedSym, BoolLit(false)}}}},
		&List{Elems: []Node{Symbol("begin"), innerLoop, afterLoop}},
	}}
	outerLoop := &List{Elems: []Node{
		Symbol("for-each"),
		&List{Elems: []Node{Symbol("lambda"), &List{Elems: []Node{Symbol(j.Var)}}, outerBody}},
		rightSrc,
	}}
	return &List{Elems: []Node{
		Symbol("let"),
		&List{Elems: []Node{&List{Elems: []Node{resSym, &List{Elems: []Node{Symbol("_list")}}}}}},
		&List{Elems: []Node{Symbol("begin"), outerLoop, resSym}},
	}}, nil
}

func convertGroupByJoinQuery(q *parser.QueryExpr) (Node, error) {
	if q == nil || q.Group == nil || len(q.Group.Exprs) != 1 || q.Distinct || q.Skip != nil || q.Take != nil {
		return nil, fmt.Errorf("unsupported query")
	}

	prevEnv := currentEnv
	env := types.NewEnv(currentEnv)

	srcType := types.ExprType(q.Source, currentEnv)
	var elem types.Type = types.AnyType{}
	if lt, ok := srcType.(types.ListType); ok {
		elem = lt.Elem
	} else if gt, ok := srcType.(types.GroupType); ok {
		elem = gt.Elem
	}
	env.SetVar(q.Var, elem, true)
	for _, f := range q.Froms {
		ft := types.ExprType(f.Src, currentEnv)
		var fe types.Type = types.AnyType{}
		if lt, ok := ft.(types.ListType); ok {
			fe = lt.Elem
		}
		env.SetVar(f.Var, fe, true)
	}
	for _, j := range q.Joins {
		jt := types.ExprType(j.Src, currentEnv)
		var je types.Type = types.AnyType{}
		if lt, ok := jt.(types.ListType); ok {
			je = lt.Elem
		}
		env.SetVar(j.Var, je, true)
	}
	currentEnv = env

	key, err := convertParserExpr(q.Group.Exprs[0])
	if err != nil {
		currentEnv = prevEnv
		return nil, err
	}

	env.SetVar(q.Group.Name, types.GroupType{Key: types.AnyType{}, Elem: elem}, true)
	sel, err := convertParserExpr(q.Select)
	if err != nil {
		currentEnv = prevEnv
		return nil, err
	}
	sel = mapToAlist(sel)
	var sortExpr Node
	if q.Sort != nil {
		sortExpr, err = convertParserExpr(q.Sort)
		if err != nil {
			currentEnv = prevEnv
			return nil, err
		}
	}

	loops := []struct {
		name   string
		source Node
	}{}
	src, err := convertParserExpr(q.Source)
	if err != nil {
		currentEnv = prevEnv
		return nil, err
	}
	if _, ok := srcType.(types.GroupType); ok {
		src = &List{Elems: []Node{Symbol("hash-table-ref"), src, StringLit("items")}}
	}
	loops = append(loops, struct {
		name   string
		source Node
	}{q.Var, src})
	for _, j := range q.Joins {
		src, err := convertParserExpr(j.Src)
		if err != nil {
			currentEnv = prevEnv
			return nil, err
		}
		if _, ok := types.ExprType(j.Src, currentEnv).(types.GroupType); ok {
			src = &List{Elems: []Node{Symbol("hash-table-ref"), src, StringLit("items")}}
		}
		loops = append(loops, struct {
			name   string
			source Node
		}{j.Var, src})
	}
	for _, f := range q.Froms {
		src, err := convertParserExpr(f.Src)
		if err != nil {
			currentEnv = prevEnv
			return nil, err
		}
		if _, ok := types.ExprType(f.Src, currentEnv).(types.GroupType); ok {
			src = &List{Elems: []Node{Symbol("hash-table-ref"), src, StringLit("items")}}
		}
		loops = append(loops, struct {
			name   string
			source Node
		}{f.Var, src})
	}

	var cond Node
	if q.Where != nil {
		cond, err = convertParserExpr(q.Where)
		if err != nil {
			currentEnv = prevEnv
			return nil, err
		}
	}
	for _, j := range q.Joins {
		jc, err := convertParserExpr(j.On)
		if err != nil {
			currentEnv = prevEnv
			return nil, err
		}
		if cond == nil {
			cond = jc
		} else {
			cond = makeBinary("&&", cond, jc)
		}
	}

	groups := gensym("groups")
	g := gensym("g")
	gParam := Symbol(q.Group.Name)
	k := gensym("k")
	res := gensym("res")

	lookup := &List{Elems: []Node{Symbol("hash-table-ref/default"), Symbol(groups), Symbol(k), Symbol("#f")}}
	newGrp := &List{Elems: []Node{
		Symbol("begin"),
		&List{Elems: []Node{
			Symbol("set!"), Symbol(g),
			&List{Elems: []Node{
				Symbol("alist->hash-table"),
				&List{Elems: []Node{
					Symbol("_list"),
					&List{Elems: []Node{Symbol("cons"), StringLit("key"), Symbol(k)}},
					&List{Elems: []Node{Symbol("cons"), StringLit("items"), &List{Elems: []Node{Symbol("_list")}}}},
				}},
			}},
		}},
		&List{Elems: []Node{Symbol("hash-table-set!"), Symbol(groups), Symbol(k), Symbol(g)}},
	}}

	var item Node
	if len(loops) == 1 {
		item = Symbol(loops[0].name)
	} else {
		itemPairs := []Node{Symbol("_list")}
		for _, l := range loops {
			itemPairs = append(itemPairs, &List{Elems: []Node{Symbol("cons"), StringLit(l.name), Symbol(l.name)}})
		}
		item = &List{Elems: []Node{Symbol("alist->hash-table"), &List{Elems: itemPairs}}}
	}

	appendItem := &List{Elems: []Node{
		Symbol("hash-table-set!"), Symbol(g), StringLit("items"),
		&List{Elems: []Node{
			Symbol("append"),
			&List{Elems: []Node{Symbol("hash-table-ref"), Symbol(g), StringLit("items")}},
			&List{Elems: []Node{Symbol("_list"), item}},
		}},
	}}

	body := &List{Elems: []Node{
		Symbol("let*"),
		&List{Elems: []Node{
			&List{Elems: []Node{Symbol(k), key}},
			&List{Elems: []Node{Symbol(g), lookup}},
		}},
		&List{Elems: []Node{
			Symbol("begin"),
			&List{Elems: []Node{Symbol("if"), &List{Elems: []Node{Symbol("not"), Symbol(g)}}, newGrp, voidSym()}},
			appendItem,
		}},
	}}

	if cond != nil {
		body = &List{Elems: []Node{Symbol("if"), cond, body, voidSym()}}
	}

	loopBody := body
	for i := len(loops) - 1; i >= 0; i-- {
		l := loops[i]
		loopBody = &List{Elems: []Node{
			Symbol("for-each"),
			&List{Elems: []Node{Symbol("lambda"), &List{Elems: []Node{Symbol(l.name)}}, loopBody}},
			l.source,
		}}
	}
	buildRes := &List{Elems: []Node{
		Symbol("for-each"),
		&List{Elems: []Node{
			Symbol("lambda"),
			&List{Elems: []Node{gParam}},
			&List{Elems: []Node{
				Symbol("set!"), Symbol(res),
				&List{Elems: []Node{Symbol("append"), Symbol(res), &List{Elems: []Node{Symbol("_list"), sel}}}},
			}},
		}},
		&List{Elems: []Node{Symbol("hash-table-values"), Symbol(groups)}},
	}}

	var sortCall Node
	if sortExpr != nil {
		a := gensym("a")
		b := gensym("b")
		keyA := replaceSymbol(sortExpr, gParam, a)
		keyB := replaceSymbol(sortExpr, gParam, b)
		cmp := &List{Elems: []Node{
			Symbol("lambda"),
			&List{Elems: []Node{a, b}},
			&List{Elems: []Node{Symbol("<"), keyA, keyB}},
		}}
		sortCall = &List{Elems: []Node{
			Symbol("set!"), Symbol(res),
			&List{Elems: []Node{Symbol("list-sort"), cmp, Symbol(res)}},
		}}
	}

	currentEnv = prevEnv
	return &List{Elems: []Node{
		Symbol("let"),
		&List{Elems: []Node{
			&List{Elems: []Node{Symbol(groups), &List{Elems: []Node{Symbol("make-hash-table")}}}},
			&List{Elems: []Node{Symbol(res), &List{Elems: []Node{Symbol("_list")}}}},
		}},
		&List{Elems: []Node{Symbol("begin"), loopBody, buildRes, sortCall, Symbol(res)}},
	}}, nil
}

func convertGroupByQuery(q *parser.QueryExpr) (Node, error) {
	if q == nil || q.Group == nil || len(q.Group.Exprs) != 1 || len(q.Froms) > 0 || len(q.Joins) > 0 || q.Distinct || q.Sort != nil || q.Skip != nil || q.Take != nil || q.Where != nil {
		return nil, fmt.Errorf("unsupported query")
	}

	src, err := convertParserExpr(q.Source)
	if err != nil {
		return nil, err
	}

	prevEnv := currentEnv
	env := types.NewEnv(currentEnv)
	srcType := types.ExprType(q.Source, currentEnv)
	var elem types.Type = types.AnyType{}
	if lt, ok := srcType.(types.ListType); ok {
		elem = lt.Elem
	}
	env.SetVar(q.Var, elem, true)
	currentEnv = env
	key, err := convertParserExpr(q.Group.Exprs[0])
	if err != nil {
		currentEnv = prevEnv
		return nil, err
	}
	env.SetVar(q.Group.Name, types.GroupType{Key: types.AnyType{}, Elem: elem}, true)
	sel, err := convertParserExpr(q.Select)
	currentEnv = prevEnv
	if err != nil {
		return nil, err
	}

	groups := gensym("groups")
	g := gensym("g")
	gParam := Symbol(q.Group.Name)
	k := gensym("k")
	res := gensym("res")

	lookup := &List{Elems: []Node{Symbol("hash-table-ref/default"), Symbol(groups), Symbol(k), Symbol("#f")}}
	newGrp := &List{Elems: []Node{
		Symbol("begin"),
		&List{Elems: []Node{
			Symbol("set!"), Symbol(g),
			&List{Elems: []Node{
				Symbol("alist->hash-table"),
				&List{Elems: []Node{
					Symbol("_list"),
					&List{Elems: []Node{Symbol("cons"), StringLit("key"), Symbol(k)}},
					&List{Elems: []Node{Symbol("cons"), StringLit("items"), &List{Elems: []Node{Symbol("_list")}}}},
				}},
			}},
		}},
		&List{Elems: []Node{Symbol("hash-table-set!"), Symbol(groups), Symbol(k), Symbol(g)}},
	}}

	appendItem := &List{Elems: []Node{
		Symbol("hash-table-set!"), Symbol(g), StringLit("items"),
		&List{Elems: []Node{
			Symbol("append"),
			&List{Elems: []Node{Symbol("hash-table-ref"), Symbol(g), StringLit("items")}},
			&List{Elems: []Node{Symbol("_list"), Symbol(q.Var)}},
		}},
	}}

	body := &List{Elems: []Node{
		Symbol("let*"),
		&List{Elems: []Node{
			&List{Elems: []Node{Symbol(k), key}},
			&List{Elems: []Node{Symbol(g), lookup}},
		}},
		&List{Elems: []Node{
			Symbol("begin"),
			&List{Elems: []Node{Symbol("if"), &List{Elems: []Node{Symbol("not"), Symbol(g)}}, newGrp, voidSym()}},
			appendItem,
		}},
	}}

	loop := &List{Elems: []Node{
		Symbol("for-each"),
		&List{Elems: []Node{Symbol("lambda"), &List{Elems: []Node{Symbol(q.Var)}}, body}},
		src,
	}}

	buildRes := &List{Elems: []Node{
		Symbol("for-each"),
		&List{Elems: []Node{
			Symbol("lambda"),
			&List{Elems: []Node{gParam}},
			&List{Elems: []Node{
				Symbol("set!"), Symbol(res),
				&List{Elems: []Node{Symbol("append"), Symbol(res), &List{Elems: []Node{Symbol("_list"), sel}}}},
			}},
		}},
		&List{Elems: []Node{Symbol("hash-table-values"), Symbol(groups)}},
	}}

	return &List{Elems: []Node{
		Symbol("let"),
		&List{Elems: []Node{
			&List{Elems: []Node{Symbol(groups), &List{Elems: []Node{Symbol("make-hash-table")}}}},
			&List{Elems: []Node{Symbol(res), &List{Elems: []Node{Symbol("_list")}}}},
		}},
		&List{Elems: []Node{Symbol("begin"), loop, buildRes, Symbol(res)}},
	}}, nil
}

func convertQueryExpr(q *parser.QueryExpr) (Node, error) {
	prevEnv := currentEnv
	env := types.NewEnv(currentEnv)

	srcType := types.ExprType(q.Source, currentEnv)
	var elemType types.Type = types.AnyType{}
	if lt, ok := srcType.(types.ListType); ok {
		elemType = lt.Elem
	} else if gt, ok := srcType.(types.GroupType); ok {
		elemType = gt.Elem
	}
	env.SetVar(q.Var, elemType, true)
	for _, f := range q.Froms {
		ft := types.ExprType(f.Src, currentEnv)
		var fe types.Type = types.AnyType{}
		if lt, ok := ft.(types.ListType); ok {
			fe = lt.Elem
		}
		env.SetVar(f.Var, fe, true)
	}
	for _, j := range q.Joins {
		jt := types.ExprType(j.Src, currentEnv)
		var je types.Type = types.AnyType{}
		if lt, ok := jt.(types.ListType); ok {
			je = lt.Elem
		}
		env.SetVar(j.Var, je, true)
	}
	currentEnv = env

	loops := []struct {
		name   string
		source Node
	}{}
	src, err := convertParserExpr(q.Source)
	if err != nil {
		currentEnv = prevEnv
		return nil, err
	}
	if _, ok := srcType.(types.GroupType); ok {
		src = &List{Elems: []Node{Symbol("hash-table-ref"), src, StringLit("items")}}
	}
	loops = append(loops, struct {
		name   string
		source Node
	}{q.Var, src})
	for _, j := range q.Joins {
		src, err := convertParserExpr(j.Src)
		if err != nil {
			currentEnv = prevEnv
			return nil, err
		}
		if _, ok := types.ExprType(j.Src, currentEnv).(types.GroupType); ok {
			src = &List{Elems: []Node{Symbol("hash-table-ref"), src, StringLit("items")}}
		}
		loops = append(loops, struct {
			name   string
			source Node
		}{j.Var, src})
	}
	for _, f := range q.Froms {
		src, err := convertParserExpr(f.Src)
		if err != nil {
			currentEnv = prevEnv
			return nil, err
		}
		if _, ok := types.ExprType(f.Src, currentEnv).(types.GroupType); ok {
			src = &List{Elems: []Node{Symbol("hash-table-ref"), src, StringLit("items")}}
		}
		loops = append(loops, struct {
			name   string
			source Node
		}{f.Var, src})
	}
	sel, err := convertParserExpr(q.Select)
	if err != nil {
		currentEnv = prevEnv
		return nil, err
	}
	var cond Node
	if q.Where != nil {
		cond, err = convertParserExpr(q.Where)
		if err != nil {
			currentEnv = prevEnv
			return nil, err
		}
	}
	for _, j := range q.Joins {
		jc, err := convertParserExpr(j.On)
		if err != nil {
			currentEnv = prevEnv
			return nil, err
		}
		if cond == nil {
			cond = jc
		} else {
			cond = makeBinary("&&", cond, jc)
		}
	}
	resSym := gensym("res")
	appendNode := Node(&List{Elems: []Node{
		Symbol("set!"), resSym,
		&List{Elems: []Node{
			Symbol("append"), resSym,
			&List{Elems: []Node{Symbol("_list"), sel}},
		}},
	}})
	if cond != nil {
		appendNode = &List{Elems: []Node{Symbol("if"), cond, appendNode, voidSym()}}
	}
	body := appendNode
	for i := len(loops) - 1; i >= 0; i-- {
		l := loops[i]
		body = &List{Elems: []Node{
			Symbol("for-each"),
			&List{Elems: []Node{Symbol("lambda"), &List{Elems: []Node{Symbol(l.name)}}, body}},
			l.source,
		}}
	}
	currentEnv = prevEnv
	return &List{Elems: []Node{
		Symbol("let"),
		&List{Elems: []Node{&List{Elems: []Node{resSym, &List{Elems: []Node{Symbol("_list")}}}}}},
		&List{Elems: []Node{Symbol("begin"), body, resSym}},
	}}, nil
}

func hasFloat(n Node) bool {
	switch v := n.(type) {
	case FloatLit:
		return true
	case *List:
		for _, e := range v.Elems {
			if hasFloat(e) {
				return true
			}
		}
	}
	return false
}

func makeBinary(op string, left, right Node) Node {
	isStr := func(n Node) bool {
		switch n.(type) {
		case StringLit:
			return true
		}
		return false
	}
	switch op {
	case "+":
		if lstr, ok := left.(StringLit); ok {
			if rstr, ok2 := right.(StringLit); ok2 {
				return StringLit(string(lstr) + string(rstr))
			}
		}
		if isStr(left) || isStr(right) {
			return &List{Elems: []Node{Symbol("string-append"), left, right}}
		}
		return &List{Elems: []Node{Symbol("+"), left, right}}
	case "-", "*":
		return &List{Elems: []Node{Symbol(op), left, right}}
	case "/":
		return &List{Elems: []Node{Symbol("_div"), left, right}}
	case "%":
		needBase = true
		return &List{Elems: []Node{Symbol("_mod"), left, right}}
	case "<":
		if isStr(left) || isStr(right) {
			return &List{Elems: []Node{Symbol("string<?"), left, right}}
		}
		return &List{Elems: []Node{Symbol("<"), left, right}}
	case "<=":
		if isStr(left) || isStr(right) {
			return &List{Elems: []Node{Symbol("string<=?"), left, right}}
		}
		return &List{Elems: []Node{Symbol("<="), left, right}}
	case ">":
		if isStr(left) || isStr(right) {
			return &List{Elems: []Node{Symbol("string>?"), left, right}}
		}
		return &List{Elems: []Node{Symbol(">"), left, right}}
	case ">=":
		if isStr(left) || isStr(right) {
			return &List{Elems: []Node{Symbol("string>=?"), left, right}}
		}
		return &List{Elems: []Node{Symbol(">="), left, right}}
	case "==":
		if isStr(left) || isStr(right) {
			if isStr(left) {
				if b, ok := right.(BoolLit); ok {
					if bool(b) {
						right = StringLit("true")
					} else {
						right = StringLit("false")
					}
				}
			}
			if isStr(right) {
				if b, ok := left.(BoolLit); ok {
					if bool(b) {
						left = StringLit("true")
					} else {
						left = StringLit("false")
					}
				}
			}
			return &List{Elems: []Node{Symbol("equal?"), left, right}}
		}
		return &List{Elems: []Node{Symbol("equal?"), left, right}}
	case "!=":
		if isStr(left) || isStr(right) {
			if isStr(left) {
				if b, ok := right.(BoolLit); ok {
					if bool(b) {
						right = StringLit("true")
					} else {
						right = StringLit("false")
					}
				}
			}
			if isStr(right) {
				if b, ok := left.(BoolLit); ok {
					if bool(b) {
						left = StringLit("true")
					} else {
						left = StringLit("false")
					}
				}
			}
			return &List{Elems: []Node{Symbol("not"), &List{Elems: []Node{Symbol("equal?"), left, right}}}}
		}
		return &List{Elems: []Node{Symbol("not"), &List{Elems: []Node{Symbol("equal?"), left, right}}}}
	case "&&":
		return &List{Elems: []Node{Symbol("and"), left, right}}
	case "||":
		return &List{Elems: []Node{Symbol("or"), left, right}}
	case "in":
		return &List{Elems: []Node{
			Symbol("cond"),
			&List{Elems: []Node{&List{Elems: []Node{Symbol("string?"), right}}, &List{Elems: []Node{Symbol("if"), &List{Elems: []Node{Symbol("string-contains"), right, left}}, BoolLit(true), BoolLit(false)}}}},
			&List{Elems: []Node{&List{Elems: []Node{Symbol("hash-table?"), right}}, &List{Elems: []Node{Symbol("if"), &List{Elems: []Node{Symbol("hash-table-exists?"), right, left}}, BoolLit(true), BoolLit(false)}}}},
			&List{Elems: []Node{Symbol("else"), &List{Elems: []Node{Symbol("if"), &List{Elems: []Node{Symbol("member"), left, right}}, BoolLit(true), BoolLit(false)}}}},
		}}
	case "union":
		return &List{Elems: []Node{Symbol("delete-duplicates"), &List{Elems: []Node{Symbol("append"), left, right}}}}
	case "union_all":
		return &List{Elems: []Node{Symbol("append"), left, right}}
	case "except":
		lambda := &List{Elems: []Node{Symbol("lambda"), &List{Elems: []Node{Symbol("x")}}, &List{Elems: []Node{Symbol("not"), &List{Elems: []Node{Symbol("member"), Symbol("x"), right}}}}}}
		return &List{Elems: []Node{Symbol("filter"), lambda, left}}
	case "intersect":
		lambda := &List{Elems: []Node{Symbol("lambda"), &List{Elems: []Node{Symbol("x")}}, &List{Elems: []Node{Symbol("member"), Symbol("x"), right}}}}
		return &List{Elems: []Node{Symbol("filter"), lambda, left}}
	default:
		return &List{Elems: []Node{Symbol(op), left, right}}
	}
}

func makeBinaryTyped(op string, left, right Node, lt, rt types.Type) Node {
	isStrType := func(t types.Type) bool {
		_, ok := t.(types.StringType)
		return ok
	}
	isBoolType := func(t types.Type) bool {
		_, ok := t.(types.BoolType)
		return ok
	}
	isListType := func(t types.Type) bool {
		_, ok := t.(types.ListType)
		return ok
	}
	if op == "/" {
		return &List{Elems: []Node{Symbol("_div"), left, right}}
	}
	if op == "%" {
		needBase = true
		return &List{Elems: []Node{Symbol("_mod"), left, right}}
	}
	if isStrType(lt) || isStrType(rt) {
		if isStrType(lt) {
			if b, ok := right.(BoolLit); ok {
				if bool(b) {
					right = StringLit("true")
				} else {
					right = StringLit("false")
				}
			}
		}
		if isStrType(rt) {
			if b, ok := left.(BoolLit); ok {
				if bool(b) {
					left = StringLit("true")
				} else {
					left = StringLit("false")
				}
			}
		}
		switch op {
		case "+":
			if lstr, ok := left.(StringLit); ok {
				if rstr, ok2 := right.(StringLit); ok2 {
					return StringLit(string(lstr) + string(rstr))
				}
			}
			return &List{Elems: []Node{Symbol("string-append"), left, right}}
		case "<":
			return &List{Elems: []Node{Symbol("string<?"), left, right}}
		case "<=":
			return &List{Elems: []Node{Symbol("string<=?"), left, right}}
		case ">":
			return &List{Elems: []Node{Symbol("string>?"), left, right}}
		case ">=":
			return &List{Elems: []Node{Symbol("string>=?"), left, right}}
		case "==":
			return &List{Elems: []Node{Symbol("equal?"), left, right}}
		case "!=":
			return &List{Elems: []Node{Symbol("not"), &List{Elems: []Node{Symbol("equal?"), left, right}}}}
		}
	}

	if op == "+" && (isListType(lt) || isListType(rt)) {
		return &List{Elems: []Node{Symbol("append"), left, right}}
	}
	if isBoolType(lt) || isBoolType(rt) {
		switch op {
		case "==":
			return &List{Elems: []Node{Symbol("eq?"), left, right}}
		case "!=":
			return &List{Elems: []Node{Symbol("not"), &List{Elems: []Node{Symbol("eq?"), left, right}}}}
		}
	}
	isAnyType := func(t types.Type) bool {
		_, ok := t.(types.AnyType)
		return ok
	}
	if isAnyType(lt) || isAnyType(rt) {
		switch op {
		case "<":
			return &List{Elems: []Node{Symbol("_lt"), left, right}}
		case "<=":
			return &List{Elems: []Node{Symbol("_le"), left, right}}
		case ">":
			return &List{Elems: []Node{Symbol("_gt"), left, right}}
		case ">=":
			return &List{Elems: []Node{Symbol("_ge"), left, right}}
		case "+":
			return &List{Elems: []Node{Symbol("_add"), left, right}}
		}
	}
	return makeBinary(op, left, right)
}

func replaceSymbol(n Node, old, new Symbol) Node {
	switch t := n.(type) {
	case Symbol:
		if t == old {
			return new
		}
		return t
	case *List:
		elems := make([]Node, len(t.Elems))
		for i, e := range t.Elems {
			elems[i] = replaceSymbol(e, old, new)
		}
		return &List{Elems: elems}
	default:
		return n
	}
}

func mapToAlist(n Node) Node {
	lst, ok := n.(*List)
	if !ok || len(lst.Elems) != 2 {
		return n
	}
	if sym, ok := lst.Elems[0].(Symbol); !ok || sym != "alist->hash-table" {
		return n
	}
	if inner, ok := lst.Elems[1].(*List); ok {
		return inner
	}
	return n
}

func precedence(op string) int {
	switch op {
	case "||":
		return 1
	case "&&":
		return 2
	case "==", "!=", "<", "<=", ">", ">=", "in":
		return 3
	case "+", "-":
		return 4
	case "*", "/", "%":
		return 5
	default:
		return 0
	}
}

func binaryResultType(op string, lt, rt types.Type) types.Type {
	isInt := func(t types.Type) bool {
		if _, ok := t.(types.IntType); ok {
			return true
		}
		if _, ok := t.(types.Int64Type); ok {
			return true
		}
		return false
	}
	switch op {
	case "+":
		if _, ok := lt.(types.StringType); ok {
			return types.StringType{}
		}
		if _, ok := rt.(types.StringType); ok {
			return types.StringType{}
		}
		if isInt(lt) && isInt(rt) {
			return types.IntType{}
		}
	case "-", "*", "/", "%":
		if isInt(lt) && isInt(rt) {
			return types.IntType{}
		}
	case "<", "<=", ">", ">=", "==", "!=", "&&", "||", "in":
		return types.BoolType{}
	}
	return types.AnyType{}
}

func isAddOne(end, start Node) bool {
	lst, ok := end.(*List)
	if !ok || len(lst.Elems) != 3 {
		return false
	}
	if sym, ok := lst.Elems[0].(Symbol); !ok || sym != Symbol("+") {
		return false
	}
	if nodesEqual(lst.Elems[1], start) && isIntOne(lst.Elems[2]) {
		return true
	}
	if nodesEqual(lst.Elems[2], start) && isIntOne(lst.Elems[1]) {
		return true
	}
	return false
}

func nodesEqual(a, b Node) bool {
	switch x := a.(type) {
	case Symbol:
		y, ok := b.(Symbol)
		return ok && x == y
	case IntLit:
		y, ok := b.(IntLit)
		return ok && x == y
	case StringLit:
		y, ok := b.(StringLit)
		return ok && x == y
	case *List:
		y, ok := b.(*List)
		if !ok || len(x.Elems) != len(y.Elems) {
			return false
		}
		for i := range x.Elems {
			if !nodesEqual(x.Elems[i], y.Elems[i]) {
				return false
			}
		}
		return true
	default:
		return false
	}
}

func isIntOne(n Node) bool {
	v, ok := n.(IntLit)
	return ok && int(v) == 1
}

func convertCall(target Node, call *parser.CallOp) (Node, error) {
	args := make([]Node, len(call.Args))
	for i, a := range call.Args {
		n, err := convertParserExpr(a)
		if err != nil {
			return nil, err
		}
		args[i] = n
	}
	// handle testpkg.ECDSAExample which is represented as (hash-table-ref testpkg "ECDSAExample")
	if l, ok := target.(*List); ok && len(l.Elems) == 3 {
		if sym, ok := l.Elems[0].(Symbol); ok && sym == "hash-table-ref" {
			if root, ok := l.Elems[1].(Symbol); ok && root == "testpkg" {
				if key, ok := l.Elems[2].(StringLit); ok && string(key) == "ECDSAExample" {
					if len(args) != 0 {
						return nil, fmt.Errorf("ECDSAExample expects no args")
					}
					r := testpkg.ECDSAExample()
					needHash = true
					obj := gensym("obj")
					bindings := &List{Elems: []Node{&List{Elems: []Node{Symbol(obj), &List{Elems: []Node{Symbol("make-hash-table")}}}}}}
					stmts := []Node{
						&List{Elems: []Node{Symbol("hash-table-set!"), Symbol(obj), StringLit("D"), StringLit(r.D)}},
						&List{Elems: []Node{Symbol("hash-table-set!"), Symbol(obj), StringLit("X"), StringLit(r.X)}},
						&List{Elems: []Node{Symbol("hash-table-set!"), Symbol(obj), StringLit("Y"), StringLit(r.Y)}},
						&List{Elems: []Node{Symbol("hash-table-set!"), Symbol(obj), StringLit("Hash"), StringLit(r.Hash)}},
						&List{Elems: []Node{Symbol("hash-table-set!"), Symbol(obj), StringLit("R"), StringLit(r.R)}},
						&List{Elems: []Node{Symbol("hash-table-set!"), Symbol(obj), StringLit("S"), StringLit(r.S)}},
					}
					valid := Symbol("#f")
					if r.Valid {
						valid = Symbol("#t")
					}
					stmts = append(stmts, &List{Elems: []Node{Symbol("hash-table-set!"), Symbol(obj), StringLit("Valid"), valid}})
					stmts = append(stmts, Symbol(obj))
					body := append([]Node{Symbol("let"), bindings}, stmts...)
					return &List{Elems: body}, nil
				}
			}
		}
	}
	if sym, ok := target.(Symbol); ok {
		if ut, ok := currentEnv.FindUnionByVariant(string(sym)); ok {
			needHash = true
			st := ut.Variants[string(sym)]
			if len(args) != len(st.Order) {
				return nil, fmt.Errorf("%s expects %d args", sym, len(st.Order))
			}
			pairs := []Node{Symbol("_list")}
			opPair := &List{Elems: []Node{Symbol("cons"), StringLit(tagKey), ensureUnionConst(string(sym))}}
			pairs = append(pairs, opPair)
			for i, field := range st.Order {
				pair := &List{Elems: []Node{Symbol("cons"), StringLit(field), args[i]}}
				pairs = append(pairs, pair)
			}
			return &List{Elems: []Node{Symbol("alist->hash-table"), &List{Elems: pairs}}}, nil
		}
		if _, ok := currentEnv.GetFunc(string(sym)); ok {
			return &List{Elems: append([]Node{Symbol(string(sym))}, args...)}, nil
		}
		switch sym {
		case "testpkg.ECDSAExample":
			if len(args) != 0 {
				return nil, fmt.Errorf("ECDSAExample expects no args")
			}
			r := testpkg.ECDSAExample()
			needHash = true
			obj := gensym("obj")
			bindings := &List{Elems: []Node{&List{Elems: []Node{Symbol(obj), &List{Elems: []Node{Symbol("make-hash-table")}}}}}}
			stmts := []Node{
				&List{Elems: []Node{Symbol("hash-table-set!"), Symbol(obj), StringLit("D"), StringLit(r.D)}},
				&List{Elems: []Node{Symbol("hash-table-set!"), Symbol(obj), StringLit("X"), StringLit(r.X)}},
				&List{Elems: []Node{Symbol("hash-table-set!"), Symbol(obj), StringLit("Y"), StringLit(r.Y)}},
				&List{Elems: []Node{Symbol("hash-table-set!"), Symbol(obj), StringLit("Hash"), StringLit(r.Hash)}},
				&List{Elems: []Node{Symbol("hash-table-set!"), Symbol(obj), StringLit("R"), StringLit(r.R)}},
				&List{Elems: []Node{Symbol("hash-table-set!"), Symbol(obj), StringLit("S"), StringLit(r.S)}},
			}
			valid := Symbol("#f")
			if r.Valid {
				valid = Symbol("#t")
			}
			stmts = append(stmts, &List{Elems: []Node{Symbol("hash-table-set!"), Symbol(obj), StringLit("Valid"), valid}})
			stmts = append(stmts, Symbol(obj))
			body := append([]Node{Symbol("let"), bindings}, stmts...)
			return &List{Elems: body}, nil
		case "print":
			forms := []Node{Symbol("begin")}
			for i, a := range args {
				if _, ok := types.ExprType(call.Args[i], currentEnv).(types.BoolType); ok {
					a = &List{Elems: []Node{Symbol("if"), a, BoolLit(true), BoolLit(false)}}
				}
				disp := &List{Elems: []Node{Symbol("if"), &List{Elems: []Node{Symbol("string?"), a}}, a, &List{Elems: []Node{Symbol("to-str"), a}}}}
				forms = append(forms, &List{Elems: []Node{Symbol("_display"), disp}})
				if i < len(args)-1 {
					forms = append(forms, &List{Elems: []Node{Symbol("_display"), StringLit(" ")}})
				}
			}
			forms = append(forms, &List{Elems: []Node{Symbol("newline")}})
			return &List{Elems: forms}, nil
		case "len", "count":
			if len(args) != 1 {
				return nil, fmt.Errorf("len expects 1 arg")
			}
			return &List{Elems: []Node{Symbol("_len"), args[0]}}, nil
		case "append":
			if len(args) != 2 {
				return nil, fmt.Errorf("append expects 2 args")
			}
			if _, ok := types.ExprType(call.Args[0], currentEnv).(types.StringType); ok {
				return &List{Elems: []Node{Symbol("string-append"), args[0], args[1]}}, nil
			}
			return &List{Elems: []Node{Symbol("append"), args[0], &List{Elems: []Node{Symbol("_list"), args[1]}}}}, nil
		case "concat":
			if len(args) != 2 {
				return nil, fmt.Errorf("concat expects 2 args")
			}
			return &List{Elems: []Node{Symbol("append"), args[0], args[1]}}, nil
		case "split":
			if len(args) != 2 {
				return nil, fmt.Errorf("split expects 2 args")
			}
			return &List{Elems: []Node{Symbol("_split"), args[0], args[1]}}, nil
		case "repeat":
			if len(args) != 2 {
				return nil, fmt.Errorf("repeat expects 2 args")
			}
			return &List{Elems: []Node{Symbol("_repeat"), args[0], args[1]}}, nil
		case "contains":
			if len(args) != 2 {
				return nil, fmt.Errorf("contains expects 2 args")
			}
			needHash = true
			return &List{Elems: []Node{
				Symbol("cond"),
				&List{Elems: []Node{&List{Elems: []Node{Symbol("hash-table?"), args[0]}}, &List{Elems: []Node{Symbol("hash-table-exists?"), args[0], args[1]}}}},
				&List{Elems: []Node{&List{Elems: []Node{Symbol("string?"), args[0]}}, &List{Elems: []Node{Symbol("if"), &List{Elems: []Node{Symbol("string-contains"), args[0], args[1]}}, BoolLit(true), BoolLit(false)}}}},
				&List{Elems: []Node{Symbol("else"), &List{Elems: []Node{Symbol("if"), &List{Elems: []Node{Symbol("member"), args[1], args[0]}}, BoolLit(true), BoolLit(false)}}}},
			}}, nil
		case "sha256":
			if len(args) != 1 {
				return nil, fmt.Errorf("sha256 expects 1 arg")
			}
			needSHA256 = true
			return &List{Elems: []Node{Symbol("_sha256"), args[0]}}, nil
		case "keys":
			if len(args) != 1 {
				return nil, fmt.Errorf("keys expects 1 arg")
			}
			needHash = true
			return &List{Elems: []Node{Symbol("hash-table-keys"), args[0]}}, nil
		case "sum":
			if len(args) != 1 {
				return nil, fmt.Errorf("sum expects 1 arg")
			}
			return &List{Elems: []Node{Symbol("apply"), Symbol("+"), args[0]}}, nil
		case "avg":
			if len(args) != 1 {
				return nil, fmt.Errorf("avg expects 1 arg")
			}
			xs := Symbol("xs")
			div := &List{Elems: []Node{Symbol("/"),
				&List{Elems: []Node{Symbol("apply"), Symbol("+"), xs}},
				&List{Elems: []Node{Symbol("length"), xs}},
			}}
			return &List{Elems: []Node{
				Symbol("let"),
				&List{Elems: []Node{&List{Elems: []Node{xs, args[0]}}}},
				&List{Elems: []Node{Symbol("exact->inexact"), div}},
			}}, nil
		case "float":
			if len(args) != 1 {
				return nil, fmt.Errorf("float expects 1 arg")
			}
			return &List{Elems: []Node{Symbol("exact->inexact"), args[0]}}, nil
		case "str":
			if len(args) != 1 {
				return nil, fmt.Errorf("str expects 1 arg")
			}
			return &List{Elems: []Node{Symbol("to-str-space"), args[0]}}, nil
		case "parseIntStr":
			if len(args) == 1 {
				args = append(args, IntLit(10))
			} else if len(args) != 2 {
				return nil, fmt.Errorf("parseIntStr expects 1 or 2 args")
			}
			return &List{Elems: []Node{Symbol("_parseIntStr"), args[0], args[1]}}, nil
		case "min", "max":
			if len(args) != 1 {
				return nil, fmt.Errorf("%s expects 1 arg", sym)
			}
			return &List{Elems: []Node{Symbol("apply"), Symbol(string(sym)), args[0]}}, nil
		case "substring", "substr":
			if len(args) != 3 {
				return nil, fmt.Errorf("%s expects 3 args", sym)
			}
			return &List{Elems: []Node{Symbol("_substring"), args[0], args[1], args[2]}}, nil
		case "exists":
			if len(args) != 1 {
				return nil, fmt.Errorf("exists expects 1 arg")
			}
			return &List{Elems: []Node{Symbol("if"), &List{Elems: []Node{Symbol("null?"), args[0]}}, StringLit("false"), StringLit("true")}}, nil
		case "pow":
			if len(args) != 2 {
				return nil, fmt.Errorf("pow expects 2 args")
			}
			return &List{Elems: []Node{Symbol("expt"), args[0], args[1]}}, nil
		case "num":
			if len(args) != 1 {
				return nil, fmt.Errorf("num expects 1 arg")
			}
			return &List{Elems: []Node{Symbol("numerator"), args[0]}}, nil
		case "denom":
			if len(args) != 1 {
				return nil, fmt.Errorf("denom expects 1 arg")
			}
			return &List{Elems: []Node{Symbol("denominator"), args[0]}}, nil
		case "int":
			if len(args) != 1 {
				return nil, fmt.Errorf("int expects 1 arg")
			}
			x := gensym("v")
			cond := &List{Elems: []Node{
				Symbol("cond"),
				&List{Elems: []Node{&List{Elems: []Node{Symbol("string?"), Symbol(x)}}, &List{Elems: []Node{Symbol("exact"), &List{Elems: []Node{Symbol("_floor"), &List{Elems: []Node{Symbol("string->number"), Symbol(x)}}}}}}}},
				&List{Elems: []Node{&List{Elems: []Node{Symbol("boolean?"), Symbol(x)}}, &List{Elems: []Node{Symbol("if"), Symbol(x), IntLit(1), IntLit(0)}}}},
				&List{Elems: []Node{Symbol("else"), &List{Elems: []Node{Symbol("exact"), &List{Elems: []Node{Symbol("_floor"), Symbol(x)}}}}}},
			}}
			return &List{Elems: []Node{
				Symbol("let"),
				&List{Elems: []Node{&List{Elems: []Node{Symbol(x), args[0]}}}},
				cond,
			}}, nil
		case "input":
			if len(args) != 0 {
				return nil, fmt.Errorf("input expects no args")
			}
			usesInput = true
			return &List{Elems: []Node{Symbol("_input")}}, nil
		case "now":
			if len(args) != 0 {
				return nil, fmt.Errorf("now expects no args")
			}
			usesNow = true
			needBase = true
			return &List{Elems: []Node{Symbol("now")}}, nil
		case "testpkg.FifteenPuzzleExample":
			if len(args) != 0 {
				return nil, fmt.Errorf("FifteenPuzzleExample expects no args")
			}
			return StringLit(testpkg.FifteenPuzzleExample()), nil
		default:
			elems := make([]Node, 0, len(args)+1)
			elems = append(elems, Symbol(sym))
			elems = append(elems, args...)
			return &List{Elems: elems}, nil
		}
	}
	// generic function call where target is an expression
	elems := make([]Node, 0, len(args)+1)
	elems = append(elems, target)
	elems = append(elems, args...)
	return &List{Elems: elems}, nil
}

func convertIndex(target Node, orig *parser.Primary, idx *parser.IndexOp) (Node, error) {
	if idx.Colon != nil || idx.Colon2 != nil || idx.End != nil || idx.Step != nil {
		var start Node = IntLit(0)
		if idx.Start != nil {
			n, err := convertParserExpr(idx.Start)
			if err != nil {
				return nil, err
			}
			start = n
		}
		var end Node
		if idx.End != nil {
			n, err := convertParserExpr(idx.End)
			if err != nil {
				return nil, err
			}
			end = n
		} else {
			t := types.ExprType(&parser.Expr{Binary: &parser.BinaryExpr{Left: &parser.Unary{Value: &parser.PostfixExpr{Target: orig}}}}, currentEnv)
			switch t.(type) {
			case types.StringType:
				end = &List{Elems: []Node{Symbol("string-length"), target}}
			case types.ListType:
				end = &List{Elems: []Node{Symbol("length"), target}}
			default:
				end = &List{Elems: []Node{
					Symbol("cond"),
					&List{Elems: []Node{&List{Elems: []Node{Symbol("string?"), target}}, &List{Elems: []Node{Symbol("string-length"), target}}}},
					&List{Elems: []Node{&List{Elems: []Node{Symbol("hash-table?"), target}}, &List{Elems: []Node{Symbol("hash-table-size"), target}}}},
					&List{Elems: []Node{Symbol("else"), &List{Elems: []Node{Symbol("length"), target}}}},
				}}
			}
		}
		lenNode := &List{Elems: []Node{Symbol("-"), end, start}}
		if orig != nil {
			switch types.ExprType(&parser.Expr{Binary: &parser.BinaryExpr{Left: &parser.Unary{Value: &parser.PostfixExpr{Target: orig}}}}, currentEnv).(type) {
			case types.StringType:
				return &List{Elems: []Node{Symbol("_substring"), target, start, end}}, nil
			case types.ListType:
				// if slicing a list by one element, treat as index access
				if isAddOne(end, start) {
					return &List{Elems: []Node{Symbol("list-ref-safe"), target, start}}, nil
				}
				return &List{Elems: []Node{Symbol("take"), &List{Elems: []Node{Symbol("drop"), target, start}}, lenNode}}, nil
			}
		}
		return &List{Elems: []Node{
			Symbol("if"),
			&List{Elems: []Node{Symbol("string?"), target}},
			&List{Elems: []Node{Symbol("_substring"), target, start, end}},
			&List{Elems: []Node{Symbol("take"), &List{Elems: []Node{Symbol("drop"), target, start}}, lenNode}},
		}}, nil
	}
	if idx.Start == nil {
		return nil, fmt.Errorf("unsupported index")
	}
	in, err := convertParserExpr(idx.Start)
	if err != nil {
		return nil, err
	}
	if orig != nil {
		switch types.ExprType(&parser.Expr{Binary: &parser.BinaryExpr{Left: &parser.Unary{Value: &parser.PostfixExpr{Target: orig}}}}, currentEnv).(type) {
		case types.StringType:
			one := &List{Elems: []Node{Symbol("+"), in, IntLit(1)}}
			return &List{Elems: []Node{Symbol("_substring"), target, in, one}}, nil
		case types.ListType:
			return &List{Elems: []Node{Symbol("list-ref-safe"), target, in}}, nil
		case types.MapType:
			needHash = true
			return &List{Elems: []Node{Symbol("hash-table-ref/default"), target, in, voidSym()}}, nil
		}
	}
	return &List{Elems: []Node{
		Symbol("cond"),
		&List{Elems: []Node{&List{Elems: []Node{Symbol("string?"), target}}, &List{Elems: []Node{Symbol("_substring"), target, in, &List{Elems: []Node{Symbol("+"), in, IntLit(1)}}}}}},
		&List{Elems: []Node{&List{Elems: []Node{Symbol("hash-table?"), target}}, &List{Elems: []Node{Symbol("hash-table-ref"), target, in}}}},
		&List{Elems: []Node{Symbol("else"), &List{Elems: []Node{Symbol("list-ref-safe"), target, in}}}},
	}}, nil
}
