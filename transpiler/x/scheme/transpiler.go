//go:build slow

package scheme

import (
	"bytes"
	"fmt"
	"io"
	"math"
	"os/exec"
	"strings"
	"time"

	"mochi/parser"
	testpkg "mochi/runtime/ffi/go/testpkg"
	"mochi/types"
)

var currentEnv *types.Env
var breakStack []Symbol
var continueStack []Node
var gensymCounter int
var needBase bool
var needHash bool
var usesInput bool
var returnStack []Symbol

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

func gensym(prefix string) Symbol {
	gensymCounter++
	return Symbol(fmt.Sprintf("%s%d", prefix, gensymCounter))
}

// Node represents a Scheme AST node.
type Node interface{ Emit(io.Writer) }

type Symbol string

func (s Symbol) Emit(w io.Writer) { io.WriteString(w, string(s)) }

type StringLit string

func (s StringLit) Emit(w io.Writer) { fmt.Fprintf(w, "%q", string(s)) }

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
	if len(src) > 0 && src[len(src)-1] != '\n' {
		src = append(src, '\n')
	}
	var buf bytes.Buffer
	indent := 0
	for i := 0; i < len(src); i++ {
		ch := src[i]
		switch ch {
		case '(':
			if i > 0 && src[i-1] == '\n' {
				buf.WriteString(strings.Repeat("  ", indent))
			}
			buf.WriteByte('(')
			indent++
		case ')':
			if indent > 0 {
				indent--
			}
			buf.WriteByte(')')
			if i+1 < len(src) && src[i+1] != '\n' {
				buf.WriteByte('\n')
			}
		case '\n':
			buf.WriteByte('\n')
		default:
			buf.WriteByte(ch)
		}
	}
	if buf.Len() > 0 && buf.Bytes()[buf.Len()-1] != '\n' {
		buf.WriteByte('\n')
	}
	return append(header(), buf.Bytes()...)
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
	if needBase {
		prelude += "(import (only (scheme base) call/cc list-ref list-set! list))\n"
		prelude += "(import (scheme time))\n"
	}
	if needHash {
		prelude += "(import (srfi 69))\n"
	}
	if usesInput {
		prelude += "(import (chibi io))\n"
	}
	prelude += `(define (to-str x)
  (cond ((pair? x)
         (string-append "[" (string-join (map to-str x) ", ") "]"))
        ((string? x) x)
        ((boolean? x) (if x "1" "0"))
        (else (number->string x))))`
	if usesInput {
		prelude += "\n(define (_input)\n  (let ((l (read-line)))\n    (if (eof-object? l) \"\" l)))"
	}
	return []byte(fmt.Sprintf(";; Generated on %s\n%s\n",
		ts.In(loc).Format("2006-01-02 15:04 -0700"), prelude))
}

func voidSym() Node { return &List{Elems: []Node{Symbol("quote"), Symbol("nil")}} }

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
		if _, ok := types.ExprType(fs.Source, prevEnv).(types.MapType); ok {
			needHash = true
			iter = &List{Elems: []Node{Symbol("hash-table-keys"), iter}}
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
					Symbol("let"), &List{Elems: []Node{&List{Elems: []Node{Symbol(fs.Name), &List{Elems: []Node{Symbol("car"), loopVar}}}}}},
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
					if _, ok := types.ExprType(call.Args[i], currentEnv).(types.BoolType); ok {
						a = &List{Elems: []Node{Symbol("if"), a, BoolLit(true), BoolLit(false)}}
					}
					forms = append(forms, &List{Elems: []Node{Symbol("display"), &List{Elems: []Node{Symbol("to-str"), a}}}})
					if i < len(args)-1 {
						forms = append(forms, &List{Elems: []Node{Symbol("display"), StringLit(" ")}})
					}
				}
				forms = append(forms, &List{Elems: []Node{Symbol("newline")}})
				return &List{Elems: forms}, nil
			case "json":
				if len(args) != 1 {
					return nil, fmt.Errorf("json expects 1 arg")
				}
				forms := []Node{Symbol("begin"),
					&List{Elems: []Node{Symbol("display"), args[0]}},
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
	case st.Assign != nil && len(st.Assign.Index) == 0 && len(st.Assign.Field) == 0:
		val, err := convertParserExpr(st.Assign.Value)
		if err != nil {
			return nil, err
		}
		return &List{Elems: []Node{Symbol("set!"), Symbol(st.Assign.Name), val}}, nil
	case st.Assign != nil && len(st.Assign.Index) > 0 && len(st.Assign.Field) == 0:
		var target Node = Symbol(st.Assign.Name)
		for i := 0; i < len(st.Assign.Index)-1; i++ {
			idx := st.Assign.Index[i]
			var err error
			target, err = convertIndex(target, &parser.Primary{Selector: &parser.SelectorExpr{Root: st.Assign.Name}}, idx)
			if err != nil {
				return nil, err
			}
		}
		last := st.Assign.Index[len(st.Assign.Index)-1]
		idxNode, err := convertParserExpr(last.Start)
		if err != nil {
			return nil, err
		}
		val, err := convertParserExpr(st.Assign.Value)
		if err != nil {
			return nil, err
		}
		typ, _ := currentEnv.GetVar(st.Assign.Name)
		if _, ok := typ.(types.MapType); ok {
			needHash = true
			return &List{Elems: []Node{Symbol("hash-table-set!"), target, idxNode, val}}, nil
		}
		return &List{Elems: []Node{Symbol("list-set!"), target, idxNode, val}}, nil
	case st.Fun != nil:
		params := []Node{}
		prevEnv := currentEnv
		currentEnv = types.NewEnv(currentEnv)
		for _, p := range st.Fun.Params {
			params = append(params, Symbol(p.Name))
			var pt types.Type = types.AnyType{}
			if p.Type != nil {
				pt = types.ResolveTypeRef(p.Type, prevEnv)
			}
			currentEnv.SetVar(p.Name, pt, true)
		}
		retSym := gensym("ret")
		pushReturn(retSym)
		bodyForms, err := convertStmts(st.Fun.Body)
		popReturn()
		currentEnv = prevEnv
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
			&List{Elems: append([]Node{Symbol(st.Fun.Name)}, params...)},
			wrapped,
		}}, nil
	case st.Return != nil:
		var val Node = voidSym()
		if st.Return.Value != nil {
			var err error
			val, err = convertParserExpr(st.Return.Value)
			if err != nil {
				return nil, err
			}
		}
		return &List{Elems: []Node{currentReturn(), val}}, nil
	case st.If != nil:
		return convertIfStmt(st.If)
	case st.While != nil:
		return convertWhileStmt(st.While)
	case st.For != nil:
		return convertForStmt(st.For)
	case st.Type != nil:
		return nil, nil
	case st.Import != nil:
		// imports have no effect in the Scheme transpiler
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
	needBase = false
	needHash = false
	usesInput = false
	p := &Program{}
	for _, st := range prog.Statements {
		form, err := convertStmt(st)
		if err != nil {
			return nil, err
		}
		if form != nil {
			if lst, ok := form.(*List); ok && len(lst.Elems) > 0 && lst.Elems[0] == Symbol("begin") {
				p.Forms = append(p.Forms, lst.Elems[1:]...)
			} else {
				p.Forms = append(p.Forms, form)
			}
		}
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
			typesStack = append(typesStack, types.AnyType{})
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
		typesStack = append(typesStack, types.AnyType{})
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
	// handle selector like `x.contains()` parsed as part of target
	if pf.Target.Selector != nil && len(pf.Target.Selector.Tail) == 1 && pf.Target.Selector.Tail[0] == "contains" && len(pf.Ops) > 0 && pf.Ops[0].Call != nil {
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
	} else {
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
				node = &List{Elems: []Node{Symbol("inexact->exact"), node}}
			} else if t.Simple != nil && *t.Simple == "string" {
				node = &List{Elems: []Node{Symbol("number->string"), node}}
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
	case p.Selector != nil && len(p.Selector.Tail) == 0:
		if currentEnv != nil {
			if _, err := currentEnv.GetVar(p.Selector.Root); err == nil {
				return Symbol(p.Selector.Root), nil
			}
		}
		return StringLit(p.Selector.Root), nil
	case p.Selector != nil:
		if p.Selector.Root == "testpkg" && len(p.Selector.Tail) == 1 && p.Selector.Tail[0] == "FifteenPuzzleExample" {
			return Symbol("testpkg.FifteenPuzzleExample"), nil
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
		elems := []Node{}
		for _, e := range p.List.Elems {
			n, err := convertParserExpr(e)
			if err != nil {
				return nil, err
			}
			elems = append(elems, n)
		}
		return &List{Elems: append([]Node{Symbol("list")}, elems...)}, nil
	case p.Map != nil:
		needHash = true
		pairs := []Node{Symbol("list")}
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
	clauses := []Node{}
	var defaultNode Node = voidSym()
	for _, c := range me.Cases {
		if isUnderscore(c.Pattern) {
			dn, err := convertParserExpr(c.Result)
			if err != nil {
				return nil, err
			}
			defaultNode = dn
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
		clauseCond := &List{Elems: []Node{Symbol("equal?"), temp, pat}}
		clauses = append(clauses, &List{Elems: []Node{clauseCond, res}})
	}
	clauses = append(clauses, &List{Elems: []Node{Symbol("else"), defaultNode}})
	condExpr := &List{Elems: append([]Node{Symbol("cond")}, clauses...)}
	return &List{Elems: []Node{
		Symbol("let"),
		&List{Elems: []Node{&List{Elems: []Node{temp, target}}}},
		condExpr,
	}}, nil
}

func isUnderscore(e *parser.Expr) bool {
	if e == nil || e.Binary == nil || e.Binary.Left == nil || e.Binary.Left.Value == nil {
		return false
	}
	t := e.Binary.Left.Value.Target
	return t != nil && t.Selector != nil && t.Selector.Root == "_" && len(t.Selector.Tail) == 0
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
		stmts, err := convertStmts(fe.BlockBody)
		if err != nil {
			currentEnv = prevEnv
			return nil, err
		}
		if len(stmts) == 1 {
			body = stmts[0]
		} else {
			body = &List{Elems: append([]Node{Symbol("begin")}, stmts...)}
		}
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
		&List{Elems: []Node{Symbol("append"), resSym, &List{Elems: []Node{Symbol("list"), sel}}}},
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
		&List{Elems: []Node{&List{Elems: []Node{resSym, &List{Elems: []Node{Symbol("list")}}}}}},
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
					Symbol("list"),
					&List{Elems: []Node{Symbol("cons"), StringLit("key"), Symbol(k)}},
					&List{Elems: []Node{Symbol("cons"), StringLit("items"), &List{Elems: []Node{Symbol("list")}}}},
				}},
			}},
		}},
		&List{Elems: []Node{Symbol("hash-table-set!"), Symbol(groups), Symbol(k), Symbol(g)}},
	}}

	var item Node
	if len(loops) == 1 {
		item = Symbol(loops[0].name)
	} else {
		itemPairs := []Node{Symbol("list")}
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
			&List{Elems: []Node{Symbol("list"), item}},
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
				&List{Elems: []Node{Symbol("append"), Symbol(res), &List{Elems: []Node{Symbol("list"), sel}}}},
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
			&List{Elems: []Node{Symbol(res), &List{Elems: []Node{Symbol("list")}}}},
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
					Symbol("list"),
					&List{Elems: []Node{Symbol("cons"), StringLit("key"), Symbol(k)}},
					&List{Elems: []Node{Symbol("cons"), StringLit("items"), &List{Elems: []Node{Symbol("list")}}}},
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
			&List{Elems: []Node{Symbol("list"), Symbol(q.Var)}},
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
				&List{Elems: []Node{Symbol("append"), Symbol(res), &List{Elems: []Node{Symbol("list"), sel}}}},
			}},
		}},
		&List{Elems: []Node{Symbol("hash-table-values"), Symbol(groups)}},
	}}

	return &List{Elems: []Node{
		Symbol("let"),
		&List{Elems: []Node{
			&List{Elems: []Node{Symbol(groups), &List{Elems: []Node{Symbol("make-hash-table")}}}},
			&List{Elems: []Node{Symbol(res), &List{Elems: []Node{Symbol("list")}}}},
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
			&List{Elems: []Node{Symbol("list"), sel}},
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
		&List{Elems: []Node{&List{Elems: []Node{resSym, &List{Elems: []Node{Symbol("list")}}}}}},
		&List{Elems: []Node{Symbol("begin"), body, resSym}},
	}}, nil
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
		if isStr(left) || isStr(right) {
			return &List{Elems: []Node{Symbol("string-append"), left, right}}
		}
		return &List{Elems: []Node{Symbol("+"), left, right}}
	case "-", "*":
		return &List{Elems: []Node{Symbol(op), left, right}}
	case "/":
		_, lfloat := left.(FloatLit)
		_, rfloat := right.(FloatLit)
		if lfloat || rfloat {
			return &List{Elems: []Node{Symbol("/"), left, right}}
		}
		return &List{Elems: []Node{Symbol("quotient"), left, right}}
	case "%":
		return &List{Elems: []Node{Symbol("modulo"), left, right}}
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
			return &List{Elems: []Node{Symbol("string=?"), left, right}}
		}
		return &List{Elems: []Node{Symbol("="), left, right}}
	case "!=":
		if isStr(left) || isStr(right) {
			return &List{Elems: []Node{Symbol("not"), &List{Elems: []Node{Symbol("string=?"), left, right}}}}
		}
		return &List{Elems: []Node{Symbol("not"), &List{Elems: []Node{Symbol("="), left, right}}}}
	case "&&":
		return &List{Elems: []Node{Symbol("and"), left, right}}
	case "||":
		return &List{Elems: []Node{Symbol("or"), left, right}}
	case "in":
		return &List{Elems: []Node{
			Symbol("cond"),
			&List{Elems: []Node{&List{Elems: []Node{Symbol("string?"), right}}, &List{Elems: []Node{Symbol("if"), &List{Elems: []Node{Symbol("string-contains"), right, left}}, StringLit("true"), StringLit("false")}}}},
			&List{Elems: []Node{&List{Elems: []Node{Symbol("hash-table?"), right}}, &List{Elems: []Node{Symbol("if"), &List{Elems: []Node{Symbol("hash-table-exists?"), right, left}}, StringLit("true"), StringLit("false")}}}},
			&List{Elems: []Node{Symbol("else"), &List{Elems: []Node{Symbol("if"), &List{Elems: []Node{Symbol("member"), left, right}}, StringLit("true"), StringLit("false")}}}},
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
	if isStrType(lt) || isStrType(rt) {
		switch op {
		case "+":
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
			return &List{Elems: []Node{Symbol("string=?"), left, right}}
		case "!=":
			return &List{Elems: []Node{Symbol("not"), &List{Elems: []Node{Symbol("string=?"), left, right}}}}
		}
	}
	if isBoolType(lt) || isBoolType(rt) {
		switch op {
		case "==":
			return &List{Elems: []Node{Symbol("eq?"), left, right}}
		case "!=":
			return &List{Elems: []Node{Symbol("not"), &List{Elems: []Node{Symbol("eq?"), left, right}}}}
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

func convertCall(target Node, call *parser.CallOp) (Node, error) {
	sym, ok := target.(Symbol)
	if !ok {
		return nil, fmt.Errorf("unsupported call")
	}
	args := make([]Node, len(call.Args))
	for i, a := range call.Args {
		n, err := convertParserExpr(a)
		if err != nil {
			return nil, err
		}
		args[i] = n
	}
	switch sym {
	case "len", "count":
		if len(args) != 1 {
			return nil, fmt.Errorf("len expects 1 arg")
		}
		switch types.ExprType(call.Args[0], currentEnv).(type) {
		case types.StringType:
			return &List{Elems: []Node{Symbol("string-length"), args[0]}}, nil
		case types.ListType:
			return &List{Elems: []Node{Symbol("length"), args[0]}}, nil
		case types.MapType:
			return &List{Elems: []Node{Symbol("hash-table-size"), args[0]}}, nil
		case types.GroupType:
			return &List{Elems: []Node{Symbol("length"), &List{Elems: []Node{Symbol("hash-table-ref"), args[0], StringLit("items")}}}}, nil
		default:
			return &List{Elems: []Node{
				Symbol("cond"),
				&List{Elems: []Node{&List{Elems: []Node{Symbol("string?"), args[0]}}, &List{Elems: []Node{Symbol("string-length"), args[0]}}}},
				&List{Elems: []Node{&List{Elems: []Node{Symbol("hash-table?"), args[0]}}, &List{Elems: []Node{Symbol("hash-table-size"), args[0]}}}},
				&List{Elems: []Node{Symbol("else"), &List{Elems: []Node{Symbol("length"), args[0]}}}},
			}}, nil
		}
	case "append":
		if len(args) != 2 {
			return nil, fmt.Errorf("append expects 2 args")
		}
		if _, ok := types.ExprType(call.Args[0], currentEnv).(types.StringType); ok {
			return &List{Elems: []Node{Symbol("string-append"), args[0], args[1]}}, nil
		}
		return &List{Elems: []Node{Symbol("append"), args[0], &List{Elems: []Node{Symbol("list"), args[1]}}}}, nil
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
	case "str":
		if len(args) != 1 {
			return nil, fmt.Errorf("str expects 1 arg")
		}
		return &List{Elems: []Node{Symbol("number->string"), args[0]}}, nil
	case "min", "max":
		if len(args) != 1 {
			return nil, fmt.Errorf("%s expects 1 arg", sym)
		}
		return &List{Elems: []Node{Symbol("apply"), Symbol(string(sym)), args[0]}}, nil
	case "substring":
		if len(args) != 3 {
			return nil, fmt.Errorf("substring expects 3 args")
		}
		return &List{Elems: []Node{Symbol("substring"), args[0], args[1], args[2]}}, nil
	case "exists":
		if len(args) != 1 {
			return nil, fmt.Errorf("exists expects 1 arg")
		}
		return &List{Elems: []Node{Symbol("if"), &List{Elems: []Node{Symbol("null?"), args[0]}}, StringLit("false"), StringLit("true")}}, nil
	case "int":
		if len(args) != 1 {
			return nil, fmt.Errorf("int expects 1 arg")
		}
		x := args[0]
		return &List{Elems: []Node{
			Symbol("cond"),
			&List{Elems: []Node{&List{Elems: []Node{Symbol("string?"), x}}, &List{Elems: []Node{Symbol("inexact->exact"), &List{Elems: []Node{Symbol("string->number"), x}}}}}},
			&List{Elems: []Node{&List{Elems: []Node{Symbol("boolean?"), x}}, &List{Elems: []Node{Symbol("if"), x, IntLit(1), IntLit(0)}}}},
			&List{Elems: []Node{Symbol("else"), &List{Elems: []Node{Symbol("inexact->exact"), x}}}},
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
		needBase = true
		return &List{Elems: []Node{Symbol("current-jiffy")}}, nil
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
			end = &List{Elems: []Node{Symbol("length"), target}}
		}
		lenNode := &List{Elems: []Node{Symbol("-"), end, start}}
		if orig != nil {
			switch types.ExprType(&parser.Expr{Binary: &parser.BinaryExpr{Left: &parser.Unary{Value: &parser.PostfixExpr{Target: orig}}}}, currentEnv).(type) {
			case types.StringType:
				return &List{Elems: []Node{Symbol("substring"), target, start, end}}, nil
			case types.ListType:
				return &List{Elems: []Node{Symbol("take"), &List{Elems: []Node{Symbol("drop"), target, start}}, lenNode}}, nil
			}
		}
		return &List{Elems: []Node{
			Symbol("if"),
			&List{Elems: []Node{Symbol("string?"), target}},
			&List{Elems: []Node{Symbol("substring"), target, start, end}},
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
			return &List{Elems: []Node{Symbol("string-ref"), target, in}}, nil
		case types.ListType:
			return &List{Elems: []Node{Symbol("list-ref"), target, in}}, nil
		case types.MapType:
			needHash = true
			return &List{Elems: []Node{Symbol("hash-table-ref"), target, in}}, nil
		}
	}
	return &List{Elems: []Node{
		Symbol("cond"),
		&List{Elems: []Node{&List{Elems: []Node{Symbol("string?"), target}}, &List{Elems: []Node{Symbol("string-ref"), target, in}}}},
		&List{Elems: []Node{&List{Elems: []Node{Symbol("hash-table?"), target}}, &List{Elems: []Node{Symbol("hash-table-ref"), target, in}}}},
		&List{Elems: []Node{Symbol("else"), &List{Elems: []Node{Symbol("list-ref"), target, in}}}},
	}}, nil
}
