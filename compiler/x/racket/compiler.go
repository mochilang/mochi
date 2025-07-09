//go:build slow

package racket

import (
	"bytes"
	"fmt"
	"strings"

	"mochi/parser"
)

type Compiler struct {
	buf              bytes.Buffer
	needListLib      bool
	needJSONLib      bool
	needFuncLib      bool
	structs          map[string][]string
	structFieldTypes map[string]map[string]string
	varTypes         map[string]string
	funArity         map[string]int
}

func New() *Compiler {
	return &Compiler{
		structs:          make(map[string][]string),
		structFieldTypes: make(map[string]map[string]string),
		varTypes:         make(map[string]string),
		funArity:         make(map[string]int),
	}
}

func (c *Compiler) Compile(prog *parser.Program) ([]byte, error) {
	c.buf.Reset()
	c.needListLib = false
	for _, st := range prog.Statements {
		if err := c.compileStmt(st); err != nil {
			return nil, err
		}
	}
	var out bytes.Buffer
	out.WriteString("#lang racket\n")
	if c.needListLib {
		out.WriteString("(require racket/list)\n")
	}
	if c.needJSONLib {
		out.WriteString("(require json)\n")
	}
	if c.needFuncLib {
		out.WriteString("(require racket/function)\n")
	}
	out.Write(c.buf.Bytes())
	if out.Len() == 0 || out.Bytes()[out.Len()-1] != '\n' {
		out.WriteByte('\n')
	}
	return out.Bytes(), nil
}

func (c *Compiler) write(s string)   { c.buf.WriteString(s) }
func (c *Compiler) writeln(s string) { c.buf.WriteString(s); c.buf.WriteByte('\n') }

func indent(ok bool, depth int) string {
	if !ok {
		return ""
	}
	return strings.Repeat("  ", depth)
}

func (c *Compiler) compileStmt(s *parser.Statement) error {
	return c.compileStmtFull(s, "", "", "")
}

func (c *Compiler) compileStmtWithLabels(s *parser.Statement, breakLbl, contLbl string) error {
	return c.compileStmtFull(s, breakLbl, contLbl, "")
}

func (c *Compiler) compileStmtFull(s *parser.Statement, breakLbl, contLbl, retLbl string) error {
	switch {
	case s.Let != nil:
		name := s.Let.Name
		expr := "0"
		if s.Let.Value != nil {
			v, err := c.compileExpr(s.Let.Value)
			if err != nil {
				return err
			}
			expr = v
		}
		c.writeln(fmt.Sprintf("(define %s %s)", name, expr))
		if t := c.getDeclType(s.Let.Type); t != "" {
			c.varTypes[name] = t
		} else if t := getCastType(s.Let.Value); t != "" {
			c.varTypes[name] = t
		} else if t := getStructLiteralType(s.Let.Value); t != "" {
			c.varTypes[name] = t
		}
	case s.Assign != nil:
		if len(s.Assign.Index) > 0 {
			rhs, err := c.compileExpr(s.Assign.Value)
			if err != nil {
				return err
			}
			expr, err := c.compileIndexedSet(s.Assign.Name, s.Assign.Index, rhs)
			if err != nil {
				return err
			}
			c.writeln(fmt.Sprintf("(set! %s %s)", s.Assign.Name, expr))
			return nil
		}
		if len(s.Assign.Field) > 0 {
			if len(s.Assign.Field) != 1 {
				return fmt.Errorf("complex assignment not supported")
			}
			val, err := c.compileExpr(s.Assign.Value)
			if err != nil {
				return err
			}
			if t, ok := c.varTypes[s.Assign.Name]; ok && t != "" {
				field := s.Assign.Field[0].Name
				c.writeln(fmt.Sprintf("(set-%s-%s! %s %s)", t, field, s.Assign.Name, val))
			} else {
				return fmt.Errorf("unsupported statement")
			}
			return nil
		}
		val, err := c.compileExpr(s.Assign.Value)
		if err != nil {
			return err
		}
		c.writeln(fmt.Sprintf("(set! %s %s)", s.Assign.Name, val))
	case s.Var != nil:
		name := s.Var.Name
		expr := "0"
		if s.Var.Value != nil {
			v, err := c.compileExpr(s.Var.Value)
			if err != nil {
				return err
			}
			expr = v
		}
		c.writeln(fmt.Sprintf("(define %s %s)", name, expr))
		if t := c.getDeclType(s.Var.Type); t != "" {
			c.varTypes[name] = t
		} else if t := getCastType(s.Var.Value); t != "" {
			c.varTypes[name] = t
		} else if t := getStructLiteralType(s.Var.Value); t != "" {
			c.varTypes[name] = t
		}
	case s.Fun != nil:
		return c.compileFun(s.Fun)
	case s.Return != nil:
		val, err := c.compileExpr(s.Return.Value)
		if err != nil {
			return err
		}
		if retLbl != "" {
			c.writeln(fmt.Sprintf("(%s %s)", retLbl, val))
		} else {
			c.writeln(val)
		}
	case s.Expect != nil:
		cond, err := c.compileExpr(s.Expect.Value)
		if err != nil {
			return err
		}
		c.writeln(fmt.Sprintf("(when %s (displayln \"ok\"))", cond))
	case s.Test != nil:
		for _, st := range s.Test.Body {
			if err := c.compileStmtFull(st, breakLbl, contLbl, retLbl); err != nil {
				return err
			}
		}
	case s.If != nil:
		return c.compileIfWithLabelsFull(s.If, breakLbl, contLbl, retLbl)
	case s.While != nil:
		return c.compileWhileWithLabels(s.While, breakLbl, contLbl, retLbl)
	case s.Expr != nil:
		e, err := c.compileExpr(s.Expr.Expr)
		if err != nil {
			return err
		}
		c.writeln(e)
	case s.For != nil:
		return c.compileForWithLabelsFull(s.For, breakLbl, contLbl, retLbl)
	case s.Break != nil:
		if breakLbl == "" {
			return fmt.Errorf("unsupported statement")
		}
		c.writeln(fmt.Sprintf("(%s)", breakLbl))
	case s.Continue != nil:
		if contLbl == "" {
			return fmt.Errorf("unsupported statement")
		}
		c.writeln(fmt.Sprintf("(%s)", contLbl))
	case s.Type != nil:
		return c.compileTypeDecl(s.Type)
	default:
		return fmt.Errorf("unsupported statement")
	}
	return nil
}

func (c *Compiler) compileExpr(e *parser.Expr) (string, error) {
	if e == nil {
		return "", fmt.Errorf("nil expr")
	}
	val, err := c.compileUnary(e.Binary.Left)
	if err != nil {
		return "", err
	}
	isStrLeft := isStringUnary(e.Binary.Left)
	for _, op := range e.Binary.Right {
		rhs, err := c.compilePostfix(op.Right)
		if err != nil {
			return "", err
		}
		rightStr := isStringPostfix(op.Right)
		operator := op.Op
		switch operator {
		case "+":
			if isStrLeft || rightStr {
				val = fmt.Sprintf("(string-append %s %s)", val, rhs)
			} else {
				val = fmt.Sprintf("(+ %s %s)", val, rhs)
			}
		case "-", "*", "/":
			val = fmt.Sprintf("(%s %s %s)", operator, val, rhs)
		case "%":
			val = fmt.Sprintf("(remainder %s %s)", val, rhs)
		case "union":
			if op.All {
				val = fmt.Sprintf("(append %s %s)", val, rhs)
			} else {
				val = fmt.Sprintf("(remove-duplicates (append %s %s))", val, rhs)
			}
		case "except":
			val = fmt.Sprintf("(filter (lambda (x) (not (member x %s))) %s)", rhs, val)
		case "intersect":
			val = fmt.Sprintf("(filter (lambda (x) (member x %s)) %s)", rhs, val)
		case "==":
			if isStrLeft || rightStr {
				val = fmt.Sprintf("(string=? %s %s)", val, rhs)
			} else {
				val = fmt.Sprintf("(equal? %s %s)", val, rhs)
			}
		case "!=":
			if isStrLeft || rightStr {
				val = fmt.Sprintf("(not (string=? %s %s))", val, rhs)
			} else {
				val = fmt.Sprintf("(not (equal? %s %s))", val, rhs)
			}
		case "<", "<=", ">", ">=":
			cmpOp := operator
			if isStrLeft || rightStr {
				switch operator {
				case "<":
					cmpOp = "string<?"
				case "<=":
					cmpOp = "string<=?"
				case ">":
					cmpOp = "string>?"
				case ">=":
					cmpOp = "string>=?"
				}
			}
			val = fmt.Sprintf("(%s %s %s)", cmpOp, val, rhs)
		case "&&":
			val = fmt.Sprintf("(and %s %s)", val, rhs)
		case "||":
			val = fmt.Sprintf("(or %s %s)", val, rhs)
		case "in":
			c.needListLib = true
			val = fmt.Sprintf("(cond [(string? %s) (regexp-match? (regexp %s) %s)] [(hash? %s) (hash-has-key? %s %s)] [else (member %s %s)])", rhs, val, rhs, rhs, rhs, val, val, rhs)
		default:
			return "", fmt.Errorf("unsupported operator %s", operator)
		}
		isStrLeft = isStrLeft || rightStr
	}
	return val, nil
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
		switch {
		case op.Call != nil:
			// handle method calls like x.contains(y)
			args := make([]string, len(op.Call.Args))
			for i, a := range op.Call.Args {
				v, err := c.compileExpr(a)
				if err != nil {
					return "", err
				}
				args[i] = v
			}
			// special case for "contains" method on strings
			if p.Target.Selector != nil && len(p.Target.Selector.Tail) > 0 &&
				p.Target.Selector.Tail[len(p.Target.Selector.Tail)-1] == "contains" && len(args) == 1 {
				recvSel := &parser.Primary{Selector: &parser.SelectorExpr{
					Root: p.Target.Selector.Root,
					Tail: p.Target.Selector.Tail[:len(p.Target.Selector.Tail)-1],
				}}
				recv, err := c.compilePrimary(recvSel)
				if err != nil {
					return "", err
				}
				val = fmt.Sprintf("(regexp-match? (regexp %s) %s)", args[0], recv)
				continue
			}
			if len(args) == 0 {
				val = fmt.Sprintf("(%s)", val)
			} else {
				val = fmt.Sprintf("(%s %s)", val, strings.Join(args, " "))
			}
		case op.Cast != nil:
			if op.Cast.Type.Simple != nil {
				tname := *op.Cast.Type.Simple
				if tname == "int" {
					val = fmt.Sprintf("(string->number %s)", val)
				} else if fields, ok := c.structs[tname]; ok {
					parts := make([]string, len(fields))
					for i, f := range fields {
						parts[i] = fmt.Sprintf("(hash-ref %s \"%s\")", val, f)
					}
					val = fmt.Sprintf("(%s %s)", tname, strings.Join(parts, " "))
				} else {
					return "", fmt.Errorf("unsupported cast")
				}
			} else {
				return "", fmt.Errorf("unsupported cast")
			}
		case op.Index != nil:
			if op.Index.Colon != nil || op.Index.Colon2 != nil || op.Index.Step != nil {
				start := "0"
				if op.Index.Start != nil {
					s, err := c.compileExpr(op.Index.Start)
					if err != nil {
						return "", err
					}
					start = s
				}
				end := ""
				if op.Index.End != nil {
					e, err := c.compileExpr(op.Index.End)
					if err != nil {
						return "", err
					}
					end = e
				} else {
					end = fmt.Sprintf("(if (string? %s) (string-length %s) (length %s))", val, val, val)
				}
				c.needListLib = true
				val = fmt.Sprintf("(cond [(string? %s) (substring %s %s %s)] [(hash? %s) (hash-ref %s %s)] [else (take (drop %s %s) (- %s %s))])", val, val, start, end, val, val, start, val, start, end, start)
			} else {
				if op.Index.Start == nil {
					return "", fmt.Errorf("empty index")
				}
				idx, err := c.compileExpr(op.Index.Start)
				if err != nil {
					return "", err
				}
				c.needListLib = true
				val = fmt.Sprintf("(cond [(string? %s) (string-ref %s %s)] [(hash? %s) (hash-ref %s %s)] [else (list-ref %s %s)])", val, val, idx, val, val, idx, val, idx)
			}
		default:
			return "", fmt.Errorf("unsupported postfix operation")
		}
	}
	return val, nil
}

func (c *Compiler) compilePrimary(p *parser.Primary) (string, error) {
	switch {
	case p.Lit != nil:
		return c.compileLiteral(p.Lit)
	case p.Call != nil:
		args := make([]string, len(p.Call.Args))
		for i, a := range p.Call.Args {
			v, err := c.compileExpr(a)
			if err != nil {
				return "", err
			}
			args[i] = v
		}
		switch p.Call.Func {
		case "print":
			if len(args) == 0 {
				return "", fmt.Errorf("print expects at least 1 arg")
			}
			if len(args) == 1 {
				return fmt.Sprintf("(displayln %s)", args[0]), nil
			}
			return fmt.Sprintf("(displayln (string-join (map ~a (list %s)) \" \"))",
				strings.Join(args, " ")), nil
		case "append":
			if len(args) != 2 {
				return "", fmt.Errorf("append expects 2 args")
			}
			return fmt.Sprintf("(append %s (list %s))", args[0], args[1]), nil
		case "avg":
			if len(args) != 1 {
				return "", fmt.Errorf("avg expects 1 arg")
			}
			tmp := args[0]
			return fmt.Sprintf("(let ([xs %s]) (/ (apply + xs) (length xs)))", tmp), nil
		case "sum":
			if len(args) != 1 {
				return "", fmt.Errorf("sum expects 1 arg")
			}
			return fmt.Sprintf("(apply + %s)", args[0]), nil
		case "count":
			if len(args) != 1 {
				return "", fmt.Errorf("count expects 1 arg")
			}
			return fmt.Sprintf("(length %s)", args[0]), nil
		case "len":
			if len(args) != 1 {
				return "", fmt.Errorf("len expects 1 arg")
			}
			x := args[0]
			return fmt.Sprintf("(cond [(string? %s) (string-length %s)] [(hash? %s) (hash-count %s)] [else (length %s)])", x, x, x, x, x), nil
		case "min":
			if len(args) != 1 {
				return "", fmt.Errorf("min expects 1 arg")
			}
			return fmt.Sprintf("(apply min %s)", args[0]), nil
		case "max":
			if len(args) != 1 {
				return "", fmt.Errorf("max expects 1 arg")
			}
			return fmt.Sprintf("(apply max %s)", args[0]), nil
		case "values":
			if len(args) != 1 {
				return "", fmt.Errorf("values expects 1 arg")
			}
			return fmt.Sprintf("(hash-values %s)", args[0]), nil
		case "str":
			if len(args) != 1 {
				return "", fmt.Errorf("str expects 1 arg")
			}
			return fmt.Sprintf("(number->string %s)", args[0]), nil
		case "exists":
			if len(args) != 1 {
				return "", fmt.Errorf("exists expects 1 arg")
			}
			return fmt.Sprintf("(not (null? %s))", args[0]), nil
		case "json":
			if len(args) != 1 {
				return "", fmt.Errorf("json expects 1 arg")
			}
			c.needJSONLib = true
			return fmt.Sprintf("(displayln (jsexpr->string %s))", args[0]), nil
		default:
			if ar, ok := c.funArity[p.Call.Func]; ok && len(args) < ar {
				c.needFuncLib = true
				if len(args) == 0 {
					return p.Call.Func, nil
				}
				return fmt.Sprintf("(curry %s %s)", p.Call.Func, strings.Join(args, " ")), nil
			}
			return fmt.Sprintf("(%s %s)", p.Call.Func, strings.Join(args, " ")), nil
		}
	case p.If != nil:
		return c.compileIfExpr(p.If)
	case p.Selector != nil:
		val := p.Selector.Root
		t, _ := c.varTypes[val]
		for _, f := range p.Selector.Tail {
			if t != "" {
				val = fmt.Sprintf("(%s-%s %s)", t, f, val)
				if ft, ok := c.structFieldTypes[t][f]; ok {
					t = ft
				} else {
					t = ""
				}
			} else {
				val = fmt.Sprintf("(hash-ref %s '%s)", val, f)
			}
		}
		return val, nil
	case p.Group != nil:
		return c.compileExpr(p.Group)
	case p.List != nil:
		elems := make([]string, len(p.List.Elems))
		for i, e := range p.List.Elems {
			v, err := c.compileExpr(e)
			if err != nil {
				return "", err
			}
			elems[i] = v
		}
		return fmt.Sprintf("(list %s)", strings.Join(elems, " ")), nil
	case p.Map != nil:
		parts := make([]string, len(p.Map.Items)*2)
		for i, it := range p.Map.Items {
			k, err := c.compileExpr(it.Key)
			if err != nil {
				return "", err
			}
			if isIdentExpr(it.Key) {
				k = fmt.Sprintf("'%s", k)
			}
			v, err := c.compileExpr(it.Value)
			if err != nil {
				return "", err
			}
			parts[i*2] = k
			parts[i*2+1] = v
		}
		return fmt.Sprintf("(hash %s)", strings.Join(parts, " ")), nil
	case p.Struct != nil:
		parts := make([]string, len(p.Struct.Fields))
		for i, f := range p.Struct.Fields {
			v, err := c.compileExpr(f.Value)
			if err != nil {
				return "", err
			}
			parts[i] = v
		}
		return fmt.Sprintf("(%s %s)", p.Struct.Name, strings.Join(parts, " ")), nil
	case p.Query != nil:
		return c.compileQuery(p.Query)
	case p.Match != nil:
		return c.compileMatchExpr(p.Match)
	case p.FunExpr != nil:
		params := make([]string, len(p.FunExpr.Params))
		for i, pa := range p.FunExpr.Params {
			params[i] = pa.Name
		}
		if p.FunExpr.ExprBody != nil {
			body, err := c.compileExpr(p.FunExpr.ExprBody)
			if err != nil {
				return "", err
			}
			return fmt.Sprintf("(lambda (%s) %s)", strings.Join(params, " "), body), nil
		}
		var buf bytes.Buffer
		buf.WriteString(fmt.Sprintf("(lambda (%s)", strings.Join(params, " ")))
		for _, st := range p.FunExpr.BlockBody {
			if err := c.compileStmt(st); err != nil {
				return "", err
			}
		}
		buf.WriteString(")")
		return buf.String(), nil
	default:
		return "", fmt.Errorf("unsupported primary")
	}
}

func (c *Compiler) compileLiteral(l *parser.Literal) (string, error) {
	switch {
	case l.Int != nil:
		return fmt.Sprintf("%d", *l.Int), nil
	case l.Float != nil:
		return fmt.Sprintf("%v", *l.Float), nil
	case l.Bool != nil:
		if *l.Bool {
			return "#t", nil
		}
		return "#f", nil
	case l.Str != nil:
		return fmt.Sprintf("%q", *l.Str), nil
	case l.Null:
		return "'()", nil
	default:
		return "", fmt.Errorf("unknown literal")
	}
}

func (c *Compiler) compileFor(f *parser.ForStmt) error {
	return c.compileForWithLabelsFull(f, "", "", "")
}

func hasBC(sts []*parser.Statement) bool {
	for _, st := range sts {
		if st.Break != nil || st.Continue != nil {
			return true
		}
		if st.If != nil {
			if hasBC(st.If.Then) || hasBC(st.If.Else) {
				return true
			}
			if st.If.ElseIf != nil && hasBC(st.If.ElseIf.Then) {
				return true
			}
		}
	}
	return false
}

func (c *Compiler) compileForWithLabelsFull(f *parser.ForStmt, breakLbl, contLbl, retLbl string) error {
	name := f.Name
	start, err := c.compileExpr(f.Source)
	if err != nil {
		return err
	}
	end := ""
	if f.RangeEnd != nil {
		end, err = c.compileExpr(f.RangeEnd)
		if err != nil {
			return err
		}
	}
	useBC := hasBC(f.Body)
	if useBC {
		c.writeln("(let/ec break")
	}
	if end != "" {
		c.writeln(fmt.Sprintf("%s(for ([%s (in-range %s %s)])", indent(useBC, 1), name, start, end))
	} else {
		c.writeln(fmt.Sprintf("%s(for ([%s (if (hash? %s) (hash-keys %s) %s)])", indent(useBC, 1), name, start, start, start))
	}
	if useBC {
		c.writeln(fmt.Sprintf("%s(let/ec continue", indent(useBC, 2)))
	}
	for _, st := range f.Body {
		if useBC {
			if err := c.compileStmtFull(st, "break", "continue", retLbl); err != nil {
				return err
			}
		} else {
			if err := c.compileStmtFull(st, breakLbl, contLbl, retLbl); err != nil {
				return err
			}
		}
	}
	if useBC {
		c.writeln(fmt.Sprintf("%s)", indent(useBC, 2)))
		c.writeln(fmt.Sprintf("%s)", indent(useBC, 1)))
		c.writeln(")")
	} else {
		c.writeln(")")
	}
	return nil
}

func (c *Compiler) compileFun(f *parser.FunStmt) error {
	params := make([]string, len(f.Params))
	prev := c.varTypes
	c.varTypes = make(map[string]string)
	for k, v := range prev {
		c.varTypes[k] = v
	}
	for i, p := range f.Params {
		params[i] = p.Name
		if p.Type != nil && p.Type.Simple != nil {
			if _, ok := c.structs[*p.Type.Simple]; ok {
				c.varTypes[p.Name] = *p.Type.Simple
			}
		}
	}
	c.funArity[f.Name] = len(f.Params)
	c.writeln(fmt.Sprintf("(define (%s %s)", f.Name, strings.Join(params, " ")))
	c.writeln("  (let/ec return")
	for _, st := range f.Body {
		if err := c.compileStmtFull(st, "", "", "return"); err != nil {
			return err
		}
	}
	c.writeln("  ))")
	c.varTypes = prev
	return nil
}

func (c *Compiler) compileIf(st *parser.IfStmt) error {
	return c.compileIfWithLabelsFull(st, "", "", "")
}

func (c *Compiler) compileIfWithLabels(st *parser.IfStmt, breakLbl, contLbl string) error {
	return c.compileIfWithLabelsFull(st, breakLbl, contLbl, "")
}

func (c *Compiler) compileIfWithLabelsFull(st *parser.IfStmt, breakLbl, contLbl, retLbl string) error {
	cond, err := c.compileExpr(st.Cond)
	if err != nil {
		return err
	}
	c.writeln(fmt.Sprintf("(if %s", cond))
	c.writeln("  (begin")
	for _, s := range st.Then {
		if err := c.compileStmtFull(s, breakLbl, contLbl, retLbl); err != nil {
			return err
		}
	}
	c.writeln("  )")
	if st.ElseIf != nil {
		if err := c.compileIfWithLabelsFull(st.ElseIf, breakLbl, contLbl, retLbl); err != nil {
			return err
		}
	} else if len(st.Else) > 0 {
		c.writeln("  (begin")
		for _, s := range st.Else {
			if err := c.compileStmtFull(s, breakLbl, contLbl, retLbl); err != nil {
				return err
			}
		}
		c.writeln("  )")
	} else {
		c.writeln("  (void)")
	}
	c.writeln(")")
	return nil
}

func (c *Compiler) compileWhile(w *parser.WhileStmt) error {
	return c.compileWhileWithLabels(w, "", "", "")
}

func (c *Compiler) compileWhileWithLabels(w *parser.WhileStmt, breakLbl, contLbl, retLbl string) error {
	cond, err := c.compileExpr(w.Cond)
	if err != nil {
		return err
	}
	c.writeln("(let/ec break")
	c.writeln("  (let loop ()")
	c.writeln(fmt.Sprintf("    (when %s", cond))
	c.writeln("      (let/ec continue")
	for _, st := range w.Body {
		if err := c.compileStmtFull(st, "break", "continue", retLbl); err != nil {
			return err
		}
	}
	c.writeln("      )")
	c.writeln("      (loop)))")
	c.writeln(")")
	return nil
}

func (c *Compiler) compileIfExpr(ie *parser.IfExpr) (string, error) {
	cond, err := c.compileExpr(ie.Cond)
	if err != nil {
		return "", err
	}
	thn, err := c.compileExpr(ie.Then)
	if err != nil {
		return "", err
	}
	var els string
	if ie.ElseIf != nil {
		els, err = c.compileIfExpr(ie.ElseIf)
		if err != nil {
			return "", err
		}
	} else if ie.Else != nil {
		els, err = c.compileExpr(ie.Else)
		if err != nil {
			return "", err
		}
	} else {
		els = "(void)"
	}
	return fmt.Sprintf("(if %s %s %s)", cond, thn, els), nil
}

func (c *Compiler) compilePattern(e *parser.Expr) (string, error) {
	if e != nil && e.Binary != nil && len(e.Binary.Right) == 0 {
		u := e.Binary.Left
		if u != nil && len(u.Ops) == 0 && u.Value != nil && u.Value.Target != nil && u.Value.Target.Selector != nil {
			sel := u.Value.Target.Selector
			if len(sel.Tail) == 0 && sel.Root == "_" {
				return "_", nil
			}
		}
	}
	return c.compileExpr(e)
}

func (c *Compiler) compileMatchExpr(m *parser.MatchExpr) (string, error) {
	target, err := c.compileExpr(m.Target)
	if err != nil {
		return "", err
	}
	parts := make([]string, len(m.Cases))
	for i, cs := range m.Cases {
		pat, err := c.compilePattern(cs.Pattern)
		if err != nil {
			return "", err
		}
		res, err := c.compileExpr(cs.Result)
		if err != nil {
			return "", err
		}
		parts[i] = fmt.Sprintf("[%s %s]", pat, res)
	}
	return fmt.Sprintf("(match %s %s)", target, strings.Join(parts, " ")), nil
}

func (c *Compiler) compileQuery(q *parser.QueryExpr) (string, error) {
	loops := []string{}
	condParts := []string{}
	src, err := c.compileExpr(q.Source)
	if err != nil {
		return "", err
	}
	loops = append(loops, fmt.Sprintf("[%s %s]", q.Var, src))
	for _, f := range q.Froms {
		s, err := c.compileExpr(f.Src)
		if err != nil {
			return "", err
		}
		loops = append(loops, fmt.Sprintf("[%s %s]", f.Var, s))
	}
	bindings := []string{}
	for _, j := range q.Joins {
		js, err := c.compileExpr(j.Src)
		if err != nil {
			return "", err
		}
		onExpr, err := c.compileExpr(j.On)
		if err != nil {
			return "", err
		}
		if j.Side == nil {
			loops = append(loops, fmt.Sprintf("[%s %s]", j.Var, js))
			condParts = append(condParts, onExpr)
			continue
		}
		switch *j.Side {
		case "left":
			c.needListLib = true
			bindings = append(bindings, fmt.Sprintf("(%s (findf (lambda (%s) %s) %s))", j.Var, j.Var, onExpr, js))
		case "right":
			if len(q.Joins) == 1 && len(q.Froms) == 0 {
				c.needListLib = true
				loops = []string{fmt.Sprintf("[%s %s]", j.Var, js)}
				bindings = append(bindings, fmt.Sprintf("(%s (findf (lambda (%s) %s) %s))", q.Var, q.Var, onExpr, src))
			} else {
				return "", fmt.Errorf("unsupported join type")
			}
		default:
			return "", fmt.Errorf("unsupported join type")
		}
	}
	cond := ""
	if q.Where != nil {
		ce, err := c.compileExpr(q.Where)
		if err != nil {
			return "", err
		}
		condParts = append(condParts, ce)
	}
	if len(condParts) > 0 {
		cond = fmt.Sprintf(" #:when (and %s)", strings.Join(condParts, " "))
	}
	body, err := c.compileExpr(q.Select)
	if err != nil {
		return "", err
	}
	if len(bindings) > 0 {
		body = fmt.Sprintf("(let (%s) %s)", strings.Join(bindings, " "), body)
	}
	expr := fmt.Sprintf("(for*/list (%s%s) %s)", strings.Join(loops, " "), cond, body)
	return expr, nil
}

func (c *Compiler) compileIndexedSet(name string, idx []*parser.IndexOp, rhs string) (string, error) {
	if len(idx) == 0 {
		return rhs, nil
	}
	ie, err := c.compileExpr(idx[0].Start)
	if err != nil {
		return "", err
	}
	if len(idx) == 1 {
		c.needListLib = true
		return fmt.Sprintf("(cond [(hash? %s) (hash-set %s %s %s)] [else (list-set %s %s %s)])", name, name, ie, rhs, name, ie, rhs), nil
	}
	c.needListLib = true
	access := fmt.Sprintf("(cond [(hash? %s) (hash-ref %s %s)] [else (list-ref %s %s)])", name, name, ie, name, ie)
	inner, err := c.compileIndexedSet(access, idx[1:], rhs)
	if err != nil {
		return "", err
	}
	return fmt.Sprintf("(cond [(hash? %s) (hash-set %s %s %s)] [else (list-set %s %s %s)])", name, name, ie, inner, name, ie, inner), nil
}

func (c *Compiler) compileTypeDecl(t *parser.TypeDecl) error {
	if len(t.Variants) > 0 {
		return fmt.Errorf("unsupported statement")
	}
	fields := []string{}
	fieldTypes := map[string]string{}
	for _, m := range t.Members {
		if m.Field == nil {
			return fmt.Errorf("unsupported statement")
		}
		fields = append(fields, m.Field.Name)
		if m.Field.Type != nil && m.Field.Type.Simple != nil {
			fieldTypes[m.Field.Name] = *m.Field.Type.Simple
		}
	}
	c.structs[t.Name] = fields
	if len(fieldTypes) > 0 {
		c.structFieldTypes[t.Name] = fieldTypes
	}
	c.writeln(fmt.Sprintf("(struct %s (%s) #:transparent #:mutable)", t.Name, strings.Join(fields, " ")))
	return nil
}

func isStringPrimary(p *parser.Primary) bool {
	return p != nil && p.Lit != nil && p.Lit.Str != nil
}

func isStringPostfix(p *parser.PostfixExpr) bool {
	return isStringPrimary(p.Target)
}

func isStringUnary(u *parser.Unary) bool {
	return isStringPostfix(u.Value)
}

func isStringExpr(e *parser.Expr) bool {
	if e == nil || e.Binary == nil {
		return false
	}
	return isStringUnary(e.Binary.Left)
}

func isIdentExpr(e *parser.Expr) bool {
	if e == nil || e.Binary == nil || e.Binary.Left == nil {
		return false
	}
	u := e.Binary.Left
	if len(u.Ops) > 0 || u.Value == nil || u.Value.Target == nil || u.Value.Target.Selector == nil {
		return false
	}
	return len(u.Value.Target.Selector.Tail) == 0
}

func (c *Compiler) getDeclType(tr *parser.TypeRef) string {
	if tr == nil || tr.Simple == nil {
		return ""
	}
	if _, ok := c.structs[*tr.Simple]; ok {
		return *tr.Simple
	}
	return ""
}

func getCastType(e *parser.Expr) string {
	if e == nil || e.Binary == nil || len(e.Binary.Right) != 0 {
		return ""
	}
	u := e.Binary.Left
	if u == nil || u.Value == nil || len(u.Ops) != 0 {
		return ""
	}
	p := u.Value
	if p.Target == nil || len(p.Ops) == 0 {
		return ""
	}
	op := p.Ops[len(p.Ops)-1]
	if op.Cast == nil || op.Cast.Type == nil || op.Cast.Type.Simple == nil {
		return ""
	}
	return *op.Cast.Type.Simple
}

func getStructLiteralType(e *parser.Expr) string {
	if e == nil || e.Binary == nil || len(e.Binary.Right) != 0 {
		return ""
	}
	u := e.Binary.Left
	if u == nil || len(u.Ops) != 0 || u.Value == nil || u.Value.Target == nil {
		return ""
	}
	if u.Value.Target.Struct != nil {
		return u.Value.Target.Struct.Name
	}
	return ""
}
