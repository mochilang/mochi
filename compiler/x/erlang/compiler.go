//go:build slow

package erlang

import (
	"bytes"
	"fmt"
	"io/fs"
	"os"
	"path/filepath"
	"strings"

	"mochi/parser"
)

// Compiler translates a subset of Mochi programs into Erlang source code.
type Compiler struct {
	buf     bytes.Buffer
	indent  int
	srcPath string

	varVers map[string]int
	lets    map[string]bool
	types   map[string]string
	funs    map[string]int
}

func New(srcPath string) *Compiler {
	return &Compiler{srcPath: srcPath, varVers: make(map[string]int), lets: make(map[string]bool), types: make(map[string]string), funs: make(map[string]int)}
}

// CompileFile compiles the Mochi source file at path.
func CompileFile(path string) ([]byte, error) {
	prog, err := parser.Parse(path)
	if err != nil {
		return nil, err
	}
	c := New(path)
	return c.Compile(prog)
}

// WriteFile compiles srcPath and writes Erlang code to dstPath.
func WriteFile(srcPath, dstPath string) error {
	code, err := CompileFile(srcPath)
	if err != nil {
		return err
	}
	if err := os.MkdirAll(filepath.Dir(dstPath), 0o755); err != nil {
		return err
	}
	return os.WriteFile(dstPath, code, 0o755)
}

// repoRoot searches upward from the current directory to locate the repository root
// containing go.mod. An empty string is returned if not found.
func repoRoot() string {
	dir, err := os.Getwd()
	if err != nil {
		return ""
	}
	for i := 0; i < 10; i++ {
		if exists(filepath.Join(dir, "go.mod")) {
			return dir
		}
		parent := filepath.Dir(dir)
		if parent == dir {
			break
		}
		dir = parent
	}
	return ""
}

// ListPrograms returns all Mochi programs under tests/vm/valid.
func ListPrograms() ([]string, error) {
	root := repoRoot()
	if root == "" {
		return nil, fmt.Errorf("repo root not found")
	}
	pattern := filepath.Join(root, "tests", "vm", "valid", "*.mochi")
	return filepath.Glob(pattern)
}

func exists(path string) bool {
	_, err := os.Stat(path)
	return err == nil
}

func RemoveOldArtifacts(dir string) error {
	return filepath.WalkDir(dir, func(path string, d fs.DirEntry, err error) error {
		if err != nil {
			return err
		}
		if d.IsDir() {
			return nil
		}
		os.Remove(path)
		return nil
	})
}

// Compile emits Erlang source implementing prog.
func (c *Compiler) Compile(prog *parser.Program) ([]byte, error) {
	c.buf.Reset()
	c.indent = 0
	c.varVers = make(map[string]int)
	c.lets = make(map[string]bool)
	c.types = make(map[string]string)
	c.funs = make(map[string]int)

	base := strings.TrimSuffix(filepath.Base(c.srcPath), filepath.Ext(c.srcPath))
	c.writeln("#!/usr/bin/env escript")
	c.writeln(fmt.Sprintf("%% %s.erl - generated from %s", base, filepath.Base(c.srcPath)))
	c.writeln("")

	var body []*parser.Statement
	for _, st := range prog.Statements {
		if st.Fun != nil {
			if err := c.compileFun(st.Fun); err != nil {
				return nil, err
			}
			c.writeln("")
			continue
		}
		body = append(body, st)
	}

	c.writeln("main(_) ->")
	c.indent++

	var lines []string
	for _, st := range body {
		l, err := c.compileStmtLine(st)
		if err != nil {
			return nil, err
		}
		lines = append(lines, l)
	}

	for i, l := range lines {
		if i == len(lines)-1 {
			c.writeln(l + ".")
		} else {
			c.writeln(l + ",")
		}
	}

	c.indent--
	return c.buf.Bytes(), nil
}

func (c *Compiler) compileLet(l *parser.LetStmt) (string, error) {
	c.lets[l.Name] = true
	val := "undefined"
	if l.Value != nil {
		v, err := c.compileExpr(l.Value)
		if err != nil {
			return "", err
		}
		val = v
		if t := c.inferExprType(l.Value); t != "" {
			c.types[l.Name] = t
		}
	}
	return fmt.Sprintf("%s = %s", capitalize(l.Name), val), nil
}

func (c *Compiler) compileVar(v *parser.VarStmt) (string, error) {
	name := c.newVarName(v.Name)
	val := "undefined"
	if v.Value != nil {
		s, err := c.compileExpr(v.Value)
		if err != nil {
			return "", err
		}
		val = s
		if t := c.inferExprType(v.Value); t != "" {
			c.types[v.Name] = t
		}
	}
	return fmt.Sprintf("%s = %s", name, val), nil
}

func (c *Compiler) compileAssign(a *parser.AssignStmt) (string, error) {
	if len(a.Field) > 0 || len(a.Index) > 2 {
		return "", fmt.Errorf("complex assignment not supported")
	}

	if len(a.Index) == 2 {
		a0 := a.Index[0]
		a1 := a.Index[1]
		if a0.Start == nil || a0.Colon != nil || a0.End != nil || a0.Colon2 != nil || a0.Step != nil {
			return "", fmt.Errorf("complex indexing not supported")
		}
		if a1.Start == nil || a1.Colon != nil || a1.End != nil || a1.Colon2 != nil || a1.Step != nil {
			return "", fmt.Errorf("complex indexing not supported")
		}
		idx0, err := c.compileExpr(a0.Start)
		if err != nil {
			return "", err
		}
		idx1, err := c.compileExpr(a1.Start)
		if err != nil {
			return "", err
		}
		val, err := c.compileExpr(a.Value)
		if err != nil {
			return "", err
		}
		prev := c.refVar(a.Name)
		inner := c.newVarName(a.Name + "Inner")
		updated := c.newVarName(a.Name + "InnerUpd")
		name := c.newVarName(a.Name)
		typ := c.types[a.Name]
		if typ == "map" || (!isIntLiteral(a0.Start)) {
			c.types[a.Name] = "map"
			return fmt.Sprintf("%s = maps:get(%s, %s), %s = maps:put(%s, %s, %s), %s = %s#{%s => %s}", inner, idx0, prev, updated, idx1, val, inner, name, prev, idx0, updated), nil
		}
		// list of lists
		return fmt.Sprintf("%s = lists:nth((%s)+1, %s), %s = lists:sublist(%s, %s) ++ [%s] ++ lists:nthtail((%s)+1, %s), %s = lists:sublist(%s, %s) ++ [%s] ++ lists:nthtail((%s)+1, %s)", inner, idx0, prev, updated, inner, idx1, val, idx1, inner, name, prev, idx0, updated, idx0, prev), nil
	}
	if len(a.Index) == 1 {
		idxOp := a.Index[0]
		if idxOp.Start == nil || idxOp.Colon != nil || idxOp.End != nil || idxOp.Colon2 != nil || idxOp.Step != nil {
			return "", fmt.Errorf("complex indexing not supported")
		}
		idx, err := c.compileExpr(idxOp.Start)
		if err != nil {
			return "", err
		}
		val, err := c.compileExpr(a.Value)
		if err != nil {
			return "", err
		}
		prev := c.refVar(a.Name)
		name := c.newVarName(a.Name)
		typ := c.types[a.Name]
		switch typ {
		case "map":
			c.types[a.Name] = "map"
			return fmt.Sprintf("%s = maps:put(%s, %s, %s)", name, idx, val, prev), nil
		case "list":
			c.types[a.Name] = "list"
			return fmt.Sprintf("%s = lists:sublist(%s, %s) ++ [%s] ++ lists:nthtail((%s)+1, %s)", name, prev, idx, val, idx, prev), nil
		default:
			if isIntLiteral(idxOp.Start) {
				return fmt.Sprintf("%s = lists:sublist(%s, %s) ++ [%s] ++ lists:nthtail((%s)+1, %s)", name, prev, idx, val, idx, prev), nil
			}
			return fmt.Sprintf("%s = maps:put(%s, %s, %s)", name, idx, val, prev), nil
		}
	}

	val, err := c.compileExpr(a.Value)
	if err != nil {
		return "", err
	}
	if t := c.inferExprType(a.Value); t != "" {
		c.types[a.Name] = t
	}
	name := c.newVarName(a.Name)
	return fmt.Sprintf("%s = %s", name, val), nil
}

func (c *Compiler) compileStmtLine(st *parser.Statement) (string, error) {
	switch {
	case st.Let != nil:
		return c.compileLet(st.Let)
	case st.Var != nil:
		return c.compileVar(st.Var)
	case st.Assign != nil:
		return c.compileAssign(st.Assign)
	case st.Break != nil:
		return c.compileBreak(st.Break)
	case st.Continue != nil:
		return c.compileContinue(st.Continue)
	case st.Return != nil:
		return c.compileReturn(st.Return)
	case st.Expr != nil:
		return c.compileExpr(st.Expr.Expr)
	case st.If != nil:
		return c.compileIfStmt(st.If)
	case st.For != nil:
		return c.compileFor(st.For)
	case st.Fun != nil:
		return c.compileNestedFun(st.Fun)
	case st.While != nil:
		return "", fmt.Errorf("unsupported statement at line %d", st.Pos.Line)
	case st.Type != nil:
		return "", nil
	default:
		return "", fmt.Errorf("unsupported statement at line %d", st.Pos.Line)
	}
}

func (c *Compiler) compileFun(fn *parser.FunStmt) error {
	params := make([]string, len(fn.Params))
	for i, p := range fn.Params {
		params[i] = capitalize(p.Name)
	}
	c.funs[fn.Name] = len(fn.Params)
	c.writeln(fmt.Sprintf("%s(%s) ->", fn.Name, strings.Join(params, ", ")))
	c.indent++
	var lines []string
	for _, st := range fn.Body {
		l, err := c.compileStmtLine(st)
		if err != nil {
			return err
		}
		lines = append(lines, l)
	}
	for i, l := range lines {
		if i == len(lines)-1 {
			c.writeln(l + ".")
		} else {
			c.writeln(l + ",")
		}
	}
	c.indent--
	return nil
}

func (c *Compiler) compileReturn(ret *parser.ReturnStmt) (string, error) {
	val, err := c.compileExpr(ret.Value)
	if err != nil {
		return "", err
	}
	return val, nil
}

func (c *Compiler) compileBreak(_ *parser.BreakStmt) (string, error) {
	return "throw(break)", nil
}

func (c *Compiler) compileContinue(_ *parser.ContinueStmt) (string, error) {
	return "throw(continue)", nil
}

func (c *Compiler) compileIfStmt(ifst *parser.IfStmt) (string, error) {
	cond, err := c.compileExpr(ifst.Cond)
	if err != nil {
		return "", err
	}
	thenLines := make([]string, len(ifst.Then))
	for i, s := range ifst.Then {
		l, err := c.compileStmtLine(s)
		if err != nil {
			return "", err
		}
		thenLines[i] = l
	}
	thenCode := strings.Join(thenLines, ", ")
	if thenCode == "" {
		thenCode = "ok"
	}
	elseCode := "ok"
	if ifst.ElseIf != nil {
		ec, err := c.compileIfStmt(ifst.ElseIf)
		if err != nil {
			return "", err
		}
		elseCode = ec
	} else if len(ifst.Else) > 0 {
		parts := make([]string, len(ifst.Else))
		for i, s := range ifst.Else {
			l, err := c.compileStmtLine(s)
			if err != nil {
				return "", err
			}
			parts[i] = l
		}
		elseCode = strings.Join(parts, ", ")
		if elseCode == "" {
			elseCode = "ok"
		}
	}
	return fmt.Sprintf("(if %s -> %s; true -> %s end)", cond, thenCode, elseCode), nil
}

func (c *Compiler) compileFor(fr *parser.ForStmt) (string, error) {
	bodyParts := make([]string, len(fr.Body))
	for i, s := range fr.Body {
		l, err := c.compileStmtLine(s)
		if err != nil {
			return "", err
		}
		bodyParts[i] = l
	}
	body := strings.Join(bodyParts, ", ")
	if body == "" {
		body = "ok"
	}
	hasBC := hasBreakOrContinue(fr.Body)
	iterBody := body
	if hasBC {
		iterBody = fmt.Sprintf("try %s catch throw:continue -> ok end", body)
	}
	if fr.RangeEnd != nil {
		start, err := c.compileExpr(fr.Source)
		if err != nil {
			return "", err
		}
		end, err := c.compileExpr(fr.RangeEnd)
		if err != nil {
			return "", err
		}
		loop := fmt.Sprintf("lists:foreach(fun(%s) -> %s end, lists:seq(%s, (%s)-1))", capitalize(fr.Name), iterBody, start, end)
		if hasBC {
			loop = fmt.Sprintf("try %s catch throw:break -> ok end", loop)
		}
		return loop, nil
	}
	src, err := c.compileExpr(fr.Source)
	if err != nil {
		return "", err
	}
	typ := c.inferExprType(fr.Source)
	if typ == "" && fr.Source.Binary != nil {
		u := fr.Source.Binary.Left
		if u.Value != nil && u.Value.Target.Selector != nil {
			typ = c.types[u.Value.Target.Selector.Root]
		}
	}
	var loop string
	if typ == "map" {
		loop = fmt.Sprintf("lists:foreach(fun({%s,_}) -> %s end, maps:to_list(%s))", capitalize(fr.Name), iterBody, src)
	} else {
		loop = fmt.Sprintf("lists:foreach(fun(%s) -> %s end, %s)", capitalize(fr.Name), iterBody, src)
	}
	if hasBC {
		loop = fmt.Sprintf("try %s catch throw:break -> ok end", loop)
	}
	return loop, nil
}

func (c *Compiler) compileNestedFun(fn *parser.FunStmt) (string, error) {
	params := make([]string, len(fn.Params))
	for i, p := range fn.Params {
		params[i] = capitalize(p.Name)
	}
	c.lets[fn.Name] = true
	bodyLines := make([]string, len(fn.Body))
	for i, st := range fn.Body {
		l, err := c.compileStmtLine(st)
		if err != nil {
			return "", err
		}
		bodyLines[i] = l
	}
	body := strings.Join(bodyLines, ", ")
	if body == "" {
		body = "ok"
	}
	name := capitalize(fn.Name)
	return fmt.Sprintf("%s = fun(%s) -> %s end", name, strings.Join(params, ", "), body), nil
}

func (c *Compiler) compileIfExpr(ix *parser.IfExpr) (string, error) {
	cond, err := c.compileExpr(ix.Cond)
	if err != nil {
		return "", err
	}
	thenExpr, err := c.compileExpr(ix.Then)
	if err != nil {
		return "", err
	}
	elseExpr := "undefined"
	if ix.ElseIf != nil {
		elseExpr, err = c.compileIfExpr(ix.ElseIf)
		if err != nil {
			return "", err
		}
	} else if ix.Else != nil {
		elseExpr, err = c.compileExpr(ix.Else)
		if err != nil {
			return "", err
		}
	}
	return fmt.Sprintf("(case %s of true -> %s; _ -> %s end)", cond, thenExpr, elseExpr), nil
}

func (c *Compiler) compileMatchExpr(mx *parser.MatchExpr) (string, error) {
	target, err := c.compileExpr(mx.Target)
	if err != nil {
		return "", err
	}
	parts := make([]string, len(mx.Cases))
	for i, cs := range mx.Cases {
		pat, err := c.compileExpr(cs.Pattern)
		if err != nil {
			return "", err
		}
		res, err := c.compileExpr(cs.Result)
		if err != nil {
			return "", err
		}
		parts[i] = fmt.Sprintf("%s -> %s", pat, res)
	}
	return fmt.Sprintf("(case %s of %s end)", target, strings.Join(parts, "; ")), nil
}

func (c *Compiler) compileQuery(q *parser.QueryExpr) (string, error) {
	gens := []string{}
	src, err := c.compileExpr(q.Source)
	if err != nil {
		return "", err
	}
	gens = append(gens, fmt.Sprintf("%s <- %s", capitalize(q.Var), src))
	for _, fr := range q.Froms {
		s, err := c.compileExpr(fr.Src)
		if err != nil {
			return "", err
		}
		gens = append(gens, fmt.Sprintf("%s <- %s", capitalize(fr.Var), s))
	}

	cond := ""
	if q.Where != nil {
		cnd, err := c.compileExpr(q.Where)
		if err != nil {
			return "", err
		}
		cond = cnd
	}

	sel, err := c.compileExpr(q.Select)
	if err != nil {
		return "", err
	}

	listExpr := "[" + sel + " || " + strings.Join(gens, ", ")
	if cond != "" {
		listExpr += ", " + cond
	}
	listExpr += "]"

	res := listExpr

	if q.Sort != nil {
		key, err := c.compileExpr(q.Sort)
		if err != nil {
			return "", err
		}
		pairList := "[{" + key + ", " + sel + "} || " + strings.Join(gens, ", ")
		if cond != "" {
			pairList += ", " + cond
		}
		pairList += "]"
		sorted := fmt.Sprintf("lists:keysort(1, %s)", pairList)
		res = fmt.Sprintf("[V || {_, V} <- %s]", sorted)
	}

	if q.Skip != nil {
		s, err := c.compileExpr(q.Skip)
		if err != nil {
			return "", err
		}
		res = fmt.Sprintf("lists:nthtail(%s, %s)", s, res)
	}
	if q.Take != nil {
		t, err := c.compileExpr(q.Take)
		if err != nil {
			return "", err
		}
		res = fmt.Sprintf("lists:sublist(%s, %s)", res, t)
	}

	return res, nil
}

func (c *Compiler) compileFunExpr(fn *parser.FunExpr) (string, error) {
	params := make([]string, len(fn.Params))
	for i, p := range fn.Params {
		params[i] = capitalize(p.Name)
	}
	if fn.ExprBody != nil {
		body, err := c.compileExpr(fn.ExprBody)
		if err != nil {
			return "", err
		}
		return fmt.Sprintf("fun(%s) -> %s end", strings.Join(params, ", "), body), nil
	}
	return "", fmt.Errorf("block function expressions not supported")
}

func (c *Compiler) compileExpr(e *parser.Expr) (string, error) {
	if e == nil {
		return "", fmt.Errorf("nil expr")
	}
	return c.compileBinary(e.Binary)
}

func (c *Compiler) compileBinary(b *parser.BinaryExpr) (string, error) {
	left, err := c.compileUnary(b.Left)
	if err != nil {
		return "", err
	}
	res := left
	leftIsStr := isStringUnary(b.Left)
	for _, op := range b.Right {
		r, err := c.compilePostfix(op.Right)
		if err != nil {
			return "", err
		}
		switch op.Op {
		case "in":
			typ := c.typeOfPostfix(op.Right)
			switch typ {
			case "map":
				res = fmt.Sprintf("maps:is_key(%s, %s)", res, r)
			case "string":
				res = fmt.Sprintf("string:str(%s, %s) > 0", r, res)
			default:
				res = fmt.Sprintf("lists:member(%s, %s)", res, r)
			}
		case "union":
			if op.All {
				res = fmt.Sprintf("(%s ++ %s)", res, r)
			} else {
				res = fmt.Sprintf("ordsets:union(%s, %s)", res, r)
			}
		case "except":
			res = fmt.Sprintf("ordsets:subtract(%s, %s)", res, r)
		case "intersect":
			res = fmt.Sprintf("ordsets:intersection(%s, %s)", res, r)
		default:
			opStr, err := c.mapBinOp(op.Op)
			if err != nil {
				return "", err
			}
			rightIsStr := isStringPostfix(op.Right)
			if op.Op == "+" && (leftIsStr || rightIsStr) {
				opStr = "++"
				leftIsStr = true
			} else {
				leftIsStr = false
			}
			res = fmt.Sprintf("(%s %s %s)", res, opStr, r)
		}
	}
	return res, nil
}

func (c *Compiler) mapBinOp(op string) (string, error) {
	switch op {
	case "+", "-", "*", "/":
		return op, nil
	case "%":
		return "rem", nil
	case "==":
		return "==", nil
	case "!=":
		return "/=", nil
	case "<", "<=", ">", ">=":
		return op, nil
	case "&&":
		return "andalso", nil
	case "||":
		return "orelse", nil
	default:
		return "", fmt.Errorf("unsupported operator %s", op)
	}
}

func (c *Compiler) compileUnary(u *parser.Unary) (string, error) {
	val, err := c.compilePostfix(u.Value)
	if err != nil {
		return "", err
	}
	for i := len(u.Ops) - 1; i >= 0; i-- {
		switch u.Ops[i] {
		case "-":
			val = "-" + val
		case "!":
			val = "not " + val
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
	typ := c.typeOfPrimary(p.Target)
	for _, op := range p.Ops {
		switch {
		case op.Index != nil:
			idxOp := op.Index
			if idxOp.Colon != nil || idxOp.End != nil || idxOp.Colon2 != nil || idxOp.Step != nil {
				start := "0"
				if idxOp.Start != nil {
					s, err := c.compileExpr(idxOp.Start)
					if err != nil {
						return "", err
					}
					start = s
				}
				end := "length(" + val + ")"
				if idxOp.End != nil {
					e, err := c.compileExpr(idxOp.End)
					if err != nil {
						return "", err
					}
					end = e
				}
				if typ == "string" {
					val = fmt.Sprintf("string:substr(%s, (%s)+1, (%s)-(%s))", val, start, end, start)
				} else {
					val = fmt.Sprintf("lists:sublist(%s, (%s)+1, (%s)-(%s))", val, start, end, start)
				}
			} else {
				if idxOp.Start == nil {
					return "", fmt.Errorf("complex indexing not supported")
				}
				idx, err := c.compileExpr(idxOp.Start)
				if err != nil {
					return "", err
				}
				if typ == "map" || (typ == "" && !isIntLiteral(idxOp.Start)) {
					val = fmt.Sprintf("maps:get(%s, %s)", idx, val)
				} else {
					val = fmt.Sprintf("lists:nth((%s)+1, %s)", idx, val)
				}
			}
			typ = ""
		case op.Cast != nil:
			if op.Cast.Type != nil && op.Cast.Type.Simple != nil {
				t := *op.Cast.Type.Simple
				if t == "int" && typ == "string" {
					val = fmt.Sprintf("list_to_integer(%s)", val)
					typ = "int"
				}
			}
		default:
			return "", fmt.Errorf("unsupported postfix at line %d", p.Target.Pos.Line)
		}
	}
	return val, nil
}

func (c *Compiler) compilePrimary(p *parser.Primary) (string, error) {
	switch {
	case p.Lit != nil:
		return c.compileLiteral(p.Lit), nil
	case p.Selector != nil:
		expr := c.refVar(p.Selector.Root)
		for _, f := range p.Selector.Tail {
			expr = fmt.Sprintf("maps:get(%s, %s)", f, expr)
		}
		return expr, nil
	case p.Query != nil:
		return c.compileQuery(p.Query)
	case p.Call != nil:
		return c.compileCall(p.Call)
	case p.FunExpr != nil:
		return c.compileFunExpr(p.FunExpr)
	case p.If != nil:
		return c.compileIfExpr(p.If)
	case p.Match != nil:
		return c.compileMatchExpr(p.Match)
	case p.List != nil:
		elems := make([]string, len(p.List.Elems))
		for i, e := range p.List.Elems {
			s, err := c.compileExpr(e)
			if err != nil {
				return "", err
			}
			elems[i] = s
		}
		return "[" + strings.Join(elems, ", ") + "]", nil
	case p.Map != nil:
		parts := make([]string, len(p.Map.Items))
		for i, it := range p.Map.Items {
			k, err := c.compileMapKey(it.Key)
			if err != nil {
				return "", err
			}
			v, err := c.compileExpr(it.Value)
			if err != nil {
				return "", err
			}
			parts[i] = fmt.Sprintf("%s => %s", k, v)
		}
		return "#{" + strings.Join(parts, ", ") + "}", nil
	case p.Group != nil:
		expr, err := c.compileExpr(p.Group)
		if err != nil {
			return "", err
		}
		return fmt.Sprintf("(%s)", expr), nil
	default:
		return "", fmt.Errorf("unsupported expression at line %d", p.Pos.Line)
	}
}

func (c *Compiler) compileCall(call *parser.CallExpr) (string, error) {
	switch call.Func {
	case "print":
		if len(call.Args) == 0 {
			return "", fmt.Errorf("print expects at least one argument")
		}
		parts := make([]string, len(call.Args))
		args := make([]string, len(call.Args))
		for i, a := range call.Args {
			s, err := c.compileExpr(a)
			if err != nil {
				return "", err
			}
			parts[i] = "~p"
			args[i] = s
		}
		fmtStr := strings.Join(parts, " ") + "~n"
		return fmt.Sprintf("io:format(\"%s\", [%s])", fmtStr, strings.Join(args, ", ")), nil
	case "len":
		if len(call.Args) != 1 {
			return "", fmt.Errorf("len expects 1 argument")
		}
		argExpr := call.Args[0]
		arg, err := c.compileExpr(argExpr)
		if err != nil {
			return "", err
		}
		if isMapLiteralExpr(argExpr) {
			return fmt.Sprintf("maps:size(%s)", arg), nil
		}
		return fmt.Sprintf("length(%s)", arg), nil
	case "append":
		if len(call.Args) != 2 {
			return "", fmt.Errorf("append expects 2 args")
		}
		a0, err := c.compileExpr(call.Args[0])
		if err != nil {
			return "", err
		}
		a1, err := c.compileExpr(call.Args[1])
		if err != nil {
			return "", err
		}
		return fmt.Sprintf("%s ++ [%s]", a0, a1), nil
	case "avg":
		if len(call.Args) != 1 {
			return "", fmt.Errorf("avg expects 1 arg")
		}
		a0, err := c.compileExpr(call.Args[0])
		if err != nil {
			return "", err
		}
		return fmt.Sprintf("(lists:sum(%s) / length(%s))", a0, a0), nil
	case "count":
		if len(call.Args) != 1 {
			return "", fmt.Errorf("count expects 1 arg")
		}
		a0, err := c.compileExpr(call.Args[0])
		if err != nil {
			return "", err
		}
		return fmt.Sprintf("length(%s)", a0), nil
	case "sum":
		if len(call.Args) != 1 {
			return "", fmt.Errorf("sum expects 1 arg")
		}
		a0, err := c.compileExpr(call.Args[0])
		if err != nil {
			return "", err
		}
		return fmt.Sprintf("lists:sum(%s)", a0), nil
	case "min":
		if len(call.Args) != 1 {
			return "", fmt.Errorf("min expects 1 arg")
		}
		a0, err := c.compileExpr(call.Args[0])
		if err != nil {
			return "", err
		}
		return fmt.Sprintf("lists:min(%s)", a0), nil
	case "max":
		if len(call.Args) != 1 {
			return "", fmt.Errorf("max expects 1 arg")
		}
		a0, err := c.compileExpr(call.Args[0])
		if err != nil {
			return "", err
		}
		return fmt.Sprintf("lists:max(%s)", a0), nil
	case "values":
		if len(call.Args) != 1 {
			return "", fmt.Errorf("values expects 1 arg")
		}
		a0, err := c.compileExpr(call.Args[0])
		if err != nil {
			return "", err
		}
		return fmt.Sprintf("maps:values(%s)", a0), nil
	case "substring":
		if len(call.Args) != 3 {
			return "", fmt.Errorf("substring expects 3 args")
		}
		str, err := c.compileExpr(call.Args[0])
		if err != nil {
			return "", err
		}
		start, err := c.compileExpr(call.Args[1])
		if err != nil {
			return "", err
		}
		end, err := c.compileExpr(call.Args[2])
		if err != nil {
			return "", err
		}
		return fmt.Sprintf("string:substr(%s, (%s)+1, (%s)-(%s))", str, start, end, start), nil
	default:
		args := make([]string, len(call.Args))
		for i, a := range call.Args {
			s, err := c.compileExpr(a)
			if err != nil {
				return "", err
			}
			args[i] = s
		}
		name := call.Func
		if ar, ok := c.funs[name]; ok && len(args) < ar {
			missing := ar - len(args)
			params := make([]string, missing)
			for i := 0; i < missing; i++ {
				params[i] = fmt.Sprintf("P%d", i)
			}
			callArgs := append(append([]string{}, args...), params...)
			return fmt.Sprintf("fun(%s) -> %s(%s) end", strings.Join(params, ", "), name, strings.Join(callArgs, ", ")), nil
		}
		if c.lets[name] || c.varVers[name] > 0 {
			name = c.refVar(name)
		}
		return fmt.Sprintf("%s(%s)", name, strings.Join(args, ", ")), nil
	}
}

func (c *Compiler) compileLiteral(l *parser.Literal) string {
	switch {
	case l.Int != nil:
		return fmt.Sprintf("%d", *l.Int)
	case l.Float != nil:
		return fmt.Sprintf("%g", *l.Float)
	case l.Bool != nil:
		if bool(*l.Bool) {
			return "true"
		}
		return "false"
	case l.Str != nil:
		return fmt.Sprintf("%q", *l.Str)
	case l.Null:
		return "undefined"
	default:
		return "undefined"
	}
}

func (c *Compiler) compileMapKey(e *parser.Expr) (string, error) {
	if e != nil && e.Binary != nil && len(e.Binary.Right) == 0 {
		u := e.Binary.Left
		if len(u.Ops) == 0 && u.Value != nil && len(u.Value.Ops) == 0 {
			if u.Value.Target.Selector != nil && len(u.Value.Target.Selector.Tail) == 0 {
				return u.Value.Target.Selector.Root, nil
			}
		}
	}
	return c.compileExpr(e)
}

func (c *Compiler) writeln(s string) {
	for i := 0; i < c.indent; i++ {
		c.buf.WriteString("    ")
	}
	c.buf.WriteString(s)
	c.buf.WriteByte('\n')
}

func (c *Compiler) newVarName(base string) string {
	idx := c.varVers[base]
	name := fmt.Sprintf("%s%d", capitalize(base), idx)
	c.varVers[base] = idx + 1
	return name
}

func (c *Compiler) refVar(base string) string {
	if c.lets[base] {
		return capitalize(base)
	}
	idx := c.varVers[base]
	if idx == 0 {
		return capitalize(base)
	}
	return fmt.Sprintf("%s%d", capitalize(base), idx-1)
}

func capitalize(s string) string {
	if s == "" {
		return s
	}
	return strings.ToUpper(s[:1]) + s[1:]
}

func hasBreakOrContinue(sts []*parser.Statement) bool {
	for _, st := range sts {
		switch {
		case st.Break != nil, st.Continue != nil:
			return true
		case st.If != nil:
			if hasBreakOrContinue(st.If.Then) || hasBreakOrContinue(st.If.Else) {
				return true
			}
			if st.If.ElseIf != nil {
				if hasBreakOrContinue(st.If.ElseIf.Then) || hasBreakOrContinue(st.If.ElseIf.Else) {
					return true
				}
			}
		case st.For != nil:
			if hasBreakOrContinue(st.For.Body) {
				return true
			}
		case st.While != nil:
			if hasBreakOrContinue(st.While.Body) {
				return true
			}
		}
	}
	return false
}

func isIntLiteral(e *parser.Expr) bool {
	if e == nil || e.Binary == nil || len(e.Binary.Right) > 0 {
		return false
	}
	u := e.Binary.Left
	if len(u.Ops) > 0 || u.Value == nil || len(u.Value.Ops) > 0 {
		return false
	}
	lit := u.Value.Target.Lit
	return lit != nil && lit.Int != nil
}

func isMapLiteralExpr(e *parser.Expr) bool {
	if e == nil || e.Binary == nil || len(e.Binary.Right) > 0 {
		return false
	}
	u := e.Binary.Left
	if len(u.Ops) > 0 || u.Value == nil || len(u.Value.Ops) > 0 {
		return false
	}
	return u.Value.Target.Map != nil
}

func isListLiteralExpr(e *parser.Expr) bool {
	if e == nil || e.Binary == nil || len(e.Binary.Right) > 0 {
		return false
	}
	u := e.Binary.Left
	if len(u.Ops) > 0 || u.Value == nil || len(u.Value.Ops) > 0 {
		return false
	}
	return u.Value.Target.List != nil
}

func isStringLiteralExpr(e *parser.Expr) bool {
	if e == nil || e.Binary == nil || len(e.Binary.Right) > 0 {
		return false
	}
	u := e.Binary.Left
	if len(u.Ops) > 0 || u.Value == nil || len(u.Value.Ops) > 0 {
		return false
	}
	lit := u.Value.Target.Lit
	return lit != nil && lit.Str != nil
}

func isLiteralExpr(e *parser.Expr) bool {
	if e == nil || e.Binary == nil || len(e.Binary.Right) > 0 {
		return false
	}
	u := e.Binary.Left
	if len(u.Ops) > 0 || u.Value == nil || len(u.Value.Ops) > 0 {
		return false
	}
	return u.Value.Target.Lit != nil
}

func (c *Compiler) inferExprType(e *parser.Expr) string {
	switch {
	case isMapLiteralExpr(e):
		return "map"
	case isListLiteralExpr(e):
		return "list"
	case isStringLiteralExpr(e):
		return "string"
	default:
		return ""
	}
}

func (c *Compiler) typeOfPrimary(p *parser.Primary) string {
	switch {
	case p.Map != nil:
		return "map"
	case p.List != nil:
		return "list"
	case p.Lit != nil && p.Lit.Str != nil:
		return "string"
	case p.Selector != nil:
		return c.types[p.Selector.Root]
	default:
		return ""
	}
}

func (c *Compiler) typeOfPostfix(p *parser.PostfixExpr) string {
	if len(p.Ops) > 0 {
		return ""
	}
	return c.typeOfPrimary(p.Target)
}

func isStringUnary(u *parser.Unary) bool {
	if len(u.Ops) > 0 || u.Value == nil || len(u.Value.Ops) > 0 {
		return false
	}
	t := u.Value.Target
	return t.Lit != nil && t.Lit.Str != nil
}

func isStringPostfix(p *parser.PostfixExpr) bool {
	if len(p.Ops) > 0 {
		return false
	}
	t := p.Target
	return t.Lit != nil && t.Lit.Str != nil
}
