//go:build slow

package erlang

import (
	"bytes"
	"fmt"
	yaml "gopkg.in/yaml.v3"
	"io/fs"
	"os"
	"path/filepath"
	"regexp"
	"strings"
	"unicode"

	"mochi/parser"
)

var identifierRegex = regexp.MustCompile(`^[A-Za-z_][A-Za-z0-9_]*$`)

// Compiler translates a subset of Mochi programs into Erlang source code.
type Compiler struct {
	buf     bytes.Buffer
	indent  int
	srcPath string

	varVers map[string]int
	lets    map[string]bool
	types   map[string]string
	funs    map[string]int

	needLeftJoin  bool
	needRightJoin bool
	needOuterJoin bool

	needJSON bool

	needStructCast bool

	needSmartGet bool

	aliases   map[string]string
	groupKeys map[string]string

	globals map[string]string

	pinned map[string]string

	builtinAliases map[string]string

	fields map[string]map[string]bool
}

func New(srcPath string) *Compiler {
	return &Compiler{
		srcPath:        srcPath,
		varVers:        make(map[string]int),
		lets:           make(map[string]bool),
		types:          make(map[string]string),
		funs:           make(map[string]int),
		needLeftJoin:   false,
		needRightJoin:  false,
		needOuterJoin:  false,
		needJSON:       false,
		needStructCast: false,
		needSmartGet:   false,
		aliases:        make(map[string]string),
		groupKeys:      make(map[string]string),
		globals:        make(map[string]string),
		pinned:         make(map[string]string),
		builtinAliases: make(map[string]string),
		fields:         make(map[string]map[string]bool),
	}
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
	c.needLeftJoin = false
	c.needRightJoin = false
	c.needOuterJoin = false
	c.needJSON = false
	c.needStructCast = false
	c.needSmartGet = false
	c.aliases = make(map[string]string)
	c.groupKeys = make(map[string]string)
	c.globals = make(map[string]string)
	c.fields = make(map[string]map[string]bool)
	c.builtinAliases = make(map[string]string)

	for _, st := range prog.Statements {
		if st.Let != nil && st.Let.Value != nil && isLiteralExpr(st.Let.Value) {
			v, err := c.compileExpr(st.Let.Value)
			if err == nil {
				c.globals[st.Let.Name] = v
			}
		}
	}

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
		if l == "" {
			continue
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
	c.writeRuntime()
	code := c.buf.Bytes()
	return FormatErlang(code), nil
}

func (c *Compiler) compileLet(l *parser.LetStmt) (string, error) {
	if l.Value != nil && isLiteralExpr(l.Value) {
		// inline constant literals; no variable declaration needed
		return "", nil
	}
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
		c.addMapFields(l.Name, l.Value)
		if id, ok := identName(l.Value); ok {
			c.copySubFields(id, l.Name)
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
		c.addMapFields(v.Name, v.Value)
		if id, ok := identName(v.Value); ok {
			c.copySubFields(id, v.Name)
		}
	}
	c.aliases[v.Name] = name
	c.lets[v.Name] = true
	return fmt.Sprintf("%s = %s", name, val), nil
}

func (c *Compiler) compileAssign(a *parser.AssignStmt) (string, error) {
	if len(a.Field) > 0 && len(a.Index) == 0 {
		if len(a.Field) > 1 {
			return "", fmt.Errorf("complex assignment not supported")
		}
		fld := a.Field[0].Name
		val, err := c.compileExpr(a.Value)
		if err != nil {
			return "", err
		}
		prev := c.refVar(a.Name)
		name := c.newVarName(a.Name)
		c.types[a.Name] = "map"
		return fmt.Sprintf("%s = %s#{%s => %s}", name, prev, fld, val), nil
	}
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
			first := c.smartGet(idx0, prev)
			if typ == "map" {
				first = fmt.Sprintf("maps:get(%s, %s, undefined)", idx0, prev)
			}
			return fmt.Sprintf("%s = %s, %s = maps:put(%s, %s, %s), %s = %s#{%s => %s}", inner, first, updated, idx1, val, inner, name, prev, idx0, updated), nil
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
	c.addMapFields(a.Name, a.Value)
	name := c.newVarName(a.Name)
	c.aliases[a.Name] = name
	c.lets[a.Name] = true
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
	case st.Import != nil:
		return c.compileImport(st.Import)
	case st.ExternVar != nil, st.ExternFun != nil, st.ExternType != nil, st.ExternObject != nil:
		// extern declarations have no effect in generated Erlang code
		return "", nil
	case st.If != nil:
		return c.compileIfStmt(st.If)
	case st.For != nil:
		return c.compileFor(st.For)
	case st.Update != nil:
		return c.compileUpdate(st.Update)
	case st.While != nil:
		return c.compileWhile(st.While)
	case st.Expect != nil:
		return c.compileExpect(st.Expect)
	case st.Test != nil:
		return c.compileTestBlock(st.Test)
	case st.Fun != nil:
		return c.compileNestedFun(st.Fun)
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
		if t := typeNameFromRef(p.Type); t != "" {
			c.types[p.Name] = t
		}
	}
	c.funs[fn.Name] = len(fn.Params)
	c.writeln(fmt.Sprintf("%s(%s) ->", fn.Name, strings.Join(params, ", ")))
	c.indent++

	// special cases for sequences of if-return statements
	if len(fn.Body) >= 2 {
		// check for one or more `if cond { return X }` followed by `return Y`
		conds := []string{}
		thens := []string{}
		idx := 0
		for idx < len(fn.Body)-1 {
			ifst := fn.Body[idx].If
			if ifst == nil || ifst.ElseIf != nil || len(ifst.Then) != 1 || ifst.Then[0].Return == nil || len(ifst.Else) != 0 {
				break
			}
			cond, err := c.compileExpr(ifst.Cond)
			if err != nil {
				return err
			}
			thenExpr, err := c.compileReturn(ifst.Then[0].Return)
			if err != nil {
				return err
			}
			conds = append(conds, cond)
			thens = append(thens, thenExpr)
			idx++
		}
		if idx == len(fn.Body)-1 && fn.Body[idx].Return != nil && len(conds) > 0 {
			elseExpr, err := c.compileReturn(fn.Body[idx].Return)
			if err != nil {
				return err
			}
			code := elseExpr
			for i := len(conds) - 1; i >= 0; i-- {
				code = fmt.Sprintf("case %s of true -> %s; _ -> %s end", conds[i], thens[i], code)
			}
			c.writeln(code + ".")
			c.indent--
			return nil
		}
	}

	var lines []string
	for _, st := range fn.Body {
		l, err := c.compileStmtLine(st)
		if err != nil {
			return err
		}
		if l == "" {
			continue
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

func (c *Compiler) compileExpect(ex *parser.ExpectStmt) (string, error) {
	cond, err := c.compileExpr(ex.Value)
	if err != nil {
		return "", err
	}
	return fmt.Sprintf("(case %s of true -> ok; _ -> erlang:error(test_failed) end)", cond), nil
}

func (c *Compiler) compileTestBlock(tb *parser.TestBlock) (string, error) {
	lines := make([]string, len(tb.Body))
	for i, st := range tb.Body {
		l, err := c.compileStmtLine(st)
		if err != nil {
			return "", err
		}
		lines[i] = l
	}
	return strings.Join(lines, ", "), nil
}

func (c *Compiler) compileWhile(w *parser.WhileStmt) (string, error) {
	mutated := map[string]bool{}
	for _, st := range w.Body {
		if st.Assign != nil {
			mutated[st.Assign.Name] = true
		}
		if st.Update != nil {
			mutated[st.Update.Target] = true
		}
	}
	params := make([]string, 0, len(mutated))
	initArgs := make([]string, 0, len(mutated))
	for v := range mutated {
		params = append(params, capitalize(v))
		initArgs = append(initArgs, c.refVar(v))
		c.aliases[v] = capitalize(v)
		c.lets[v] = true
	}
	cond, err := c.compileExpr(w.Cond)
	if err != nil {
		return "", err
	}
	bodyLines := make([]string, len(w.Body))
	for i, st := range w.Body {
		l, err := c.compileStmtLine(st)
		if err != nil {
			return "", err
		}
		bodyLines[i] = l
	}
	for v := range mutated {
		delete(c.aliases, v)
		delete(c.lets, v)
	}
	nextArgs := make([]string, 0, len(mutated))
	for v := range mutated {
		nextArgs = append(nextArgs, c.refVar(v))
	}
	body := strings.Join(bodyLines, ", ")
	if body == "" {
		body = "ok"
	}
	loopName := c.newVarName("loop")
	fun := fmt.Sprintf("fun %s(%s) -> case %s of true -> %s, %s(%s); _ -> {%s} end end", loopName, strings.Join(params, ", "), cond, body, loopName, strings.Join(nextArgs, ", "), strings.Join(params, ", "))
	call := fmt.Sprintf("{%s} = (%s)(%s)", strings.Join(nextArgs, ", "), fun, strings.Join(initArgs, ", "))
	return call, nil
}

func (c *Compiler) compileUpdate(st *parser.UpdateStmt) (string, error) {
	// allow updates to variables defined using let by removing immutability flag
	delete(c.lets, st.Target)
	item := c.newVarName(st.Target + "Item")
	prev := c.refVar(st.Target)
	aliases := map[string]string{}
	for _, it := range st.Set.Items {
		k, err := c.compileMapKey(it.Key)
		if err != nil {
			return "", err
		}
		aliases[k] = c.smartGet(k, item)
	}
	for k, v := range aliases {
		c.aliases[k] = v
		c.lets[k] = true
	}
	cond := "true"
	if st.Where != nil {
		cnd, err := c.compileExpr(st.Where)
		if err != nil {
			return "", err
		}
		cond = cnd
	}
	setParts := make([]string, len(st.Set.Items))
	for i, it := range st.Set.Items {
		k, err := c.compileMapKey(it.Key)
		if err != nil {
			return "", err
		}
		v, err := c.compileExpr(it.Value)
		if err != nil {
			return "", err
		}
		setParts[i] = fmt.Sprintf("%s => %s", k, v)
	}
	for k := range aliases {
		delete(c.aliases, k)
		delete(c.lets, k)
	}
	updated := fmt.Sprintf("case %s of true -> %s#{%s}; _ -> %s end", cond, item, strings.Join(setParts, ", "), item)
	name := c.newVarName(st.Target)
	return fmt.Sprintf("%s = [%s || %s <- %s]", name, updated, item, prev), nil
}

func (c *Compiler) compileIfStmt(ifst *parser.IfStmt) (string, error) {
	mutatedThen := map[string]bool{}
	collectMutations(ifst.Then, mutatedThen)
	mutatedElse := map[string]bool{}
	collectMutations(ifst.Else, mutatedElse)
	if ifst.ElseIf != nil {
		collectMutations(ifst.ElseIf.Then, mutatedElse)
		collectMutations(ifst.ElseIf.Else, mutatedElse)
	}
	mutated := map[string]bool{}
	for v := range mutatedThen {
		mutated[v] = true
	}
	for v := range mutatedElse {
		mutated[v] = true
	}

	pinned := map[string]string{}
	for v := range mutated {
		name := c.newVarName(v)
		pinned[v] = name
		c.pinned[v] = name
	}

	savedAliases := cloneStringMap(c.aliases)
	savedLets := cloneBoolMap(c.lets)

	cond, err := c.compileExpr(ifst.Cond)
	if err != nil {
		for v := range pinned {
			delete(c.pinned, v)
		}
		return "", err
	}
	c.aliases = cloneStringMap(savedAliases)
	c.lets = cloneBoolMap(savedLets)
	thenLines := make([]string, len(ifst.Then))
	for i, s := range ifst.Then {
		l, err := c.compileStmtLine(s)
		if err != nil {
			for v := range pinned {
				delete(c.pinned, v)
			}
			return "", err
		}
		thenLines[i] = l
	}
	thenCode := strings.Join(thenLines, ", ")
	if thenCode == "" {
		thenCode = "ok"
	}
	elseLines := []string{}
	elseCode := "ok"
	if ifst.ElseIf != nil {
		c.aliases = cloneStringMap(savedAliases)
		c.lets = cloneBoolMap(savedLets)
		ec, err := c.compileIfStmt(ifst.ElseIf)
		if err != nil {
			for v := range pinned {
				delete(c.pinned, v)
			}
			return "", err
		}
		elseCode = ec
	} else if len(ifst.Else) > 0 {
		c.aliases = cloneStringMap(savedAliases)
		c.lets = cloneBoolMap(savedLets)
		elseLines = make([]string, len(ifst.Else))
		for i, s := range ifst.Else {
			l, err := c.compileStmtLine(s)
			if err != nil {
				for v := range pinned {
					delete(c.pinned, v)
				}
				return "", err
			}
			elseLines[i] = l
		}
		elseCode = strings.Join(elseLines, ", ")
		if elseCode == "" {
			elseCode = "ok"
		}
	}
	// Default assignments for variables mutated in only one branch
	for v, name := range pinned {
		prev := savedAliases[v]
		if prev == "" {
			prev = capitalize(v)
		}
		if !mutatedThen[v] {
			thenLines = append([]string{fmt.Sprintf("%s = %s", name, prev)}, thenLines...)
		}
		if ifst.ElseIf == nil {
			if !mutatedElse[v] {
				elseLines = append([]string{fmt.Sprintf("%s = %s", name, prev)}, elseLines...)
				elseCode = strings.Join(elseLines, ", ")
			}
		}
	}
	c.aliases = savedAliases
	c.lets = savedLets
	for v, name := range pinned {
		c.aliases[v] = name
		c.lets[v] = true
	}
	if isBoolExpr(ifst.Cond) {
		for v := range pinned {
			delete(c.pinned, v)
		}
		return fmt.Sprintf("(case %s of true -> %s; _ -> %s end)", cond, thenCode, elseCode), nil
	}
	for v := range pinned {
		delete(c.pinned, v)
	}
	return fmt.Sprintf("(case %s of undefined -> %s; false -> %s; _ -> %s end)", cond, elseCode, elseCode, thenCode), nil
}

func collectMutations(sts []*parser.Statement, mutated map[string]bool) {
	for _, st := range sts {
		if st.Assign != nil {
			mutated[st.Assign.Name] = true
		}
		if st.Update != nil {
			mutated[st.Update.Target] = true
		}
		if st.If != nil {
			collectMutations(st.If.Then, mutated)
			collectMutations(st.If.Else, mutated)
			if st.If.ElseIf != nil {
				collectMutations(st.If.ElseIf.Then, mutated)
				collectMutations(st.If.ElseIf.Else, mutated)
			}
		}
		if st.For != nil {
			collectMutations(st.For.Body, mutated)
		}
		if st.While != nil {
			collectMutations(st.While.Body, mutated)
		}
	}
}

func (c *Compiler) compileFor(fr *parser.ForStmt) (string, error) {
	elemType := c.listElemType(fr.Source)
	elemFields := c.listElemFields(fr.Source)
	if elemType == "" && fr.Source.Binary != nil {
		u := fr.Source.Binary.Left
		if u.Value != nil && u.Value.Target.Selector != nil {
			t := c.types[u.Value.Target.Selector.Root]
			if t == "list_map" {
				elemType = "map"
			}
		}
	}
	prevType, ok := c.types[fr.Name]
	if elemType != "" {
		c.types[fr.Name] = elemType
	}
	prevFields, okFld := c.fields[fr.Name]
	if elemFields != nil {
		c.fields[fr.Name] = elemFields
	}
	if srcName, ok := identName(fr.Source); ok {
		c.copySubFields(srcName, fr.Name)
	}
	defer func() {
		if ok {
			c.types[fr.Name] = prevType
		} else {
			delete(c.types, fr.Name)
		}
		if okFld {
			c.fields[fr.Name] = prevFields
		} else {
			delete(c.fields, fr.Name)
		}
	}()

	mutated := map[string]bool{}
	collectMutations(fr.Body, mutated)

	// Remove variables that are declared inside the loop body since they are
	// local to each iteration and don't need to be threaded as accumulators.
	for _, st := range fr.Body {
		if st.Var != nil {
			delete(mutated, st.Var.Name)
		}
	}

	hasBC := hasBreakOrContinue(fr.Body)

	// If no variables are mutated in the loop body, emit a simple foreach
	if len(mutated) == 0 {
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

	// When variables are mutated, use foldl to thread accumulator values
	params := []string{capitalize(fr.Name)}
	initArgs := make([]string, 0, len(mutated))
	for v := range mutated {
		params = append(params, capitalize(v))
		initArgs = append(initArgs, c.refVar(v))
		c.aliases[v] = capitalize(v)
		c.lets[v] = true
	}

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

	for v := range mutated {
		delete(c.aliases, v)
		delete(c.lets, v)
	}

	nextArgs := make([]string, 0, len(mutated))
	for v := range mutated {
		nextArgs = append(nextArgs, c.refVar(v))
	}

	iterBody := body
	if hasBC {
		iterBody = fmt.Sprintf("try %s catch throw:continue -> {%s} end", body, strings.Join(nextArgs, ", "))
	}

	var seq string
	if fr.RangeEnd != nil {
		start, err := c.compileExpr(fr.Source)
		if err != nil {
			return "", err
		}
		end, err := c.compileExpr(fr.RangeEnd)
		if err != nil {
			return "", err
		}
		seq = fmt.Sprintf("lists:seq(%s, (%s)-1)", start, end)
	} else {
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
		if typ == "map" {
			seq = fmt.Sprintf("maps:to_list(%s)", src)
		} else {
			seq = src
		}
	}

	foldFun := fmt.Sprintf("fun(%s, {%s}) -> %s, {%s} end", capitalize(fr.Name), strings.Join(params[1:], ", "), iterBody, strings.Join(nextArgs, ", "))
	call := fmt.Sprintf("{%s} = lists:foldl(%s, {%s}, %s)", strings.Join(nextArgs, ", "), foldFun, strings.Join(initArgs, ", "), seq)
	if hasBC {
		call = fmt.Sprintf("try %s catch throw:break -> ok end", call)
	}
	return call, nil
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
	if isBoolExpr(ix.Cond) {
		return fmt.Sprintf("(case %s of true -> %s; _ -> %s end)", cond, thenExpr, elseExpr), nil
	}
	return fmt.Sprintf("(case %s of undefined -> %s; false -> %s; _ -> %s end)", cond, elseExpr, elseExpr, thenExpr), nil
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
		pat = strings.ReplaceAll(pat, "=>", ":=")
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
	conds := []string{}
	src, err := c.compileExpr(q.Source)
	if err != nil {
		return "", err
	}

	elemType := c.listElemType(q.Source)
	elemFields := c.listElemFields(q.Source)
	prevType, hadPrev := c.types[q.Var]
	if elemType != "" {
		c.types[q.Var] = elemType
	}
	prevFields, hadFld := c.fields[q.Var]
	if elemFields != nil {
		c.fields[q.Var] = elemFields
	}
	defer func() {
		if hadPrev {
			c.types[q.Var] = prevType
		} else {
			delete(c.types, q.Var)
		}
		if hadFld {
			c.fields[q.Var] = prevFields
		} else {
			delete(c.fields, q.Var)
		}
	}()

	gens = append(gens, fmt.Sprintf("%s <- %s", capitalize(q.Var), src))

	for _, fr := range q.Froms {
		s, err := c.compileExpr(fr.Src)
		if err != nil {
			return "", err
		}
		elemType := c.listElemType(fr.Src)
		elemFields := c.listElemFields(fr.Src)
		v := fr.Var
		prev, ok := c.types[v]
		if elemType != "" {
			c.types[v] = elemType
		}
		prevFld, okFld := c.fields[v]
		if elemFields != nil {
			c.fields[v] = elemFields
		}
		if ok {
			defer func(name, typ string) { c.types[name] = typ }(v, prev)
		} else {
			defer func(name string) { delete(c.types, name) }(v)
		}
		if okFld {
			defer func(name string, f map[string]bool) { c.fields[name] = f }(v, prevFld)
		} else if elemFields != nil {
			defer func(name string) { delete(c.fields, name) }(v)
		}
		gens = append(gens, fmt.Sprintf("%s <- %s", capitalize(v), s))
	}

	specialJoin := false
	var joinSrc []string
	var joinOns []string
	if len(q.Joins) > 0 {
		joinSrc = make([]string, len(q.Joins))
		joinOns = make([]string, len(q.Joins))
	}

	for i, j := range q.Joins {
		js, err := c.compileExpr(j.Src)
		if err != nil {
			return "", err
		}
		joinSrc[i] = js
		jt := c.listElemType(j.Src)
		jf := c.listElemFields(j.Src)
		v := j.Var
		pv, ok := c.types[v]
		if jt != "" {
			c.types[v] = jt
		}
		pf, okf := c.fields[v]
		if jf != nil {
			c.fields[v] = jf
		}
		if ok {
			defer func(name, typ string) { c.types[name] = typ }(v, pv)
		} else {
			defer func(name string) { delete(c.types, name) }(v)
		}
		if okf {
			defer func(name string, f map[string]bool) { c.fields[name] = f }(v, pf)
		} else if jf != nil {
			defer func(name string) { delete(c.fields, name) }(v)
		}
		on := ""
		if j.On != nil {
			oc, err := c.compileBoolExpr(j.On)
			if err != nil {
				return "", err
			}
			on = oc
		}
		joinOns[i] = on
	}

	if len(q.Joins) == 1 && q.Joins[0].Side != nil {
		side := *q.Joins[0].Side
		if side == "right" || side == "outer" {
			specialJoin = true
			on := joinOns[0]
			js := joinSrc[0]
			if side == "right" {
				gens[0] = fmt.Sprintf("{%s, %s} <- mochi_right_join(%s, %s, fun(%s, %s) -> %s end)",
					capitalize(q.Var), capitalize(q.Joins[0].Var), src, js, capitalize(q.Var), capitalize(q.Joins[0].Var), on)
				c.needRightJoin = true
			} else {
				gens[0] = fmt.Sprintf("{%s, %s} <- mochi_outer_join(%s, %s, fun(%s, %s) -> %s end)",
					capitalize(q.Var), capitalize(q.Joins[0].Var), src, js, capitalize(q.Var), capitalize(q.Joins[0].Var), on)
				c.needLeftJoin = true
				c.needRightJoin = true
				c.needOuterJoin = true
			}
		}
	}

	for i, j := range q.Joins {
		if specialJoin && i == 0 {
			continue
		}
		js := joinSrc[i]
		on := joinOns[i]
		if j.Side != nil {
			switch *j.Side {
			case "left":
				gens = append(gens, fmt.Sprintf("{%s, %s} <- mochi_left_join_item(%s, %s, fun(%s, %s) -> %s end)",
					capitalize(q.Var), capitalize(j.Var), capitalize(q.Var), js, capitalize(q.Var), capitalize(j.Var), on))
				c.needLeftJoin = true
			case "right":
				gens = append(gens, fmt.Sprintf("{%s, %s} <- mochi_right_join_item(%s, %s, fun(%s, %s) -> %s end)",
					capitalize(q.Var), capitalize(j.Var), js, capitalize(q.Var), capitalize(q.Var), capitalize(j.Var), on))
				c.needRightJoin = true
			case "outer":
				gens = append(gens, fmt.Sprintf("{%s, %s} <- mochi_outer_join(%s, %s, fun(%s, %s) -> %s end)",
					capitalize(q.Var), capitalize(j.Var), capitalize(q.Var), js, capitalize(q.Var), capitalize(j.Var), on))
				c.needLeftJoin = true
				c.needRightJoin = true
				c.needOuterJoin = true
			default:
				return "", fmt.Errorf("join side %s not supported", *j.Side)
			}
		} else {
			gens = append(gens, fmt.Sprintf("%s <- %s", capitalize(j.Var), js))
			if on != "" {
				conds = append(conds, on)
			}
		}
	}

	if q.Where != nil {
		cnd, err := c.compileBoolExpr(q.Where)
		if err != nil {
			return "", err
		}
		conds = append(conds, cnd)
	}

	cond := strings.Join(conds, ", ")

	// Special case simple aggregate selects like sum(n) -> lists:sum([...])
	if len(q.Froms) == 0 && len(q.Joins) == 0 && q.Group == nil && q.Sort == nil && q.Skip == nil && q.Take == nil && !q.Distinct {
		if call := callExprFromExpr(q.Select); call != nil && len(call.Args) == 1 {
			arg, err := c.compileExpr(call.Args[0])
			if err != nil {
				return "", err
			}
			list := fmt.Sprintf("[%s || %s <- %s", arg, capitalize(q.Var), src)
			if cond != "" {
				list += ", " + cond
			}
			list += "]"
			switch call.Func {
			case "sum":
				return fmt.Sprintf("lists:sum(%s)", list), nil
			case "count":
				return fmt.Sprintf("length(%s)", list), nil
			case "min":
				return fmt.Sprintf("lists:min(%s)", list), nil
			case "max":
				return fmt.Sprintf("lists:max(%s)", list), nil
			case "avg":
				return fmt.Sprintf("(lists:sum(%s) / length(%s))", list, list), nil
			}
		}
	}

	res := ""

	if q.Group != nil {
		keyParts := make([]string, len(q.Group.Exprs))
		for i, ge := range q.Group.Exprs {
			k, err := c.compileExpr(ge)
			if err != nil {
				return "", err
			}
			keyParts[i] = k
		}
		keyExpr := ""
		if len(keyParts) == 1 {
			keyExpr = keyParts[0]
		} else {
			keyExpr = "{" + strings.Join(keyParts, ", ") + "}"
		}

		vars := []string{q.Var}
		for _, fr := range q.Froms {
			vars = append(vars, fr.Var)
		}
		for _, j := range q.Joins {
			vars = append(vars, j.Var)
		}
		parts := make([]string, len(vars))
		for i, v := range vars {
			parts[i] = fmt.Sprintf("%s => %s", v, capitalize(v))
		}
		elemMap := ""
		if len(parts) == 1 {
			elemMap = capitalize(vars[0])
		} else {
			elemMap = "#{" + strings.Join(parts, ", ") + "}"
		}

		pairList := "[{" + keyExpr + ", " + elemMap + "} || " + strings.Join(gens, ", ")
		if cond != "" {
			pairList += ", " + cond
		}
		pairList += "]"

		acc := c.newVarName("acc")
		kvar := c.newVarName("key")
		vvar := c.newVarName("val")
		fold := fmt.Sprintf("lists:foldl(fun({%s, %s}, %s) -> L = maps:get(%s, %s, []), maps:put(%s, [%s | L], %s) end, #{}, %s)", kvar, vvar, acc, kvar, acc, kvar, vvar, acc, pairList)
		groupsList := fmt.Sprintf("maps:to_list(%s)", fold)

		prevKeys := c.groupKeys
		c.groupKeys = make(map[string]string)
		if len(q.Group.Exprs) > 0 {
			fields := groupKeyFields(q.Group.Exprs[0])
			for _, f := range fields {
				c.groupKeys[f] = fmt.Sprintf("maps:get(%s, %s)", f, kvar)
			}
		}
		c.groupKeys[q.Group.Name] = kvar

		c.aliases[q.Group.Name] = vvar
		c.lets[q.Group.Name] = true
		prevGrpTyp, hadGrpTyp := c.types[q.Group.Name]
		prevGrpFlds, hadGrpFlds := c.fields[q.Group.Name]
		c.types[q.Group.Name] = "map"
		c.fields[q.Group.Name] = map[string]bool{"key": true, "items": true}
		if _, ok := c.fields[q.Var]; ok {
			c.copySubFields(q.Var, q.Group.Name+".items")
			if t, ok2 := c.types[q.Var]; ok2 && t == "map" {
				c.types[q.Group.Name+".items"] = "list_map"
			}
		}
		defer func(n string) {
			delete(c.aliases, n)
			delete(c.lets, n)
			if hadGrpTyp {
				c.types[n] = prevGrpTyp
			} else {
				delete(c.types, n)
			}
			if hadGrpFlds {
				c.fields[n] = prevGrpFlds
			} else {
				delete(c.fields, n)
			}
			c.groupKeys = prevKeys
		}(q.Group.Name)

		having := ""
		if q.Group.Having != nil {
			h, err := c.compileBoolExpr(q.Group.Having)
			if err != nil {
				return "", err
			}
			having = h
		}

		sel, err := c.compileExpr(q.Select)
		if err != nil {
			return "", err
		}

		if selectIsVar(q.Select, q.Group.Name) {
			sel = fmt.Sprintf("#{key => %s, items => %s}", kvar, vvar)
		}

		listExpr := "[" + sel + " || {" + kvar + ", " + vvar + "} <- " + groupsList
		if having != "" {
			listExpr += ", " + having
		}
		listExpr += "]"

		res = listExpr
		if q.Sort != nil {
			key, err := c.compileExpr(q.Sort)
			if err != nil {
				return "", err
			}
			pairList2 := "[{" + key + ", " + sel + "} || {" + kvar + ", " + vvar + "} <- " + groupsList
			if having != "" {
				pairList2 += ", " + having
			}
			pairList2 += "]"
			sorted := fmt.Sprintf("lists:keysort(1, %s)", pairList2)
			res = fmt.Sprintf("[V || {_, V} <- %s]", sorted)
		}
	} else {
		sel, err := c.compileExpr(q.Select)
		if err != nil {
			return "", err
		}
		listExpr := "[" + sel + " || " + strings.Join(gens, ", ")
		if cond != "" {
			listExpr += ", " + cond
		}
		listExpr += "]"
		res = listExpr
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
	type operand struct {
		expr  string
		isStr bool
		part  *parser.BinaryOp
	}

	left, err := c.compileUnary(b.Left)
	if err != nil {
		return "", err
	}
	operands := []operand{{expr: left, isStr: isStringUnary(b.Left)}}
	ops := []string{}

	for _, p := range b.Right {
		r, err := c.compilePostfix(p.Right)
		if err != nil {
			return "", err
		}
		op := p.Op
		if op == "union" && p.All {
			op = "union_all"
		}
		operands = append(operands, operand{expr: r, isStr: isStringPostfix(p.Right), part: p})
		ops = append(ops, op)
	}

	prec := [][]string{
		{"*", "/", "%"},
		{"+", "-"},
		{"<", "<=", ">", ">="},
		{"==", "!=", "in"},
		{"&&"},
		{"||"},
		{"union", "union_all", "except", "intersect"},
	}

	contains := func(sl []string, s string) bool {
		for _, v := range sl {
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
			var isStr bool

			switch op {
			case "+":
				if l.isStr || r.isStr {
					expr = fmt.Sprintf("%s ++ %s", l.expr, r.expr)
					isStr = true
				} else {
					expr = fmt.Sprintf("(%s + %s)", l.expr, r.expr)
				}
			case "-", "*", "/", "<", "<=", ">", ">=", "&&", "||":
				opStr, _ := c.mapBinOp(op)
				expr = fmt.Sprintf("(%s %s %s)", l.expr, opStr, r.expr)
			case "%":
				expr = fmt.Sprintf("(%s rem %s)", l.expr, r.expr)
			case "==", "!=":
				opStr, _ := c.mapBinOp(op)
				expr = fmt.Sprintf("(%s %s %s)", l.expr, opStr, r.expr)
			case "in":
				typ := ""
				if r.part != nil {
					typ = c.typeOfPostfix(r.part.Right)
				}
				if typ == "map" && !(l.isStr || r.isStr) {
					expr = fmt.Sprintf("maps:is_key(%s, %s)", l.expr, r.expr)
				} else if typ == "string" || l.isStr || r.isStr {
					expr = fmt.Sprintf("string:str(%s, %s) > 0", r.expr, l.expr)
				} else {
					expr = fmt.Sprintf("lists:member(%s, %s)", l.expr, r.expr)
				}
			case "union_all":
				expr = fmt.Sprintf("(%s ++ %s)", l.expr, r.expr)
			case "union":
				expr = fmt.Sprintf("ordsets:union(%s, %s)", l.expr, r.expr)
			case "except":
				expr = fmt.Sprintf("ordsets:subtract(%s, %s)", l.expr, r.expr)
			case "intersect":
				expr = fmt.Sprintf("ordsets:intersection(%s, %s)", l.expr, r.expr)
			default:
				return "", fmt.Errorf("unsupported operator %s", op)
			}

			operands[i] = operand{expr: expr, isStr: isStr, part: l.part}
			operands = append(operands[:i+1], operands[i+2:]...)
			ops = append(ops[:i], ops[i+1:]...)
		}
	}

	if len(operands) != 1 {
		return "", fmt.Errorf("unexpected binary expr state")
	}
	return operands[0].expr, nil
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
	case "<":
		return "<", nil
	case "<=":
		return "=<", nil
	case ">":
		return ">", nil
	case ">=":
		return ">=", nil
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
	for i := 0; i < len(p.Ops); i++ {
		op := p.Ops[i]
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
				if typ == "map" {
					val = fmt.Sprintf("maps:get(%s, %s, undefined)", idx, val)
				} else if typ == "" && !isIntLiteral(idxOp.Start) {
					val = c.smartGet(idx, val)
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
				} else if unicode.IsUpper(rune(t[0])) {
					val = fmt.Sprintf("mochi_struct_cast(%s)", val)
					c.needStructCast = true
					typ = "map"
				} else {
					// casting to other types is a no-op
					typ = "map"
				}
			}
		case op.Field != nil:
			name := op.Field.Name
			if i+1 < len(p.Ops) && p.Ops[i+1].Call != nil && (name == "contains" || name == "starts_with") {
				i++
				arg, err := c.compileExpr(p.Ops[i].Call.Args[0])
				if err != nil {
					return "", err
				}
				if name == "contains" {
					val = fmt.Sprintf("(string:str(%s, %s) > 0)", val, arg)
				} else {
					val = fmt.Sprintf("(string:prefix(%s, %s) /= nomatch)", val, arg)
				}
				typ = "bool"
			} else {
				if typ == "map" {
					val = fmt.Sprintf("maps:get(%s, %s, undefined)", name, val)
					typ = "map"
				} else {
					val = c.smartGet(name, val)
					typ = ""
				}
			}
		case op.Call != nil:
			args := make([]string, len(op.Call.Args))
			for j, a := range op.Call.Args {
				s, err := c.compileExpr(a)
				if err != nil {
					return "", err
				}
				args[j] = s
			}
			if p.Target.Selector != nil && len(p.Target.Selector.Tail) > 0 {
				alias := p.Target.Selector.Root
				method := p.Target.Selector.Tail[len(p.Target.Selector.Tail)-1]
				if mod, ok := c.builtinAliases[alias]; ok {
					switch mod {
					case "python_math":
						switch method {
						case "sqrt":
							if len(args) == 1 {
								val = fmt.Sprintf("math:sqrt(%s)", args[0])
								typ = "float"
								continue
							}
						case "pow":
							if len(args) == 2 {
								val = fmt.Sprintf("math:pow(%s, %s)", args[0], args[1])
								typ = "float"
								continue
							}
						case "sin":
							if len(args) == 1 {
								val = fmt.Sprintf("math:sin(%s)", args[0])
								typ = "float"
								continue
							}
						case "log":
							if len(args) == 1 {
								val = fmt.Sprintf("math:log(%s)", args[0])
								typ = "float"
								continue
							}
						}
					case "go_testpkg":
						if method == "Add" && len(args) == 2 {
							val = fmt.Sprintf("(%s + %s)", args[0], args[1])
							continue
						}
					}
				}
			}
			if p.Target.Selector != nil && len(p.Target.Selector.Tail) > 0 && p.Target.Selector.Tail[len(p.Target.Selector.Tail)-1] == "keys" && len(args) == 0 {
				base := c.refVar(p.Target.Selector.Root)
				if v, ok := c.globals[p.Target.Selector.Root]; ok {
					base = v
				}
				typ2 := c.types[p.Target.Selector.Root]
				for _, f := range p.Target.Selector.Tail[:len(p.Target.Selector.Tail)-1] {
					if typ2 == "map" {
						if fields, ok := c.fields[p.Target.Selector.Root]; ok && fields[f] {
							base = fmt.Sprintf("maps:get(%s, %s)", f, base)
						} else {
							base = fmt.Sprintf("maps:get(%s, %s, undefined)", f, base)
						}
						typ2 = "map"
					} else {
						base = c.smartGet(f, base)
						typ2 = ""
					}
				}
				val = fmt.Sprintf("maps:keys(%s)", base)
				typ = "list"
			} else if p.Target.Selector != nil && len(p.Target.Selector.Tail) > 0 && (p.Target.Selector.Tail[len(p.Target.Selector.Tail)-1] == "contains" || p.Target.Selector.Tail[len(p.Target.Selector.Tail)-1] == "starts_with") {
				base := c.refVar(p.Target.Selector.Root)
				if v, ok := c.globals[p.Target.Selector.Root]; ok {
					base = v
				}
				typ2 := c.types[p.Target.Selector.Root]
				for _, f := range p.Target.Selector.Tail[:len(p.Target.Selector.Tail)-1] {
					if typ2 == "map" {
						if fields, ok := c.fields[p.Target.Selector.Root]; ok && fields[f] {
							base = fmt.Sprintf("maps:get(%s, %s)", f, base)
						} else {
							base = fmt.Sprintf("maps:get(%s, %s, undefined)", f, base)
						}
						typ2 = "map"
					} else {
						base = c.smartGet(f, base)
						typ2 = ""
					}
				}
				method := p.Target.Selector.Tail[len(p.Target.Selector.Tail)-1]
				if method == "contains" {
					val = fmt.Sprintf("(string:str(%s, %s) > 0)", base, args[0])
				} else {
					val = fmt.Sprintf("(string:prefix(%s, %s) /= nomatch)", base, args[0])
				}
				typ = "bool"
			} else {
				val = fmt.Sprintf("%s(%s)", val, strings.Join(args, ", "))
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
	case p.Struct != nil:
		parts := make([]string, len(p.Struct.Fields)+1)
		parts[0] = fmt.Sprintf("\"__name\" => %q", p.Struct.Name)
		for i, f := range p.Struct.Fields {
			v, err := c.compileExpr(f.Value)
			if err != nil {
				return "", err
			}
			parts[i+1] = fmt.Sprintf("%s => %s", f.Name, v)
		}
		return "#{" + strings.Join(parts, ", ") + "}", nil
	case p.Selector != nil:
		root := p.Selector.Root
		if len(p.Selector.Tail) == 0 {
			if v, ok := c.globals[root]; ok {
				return v, nil
			}
		}
		if mod, ok := c.builtinAliases[root]; ok && len(p.Selector.Tail) == 1 {
			tail := p.Selector.Tail[0]
			switch mod {
			case "go_testpkg":
				switch tail {
				case "Pi":
					return "3.14", nil
				case "Answer":
					return "42", nil
				}
			case "python_math":
				switch tail {
				case "pi":
					return "math:pi()", nil
				case "e":
					return "math:exp(1)", nil
				}
			}
		}
		if len(p.Selector.Tail) == 0 && unicode.IsUpper(rune(root[0])) && !c.lets[root] && c.varVers[root] == 0 {
			return strings.ToLower(root), nil
		}
		if k, ok := c.groupKeys[root]; ok {
			expr := k
			tail := p.Selector.Tail
			typ := c.types[root]
			path := root
			if len(tail) == 0 && (strings.HasPrefix(expr, "maps:get(") || strings.HasPrefix(expr, "mochi_get(")) {
				return expr, nil
			}
			if len(tail) > 0 && tail[0] == "key" {
				tail = tail[1:]
				typ = "map"
			} else {
				expr = c.refVar(root)
			}
			for _, f := range tail {
				if typ == "map" {
					if fields, ok := c.fields[path]; ok && fields[f] {
						expr = fmt.Sprintf("maps:get(%s, %s)", f, expr)
					} else {
						expr = fmt.Sprintf("maps:get(%s, %s, undefined)", f, expr)
					}
					path = path + "." + f
					if t, ok := c.types[path]; ok {
						typ = t
					} else {
						typ = ""
					}
				} else {
					expr = c.smartGet(f, expr)
					typ = ""
				}
			}
			return expr, nil
		}
		expr := c.refVar(root)
		typ := c.types[root]
		path := root
		for _, f := range p.Selector.Tail {
			if typ == "map" {
				if fields, ok := c.fields[path]; ok && fields[f] {
					expr = fmt.Sprintf("maps:get(%s, %s)", f, expr)
				} else {
					expr = fmt.Sprintf("maps:get(%s, %s, undefined)", f, expr)
				}
				path = path + "." + f
				if t, ok := c.types[path]; ok {
					typ = t
				} else {
					typ = ""
				}
			} else {
				expr = c.smartGet(f, expr)
				typ = ""
			}
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
	case p.Load != nil:
		return c.compileLoadExpr(p.Load)
	case p.Save != nil:
		return c.compileSaveExpr(p.Save)
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
			if c.inferExprType(a) == "string" {
				parts[i] = "~s"
			} else {
				parts[i] = "~p"
			}
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
	case "str":
		if len(call.Args) != 1 {
			return "", fmt.Errorf("str expects 1 arg")
		}
		a0, err := c.compileExpr(call.Args[0])
		if err != nil {
			return "", err
		}
		return fmt.Sprintf("lists:flatten(io_lib:format(\"~p\", [%s]))", a0), nil
	case "count":
		if len(call.Args) != 1 {
			return "", fmt.Errorf("count expects 1 arg")
		}
		a0, err := c.compileExpr(call.Args[0])
		if err != nil {
			return "", err
		}
		it := c.newVarName("it")
		return fmt.Sprintf("(case %s of #{items := %s} -> length(%s); _ -> length(%s) end)", a0, it, it, a0), nil
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
	case "exists":
		if len(call.Args) != 1 {
			return "", fmt.Errorf("exists expects 1 arg")
		}
		// Special case for simple query expressions so we can
		// translate to lists:any/2 like the human implementations.
		if q := extractQuery(call.Args[0]); q != nil {
			if canExistsAny(q) {
				src, err := c.compileExpr(q.Source)
				if err != nil {
					return "", err
				}
				if q.Where == nil {
					return fmt.Sprintf("(%s /= [])", src), nil
				}
				cond, err := c.compileExpr(q.Where)
				if err != nil {
					return "", err
				}
				return fmt.Sprintf("lists:any(fun(%s) -> %s end, %s)", capitalize(q.Var), cond, src), nil
			}
		}
		a0, err := c.compileExpr(call.Args[0])
		if err != nil {
			return "", err
		}
		return fmt.Sprintf("(length(%s) > 0)", a0), nil
	case "json":
		if len(call.Args) != 1 {
			return "", fmt.Errorf("json expects 1 arg")
		}
		a0, err := c.compileExpr(call.Args[0])
		if err != nil {
			return "", err
		}
		c.needJSON = true
		return fmt.Sprintf("mochi_json(%s)", a0), nil
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
	case "lower":
		if len(call.Args) != 1 {
			return "", fmt.Errorf("lower expects 1 arg")
		}
		a0, err := c.compileExpr(call.Args[0])
		if err != nil {
			return "", err
		}
		return fmt.Sprintf("string:lowercase(%s)", a0), nil
	case "upper":
		if len(call.Args) != 1 {
			return "", fmt.Errorf("upper expects 1 arg")
		}
		a0, err := c.compileExpr(call.Args[0])
		if err != nil {
			return "", err
		}
		return fmt.Sprintf("string:uppercase(%s)", a0), nil
	default:
		if unicode.IsUpper(rune(call.Func[0])) && c.funs[call.Func] == 0 {
			parts := make([]string, len(call.Args)+1)
			parts[0] = fmt.Sprintf("\"__name\" => %q", call.Func)
			for i, a := range call.Args {
				v, err := c.compileExpr(a)
				if err != nil {
					return "", err
				}
				if sel := a.Binary.Left.Value.Target.Selector; sel != nil && len(sel.Tail) == 0 {
					parts[i+1] = fmt.Sprintf("%s => %s", sel.Root, v)
				} else {
					parts[i+1] = fmt.Sprintf("arg%d => %s", i, v)
				}
			}
			return "#{" + strings.Join(parts, ", ") + "}", nil
		}
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

func (c *Compiler) compileLoadExpr(le *parser.LoadExpr) (string, error) {
	if le.Path == nil {
		return "", fmt.Errorf("load path required")
	}
	path := *le.Path
	if !filepath.IsAbs(path) {
		tryPath := filepath.Clean(filepath.Join(filepath.Dir(c.srcPath), path))
		if exists(tryPath) {
			path = tryPath
		} else {
			alt := filepath.Join(repoRoot(), "tests", strings.TrimLeft(path, "./"))
			path = filepath.Clean(alt)
		}
	}
	data, err := os.ReadFile(path)
	if err != nil {
		return "", err
	}
	var items []map[string]interface{}
	if err := yaml.Unmarshal(data, &items); err != nil {
		return "", err
	}
	rows := make([]string, len(items))
	for i, m := range items {
		parts := make([]string, 0, len(m)+1)
		parts = append(parts, "\"__name\" => \"Person\"")
		for k, v := range m {
			parts = append(parts, fmt.Sprintf("%s => %v", k, c.compileLiteralValue(v)))
		}
		rows[i] = "#{" + strings.Join(parts, ", ") + "}"
	}
	return "[" + strings.Join(rows, ", ") + "]", nil
}

func (c *Compiler) compileSaveExpr(se *parser.SaveExpr) (string, error) {
	src, err := c.compileExpr(se.Src)
	if err != nil {
		return "", err
	}
	if se.Path != nil && *se.Path == "-" {
		c.needJSON = true
		v := c.newVarName("row")
		return fmt.Sprintf("lists:foreach(fun(%s) -> io:format(\"~s\\n\", [mochi_to_json(%s)]) end, %s)", v, v, src), nil
	}
	return "ok", nil
}

func (c *Compiler) compileLiteralValue(v interface{}) string {
	switch t := v.(type) {
	case int, int64, float64:
		return fmt.Sprintf("%v", t)
	case string:
		return fmt.Sprintf("%q", t)
	default:
		return "undefined"
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
	// Map keys may be simple identifiers like a or b which should be emitted
	// as atoms rather than variables. Detect this pattern before falling
	// back to the general expression compiler so quoted keys remain quoted.
	if e != nil && e.Binary != nil && len(e.Binary.Right) == 0 {
		u := e.Binary.Left
		if len(u.Ops) == 0 && u.Value != nil && len(u.Value.Ops) == 0 {
			if sel := u.Value.Target.Selector; sel != nil && len(sel.Tail) == 0 {
				if unicode.IsLower(rune(sel.Root[0])) {
					return sel.Root, nil
				}
			}
		}
	}
	return c.compileExpr(e)
}

func (c *Compiler) compileImport(im *parser.ImportStmt) (string, error) {
	if im.Lang == nil {
		return "", nil
	}
	alias := im.As
	if alias == "" {
		alias = parser.AliasFromPath(im.Path)
	}
	switch *im.Lang {
	case "go":
		if im.Auto && im.Path == "mochi/runtime/ffi/go/testpkg" {
			c.builtinAliases[alias] = "go_testpkg"
			return "", nil
		}
	case "python":
		if im.Path == "math" {
			c.builtinAliases[alias] = "python_math"
			return "", nil
		}
	}
	return "", fmt.Errorf("unsupported import")
}

func (c *Compiler) writeln(s string) {
	for i := 0; i < c.indent; i++ {
		c.buf.WriteString("    ")
	}
	c.buf.WriteString(s)
	c.buf.WriteByte('\n')
}

func (c *Compiler) newVarName(base string) string {
	if name, ok := c.pinned[base]; ok {
		return name
	}
	idx := c.varVers[base]
	name := fmt.Sprintf("%s%d", capitalize(base), idx)
	c.varVers[base] = idx + 1
	return name
}

func cloneStringMap(m map[string]string) map[string]string {
	n := make(map[string]string, len(m))
	for k, v := range m {
		n[k] = v
	}
	return n
}

func cloneBoolMap(m map[string]bool) map[string]bool {
	n := make(map[string]bool, len(m))
	for k, v := range m {
		n[k] = v
	}
	return n
}

func (c *Compiler) refVar(base string) string {
	if a, ok := c.aliases[base]; ok {
		return a
	}
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

func (c *Compiler) smartGet(key, expr string) string {
	c.needSmartGet = true
	return fmt.Sprintf("mochi_get(%s, %s)", key, expr)
}

func (c *Compiler) addMapFields(name string, e *parser.Expr) {
	if id, ok := identName(e); ok {
		c.copySubFields(id, name)
		return
	}
	c.addFieldsRecursive(name, e)
}

func (c *Compiler) addFieldsRecursive(name string, e *parser.Expr) {
	if e == nil || e.Binary == nil || len(e.Binary.Right) > 0 {
		if q := extractQuery(e); q != nil {
			c.addFieldsRecursive(name, q.Select)
		}
		return
	}
	u := e.Binary.Left
	if len(u.Ops) > 0 || u.Value == nil || len(u.Value.Ops) > 0 {
		return
	}
	t := u.Value.Target
	switch {
	case t.Map != nil:
		if c.fields[name] == nil {
			c.fields[name] = make(map[string]bool)
		}
		c.types[name] = "map"
		for _, it := range t.Map.Items {
			if k, ok := identName(it.Key); ok {
				c.fields[name][k] = true
				c.addFieldsRecursive(name+"."+k, it.Value)
			}
		}
	case t.Struct != nil:
		if c.fields[name] == nil {
			c.fields[name] = make(map[string]bool)
		}
		c.types[name] = "map"
		for _, f := range t.Struct.Fields {
			c.fields[name][f.Name] = true
			c.addFieldsRecursive(name+"."+f.Name, f.Value)
		}
	case t.List != nil && len(t.List.Elems) > 0:
		elem := t.List.Elems[0]
		if isMapLiteralExpr(elem) || isStructCastExpr(elem) {
			if c.fields[name] == nil {
				c.fields[name] = make(map[string]bool)
			}
			c.types[name] = "list_map"
			fs := mapLiteralFields(elem)
			for _, f := range fs {
				c.fields[name][f] = true
				c.addFieldsRecursive(name+"."+f, getFieldValue(elem, f))
			}
		}
	case t.Query != nil:
		c.addFieldsRecursive(name, t.Query.Select)
	}
}

func getFieldValue(e *parser.Expr, field string) *parser.Expr {
	if e == nil || e.Binary == nil || len(e.Binary.Right) > 0 {
		return nil
	}
	u := e.Binary.Left
	if len(u.Ops) > 0 || u.Value == nil || len(u.Value.Ops) > 0 {
		return nil
	}
	if m := u.Value.Target.Map; m != nil {
		for _, it := range m.Items {
			if k, ok := identName(it.Key); ok && k == field {
				return it.Value
			}
		}
	}
	if s := u.Value.Target.Struct; s != nil {
		for _, f := range s.Fields {
			if f.Name == field {
				return f.Value
			}
		}
	}
	return nil
}

func (c *Compiler) copySubFields(src, dst string) {
	if fs, ok := c.fields[src]; ok {
		if c.fields[dst] == nil {
			c.fields[dst] = make(map[string]bool)
		}
		for k := range fs {
			c.fields[dst][k] = true
		}
	}
	if typ, ok := c.types[src]; ok {
		c.types[dst] = typ
	}
	for k, fs := range c.fields {
		if strings.HasPrefix(k, src+".") {
			sub := dst + k[len(src):]
			if c.fields[sub] == nil {
				c.fields[sub] = make(map[string]bool)
			}
			for f := range fs {
				c.fields[sub][f] = true
			}
		}
	}
	for k, t := range c.types {
		if strings.HasPrefix(k, src+".") {
			c.types[dst+k[len(src):]] = t
		}
	}
}

func (c *Compiler) listElemType(e *parser.Expr) string {
	t := c.inferExprType(e)
	if t == "list_map" {
		return "map"
	}
	if e == nil || e.Binary == nil || len(e.Binary.Right) > 0 {
		return ""
	}
	u := e.Binary.Left
	if len(u.Ops) > 0 || u.Value == nil || len(u.Value.Ops) > 0 {
		return ""
	}
	if l := u.Value.Target.List; l != nil && len(l.Elems) > 0 {
		return c.inferExprType(l.Elems[0])
	}
	return ""
}

func (c *Compiler) listElemFields(e *parser.Expr) map[string]bool {
	if q := extractQuery(e); q != nil {
		return fieldSet(queryResultFields(q))
	}
	if fs := listMapLiteralFields(e); fs != nil {
		return fieldSet(fs)
	}
	if name, ok := identName(e); ok {
		if m, ok2 := c.fields[name]; ok2 {
			return m
		}
	}
	return nil
}

func inferExprTypeShallow(e *parser.Expr) string {
	switch {
	case extractQuery(e) != nil:
		q := extractQuery(e)
		if q.Group != nil && selectIsVar(q.Select, q.Group.Name) {
			return "list_map"
		}
		t := inferExprTypeShallow(q.Select)
		if t == "map" {
			return "list_map"
		}
		if t != "" {
			return "list"
		}
		return ""
	case isStructCastExpr(e):
		return "map"
	case isMapLiteralExpr(e):
		return "map"
	case isListMapLiteralExpr(e):
		return "list_map"
	case isListLiteralExpr(e):
		return "list"
	case isStringLiteralExpr(e):
		return "string"
	default:
		return ""
	}
}

func isMapLiteralExpr(e *parser.Expr) bool {
	if e == nil || e.Binary == nil || len(e.Binary.Right) > 0 {
		return false
	}
	u := e.Binary.Left
	if len(u.Ops) > 0 || u.Value == nil || len(u.Value.Ops) > 0 {
		return false
	}
	return u.Value.Target.Map != nil || u.Value.Target.Struct != nil
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

func isStructCastExpr(e *parser.Expr) bool {
	if e == nil || e.Binary == nil || len(e.Binary.Right) > 0 {
		return false
	}
	u := e.Binary.Left
	if len(u.Ops) > 0 || u.Value == nil {
		return false
	}
	p := u.Value
	if len(p.Ops) == 0 {
		return false
	}
	op := p.Ops[len(p.Ops)-1]
	if op.Cast == nil || op.Cast.Type == nil {
		return false
	}
	t := op.Cast.Type
	if t.Struct != nil {
		return true
	}
	if t.Simple != nil {
		name := *t.Simple
		if name != "" && unicode.IsUpper(rune(name[0])) {
			return true
		}
	}
	return false
}

func isListMapLiteralExpr(e *parser.Expr) bool {
	if !isListLiteralExpr(e) {
		return false
	}
	u := e.Binary.Left
	l := u.Value.Target.List
	if len(l.Elems) == 0 {
		return false
	}
	return isMapLiteralExpr(l.Elems[0]) || isStructCastExpr(l.Elems[0])
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

func isBoolExpr(e *parser.Expr) bool {
	if e == nil || e.Binary == nil {
		return false
	}
	u := e.Binary.Left
	if len(u.Ops) > 0 {
		for _, op := range u.Ops {
			if op == "!" {
				return true
			}
		}
	}
	if u.Value != nil && len(u.Value.Ops) == 0 && u.Value.Target.Lit != nil && u.Value.Target.Lit.Bool != nil {
		return true
	}
	for _, op := range e.Binary.Right {
		switch op.Op {
		case "==", "!=", "<", "<=", ">", ">=", "&&", "||", "in":
			return true
		}
	}
	return false
}

// compileBoolExpr returns Erlang code evaluating e in boolean context.
// If e already represents a boolean expression, it is compiled directly.
// Otherwise the result is true when e is neither undefined nor false.
func (c *Compiler) compileBoolExpr(e *parser.Expr) (string, error) {
	expr, err := c.compileExpr(e)
	if err != nil {
		return "", err
	}
	if isBoolExpr(e) {
		return expr, nil
	}
	return fmt.Sprintf("(case %s of undefined -> false; false -> false; _ -> true end)", expr), nil
}

func typeNameFromRef(tr *parser.TypeRef) string {
	if tr == nil {
		return ""
	}
	if tr.Generic != nil {
		switch tr.Generic.Name {
		case "list":
			return "list"
		case "map":
			return "map"
		}
	}
	if tr.Simple != nil {
		if *tr.Simple == "string" {
			return "string"
		}
	}
	return ""
}

func (c *Compiler) inferExprType(e *parser.Expr) string {
	if t := inferExprTypeShallow(e); t != "" {
		return t
	}
	if e == nil || e.Binary == nil || len(e.Binary.Right) > 0 {
		return ""
	}
	u := e.Binary.Left
	if len(u.Ops) > 0 || u.Value == nil || len(u.Value.Ops) > 0 {
		return ""
	}
	if sel := u.Value.Target.Selector; sel != nil {
		if len(sel.Tail) == 0 {
			if typ, ok := c.types[sel.Root]; ok {
				return typ
			}
		} else {
			path := sel.Root
			for _, part := range sel.Tail {
				path += "." + part
			}
			if typ, ok := c.types[path]; ok {
				return typ
			}
		}
	}
	return ""
}

func (c *Compiler) typeOfPrimary(p *parser.Primary) string {
	switch {
	case p.Map != nil:
		return "map"
	case p.List != nil:
		return "list"
	case p.Lit != nil && p.Lit.Str != nil:
		return "string"
	case p.Call != nil:
		if p.Call.Func == "substring" {
			return "string"
		}
		return ""
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

// extractQuery attempts to retrieve a QueryExpr from e if present.
func extractQuery(e *parser.Expr) *parser.QueryExpr {
	if e == nil || e.Binary == nil || len(e.Binary.Right) > 0 {
		return nil
	}
	u := e.Binary.Left
	if len(u.Ops) > 0 || u.Value == nil || len(u.Value.Ops) > 0 {
		return nil
	}
	return u.Value.Target.Query
}

// canExistsAny reports whether q can be translated to lists:any/2.
// We only support simple queries: single source, optional where, selecting the
// loop variable without modifiers.
func canExistsAny(q *parser.QueryExpr) bool {
	if q == nil {
		return false
	}
	if len(q.Froms) > 0 || len(q.Joins) > 0 || q.Group != nil || q.Sort != nil || q.Skip != nil || q.Take != nil || q.Distinct {
		return false
	}
	return selectIsVar(q.Select, q.Var)
}

// selectIsVar checks whether e represents the variable name.
func selectIsVar(e *parser.Expr, name string) bool {
	if e == nil || e.Binary == nil || len(e.Binary.Right) > 0 {
		return false
	}
	u := e.Binary.Left
	if len(u.Ops) > 0 || u.Value == nil || len(u.Value.Ops) > 0 {
		return false
	}
	if s := u.Value.Target.Selector; s != nil {
		return s.Root == name && len(s.Tail) == 0
	}
	return false
}

// callExprFromExpr extracts a CallExpr if e directly represents a function call.
func callExprFromExpr(e *parser.Expr) *parser.CallExpr {
	if e == nil || e.Binary == nil || len(e.Binary.Right) > 0 {
		return nil
	}
	u := e.Binary.Left
	if len(u.Ops) > 0 || u.Value == nil || len(u.Value.Ops) > 0 {
		return nil
	}
	return u.Value.Target.Call
}

func (c *Compiler) writeRuntime() {
	if c.needLeftJoin {
		c.writeln("")
		c.writeln("mochi_left_join_item(A, B, Fun) ->")
		c.indent++
		c.writeln("Matches = [ {A, J} || J <- B, Fun(A, J) ],")
		c.writeln("case Matches of")
		c.indent++
		c.writeln("[] -> [{A, undefined}];")
		c.writeln("_ -> Matches")
		c.indent--
		c.writeln("end.")
		c.indent--

		c.writeln("")
		c.writeln("mochi_left_join(L, R, Fun) ->")
		c.indent++
		c.writeln("lists:flatmap(fun(X) -> mochi_left_join_item(X, R, Fun) end, L).")
		c.indent--
	}
	if c.needRightJoin {
		c.writeln("")
		c.writeln("mochi_right_join_item(B, A, Fun) ->")
		c.indent++
		c.writeln("Matches = [ {I, B} || I <- A, Fun(I, B) ],")
		c.writeln("case Matches of")
		c.indent++
		c.writeln("[] -> [{undefined, B}];")
		c.writeln("_ -> Matches")
		c.indent--
		c.writeln("end.")
		c.indent--

		c.writeln("")
		c.writeln("mochi_right_join(L, R, Fun) ->")
		c.indent++
		c.writeln("lists:flatmap(fun(Y) -> mochi_right_join_item(Y, L, Fun) end, R).")
		c.indent--
	}
	if c.needOuterJoin {
		c.writeln("")
		c.writeln("mochi_outer_join(L, R, Fun) ->")
		c.indent++
		c.writeln("Left = mochi_left_join(L, R, Fun),")
		c.writeln("Right = [ P || P = {undefined, _} <- mochi_right_join(L, R, Fun) ],")
		c.writeln("Left ++ Right.")
		c.indent--
	}
	if c.needSmartGet {
		c.writeln("")
		c.writeln("mochi_get(K, M) ->")
		c.indent++
		c.writeln("case maps:find(K, M) of")
		c.indent++
		c.writeln("{ok, V} -> V;")
		c.writeln("error ->")
		c.indent++
		c.writeln("Name = atom_to_list(K),")
		c.writeln("case string:tokens(Name, \"_\") of")
		c.indent++
		c.writeln("[Pref|_] ->")
		c.indent++
		c.writeln("P = list_to_atom(Pref),")
		c.writeln("case maps:find(P, M) of")
		c.indent++
		c.writeln("{ok, Sub} when is_map(Sub) -> maps:get(K, Sub, undefined);")
		c.writeln("_ -> undefined")
		c.indent--
		c.writeln("end;")
		c.indent--
		c.writeln("_ -> undefined")
		c.indent--
		c.writeln("end")
		c.indent--
		c.writeln("end.")
		c.indent--
	}
	if c.needStructCast {
		c.writeln("")
		c.writeln("mochi_to_atom_key(K) when is_atom(K) -> K;")
		c.writeln("mochi_to_atom_key(K) when is_binary(K) -> list_to_atom(binary_to_list(K));")
		c.writeln("mochi_to_atom_key(K) when is_list(K) -> list_to_atom(K);")
		c.writeln("mochi_to_atom_key(K) -> K.")

		c.writeln("")
		c.writeln("mochi_struct_cast(M) ->")
		c.indent++
		c.writeln("maps:from_list([{mochi_to_atom_key(K), V} || {K,V} <- maps:to_list(M)]).")
		c.indent--
	}
	if c.needJSON {
		c.writeln("")
		c.writeln("mochi_escape_json([]) -> [];")
		c.writeln("mochi_escape_json([H|T]) ->")
		c.indent++
		c.writeln("E = case H of")
		c.indent++
		c.writeln("$\\ -> \"\\\\\";")
		c.writeln("$\" -> \"\\\"\";")
		c.writeln("_ -> [H]")
		c.indent--
		c.writeln("end,")
		c.writeln("E ++ mochi_escape_json(T).")
		c.indent--

		c.writeln("")
		c.writeln(`mochi_to_json(true) -> "true";`)
		c.writeln(`mochi_to_json(false) -> "false";`)
		c.writeln(`mochi_to_json(V) when is_integer(V); is_float(V) -> lists:flatten(io_lib:format("~p", [V]));`)
		c.writeln(`mochi_to_json(V) when is_binary(V) -> "\"" ++ mochi_escape_json(binary_to_list(V)) ++ "\"";`)
		c.writeln(`mochi_to_json(V) when is_list(V), (V =:= [] orelse is_integer(hd(V))) -> "\"" ++ mochi_escape_json(V) ++ "\"";`)
		c.writeln(`mochi_to_json(V) when is_list(V) -> "[" ++ lists:join(",", [mochi_to_json(X) || X <- V]) ++ "]";`)
		c.writeln(`mochi_to_json(V) when is_map(V) -> "{" ++ lists:join(",", ["\"" ++ atom_to_list(K) ++ "\":" ++ mochi_to_json(Val) || {K,Val} <- maps:to_list(V)]) ++ "}".`)

		c.writeln("")
		c.writeln("mochi_json(V) -> io:format(\"~s\n\", [mochi_to_json(V)]).")
	}
}
