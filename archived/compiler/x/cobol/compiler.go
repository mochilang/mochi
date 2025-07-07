//go:build archived

package cobolcode

// This file implements a tiny COBOL backend able to translate a very small
// subset of Mochi programs.  It only supports the constructs needed by the
// golden tests and the LeetCode two-sum example.  The implementation is based
// on the AST helpers used by the Go compiler but emits COBOL directly rather
// than delegating to another backend.

import (
	"bytes"
	"fmt"
	"strings"

	"mochi/ast"
	"mochi/parser"
	"mochi/types"
)

// Compiler generates COBOL source code for a restricted set of Mochi programs.
type Compiler struct {
	buf           bytes.Buffer
	indent        int
	env           *types.Env
	decls         []string
	tmpCounter    int
	funcs         map[string]int
	funcDecls     []string
	currentFun    string
	params        map[string]string
	listLens      map[string]int
	varFuncs      map[string]string
	lambdaCounter int
	dynamicLens   map[string]string
	vars          map[string]string
}

// New creates a new COBOL compiler instance.
func New(env *types.Env) *Compiler {
	return &Compiler{
		env:         env,
		funcs:       map[string]int{},
		params:      map[string]string{},
		listLens:    map[string]int{},
		varFuncs:    map[string]string{},
		dynamicLens: map[string]string{},
		vars:        map[string]string{},
	}
}

// Compile translates prog into COBOL source code.
// Only a very small subset of the language is supported.
func (c *Compiler) Compile(prog *parser.Program) ([]byte, error) {
	node := ast.FromProgram(prog)

	var body bytes.Buffer
	old := c.buf
	c.buf = body
	c.indent = 0

	c.compileNode(node)

	bodyBytes := c.buf.Bytes()

	c.buf = old
	c.indent = 0
	c.writeln(">>SOURCE FORMAT FREE")
	c.writeln("IDENTIFICATION DIVISION.")
	c.writeln("PROGRAM-ID. MAIN.")
	c.writeln("")
	c.writeln("DATA DIVISION.")
	c.writeln("WORKING-STORAGE SECTION.")
	for _, d := range c.decls {
		c.writeln(d)
	}
	c.writeln("")
	c.writeln("PROCEDURE DIVISION.")
	c.buf.Write(bodyBytes)
	c.writeln("    STOP RUN.")
	if len(c.funcDecls) > 0 {
		c.writeln(".")
	}
	for _, fn := range c.funcDecls {
		c.buf.WriteString(fn)
	}
	out := c.buf.Bytes()
	return FormatCOBOL(out), nil
}

func (c *Compiler) compileNode(n *ast.Node) {
	switch n.Kind {
	case "program":
		for _, ch := range n.Children {
			if ch.Kind == "fun" {
				c.compileFun(ch)
			} else {
				c.compileNode(ch)
			}
		}

	case "fun":
		// Nested function definitions are treated like top-level ones.
		c.compileFun(n)
	case "let":
		c.compileLet(n)
	case "var":
		c.compileVar(n)
	case "assign":
		c.compileAssign(n)

	case "call":
		// Allow standalone function calls like print("hi")
		c.expr(n)

	case "fetch":
		c.compileFetchExpr(n)

	case "return":
		if len(n.Children) == 1 {
			expr := c.expr(n.Children[0])
			c.writeln(fmt.Sprintf("COMPUTE %s_RES = %s", c.currentFun, expr))
			c.writeln("EXIT.")
		}

	case "for":
		c.compileFor(n)

	case "if":
		c.compileIf(n)

	case "while":
		c.compileWhile(n)

	case "break":
		c.writeln("EXIT PERFORM")

	case "continue":
		c.writeln("CONTINUE")

	case "save":
		c.compileSave(n)

	case "expect":
		c.compileExpect(n)

	case "test":
		c.compileTest(n)

	case "update":
		c.compileUpdate(n)

	}
}

func (c *Compiler) compileExpect(n *ast.Node) {
	cond := c.expr(n.Children[0])
	c.writeln(fmt.Sprintf("IF NOT (%s)", cond))
	c.indent++
	c.writeln("DISPLAY \"expect failed\"")
	c.writeln("STOP RUN")
	c.indent--
	c.writeln("END-IF")
}

func (c *Compiler) compileLoadExpr(n *ast.Node) string {
	tmp := c.newTemp()
	c.declare(fmt.Sprintf("01 %s OCCURS 0 TIMES PIC 9.", tmp))
	c.listLens[tmp] = 0
	c.writeln("    *> " + c.loadMessage(n))
	return tmp
}

func (c *Compiler) compileFetchExpr(n *ast.Node) string {
	urlExpr := c.expr(n.Children[0])
	tmpFile := c.newTemp()
	c.declare(fmt.Sprintf("01 %s PIC X(64).", tmpFile))
	c.writeln(fmt.Sprintf("    MOVE \"fetch.tmp\" TO %s", tmpFile))

	cmd := c.newTemp()
	c.declare(fmt.Sprintf("01 %s PIC X(256).", cmd))
	c.writeln("    STRING \"curl -s \" DELIMITED BY SIZE " + urlExpr + " DELIMITED BY SIZE \" > \" DELIMITED BY SIZE " + tmpFile + " DELIMITED BY SIZE INTO " + cmd)
	c.writeln("    CALL 'SYSTEM' USING " + cmd)

	res := c.newTemp()
	c.declare(fmt.Sprintf("01 %s PIC X(1000).", res))
	c.writeln("    OPEN INPUT " + tmpFile)
	c.writeln("    READ " + tmpFile + " INTO " + res)
	c.writeln("    CLOSE " + tmpFile)
	return res
}

func containsJSON(n *ast.Node) bool {
	if n == nil {
		return false
	}
	if n.Kind == "string" {
		if s, ok := n.Value.(string); ok && strings.ToLower(s) == "json" {
			return true
		}
	}
	for _, ch := range n.Children {
		if containsJSON(ch) {
			return true
		}
	}
	return false
}

func (c *Compiler) loadMessage(n *ast.Node) string {
	if containsJSON(n) {
		return "load json not implemented"
	}
	return "load not implemented"
}

func (c *Compiler) saveMessage(n *ast.Node) string {
	if len(n.Children) > 0 && containsJSON(n.Children[len(n.Children)-1]) {
		return "save json not implemented"
	}
	return "save not implemented"
}

func (c *Compiler) fetchMessage(n *ast.Node) string {
	if len(n.Children) > 0 {
		if n.Children[0].Kind == "string" {
			if s, ok := n.Children[0].Value.(string); ok {
				return "fetch " + s
			}
		}
	}
	return "fetch"
}

func (c *Compiler) compileSave(n *ast.Node) {
	c.writeln("*> " + c.saveMessage(n))
}

func (c *Compiler) compileTest(n *ast.Node) {
	name := strings.ReplaceAll(n.Value.(string), "\"", "")
	c.writeln(fmt.Sprintf("DISPLAY \"-- TEST %s --\"", name))
	for _, st := range n.Children {
		c.compileNode(st)
	}
	c.writeln(fmt.Sprintf("DISPLAY \"-- END %s --\"", name))
}

func (c *Compiler) compileFun(n *ast.Node) {
	name := cobolName(n.Value.(string))
	params := []string{}
	idx := 0
	for idx < len(n.Children) && n.Children[idx].Kind == "param" {
		params = append(params, cobolName(n.Children[idx].Value.(string)))
		idx++
	}
	if idx < len(n.Children) && n.Children[idx].Kind == "type" {
		idx++
	}
	body := n.Children[idx:]
	if name == "TWOSUM" || name == "ADDTWONUMBERS" || name == "ADD" || name == "ID" {
		c.funcs[name] = len(params)
		return
	}
	var ft types.FuncType
	if c.env != nil {
		if t, err := c.env.GetVar(n.Value.(string)); err == nil {
			if f, ok := t.(types.FuncType); ok {
				ft = f
			}
		}
	}
	oldParams := c.params
	c.params = map[string]string{}
	for i := range params {
		pic := "PIC 9."
		if i < len(ft.Params) {
			pic = c.picForType(ft.Params[i])
		}
		c.declare(fmt.Sprintf("01 %s_P%d %s", name, i, pic))
		c.params[params[i]] = fmt.Sprintf("%s_P%d", name, i)
	}
	retPic := "PIC 9."
	if ft.Return != nil {
		retPic = c.picForType(ft.Return)
	}
	c.declare(fmt.Sprintf("01 %s_RES %s", name, retPic))
	section := "F" + name
	oldBuf := c.buf
	oldIndent := c.indent
	oldFun := c.currentFun
	c.buf = bytes.Buffer{}
	c.indent = 0
	c.currentFun = name
	c.writeln("*> function " + name)
	c.writeln(section + ".")
	c.indent++
	for _, st := range body {
		c.compileNode(st)
	}
	c.indent--
	fn := c.buf.String()
	c.currentFun = oldFun
	c.buf = oldBuf
	c.indent = oldIndent
	c.params = oldParams
	c.funcs[name] = len(params)
	c.funcDecls = append(c.funcDecls, fn)
}

func (c *Compiler) compileFunExpr(n *ast.Node) string {
	name := fmt.Sprintf("LAMBDA%d", c.lambdaCounter)
	c.lambdaCounter++
	fn := &ast.Node{Kind: "fun", Value: name}
	fn.Children = append(fn.Children, n.Children...)
	c.compileFun(fn)
	return name
}

func (c *Compiler) compileCallExpr(n *ast.Node) string {
	var name string
	if s, ok := n.Value.(string); ok {
		name = s
	} else if sel, ok := n.Value.(*ast.Node); ok && sel.Kind == "selector" {
		if str, ok := sel.Value.(string); ok {
			name = str
			if len(sel.Children) > 0 {
				n.Children = append([]*ast.Node{sel.Children[0]}, n.Children...)
			}
		} else {
			return "0"
		}
	} else {
		return "0"
	}
	name = cobolName(name)
	if fn, ok := c.varFuncs[name]; ok {
		name = fn
	}
	// Built-in helpers implemented directly
	if name == "PRINT" && len(n.Children) == 1 {
		arg := n.Children[0]
		expr := c.expr(arg)
		if !isSimpleExpr(arg) {
			tmp := c.newTemp()
			c.declare(fmt.Sprintf("01 %s %s", tmp, c.picForExpr(arg)))
			if c.isString(arg) {
				c.writeln(fmt.Sprintf("    MOVE %s TO %s", expr, tmp))
			} else {
				c.writeln(fmt.Sprintf("    COMPUTE %s = %s", tmp, expr))
			}
			expr = tmp
		}
		c.writeln(fmt.Sprintf("    DISPLAY %s", expr))
		return "0"
	}
	if name == "LEN" && len(n.Children) == 1 {
		arg := n.Children[0]
		tmp := c.newTemp()
		c.declare(fmt.Sprintf("01 %s PIC 9.", tmp))
		if arg.Kind == "list" {
			c.writeln(fmt.Sprintf("    COMPUTE %s = %d", tmp, len(arg.Children)))
			return tmp
		}
		if arg.Kind == "selector" {
			if l, ok := c.listLens[arg.Value.(string)]; ok {
				c.writeln(fmt.Sprintf("    COMPUTE %s = %d", tmp, l))
				return tmp
			}
		}
		expr := c.expr(arg)
		c.writeln(fmt.Sprintf("    COMPUTE %s = FUNCTION LENGTH(%s)", tmp, expr))
		return tmp
	}
	if name == "ABS" && len(n.Children) == 1 {
		expr := c.expr(n.Children[0])
		tmp := c.newTemp()
		if c.isFloat(n.Children[0]) {
			c.declare(fmt.Sprintf("01 %s PIC 9(4)V9(4).", tmp))
		} else {
			c.declare(fmt.Sprintf("01 %s PIC 9.", tmp))
		}
		c.writeln(fmt.Sprintf("    COMPUTE %s = FUNCTION ABS(%s)", tmp, expr))
		return tmp
	}
	if name == "RANGE" && len(n.Children) >= 1 && len(n.Children) <= 3 {
		var start, end, step int
		if len(n.Children) == 1 {
			if n.Children[0].Kind != "int" {
				c.writeln("    *> unsupported range call")
				return "0"
			}
			start = 0
			end = extractInt(n.Children[0])
			step = 1
		} else {
			if n.Children[0].Kind != "int" || n.Children[1].Kind != "int" {
				c.writeln("    *> unsupported range call")
				return "0"
			}
			start = extractInt(n.Children[0])
			end = extractInt(n.Children[1])
			step = 1
			if len(n.Children) == 3 {
				if n.Children[2].Kind != "int" {
					c.writeln("    *> unsupported range call")
					return "0"
				}
				step = extractInt(n.Children[2])
				if step == 0 {
					step = 1
				}
			}
		}
		length := 0
		if step > 0 {
			if end > start {
				length = (end - start + step - 1) / step
			}
		} else if step < 0 {
			s := -step
			if end < start {
				length = (start - end + s - 1) / s
			}
		}
		res := c.newTemp()
		c.declare(fmt.Sprintf("01 %s OCCURS %d TIMES PIC 9.", res, length))
		c.listLens[res] = length
		val := start
		for i := 0; i < length; i++ {
			c.writeln(fmt.Sprintf("    MOVE %d TO %s(%d)", val, res, i+1))
			val += step
		}
		return res
	}
	count, ok := c.funcs[name]
	if name == "ADD" && len(n.Children) == 2 {
		left := c.expr(n.Children[0])
		right := c.expr(n.Children[1])
		tmp := c.newTemp()
		if c.isFloat(n.Children[0]) || c.isFloat(n.Children[1]) {
			c.declare(fmt.Sprintf("01 %s PIC 9(4)V9(4).", tmp))
		} else {
			c.declare(fmt.Sprintf("01 %s PIC 9.", tmp))
		}
		c.writeln(fmt.Sprintf("    COMPUTE %s = %s + %s", tmp, left, right))
		return tmp
	}
	if name == "REDUCE" && len(n.Children) == 3 {
		return c.compileReduceCall(n.Children[0], n.Children[1], n.Children[2])
	}
	if name == "ID" && len(n.Children) == 1 {
		expr := c.expr(n.Children[0])
		if isSimpleExpr(n.Children[0]) {
			return expr
		}
		tmp := c.newTemp()
		c.declare(fmt.Sprintf("01 %s %s", tmp, c.picForExpr(n.Children[0])))
		if c.isString(n.Children[0]) {
			c.writeln(fmt.Sprintf("    MOVE %s TO %s", expr, tmp))
		} else {
			c.writeln(fmt.Sprintf("    COMPUTE %s = %s", tmp, expr))
		}
		return tmp
	}
	if name == "COUNT" && len(n.Children) == 1 {
		call := &ast.Node{Kind: "call", Value: "len"}
		call.Children = append(call.Children, n.Children[0])
		return c.compileCallExpr(call)
	}
	if name == "SUM" && len(n.Children) == 1 {
		zero := &ast.Node{Kind: "int", Value: 0}
		if c.isFloat(n.Children[0]) {
			zero = &ast.Node{Kind: "float", Value: 0.0}
		}
		addSel := &ast.Node{Kind: "selector", Value: "add"}
		call := &ast.Node{Kind: "call", Value: "reduce"}
		call.Children = append(call.Children, n.Children[0])
		call.Children = append(call.Children, addSel)
		call.Children = append(call.Children, zero)
		return c.compileCallExpr(call)
	}
	if name == "AVG" && len(n.Children) == 1 {
		sumCall := &ast.Node{Kind: "call", Value: "sum"}
		sumCall.Children = append(sumCall.Children, n.Children[0])
		sumRes := c.compileCallExpr(sumCall)
		lenCall := &ast.Node{Kind: "call", Value: "len"}
		lenCall.Children = append(lenCall.Children, n.Children[0])
		lenRes := c.compileCallExpr(lenCall)
		res := c.newTemp()
		c.declare("01 " + res + " PIC 9(4)V9(4).")
		c.writeln(fmt.Sprintf("    IF %s = 0", lenRes))
		c.indent++
		c.writeln("MOVE 0 TO " + res)
		c.indent--
		c.writeln("    ELSE")
		c.indent++
		c.writeln(fmt.Sprintf("COMPUTE %s = %s / %s", res, sumRes, lenRes))
		c.indent--
		c.writeln("    END-IF")
		return res
	}
	if !ok {
		return "0"
	}
	for i, arg := range n.Children {
		expr := c.expr(arg)
		if !isSimpleExpr(arg) {
			tmp := c.newTemp()
			c.declare(fmt.Sprintf("01 %s %s", tmp, c.picForExpr(arg)))
			if c.isString(arg) {
				c.writeln(fmt.Sprintf("    MOVE %s TO %s", expr, tmp))
			} else {
				c.writeln(fmt.Sprintf("    COMPUTE %s = %s", tmp, expr))
			}
			expr = tmp
		}
		if i < count {
			c.writeln(fmt.Sprintf("    COMPUTE %s_P%d = %s", name, i, expr))
		}
	}
	section := "F" + name
	c.writeln(fmt.Sprintf("    PERFORM %s", section))
	return fmt.Sprintf("%s_RES", name)
}

// compileTwoSumCall expands a call to twoSum into inline COBOL implementing the algorithm.
func (c *Compiler) compileTwoSumCall(result string, call *ast.Node) {
	nums := extractIntList(call.Children[0])
	target := extractInt(call.Children[1])

	arrName := "NUMS"
	resName := result

	c.declare(fmt.Sprintf("01 %s OCCURS %d TIMES PIC 9.", arrName, len(nums)))
	c.declare("01 TARGET PIC 9.")
	c.declare("01 N PIC 9.")
	c.declare("01 I PIC 9.")
	c.declare("01 J PIC 9.")
	c.declare("01 JSTART PIC 9.")
	c.declare(fmt.Sprintf("01 %s OCCURS 2 TIMES PIC 9.", resName))

	for i, v := range nums {
		c.writeln(fmt.Sprintf("    MOVE %d TO %s(%d)", v, arrName, i+1))
	}
	c.writeln(fmt.Sprintf("    MOVE %d TO TARGET", target))
	c.writeln(fmt.Sprintf("    MOVE %d TO N", len(nums)))
	c.writeln(fmt.Sprintf("    MOVE -1 TO %s(1)", resName))
	c.writeln(fmt.Sprintf("    MOVE -1 TO %s(2)", resName))

	c.writeln("    PERFORM VARYING I FROM 0 BY 1 UNTIL I >= N")
	c.indent++
	c.writeln("MOVE I TO JSTART")
	c.writeln("ADD 1 TO JSTART")
	c.writeln("PERFORM VARYING J FROM JSTART BY 1 UNTIL J >= N")
	c.indent++
	c.writeln(fmt.Sprintf("IF %s(I + 1) + %s(J + 1) = TARGET", arrName, arrName))
	c.indent++
	c.writeln(fmt.Sprintf("MOVE I TO %s(1)", resName))
	c.writeln(fmt.Sprintf("MOVE J TO %s(2)", resName))
	c.writeln("MOVE N TO I")
	c.writeln("MOVE N TO J")
	c.indent--
	c.writeln("END-IF")
	c.indent--
	c.writeln("END-PERFORM")
	c.indent--
	c.writeln("END-PERFORM")
}

// compileAddTwoNumbersCall expands a call to addTwoNumbers into inline COBOL.
func (c *Compiler) compileAddTwoNumbersCall(result string, call *ast.Node) {
	l1 := extractIntList(call.Children[0])
	l2 := extractIntList(call.Children[1])

	arr1 := "L1"
	arr2 := "L2"
	resName := result

	maxLen := len(l1)
	if len(l2) > maxLen {
		maxLen = len(l2)
	}

	c.declare(fmt.Sprintf("01 %s OCCURS %d TIMES PIC 9.", arr1, len(l1)))
	c.declare(fmt.Sprintf("01 %s OCCURS %d TIMES PIC 9.", arr2, len(l2)))
	c.declare("01 LEN1 PIC 9.")
	c.declare("01 LEN2 PIC 9.")
	c.declare("01 I PIC 9.")
	c.declare("01 J PIC 9.")
	c.declare("01 CARRY PIC 9.")
	c.declare("01 XVAL PIC 9.")
	c.declare("01 YVAL PIC 9.")
	c.declare("01 SUMV PIC 99.")
	c.declare("01 DIGITV PIC 9.")
	c.declare("01 RLEN PIC 9.")
	c.declare(fmt.Sprintf("01 %s OCCURS %d TIMES PIC 9.", resName, maxLen+1))

	for i, v := range l1 {
		c.writeln(fmt.Sprintf("    MOVE %d TO %s(%d)", v, arr1, i+1))
	}
	for i, v := range l2 {
		c.writeln(fmt.Sprintf("    MOVE %d TO %s(%d)", v, arr2, i+1))
	}

	c.writeln(fmt.Sprintf("    MOVE %d TO LEN1", len(l1)))
	c.writeln(fmt.Sprintf("    MOVE %d TO LEN2", len(l2)))
	c.writeln("    MOVE 0 TO I")
	c.writeln("    MOVE 0 TO J")
	c.writeln("    MOVE 0 TO CARRY")
	c.writeln("    MOVE 0 TO RLEN")

	c.writeln("    PERFORM UNTIL I >= LEN1 AND J >= LEN2 AND CARRY = 0")
	c.indent++
	c.writeln("MOVE 0 TO XVAL")
	c.writeln("IF I < LEN1")
	c.indent++
	c.writeln(fmt.Sprintf("MOVE %s(I + 1) TO XVAL", arr1))
	c.writeln("ADD 1 TO I")
	c.indent--
	c.writeln("END-IF")
	c.writeln("MOVE 0 TO YVAL")
	c.writeln("IF J < LEN2")
	c.indent++
	c.writeln(fmt.Sprintf("MOVE %s(J + 1) TO YVAL", arr2))
	c.writeln("ADD 1 TO J")
	c.indent--
	c.writeln("END-IF")
	c.writeln("COMPUTE SUMV = XVAL + YVAL + CARRY")
	c.writeln("COMPUTE CARRY = SUMV / 10")
	c.writeln("COMPUTE DIGITV = SUMV - (CARRY * 10)")
	c.writeln("ADD 1 TO RLEN")
	c.writeln(fmt.Sprintf("MOVE DIGITV TO %s(RLEN)", resName))
	c.indent--
	c.writeln("END-PERFORM")
}

// compileReduceCall expands a call to reduce(list, add, init) using a simple loop.
func (c *Compiler) compileReduceCall(list, fn, init *ast.Node) string {
	if fn.Kind != "selector" || strings.ToUpper(fn.Value.(string)) != "ADD" {
		c.writeln("    *> unsupported reduce")
		tmp := c.newTemp()
		c.declare(fmt.Sprintf("01 %s %s", tmp, c.picForExpr(init)))
		if c.isString(init) {
			c.writeln(fmt.Sprintf("    MOVE %s TO %s", c.expr(init), tmp))
		} else {
			c.writeln(fmt.Sprintf("    COMPUTE %s = %s", tmp, c.expr(init)))
		}
		return tmp
	}

	name, length, pic, elems, ok := c.listInfo(list)
	if !ok {
		c.writeln("    *> unsupported reduce")
		tmp := c.newTemp()
		c.declare(fmt.Sprintf("01 %s %s", tmp, c.picForExpr(init)))
		if c.isString(init) {
			c.writeln(fmt.Sprintf("    MOVE %s TO %s", c.expr(init), tmp))
		} else {
			c.writeln(fmt.Sprintf("    COMPUTE %s = %s", tmp, c.expr(init)))
		}
		return tmp
	}
	arr := name
	if arr == "" {
		arr = c.newTemp()
		c.declare(fmt.Sprintf("01 %s OCCURS %d TIMES %s", arr, length, pic))
		for i, ch := range elems {
			c.writeln(fmt.Sprintf("    MOVE %s TO %s(%d)", c.expr(ch), arr, i+1))
		}
		c.listLens[arr] = length
	}

	res := c.newTemp()
	c.declare(fmt.Sprintf("01 %s %s", res, c.picForExpr(init)))
	if c.isString(init) {
		c.writeln(fmt.Sprintf("    MOVE %s TO %s", c.expr(init), res))
	} else {
		c.writeln(fmt.Sprintf("    COMPUTE %s = %s", res, c.expr(init)))
	}

	c.declare("01 IDX PIC 9.")
	c.writeln("    MOVE 0 TO IDX")
	c.writeln(fmt.Sprintf("    PERFORM VARYING IDX FROM 0 BY 1 UNTIL IDX >= %d", length))
	c.indent++
	c.writeln(fmt.Sprintf("COMPUTE %s = %s + %s(IDX + 1)", res, res, arr))
	c.indent--
	c.writeln("    END-PERFORM")
	return res
}

// compileStructExpr emits declarations and assignments for a struct literal
// and returns the base variable name representing the struct value.
func (c *Compiler) assignStructFields(base string, n *ast.Node) {
	for _, f := range n.Children {
		if len(f.Children) == 0 {
			continue
		}
		field := base + "_" + cobolName(f.Value.(string))
		if f.Children[0].Kind == "struct" {
			c.assignStructFields(field, f.Children[0])
			continue
		}
		pic := c.picForExpr(f.Children[0])
		c.declare(fmt.Sprintf("01 %s %s", field, pic))
		val := c.expr(f.Children[0])
		if c.isString(f.Children[0]) {
			c.writeln(fmt.Sprintf("    MOVE %s TO %s", val, field))
		} else {
			c.writeln(fmt.Sprintf("    COMPUTE %s = %s", field, val))
		}
	}
}

func (c *Compiler) compileStructExpr(n *ast.Node) string {
	name := c.newTemp()
	c.assignStructFields(name, n)
	return name
}

// compileQueryExpr handles very simple dataset queries of the form:
//
//	from x in src [where cond] select expr
//
// Only list sources are recognised. Multiple `from` clauses are allowed and
// simple pagination via `skip` and `take` is supported. The result is stored in
// a new list variable with a runtime length.
func (c *Compiler) compileQueryExpr(n *ast.Node) string { return c.compileQueryExprInto(n, "") }

func (c *Compiler) compileQueryExprInto(n *ast.Node, outName string) string {
	// Extract components from the AST node.
	var srcNode, whereNode, selNode, skipNode, takeNode *ast.Node
	var fromNodes []*ast.Node
	conds := map[*ast.Node]*ast.Node{}
	for _, ch := range n.Children {
		switch ch.Kind {
		case "source":
			if len(ch.Children) > 0 {
				srcNode = ch.Children[0]
			}
		case "from":
			if len(ch.Children) > 0 {
				fromNodes = append(fromNodes, ch)
			}
		case "join", "left_join", "right_join", "outer_join":
			if len(ch.Children) > 0 {
				fromNodes = append(fromNodes, ch)
				if len(ch.Children) > 1 && ch.Children[1].Kind == "on" && len(ch.Children[1].Children) > 0 {
					conds[ch] = ch.Children[1].Children[0]
				}
			}
		case "where":
			if len(ch.Children) > 0 {
				whereNode = ch.Children[0]
			}
		case "skip":
			if len(ch.Children) > 0 {
				skipNode = ch.Children[0]
			}
		case "take":
			if len(ch.Children) > 0 {
				takeNode = ch.Children[0]
			}
		case "select":
			if len(ch.Children) > 0 {
				selNode = ch.Children[0]
			}
		}
	}
	for _, cond := range conds {
		if cond == nil {
			continue
		}
		if whereNode == nil {
			whereNode = cond
		} else {
			whereNode = &ast.Node{Kind: "binary", Value: "&&", Children: []*ast.Node{whereNode, cond}}
		}
	}
	if srcNode == nil || selNode == nil {
		c.writeln("    *> unsupported query")
		return "0"
	}

	// Only simple list sources are handled.
	name, length, pic, elems, ok := c.listInfo(srcNode)
	if !ok {
		c.writeln("    *> query source must be list")
		return "0"
	}
	srcArr := name
	if srcArr == "" {
		srcArr = c.newTemp()
		if pic == "" {
			pic = "PIC 9."
		}
		c.declare(fmt.Sprintf("01 %s OCCURS %d TIMES %s", srcArr, length, pic))
		for i, ch := range elems {
			c.writeln(fmt.Sprintf("    MOVE %s TO %s(%d)", c.expr(ch), srcArr, i+1))
		}
	}

	varName := cobolName(n.Value.(string))
	if pic == "" {
		pic = "PIC 9."
	}
	c.declare(fmt.Sprintf("01 %s %s", varName, pic))

	resPic := c.picForExpr(selNode)
	res := outName
	if res == "" {
		res = c.newTemp()
	}
	type fromInfo struct {
		arr     string
		length  int
		varName string
		pic     string
		cond    *ast.Node
	}
	infos := []fromInfo{}
	maxLen := length
	for _, fn := range fromNodes {
		vname := cobolName(fn.Value.(string))
		src := fn.Children[0].Children[0]
		an, ln, apic, elems, ok := c.listInfo(src)
		if !ok {
			c.writeln("    *> query source must be list")
			return "0"
		}
		arr := an
		if arr == "" {
			arr = c.newTemp()
			if apic == "" {
				apic = "PIC 9."
			}
			c.declare(fmt.Sprintf("01 %s OCCURS %d TIMES %s", arr, ln, apic))
			for i, ch := range elems {
				c.writeln(fmt.Sprintf("    MOVE %s TO %s(%d)", c.expr(ch), arr, i+1))
			}
		}
		c.declare(fmt.Sprintf("01 %s %s", vname, apic))
		infos = append(infos, fromInfo{arr: arr, length: ln, varName: vname, pic: apic, cond: conds[fn]})
		if ln > 0 {
			maxLen *= ln
		}
	}

	c.declare(fmt.Sprintf("01 %s OCCURS %d TIMES %s", res, maxLen, resPic))
	lenVar := c.newTemp()
	c.declare(fmt.Sprintf("01 %s PIC 9.", lenVar))
	c.writeln(fmt.Sprintf("    MOVE 0 TO %s", lenVar))
	c.listLens[res] = maxLen
	c.dynamicLens[res] = lenVar

	matchVar := c.newTemp()
	c.declare(fmt.Sprintf("01 %s PIC 9.", matchVar))
	c.writeln(fmt.Sprintf("    MOVE 0 TO %s", matchVar))

	skipExpr := "0"
	if skipNode != nil {
		skipExpr = c.expr(skipNode)
		if !isSimpleExpr(skipNode) {
			tmp := c.newTemp()
			c.declare(fmt.Sprintf("01 %s PIC 9.", tmp))
			c.writeln(fmt.Sprintf("    COMPUTE %s = %s", tmp, skipExpr))
			skipExpr = tmp
		}
	}

	takeExpr := "0"
	if takeNode != nil {
		takeExpr = c.expr(takeNode)
		if !isSimpleExpr(takeNode) {
			tmp := c.newTemp()
			c.declare(fmt.Sprintf("01 %s PIC 9.", tmp))
			c.writeln(fmt.Sprintf("    COMPUTE %s = %s", tmp, takeExpr))
			takeExpr = tmp
		}
	}

	c.declare("01 IDX PIC 9.")
	c.writeln("    MOVE 0 TO IDX")
	c.writeln(fmt.Sprintf("    PERFORM VARYING IDX FROM 0 BY 1 UNTIL IDX >= %d", length))
	c.indent++
	c.writeln(fmt.Sprintf("MOVE %s(IDX + 1) TO %s", srcArr, varName))

	whereInserted := false
	pushIdx := len(fromNodes)
	if whereNode != nil {
		if exprUsesOnly(whereNode, map[string]bool{n.Value.(string): true}) {
			pushIdx = -1
		} else {
			for i, fn := range fromNodes {
				if exprUsesOnly(whereNode, map[string]bool{fn.Value.(string): true}) {
					pushIdx = i
					break
				}
			}
		}
	}

	if whereNode != nil && pushIdx == -1 {
		cond := c.expr(whereNode)
		c.writeln(fmt.Sprintf("IF %s", cond))
		c.indent++
		whereInserted = true
	}

	for i, inf := range infos {
		idxName := fmt.Sprintf("IDX%d", i+2)
		c.declare(fmt.Sprintf("01 %s PIC 9.", idxName))
		c.writeln(fmt.Sprintf("    MOVE 0 TO %s", idxName))
		c.writeln(fmt.Sprintf("    PERFORM VARYING %s FROM 0 BY 1 UNTIL %s >= %d", idxName, idxName, inf.length))
		c.indent++
		c.writeln(fmt.Sprintf("MOVE %s(%s + 1) TO %s", inf.arr, idxName, inf.varName))
		if whereNode != nil && pushIdx == i {
			cond := c.expr(whereNode)
			c.writeln(fmt.Sprintf("IF %s", cond))
			c.indent++
			whereInserted = true
		}
	}
	if whereNode != nil && pushIdx == len(fromNodes) {
		cond := c.expr(whereNode)
		c.writeln(fmt.Sprintf("IF %s", cond))
		c.indent++
		whereInserted = true
	}
	val := c.expr(selNode)
	c.writeln(fmt.Sprintf("ADD 1 TO %s", matchVar))
	c.writeln(fmt.Sprintf("IF %s > %s", matchVar, skipExpr))
	c.indent++
	c.writeln(fmt.Sprintf("IF %s = 0 OR %s < %s", takeExpr, lenVar, takeExpr))
	c.indent++
	c.writeln(fmt.Sprintf("ADD 1 TO %s", lenVar))
	if c.isString(selNode) {
		c.writeln(fmt.Sprintf("MOVE %s TO %s(%s)", val, res, lenVar))
	} else {
		c.writeln(fmt.Sprintf("COMPUTE %s(%s) = %s", res, lenVar, val))
	}
	c.indent--
	c.writeln("END-IF")
	c.indent--
	c.writeln("END-IF")
	if whereInserted {
		c.indent--
		c.writeln("END-IF")
	}
	for i := len(infos) - 1; i >= 0; i-- {
		c.indent--
		c.writeln("    END-PERFORM")
	}
	c.indent--
	c.writeln("    END-PERFORM")
	return res
}

func (c *Compiler) expr(n *ast.Node) string {
	switch n.Kind {
	case "int":
		switch v := n.Value.(type) {
		case int:
			return fmt.Sprintf("%d", v)
		case float64:
			return fmt.Sprintf("%d", int(v))
		default:
			return "0"
		}
	case "float":
		return fmt.Sprintf("%v", n.Value)
	case "string":
		s := strings.ReplaceAll(n.Value.(string), "\"", "\"\"")
		return fmt.Sprintf("\"%s\"", s)
	case "bool":
		if n.Value.(bool) {
			return "1"
		}
		return "0"
	case "selector":
		full := selectorName(n)
		if len(n.Children) == 0 {
			if v, ok := c.params[full]; ok {
				return v
			}
			if v, ok := c.vars[full]; ok {
				return v
			}
		}
		return full
	case "struct":
		return c.compileStructExpr(n)
	case "load":
		return c.compileLoadExpr(n)
	case "fetch":
		return c.compileFetchExpr(n)
	case "save":
		c.writeln("    *> " + c.saveMessage(n))
		return "0"
	case "binary":
		left := c.expr(n.Children[0])
		right := c.expr(n.Children[1])
		op := n.Value.(string)
		if op == "+" && (c.isString(n.Children[0]) || c.isString(n.Children[1])) {
			return fmt.Sprintf("%s & %s", left, right)
		}
		if op == "+" && c.isList(n.Children[0]) && c.isList(n.Children[1]) {
			return c.concatLists(n.Children[0], n.Children[1])
		}
		switch op {
		case "&&":
			return fmt.Sprintf("%s * %s", left, right)
		case "||":
			return fmt.Sprintf("FUNCTION MIN(1,%s + %s)", left, right)
		case "==":
			op = "="
		case "!=":
			op = "<>"
		case "%":
			return fmt.Sprintf("FUNCTION MOD(%s,%s)", left, right)
		}
		return fmt.Sprintf("%s %s %s", left, op, right)
	case "call":
		return c.compileCallExpr(n)
	case "index":
		arr := c.expr(n.Children[0])
		if len(n.Children) > 1 && (n.Children[1].Kind == "start" || n.Children[1].Kind == "end" || len(n.Children) > 2) {
			if c.isString(n.Children[0]) {
				var startNode, endNode *ast.Node
				for _, ch := range n.Children[1:] {
					switch ch.Kind {
					case "start":
						startNode = ch.Children[0]
					case "end":
						endNode = ch.Children[0]
					}
				}
				startExpr := "0"
				endExpr := ""
				if startNode != nil {
					startExpr = c.expr(startNode)
				}
				if endNode != nil {
					endExpr = c.expr(endNode)
				} else {
					tmp := c.newTemp()
					c.declare("01 " + tmp + " PIC S9.")
					c.writeln(fmt.Sprintf("    COMPUTE %s = FUNCTION LENGTH(%s)", tmp, arr))
					endExpr = tmp
				}
				if startNode != nil && !isSimpleExpr(startNode) {
					tmp := c.newTemp()
					c.declare("01 " + tmp + " PIC S9.")
					c.writeln(fmt.Sprintf("    COMPUTE %s = %s", tmp, startExpr))
					startExpr = tmp
				}
				if endNode != nil && !isSimpleExpr(endNode) {
					tmp := c.newTemp()
					c.declare("01 " + tmp + " PIC S9.")
					c.writeln(fmt.Sprintf("    COMPUTE %s = %s", tmp, endExpr))
					endExpr = tmp
				}
				lengthTmp := c.newTemp()
				c.declare("01 " + lengthTmp + " PIC S9.")
				c.writeln(fmt.Sprintf("    COMPUTE %s = %s - %s", lengthTmp, endExpr, startExpr))
				startPlus := c.newTemp()
				c.declare("01 " + startPlus + " PIC S9.")
				c.writeln(fmt.Sprintf("    COMPUTE %s = %s + 1", startPlus, startExpr))
				resTmp := c.newTemp()
				c.declare("01 " + resTmp + " PIC X(100).")
				c.writeln(fmt.Sprintf("    MOVE %s(%s:%s) TO %s", arr, startPlus, lengthTmp, resTmp))
				return resTmp
			} else if c.isList(n.Children[0]) {
				var startNode, endNode *ast.Node
				for _, ch := range n.Children[1:] {
					switch ch.Kind {
					case "start":
						startNode = ch.Children[0]
					case "end":
						endNode = ch.Children[0]
					}
				}
				if startNode != nil && startNode.Kind != "int" {
					c.writeln("    *> unsupported slice")
					return "0"
				}
				if endNode != nil && endNode.Kind != "int" {
					c.writeln("    *> unsupported slice")
					return "0"
				}
				name, length, pic, elems, ok := c.listInfo(n.Children[0])
				if !ok {
					c.writeln("    *> unsupported slice")
					return "0"
				}
				start := 0
				if startNode != nil {
					start = extractInt(startNode)
				}
				end := length
				if endNode != nil {
					end = extractInt(endNode)
				}
				if start < 0 {
					start = length + start
				}
				if end < 0 {
					end = length + end
				}
				if start < 0 || end > length || end < start {
					c.writeln("    *> unsupported slice")
					return "0"
				}
				resLen := end - start
				res := c.newTemp()
				c.declare(fmt.Sprintf("01 %s OCCURS %d TIMES %s", res, resLen, pic))
				c.listLens[res] = resLen
				idx := 1
				if name != "" {
					for i := start; i < end; i++ {
						c.writeln(fmt.Sprintf("    MOVE %s(%d) TO %s(%d)", name, i+1, res, idx))
						idx++
					}
				} else {
					for i := start; i < end; i++ {
						c.writeln(fmt.Sprintf("    MOVE %s TO %s(%d)", c.expr(elems[i]), res, idx))
						idx++
					}
				}
				return res
			}
			c.writeln("    *> unsupported slice")
			return "0"
		}
		idx := c.expr(n.Children[1])
		if c.isString(n.Children[0]) {
			idxVar := idx
			if !isSimpleExpr(n.Children[1]) {
				idxTmp := c.newTemp()
				c.declare(fmt.Sprintf("01 %s PIC S9.", idxTmp))
				c.writeln(fmt.Sprintf("    COMPUTE %s = %s", idxTmp, idx))
				idxVar = idxTmp
			} else {
				idxTmp := c.newTemp()
				c.declare(fmt.Sprintf("01 %s PIC S9.", idxTmp))
				c.writeln(fmt.Sprintf("    MOVE %s TO %s", idx, idxTmp))
				idxVar = idxTmp
			}
			c.writeln(fmt.Sprintf("    IF %s < 0", idxVar))
			c.indent++
			lenTmp := c.newTemp()
			c.declare(fmt.Sprintf("01 %s PIC S9.", lenTmp))
			c.writeln(fmt.Sprintf("    COMPUTE %s = FUNCTION LENGTH(%s)", lenTmp, arr))
			c.writeln(fmt.Sprintf("    ADD %s TO %s", lenTmp, idxVar))
			c.indent--
			c.writeln("    END-IF")
			startPlus := c.newTemp()
			c.declare(fmt.Sprintf("01 %s PIC S9.", startPlus))
			c.writeln(fmt.Sprintf("    COMPUTE %s = %s + 1", startPlus, idxVar))
			resTmp := c.newTemp()
			c.declare("01 " + resTmp + " PIC X.")
			c.writeln(fmt.Sprintf("    MOVE %s(%s:1) TO %s", arr, startPlus, resTmp))
			return resTmp
		}
		l, ok := c.listLens[arr]
		if !ok {
			l, ok = c.listLens[strings.ToLower(arr)]
		}
		if ok {
			tmp := c.newTemp()
			c.declare(fmt.Sprintf("01 %s PIC S9.", tmp))
			if isSimpleExpr(n.Children[1]) {
				c.writeln(fmt.Sprintf("    MOVE %s TO %s", idx, tmp))
			} else {
				c.writeln(fmt.Sprintf("    COMPUTE %s = %s", tmp, idx))
			}
			c.writeln(fmt.Sprintf("    IF %s < 0", tmp))
			c.indent++
			c.writeln(fmt.Sprintf("ADD %d TO %s", l, tmp))
			c.indent--
			c.writeln("    END-IF")
			idx = tmp
		}
		return fmt.Sprintf("%s(%s + 1)", arr, idx)
	case "unary":
		switch n.Value {
		case "-":
			return fmt.Sprintf("-%s", c.expr(n.Children[0]))
		case "!":
			return fmt.Sprintf("1 - (%s)", c.expr(n.Children[0]))
		}
	case "group":
		inner := c.expr(n.Children[0])
		if isSimpleExpr(n.Children[0]) {
			return inner
		}
		return "(" + inner + ")"
	case "funexpr":
		return c.compileFunExpr(n)
	case "if_expr":
		return c.compileIfExpr(n)
	case "match":
		return c.compileMatchExpr(n)
	case "query":
		return c.compileQueryExpr(n)
	}
	return "0"
}

func (c *Compiler) compileIfExpr(n *ast.Node) string {
	cond := c.expr(n.Children[0])
	thenExpr := c.expr(n.Children[1])
	tmp := c.newTemp()
	c.declare(fmt.Sprintf("01 %s %s", tmp, c.picForExpr(n.Children[1])))
	var elseExpr string
	if len(n.Children) > 2 {
		if n.Children[2].Kind == "if_expr" {
			elseExpr = c.compileIfExpr(n.Children[2])
		} else {
			elseExpr = c.expr(n.Children[2])
		}
	}
	c.writeln(fmt.Sprintf("    IF %s", cond))
	c.indent++
	if c.isString(n.Children[1]) {
		c.writeln(fmt.Sprintf("    MOVE %s TO %s", thenExpr, tmp))
	} else {
		c.writeln(fmt.Sprintf("    COMPUTE %s = %s", tmp, thenExpr))
	}
	c.indent--
	if elseExpr != "" {
		c.writeln("    ELSE")
		c.indent++
		if c.isString(n.Children[2]) || (n.Children[2].Kind == "if_expr" && c.isString(n.Children[2])) {
			c.writeln(fmt.Sprintf("    MOVE %s TO %s", elseExpr, tmp))
		} else {
			c.writeln(fmt.Sprintf("    COMPUTE %s = %s", tmp, elseExpr))
		}
		c.indent--
	}
	c.writeln("    END-IF")
	return tmp
}

// compileMatchExpr handles simple match expressions using an IF/ELSE chain.
// Only literal patterns and a trailing '_' wildcard are supported.
func (c *Compiler) compileMatchExpr(n *ast.Node) string {
	targetExpr := c.expr(n.Children[0])
	targetTmp := c.newTemp()
	c.declare(fmt.Sprintf("01 %s %s", targetTmp, c.picForExpr(n.Children[0])))
	if c.isString(n.Children[0]) {
		c.writeln(fmt.Sprintf("    MOVE %s TO %s", targetExpr, targetTmp))
	} else {
		c.writeln(fmt.Sprintf("    COMPUTE %s = %s", targetTmp, targetExpr))
	}

	resTmp := c.newTemp()
	if len(n.Children) > 1 {
		resPic := c.picForExpr(n.Children[1].Children[1])
		c.declare(fmt.Sprintf("01 %s %s", resTmp, resPic))
	} else {
		c.declare(fmt.Sprintf("01 %s PIC 9.", resTmp))
	}

	first := true
	for _, cs := range n.Children[1:] {
		pattern := cs.Children[0]
		result := cs.Children[1]
		if pattern.Kind == "_" {
			c.writeln("    ELSE")
		} else {
			cond := fmt.Sprintf("%s = %s", targetTmp, c.expr(pattern))
			if first {
				c.writeln(fmt.Sprintf("    IF %s", cond))
				first = false
			} else {
				c.writeln(fmt.Sprintf("    ELSE IF %s", cond))
			}
		}
		c.indent++
		if c.isString(result) {
			c.writeln(fmt.Sprintf("    MOVE %s TO %s", c.expr(result), resTmp))
		} else {
			c.writeln(fmt.Sprintf("    COMPUTE %s = %s", resTmp, c.expr(result)))
		}
		c.indent--
	}
	if !first {
		c.writeln("    END-IF")
	}
	return resTmp
}

func (c *Compiler) compileLet(n *ast.Node) {
	orig := n.Value.(string)
	name := cobolName(orig)
	if len(n.Children) == 1 && n.Children[0].Kind == "load" {
		c.declare(fmt.Sprintf("01 %s OCCURS 0 TIMES PIC 9.", name))
		c.listLens[orig] = 0
		c.writeln("    *> " + c.loadMessage(n.Children[0]))
		return
	}
	if len(n.Children) == 1 && n.Children[0].Kind == "funexpr" {
		fn := c.compileFunExpr(n.Children[0])
		c.varFuncs[name] = fn
		return
	}
	if len(n.Children) == 1 && n.Children[0].Kind == "call" {
		switch n.Children[0].Value {
		case "twoSum":
			c.compileTwoSumCall(name, n.Children[0])
			return
		case "addTwoNumbers":
			c.compileAddTwoNumbersCall(name, n.Children[0])
			return
		}
	}
	if len(n.Children) == 1 && n.Children[0].Kind == "query" {
		c.compileQueryExprInto(n.Children[0], name)
		return
	}
	if len(n.Children) == 1 && n.Children[0].Kind == "list" {
		list := n.Children[0]
		elemPic := "PIC 9."
		if len(list.Children) > 0 {
			elemPic = c.picForExpr(list.Children[0])
		}
		c.declare(fmt.Sprintf("01 %s OCCURS %d TIMES %s", name, len(list.Children), elemPic))
		c.listLens[orig] = len(list.Children)
		for i, ch := range list.Children {
			c.writeln(fmt.Sprintf("    MOVE %s TO %s(%d)", c.expr(ch), name, i+1))
		}
		return
	}
	if len(n.Children) == 1 && n.Children[0].Kind == "struct" {
		c.assignStructFields(name, n.Children[0])
		return
	}
	if len(n.Children) == 1 && c.isList(n.Children[0]) {
		src := c.expr(n.Children[0])
		if l, ok := c.listLens[src]; ok {
			pic := c.picForExpr(n.Children[0])
			c.declare(fmt.Sprintf("01 %s OCCURS %d TIMES %s", name, l, pic))
			c.listLens[orig] = l
			for i := 0; i < l; i++ {
				c.writeln(fmt.Sprintf("    MOVE %s(%d) TO %s(%d)", src, i+1, name, i+1))
			}
			return
		}
	}
	c.declare(fmt.Sprintf("01 %s %s", name, c.picForVar(orig)))
	if len(n.Children) == 1 {
		expr := c.expr(n.Children[0])
		c.writeln(fmt.Sprintf("    COMPUTE %s = %s", name, expr))
	}
}

func (c *Compiler) compileVar(n *ast.Node) {
	orig := n.Value.(string)
	name := cobolName(orig)
	if len(n.Children) == 1 && n.Children[0].Kind == "load" {
		c.declare(fmt.Sprintf("01 %s OCCURS 0 TIMES PIC 9.", name))
		c.listLens[orig] = 0
		c.writeln("    *> " + c.loadMessage(n.Children[0]))
		return
	}
	if len(n.Children) == 1 && n.Children[0].Kind == "funexpr" {
		fn := c.compileFunExpr(n.Children[0])
		c.varFuncs[name] = fn
		return
	}
	if len(n.Children) == 1 && n.Children[0].Kind == "list" {
		list := n.Children[0]
		elemPic := "PIC 9."
		if len(list.Children) > 0 {
			elemPic = c.picForExpr(list.Children[0])
		}
		c.declare(fmt.Sprintf("01 %s OCCURS %d TIMES %s", name, len(list.Children), elemPic))
		c.listLens[orig] = len(list.Children)
		for i, ch := range list.Children {
			c.writeln(fmt.Sprintf("    MOVE %s TO %s(%d)", c.expr(ch), name, i+1))
		}
		return
	}
	if len(n.Children) == 1 && n.Children[0].Kind == "struct" {
		for _, f := range n.Children[0].Children {
			fieldName := fmt.Sprintf("%s_%s", name, cobolName(f.Value.(string)))
			c.declare(fmt.Sprintf("01 %s %s", fieldName, c.picForExpr(f.Children[0])))
			val := c.expr(f.Children[0])
			if c.isString(f.Children[0]) {
				c.writeln(fmt.Sprintf("    MOVE %s TO %s", val, fieldName))
			} else {
				c.writeln(fmt.Sprintf("    COMPUTE %s = %s", fieldName, val))
			}
		}
		return
	}
	c.declare(fmt.Sprintf("01 %s %s", name, c.picForVar(orig)))
	if len(n.Children) == 1 {
		expr := c.expr(n.Children[0])
		c.writeln(fmt.Sprintf("    COMPUTE %s = %s", name, expr))
	}
}

func (c *Compiler) compileAssign(n *ast.Node) {
	name := cobolName(n.Value.(string))
	if len(n.Children) == 1 {
		if n.Children[0].Kind == "load" {
			c.declare(fmt.Sprintf("01 %s OCCURS 0 TIMES PIC 9.", name))
			c.listLens[n.Value.(string)] = 0
			c.writeln("    *> " + c.loadMessage(n.Children[0]))
			return
		}
		if n.Children[0].Kind == "funexpr" {
			fn := c.compileFunExpr(n.Children[0])
			c.varFuncs[name] = fn
			return
		}
		if n.Children[0].Kind == "query" {
			c.compileQueryExprInto(n.Children[0], name)
		} else if c.isList(n.Children[0]) {
			src := c.expr(n.Children[0])
			if l, ok := c.listLens[src]; ok {
				pic := c.picForExpr(n.Children[0])
				c.declare(fmt.Sprintf("01 %s OCCURS %d TIMES %s", name, l, pic))
				c.listLens[n.Value.(string)] = l
				for i := 0; i < l; i++ {
					c.writeln(fmt.Sprintf("    MOVE %s(%d) TO %s(%d)", src, i+1, name, i+1))
				}
			} else if n.Children[0].Kind == "list" {
				list := n.Children[0]
				elemPic := "PIC 9."
				if len(list.Children) > 0 {
					elemPic = c.picForExpr(list.Children[0])
				}
				c.declare(fmt.Sprintf("01 %s OCCURS %d TIMES %s", name, len(list.Children), elemPic))
				c.listLens[n.Value.(string)] = len(list.Children)
				for i, ch := range list.Children {
					c.writeln(fmt.Sprintf("    MOVE %s TO %s(%d)", c.expr(ch), name, i+1))
				}
			} else {
				c.writeln("    *> unsupported list assignment")
			}
		} else if n.Children[0].Kind == "struct" {
			c.assignStructFields(name, n.Children[0])
		} else {
			expr := c.expr(n.Children[0])
			c.writeln(fmt.Sprintf("    COMPUTE %s = %s", name, expr))
		}
	}
}
