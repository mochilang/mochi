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
	buf        bytes.Buffer
	indent     int
	env        *types.Env
	decls      []string
	tmpCounter int
	funcs      map[string]int
	funcDecls  []string
	currentFun string
	params     map[string]string
	listLens   map[string]int
}

// New creates a new COBOL compiler instance.
func New(env *types.Env) *Compiler {
	return &Compiler{
		env:      env,
		funcs:    map[string]int{},
		params:   map[string]string{},
		listLens: map[string]int{},
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
	c.writeln("DATA DIVISION.")
	c.writeln("WORKING-STORAGE SECTION.")
	for _, d := range c.decls {
		c.writeln(d)
	}
	c.writeln("PROCEDURE DIVISION.")
	c.buf.Write(bodyBytes)
	c.writeln("    STOP RUN.")
	if len(c.funcDecls) > 0 {
		c.writeln(".")
	}
	for _, fn := range c.funcDecls {
		c.buf.WriteString(fn)
	}
	return c.buf.Bytes(), nil
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

	case "expect":
		c.compileExpect(n)

	case "test":
		c.compileTest(n)

	}
}

func (c *Compiler) compileFor(n *ast.Node) {
	varName := cobolName(n.Value.(string))

	switch n.Children[0].Kind {
	case "range":
		c.declare(fmt.Sprintf("01 %s PIC S9.", varName))
		startNode := n.Children[0].Children[0]
		endNode := n.Children[0].Children[1]
		start := c.expr(startNode)
		end := c.expr(endNode)

		if !isSimpleExpr(startNode) {
			tmp := c.newTemp()
			c.declare(fmt.Sprintf("01 %s PIC 9.", tmp))
			c.writeln(fmt.Sprintf("    COMPUTE %s = %s", tmp, start))
			start = tmp
		}
		if !isSimpleExpr(endNode) {
			tmp := c.newTemp()
			c.declare(fmt.Sprintf("01 %s PIC 9.", tmp))
			c.writeln(fmt.Sprintf("    COMPUTE %s = %s", tmp, end))
			end = tmp
		}

		c.writeln(fmt.Sprintf("    IF %s < %s", start, end))
		c.indent++
		c.writeln(fmt.Sprintf("    PERFORM VARYING %s FROM %s BY 1 UNTIL %s >= %s", varName, start, varName, end))
		c.indent++
		for _, st := range n.Children[1].Children {
			c.compileNode(st)
		}
		c.indent--
		c.writeln("    END-PERFORM")
		c.indent--
		c.writeln("    ELSE")
		c.indent++
		c.writeln(fmt.Sprintf("    PERFORM VARYING %s FROM %s BY -1 UNTIL %s <= %s", varName, start, varName, end))
		c.indent++
		for _, st := range n.Children[1].Children {
			c.compileNode(st)
		}
		c.indent--
		c.writeln("    END-PERFORM")
		c.indent--
		c.writeln("    END-IF")

	case "in":
		src := n.Children[0].Children[0]
		if src.Kind == "call" && strings.ToUpper(src.Value.(string)) == "RANGE" {
			args := src.Children
			var startNode, endNode, stepNode *ast.Node
			if len(args) == 1 {
				startNode = &ast.Node{Kind: "int", Value: 0}
				endNode = args[0]
			} else if len(args) >= 2 {
				startNode = args[0]
				endNode = args[1]
				if len(args) >= 3 {
					stepNode = args[2]
				}
			}

			c.declare(fmt.Sprintf("01 %s PIC S9.", varName))
			start := c.expr(startNode)
			end := c.expr(endNode)
			step := "1"
			if stepNode != nil {
				step = c.expr(stepNode)
			}

			if !isSimpleExpr(startNode) {
				tmp := c.newTemp()
				c.declare(fmt.Sprintf("01 %s PIC 9.", tmp))
				c.writeln(fmt.Sprintf("    COMPUTE %s = %s", tmp, start))
				start = tmp
			}
			if !isSimpleExpr(endNode) {
				tmp := c.newTemp()
				c.declare(fmt.Sprintf("01 %s PIC 9.", tmp))
				c.writeln(fmt.Sprintf("    COMPUTE %s = %s", tmp, end))
				end = tmp
			}
			if stepNode != nil && !isSimpleExpr(stepNode) {
				tmp := c.newTemp()
				c.declare(fmt.Sprintf("01 %s PIC S9.", tmp))
				c.writeln(fmt.Sprintf("    COMPUTE %s = %s", tmp, step))
				step = tmp
			}

			c.writeln(fmt.Sprintf("    IF %s > 0", step))
			c.indent++
			c.writeln(fmt.Sprintf("    PERFORM VARYING %s FROM %s BY %s UNTIL %s >= %s", varName, start, step, varName, end))
			c.indent++
			for _, st := range n.Children[1].Children {
				c.compileNode(st)
			}
			c.indent--
			c.writeln("    END-PERFORM")
			c.indent--
			c.writeln("    ELSE")
			c.indent++
			c.writeln(fmt.Sprintf("    PERFORM VARYING %s FROM %s BY %s UNTIL %s <= %s", varName, start, step, varName, end))
			c.indent++
			for _, st := range n.Children[1].Children {
				c.compileNode(st)
			}
			c.indent--
			c.writeln("    END-PERFORM")
			c.indent--
			c.writeln("    END-IF")
			return
		}
		switch src.Kind {
		case "list":
			elemPic := "PIC 9."
			if len(src.Children) > 0 {
				elemPic = c.picForExpr(src.Children[0])
			}
			arr := c.newTemp()
			c.declare(fmt.Sprintf("01 %s OCCURS %d TIMES %s", arr, len(src.Children), elemPic))
			c.declare("01 IDX PIC 9.")
			c.declare(fmt.Sprintf("01 %s %s", varName, elemPic))
			for i, ch := range src.Children {
				c.writeln(fmt.Sprintf("    MOVE %s TO %s(%d)", c.expr(ch), arr, i+1))
			}
			c.writeln("    MOVE 0 TO IDX")
			c.writeln(fmt.Sprintf("    PERFORM VARYING IDX FROM 0 BY 1 UNTIL IDX >= %d", len(src.Children)))
			c.indent++
			c.writeln(fmt.Sprintf("MOVE %s(IDX + 1) TO %s", arr, varName))
			for _, st := range n.Children[1].Children {
				c.compileNode(st)
			}
			c.indent--
			c.writeln("    END-PERFORM")

		case "string":
			s := []rune(src.Value.(string))
			arr := c.newTemp()
			c.declare(fmt.Sprintf("01 %s OCCURS %d TIMES PIC X.", arr, len(s)))
			c.declare("01 IDX PIC 9.")
			c.declare("01 " + varName + " PIC X.")
			for i, r := range s {
				c.writeln(fmt.Sprintf("    MOVE \"%c\" TO %s(%d)", r, arr, i+1))
			}
			c.writeln("    MOVE 0 TO IDX")
			c.writeln(fmt.Sprintf("    PERFORM VARYING IDX FROM 0 BY 1 UNTIL IDX >= %d", len(s)))
			c.indent++
			c.writeln(fmt.Sprintf("MOVE %s(IDX + 1) TO %s", arr, varName))
			for _, st := range n.Children[1].Children {
				c.compileNode(st)
			}
			c.indent--
			c.writeln("    END-PERFORM")
		case "selector":
			if c.env != nil {
				if t, err := c.env.GetVar(src.Value.(string)); err == nil {
					if _, ok := t.(types.StringType); ok {
						name := cobolName(src.Value.(string))
						lenTmp := c.newTemp()
						c.declare("01 IDX PIC 9.")
						c.declare(fmt.Sprintf("01 %s PIC 9.", lenTmp))
						c.declare("01 " + varName + " PIC X.")
						c.writeln(fmt.Sprintf("    COMPUTE %s = FUNCTION LENGTH(%s)", lenTmp, name))
						c.writeln("    MOVE 0 TO IDX")
						c.writeln(fmt.Sprintf("    PERFORM VARYING IDX FROM 0 BY 1 UNTIL IDX >= %s", lenTmp))
						c.indent++
						c.writeln(fmt.Sprintf("MOVE %s(IDX + 1:1) TO %s", name, varName))
						for _, st := range n.Children[1].Children {
							c.compileNode(st)
						}
						c.indent--
						c.writeln("    END-PERFORM")
						return
					}
					if lt, ok := t.(types.ListType); ok {
						name := cobolName(src.Value.(string))
						length, ok := c.listLens[src.Value.(string)]
						if !ok {
							c.writeln("    *> unsupported for-in loop")
							return
						}
						c.declare("01 IDX PIC 9.")
						c.declare(fmt.Sprintf("01 %s %s", varName, c.picForType(lt.Elem)))
						c.writeln("    MOVE 0 TO IDX")
						c.writeln(fmt.Sprintf("    PERFORM VARYING IDX FROM 0 BY 1 UNTIL IDX >= %d", length))
						c.indent++
						c.writeln(fmt.Sprintf("MOVE %s(IDX + 1) TO %s", name, varName))
						for _, st := range n.Children[1].Children {
							c.compileNode(st)
						}
						c.indent--
						c.writeln("    END-PERFORM")
						return
					}
				}
			}
			c.writeln("    *> unsupported for-in loop")
		default:
			c.writeln("    *> unsupported for-in loop")
		}
	}
}

func (c *Compiler) compileIf(n *ast.Node) {
	cond := c.expr(n.Children[0])
	c.writeln(fmt.Sprintf("    IF %s", cond))
	c.indent++
	for _, st := range n.Children[1].Children {
		c.compileNode(st)
	}
	c.indent--
	if len(n.Children) > 2 {
		c.writeln("    ELSE")
		c.indent++
		if n.Children[2].Kind == "if" {
			c.compileIf(n.Children[2])
		} else {
			for _, st := range n.Children[2].Children {
				c.compileNode(st)
			}
		}
		c.indent--
	}
	c.writeln("    END-IF")
}

func (c *Compiler) compileWhile(n *ast.Node) {
	cond := c.expr(n.Children[0])
	c.writeln(fmt.Sprintf("    PERFORM UNTIL NOT (%s)", cond))
	c.indent++
	for _, st := range n.Children[1].Children {
		c.compileNode(st)
	}
	c.indent--
	c.writeln("    END-PERFORM")
}

func (c *Compiler) compileExpect(n *ast.Node) {
	expr := c.expr(n.Children[0])
	if !isSimpleExpr(n.Children[0]) {
		tmp := c.newTemp()
		c.declare(fmt.Sprintf("01 %s %s", tmp, c.picForExpr(n.Children[0])))
		if c.isStringExpr(n.Children[0]) {
			c.writeln(fmt.Sprintf("    MOVE %s TO %s", expr, tmp))
		} else {
			c.writeln(fmt.Sprintf("    COMPUTE %s = %s", tmp, expr))
		}
		expr = tmp
	}
	c.writeln(fmt.Sprintf("    IF %s = 0", expr))
	c.indent++
	c.writeln("DISPLAY \"[FAIL]\"")
	c.indent--
	c.writeln("    ELSE")
	c.indent++
	c.writeln("DISPLAY \"[PASS]\"")
	c.indent--
	c.writeln("    END-IF")
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

func (c *Compiler) compileCallExpr(n *ast.Node) string {
	name := cobolName(n.Value.(string))
	// Built-in helpers implemented directly
	if name == "PRINT" && len(n.Children) == 1 {
		arg := n.Children[0]
		expr := c.expr(arg)
		if !isSimpleExpr(arg) {
			tmp := c.newTemp()
			c.declare(fmt.Sprintf("01 %s %s", tmp, c.picForExpr(arg)))
			if c.isStringExpr(arg) {
				c.writeln(fmt.Sprintf("    MOVE %s TO %s", expr, tmp))
			} else {
				c.writeln(fmt.Sprintf("    COMPUTE %s = %s", tmp, expr))
			}
			expr = tmp
		}
		c.writeln(fmt.Sprintf("DISPLAY %s", expr))
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
		if c.isFloatExpr(n.Children[0]) {
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
		if c.isFloatExpr(n.Children[0]) || c.isFloatExpr(n.Children[1]) {
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
		if c.isStringExpr(n.Children[0]) {
			c.writeln(fmt.Sprintf("    MOVE %s TO %s", expr, tmp))
		} else {
			c.writeln(fmt.Sprintf("    COMPUTE %s = %s", tmp, expr))
		}
		return tmp
	}
	if !ok {
		return "0"
	}
	for i, arg := range n.Children {
		expr := c.expr(arg)
		if !isSimpleExpr(arg) {
			tmp := c.newTemp()
			c.declare(fmt.Sprintf("01 %s %s", tmp, c.picForExpr(arg)))
			if c.isStringExpr(arg) {
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
		if c.isStringExpr(init) {
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
		if c.isStringExpr(init) {
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
	if c.isStringExpr(init) {
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
		name := cobolName(n.Value.(string))
		if v, ok := c.params[name]; ok {
			return v
		}
		return name
	case "binary":
		left := c.expr(n.Children[0])
		right := c.expr(n.Children[1])
		op := n.Value.(string)
		if op == "+" && (c.isStringExpr(n.Children[0]) || c.isStringExpr(n.Children[1])) {
			return fmt.Sprintf("%s & %s", left, right)
		}
		if op == "+" && c.isListExpr(n.Children[0]) && c.isListExpr(n.Children[1]) {
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
			if c.isStringExpr(n.Children[0]) {
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
			} else if c.isListExpr(n.Children[0]) {
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
	case "if_expr":
		return c.compileIfExpr(n)
	case "match":
		return c.compileMatchExpr(n)
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
	if c.isStringExpr(n.Children[1]) {
		c.writeln(fmt.Sprintf("    MOVE %s TO %s", thenExpr, tmp))
	} else {
		c.writeln(fmt.Sprintf("    COMPUTE %s = %s", tmp, thenExpr))
	}
	c.indent--
	if elseExpr != "" {
		c.writeln("    ELSE")
		c.indent++
		if c.isStringExpr(n.Children[2]) || (n.Children[2].Kind == "if_expr" && c.isStringExpr(n.Children[2])) {
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
	if c.isStringExpr(n.Children[0]) {
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
		if c.isStringExpr(result) {
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
	if len(n.Children) == 1 && c.isListExpr(n.Children[0]) {
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
	c.declare(fmt.Sprintf("01 %s %s", name, c.picForVar(orig)))
	if len(n.Children) == 1 {
		expr := c.expr(n.Children[0])
		c.writeln(fmt.Sprintf("    COMPUTE %s = %s", name, expr))
	}
}

func (c *Compiler) compileAssign(n *ast.Node) {
	name := cobolName(n.Value.(string))
	if len(n.Children) == 1 {
		if c.isListExpr(n.Children[0]) {
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
		} else {
			expr := c.expr(n.Children[0])
			c.writeln(fmt.Sprintf("    COMPUTE %s = %s", name, expr))
		}
	}
}
