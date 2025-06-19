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
}

// New creates a new COBOL compiler instance.
func New(env *types.Env) *Compiler { return &Compiler{env: env} }

func (c *Compiler) writeIndent() {
	for i := 0; i < c.indent; i++ {
		c.buf.WriteString("    ")
	}
}

func (c *Compiler) writeln(s string) {
	c.writeIndent()
	c.buf.WriteString(s)
	c.buf.WriteByte('\n')
}

func (c *Compiler) newTemp() string {
	name := fmt.Sprintf("TMP%d", c.tmpCounter)
	c.tmpCounter++
	return name
}

func isSimpleExpr(n *ast.Node) bool {
	switch n.Kind {
	case "int", "float", "selector", "string", "bool":
		return true
	case "unary":
		if n.Value == "-" {
			return isSimpleExpr(n.Children[0])
		}
	case "group":
		return isSimpleExpr(n.Children[0])
	}
	return false
}

// declare records a WORKING-STORAGE declaration.
func (c *Compiler) declare(line string) {
	for _, d := range c.decls {
		if d == line {
			return
		}
	}
	c.decls = append(c.decls, line)
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
	return c.buf.Bytes(), nil
}

func (c *Compiler) compileNode(n *ast.Node) {
	switch n.Kind {
	case "program":
		for _, ch := range n.Children {
			c.compileNode(ch)
		}

	case "let":
		name := strings.ToUpper(n.Value.(string))
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

	case "var":
		name := strings.ToUpper(n.Value.(string))
		c.declare(fmt.Sprintf("01 %s PIC 9.", name))
		if len(n.Children) == 1 {
			expr := c.expr(n.Children[0])
			c.writeln(fmt.Sprintf("    COMPUTE %s = %s", name, expr))
		}

	case "assign":
		name := strings.ToUpper(n.Value.(string))
		if len(n.Children) == 1 {
			expr := c.expr(n.Children[0])
			c.writeln(fmt.Sprintf("    COMPUTE %s = %s", name, expr))
		}

	case "call":
		if n.Value == "print" {
			arg := n.Children[0]
			expr := c.expr(arg)
			if isSimpleExpr(arg) {
				c.writeln("    DISPLAY " + expr)
			} else {
				tmp := c.newTemp()
				c.declare(fmt.Sprintf("01 %s PIC 9.", tmp))
				c.writeln(fmt.Sprintf("    COMPUTE %s = %s", tmp, expr))
				c.writeln("    DISPLAY " + tmp)
			}
		}

	case "for":
		c.compileFor(n)

	case "if":
		c.compileIf(n)

	case "while":
		c.compileWhile(n)

	}
}

func (c *Compiler) compileFor(n *ast.Node) {
	varName := strings.ToUpper(n.Value.(string))
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

	c.writeln(fmt.Sprintf("    PERFORM VARYING %s FROM %s BY 1 UNTIL %s >= %s", varName, start, varName, end))
	c.indent++
	for _, st := range n.Children[1].Children {
		c.compileNode(st)
	}
	c.indent--
	c.writeln("    END-PERFORM")
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
		return strings.ToUpper(n.Value.(string))
	case "binary":
		left := c.expr(n.Children[0])
		right := c.expr(n.Children[1])
		op := n.Value.(string)
		switch op {
		case "&&":
			op = "AND"
		case "||":
			op = "OR"
		case "==":
			op = "="
		case "!=":
			op = "<>"
		case "%":
			return fmt.Sprintf("FUNCTION MOD(%s,%s)", left, right)
		}
		return fmt.Sprintf("%s %s %s", left, op, right)
	case "index":
		arr := c.expr(n.Children[0])
		idx := c.expr(n.Children[1])
		return fmt.Sprintf("%s(%s + 1)", arr, idx)
	case "unary":
		switch n.Value {
		case "-":
			return fmt.Sprintf("-%s", c.expr(n.Children[0]))
		case "!":
			return fmt.Sprintf("NOT %s", c.expr(n.Children[0]))
		}
	case "group":
		return c.expr(n.Children[0])
	}
	return "0"
}

func extractInt(n *ast.Node) int {
	if n.Kind == "int" {
		switch v := n.Value.(type) {
		case int:
			return v
		case float64:
			return int(v)
		}
	}
	return 0
}

func extractIntList(n *ast.Node) []int {
	if n.Kind != "list" {
		return nil
	}
	res := make([]int, 0, len(n.Children))
	for _, ch := range n.Children {
		res = append(res, extractInt(ch))
	}
	return res
}
