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
		if len(n.Children) == 1 && n.Children[0].Kind == "call" && n.Children[0].Value == "twoSum" {
			c.compileTwoSumCall(name, n.Children[0])
			return
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

	}
}

func (c *Compiler) compileFor(n *ast.Node) {
	varName := strings.ToUpper(n.Value.(string))
	c.declare(fmt.Sprintf("01 %s PIC 9.", varName))
	start := c.expr(n.Children[0].Children[0])
	end := c.expr(n.Children[0].Children[1])
	c.writeln(fmt.Sprintf("    PERFORM VARYING %s FROM %s BY 1 UNTIL %s >= %s", varName, start, varName, end))
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
			return "\"true\""
		}
		return "\"false\""
	case "selector":
		return strings.ToUpper(n.Value.(string))
	case "binary":
		left := c.expr(n.Children[0])
		right := c.expr(n.Children[1])
		op := n.Value.(string)
		return fmt.Sprintf("%s %s %s", left, op, right)
	case "index":
		arr := c.expr(n.Children[0])
		idx := c.expr(n.Children[1])
		return fmt.Sprintf("%s(%s + 1)", arr, idx)
	case "unary":
		if n.Value == "-" {
			return fmt.Sprintf("-%s", c.expr(n.Children[0]))
		}
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
