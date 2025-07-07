//go:build archived

package cobolcode

import (
	"fmt"
	"strings"

	"mochi/ast"
	"mochi/types"
)

// compileFor handles 'for' loops including range-based and for-in loops.
func (c *Compiler) compileFor(n *ast.Node) {
	varName := cobolName(n.Value.(string))
	switch n.Children[0].Kind {
	case "range":
		c.compileForRange(varName, n.Children[0].Children[0], n.Children[0].Children[1], n.Children[1])
	case "in":
		c.compileForIn(varName, n.Children[0].Children[0], n.Children[1])
	}
}

func (c *Compiler) compileForRange(varName string, startNode, endNode *ast.Node, body *ast.Node) {
	c.declare(fmt.Sprintf("01 %s PIC S9.", varName))
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
	for _, st := range body.Children {
		c.compileNode(st)
	}
	c.indent--
	c.writeln("    END-PERFORM")
	c.indent--
	c.writeln("    ELSE")
	c.indent++
	c.writeln(fmt.Sprintf("    PERFORM VARYING %s FROM %s BY -1 UNTIL %s <= %s", varName, start, varName, end))
	c.indent++
	for _, st := range body.Children {
		c.compileNode(st)
	}
	c.indent--
	c.writeln("    END-PERFORM")
	c.indent--
	c.writeln("    END-IF")
}

func (c *Compiler) compileForIn(varName string, src *ast.Node, body *ast.Node) {
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
		for _, st := range body.Children {
			c.compileNode(st)
		}
		c.indent--
		c.writeln("    END-PERFORM")
		c.indent--
		c.writeln("    ELSE")
		c.indent++
		c.writeln(fmt.Sprintf("    PERFORM VARYING %s FROM %s BY %s UNTIL %s <= %s", varName, start, step, varName, end))
		c.indent++
		for _, st := range body.Children {
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
		for _, st := range body.Children {
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
		for _, st := range body.Children {
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
					for _, st := range body.Children {
						c.compileNode(st)
					}
					c.indent--
					c.writeln("    END-PERFORM")
					return
				}
				if lt, ok := t.(types.ListType); ok {
					name := cobolName(src.Value.(string))
					if length, ok := c.listLens[src.Value.(string)]; ok {
						c.declare("01 IDX PIC 9.")
						c.declare(fmt.Sprintf("01 %s %s", varName, c.picForType(lt.Elem)))
						c.writeln("    MOVE 0 TO IDX")
						c.writeln(fmt.Sprintf("    PERFORM VARYING IDX FROM 0 BY 1 UNTIL IDX >= %d", length))
						c.indent++
						c.writeln(fmt.Sprintf("MOVE %s(IDX + 1) TO %s", name, varName))
						for _, st := range body.Children {
							c.compileNode(st)
						}
						c.indent--
						c.writeln("    END-PERFORM")
						return
					}
					if dyn, ok := c.dynamicLens[src.Value.(string)]; ok {
						c.declare("01 IDX PIC 9.")
						c.declare(fmt.Sprintf("01 %s %s", varName, c.picForType(lt.Elem)))
						c.writeln("    MOVE 0 TO IDX")
						c.writeln(fmt.Sprintf("    PERFORM VARYING IDX FROM 0 BY 1 UNTIL IDX >= %s", dyn))
						c.indent++
						c.writeln(fmt.Sprintf("MOVE %s(IDX + 1) TO %s", name, varName))
						for _, st := range body.Children {
							c.compileNode(st)
						}
						c.indent--
						c.writeln("    END-PERFORM")
						return
					}
					c.writeln("    *> unsupported for-in loop")
					return
				}
			}
		}
		c.writeln("    *> unsupported for-in loop")
	default:
		c.writeln("    *> unsupported for-in loop")
	}
}

// compileWhile emits a simple while loop.
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
