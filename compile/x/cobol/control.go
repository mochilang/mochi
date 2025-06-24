package cobolcode

import (
	"fmt"

	"mochi/ast"
	"mochi/types"
)

// compileIf emits a simple IF/ELSE chain.
func (c *Compiler) compileIf(n *ast.Node) {
	condExpr := c.expr(n.Children[0])
	cond := condExpr
	if _, ok := c.inferType(n.Children[0]).(types.BoolType); !ok {
		cond = fmt.Sprintf("%s <> 0", condExpr)
	}
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
