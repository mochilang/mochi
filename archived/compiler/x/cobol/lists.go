//go:build archived

package cobolcode

import (
	"fmt"

	"mochi/ast"
	"mochi/types"
)

// listInfo extracts information about a list expression. It returns the COBOL
// variable name if the list is stored in a variable, the list length, the
// element picture clause and the literal elements if any.
func (c *Compiler) listInfo(n *ast.Node) (name string, length int, pic string, elems []*ast.Node, ok bool) {
	switch n.Kind {
	case "list":
		length = len(n.Children)
		if length > 0 {
			pic = c.picForExpr(n.Children[0])
		} else {
			pic = "PIC 9."
		}
		elems = n.Children
		ok = true
		return
	case "selector":
		if l, ok2 := c.listLens[n.Value.(string)]; ok2 {
			length = l
			ok = true
		}
		if c.env != nil {
			if t, err := c.env.GetVar(n.Value.(string)); err == nil {
				if lt, ok2 := t.(types.ListType); ok2 {
					if pic == "" {
						pic = c.picForType(lt.Elem)
					}
					ok = true
				}
			}
		}
		name = cobolName(n.Value.(string))
		if pic == "" {
			pic = "PIC 9."
		}
		return
	}
	return
}

// concatLists emits COBOL code to concatenate two lists and returns the result
// variable name.
func (c *Compiler) concatLists(left, right *ast.Node) string {
	ln, llen, lpic, lelems, ok1 := c.listInfo(left)
	rn, rlen, rpic, relems, ok2 := c.listInfo(right)
	if !ok1 || !ok2 {
		c.writeln("    *> unsupported list concatenation")
		return "0"
	}
	pic := lpic
	if pic == "" {
		pic = rpic
	}
	res := c.newTemp()
	c.declare(fmt.Sprintf("01 %s OCCURS %d TIMES %s", res, llen+rlen, pic))
	c.listLens[res] = llen + rlen
	idx := 1
	if ln != "" {
		for i := 0; i < llen; i++ {
			c.writeln(fmt.Sprintf("    MOVE %s(%d) TO %s(%d)", ln, i+1, res, idx))
			idx++
		}
	} else {
		for _, ch := range lelems {
			c.writeln(fmt.Sprintf("    MOVE %s TO %s(%d)", c.expr(ch), res, idx))
			idx++
		}
	}
	if rn != "" {
		for i := 0; i < rlen; i++ {
			c.writeln(fmt.Sprintf("    MOVE %s(%d) TO %s(%d)", rn, i+1, res, idx))
			idx++
		}
	} else {
		for _, ch := range relems {
			c.writeln(fmt.Sprintf("    MOVE %s TO %s(%d)", c.expr(ch), res, idx))
			idx++
		}
	}
	return res
}
