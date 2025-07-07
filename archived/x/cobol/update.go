//go:build archived

package cobolcode

import (
	"fmt"

	"mochi/ast"
	"mochi/types"
)

// compileUpdate handles the `update` statement. Only simple list variables
// of struct type are supported. Unsupported cases emit a comment.
func (c *Compiler) compileUpdate(n *ast.Node) {
	target := n.Value.(string)
	listName := cobolName(target)

	// Determine the list length expression.
	var lengthExpr string
	if l, ok := c.listLens[target]; ok {
		lengthExpr = fmt.Sprintf("%d", l)
	} else if dyn, ok := c.dynamicLens[target]; ok {
		lengthExpr = dyn
	} else {
		c.writeln("    *> unsupported update statement")
		return
	}

	// Determine element struct type.
	var st types.StructType
	if c.env != nil {
		if t, err := c.env.GetVar(target); err == nil {
			if lt, ok := t.(types.ListType); ok {
				if s, ok := lt.Elem.(types.StructType); ok {
					st = s
				}
			}
		}
	}
	if len(st.Fields) == 0 {
		c.writeln("    *> unsupported update statement")
		return
	}

	// Temporary variable holding each element.
	item := c.newTemp()
	c.declare("01 " + item + " PIC 9.")
	for _, f := range st.Order {
		pic := c.picForType(st.Fields[f])
		c.declare(fmt.Sprintf("01 %s_%s %s", item, cobolName(f), pic))
	}

	// Index variable for looping.
	c.declare("01 IDX PIC 9.")
	c.writeln("    MOVE 0 TO IDX")
	c.writeln(fmt.Sprintf("    PERFORM VARYING IDX FROM 0 BY 1 UNTIL IDX >= %s", lengthExpr))
	c.indent++
	c.writeln(fmt.Sprintf("MOVE %s(IDX + 1) TO %s", listName, item))

	// Load fields into scope for condition and assignments.
	oldVars := map[string]string{}
	for _, f := range st.Order {
		oldVars[f] = c.vars[f]
		c.vars[f] = fmt.Sprintf("%s_%s", item, cobolName(f))
	}

	// WHERE clause
	var where *ast.Node
	for _, ch := range n.Children {
		if ch.Kind == "where" && len(ch.Children) > 0 {
			where = ch.Children[0]
			break
		}
	}
	if where != nil {
		cond := c.expr(where)
		c.writeln(fmt.Sprintf("IF %s", cond))
		c.indent++
	}

	// SET clause
	var setMap *ast.Node
	for _, ch := range n.Children {
		if ch.Kind == "set" && len(ch.Children) > 0 {
			if len(ch.Children[0].Children) > 0 {
				setMap = ch.Children[0].Children[0]
			}
			break
		}
	}
	if setMap != nil {
		for _, entry := range setMap.Children {
			if len(entry.Children) != 2 {
				continue
			}
			key := selectorName(entry.Children[0])
			field := fmt.Sprintf("%s_%s", item, key)
			val := c.expr(entry.Children[1])
			if c.isString(entry.Children[1]) {
				c.writeln(fmt.Sprintf("MOVE %s TO %s", val, field))
			} else {
				c.writeln(fmt.Sprintf("COMPUTE %s = %s", field, val))
			}
		}
	}

	if where != nil {
		c.indent--
		c.writeln("END-IF")
	}

	c.writeln(fmt.Sprintf("MOVE %s TO %s(IDX + 1)", item, listName))
	c.indent--
	c.writeln("    END-PERFORM")

	for _, f := range st.Order {
		if v, ok := oldVars[f]; ok && v != "" {
			c.vars[f] = v
		} else {
			delete(c.vars, f)
		}
	}
}
