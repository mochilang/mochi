//go:build slow

package schemecode

import (
	"fmt"

	"mochi/parser"
)

func (c *Compiler) compileIf(st *parser.IfStmt) error {
	cond, err := c.compileExpr(st.Cond)
	if err != nil {
		return err
	}

	// use (when ...) form when there is no else branch for readability
	if st.ElseIf == nil && len(st.Else) == 0 {
		c.writeln(fmt.Sprintf("(when %s", cond))
		c.indent++
		for _, s := range st.Then {
			if err := c.compileStmt(s); err != nil {
				return err
			}
		}
		c.indent--
		c.writeln(")")
		return nil
	}

	c.writeln(fmt.Sprintf("(if %s", cond))
	c.indent++
	c.writeln("(begin")
	c.indent++
	for _, s := range st.Then {
		if err := c.compileStmt(s); err != nil {
			return err
		}
	}
	c.indent--
	c.writeln(")")
	if st.ElseIf != nil {
		if err := c.compileIf(st.ElseIf); err != nil {
			return err
		}
	} else if len(st.Else) > 0 {
		c.writeln("(begin")
		c.indent++
		for _, s := range st.Else {
			if err := c.compileStmt(s); err != nil {
				return err
			}
		}
		c.indent--
		c.writeln(")")
	} else {
		c.writeln("'()")
	}
	c.indent--
	c.writeln(")")
	return nil
}
