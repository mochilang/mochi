//go:build slow

package schemecode

import (
	"fmt"

	"mochi/parser"
)

func (c *Compiler) compileFor(st *parser.ForStmt) error {
	name := sanitizeName(st.Name)
	ctrl := hasLoopCtrl(st.Body)
	if st.RangeEnd != nil {
		start, err := c.compileExpr(st.Source)
		if err != nil {
			return err
		}
		end, err := c.compileExpr(st.RangeEnd)
		if err != nil {
			return err
		}
		if !ctrl {
			c.writeln(fmt.Sprintf("(let loop ((%s %s))", name, start))
			c.indent++
			c.writeln(fmt.Sprintf("(if (< %s %s)", name, end))
			c.indent++
			c.writeln("(begin")
			c.indent++
			for _, s := range st.Body {
				if err := c.compileStmt(s); err != nil {
					return err
				}
			}
			c.writeln(fmt.Sprintf("(loop (+ %s 1))", name))
			c.indent--
			c.writeln(")")
			c.indent--
			c.writeln("'()")
			c.indent--
			c.writeln(")")
			c.indent--
			c.writeln(")")
			return nil
		}
		brk := fmt.Sprintf("brk%d", len(c.loops))
		loop := fmt.Sprintf("loop%d", len(c.loops))
		c.writeln("(call/cc (lambda (" + brk + ")")
		c.indent++
		c.writeln("(let " + loop + " ((" + name + " " + start + "))")
		c.indent++
		c.writeln("(when (< " + name + " " + end + ")")
		c.indent++
		c.pushLoop(brk, loop, "(+ "+name+" 1)")
		for _, s := range st.Body {
			if err := c.compileStmt(s); err != nil {
				c.popLoop()
				return err
			}
		}
		c.writeln("(" + loop + " (+ " + name + " 1)))")
		c.popLoop()
		c.indent--
		c.writeln(")")
		c.indent--
		c.writeln(")")
		c.indent--
		c.writeln(")")
		return nil
	}

	src, err := c.compileExpr(st.Source)
	if err != nil {
		return err
	}
	root := rootNameExpr(st.Source)
	isStr := c.varType(root) == "string" || c.isStringExpr(st.Source)
	isMap := c.varType(root) == "map" || c.isMapExpr(st.Source)
	idx := sanitizeName(name + "_idx")
	if isMap {
		src = fmt.Sprintf("(map car %s)", src)
	}
	lenExpr := fmt.Sprintf("(length %s)", src)
	elemExpr := fmt.Sprintf("(list-ref %s %s)", src, idx)
	if isStr {
		lenExpr = fmt.Sprintf("(string-length %s)", src)
		elemExpr = fmt.Sprintf("(string-ref %s %s)", src, idx)
	}
	if !ctrl {
		c.writeln(fmt.Sprintf("(let loop ((%s 0))", idx))
		c.indent++
		c.writeln(fmt.Sprintf("(if (< %s %s)", idx, lenExpr))
		c.indent++
		c.writeln("(begin")
		c.indent++
		if st.Name != "_" {
			c.writeln(fmt.Sprintf("(let ((%s %s))", name, elemExpr))
			c.indent++
			for _, s := range st.Body {
				if err := c.compileStmt(s); err != nil {
					return err
				}
			}
			c.indent--
			c.writeln(")")
		} else {
			for _, s := range st.Body {
				if err := c.compileStmt(s); err != nil {
					return err
				}
			}
		}
		c.writeln(fmt.Sprintf("(loop (+ %s 1))", idx))
		c.indent--
		c.writeln(")")
		c.indent--
		c.writeln("'()")
		c.writeln(")")
		c.indent--
		c.writeln(")")
		return nil
	}
	brk := fmt.Sprintf("brk%d", len(c.loops))
	loop := fmt.Sprintf("loop%d", len(c.loops))
	c.writeln("(call/cc (lambda (" + brk + ")")
	c.indent++
	c.writeln("(let " + loop + " ((" + idx + " 0))")
	c.indent++
	c.writeln("(when (< " + idx + " " + lenExpr + ")")
	c.indent++
	if st.Name != "_" {
		c.writeln(fmt.Sprintf("(let ((%s %s))", name, elemExpr))
		c.indent++
	}
	c.pushLoop(brk, loop, "(+ "+idx+" 1)")
	for _, s := range st.Body {
		if err := c.compileStmt(s); err != nil {
			c.popLoop()
			return err
		}
	}
	c.popLoop()
	if st.Name != "_" {
		c.indent--
		c.writeln(")")
	}
	c.writeln("(" + loop + " (+ " + idx + " 1)))")
	c.indent--
	c.writeln(")")
	c.indent--
	c.writeln(")")
	c.indent--
	c.writeln(")")
	return nil
}

func (c *Compiler) compileWhile(st *parser.WhileStmt) error {
	cond, err := c.compileExpr(st.Cond)
	if err != nil {
		return err
	}
	ctrl := hasLoopCtrl(st.Body)
	if !ctrl {
		c.writeln("(let loop ()")
		c.indent++
		c.writeln(fmt.Sprintf("(if %s", cond))
		c.indent++
		c.writeln("(begin")
		c.indent++
		for _, s := range st.Body {
			if err := c.compileStmt(s); err != nil {
				return err
			}
		}
		c.writeln("(loop)")
		c.indent--
		c.writeln(")")
		c.indent--
		c.writeln("'()")
		c.indent--
		c.writeln(")")
		return nil
	}
	brk := fmt.Sprintf("brk%d", len(c.loops))
	loop := fmt.Sprintf("loop%d", len(c.loops))
	c.writeln("(call/cc (lambda (" + brk + ")")
	c.indent++
	c.writeln("(let " + loop + " ()")
	c.indent++
	c.writeln(fmt.Sprintf("(when %s", cond))
	c.indent++
	c.pushLoop(brk, loop, "")
	for _, s := range st.Body {
		if err := c.compileStmt(s); err != nil {
			c.popLoop()
			return err
		}
	}
	c.writeln("(" + loop + "))")
	c.popLoop()
	c.indent--
	c.writeln(")")
	c.indent--
	c.writeln(")")
	c.indent--
	c.writeln(")")
	return nil
}

func (c *Compiler) compileBreak() error {
	if len(c.loops) == 0 {
		return fmt.Errorf("break not in loop")
	}
	lbl := c.loops[len(c.loops)-1].brk
	c.writeln("(" + lbl + " '())")
	return nil
}

func (c *Compiler) compileContinue() error {
	if len(c.loops) == 0 {
		return fmt.Errorf("continue not in loop")
	}
	ctx := c.loops[len(c.loops)-1]
	if ctx.cont == "" {
		return fmt.Errorf("continue unsupported in this loop")
	}
	if ctx.contArg != "" {
		c.writeln(fmt.Sprintf("(%s %s)", ctx.cont, ctx.contArg))
	} else {
		c.writeln("(" + ctx.cont + ")")
	}
	return nil
}
