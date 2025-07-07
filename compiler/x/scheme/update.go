package schemecode

import (
	"fmt"
	"sort"

	"mochi/parser"
	"mochi/types"
)

func (c *Compiler) compileUpdate(u *parser.UpdateStmt) error {
	name := sanitizeName(u.Target)
	idx := sanitizeName(name + "_idx")
	c.writeln(fmt.Sprintf("(let loop ((%s 0))", idx))
	c.indent++
	c.writeln(fmt.Sprintf("(if (< %s (length %s))", idx, name))
	c.indent++
	c.writeln("(begin")
	c.indent++
	c.writeln(fmt.Sprintf("(let ((item (list-ref %s %s)))", name, idx))
	c.indent++

	// load struct fields if known
	var fields []string
	if c.env != nil {
		if typ, err := c.env.GetVar(u.Target); err == nil {
			if lt, ok := typ.(types.ListType); ok {
				if st, ok := lt.Elem.(types.StructType); ok {
					fields = st.Order
					if len(fields) == 0 {
						for fname := range st.Fields {
							fields = append(fields, fname)
						}
						sort.Strings(fields)
					}
				}
			}
		}
	}
	if len(fields) > 0 {
		c.writeln("(let (")
		c.indent++
		for _, f := range fields {
			c.writeln(fmt.Sprintf("(%s (map-get item '%s))", sanitizeName(f), sanitizeName(f)))
		}
		c.indent--
		c.writeln(")")
		c.indent++
	}

	if u.Where != nil {
		cond, err := c.compileExpr(u.Where)
		if err != nil {
			return err
		}
		c.writeln(fmt.Sprintf("(when %s", cond))
		c.indent++
		for _, it := range u.Set.Items {
			key, _ := identName(it.Key)
			val, err := c.compileExpr(it.Value)
			if err != nil {
				return err
			}
			c.needMapHelpers = true
			c.writeln(fmt.Sprintf("(set! item (map-set item '%s %s))", sanitizeName(key), val))
		}
		c.indent--
		c.writeln(")")
	} else {
		for _, it := range u.Set.Items {
			key, _ := identName(it.Key)
			val, err := c.compileExpr(it.Value)
			if err != nil {
				return err
			}
			c.needMapHelpers = true
			c.writeln(fmt.Sprintf("(set! item (map-set item '%s %s))", sanitizeName(key), val))
		}
	}

	if len(fields) > 0 {
		c.indent--
		c.writeln(")")
	}

	c.writeln(fmt.Sprintf("(set! %s (list-set %s %s item))", name, name, idx))
	c.writeln(fmt.Sprintf("(loop (+ %s 1))", idx))
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
