package luacode

import (
	"fmt"
	"strings"

	"mochi/parser"
)

func (c *Compiler) compileQueryExprWithHelper(q *parser.QueryExpr) (string, error) {
	src, err := c.compileExpr(q.Source)
	if err != nil {
		return "", err
	}
	varNames := []string{sanitizeName(q.Var)}
	joins := make([]string, 0, len(q.Froms)+len(q.Joins))
	for _, f := range q.Froms {
		fs, err := c.compileExpr(f.Src)
		if err != nil {
			return "", err
		}
		joins = append(joins, fmt.Sprintf("{items=%s}", fs))
		varNames = append(varNames, sanitizeName(f.Var))
	}
	params := append([]string(nil), varNames...)
	for _, j := range q.Joins {
		js, err := c.compileExpr(j.Src)
		if err != nil {
			return "", err
		}
		onParams := append(params, sanitizeName(j.Var))
		onCond, err := c.compileExpr(j.On)
		if err != nil {
			return "", err
		}
		onFn := fmt.Sprintf("function(%s) return %s end", strings.Join(onParams, ", "), onCond)
		spec := fmt.Sprintf("{items=%s, on=%s", js, onFn)
		if j.Side != nil && (*j.Side == "left" || *j.Side == "outer") {
			spec += ", left=true"
		}
		if j.Side != nil && (*j.Side == "right" || *j.Side == "outer") {
			spec += ", right=true"
		}
		spec += "}"
		joins = append(joins, spec)
		params = append(params, sanitizeName(j.Var))
	}
	allParams := strings.Join(params, ", ")
	sel, err := c.compileExpr(q.Select)
	if err != nil {
		return "", err
	}
	selectFn := fmt.Sprintf("function(%s) return %s end", allParams, sel)
	var whereFn, sortFn, skipExpr, takeExpr string
	if q.Where != nil {
		cond, err := c.compileExpr(q.Where)
		if err != nil {
			return "", err
		}
		whereFn = fmt.Sprintf("function(%s) return %s end", allParams, cond)
	}
	if q.Sort != nil {
		sortExpr, err := c.compileExpr(q.Sort)
		if err != nil {
			return "", err
		}
		sortFn = fmt.Sprintf("function(%s) return %s end", allParams, sortExpr)
	}
	if q.Skip != nil {
		skipExpr, err = c.compileExpr(q.Skip)
		if err != nil {
			return "", err
		}
	}
	if q.Take != nil {
		takeExpr, err = c.compileExpr(q.Take)
		if err != nil {
			return "", err
		}
	}
	opts := []string{"select=" + selectFn}
	if whereFn != "" {
		opts = append(opts, "where="+whereFn)
	}
	if sortFn != "" {
		opts = append(opts, "sortKey="+sortFn)
	}
	if skipExpr != "" {
		opts = append(opts, "skip="+skipExpr)
	}
	if takeExpr != "" {
		opts = append(opts, "take="+takeExpr)
	}
	c.helpers["query"] = true
	var b strings.Builder
	b.WriteString("(function()\n")
	b.WriteString(fmt.Sprintf("\tlocal src = %s\n", src))
	b.WriteString(fmt.Sprintf("\treturn __query(src, {%s}, {%s})\n", strings.Join(joins, ", "), strings.Join(opts, ", ")))
	b.WriteString("end)()")
	return b.String(), nil
}
