//go:build slow && ignore

// Solution for SPOJ CMEXPR - Complicated Expressions
// https://www.spoj.com/problems/CMEXPR
package main

import (
	"bufio"
	"fmt"
	"os"
)

type node struct {
	op    byte
	val   byte
	left  *node
	right *node
}

func parse(expr []byte) *node {
	pos := 0
	var parseExpr func() *node
	var parseTerm func() *node
	var parseFactor func() *node

	parseExpr = func() *node {
		n := parseTerm()
		for pos < len(expr) && (expr[pos] == '+' || expr[pos] == '-') {
			op := expr[pos]
			pos++
			r := parseTerm()
			n = &node{op: op, left: n, right: r}
		}
		return n
	}

	parseTerm = func() *node {
		n := parseFactor()
		for pos < len(expr) && (expr[pos] == '*' || expr[pos] == '/') {
			op := expr[pos]
			pos++
			r := parseFactor()
			n = &node{op: op, left: n, right: r}
		}
		return n
	}

	parseFactor = func() *node {
		if expr[pos] == '(' {
			pos++
			n := parseExpr()
			pos++ // skip ')'
			return n
		}
		ch := expr[pos]
		pos++
		return &node{val: ch}
	}

	return parseExpr()
}

func precedence(op byte) int {
	switch op {
	case '+', '-':
		return 1
	case '*', '/':
		return 2
	default:
		return 0
	}
}

func format(n *node, parentOp byte, isLeft bool, w *bufio.Writer) {
	if n.op == 0 {
		w.WriteByte(n.val)
		return
	}
	precP := precedence(parentOp)
	precC := precedence(n.op)
	need := false
	if precC < precP {
		need = true
	} else if precC == precP {
		switch parentOp {
		case '-', '/':
			if !isLeft {
				need = true
			}
		}
	}
	if need {
		w.WriteByte('(')
	}
	format(n.left, n.op, true, w)
	w.WriteByte(n.op)
	format(n.right, n.op, false, w)
	if need {
		w.WriteByte(')')
	}
}

func main() {
	in := bufio.NewReader(os.Stdin)
	out := bufio.NewWriter(os.Stdout)
	var t int
	fmt.Fscan(in, &t)
	for i := 0; i < t; i++ {
		var s string
		fmt.Fscan(in, &s)
		root := parse([]byte(s))
		format(root, 0, true, out)
		if i+1 < t {
			out.WriteByte('\n')
		}
	}
	out.Flush()
}
