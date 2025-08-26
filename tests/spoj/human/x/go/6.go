//go:build slow && ignore

// Solution for SPOJ ARITH - Simple Arithmetics
// https://www.spoj.com/problems/ARITH
package main

import (
	"bufio"
	"fmt"
	"math/big"
	"os"
	"strings"
)

func main() {
	in := bufio.NewReader(os.Stdin)
	out := bufio.NewWriter(os.Stdout)
	var t int
	fmt.Fscan(in, &t)
	for ; t > 0; t-- {
		var expr string
		fmt.Fscan(in, &expr)
		// find operator
		var op byte
		var idx int
		for i := 0; i < len(expr); i++ {
			if expr[i] == '+' || expr[i] == '-' || expr[i] == '*' {
				op = expr[i]
				idx = i
				break
			}
		}
		aStr := expr[:idx]
		bStr := expr[idx+1:]
		a := new(big.Int)
		b := new(big.Int)
		a.SetString(aStr, 10)
		b.SetString(bStr, 10)
		var res big.Int
		switch op {
		case '+':
			res.Add(a, b)
		case '-':
			res.Sub(a, b)
		case '*':
			res.Mul(a, b)
		}
		resStr := res.String()
		opLine := string(op) + bStr

		if op == '+' || op == '-' {
			width := max(len(aStr), len(opLine), len(resStr))
			lineLen := max(len(opLine), len(resStr))
			fmt.Fprintf(out, "%*s\n", width, aStr)
			fmt.Fprintf(out, "%*s\n", width, opLine)
			fmt.Fprintf(out, "%s\n", strings.Repeat(" ", width-lineLen)+strings.Repeat("-", lineLen))
			fmt.Fprintf(out, "%*s\n", width, resStr)
		} else { // multiplication
			// build partial products
			type part struct {
				s     string
				shift int
			}
			parts := make([]part, 0, len(bStr))
			for i := len(bStr) - 1; i >= 0; i-- {
				digit := int(bStr[i] - '0')
				var pStr string
				if digit == 0 {
					pStr = "0"
				} else {
					var p big.Int
					p.Mul(a, big.NewInt(int64(digit)))
					pStr = p.String()
				}
				shift := len(bStr) - 1 - i
				parts = append(parts, part{pStr, shift})
			}
			maxPart := 0
			for _, p := range parts {
				if l := len(p.s) + p.shift; l > maxPart {
					maxPart = l
				}
			}
			width := max(len(aStr), len(opLine), len(resStr), maxPart)
			lineLen := max(len(opLine), len(parts[0].s))
			fmt.Fprintf(out, "%*s\n", width, aStr)
			fmt.Fprintf(out, "%*s\n", width, opLine)
			fmt.Fprintf(out, "%s\n", strings.Repeat(" ", width-lineLen)+strings.Repeat("-", lineLen))
			if len(parts) == 1 { // only one digit in multiplier
				fmt.Fprintf(out, "%*s\n", width, resStr)
			} else {
				for _, p := range parts {
					fmt.Fprintf(out, "%s%s\n", strings.Repeat(" ", width-p.shift-len(p.s)), p.s)
				}
				fmt.Fprintf(out, "%s\n", strings.Repeat("-", width))
				fmt.Fprintf(out, "%s\n", strings.Repeat(" ", width-len(resStr))+resStr)
			}
		}
		if t > 0 {
			fmt.Fprintln(out)
		}
	}
	out.Flush()
}

func max(a int, b ...int) int {
	m := a
	for _, x := range b {
		if x > m {
			m = x
		}
	}
	return m
}
