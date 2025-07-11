//go:build ignore

package main

import (
	"fmt"
	"unicode"
)

// simple integer expression parser supporting + - * / ^ and parentheses
// using recursive descent

// parser struct
type parser struct {
	s   []rune
	pos int
}

func (p *parser) skip() {
	for p.pos < len(p.s) && unicode.IsSpace(p.s[p.pos]) {
		p.pos++
	}
}

func (p *parser) number() int {
	p.skip()
	start := p.pos
	for p.pos < len(p.s) && unicode.IsDigit(p.s[p.pos]) {
		p.pos++
	}
	n := 0
	for _, r := range p.s[start:p.pos] {
		n = n*10 + int(r-'0')
	}
	return n
}

func ipow(a, b int) int {
	res := 1
	for b > 0 {
		if b&1 == 1 {
			res *= a
		}
		a *= a
		b >>= 1
	}
	return res
}

func (p *parser) factor() int {
	p.skip()
	if p.pos < len(p.s) && p.s[p.pos] == '(' {
		p.pos++
		v := p.expr()
		p.skip()
		if p.pos < len(p.s) && p.s[p.pos] == ')' {
			p.pos++
		}
		return v
	}
	if p.pos < len(p.s) && p.s[p.pos] == '-' {
		p.pos++
		return -p.factor()
	}
	return p.number()
}

func (p *parser) power() int {
	v := p.factor()
	for {
		p.skip()
		if p.pos < len(p.s) && p.s[p.pos] == '^' {
			p.pos++
			exp := p.factor()
			v = ipow(v, exp)
		} else {
			break
		}
	}
	return v
}

func (p *parser) term() int {
	v := p.power()
	for {
		p.skip()
		if p.pos < len(p.s) {
			switch p.s[p.pos] {
			case '*':
				p.pos++
				v *= p.power()
				continue
			case '/':
				p.pos++
				v /= p.power()
				continue
			}
		}
		break
	}
	return v
}

func (p *parser) expr() int {
	v := p.term()
	for {
		p.skip()
		if p.pos < len(p.s) {
			switch p.s[p.pos] {
			case '+':
				p.pos++
				v += p.term()
				continue
			case '-':
				p.pos++
				v -= p.term()
				continue
			}
		}
		break
	}
	return v
}

func evalExpr(s string) int {
	p := &parser{s: []rune(s)}
	return p.expr()
}

func main() {
	expr := "2*(3-1)+2*5"
	fmt.Printf("%s = %d\n", expr, evalExpr(expr))
}
