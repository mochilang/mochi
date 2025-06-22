package main

import (
	"fmt"
	"io/ioutil"
	"os"
	"strconv"
)

const (
	tokEOF = iota
	tokIdent
	tokNumber
	tokLParen
	tokRParen
	tokLBrace
	tokRBrace
	tokSemicolon
	tokAssign
	tokPlus
	tokMinus
	tokStar
	tokSlash
	tokComma
)

type token struct {
	typ int
	lit string
}

type node interface{}
type exprNode interface{}

// AST types
type numberNode struct{ val int }
type identNode struct{ name string }
type binaryNode struct {
	op          int
	left, right exprNode
}
type printNode struct{ value exprNode }
type assignNode struct {
	name  string
	value exprNode
}
type blockNode struct{ stmts []node }

func tokenize(src string) []token {
	isSpace := func(b byte) bool { return b == ' ' || b == '\n' || b == '\t' || b == '\r' }
	isLetter := func(b byte) bool { return (b >= 'a' && b <= 'z') || (b >= 'A' && b <= 'Z') || b == '_' }
	isDigit := func(b byte) bool { return b >= '0' && b <= '9' }

	var tokens []token
	for i := 0; i < len(src); {
		c := src[i]
		if isSpace(c) {
			i++
			continue
		}
		if isLetter(c) {
			start := i
			for i < len(src) && (isLetter(src[i]) || isDigit(src[i])) {
				i++
			}
			tokens = append(tokens, token{tokIdent, src[start:i]})
			continue
		}
		if isDigit(c) {
			start := i
			for i < len(src) && isDigit(src[i]) {
				i++
			}
			tokens = append(tokens, token{tokNumber, src[start:i]})
			continue
		}
		switch c {
		case '(':
			tokens = append(tokens, token{tokLParen, "("})
		case ')':
			tokens = append(tokens, token{tokRParen, ")"})
		case '{':
			tokens = append(tokens, token{tokLBrace, "{"})
		case '}':
			tokens = append(tokens, token{tokRBrace, "}"})
		case ';':
			tokens = append(tokens, token{tokSemicolon, ";"})
		case '+':
			tokens = append(tokens, token{tokPlus, "+"})
		case '-':
			tokens = append(tokens, token{tokMinus, "-"})
		case '*':
			tokens = append(tokens, token{tokStar, "*"})
		case '/':
			tokens = append(tokens, token{tokSlash, "/"})
		case '=':
			tokens = append(tokens, token{tokAssign, "="})
		case ',':
			tokens = append(tokens, token{tokComma, ","})
		}
		i++
	}
	return append(tokens, token{typ: tokEOF})
}

func parse(tokens []token) *blockNode {
	type parser struct {
		tokens []token
		pos    int
	}
	p := &parser{tokens: tokens}

	peek := func() token {
		if p.pos >= len(p.tokens) {
			return token{typ: tokEOF}
		}
		return p.tokens[p.pos]
	}
	next := func() token {
		t := peek()
		if p.pos < len(p.tokens) {
			p.pos++
		}
		return t
	}

	var parseExpr func() exprNode
	parseNumber := func() exprNode { v, _ := strconv.Atoi(next().lit); return &numberNode{val: v} }
	parseFactor := func() exprNode {
		t := peek()
		switch t.typ {
		case tokNumber:
			return parseNumber()
		case tokIdent:
			next()
			return &identNode{name: t.lit}
		case tokLParen:
			next()
			e := parseExpr()
			next()
			return e
		}
		next()
		return &numberNode{val: 0}
	}
	parseTerm := func() exprNode {
		e := parseFactor()
		for {
			t := peek()
			if t.typ == tokStar || t.typ == tokSlash {
				next()
				e = &binaryNode{op: t.typ, left: e, right: parseFactor()}
			} else {
				break
			}
		}
		return e
	}
	parseExpr = func() exprNode {
		e := parseTerm()
		for {
			t := peek()
			if t.typ == tokPlus || t.typ == tokMinus {
				next()
				e = &binaryNode{op: t.typ, left: e, right: parseTerm()}
			} else {
				break
			}
		}
		return e
	}
	parseStatement := func() node {
		t := peek()
		if t.typ == tokIdent && t.lit == "fmt" {
			next()
			next()
			next()
			val := parseExpr()
			next()
			if peek().typ == tokSemicolon {
				next()
			}
			return &printNode{value: val}
		}
		if t.typ == tokIdent {
			name := t.lit
			next()
			if peek().typ == tokAssign {
				next()
			}
			val := parseExpr()
			if peek().typ == tokSemicolon {
				next()
			}
			return &assignNode{name: name, value: val}
		}
		next()
		return nil
	}
	parseBlock := func() *blockNode {
		var stmts []node
		if peek().typ == tokLBrace {
			next()
		}
		for peek().typ != tokRBrace && peek().typ != tokEOF {
			if s := parseStatement(); s != nil {
				stmts = append(stmts, s)
			}
		}
		if peek().typ == tokRBrace {
			next()
		}
		return &blockNode{stmts: stmts}
	}

	for peek().typ != tokLBrace && peek().typ != tokEOF {
		next()
	}
	return parseBlock()
}

func run(b *blockNode, dump bool) {
	const (
		opPush = iota
		opLoad
		opStore
		opAdd
		opSub
		opMul
		opDiv
		opPrint
	)

	type instr struct {
		op   int
		val  int
		name string
	}

	var code []instr
	var compileExpr func(exprNode)
	compileExpr = func(e exprNode) {
		switch n := e.(type) {
		case *numberNode:
			code = append(code, instr{op: opPush, val: n.val})
		case *identNode:
			code = append(code, instr{op: opLoad, name: n.name})
		case *binaryNode:
			compileExpr(n.left)
			compileExpr(n.right)
			op := opAdd
			switch n.op {
			case tokPlus:
				op = opAdd
			case tokMinus:
				op = opSub
			case tokStar:
				op = opMul
			case tokSlash:
				op = opDiv
			}
			code = append(code, instr{op: op})
		}
	}
	compileStmt := func(s node) {
		switch n := s.(type) {
		case *assignNode:
			compileExpr(n.value)
			code = append(code, instr{op: opStore, name: n.name})
		case *printNode:
			compileExpr(n.value)
			code = append(code, instr{op: opPrint})
		}
	}
	for _, s := range b.stmts {
		compileStmt(s)
	}

	if dump {
		names := map[int]string{
			opPush:  "PUSH",
			opLoad:  "LOAD",
			opStore: "STORE",
			opAdd:   "ADD",
			opSub:   "SUB",
			opMul:   "MUL",
			opDiv:   "DIV",
			opPrint: "PRINT",
		}
		for _, ins := range code {
			switch ins.op {
			case opPush:
				fmt.Printf("%s %d\n", names[ins.op], ins.val)
			case opLoad, opStore:
				fmt.Printf("%s %s\n", names[ins.op], ins.name)
			default:
				fmt.Println(names[ins.op])
			}
		}
		return
	}

	stack := []int{}
	env := map[string]int{}
	for pc := 0; pc < len(code); pc++ {
		ins := code[pc]
		switch ins.op {
		case opPush:
			stack = append(stack, ins.val)
		case opLoad:
			stack = append(stack, env[ins.name])
		case opStore:
			v := stack[len(stack)-1]
			stack = stack[:len(stack)-1]
			env[ins.name] = v
		case opAdd, opSub, opMul, opDiv:
			b := stack[len(stack)-1]
			stack = stack[:len(stack)-1]
			a := stack[len(stack)-1]
			stack = stack[:len(stack)-1]
			var r int
			switch ins.op {
			case opAdd:
				r = a + b
			case opSub:
				r = a - b
			case opMul:
				r = a * b
			case opDiv:
				if b == 0 {
					r = 0
				} else {
					r = a / b
				}
			}
			stack = append(stack, r)
		case opPrint:
			v := stack[len(stack)-1]
			stack = stack[:len(stack)-1]
			fmt.Println(v)
		}
	}
}

func main() {
	args := os.Args[1:]
	if len(args) == 0 {
		fmt.Println("usage: go4 file.go")
		return
	}
	if args[0] == "-s" && len(args) > 1 {
		bts, _ := ioutil.ReadFile(args[1])
		run(parse(tokenize(string(bts))), true)
		return
	}
	for _, f := range args {
		bts, err := ioutil.ReadFile(f)
		if err != nil {
			fmt.Println(err)
			continue
		}
		run(parse(tokenize(string(bts))), false)
	}
}
