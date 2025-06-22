package main

import (
	"fmt"
	"io/ioutil"
	"os"
	"strconv"
)

// This is a tiny experimental self-hosting Go interpreter inspired by c4.c.
// It provides a hand written tokenizer, parser and a very small stack based VM.
// Only a tiny subset of Go is supported. The goal is educational rather than
// production ready; it can interpret simple programs consisting of one main
// function with if/for statements, integer expressions and fmt.Println calls.

// ---- Tokenizer ----

// token represents a lexical token.
type token struct {
	typ tokenType
	lit string
}

type tokenType int

const (
	tokEOF tokenType = iota
	tokIdent
	tokNumber
	tokString
	tokFunc
	tokReturn
	tokIf
	tokElse
	tokFor
	tokLBrace
	tokRBrace
	tokLParen
	tokRParen
	tokSemicolon
	tokAssign
	tokPlus
	tokMinus
	tokMul
	tokDiv
	tokLess
	tokGreater
	tokEqual
	tokNotEqual
	tokComma
	tokUnknown
)

var keywords = map[string]tokenType{
	"func":   tokFunc,
	"return": tokReturn,
	"if":     tokIf,
	"else":   tokElse,
	"for":    tokFor,
}

// tokenizer converts source code into tokens.
type tokenizer struct {
	src []rune
	pos int
}

func newTokenizer(src string) *tokenizer {
	return &tokenizer{src: []rune(src)}
}

func (t *tokenizer) next() token {
	for t.pos < len(t.src) {
		ch := t.src[t.pos]
		// skip whitespace
		if ch == ' ' || ch == '\t' || ch == '\r' || ch == '\n' {
			t.pos++
			continue
		}
		// identifiers or keywords
		if isLetter(ch) || ch == '_' {
			start := t.pos
			for t.pos < len(t.src) && (isLetter(t.src[t.pos]) || isDigit(t.src[t.pos]) || t.src[t.pos] == '_') {
				t.pos++
			}
			lit := string(t.src[start:t.pos])
			if typ, ok := keywords[lit]; ok {
				return token{typ: typ, lit: lit}
			}
			return token{typ: tokIdent, lit: lit}
		}
		// numbers
		if isDigit(ch) {
			start := t.pos
			for t.pos < len(t.src) && isDigit(t.src[t.pos]) {
				t.pos++
			}
			return token{typ: tokNumber, lit: string(t.src[start:t.pos])}
		}
		// string literals
		if ch == '"' {
			t.pos++
			start := t.pos
			for t.pos < len(t.src) && t.src[t.pos] != '"' {
				// simplistic escaping support
				if t.src[t.pos] == '\\' && t.pos+1 < len(t.src) {
					t.pos += 2
					continue
				}
				t.pos++
			}
			lit := string(t.src[start:t.pos])
			if t.pos < len(t.src) {
				t.pos++
			}
			return token{typ: tokString, lit: lit}
		}
		// operators and punctuation
		switch ch {
		case '{':
			t.pos++
			return token{typ: tokLBrace, lit: "{"}
		case '}':
			t.pos++
			return token{typ: tokRBrace, lit: "}"}
		case '(':
			t.pos++
			return token{typ: tokLParen, lit: "("}
		case ')':
			t.pos++
			return token{typ: tokRParen, lit: ")"}
		case ';':
			t.pos++
			return token{typ: tokSemicolon, lit: ";"}
		case ',':
			t.pos++
			return token{typ: tokComma, lit: ","}
		case '+':
			t.pos++
			return token{typ: tokPlus, lit: "+"}
		case '-':
			t.pos++
			return token{typ: tokMinus, lit: "-"}
		case '*':
			t.pos++
			return token{typ: tokMul, lit: "*"}
		case '/':
			// skip line comments
			if t.pos+1 < len(t.src) && t.src[t.pos+1] == '/' {
				t.pos += 2
				for t.pos < len(t.src) && t.src[t.pos] != '\n' {
					t.pos++
				}
				continue
			}
			t.pos++
			return token{typ: tokDiv, lit: "/"}
		case '=':
			if t.pos+1 < len(t.src) && t.src[t.pos+1] == '=' {
				t.pos += 2
				return token{typ: tokEqual, lit: "=="}
			}
			t.pos++
			return token{typ: tokAssign, lit: "="}
		case '<':
			t.pos++
			return token{typ: tokLess, lit: "<"}
		case '>':
			t.pos++
			return token{typ: tokGreater, lit: ">"}
		case '!':
			if t.pos+1 < len(t.src) && t.src[t.pos+1] == '=' {
				t.pos += 2
				return token{typ: tokNotEqual, lit: "!="}
			}
			t.pos++
			return token{typ: tokUnknown, lit: "!"}
		}
		// unknown
		t.pos++
		return token{typ: tokUnknown, lit: string(ch)}
	}
	return token{typ: tokEOF}
}

func isLetter(ch rune) bool {
	return ch >= 'a' && ch <= 'z' || ch >= 'A' && ch <= 'Z'
}

func isDigit(ch rune) bool {
	return ch >= '0' && ch <= '9'
}

// ---- Parser ----

// AST node types

type (
	node interface{}

	stmt interface{ node }
	expr interface{ node }

	program    struct{ body []stmt }
	printStmt  struct{ value expr }
	assignStmt struct {
		name  string
		value expr
	}
	returnStmt struct{ value expr }
	ifStmt     struct {
		cond     expr
		thenBody []stmt
		elseBody []stmt
	}
	forStmt struct {
		cond expr
		body []stmt
	}

	binaryExpr struct {
		op          tokenType
		left, right expr
	}
	identExpr  struct{ name string }
	numberExpr struct{ value int }
)

// parser converts tokens into an AST.
type parser struct {
	toks []token
	pos  int
}

func newParser(toks []token) *parser { return &parser{toks: toks} }

func (p *parser) peek() token {
	if p.pos >= len(p.toks) {
		return token{typ: tokEOF}
	}
	return p.toks[p.pos]
}

func (p *parser) next() token {
	if p.pos >= len(p.toks) {
		return token{typ: tokEOF}
	}
	tok := p.toks[p.pos]
	p.pos++
	return tok
}

func (p *parser) expect(tt tokenType) token {
	tok := p.next()
	if tok.typ != tt {
		panic(fmt.Sprintf("expected %v got %v", tt, tok.typ))
	}
	return tok
}

func (p *parser) parseProgram() program {
	var body []stmt
	// skip everything until 'func'
	for p.peek().typ != tokEOF && p.peek().typ != tokFunc {
		p.next()
	}
	if p.peek().typ == tokFunc {
		p.next()           // func
		p.expect(tokIdent) // main
		p.expect(tokLParen)
		p.expect(tokRParen)
		p.expect(tokLBrace)
		body = p.parseBlock()
		p.expect(tokRBrace)
	}
	return program{body: body}
}

func (p *parser) parseBlock() []stmt {
	var stmts []stmt
	for {
		tok := p.peek()
		switch tok.typ {
		case tokRBrace, tokEOF:
			return stmts
		case tokIf:
			stmts = append(stmts, p.parseIf())
		case tokFor:
			stmts = append(stmts, p.parseFor())
		case tokReturn:
			stmts = append(stmts, p.parseReturn())
		case tokIdent:
			// could be assignment or fmt.Println
			stmts = append(stmts, p.parseIdentStmt())
		default:
			// skip unknown
			p.next()
		}
		// optional semicolon
		if p.peek().typ == tokSemicolon {
			p.next()
		}
	}
}

func (p *parser) parseIf() stmt {
	p.expect(tokIf)
	cond := p.parseExpr()
	p.expect(tokLBrace)
	thenBody := p.parseBlock()
	p.expect(tokRBrace)
	var elseBody []stmt
	if p.peek().typ == tokElse {
		p.next()
		p.expect(tokLBrace)
		elseBody = p.parseBlock()
		p.expect(tokRBrace)
	}
	return &ifStmt{cond: cond, thenBody: thenBody, elseBody: elseBody}
}

func (p *parser) parseFor() stmt {
	p.expect(tokFor)
	cond := p.parseExpr()
	p.expect(tokLBrace)
	body := p.parseBlock()
	p.expect(tokRBrace)
	return &forStmt{cond: cond, body: body}
}

func (p *parser) parseReturn() stmt {
	p.expect(tokReturn)
	expr := p.parseExpr()
	return &returnStmt{value: expr}
}

func (p *parser) parseIdentStmt() stmt {
	// lookahead for fmt.Println
	tok := p.next() // identifier
	if tok.lit == "fmt" && p.peek().typ == tokUnknown && p.peek().lit == "." {
		p.next() // '.'
		name := p.next()
		if name.typ == tokIdent && name.lit == "Println" {
			p.expect(tokLParen)
			arg := p.parseExpr()
			p.expect(tokRParen)
			return &printStmt{value: arg}
		}
	}
	// assignment
	name := tok.lit
	if p.peek().typ == tokAssign {
		p.next()
		value := p.parseExpr()
		return &assignStmt{name: name, value: value}
	}
	return &printStmt{value: &identExpr{name}}
}

func (p *parser) parseExpr() expr {
	return p.parseEquality()
}

func (p *parser) parseEquality() expr {
	left := p.parseAdd()
	for {
		switch p.peek().typ {
		case tokEqual, tokNotEqual, tokLess, tokGreater:
			op := p.next().typ
			right := p.parseAdd()
			left = &binaryExpr{op: op, left: left, right: right}
		default:
			return left
		}
	}
}

func (p *parser) parseAdd() expr {
	left := p.parseMul()
	for {
		switch p.peek().typ {
		case tokPlus, tokMinus:
			op := p.next().typ
			right := p.parseMul()
			left = &binaryExpr{op: op, left: left, right: right}
		default:
			return left
		}
	}
}

func (p *parser) parseMul() expr {
	left := p.parseUnary()
	for {
		switch p.peek().typ {
		case tokMul, tokDiv:
			op := p.next().typ
			right := p.parseUnary()
			left = &binaryExpr{op: op, left: left, right: right}
		default:
			return left
		}
	}
}

func (p *parser) parseUnary() expr {
	tok := p.peek()
	switch tok.typ {
	case tokNumber:
		p.next()
		val, _ := strconv.Atoi(tok.lit)
		return &numberExpr{value: val}
	case tokString:
		p.next()
		// treat string as identifier to be printed directly
		return &identExpr{name: tok.lit}
	case tokIdent:
		p.next()
		return &identExpr{name: tok.lit}
	case tokLParen:
		p.next()
		e := p.parseExpr()
		p.expect(tokRParen)
		return e
	default:
		p.next()
		return &numberExpr{value: 0}
	}
}

// ---- VM ----

type opcode int

const (
	opIMM opcode = iota
	opLOAD
	opSTORE
	opADD
	opSUB
	opMUL
	opDIV
	opEQ
	opNE
	opLT
	opGT
	opPRINT
	opJMP
	opJZ
	opHALT
)

type instr struct {
	op  opcode
	arg int
}

// compiler turns AST into bytecode

type compiler struct {
	consts []string
	vars   map[string]int
	code   []instr
}

func newCompiler() *compiler {
	return &compiler{vars: make(map[string]int)}
}

func (c *compiler) emit(op opcode, arg int) int {
	c.code = append(c.code, instr{op, arg})
	return len(c.code) - 1
}

func (c *compiler) compileProg(p program) {
	for _, s := range p.body {
		c.compileStmt(s)
	}
	c.emit(opHALT, 0)
}

func (c *compiler) compileStmt(s stmt) {
	switch v := s.(type) {
	case *printStmt:
		c.compileExpr(v.value)
		c.emit(opPRINT, 0)
	case *assignStmt:
		c.compileExpr(v.value)
		idx, ok := c.vars[v.name]
		if !ok {
			idx = len(c.vars)
			c.vars[v.name] = idx
		}
		c.emit(opSTORE, idx)
	case *ifStmt:
		c.compileExpr(v.cond)
		jz := c.emit(opJZ, 0)
		for _, st := range v.thenBody {
			c.compileStmt(st)
		}
		if len(v.elseBody) > 0 {
			jmp := c.emit(opJMP, 0)
			c.code[jz].arg = len(c.code)
			for _, st := range v.elseBody {
				c.compileStmt(st)
			}
			c.code[jmp].arg = len(c.code)
		} else {
			c.code[jz].arg = len(c.code)
		}
	case *forStmt:
		start := len(c.code)
		c.compileExpr(v.cond)
		jz := c.emit(opJZ, 0)
		for _, st := range v.body {
			c.compileStmt(st)
		}
		c.emit(opJMP, start)
		c.code[jz].arg = len(c.code)
	case *returnStmt:
		c.compileExpr(v.value)
		c.emit(opHALT, 0)
	}
}

func (c *compiler) compileExpr(e expr) {
	switch v := e.(type) {
	case *numberExpr:
		c.emit(opIMM, v.value)
	case *identExpr:
		if idx, ok := c.vars[v.name]; ok {
			c.emit(opLOAD, idx)
		} else {
			// string constant for printing
			idx := len(c.consts)
			c.consts = append(c.consts, v.name)
			c.emit(opIMM, idx)
		}
	case *binaryExpr:
		c.compileExpr(v.left)
		c.compileExpr(v.right)
		switch v.op {
		case tokPlus:
			c.emit(opADD, 0)
		case tokMinus:
			c.emit(opSUB, 0)
		case tokMul:
			c.emit(opMUL, 0)
		case tokDiv:
			c.emit(opDIV, 0)
		case tokEqual:
			c.emit(opEQ, 0)
		case tokNotEqual:
			c.emit(opNE, 0)
		case tokLess:
			c.emit(opLT, 0)
		case tokGreater:
			c.emit(opGT, 0)
		}
	}
}

// virtual machine

type vm struct {
	code   []instr
	consts []string
	vars   []int
	ip     int
	stack  []int
}

func newVM(c *compiler) *vm {
	return &vm{code: c.code, consts: c.consts, vars: make([]int, len(c.vars))}
}

func (v *vm) push(x int) { v.stack = append(v.stack, x) }
func (v *vm) pop() int {
	n := len(v.stack) - 1
	x := v.stack[n]
	v.stack = v.stack[:n]
	return x
}

func (v *vm) run() {
	for v.ip < len(v.code) {
		ins := v.code[v.ip]
		v.ip++
		switch ins.op {
		case opIMM:
			v.push(ins.arg)
		case opLOAD:
			v.push(v.vars[ins.arg])
		case opSTORE:
			v.vars[ins.arg] = v.pop()
		case opADD:
			b := v.pop()
			a := v.pop()
			v.push(a + b)
		case opSUB:
			b := v.pop()
			a := v.pop()
			v.push(a - b)
		case opMUL:
			b := v.pop()
			a := v.pop()
			v.push(a * b)
		case opDIV:
			b := v.pop()
			a := v.pop()
			v.push(a / b)
		case opEQ:
			b := v.pop()
			a := v.pop()
			if a == b {
				v.push(1)
			} else {
				v.push(0)
			}
		case opNE:
			b := v.pop()
			a := v.pop()
			if a != b {
				v.push(1)
			} else {
				v.push(0)
			}
		case opLT:
			b := v.pop()
			a := v.pop()
			if a < b {
				v.push(1)
			} else {
				v.push(0)
			}
		case opGT:
			b := v.pop()
			a := v.pop()
			if a > b {
				v.push(1)
			} else {
				v.push(0)
			}
		case opPRINT:
			val := v.pop()
			if val >= 0 && val < len(v.consts) {
				fmt.Println(v.consts[val])
			} else {
				fmt.Println(val)
			}
		case opJMP:
			v.ip = ins.arg
		case opJZ:
			cond := v.pop()
			if cond == 0 {
				v.ip = ins.arg
			}
		case opHALT:
			return
		}
	}
}

// ---- driver ----

func runFile(path string) error {
	data, err := ioutil.ReadFile(path)
	if err != nil {
		return err
	}
	t := newTokenizer(string(data))
	var toks []token
	for {
		tok := t.next()
		if tok.typ == tokEOF {
			break
		}
		toks = append(toks, tok)
	}
	p := newParser(toks)
	prog := p.parseProgram()
	c := newCompiler()
	c.compileProg(prog)
	v := newVM(c)
	v.run()
	return nil
}

func main() {
	if len(os.Args) < 2 {
		fmt.Println("usage: go4 <file.go>")
		return
	}
	for _, f := range os.Args[1:] {
		if err := runFile(f); err != nil {
			fmt.Fprintln(os.Stderr, err)
		}
	}
}
