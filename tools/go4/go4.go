//go:build interpreter

package main

import (
	"fmt"
	"io/ioutil"
	"os"
	"strconv"
	"strings"
)

// token represents a lexical token.
type token int

const (
	tokEOF token = iota
	tokIdent
	tokNumber
	tokString
	tokFunc
	tokReturn
	tokIf
	tokElse
	tokLbrace
	tokRbrace
	tokLparen
	tokRparen
	tokSemicolon
	tokAssign
	tokEqual
	tokPlus
	tokMinus
	tokComma
)

// scanner turns a source string into tokens without using go/parser.
type scanner struct {
	src []rune
	pos int
	tok token
	lit string
}

func isLetter(r rune) bool {
	return (r >= 'a' && r <= 'z') || (r >= 'A' && r <= 'Z') || r == '_'
}

func isDigit(r rune) bool {
	return r >= '0' && r <= '9'
}

func (s *scanner) next() {
	for s.pos < len(s.src) && (s.src[s.pos] == ' ' || s.src[s.pos] == '\t' || s.src[s.pos] == '\n' || s.src[s.pos] == '\r') {
		s.pos++
	}
	if s.pos >= len(s.src) {
		s.tok = tokEOF
		s.lit = ""
		return
	}
	ch := s.src[s.pos]
	switch ch {
	case '{':
		s.tok, s.lit = tokLbrace, "{"
		s.pos++
	case '}':
		s.tok, s.lit = tokRbrace, "}"
		s.pos++
	case '(':
		s.tok, s.lit = tokLparen, "("
		s.pos++
	case ')':
		s.tok, s.lit = tokRparen, ")"
		s.pos++
	case ';':
		s.tok, s.lit = tokSemicolon, ";"
		s.pos++
	case ',':
		s.tok, s.lit = tokComma, ","
		s.pos++
	case '+':
		s.tok, s.lit = tokPlus, "+"
		s.pos++
	case '-':
		s.tok, s.lit = tokMinus, "-"
		s.pos++
	case '=':
		if s.pos+1 < len(s.src) && s.src[s.pos+1] == '=' {
			s.tok, s.lit = tokEqual, "=="
			s.pos += 2
		} else {
			s.tok, s.lit = tokAssign, "="
			s.pos++
		}
	case '"':
		start := s.pos + 1
		s.pos++
		for s.pos < len(s.src) && s.src[s.pos] != '"' {
			if s.src[s.pos] == '\\' && s.pos+1 < len(s.src) {
				s.pos += 2
			} else {
				s.pos++
			}
		}
		if s.pos < len(s.src) {
			s.lit = string(s.src[start:s.pos])
			s.pos++
		}
		s.tok = tokString
	default:
		if isLetter(ch) {
			start := s.pos
			for s.pos < len(s.src) && (isLetter(s.src[s.pos]) || isDigit(s.src[s.pos]) || s.src[s.pos] == '.') {
				s.pos++
			}
			s.lit = string(s.src[start:s.pos])
			switch s.lit {
			case "func":
				s.tok = tokFunc
			case "return":
				s.tok = tokReturn
			case "if":
				s.tok = tokIf
			case "else":
				s.tok = tokElse
			default:
				s.tok = tokIdent
			}
		} else if isDigit(ch) {
			start := s.pos
			for s.pos < len(s.src) && isDigit(s.src[s.pos]) {
				s.pos++
			}
			s.lit = string(s.src[start:s.pos])
			s.tok = tokNumber
		} else {
			// unknown char, skip
			s.pos++
			s.next()
		}
	}
}

// AST nodes
type (
	expr interface{}
	stmt interface{}

	ident   struct{ name string }
	intLit  struct{ val int }
	strLit  struct{ val string }
	binExpr struct {
		op          token
		left, right expr
	}
	callExpr struct {
		name string
		args []expr
	}

	assignStmt struct {
		name  string
		value expr
	}
	returnStmt struct{ value expr }
	ifStmt     struct {
		cond      expr
		then, els []stmt
	}
	exprStmt  struct{ e expr }
	blockStmt struct{ body []stmt }

	funcDecl struct {
		name string
		body []stmt
	}
)

type parser struct {
	s     scanner
	funcs map[string]*funcDecl
}

func newParser(src string) *parser {
	p := &parser{funcs: make(map[string]*funcDecl)}
	p.s.src = []rune(src)
	p.s.next()
	return p
}

func (p *parser) expect(t token) {
	if p.s.tok != t {
		panic("unexpected token: " + p.s.lit)
	}
	p.s.next()
}

func (p *parser) parseIdent() string {
	if p.s.tok != tokIdent {
		panic("identifier expected")
	}
	v := p.s.lit
	p.s.next()
	return v
}

func (p *parser) parseExpr() expr {
	var e expr
	switch p.s.tok {
	case tokNumber:
		v, _ := strconv.Atoi(p.s.lit)
		e = intLit{v}
		p.s.next()
	case tokString:
		e = strLit{p.s.lit}
		p.s.next()
	case tokIdent:
		name := p.s.lit
		p.s.next()
		if p.s.tok == tokLparen {
			p.s.next()
			var args []expr
			for p.s.tok != tokRparen {
				args = append(args, p.parseExpr())
				if p.s.tok == tokComma {
					p.s.next()
				} else {
					break
				}
			}
			p.expect(tokRparen)
			e = callExpr{name, args}
		} else {
			e = ident{name}
		}
	default:
		panic("bad expression")
	}

	for p.s.tok == tokPlus || p.s.tok == tokMinus || p.s.tok == tokEqual {
		op := p.s.tok
		p.s.next()
		right := p.parseExpr()
		e = binExpr{op: op, left: e, right: right}
	}
	return e
}

func (p *parser) parseStmt() stmt {
	switch p.s.tok {
	case tokIf:
		p.s.next()
		var cond expr
		if p.s.tok == tokLparen {
			p.s.next()
			cond = p.parseExpr()
			p.expect(tokRparen)
		} else {
			cond = p.parseExpr()
		}
		thenBlock := p.parseBlock()
		var elseBlock []stmt
		if p.s.tok == tokElse {
			p.s.next()
			elseBlock = p.parseBlock()
		}
		return ifStmt{cond: cond, then: thenBlock, els: elseBlock}
	case tokReturn:
		p.s.next()
		var val expr
		if p.s.tok != tokSemicolon && p.s.tok != tokRbrace {
			val = p.parseExpr()
		}
		if p.s.tok == tokSemicolon {
			p.s.next()
		}
		return returnStmt{value: val}
	case tokIdent:
		name := p.s.lit
		p.s.next()
		if p.s.tok == tokAssign {
			p.s.next()
			value := p.parseExpr()
			if p.s.tok == tokSemicolon {
				p.s.next()
			}
			return assignStmt{name: name, value: value}
		} else if p.s.tok == tokLparen {
			p.s.next()
			var args []expr
			for p.s.tok != tokRparen {
				args = append(args, p.parseExpr())
				if p.s.tok == tokComma {
					p.s.next()
				} else {
					break
				}
			}
			p.expect(tokRparen)
			if p.s.tok == tokSemicolon {
				p.s.next()
			}
			return exprStmt{callExpr{name, args}}
		}
		panic("unexpected statement")
	case tokLbrace:
		body := p.parseBlock()
		return blockStmt{body}
	default:
		panic("unknown statement")
	}
}

func (p *parser) parseBlock() []stmt {
	p.expect(tokLbrace)
	var body []stmt
	for p.s.tok != tokRbrace && p.s.tok != tokEOF {
		body = append(body, p.parseStmt())
	}
	p.expect(tokRbrace)
	return body
}

func (p *parser) parseFunc() {
	p.expect(tokFunc)
	name := p.parseIdent()
	p.expect(tokLparen)
	p.expect(tokRparen)
	// skip optional return type
	for p.s.tok != tokLbrace && p.s.tok != tokEOF {
		p.s.next()
	}
	body := p.parseBlock()
	p.funcs[name] = &funcDecl{name: name, body: body}
}

func (p *parser) parse() {
	// skip package and imports by reading until 'func'
	for p.s.tok != tokEOF {
		if p.s.tok == tokFunc {
			p.parseFunc()
		} else {
			p.s.next()
		}
	}
}

type env struct {
	vars map[string]interface{}
}

func newEnv() *env { return &env{vars: make(map[string]interface{})} }

func evalExpr(e expr, fns map[string]*funcDecl, env *env) interface{} {
	switch v := e.(type) {
	case ident:
		return env.vars[v.name]
	case intLit:
		return v.val
	case strLit:
		return v.val
	case binExpr:
		lhs := evalExpr(v.left, fns, env)
		rhs := evalExpr(v.right, fns, env)
		li, lok := lhs.(int)
		ri, rok := rhs.(int)
		if !lok || !rok {
			return 0
		}
		switch v.op {
		case tokPlus:
			return li + ri
		case tokMinus:
			return li - ri
		case tokEqual:
			if li == ri {
				return 1
			} else {
				return 0
			}
		default:
			return 0
		}
	case callExpr:
		if v.name == "fmt.Println" {
			var out []interface{}
			for _, a := range v.args {
				out = append(out, evalExpr(a, fns, env))
			}
			fmt.Println(out...)
			return nil
		}
		fn := fns[v.name]
		if fn == nil {
			return nil
		}
		res, _ := execFunc(fn, fns, newEnv())
		return res
	default:
		return nil
	}
}

func execFunc(fn *funcDecl, fns map[string]*funcDecl, env *env) (interface{}, bool) {
	for _, s := range fn.body {
		switch st := s.(type) {
		case assignStmt:
			env.vars[st.name] = evalExpr(st.value, fns, env)
		case returnStmt:
			if st.value != nil {
				return evalExpr(st.value, fns, env), true
			}
			return nil, true
		case ifStmt:
			condVal := evalExpr(st.cond, fns, env)
			if iv, ok := condVal.(int); ok && iv != 0 {
				res, ok := execBlock(st.then, fns, env)
				if ok {
					return res, true
				}
			} else if st.els != nil {
				res, ok := execBlock(st.els, fns, env)
				if ok {
					return res, true
				}
			}
		case exprStmt:
			evalExpr(st.e, fns, env)
		case blockStmt:
			res, ok := execBlock(st.body, fns, env)
			if ok {
				return res, true
			}
		}
	}
	return nil, false
}

func execBlock(body []stmt, fns map[string]*funcDecl, env *env) (interface{}, bool) {
	for _, s := range body {
		switch st := s.(type) {
		case assignStmt:
			env.vars[st.name] = evalExpr(st.value, fns, env)
		case returnStmt:
			if st.value != nil {
				return evalExpr(st.value, fns, env), true
			}
			return nil, true
		case ifStmt:
			condVal := evalExpr(st.cond, fns, env)
			if iv, ok := condVal.(int); ok && iv != 0 {
				res, ok := execBlock(st.then, fns, env)
				if ok {
					return res, true
				}
			} else if st.els != nil {
				res, ok := execBlock(st.els, fns, env)
				if ok {
					return res, true
				}
			}
		case exprStmt:
			evalExpr(st.e, fns, env)
		case blockStmt:
			res, ok := execBlock(st.body, fns, env)
			if ok {
				return res, true
			}
		}
	}
	return nil, false
}

func interpret(path string) error {
	data, err := ioutil.ReadFile(path)
	if err != nil {
		return err
	}
	p := newParser(string(data))
	p.parse()
	mainFn := p.funcs["main"]
	if mainFn == nil {
		return fmt.Errorf("no main function")
	}
	execFunc(mainFn, p.funcs, newEnv())
	return nil
}

func main() {
	if os.Getenv("GO4_LEVEL") != "" {
		return
	}
	if len(os.Args) < 2 {
		fmt.Println("usage: go4 <file.go>")
		return
	}
	src := os.Args[1]
	if src == "--" && len(os.Args) > 2 {
		src = os.Args[2]
	}
	if strings.HasSuffix(src, "go4.go") {
		return
	}
	os.Setenv("GO4_LEVEL", "1")
	defer os.Unsetenv("GO4_LEVEL")
	if err := interpret(src); err != nil {
		fmt.Fprintln(os.Stderr, err)
		os.Exit(1)
	}
}
