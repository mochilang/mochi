package main

import (
	"fmt"
	"os"
	"strconv"
)

// Tokenizer and basic types

type TokenType int

type Token struct {
	typ TokenType
	val string
}

const (
	TokEOF TokenType = iota
	TokIdent
	TokNumber
	TokString
	TokFunc
	TokReturn
	TokIf
	TokElse
	TokVar
	TokPackage
	TokImport
	TokLParen
	TokRParen
	TokLBrace
	TokRBrace
	TokSemicolon
	TokAssign
	TokComma
	TokAdd
	TokSub
	TokMul
	TokDiv
)

var keywords = map[string]TokenType{
	"func":    TokFunc,
	"return":  TokReturn,
	"if":      TokIf,
	"else":    TokElse,
	"var":     TokVar,
	"package": TokPackage,
	"import":  TokImport,
}

// tokenize converts source to a slice of tokens
func tokenize(src string) []Token {
	isLetter := func(ch byte) bool {
		return (ch >= 'a' && ch <= 'z') || (ch >= 'A' && ch <= 'Z') || ch == '_'
	}
	isDigit := func(ch byte) bool { return ch >= '0' && ch <= '9' }

	var tokens []Token
	i := 0
	for i < len(src) {
		ch := src[i]
		if ch == ' ' || ch == '\n' || ch == '\t' || ch == '\r' {
			i++
			continue
		}
		if ch == '/' && i+1 < len(src) && src[i+1] == '/' {
			for i < len(src) && src[i] != '\n' {
				i++
			}
			continue
		}
		if isLetter(ch) {
			start := i
			for i < len(src) && (isLetter(src[i]) || isDigit(src[i])) {
				i++
			}
			word := src[start:i]
			if t, ok := keywords[word]; ok {
				tokens = append(tokens, Token{t, word})
			} else {
				tokens = append(tokens, Token{TokIdent, word})
			}
			continue
		}
		if isDigit(ch) {
			start := i
			for i < len(src) && isDigit(src[i]) {
				i++
			}
			tokens = append(tokens, Token{TokNumber, src[start:i]})
			continue
		}
		switch ch {
		case '(':
			tokens = append(tokens, Token{TokLParen, "("})
			i++
		case ')':
			tokens = append(tokens, Token{TokRParen, ")"})
			i++
		case '{':
			tokens = append(tokens, Token{TokLBrace, "{"})
			i++
		case '}':
			tokens = append(tokens, Token{TokRBrace, "}"})
			i++
		case ';':
			tokens = append(tokens, Token{TokSemicolon, ";"})
			i++
		case '=':
			tokens = append(tokens, Token{TokAssign, "="})
			i++
		case ',':
			tokens = append(tokens, Token{TokComma, ","})
			i++
		case '+':
			tokens = append(tokens, Token{TokAdd, "+"})
			i++
		case '-':
			tokens = append(tokens, Token{TokSub, "-"})
			i++
		case '*':
			tokens = append(tokens, Token{TokMul, "*"})
			i++
		case '/':
			tokens = append(tokens, Token{TokDiv, "/"})
			i++
		case '"':
			start := i + 1
			i++
			for i < len(src) && src[i] != '"' {
				if src[i] == '\\' && i+1 < len(src) {
					i += 2
				} else {
					i++
				}
			}
			val := src[start:i]
			if i < len(src) && src[i] == '"' {
				i++
			}
			tokens = append(tokens, Token{TokString, val})
		default:
			i++
		}
	}
	tokens = append(tokens, Token{TokEOF, ""})
	return tokens
}

// AST types

type Expr interface{}
type Stmt interface{}

type NumberLiteral struct{ Value int }
type StringLiteral struct{ Value string }
type Ident struct{ Name string }
type BinaryExpr struct {
	Op          TokenType
	Left, Right Expr
}
type CallExpr struct {
	Func Expr
	Args []Expr
}

type VarStmt struct {
	Name  string
	Value Expr
}
type ExprStmt struct{ Expr Expr }
type ReturnStmt struct{ Value Expr }
type IfStmt struct {
	Cond       Expr
	Then, Else []Stmt
}
type FuncDecl struct {
	Name string
	Body []Stmt
}
type Program struct{ Funcs map[string]*FuncDecl }

// parse builds an AST from tokens
func parse(tokens []Token) *Program {
	type Parser struct {
		tokens []Token
		pos    int
	}
	p := &Parser{tokens: tokens}

	cur := func() Token { return p.tokens[p.pos] }
	nextTok := func() {
		if p.pos < len(p.tokens)-1 {
			p.pos++
		}
	}
	accept := func(t TokenType) bool {
		if cur().typ == t {
			nextTok()
			return true
		}
		return false
	}
	expect := func(t TokenType) {
		if !accept(t) {
			panic(fmt.Sprintf("expected %v got %v", t, cur().typ))
		}
	}

	var parseExpr func() Expr
	var parseTerm func() Expr
	var parseFactor func() Expr

	parseFactor = func() Expr {
		tok := cur()
		switch tok.typ {
		case TokNumber:
			nextTok()
			v, _ := strconv.Atoi(tok.val)
			return &NumberLiteral{Value: v}
		case TokString:
			nextTok()
			return &StringLiteral{Value: tok.val}
		case TokIdent:
			nextTok()
			if cur().typ == TokLParen {
				nextTok()
				var args []Expr
				if cur().typ != TokRParen {
					for {
						args = append(args, parseExpr())
						if cur().typ == TokComma {
							nextTok()
							continue
						}
						break
					}
				}
				expect(TokRParen)
				return &CallExpr{Func: &Ident{Name: tok.val}, Args: args}
			}
			return &Ident{Name: tok.val}
		case TokLParen:
			nextTok()
			e := parseExpr()
			expect(TokRParen)
			return e
		default:
			panic(fmt.Sprintf("unexpected token %v", tok))
		}
	}

	parseTerm = func() Expr {
		e := parseFactor()
		for cur().typ == TokMul || cur().typ == TokDiv {
			op := cur().typ
			nextTok()
			right := parseFactor()
			e = &BinaryExpr{Op: op, Left: e, Right: right}
		}
		return e
	}

	parseExpr = func() Expr {
		e := parseTerm()
		for cur().typ == TokAdd || cur().typ == TokSub {
			op := cur().typ
			nextTok()
			right := parseTerm()
			e = &BinaryExpr{Op: op, Left: e, Right: right}
		}
		return e
	}

	var parseStmt func() Stmt
	var parseBlock func() []Stmt
	var parseFunc func() *FuncDecl

	parseStmt = func() Stmt {
		switch cur().typ {
		case TokReturn:
			nextTok()
			var val Expr
			if cur().typ != TokSemicolon {
				val = parseExpr()
			}
			accept(TokSemicolon)
			return &ReturnStmt{Value: val}
		case TokVar:
			nextTok()
			nameTok := cur()
			expect(TokIdent)
			expect(TokAssign)
			v := parseExpr()
			accept(TokSemicolon)
			return &VarStmt{Name: nameTok.val, Value: v}
		case TokIf:
			nextTok()
			expect(TokLParen)
			cond := parseExpr()
			expect(TokRParen)
			thenBlock := parseBlock()
			var elseBlock []Stmt
			if cur().typ == TokElse {
				nextTok()
				elseBlock = parseBlock()
			}
			return &IfStmt{Cond: cond, Then: thenBlock, Else: elseBlock}
		default:
			e := parseExpr()
			accept(TokSemicolon)
			return &ExprStmt{Expr: e}
		}
	}

	parseBlock = func() []Stmt {
		expect(TokLBrace)
		var stmts []Stmt
		for cur().typ != TokRBrace && cur().typ != TokEOF {
			stmts = append(stmts, parseStmt())
		}
		expect(TokRBrace)
		return stmts
	}

	parseFunc = func() *FuncDecl {
		expect(TokFunc)
		nameTok := cur()
		expect(TokIdent)
		expect(TokLParen)
		for cur().typ != TokRParen {
			nextTok()
		}
		expect(TokRParen)
		body := parseBlock()
		return &FuncDecl{Name: nameTok.val, Body: body}
	}

	prog := &Program{Funcs: make(map[string]*FuncDecl)}
	for cur().typ != TokEOF {
		if cur().typ == TokPackage {
			nextTok()
			expect(TokIdent)
			continue
		}
		if cur().typ == TokImport {
			nextTok()
			if cur().typ == TokLParen {
				nextTok()
				for cur().typ != TokRParen && cur().typ != TokEOF {
					nextTok()
				}
				accept(TokRParen)
			} else {
				nextTok()
			}
			continue
		}
		if cur().typ == TokFunc {
			f := parseFunc()
			prog.Funcs[f.Name] = f
			continue
		}
		nextTok()
	}
	return prog
}

// run executes the parsed program
func run(prog *Program) {
	type Env map[string]interface{}
	var evalBlock func(env Env, stmts []Stmt) (interface{}, bool)
	newEnv := func() Env { return make(map[string]interface{}) }

	var evalExpr func(env Env, e Expr) interface{}
	evalExpr = func(env Env, e Expr) interface{} {
		switch v := e.(type) {
		case *NumberLiteral:
			return v.Value
		case *StringLiteral:
			return v.Value
		case *Ident:
			if val, ok := env[v.Name]; ok {
				return val
			}
			return nil
		case *BinaryExpr:
			l := evalExpr(env, v.Left).(int)
			r := evalExpr(env, v.Right).(int)
			switch v.Op {
			case TokAdd:
				return l + r
			case TokSub:
				return l - r
			case TokMul:
				return l * r
			case TokDiv:
				if r != 0 {
					return l / r
				}
				return 0
			}
		case *CallExpr:
			if id, ok := v.Func.(*Ident); ok {
				if id.Name == "print" || id.Name == "println" {
					var args []interface{}
					for _, a := range v.Args {
						args = append(args, evalExpr(env, a))
					}
					if id.Name == "println" {
						fmt.Println(args...)
					} else {
						fmt.Print(args...)
					}
					return nil
				}
				if fn, ok := env[id.Name].(*FuncDecl); ok {
					res, _ := evalBlock(newEnv(), fn.Body)
					return res
				}
			}
		}
		return nil
	}

	var evalStmt func(env Env, s Stmt) (interface{}, bool)
	evalStmt = func(env Env, s Stmt) (interface{}, bool) {
		switch st := s.(type) {
		case *VarStmt:
			env[st.Name] = evalExpr(env, st.Value)
		case *ExprStmt:
			evalExpr(env, st.Expr)
		case *ReturnStmt:
			if st.Value != nil {
				return evalExpr(env, st.Value), true
			}
			return nil, true
		case *IfStmt:
			cond := evalExpr(env, st.Cond)
			if condInt, ok := cond.(int); ok && condInt != 0 {
				return evalBlock(env, st.Then)
			}
			if st.Else != nil {
				return evalBlock(env, st.Else)
			}
		}
		return nil, false
	}

	evalBlock = func(env Env, stmts []Stmt) (interface{}, bool) {
		for _, s := range stmts {
			if r, ok := evalStmt(env, s); ok {
				return r, true
			}
		}
		return nil, false
	}

	env := newEnv()
	for name, fn := range prog.Funcs {
		env[name] = fn
	}
	if mainFn, ok := prog.Funcs["main"]; ok {
		evalBlock(env, mainFn.Body)
	} else {
		fmt.Println("no main function")
	}
}

// entry point
func main() {
	showTokens := false
	var files []string
	for _, arg := range os.Args[1:] {
		if arg == "-s" {
			showTokens = true
			continue
		}
		files = append(files, arg)
	}
	if len(files) == 0 {
		fmt.Println("usage: go4 [-s] file.go [...]")
		return
	}
	for _, f := range files {
		srcBytes, err := os.ReadFile(f)
		if err != nil {
			fmt.Println("cannot read", f)
			return
		}
		src := string(srcBytes)
		tokens := tokenize(src)
		if showTokens {
			for _, t := range tokens {
				fmt.Printf("%v %q\n", t.typ, t.val)
			}
			continue
		}
		prog := parse(tokens)
		run(prog)
	}
}
