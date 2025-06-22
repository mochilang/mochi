package main

import (
	"fmt"
	"io/ioutil"
	"os"
	"path/filepath"
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
	tokIf
	tokFor
	tokEqual
	tokGreater
	tokAnd
	tokNotEqual
	tokString
	tokLBracket
	tokRBracket
	tokDefine
)

type node = map[string]interface{}

type token = map[string]interface{}

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
			for i < len(src) && (isLetter(src[i]) || isDigit(src[i]) || (src[i] == '.' && i+1 < len(src) && isLetter(src[i+1]))) {
				i++
			}
			lit := src[start:i]
			switch lit {
			case "if":
				tokens = append(tokens, token{"typ": tokIf, "lit": lit})
			case "for":
				tokens = append(tokens, token{"typ": tokFor, "lit": lit})
			default:
				tokens = append(tokens, token{"typ": tokIdent, "lit": lit})
			}
			continue
		}
		if isDigit(c) {
			start := i
			for i < len(src) && isDigit(src[i]) {
				i++
			}
			tokens = append(tokens, token{"typ": tokNumber, "lit": src[start:i]})
			continue
		}
		if c == '"' {
			i++
			start := i
			for i < len(src) && src[i] != '"' {
				i++
			}
			tokens = append(tokens, token{"typ": tokString, "lit": src[start:i]})
			i++
			continue
		}
		switch c {
		case '(':
			tokens = append(tokens, token{"typ": tokLParen, "lit": "("})
		case ')':
			tokens = append(tokens, token{"typ": tokRParen, "lit": ")"})
		case '[':
			tokens = append(tokens, token{"typ": tokLBracket, "lit": "["})
		case ']':
			tokens = append(tokens, token{"typ": tokRBracket, "lit": "]"})
		case '{':
			tokens = append(tokens, token{"typ": tokLBrace, "lit": "{"})
		case '}':
			tokens = append(tokens, token{"typ": tokRBrace, "lit": "}"})
		case ';':
			tokens = append(tokens, token{"typ": tokSemicolon, "lit": ";"})
		case '+':
			tokens = append(tokens, token{"typ": tokPlus, "lit": "+"})
		case '-':
			tokens = append(tokens, token{"typ": tokMinus, "lit": "-"})
		case '*':
			tokens = append(tokens, token{"typ": tokStar, "lit": "*"})
		case '/':
			tokens = append(tokens, token{"typ": tokSlash, "lit": "/"})
		case '=':
			if i+1 < len(src) && src[i+1] == '=' {
				i++
				tokens = append(tokens, token{"typ": tokEqual, "lit": "=="})
			} else {
				tokens = append(tokens, token{"typ": tokAssign, "lit": "="})
			}
		case '!':
			if i+1 < len(src) && src[i+1] == '=' {
				i++
				tokens = append(tokens, token{"typ": tokNotEqual, "lit": "!="})
			}
		case ',':
			tokens = append(tokens, token{"typ": tokComma, "lit": ","})
		case '&':
			if i+1 < len(src) && src[i+1] == '&' {
				i++
				tokens = append(tokens, token{"typ": tokAnd, "lit": "&&"})
			}
		case '>':
			tokens = append(tokens, token{"typ": tokGreater, "lit": ">"})
		case ':':
			if i+1 < len(src) && src[i+1] == '=' {
				i++
				tokens = append(tokens, token{"typ": tokDefine, "lit": ":="})
			}
		}
		i++
	}
	return append(tokens, token{"typ": tokEOF})
}

func parse(tokens []token) node {
	pos := 0
	peek := func() token {
		if pos >= len(tokens) {
			return token{"typ": tokEOF}
		}
		return tokens[pos]
	}
	next := func() token {
		t := peek()
		if pos < len(tokens) {
			pos++
		}
		return t
	}

	var parseExpr func() node

	parseNumber := func() node {
		v, _ := strconv.Atoi(next()["lit"].(string))
		return node{"tag": "num", "val": v}
	}

	parseFactor := func() node {
		t := peek()
		typ := t["typ"].(int)
		if typ == tokNumber {
			return parseNumber()
		}
		if typ == tokString {
			next()
			return node{"tag": "str", "val": t["lit"].(string)}
		}
		if typ == tokIdent {
			next()
			if peek()["typ"].(int) == tokLParen {
				next()
				var args []node
				if peek()["typ"].(int) != tokRParen {
					args = append(args, parseExpr())
					for peek()["typ"].(int) == tokComma {
						next()
						args = append(args, parseExpr())
					}
				}
				if peek()["typ"].(int) == tokRParen {
					next()
				}
				return node{"tag": "call", "name": t["lit"], "args": args}
			}
			if peek()["typ"].(int) == tokLBracket {
				next()
				idx := parseExpr()
				if peek()["typ"].(int) == tokRBracket {
					next()
				}
				return node{"tag": "index", "name": t["lit"], "right": idx}
			}
			return node{"tag": "ident", "name": t["lit"]}
		}
		if typ == tokLParen {
			next()
			e := parseExpr()
			next()
			return e
		}
		next()
		return node{"tag": "num", "val": 0}
	}

	parseTerm := func() node {
		e := parseFactor()
		for {
			t := peek()
			typ := t["typ"].(int)
			if typ == tokStar || typ == tokSlash {
				next()
				e = node{"tag": "bin", "op": typ, "left": e, "right": parseFactor()}
			} else {
				break
			}
		}
		return e
	}

	parseAdd := func() node {
		e := parseTerm()
		for {
			t := peek()
			typ := t["typ"].(int)
			if typ == tokPlus || typ == tokMinus {
				next()
				e = node{"tag": "bin", "op": typ, "left": e, "right": parseTerm()}
			} else {
				break
			}
		}
		return e
	}

	parseCmp := func() node {
		e := parseAdd()
		for {
			t := peek()
			typ := t["typ"].(int)
			if typ == tokEqual || typ == tokGreater || typ == tokNotEqual {
				next()
				e = node{"tag": "bin", "op": typ, "left": e, "right": parseAdd()}
			} else {
				break
			}
		}
		return e
	}

	parseExpr = func() node {
		e := parseCmp()
		for {
			if peek()["typ"].(int) == tokAnd {
				next()
				e = node{"tag": "bin", "op": tokAnd, "left": e, "right": parseCmp()}
			} else {
				break
			}
		}
		return e
	}

	var parseStatement func() node
	var parseBlock func() node

	parseStatement = func() node {
		t := peek()
		typ := t["typ"].(int)
		if typ == tokIf {
			next()
			if peek()["typ"].(int) == tokLParen {
				next()
			}
			cond := parseExpr()
			if peek()["typ"].(int) == tokRParen {
				next()
			}
			body := parseBlock()
			return node{"tag": "if", "cond": cond, "body": body}
		}
		if typ == tokFor {
			next()
			if peek()["typ"].(int) == tokLParen {
				next()
			}
			init := parseStatement()
			cond := parseExpr()
			if peek()["typ"].(int) == tokSemicolon {
				next()
			}
			post := parseStatement()
			if peek()["typ"].(int) == tokRParen {
				next()
			}
			body := parseBlock()
			return node{"tag": "for", "init": init, "cond": cond, "post": post, "body": body}
		}
		if typ == tokIdent && (t["lit"] == "fmt" || t["lit"] == "fmt.Println") {
			if t["lit"] == "fmt" {
				next()
				next()
			} else {
				next()
			}
			if peek()["typ"].(int) == tokLParen {
				next()
			}
			val := parseExpr()
			if peek()["typ"].(int) == tokRParen {
				next()
			}
			if peek()["typ"].(int) == tokSemicolon {
				next()
			}
			return node{"tag": "print", "value": val}
		}
		if typ == tokIdent {
			name := t["lit"].(string)
			next()
			if peek()["typ"].(int) == tokAssign || peek()["typ"].(int) == tokDefine {
				next()
			}
			val := parseExpr()
			if peek()["typ"].(int) == tokSemicolon {
				next()
			}
			return node{"tag": "assign", "name": name, "value": val}
		}
		next()
		return nil
	}

	parseBlock = func() node {
		var stmts []node
		if peek()["typ"].(int) == tokLBrace {
			next()
		}
		for peek()["typ"].(int) != tokRBrace && peek()["typ"].(int) != tokEOF {
			if s := parseStatement(); s != nil {
				stmts = append(stmts, s)
			}
		}
		if peek()["typ"].(int) == tokRBrace {
			next()
		}
		return node{"tag": "block", "stmts": stmts}
	}

	for {
		t := peek()
		if t["typ"].(int) == tokIdent && t["lit"] == "func" {
			next()
			if peek()["typ"].(int) == tokIdent && peek()["lit"] == "main" {
				next()
				for peek()["typ"].(int) != tokLBrace && peek()["typ"].(int) != tokEOF {
					next()
				}
				if peek()["typ"].(int) == tokLBrace {
					return parseBlock()
				}
				break
			}
		}
		if t["typ"].(int) == tokEOF {
			break
		}
		next()
	}
	return node{"tag": "block", "stmts": []node{}}
}

func run(b node, dump bool) {
	const (
		opPush = iota
		opLoad
		opStore
		opAdd
		opSub
		opMul
		opDiv
		opPrint
		opPushStr
		opEq
		opGt
		opNeq
		opAndOp
		opJmp
		opJz
		opCall
	)

	var code []map[string]interface{}

	var compileExpr func(node)
	var compileStmt func(node)

	compileExpr = func(e node) {
		if e == nil {
			return
		}
		switch e["tag"] {
		case "num":
			code = append(code, map[string]interface{}{"op": opPush, "val": e["val"]})
		case "str":
			code = append(code, map[string]interface{}{"op": opPushStr, "str": e["val"]})
		case "ident":
			code = append(code, map[string]interface{}{"op": opLoad, "name": e["name"]})
		case "call":
			for _, a := range e["args"].([]node) {
				compileExpr(a)
			}
			code = append(code, map[string]interface{}{"op": opCall, "name": e["name"], "val": len(e["args"].([]node))})
		case "bin":
			compileExpr(e["left"].(node))
			compileExpr(e["right"].(node))
			op := opAdd
			switch e["op"].(int) {
			case tokPlus:
				op = opAdd
			case tokMinus:
				op = opSub
			case tokStar:
				op = opMul
			case tokSlash:
				op = opDiv
			case tokEqual:
				op = opEq
			case tokGreater:
				op = opGt
			case tokNotEqual:
				op = opNeq
			case tokAnd:
				op = opAndOp
			case tokLBracket:
				op = opLoad
			}
			code = append(code, map[string]interface{}{"op": op})
		}
	}

	compileStmt = func(s node) {
		if s == nil {
			return
		}
		switch s["tag"] {
		case "assign":
			compileExpr(s["value"].(node))
			code = append(code, map[string]interface{}{"op": opStore, "name": s["name"]})
		case "print":
			compileExpr(s["value"].(node))
			code = append(code, map[string]interface{}{"op": opPrint})
		case "if":
			compileExpr(s["cond"].(node))
			jz := len(code)
			code = append(code, map[string]interface{}{"op": opJz})
			for _, st := range s["body"].(node)["stmts"].([]node) {
				compileStmt(st)
			}
			code[jz]["val"] = len(code)
		case "for":
			compileStmt(s["init"].(node))
			start := len(code)
			compileExpr(s["cond"].(node))
			jz := len(code)
			code = append(code, map[string]interface{}{"op": opJz})
			for _, st := range s["body"].(node)["stmts"].([]node) {
				compileStmt(st)
			}
			compileStmt(s["post"].(node))
			code = append(code, map[string]interface{}{"op": opJmp, "val": start})
			code[jz]["val"] = len(code)
		}
	}

	for _, s := range b["stmts"].([]node) {
		compileStmt(s)
	}

	if dump {
		names := map[int]string{opPush: "PUSH", opLoad: "LOAD", opStore: "STORE", opAdd: "ADD", opSub: "SUB", opMul: "MUL", opDiv: "DIV", opPrint: "PRINT", opPushStr: "PUSHS", opEq: "EQ", opGt: "GT", opNeq: "NEQ", opAndOp: "AND", opJmp: "JMP", opJz: "JZ", opCall: "CALL"}
		for _, ins := range code {
			op := ins["op"].(int)
			switch op {
			case opPush:
				fmt.Printf("%s %d\n", names[op], ins["val"].(int))
			case opPushStr:
				fmt.Printf("%s %s\n", names[op], ins["str"].(string))
			case opLoad, opStore:
				fmt.Printf("%s %s\n", names[op], ins["name"].(string))
			default:
				fmt.Println(names[op])
			}
		}
		return
	}

	stack := []interface{}{}
	env := map[string]interface{}{}
	for pc := 0; pc < len(code); pc++ {
		ins := code[pc]
		switch ins["op"].(int) {
		case opPush:
			stack = append(stack, ins["val"].(int))
		case opPushStr:
			stack = append(stack, ins["str"].(string))
		case opLoad:
			stack = append(stack, env[ins["name"].(string)])
		case opStore:
			v := stack[len(stack)-1]
			stack = stack[:len(stack)-1]
			env[ins["name"].(string)] = v
		case opAdd, opSub, opMul, opDiv:
			b := stack[len(stack)-1].(int)
			stack = stack[:len(stack)-1]
			a := stack[len(stack)-1].(int)
			stack = stack[:len(stack)-1]
			r := 0
			switch ins["op"].(int) {
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
		case opEq, opGt, opNeq, opAndOp:
			b := stack[len(stack)-1].(int)
			stack = stack[:len(stack)-1]
			a := stack[len(stack)-1].(int)
			stack = stack[:len(stack)-1]
			r := 0
			switch ins["op"].(int) {
			case opEq:
				if a == b {
					r = 1
				}
			case opGt:
				if a > b {
					r = 1
				}
			case opNeq:
				if a != b {
					r = 1
				}
			case opAndOp:
				if a != 0 && b != 0 {
					r = 1
				}
			}
			stack = append(stack, r)
		case opJmp:
			pc = ins["val"].(int) - 1
		case opJz:
			v := stack[len(stack)-1].(int)
			stack = stack[:len(stack)-1]
			if v == 0 {
				pc = ins["val"].(int) - 1
			}
		case opPrint:
			v := stack[len(stack)-1]
			stack = stack[:len(stack)-1]
			fmt.Println(v)
		case opCall:
			// no-op for compiled calls
		}
	}
}

func main() {
	args := os.Args
	i := 1
	if len(args) <= i {
		fmt.Println("usage: go4 file.go")
		return
	}
	if args[i] == "-s" && len(args) > i+1 {
		bts, _ := ioutil.ReadFile(args[i+1])
		run(parse(tokenize(string(bts))), true)
		return
	}
	for i < len(args) && filepath.Base(args[i]) == "go4.go" {
		i = i + 1
	}
	for ; i < len(args); i = i + 1 {
		f := args[i]
		bts, err := ioutil.ReadFile(f)
		if err != nil {
			fmt.Println(err)
		} else {
			run(parse(tokenize(string(bts))), false)
		}
	}
}
