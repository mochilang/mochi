//go:build ccompiler

package main

import (
	"fmt"
	"io/ioutil"
	"log"
	"os"
	"strings"
	"unicode"
)

var (
	src      []byte
	p        int
	line     int
	token    int
	tokenVal int

	text []int
	e    int

	sym []int
	id  int
)

const (
	// tokens
	Num = 128 + iota
	Fun
	Sys
	Glo
	Loc
	Id

	Char
	Else
	Enum
	If
	Int
	Return
	Sizeof
	While

	Assign
	Cond
	Lor
	Lan
	Or
	Xor
	And
	Eq
	Ne
	Lt
	Gt
	Le
	Ge
	Shl
	Shr
	Add
	Sub
	Mul
	Div
	Mod
	Inc
	Dec
	Brak
)

const (
	LEA = iota
	IMM
	JMP
	JSR
	BZ
	BNZ
	ENT
	ADJ
	LEV
	LI
	LC
	SI
	SC
	PSH
	OR
	XOR
	AND
	EQ
	NE
	LT
	GT
	LE
	GE
	SHL
	SHR
	ADD
	SUB
	MUL
	DIV
	MOD
	OPEN
	READ
	CLOS
	PRTF
	MALC
	FREE
	MSET
	MCMP
	EXIT
)

const (
	CHAR = iota
	INT
	PTR
)

const (
	Tk = iota
	Hash
	Name
	Class
	Type
	Val
	HClass
	HType
	HVal
	Idsz
)

func next() {
	for token = int(src[p]); token > 0; p++ {
		if token == '\n' {
			line++
		} else if token == '#' {
			for src[p] != '\n' && src[p] != 0 {
				p++
			}
		} else if unicode.IsSpace(rune(token)) {
			// skip
		} else if unicode.IsLetter(rune(token)) || token == '_' {
			start := p
			hash := 0
			for unicode.IsLetter(rune(src[p])) || unicode.IsDigit(rune(src[p])) || src[p] == '_' {
				hash = hash*147 + int(src[p])
				p++
			}
			length := p - start
			hash = (hash << 6) + length
			id = 0
			for id < len(sym) && sym[id+Tk] != 0 {
				if sym[id+Hash] == hash && string(src[start:p]) == string(src[sym[id+Name]:sym[id+Name]+sym[id+Hash]>>6]) {
					token = sym[id+Tk]
					return
				}
				id += Idsz
			}
			sym = append(sym, make([]int, Idsz)...)
			sym[len(sym)-Idsz+Name] = start
			sym[len(sym)-Idsz+Hash] = hash
			sym[len(sym)-Idsz+Tk] = Id
			id = len(sym) - Idsz
			token = Id
			return
		} else if unicode.IsDigit(rune(token)) {
			val := int(token - '0')
			for unicode.IsDigit(rune(src[p+1])) {
				p++
				val = val*10 + int(src[p]-'0')
			}
			tokenVal = val
			token = Num
			return
		} else if token == '/' && src[p+1] == '/' {
			p += 2
			for src[p] != 0 && src[p] != '\n' {
				p++
			}
		} else if token == '"' {
			p++
			start := p
			for src[p] != '"' && src[p] != 0 {
				if src[p] == '\\' {
					p++
					if src[p] == 'n' {
						p++
					}
				} else {
					p++
				}
			}
			tokenVal = start
			token = '"'
			p++
			return
		} else if token == '=' {
			if src[p+1] == '=' {
				p += 2
				token = Eq
			} else {
				p++
				token = Assign
			}
			return
		} else if token == '+' {
			if src[p+1] == '+' {
				p += 2
				token = Inc
			} else {
				p++
				token = Add
			}
			return
		} else if token == '-' {
			if src[p+1] == '-' {
				p += 2
				token = Dec
			} else {
				p++
				token = Sub
			}
			return
		} else if token == '!' {
			if src[p+1] == '=' {
				p += 2
				token = Ne
			} else {
				p++
			}
			return
		} else if token == '<' {
			if src[p+1] == '=' {
				p += 2
				token = Le
			} else if src[p+1] == '<' {
				p += 2
				token = Shl
			} else {
				p++
				token = Lt
			}
			return
		} else if token == '>' {
			if src[p+1] == '=' {
				p += 2
				token = Ge
			} else if src[p+1] == '>' {
				p += 2
				token = Shr
			} else {
				p++
				token = Gt
			}
			return
		} else if token == '|' {
			if src[p+1] == '|' {
				p += 2
				token = Lor
			} else {
				p++
				token = Or
			}
			return
		} else if token == '&' {
			if src[p+1] == '&' {
				p += 2
				token = Lan
			} else {
				p++
				token = And
			}
			return
		} else if token == '^' {
			p++
			token = Xor
			return
		} else if token == '%' {
			p++
			token = Mod
			return
		} else if token == '*' {
			p++
			token = Mul
			return
		} else if token == '[' {
			p++
			token = Brak
			return
		} else if token == '?' {
			p++
			token = Cond
			return
		} else if strings.ContainsRune("~;{}()]:,", rune(token)) {
			p++
			return
		} else {
			p++
			return
		}
	}
}

func expr(lev int) {
	// simplified implementation: only handle numbers and + - * / operations
	if token == Num {
		text = append(text, IMM, tokenVal)
		e += 2
		next()
	} else {
		log.Fatalf("unexpected token: %d", token)
	}

	for token >= lev {
		if token == Add {
			next()
			expr(Mul)
			text = append(text, ADD)
			e++
		} else if token == Sub {
			next()
			expr(Mul)
			text = append(text, SUB)
			e++
		} else if token == Mul {
			next()
			expr(Inc)
			text = append(text, MUL)
			e++
		} else if token == Div {
			next()
			expr(Inc)
			text = append(text, DIV)
			e++
		} else {
			break
		}
	}
}

func stmt() {
	if token == If {
		next()
		if token != '(' {
			log.Fatalf("expected (")
		}
		next()
		expr(Assign)
		if token != ')' {
			log.Fatalf("expected )")
		}
		next()
		stmt()
	} else if token == '{' {
		next()
		for token != '}' {
			stmt()
		}
		next()
	} else if token == ';' {
		next()
	} else {
		expr(Assign)
		if token == ';' {
			next()
		}
	}
}

func main() {
	if len(os.Args) < 2 {
		fmt.Println("usage: go4 <file>")
		return
	}
	source, err := ioutil.ReadFile(os.Args[1])
	if err != nil {
		log.Fatal(err)
	}
	src = append(source, 0)
	line = 1
	next()
	for token > 0 {
		stmt()
	}
}
