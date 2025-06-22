package main

import (
	"fmt"
	"os"
	"strconv"
	"strings"
	"unicode"
)

var (
	src      []rune
	pos      int
	tok      string
	lit      string
	val      int
	ops      []string
	as       []int
	bs       []int
	cs       []int
	ss       []string
	reg      int
	varNames []string
	varVals  []int
)

func ensureReg(i int) {
	if i >= reg {
		reg = i + 1
	}
}

func ensureVar(i int) {
	for i >= len(varVals) {
		varVals = append(varVals, 0)
		varNames = append(varNames, fmt.Sprintf("v%d", len(varNames)))
	}
}

func varIndex(name string) int {
	for i, n := range varNames {
		if n == name {
			return i
		}
	}
	varNames = append(varNames, name)
	varVals = append(varVals, 0)
	return len(varNames) - 1
}

func loadAsm(path string) error {
	b, err := os.ReadFile(path)
	if err != nil {
		return err
	}
	lines := strings.Split(strings.TrimSpace(string(b)), "\n")
	for _, line := range lines {
		if strings.TrimSpace(line) == "" {
			continue
		}
		parts := strings.Fields(line)
		if len(parts) < 4 {
			continue
		}
		op := parts[0]
		a, _ := strconv.Atoi(parts[1])
		bval, _ := strconv.Atoi(parts[2])
		cval, _ := strconv.Atoi(parts[3])
		s := ""
		if len(parts) > 4 {
			s = strings.Join(parts[4:], " ")
		}
		ops = append(ops, op)
		as = append(as, a)
		bs = append(bs, bval)
		cs = append(cs, cval)
		ss = append(ss, s)
		switch op {
		case "IMM", "LOAD", "STORE", "ADD", "SUB", "MUL", "DIV", "EQ", "NE", "LT", "GT", "LE", "GE", "PRINT", "RET", "JZ":
			ensureReg(a)
		}
		switch op {
		case "ADD", "SUB", "MUL", "DIV", "EQ", "NE", "LT", "GT", "LE", "GE":
			ensureReg(bval)
			ensureReg(cval)
		case "LOAD", "STORE":
			ensureVar(bval)
		}
	}
	return nil
}

func next() {
	for {
		for pos < len(src) {
			if !unicode.IsSpace(src[pos]) {
				break
			}
			pos++
		}
		if pos+1 < len(src) {
			if src[pos] == '/' {
				if src[pos+1] == '/' {
					for pos < len(src) {
						if src[pos] == '\n' {
							break
						}
						pos++
					}
					continue
				}
			}
		}
		break
	}
	if pos >= len(src) {
		tok = "EOF"
		return
	}
	ch := src[pos]
	if unicode.IsLetter(ch) || ch == '_' {
		start := pos
		pos++
		for pos < len(src) {
			if !(unicode.IsLetter(src[pos]) || unicode.IsDigit(src[pos]) || src[pos] == '_') {
				break
			}
			pos++
		}
		lit = string(src[start:pos])
		switch lit {
		case "func", "return", "var", "if", "else", "for", "package":
			tok = lit
		default:
			tok = "IDENT"
		}
		return
	}
	if unicode.IsDigit(ch) {
		start := pos
		pos++
		for pos < len(src) {
			if !unicode.IsDigit(src[pos]) {
				break
			}
			pos++
		}
		lit = string(src[start:pos])
		val, _ = strconv.Atoi(lit)
		tok = "NUMBER"
		return
	}
	pos++
	switch ch {
	case '+', '-', '*', '/', '{', '}', '(', ')', '[', ']', ';', '=', ',', '.', ':', '<', '>', '!':
		if pos < len(src) {
			if ch == '=' || ch == '<' || ch == '>' || ch == '!' || ch == ':' {
				if src[pos] == '=' {
					tok = string([]rune{ch, '='})
					lit = tok
					pos++
				} else {
					tok = string(ch)
					lit = tok
				}
			} else {
				tok = string(ch)
				lit = tok
			}
		} else {
			tok = string(ch)
			lit = tok
		}
	case '"':
		start := pos
		for pos < len(src) {
			if src[pos] == '"' {
				break
			}
			pos++
		}
		lit = string(src[start:pos])
		pos++
		tok = "STRING"
	default:
		tok = "EOF"
	}
}

func precTok(t string) int {
	if t == "==" || t == "!=" || t == "<" || t == ">" || t == "<=" || t == ">=" {
		return 1
	}
	if t == "+" || t == "-" {
		return 2
	}
	if t == "*" || t == "/" {
		return 3
	}
	return 0
}

func expr(p int) int {
	var r int
	if tok == "NUMBER" {
		r = reg
		reg++
		ops = append(ops, "IMM")
		as = append(as, r)
		bs = append(bs, val)
		cs = append(cs, 0)
		ss = append(ss, "")
		next()
	} else if tok == "IDENT" {
		name := lit
		next()
		if tok == "(" {
			next()
			arg := 0
			if tok != ")" {
				arg = expr(0)
			}
			for tok != ")" {
				if tok == "EOF" {
					break
				}
				next()
			}
			if tok == ")" {
				next()
			}
			if name == "print" {
				ops = append(ops, "PRINT")
				as = append(as, arg)
				bs = append(bs, 0)
				cs = append(cs, 0)
				ss = append(ss, "")
			}
			r = arg
		} else {
			r = reg
			reg++
			idx := varIndex(name)
			ops = append(ops, "LOAD")
			as = append(as, r)
			bs = append(bs, idx)
			cs = append(cs, 0)
			ss = append(ss, "")
		}
	} else if tok == "(" {
		next()
		r = expr(0)
		if tok == ")" {
			next()
		}
	}
	for {
		opPrec := precTok(tok)
		if opPrec <= p {
			break
		}
		opTok := tok
		next()
		b := expr(opPrec)
		a := r
		r = reg
		reg++
		op := map[string]string{
			"+": "ADD", "-": "SUB", "*": "MUL", "/": "DIV",
			"==": "EQ", "!=": "NE", "<": "LT", ">": "GT",
			"<=": "LE", ">=": "GE",
		}[opTok]
		ops = append(ops, op)
		as = append(as, r)
		bs = append(bs, a)
		cs = append(cs, b)
		ss = append(ss, "")
	}
	return r
}

func stmt() bool {
	if tok == "if" {
		next()
		cond := expr(0)
		if tok == "{" {
			next()
		}
		jz := len(ops)
		ops = append(ops, "JZ")
		as = append(as, cond)
		bs = append(bs, 0)
		cs = append(cs, 0)
		ss = append(ss, "")
		for tok != "}" {
			if tok == "EOF" {
				break
			}
			if stmt() {
				break
			}
		}
		if tok == "}" {
			next()
		}
		cs[jz] = len(ops)
		if tok == "else" {
			jmp := len(ops)
			ops = append(ops, "JMP")
			as = append(as, 0)
			bs = append(bs, 0)
			cs = append(cs, 0)
			ss = append(ss, "")
			next()
			if tok == "{" {
				next()
			}
			for tok != "}" {
				if tok == "EOF" {
					break
				}
				if stmt() {
					break
				}
			}
			if tok == "}" {
				next()
			}
			cs[jmp] = len(ops)
		}
		return false
	}
	if tok == "for" {
		next()
		start := len(ops)
		condReg := -1
		if tok != "{" {
			condReg = expr(0)
		}
		if tok == "{" {
			next()
		}
		jz := -1
		if condReg != -1 {
			jz = len(ops)
			ops = append(ops, "JZ")
			as = append(as, condReg)
			bs = append(bs, 0)
			cs = append(cs, 0)
			ss = append(ss, "")
		}
		for tok != "}" {
			if tok == "EOF" {
				break
			}
			if stmt() {
				break
			}
		}
		if tok == "}" {
			next()
		}
		ops = append(ops, "JMP")
		as = append(as, 0)
		bs = append(bs, 0)
		cs = append(cs, start)
		ss = append(ss, "")
		if jz != -1 {
			cs[jz] = len(ops)
		}
		return false
	}
	if tok == "IDENT" {
		name := lit
		next()
		if tok == "=" {
			next()
			r := expr(0)
			idx := varIndex(name)
			ops = append(ops, "STORE")
			as = append(as, r)
			bs = append(bs, idx)
			cs = append(cs, 0)
			ss = append(ss, "")
			if tok == ";" {
				next()
			}
			return false
		}
		if tok == "(" {
			next()
			if name == "print" {
				if tok == "STRING" {
					str := lit
					next()
					for tok != ")" {
						if tok == "EOF" {
							break
						}
						next()
					}
					if tok == ")" {
						next()
					}
					ops = append(ops, "PRINTS")
					as = append(as, 0)
					bs = append(bs, 0)
					cs = append(cs, 0)
					ss = append(ss, str)
					if tok == ";" {
						next()
					}
					return false
				}
			}
			arg := 0
			if tok != ")" {
				arg = expr(0)
			}
			for tok != ")" {
				if tok == "EOF" {
					break
				}
				next()
			}
			if tok == ")" {
				next()
			}
			if name == "print" {
				ops = append(ops, "PRINT")
				as = append(as, arg)
				bs = append(bs, 0)
				cs = append(cs, 0)
				ss = append(ss, "")
			}
			if tok == ";" {
				next()
			}
			return false
		}
	}
	if tok == "return" {
		next()
		r := expr(0)
		ops = append(ops, "RET")
		as = append(as, r)
		bs = append(bs, 0)
		cs = append(cs, 0)
		ss = append(ss, "")
		if tok == ";" {
			next()
		}
		return true
	}
	next()
	return false
}

func main() {
	args := os.Args[1:]
	show := false
	if len(args) > 0 {
		if args[0] == "-s" {
			show = true
			args = args[1:]
		}
	}
	if len(args) == 0 {
		fmt.Println("usage: go4 [-s] file.go")
		return
	}
	for _, path := range args {
		if strings.HasSuffix(path, ".s") {
			if err := loadAsm(path); err != nil {
				fmt.Println("error:", err)
				return
			}
			continue
		}
		b, err := os.ReadFile(path)
		if err != nil {
			fmt.Println("error:", err)
			return
		}
		src = []rune(string(b))
		pos = 0
		next()
		if tok == "package" {
			for pos < len(src) {
				if src[pos] == '\n' {
					break
				}
				pos++
			}
			next()
		}
		for tok != "EOF" {
			if stmt() {
				break
			}
		}
	}
	if show {
		for i, op := range ops {
			if ss[i] != "" {
				fmt.Printf("%s %d %d %d %s\n", op, as[i], bs[i], cs[i], ss[i])
			} else {
				fmt.Printf("%s %d %d %d\n", op, as[i], bs[i], cs[i])
			}
		}
		return
	}
	regs := make([]int, reg)
	for pc := 0; pc < len(ops); pc++ {
		op := ops[pc]
		a := as[pc]
		b := bs[pc]
		c := cs[pc]
		s := ss[pc]
		switch op {
		case "IMM":
			regs[a] = b
		case "LOAD":
			regs[a] = varVals[b]
		case "STORE":
			varVals[b] = regs[a]
		case "ADD":
			regs[a] = regs[b] + regs[c]
		case "SUB":
			regs[a] = regs[b] - regs[c]
		case "MUL":
			regs[a] = regs[b] * regs[c]
		case "DIV":
			regs[a] = regs[b] / regs[c]
		case "EQ":
			if regs[b] == regs[c] {
				regs[a] = 1
			} else {
				regs[a] = 0
			}
		case "NE":
			if regs[b] != regs[c] {
				regs[a] = 1
			} else {
				regs[a] = 0
			}
		case "LT":
			if regs[b] < regs[c] {
				regs[a] = 1
			} else {
				regs[a] = 0
			}
		case "GT":
			if regs[b] > regs[c] {
				regs[a] = 1
			} else {
				regs[a] = 0
			}
		case "LE":
			if regs[b] <= regs[c] {
				regs[a] = 1
			} else {
				regs[a] = 0
			}
		case "GE":
			if regs[b] >= regs[c] {
				regs[a] = 1
			} else {
				regs[a] = 0
			}
		case "JMP":
			pc = c - 1
		case "JZ":
			if regs[a] == 0 {
				pc = c - 1
			}
		case "PRINT":
			fmt.Println(regs[a])
		case "PRINTS":
			fmt.Println(s)
		case "RET":
			return
		}
	}
}
