package main

import (
	"bytes"
	"flag"
	"fmt"
	"os"
)

const (
	EOF = iota
	IDENT
	INT
	STRING
	LPAREN
	RPAREN
	LBRACE
	RBRACE
	SEMICOLON
	COMMA
	FUNC
	PACKAGE
	RETURN
)

var src []rune
var pos int
var tk int
var lit string

type instr struct {
	op string
	r  int
	a  int
	b  int
	s  string
}

type Function struct {
	code []instr
	regs int
}

var functions map[string]Function
var cur *[]instr
var reg int

func next() {
	for {
		if pos >= len(src) {
			tk = EOF
			lit = ""
			return
		}
		ch := src[pos]
		pos++
		switch ch {
		case ' ', '\t', '\r', '\n':
			continue
		case '(':
			tk = LPAREN
			lit = "("
			return
		case ')':
			tk = RPAREN
			lit = ")"
			return
		case '{':
			tk = LBRACE
			lit = "{"
			return
		case '}':
			tk = RBRACE
			lit = "}"
			return
		case ';':
			tk = SEMICOLON
			lit = ";"
			return
		case ',':
			tk = COMMA
			lit = ","
			return
		case '/':
			if pos < len(src) && src[pos] == '/' {
				for pos < len(src) && src[pos] != '\n' {
					pos++
				}
				continue
			}
		case '"':
			start := pos
			for pos < len(src) && src[pos] != '"' {
				pos++
			}
			lit = string(src[start:pos])
			if pos < len(src) {
				pos++
			}
			tk = STRING
			return
		default:
			if (ch >= 'a' && ch <= 'z') || (ch >= 'A' && ch <= 'Z') || ch == '_' {
				start := pos - 1
				for pos < len(src) {
					c := src[pos]
					if (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') || c == '_' {
						pos++
					} else {
						break
					}
				}
				lit = string(src[start:pos])
				switch lit {
				case "func":
					tk = FUNC
				case "package":
					tk = PACKAGE
				case "return":
					tk = RETURN
				default:
					tk = IDENT
				}
				return
			}
			if ch >= '0' && ch <= '9' {
				start := pos - 1
				for pos < len(src) && src[pos] >= '0' && src[pos] <= '9' {
					pos++
				}
				lit = string(src[start:pos])
				tk = INT
				return
			}
		}
	}
}

func expr() int {
	if tk == IDENT {
		name := lit
		next()
		if tk == LPAREN {
			next()
			var args []int
			if tk != RPAREN {
				args = append(args, expr())
				for tk == COMMA {
					next()
					args = append(args, expr())
				}
			}
			if tk == RPAREN {
				next()
			}
			r := reg
			reg++
			if name == "println" || name == "fmt.Println" || name == "Println" {
				if len(args) > 0 {
					*cur = append(*cur, instr{op: "PRINT", a: args[0]})
				} else {
					*cur = append(*cur, instr{op: "PRINT"})
				}
				*cur = append(*cur, instr{op: "CONST", r: r})
				return r
			}
			if len(args) > 0 {
				*cur = append(*cur, instr{op: "CALL", r: r, a: args[0], s: name})
			} else {
				*cur = append(*cur, instr{op: "CALL", r: r, s: name})
			}
			return r
		}
		r := reg
		reg++
		*cur = append(*cur, instr{op: "CONST", r: r, s: name})
		return r
	}
	if tk == STRING || tk == INT {
		r := reg
		reg++
		v := lit
		next()
		*cur = append(*cur, instr{op: "CONST", r: r, s: v})
		return r
	}
	// Skip unhandled tokens so parsing continues when encountering
	// syntax features the interpreter does not understand.
	next()
	return 0
}

func stmt() {
	if tk == RETURN {
		next()
		r := expr()
		*cur = append(*cur, instr{op: "RET", a: r})
		if tk == SEMICOLON {
			next()
		}
		return
	}
	r := expr()
	*cur = append(*cur, instr{op: "DROP", a: r})
	if tk == SEMICOLON {
		next()
	}
	// If we did not consume anything meaningful and the token did not
	// end with a semicolon, advance to avoid getting stuck.
	if tk != EOF && tk != SEMICOLON {
		next()
	}
}

func main() {
	show := flag.Bool("s", false, "show instructions")
	flag.Parse()
	if flag.NArg() == 0 {
		fmt.Println("usage: go4 [-s] file.go [file2.go ...]")
		os.Exit(1)
	}
	args := flag.Args()
	var prev string
	for i, path := range args {
		var data []byte
		var err error
		if prev != "" {
			data = []byte(prev)
		} else {
			data, err = os.ReadFile(path)
			if err != nil {
				fmt.Fprintln(os.Stderr, err)
				continue
			}
		}

		functions = make(map[string]Function)
		src = []rune(string(data))
		pos = 0
		next()
		for tk != EOF {
			if tk == PACKAGE {
				// Skip `package` declaration
				for tk != SEMICOLON && tk != EOF && tk != FUNC {
					next()
				}
				if tk == SEMICOLON {
					next()
				}
				continue
			}
			if tk == FUNC {
				next()
				fname := lit
				next()
				if tk == LPAREN {
					for tk != RPAREN && tk != EOF {
						next()
					}
					if tk == RPAREN {
						next()
					}
				}
				if tk == LBRACE {
					start := pos
					depth := 1
					for pos < len(src) && depth > 0 {
						ch := src[pos]
						pos++
						if ch == '{' {
							depth++
						}
						if ch == '}' {
							depth--
						}
					}
					body := src[start : pos-1]
					savedSrc, savedPos := src, pos
					src = body
					pos = 0
					next()
					ops := []instr{}
					cur = &ops
					reg = 0
					for tk != EOF {
						stmt()
					}
					functions[fname] = Function{code: ops, regs: reg}
					src = savedSrc
					pos = savedPos
					next()
					continue
				}
			}
			next()
		}

		if *show {
			format := func(in instr) string {
				switch in.op {
				case "CONST":
					return fmt.Sprintf("MOV R%d, %q", in.r, in.s)
				case "PRINT":
					return fmt.Sprintf("PRINT R%d", in.a)
				case "CALL":
					if in.a > 0 {
						return fmt.Sprintf("CALL %s, R%d, R%d", in.s, in.r, in.a)
					}
					return fmt.Sprintf("CALL %s, R%d", in.s, in.r)
				case "RET":
					return fmt.Sprintf("RET R%d", in.a)
				case "DROP":
					return fmt.Sprintf("DROP R%d", in.a)
				}
				out := fmt.Sprintf("%s R%d", in.op, in.r)
				if in.a != 0 {
					out += fmt.Sprintf(", R%d", in.a)
				}
				if in.b != 0 {
					out += fmt.Sprintf(", R%d", in.b)
				}
				if in.s != "" {
					out += fmt.Sprintf(" ; %s", in.s)
				}
				return out
			}
			for name, fn := range functions {
				fmt.Printf("%s:\n", name)
				for _, ins := range fn.code {
					fmt.Printf("    %s\n", format(ins))
				}
			}
			continue
		}

		var run func(string, *bytes.Buffer) string
		run = func(fname string, out *bytes.Buffer) string {
			fn, ok := functions[fname]
			if !ok {
				return ""
			}
			regs := make([]string, fn.regs+10)
			ip := 0
			for ip < len(fn.code) {
				ins := fn.code[ip]
				switch ins.op {
				case "CONST":
					regs[ins.r] = ins.s
				case "PRINT":
					if ins.a < len(regs) {
						fmt.Fprintln(out, regs[ins.a])
					} else {
						out.WriteByte('\n')
					}
				case "CALL":
					regs[ins.r] = run(ins.s, out)
				case "RET":
					if ins.a < len(regs) {
						return regs[ins.a]
					}
					return ""
				}
				ip++
			}
			return ""
		}

		var buf bytes.Buffer
		run("main", &buf)
		if i < len(args)-1 {
			prev = buf.String()
		}
		if i == len(args)-1 {
			fmt.Print(buf.String())
		}
	}
}
