package main

import (
	"bufio"
	"flag"
	"fmt"
	"io"
	"os"
	"os/exec"
	"strings"
)

type parser struct {
	r   *bufio.Reader
	buf []string
}

type stringLit string

func newParser(src string) *parser {
	return &parser{r: bufio.NewReader(strings.NewReader(src))}
}

func isSpace(r rune) bool {
	return r == ' ' || r == '\n' || r == '\t' || r == '\r'
}

func (p *parser) readToken() (string, error) {
	if len(p.buf) > 0 {
		tok := p.buf[0]
		p.buf = p.buf[1:]
		return tok, nil
	}
	for {
		ch, _, err := p.r.ReadRune()
		if err != nil {
			return "", err
		}
		if isSpace(ch) {
			continue
		}
		if ch == ';' {
			for {
				c, _, err := p.r.ReadRune()
				if err != nil || c == '\n' {
					break
				}
			}
			continue
		}
		if ch == '(' || ch == ')' {
			return string(ch), nil
		}
		if ch == '"' {
			var sb strings.Builder
			for {
				c, _, err := p.r.ReadRune()
				if err != nil {
					return "", err
				}
				if c == '\\' {
					d, _, err := p.r.ReadRune()
					if err != nil {
						return "", err
					}
					sb.WriteRune(d)
					continue
				}
				if c == '"' {
					break
				}
				sb.WriteRune(c)
			}
			return "\"" + sb.String() + "\"", nil
		}
		var sb strings.Builder
		sb.WriteRune(ch)
		for {
			r, _, err := p.r.ReadRune()
			if err == io.EOF {
				return sb.String(), nil
			}
			if err != nil {
				return "", err
			}
			if isSpace(r) || r == '(' || r == ')' {
				p.r.UnreadRune()
				return sb.String(), nil
			}
			sb.WriteRune(r)
		}
	}
}

func (p *parser) peekToken() (string, error) {
	tok, err := p.readToken()
	if err != nil {
		return "", err
	}
	p.buf = append([]string{tok}, p.buf...)
	return tok, nil
}

func parseAtom(tok string) interface{} {
	switch tok {
	case "#t":
		return true
	case "#f":
		return false
	}
	if strings.HasPrefix(tok, "\"") && strings.HasSuffix(tok, "\"") {
		return stringLit(strings.Trim(tok, "\""))
	}
	if strings.HasPrefix(tok, "#\\") && len(tok) > 2 {
		r := []rune(tok[2:])
		if len(r) > 0 {
			return r[0]
		}
	}
	return tok
}

func (p *parser) parseExpr() (interface{}, error) {
	tok, err := p.readToken()
	if err != nil {
		return nil, err
	}
	if tok == "(" {
		var list []interface{}
		for {
			next, err := p.peekToken()
			if err != nil {
				return nil, err
			}
			if next == ")" {
				p.readToken()
				break
			}
			elem, err := p.parseExpr()
			if err != nil {
				return nil, err
			}
			list = append(list, elem)
		}
		return list, nil
	}
	if tok == ")" {
		return nil, fmt.Errorf("unexpected )")
	}
	return parseAtom(tok), nil
}

func (p *parser) parseForms() ([]interface{}, error) {
	var forms []interface{}
	for {
		tok, err := p.peekToken()
		if err == io.EOF {
			break
		}
		if err != nil {
			return nil, err
		}
		if tok == "" {
			break
		}
		expr, err := p.parseExpr()
		if err != nil {
			if err == io.EOF {
				break
			}
			return nil, err
		}
		forms = append(forms, expr)
	}
	return forms, nil
}

func compileTypeArg(arg []interface{}) string {
	if len(arg) == 0 {
		return ""
	}
	names := make([]string, len(arg)-1)
	for i := 0; i < len(arg)-1; i++ {
		names[i] = compileExpr(arg[i])
	}
	typ := compileExpr(arg[len(arg)-1])
	return fmt.Sprintf("%s %s", strings.Join(names, ","), typ)
}

func compileExpr(e interface{}) string {
	switch v := e.(type) {
	case []interface{}:
		if len(v) == 0 {
			return ""
		}
		head, _ := v[0].(string)
		switch head {
		case "define-syntax", "define-syntax-rule", "defmacro":
			return ""
		case "import":
			var lines []string
			for _, x := range v[1:] {
				lines = append(lines, fmt.Sprintf("import %s\n", compileExpr(x)))
			}
			return strings.Join(lines, "")
		case "defn":
			name := compileExpr(v[1])
			var args []string
			if lst, ok := v[2].([]interface{}); ok {
				for _, a := range lst {
					if alst, ok := a.([]interface{}); ok {
						args = append(args, compileTypeArg(alst))
					}
				}
			}
			ret := compileExpr(v[3])
			var body []string
			for _, b := range v[4:] {
				body = append(body, compileExpr(b))
			}
			return fmt.Sprintf("func %s (%s) %s {\n%s;}", name, strings.Join(args, ","), ret, strings.Join(body, ";\n"))
		case "def":
			var parts []string
			for i := 1; i+1 < len(v); i += 2 {
				left := compileExpr(v[i])
				right := compileExpr(v[i+1])
				parts = append(parts, fmt.Sprintf("%s:=%s", left, right))
			}
			return strings.Join(parts, ";\n")
		case "var":
			name := compileExpr(v[1])
			typ := compileExpr(v[2])
			val := ""
			if len(v) > 3 {
				val = "=" + compileExpr(v[3])
			}
			return fmt.Sprintf("var %s %s %s", name, typ, val)
		case "+", "-", "*", "/", "<<", ">>":
			var parts []string
			for _, a := range v[1:] {
				parts = append(parts, compileExpr(a))
			}
			return "(" + strings.Join(parts, fmt.Sprintf(" %s ", head)) + ")"
		case "<-":
			if len(v) == 2 {
				return fmt.Sprintf("(<- %s)", compileExpr(v[1]))
			}
			return fmt.Sprintf("(%s <- %s)", compileExpr(v[1]), compileExpr(v[2]))
		case "++", "--":
			return fmt.Sprintf("%s%s", compileExpr(v[1]), head)
		case "<", "<=", ">", ">=", "==", "!=", "%":
			return fmt.Sprintf("(%s %s %s)", compileExpr(v[1]), head, compileExpr(v[2]))
		case "=", "+=", "-=", "*=", "/=":
			return fmt.Sprintf("%s %s %s", compileExpr(v[1]), head, compileExpr(v[2]))
		case "and":
			return fmt.Sprintf("(%s && %s)", compileExpr(v[1]), compileExpr(v[2]))
		case "or":
			return fmt.Sprintf("(%s || %s)", compileExpr(v[1]), compileExpr(v[2]))
		case "not":
			return fmt.Sprintf("(!%s)", compileExpr(v[1]))
		case "return":
			return fmt.Sprintf("return %s", compileExpr(v[1]))
		case "if":
			if len(v) == 4 {
				return fmt.Sprintf("if %s {%s} else {%s}", compileExpr(v[1]), compileExpr(v[2]), compileExpr(v[3]))
			}
			return fmt.Sprintf("if %s {%s}", compileExpr(v[1]), compileExpr(v[2]))
		case "when":
			var body []string
			for _, b := range v[2:] {
				body = append(body, compileExpr(b))
			}
			return fmt.Sprintf("if %s {\n%s;}", compileExpr(v[1]), strings.Join(body, ";\n"))
		case "while":
			var body []string
			for _, b := range v[2:] {
				body = append(body, compileExpr(b))
			}
			return fmt.Sprintf("for %s {\n%s;}", compileExpr(v[1]), strings.Join(body, ";\n"))
		case "for":
			var body []string
			for _, b := range v[4:] {
				body = append(body, compileExpr(b))
			}
			return fmt.Sprintf("for %s; %s; %s {\n%s;}", compileExpr(v[1]), compileExpr(v[2]), compileExpr(v[3]), strings.Join(body, ";\n"))
		case "do":
			var body []string
			for _, b := range v[1:] {
				body = append(body, compileExpr(b))
			}
			return fmt.Sprintf("{%s;}", strings.Join(body, ";\n"))
		case "dot":
			return fmt.Sprintf("%s.%s", compileExpr(v[1]), compileExpr(v[2]))
		case "comma":
			var parts []string
			for _, b := range v[1:] {
				parts = append(parts, compileExpr(b))
			}
			return strings.Join(parts, ",")
		case "at":
			idx := ""
			if len(v) == 3 {
				idx = compileExpr(v[2])
			}
			return fmt.Sprintf("%s[%s]", compileExpr(v[1]), idx)
		case "main":
			var body []string
			for _, b := range v[1:] {
				body = append(body, compileExpr(b))
			}
			return fmt.Sprintf("func main() {\n%s;}", strings.Join(body, ";\n"))
		case "label":
			return fmt.Sprintf("%s:", compileExpr(v[1]))
		case "goto":
			return fmt.Sprintf("goto %s", compileExpr(v[1]))
		case "switch":
			var body []string
			for _, b := range v[2:] {
				body = append(body, compileExpr(b))
			}
			return fmt.Sprintf("switch %s {\n%s;}", compileExpr(v[1]), strings.Join(body, ";\n"))
		case "case":
			var parts []string
			for _, x := range v[1:] {
				parts = append(parts, fmt.Sprintf("case %s:", compileExpr(x)))
			}
			return strings.Join(parts, "")
		case "default":
			return "default:"
		case "fn":
			var args []string
			if lst, ok := v[1].([]interface{}); ok {
				for _, a := range lst {
					if alst, ok := a.([]interface{}); ok {
						args = append(args, compileTypeArg(alst))
					}
				}
			}
			ret := compileExpr(v[2])
			var body []string
			for _, b := range v[3:] {
				body = append(body, compileExpr(b))
			}
			return fmt.Sprintf("func (%s) %s {\n%s;}", strings.Join(args, ","), ret, strings.Join(body, ";\n"))
		case "type":
			return fmt.Sprintf("type %s %s", compileExpr(v[1]), compileExpr(v[2]))
		case "struct":
			var fields []string
			if lst, ok := v[1].([]interface{}); ok {
				for _, a := range lst {
					if alst, ok := a.([]interface{}); ok {
						fields = append(fields, compileTypeArg(alst))
					}
				}
			}
			return fmt.Sprintf("struct {%s}", strings.Join(fields, ";"))
		case "code":
			return compileExpr(v[1])
		case "format":
			if len(v) < 2 {
				return ""
			}
			fmtstr := compileExpr(v[1])
			var args []interface{}
			for _, a := range v[2:] {
				args = append(args, compileExpr(a))
			}
			return fmt.Sprintf(fmtstr, args...)
		default:
			var args []string
			for _, a := range v[1:] {
				args = append(args, compileExpr(a))
			}
			return fmt.Sprintf("%s(%s)", compileExpr(v[0]), strings.Join(args, ","))
		}
	case rune:
		return fmt.Sprintf("'%c'", v)
	case stringLit:
		return fmt.Sprintf("%q", string(v))
	case string:
		return v
	default:
		return fmt.Sprintf("%v", v)
	}
}

func main() {
	var compileOnly bool
	var verbose bool
	var outName string

	flag.BoolVar(&compileOnly, "c", false, "Compile only; do not run")
	flag.BoolVar(&compileOnly, "compile", false, "Compile only; do not run")
	flag.BoolVar(&verbose, "v", false, "Display verbose messages")
	flag.BoolVar(&verbose, "verbose", false, "Display verbose messages")
	flag.StringVar(&outName, "o", "a-out", "Place the output into <file>")
	flag.Parse()
	filenames := flag.Args()

	var input string
	if len(filenames) == 0 {
		fmt.Println("Kome Compiler")
		fmt.Println("Enter code (EOF when done):")
		buf, _ := io.ReadAll(os.Stdin)
		input = string(buf)
	} else {
		var parts []string
		for _, f := range filenames {
			data, err := os.ReadFile(f)
			if err != nil {
				fmt.Fprintln(os.Stderr, err)
				os.Exit(1)
			}
			parts = append(parts, string(data))
		}
		input = strings.Join(parts, "")
	}

	code := fmt.Sprintf("(%s\n)", input)
	forms, err := newParser(code).parseForms()
	if len(forms) == 1 {
		if lst, ok := forms[0].([]interface{}); ok {
			forms = lst
		}
	}
	if err != nil {
		fmt.Fprintln(os.Stderr, err)
		os.Exit(1)
	}
	var outParts []string
	for _, f := range forms {
		outParts = append(outParts, compileExpr(f))
	}
	compiled := "package main\n" + strings.Join(outParts, "\n") + "\n"

	if verbose {
		fmt.Println("Compiled:")
		fmt.Println(compiled)
	}

	outsrc := outName + ".go"
	if err := os.WriteFile(outsrc, []byte(compiled), 0644); err != nil {
		fmt.Fprintln(os.Stderr, err)
		os.Exit(1)
	}

	dir, _ := os.Getwd()
	if verbose {
		fmt.Printf("Current directory: %s\n", dir)
		fmt.Printf("Output written to: %s\n", outsrc)
	}

	exec.Command("go", "fmt", outsrc).Run()
	if verbose {
		if data, err := os.ReadFile(outsrc); err == nil {
			fmt.Println(string(data))
		}
	}
	if err := exec.Command("go", "build", outsrc).Run(); err == nil {
		if verbose {
			fmt.Printf("Binary written to: %s\n", outName)
		}
		if !compileOnly {
			cmd := exec.Command("./" + outName)
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			cmd.Stdin = os.Stdin
			cmd.Run()
		}
	}
}
