package scheme

import (
	"bufio"
	"fmt"
	"io"
	"strconv"
	"strings"
)

// Item represents a top-level Scheme definition discovered by the parser.
type Item struct {
	Kind   string        `json:"kind"`
	Name   string        `json:"name"`
	Params []string      `json:"params,omitempty"`
	Value  interface{}   `json:"value,omitempty"`
	Body   []interface{} `json:"body,omitempty"`
}

// Program holds the parsed representation of a Scheme source file.
type Program struct {
	Source string
	Items  []Item
}

// Parse parses Scheme source using a minimal S-expression parser implemented in Go.
func Parse(src string) (*Program, error) {
	p := &sexprParser{r: bufio.NewReader(strings.NewReader(src))}
	forms, err := p.parseForms()
	if err != nil {
		return nil, err
	}
	var items []Item
	for _, f := range forms {
		items = append(items, toItem(f)...)
	}
	return &Program{Source: src, Items: items}, nil
}

type sexprParser struct {
	r   *bufio.Reader
	buf []string
}

func isSpace(r rune) bool {
	return r == ' ' || r == '\n' || r == '\t' || r == '\r'
}

func (p *sexprParser) readToken() (string, error) {
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
			// comment to end of line
			if _, err := p.r.ReadString('\n'); err != nil && err != io.EOF {
				return "", err
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

func (p *sexprParser) parseAtom(tok string) interface{} {
	if tok == "#t" {
		return true
	}
	if tok == "#f" {
		return false
	}
	if strings.HasPrefix(tok, "\"") && strings.HasSuffix(tok, "\"") {
		return strings.Trim(tok, "\"")
	}
	if i, err := strconv.Atoi(tok); err == nil {
		return float64(i)
	}
	if f, err := strconv.ParseFloat(tok, 64); err == nil {
		return f
	}
	return tok
}

func (p *sexprParser) parseExpr() (interface{}, error) {
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
				p.readToken() // consume
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
	return p.parseAtom(tok), nil
}

func (p *sexprParser) peekToken() (string, error) {
	tok, err := p.readToken()
	if err != nil {
		return "", err
	}
	p.buf = append([]string{tok}, p.buf...)
	return tok, nil
}

func (p *sexprParser) parseForms() ([]interface{}, error) {
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
			return nil, err
		}
		forms = append(forms, expr)
	}
	return forms, nil
}

func toItem(x interface{}) []Item {
	list, ok := x.([]interface{})
	if !ok || len(list) == 0 {
		return nil
	}
	head, ok := list[0].(string)
	if !ok {
		return nil
	}
	switch head {
	case "define":
		if len(list) < 3 {
			return nil
		}
		name := list[1]
		body := list[2:]
		if nlist, ok := name.([]interface{}); ok && len(nlist) > 0 {
			fname, _ := nlist[0].(string)
			var params []string
			for _, p := range nlist[1:] {
				if s, ok := p.(string); ok {
					params = append(params, s)
				}
			}
			return []Item{{Kind: "func", Name: fname, Params: params}}
		}
		if n, ok := name.(string); ok {
			if len(body) == 1 {
				if l, ok := body[0].([]interface{}); ok && len(l) > 0 && l[0] == "lambda" {
					paramsList, _ := l[1].([]interface{})
					var params []string
					for _, p := range paramsList {
						if s, ok := p.(string); ok {
							params = append(params, s)
						}
					}
					funBody := []interface{}{}
					for _, b := range l[2:] {
						funBody = append(funBody, exprToVal(b))
					}
					return []Item{{Kind: "func", Name: n, Params: params, Body: funBody}}
				}
				return []Item{{Kind: "var", Name: n, Value: exprToVal(body[0])}}
			}
		}
	case "import":
		var items []Item
		for _, m := range list[1:] {
			if s, ok := m.(string); ok {
				items = append(items, Item{Kind: "import", Name: s})
			}
		}
		return items
	case "begin":
		var items []Item
		for _, b := range list[1:] {
			items = append(items, toItem(b)...)
		}
		return items
	case "set!":
		if len(list) == 3 {
			if n, ok := list[1].(string); ok {
				return []Item{{Kind: "assign", Name: n, Value: exprToVal(list[2])}}
			}
		}
	case "display":
		if len(list) == 2 {
			return []Item{{Kind: "print", Value: exprToVal(list[1])}}
		}
	case "newline":
		return nil
	}
	return nil
}

func exprToVal(x interface{}) interface{} {
	switch v := x.(type) {
	case []interface{}:
		if len(v) == 0 {
			return nil
		}
		head, _ := v[0].(string)
		switch head {
		case "list":
			var lst []interface{}
			for _, e := range v[1:] {
				lst = append(lst, exprToVal(e))
			}
			return map[string]interface{}{"list": lst}
		case "let":
			if len(v) >= 3 {
				bindsList, _ := v[1].([]interface{})
				var binds []interface{}
				for _, b := range bindsList {
					pair, _ := b.([]interface{})
					if len(pair) >= 2 {
						name, _ := pair[0].(string)
						binds = append(binds, map[string]interface{}{"name": name, "value": exprToVal(pair[1])})
					}
				}
				body := []interface{}{}
				for _, b := range v[2:] {
					body = append(body, exprToVal(b))
				}
				return map[string]interface{}{"let": binds, "body": body}
			}
		case "begin":
			var body []interface{}
			for _, b := range v[1:] {
				body = append(body, exprToVal(b))
			}
			return map[string]interface{}{"begin": body}
		case "cond":
			// convert cond to nested if
			expr := interface{}(nil)
			for i := len(v) - 1; i >= 1; i-- {
				clause, _ := v[i].([]interface{})
				if len(clause) == 0 {
					continue
				}
				if hd, ok := clause[0].(string); ok && hd == "else" {
					if len(clause) > 1 {
						expr = exprToVal(clause[1])
					}
					continue
				}
				cond := exprToVal(clause[0])
				then := interface{}(nil)
				if len(clause) > 1 {
					then = exprToVal(clause[1])
				}
				if expr == nil {
					expr = then
				} else {
					expr = map[string]interface{}{"call": "if", "args": []interface{}{cond, then, expr}}
				}
			}
			return expr
		default:
			var args []interface{}
			for _, a := range v[1:] {
				args = append(args, exprToVal(a))
			}
			return map[string]interface{}{"call": head, "args": args}
		}
	case string:
		return map[string]interface{}{"var": v}
	default:
		return v
	}
	return nil
}
