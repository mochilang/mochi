package scheme

import (
	"strings"
)

// convertSimple converts Scheme source code using the internal parser when no
// language server is available. It translates a very small subset of Scheme
// used in the golden tests back into Mochi.
func convertSimple(src string) ([]byte, error) {
	nodes, err := Parse(src)
	if err != nil {
		return nil, err
	}
	var out strings.Builder
	for _, n := range nodes {
		lines := convertNode(n)
		for _, ln := range lines {
			out.WriteString(ln)
			if !strings.HasSuffix(ln, "\n") {
				out.WriteByte('\n')
			}
		}
	}
	if out.Len() == 0 {
		return nil, nil
	}
	return []byte(out.String()), nil
}

func convertNode(n Node) []string {
	if n.Atom != "" {
		return nil
	}
	if len(n.List) == 0 {
		return nil
	}
	head := n.List[0].Atom
	switch head {
	case "define":
		return convertDefine(n)
	case "set!":
		if len(n.List) == 3 && n.List[1].Atom != "" {
			return []string{n.List[1].Atom + " = " + expr(n.List[2])}
		}
	case "begin":
		return convertBegin(n.List[1:])
	case "if":
		return convertIf(n)
	case "let":
		if f := convertFor(n); f != nil {
			return f
		}
		if w := convertWhile(n); w != nil {
			return w
		}
	}
	return nil
}

func convertDefine(n Node) []string {
	if len(n.List) < 3 {
		return nil
	}
	def := n.List[1]
	if len(def.List) > 0 {
		name := def.List[0].Atom
		params := []string{}
		for _, p := range def.List[1:] {
			if p.Atom != "" {
				params = append(params, p.Atom)
			}
		}
		body := n.List[2]
		if call, ok := unwrapCallCC(body); ok {
			return []string{"let " + name + " = fun(" + strings.Join(params, ", ") + ") => " + expr(call)}
		}
		return []string{"fun " + name + "(" + strings.Join(params, ", ") + ") {}"}
	}
	if def.Atom != "" {
		val := expr(n.List[2])
		return []string{"let " + def.Atom + " = " + val}
	}
	return nil
}

func convertBegin(nodes []Node) []string {
	var out []string
	for _, c := range nodes {
		if c.Atom == "" && len(c.List) > 0 && c.List[0].Atom == "display" {
			if len(c.List) > 1 {
				out = append(out, "print("+expr(c.List[1])+")")
			}
			continue
		}
		if isPrint(c) {
			target := c.List[1]
			if target.List[0].Atom == "begin" {
				target = target.List[1]
			}
			out = append(out, "print("+expr(target.List[1])+")")
			continue
		}
		if setLines := convertNode(c); len(setLines) > 0 {
			out = append(out, setLines...)
			continue
		}
	}
	return out
}

func convertIf(n Node) []string {
	if len(n.List) < 3 {
		return nil
	}
	cond := expr(n.List[1])
	thenLines := convertBegin([]Node{n.List[2]})
	var elseLines []string
	if len(n.List) >= 4 {
		elseNode := n.List[3]
		if elseNode.Atom == "" && len(elseNode.List) > 0 && elseNode.List[0].Atom == "if" {
			elseLines = convertIf(elseNode)
			if len(elseLines) > 0 {
				elseLines[0] = "else " + elseLines[0]
			}
		} else {
			elseLines = convertBegin([]Node{elseNode})
			if len(elseLines) > 0 {
				elseLines = append([]string{"else {"}, indent(elseLines)...)
				elseLines = append(elseLines, "}")
			}
		}
	}
	out := []string{"if " + cond + " {"}
	out = append(out, indent(thenLines)...)
	if len(elseLines) == 0 && len(n.List) >= 4 {
		elseLines = []string{"else {}"}
	}
	if len(elseLines) > 0 {
		if strings.HasPrefix(elseLines[0], "else if") {
			out = append(out, "}"+" "+elseLines[0])
			out = append(out, elseLines[1:]...)
		} else {
			out = append(out, "}")
			out = append(out, elseLines...)
		}
	} else {
		out = append(out, "}")
	}
	return out
}

func convertFor(n Node) []string {
	if len(n.List) != 4 || n.List[0].Atom != "let" || n.List[1].Atom == "" {
		return nil
	}
	if len(n.List[2].List) != 1 {
		return nil
	}
	bind := n.List[2].List[0]
	if len(bind.List) != 2 || bind.List[0].Atom == "" {
		return nil
	}
	idx := bind.List[0].Atom
	start := expr(bind.List[1])

	bodyIf := n.List[3]
	if bodyIf.Atom != "" || len(bodyIf.List) < 3 || bodyIf.List[0].Atom != "if" {
		return nil
	}
	cond := bodyIf.List[1]
	if cond.Atom != "" || len(cond.List) != 3 || cond.List[0].Atom != "<" || cond.List[1].Atom != idx {
		return nil
	}
	end := expr(cond.List[2])
	body := bodyIf.List[2]
	if body.Atom != "" || len(body.List) < 1 || body.List[0].Atom != "begin" {
		return nil
	}
	stmts := body.List[1:]
	// detect optional element binding
	name := ""
	src := ""
	if len(stmts) > 0 {
		first := stmts[0]
		if first.Atom == "" && len(first.List) >= 3 && first.List[0].Atom == "let" {
			b := first.List[1]
			if len(b.List) == 1 && len(b.List[0].List) == 2 && b.List[0].List[0].Atom != "" {
				get := b.List[0].List[1]
				if get.Atom == "" && len(get.List) == 3 && (get.List[0].Atom == "list-ref" || get.List[0].Atom == "string-ref") && get.List[2].Atom == idx {
					name = b.List[0].List[0].Atom
					src = expr(get.List[1])
					inner := first.List[2]
					if inner.Atom == "" && len(inner.List) > 0 && inner.List[0].Atom == "begin" {
						stmts = append(inner.List[1:], stmts[1:]...)
					} else {
						stmts = append(inner.List, stmts[1:]...)
					}
				}
			}
		}
	}
	// drop recursive call
	if len(stmts) > 0 {
		last := stmts[len(stmts)-1]
		if last.Atom == "" && len(last.List) >= 1 && last.List[0].Atom == n.List[1].Atom {
			stmts = stmts[:len(stmts)-1]
		}
	}
	lines := convertBegin(stmts)
	var out []string
	if src != "" && name != "" {
		out = []string{"for " + name + " in " + src + " {"}
	} else {
		out = []string{"for " + idx + " in " + start + ".." + end + " {"}
	}
	out = append(out, indent(lines)...)
	out = append(out, "}")
	return out
}

func convertWhile(n Node) []string {
	if len(n.List) != 4 || n.List[1].Atom == "" || len(n.List[2].List) != 0 {
		return nil
	}
	bodyIf := n.List[3]
	if bodyIf.Atom != "" || len(bodyIf.List) < 3 || bodyIf.List[0].Atom != "if" {
		return nil
	}
	cond := expr(bodyIf.List[1])
	body := bodyIf.List[2]
	lines := convertBegin(body.List[1 : len(body.List)-1])
	out := []string{"while " + cond + " {"}
	out = append(out, indent(lines)...)
	out = append(out, "}")
	return out
}

func unwrapCallCC(n Node) (Node, bool) {
	if n.Atom == "" && len(n.List) == 2 && n.List[0].Atom == "call/cc" {
		lam := n.List[1]
		if len(lam.List) == 3 && lam.List[0].Atom == "lambda" {
			call := lam.List[2]
			if call.Atom == "" && len(call.List) == 2 && call.List[0].Atom == "return" {
				return call.List[1], true
			}
		}
	}
	return Node{}, false
}

func isPrint(n Node) bool {
	if n.Atom != "" || len(n.List) < 2 {
		return false
	}
	if n.List[0].Atom != "begin" {
		return false
	}
	if len(n.List[1].List) == 2 && n.List[1].List[0].Atom == "display" {
		return true
	}
	if len(n.List) >= 2 && len(n.List[1].List) == 3 && n.List[1].List[0].Atom == "begin" {
		inner := n.List[1].List
		if len(inner[1].List) == 2 && inner[1].List[0].Atom == "display" {
			return true
		}
	}
	return false
}

func indent(lines []string) []string {
	if len(lines) == 0 {
		return lines
	}
	out := make([]string, len(lines))
	for i, l := range lines {
		out[i] = "  " + l
	}
	return out
}

func expr(n Node) string {
	if n.Atom != "" {
		switch n.Atom {
		case "'()":
			return "[]"
		default:
			return n.Atom
		}
	}
	if len(n.List) == 0 {
		return ""
	}
	head := n.List[0].Atom
	switch head {
	case "+", "-", "*", "/", "<", ">", "<=", ">=", "=":
		parts := make([]string, len(n.List)-1)
		for i := 1; i < len(n.List); i++ {
			parts[i-1] = maybeParen(n.List[i])
		}
		return strings.Join(parts, " "+head+" ")
	case "list":
		elems := make([]string, len(n.List)-1)
		for i := 1; i < len(n.List); i++ {
			elems[i-1] = expr(n.List[i])
		}
		return "[" + strings.Join(elems, ", ") + "]"
	case "list-ref", "string-ref":
		if len(n.List) == 3 {
			return expr(n.List[1]) + "[" + expr(n.List[2]) + "]"
		}
	case "lambda":
		if len(n.List) >= 3 {
			params := n.List[1].List
			ps := make([]string, len(params))
			for i, p := range params {
				ps[i] = p.Atom
			}
			if body, ok := unwrapCallCC(n.List[2]); ok {
				return "fun(" + strings.Join(ps, ", ") + ") => " + expr(body)
			}
		}
	default:
		args := make([]string, len(n.List)-1)
		for i := 1; i < len(n.List); i++ {
			args[i-1] = expr(n.List[i])
		}
		return head + "(" + strings.Join(args, ", ") + ")"
	}
	return n.String()
}

func maybeParen(n Node) string {
	e := expr(n)
	if n.Atom == "" && len(n.List) > 0 {
		head := n.List[0].Atom
		switch head {
		case "+", "-", "*", "/", "<", ">", "<=", ">=", "=":
			return "(" + e + ")"
		}
	}
	return e
}
