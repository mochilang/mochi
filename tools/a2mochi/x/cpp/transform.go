//go:build slow

package cpp

import (
	"fmt"
	"regexp"
	"strings"

	"mochi/ast"
	"mochi/parser"
)

var negRe = regexp.MustCompile(`([+\-*/])\s*-([A-Za-z0-9_]+)`) // reuse across transforms

// Transform parses C++ source and returns the corresponding Mochi AST node.
func Transform(p *Program) (*ast.Node, error) {
	if p == nil {
		return nil, fmt.Errorf("nil program")
	}

	prog := &ast.Node{Kind: "program"}

	addStmt := func(n *ast.Node, err error) error {
		if err != nil {
			return err
		}
		prog.Children = append(prog.Children, n)
		return nil
	}

	for _, g := range p.Globals {
		n := &ast.Node{Kind: "var", Value: g.Name}
		if g.Typ != "" {
			n.Children = append(n.Children, &ast.Node{Kind: "type", Value: g.Typ})
		}
		if g.Value != "" {
			expr, err := parseSingle(convertExpression(g.Value))
			if err != nil {
				return nil, err
			}
			n.Children = append(n.Children, expr)
		}
		if err := addStmt(n, nil); err != nil {
			return nil, err
		}
	}

	for _, st := range p.Structs {
		n := &ast.Node{Kind: "type", Value: st.Name}
		for _, f := range st.Fields {
			field := &ast.Node{Kind: "field", Value: f.Name}
			if f.Typ != "" {
				field.Children = append(field.Children, &ast.Node{Kind: "type", Value: f.Typ})
			}
			n.Children = append(n.Children, field)
		}
		if err := addStmt(n, nil); err != nil {
			return nil, err
		}
	}

	for _, e := range p.Enums {
		n := &ast.Node{Kind: "type", Value: e.Name}
		for _, v := range e.Variants {
			n.Children = append(n.Children, &ast.Node{Kind: "variant", Value: v})
		}
		if err := addStmt(n, nil); err != nil {
			return nil, err
		}
	}

	hasMain := false
	for _, f := range p.Funcs {
		if f.Name == "main" {
			hasMain = true
		}
		fn, err := transformFunc(f)
		if err != nil {
			return nil, err
		}
		if err := addStmt(fn, nil); err != nil {
			return nil, err
		}
	}

	if hasMain {
		prog.Children = append(prog.Children, &ast.Node{Kind: "call", Value: "main"})
	}

	return prog, nil
}

// --- Parsing helpers below (adapted from archived any2mochi) ---
func transformBody(body string) ([]*ast.Node, error) {
	lines := strings.Split(body, "\n")
	if len(lines) > 0 && strings.TrimSpace(lines[0]) == "{" {
		lines = lines[1:]
	}
	if len(lines) > 0 && strings.TrimSpace(lines[len(lines)-1]) == "}" {
		lines = lines[:len(lines)-1]
	}
	var out []*ast.Node
	for _, l := range lines {
		l = strings.TrimSpace(l)
		l = strings.TrimSuffix(l, ";")
		if strings.HasPrefix(l, "}") {
			l = strings.TrimSpace(strings.TrimPrefix(l, "}"))
			if l == "" {
				continue
			}
		}
		if l == "" {
			continue
		}
		switch {
		case strings.HasPrefix(l, "return"):
			expr := strings.TrimSpace(strings.TrimPrefix(l, "return"))
			if expr != "" {
				expr = convertExpression(expr)
				expr = negRe.ReplaceAllString(expr, `$1 (-$2)`)
				stmt, err := parseSingle("return " + expr)
				if err != nil {
					return nil, err
				}
				out = append(out, stmt)
			} else {
				stmt, err := parseSingle("return")
				if err != nil {
					return nil, err
				}
				out = append(out, stmt)
			}
		case l == "break":
			stmt, err := parseSingle("break")
			if err != nil {
				return nil, err
			}
			out = append(out, stmt)
		case l == "continue":
			stmt, err := parseSingle("continue")
			if err != nil {
				return nil, err
			}
			out = append(out, stmt)
		case strings.Contains(l, "std::cout") || strings.HasPrefix(l, "cout <<"):
			_ = strings.Contains(l, "std::boolalpha") || strings.Contains(l, "boolalpha <<")
			l = strings.TrimPrefix(l, "std::cout <<")
			l = strings.TrimPrefix(l, "cout <<")
			l = strings.TrimSpace(l)
			l = strings.TrimPrefix(l, "std::boolalpha <<")
			l = strings.TrimPrefix(l, "boolalpha <<")
			l = strings.TrimSuffix(l, "<< std::endl")
			l = strings.TrimSuffix(l, "<< endl")
			l = strings.TrimSpace(l)

			parts := strings.Split(l, "<<")
			for i := range parts {
				p := strings.TrimSpace(parts[i])
				if p == "' '" {
					p = "\" \""
				}
				p = convertExpression(p)
				parts[i] = strings.TrimSpace(p)
			}
			l = strings.Join(parts, " + ")

			l = negRe.ReplaceAllString(l, `$1 (-$2)`)
			stmt, err := parseSingle("print(" + l + ")")
			if err != nil {
				return nil, err
			}
			out = append(out, stmt)
		case strings.HasPrefix(l, "for (") && strings.Contains(l, ":"):
			re := regexp.MustCompile(`^for \((?:const\s+)?(?:auto|[\w:<>,]+)[\s*&]*([A-Za-z_][A-Za-z0-9_]*)\s*:\s*([^\)]+)\)\s*\{?$`)
			if m := re.FindStringSubmatch(l); m != nil {
				name := m[1]
				src := strings.TrimSpace(m[2])
				stmt, err := parseSingle(fmt.Sprintf("for %s in %s {", name, src))
				if err != nil {
					return nil, err
				}
				out = append(out, stmt)
			} else {
				stmt, err := parseSingle(l)
				if err != nil {
					return nil, err
				}
				out = append(out, stmt)
			}
		case strings.HasPrefix(l, "for ("):
			re := regexp.MustCompile(`^for \((?:[A-Za-z_][A-Za-z0-9_<>,\s]*\s+)?([A-Za-z_][A-Za-z0-9_]*)\s*=\s*([^;]+);[^;]+<\s*([^;]+);[^\)]*\)\s*\{?$`)
			if m := re.FindStringSubmatch(l); m != nil {
				stmt, err := parseSingle(fmt.Sprintf("for %s in %s..%s {", m[1], strings.TrimSpace(m[2]), strings.TrimSpace(m[3])))
				if err != nil {
					return nil, err
				}
				out = append(out, stmt)
			} else {
				stmt, err := parseSingle(l)
				if err != nil {
					return nil, err
				}
				out = append(out, stmt)
			}
		case strings.HasPrefix(l, "while ("):
			if m := regexp.MustCompile(`^while \((.*)\)\s*\{?$`).FindStringSubmatch(l); m != nil {
				cond := strings.TrimSpace(m[1])
				if strings.HasPrefix(cond, "(") && strings.HasSuffix(cond, ")") {
					cond = strings.TrimSuffix(strings.TrimPrefix(cond, "("), ")")
				}
				stmt, err := parseSingle(fmt.Sprintf("while %s {", cond))
				if err != nil {
					return nil, err
				}
				out = append(out, stmt)
			} else {
				stmt, err := parseSingle(l)
				if err != nil {
					return nil, err
				}
				out = append(out, stmt)
			}
		case strings.HasPrefix(l, "if ("):
			if m := regexp.MustCompile(`^if \((.*)\)\s*\{?$`).FindStringSubmatch(l); m != nil {
				cond := strings.TrimSpace(m[1])
				stmt, err := parseSingle(fmt.Sprintf("if %s {", cond))
				if err != nil {
					return nil, err
				}
				out = append(out, stmt)
			} else {
				stmt, err := parseSingle(l)
				if err != nil {
					return nil, err
				}
				out = append(out, stmt)
			}
		case strings.HasPrefix(l, "else if ("):
			if m := regexp.MustCompile(`^else if \((.*)\)\s*\{?$`).FindStringSubmatch(l); m != nil {
				cond := strings.TrimSpace(m[1])
				stmt, err := parseSingle(fmt.Sprintf("else if %s {", cond))
				if err != nil {
					return nil, err
				}
				out = append(out, stmt)
			} else {
				stmt, err := parseSingle(l)
				if err != nil {
					return nil, err
				}
				out = append(out, stmt)
			}
		case strings.HasPrefix(l, "else"):
			stmt, err := parseSingle("else {")
			if err != nil {
				return nil, err
			}
			out = append(out, stmt)
		default:
			decl := false
			if strings.HasPrefix(l, "const ") {
				l = strings.TrimPrefix(l, "const ")
			}
			for _, pre := range []string{"int ", "float ", "double ", "long ", "unsigned ", "bool ", "std::string ", "string ", "auto "} {
				if strings.HasPrefix(l, pre) {
					l = strings.TrimPrefix(l, pre)
					decl = true
					break
				}
			}
			if !decl {
				for _, pre := range []string{"std::vector<", "vector<", "std::map<", "std::unordered_map<", "map<", "unordered_map<", "std::set<", "set<", "std::array<", "array<"} {
					if strings.HasPrefix(l, pre) {
						if idx := strings.Index(l, ">"); idx != -1 {
							l = strings.TrimSpace(l[idx+1:])
							decl = true
						}
						break
					}
				}
			}
			if decl {
				l = "let " + l
			}
			l = convertExpression(l)
			l = negRe.ReplaceAllString(l, `$1 (-$2)`)
			stmt, err := parseSingle(l)
			if err != nil {
				return nil, err
			}
			out = append(out, stmt)
		}
	}
	return out, nil
}

func convertExpression(s string) string {
	s = strings.TrimSpace(s)
	if m := regexp.MustCompile(`^\(?\s*\[&\]\s*\{\s*auto\s+(\w+)\s*=\s*([A-Za-z_][A-Za-z0-9_]*)\s*;\s*\w+\.push_back\(([^)]+)\);\s*return\s+\w+;\s*\}\(\)\s*\)?$`).FindStringSubmatch(s); m != nil {
		list := convertExpression(strings.TrimSpace(m[2]))
		val := convertExpression(strings.TrimSpace(m[3]))
		return fmt.Sprintf("append(%s, %s)", list, val)
	}
	if strings.HasPrefix(s, "std::stoi(") && strings.HasSuffix(s, ")") {
		inner := strings.TrimSuffix(strings.TrimPrefix(s, "std::stoi("), ")")
		inner = convertExpression(inner)
		return inner + " as int"
	}
	if strings.HasPrefix(s, "stoi(") && strings.HasSuffix(s, ")") {
		inner := strings.TrimSuffix(strings.TrimPrefix(s, "stoi("), ")")
		inner = convertExpression(inner)
		return inner + " as int"
	}
	if m := regexp.MustCompile(`^(.*)\.find\((.*)\)\s*!=\s*std::string::npos$`).FindStringSubmatch(s); m != nil {
		recv := convertExpression(strings.TrimSpace(m[1]))
		arg := convertExpression(strings.TrimSpace(m[2]))
		return recv + ".contains(" + arg + ")"
	}
	if strings.Contains(s, ".substr(") {
		parts := strings.SplitN(s, ".substr(", 2)
		base := convertExpression(parts[0])
		args := strings.TrimSuffix(parts[1], ")")
		ap := strings.SplitN(args, ",", 2)
		if len(ap) == 2 {
			start := strings.TrimSpace(ap[0])
			length := strings.TrimSpace(ap[1])
			end := ""
			if m := regexp.MustCompile(`^([^\-]+)\s*-\s*(.+)$`).FindStringSubmatch(length); m != nil && strings.TrimSpace(m[2]) == start {
				end = convertExpression(strings.TrimSpace(m[1]))
			} else {
				end = start + " + " + convertExpression(length)
			}
			start = convertExpression(start)
			return fmt.Sprintf("substring(%s, %s, %s)", base, start, end)
		}
	}
	if strings.HasPrefix(s, "(") && strings.Contains(s, ")(") && strings.HasSuffix(s, ")") {
		content := strings.TrimSuffix(strings.TrimPrefix(s, "("), ")")
		parts := strings.SplitN(content, ")(", 2)
		if len(parts) == 2 {
			typ := strings.TrimSpace(parts[0])
			inner := convertExpression(strings.TrimSpace(parts[1]))
			if strings.HasPrefix(inner, typ+"{") && strings.HasSuffix(inner, "}") {
				inner = strings.TrimPrefix(inner, typ)
			}
			return inner + " as " + typ
		}
	}
	if strings.HasPrefix(s, "Todo{") && strings.HasSuffix(s, "}") {
		inner := strings.TrimSuffix(strings.TrimPrefix(s, "Todo{"), "}")
		fields := strings.Split(inner, ",")
		var pairs []string
		for _, f := range fields {
			fv := strings.SplitN(f, "=", 2)
			if len(fv) != 2 {
				continue
			}
			name := strings.TrimSpace(strings.TrimPrefix(fv[0], "."))
			val := convertExpression(strings.TrimSpace(fv[1]))
			pairs = append(pairs, fmt.Sprintf("\"%s\": %s", name, val))
		}
		return "{" + strings.Join(pairs, ", ") + "}"
	}
	if strings.HasSuffix(s, ".size()") {
		inner := strings.TrimSuffix(s, ".size()")
		inner = convertExpression(inner)
		return "len(" + inner + ")"
	}
	if strings.HasPrefix(s, "std::vector") && strings.Contains(s, "{") && strings.HasSuffix(s, "}") {
		start := strings.Index(s, "{")
		inner := strings.TrimSpace(s[start+1 : len(s)-1])
		return "[" + inner + "]"
	}
	if strings.HasPrefix(s, "std::string(") && strings.HasSuffix(s, ")") {
		return strings.TrimSuffix(strings.TrimPrefix(s, "std::string("), ")")
	}
	if strings.HasPrefix(s, "std::map") && strings.Contains(s, "{{") && strings.HasSuffix(s, "}}") {
		start := strings.Index(s, "{{")
		// Trim the surrounding "{{" and "}}" pairs.
		inner := s[start+1 : len(s)-1]
		parts := strings.Split(inner, "},")
		var pairs []string
		for _, p := range parts {
			p = strings.Trim(strings.TrimSpace(p), "{}")
			kv := strings.SplitN(p, ",", 2)
			if len(kv) != 2 {
				continue
			}
			k := convertExpression(strings.TrimSpace(kv[0]))
			v := convertExpression(strings.TrimSpace(kv[1]))
			pairs = append(pairs, fmt.Sprintf("%s: %s", k, v))
		}
		return "{" + strings.Join(pairs, ", ") + "}"
	}
	if strings.Contains(s, "std::string(") {
		re := regexp.MustCompile(`std::string\(([^()]*)\)`)
		s = re.ReplaceAllString(s, `$1`)
	}
	return s
}

func parseSingle(src string) (*ast.Node, error) {

	prog, err := parser.ParseString(src)
	if err != nil {
		return nil, err
	}
	node := ast.FromProgram(prog)
	if len(node.Children) == 0 {
		return nil, fmt.Errorf("empty node")
	}
	return node.Children[0], nil
}

func transformFunc(f Func) (*ast.Node, error) {
	n := &ast.Node{Kind: "fun", Value: f.Name}
	for _, p := range f.Params {
		pn := &ast.Node{Kind: "param", Value: p.Name}
		if p.Typ != "" {
			pn.Children = append(pn.Children, &ast.Node{Kind: "type", Value: p.Typ})
		}
		n.Children = append(n.Children, pn)
	}
	if f.Ret != "" && f.Ret != "void" {
		n.Children = append(n.Children, &ast.Node{Kind: "type", Value: f.Ret})
	}
	stmts, err := transformBody(f.Body)
	if err != nil {
		return nil, err
	}
	n.Children = append(n.Children, stmts...)
	return n, nil
}
