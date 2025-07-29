//go:build slow

package cpp

import (
	"fmt"
	"regexp"
	"strings"

	"mochi/ast"
	"mochi/parser"
)

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
type frame struct {
	block  *ast.Node
	ifNode *ast.Node
}

func transformBody(body string) ([]*ast.Node, error) {
	lines := strings.Split(body, "\n")
	if len(lines) > 0 && strings.TrimSpace(lines[0]) == "{" {
		lines = lines[1:]
	}
	if len(lines) > 0 && strings.TrimSpace(lines[len(lines)-1]) == "}" {
		lines = lines[:len(lines)-1]
	}

	for i := 0; i < len(lines); i++ {
		trimmed := strings.TrimSpace(lines[i])
		if strings.HasPrefix(trimmed, "} else {") {
			lines[i] = "}"
			lines = append(lines[:i+1], append([]string{"else {"}, lines[i+1:]...)...)
		}
	}

	var out []*ast.Node
	var stack []frame

	addStmt := func(n *ast.Node) {
		if len(stack) == 0 {
			out = append(out, n)
		} else {
			stack[len(stack)-1].block.Children = append(stack[len(stack)-1].block.Children, n)
		}
	}

	nextLine := func(i int) string {
		if i+1 < len(lines) {
			return strings.TrimSpace(lines[i+1])
		}
		return ""
	}

	rangeFor := regexp.MustCompile(`^for \((?:const\s+)?(?:auto|[\w:<>,]+)[\s*&]*([A-Za-z_][A-Za-z0-9_]*)\s*:\s*([^\)]+)\)\s*\{$`)
	cFor := regexp.MustCompile(`^for \((?:[A-Za-z_][A-Za-z0-9_<>,\s]*\s+)?([A-Za-z_][A-Za-z0-9_]*)\s*=\s*([^;]+);[^;]+<\s*([^;]+);[^\)]*\)\s*\{$`)
	whileRe := regexp.MustCompile(`^while \((.*)\)\s*\{$`)
	ifRe := regexp.MustCompile(`^if \((.*)\)\s*\{$`)

	for i := 0; i < len(lines); i++ {
		l := strings.TrimSpace(lines[i])
		l = strings.TrimSuffix(l, ";")
		if l == "" {
			continue
		}

		// handle end of block and optional else
		if l == "}" {
			if len(stack) > 0 {
				fr := stack[len(stack)-1]
				stack = stack[:len(stack)-1]
				// else clause follows
				nl := nextLine(i)
				if nl == "else {" && fr.ifNode != nil {
					elseBlock := &ast.Node{Kind: "block"}
					fr.ifNode.Children = append(fr.ifNode.Children, elseBlock)
					stack = append(stack, frame{block: elseBlock})
					i++
				}
			}
			continue
		}

		switch {
		case strings.HasPrefix(l, "return"):
			expr := strings.TrimSpace(strings.TrimPrefix(l, "return"))
			if expr != "" {
				expr = convertExpression(expr)
				negRe := regexp.MustCompile(`([+\-*/])\s*-([A-Za-z0-9_]+)`)
				expr = negRe.ReplaceAllString(expr, `$1 (-$2)`)
				stmt, err := parseSingle("return " + expr)
				if err != nil {
					return nil, err
				}
				addStmt(stmt)
			} else {
				stmt, err := parseSingle("return")
				if err != nil {
					return nil, err
				}
				addStmt(stmt)
			}
		case l == "break":
			stmt, err := parseSingle("break")
			if err != nil {
				return nil, err
			}
			addStmt(stmt)
		case l == "continue":
			stmt, err := parseSingle("continue")
			if err != nil {
				return nil, err
			}
			addStmt(stmt)
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

			negRe := regexp.MustCompile(`([+\-*/])\s*-([A-Za-z0-9_]+)`)
			l = negRe.ReplaceAllString(l, `$1 (-$2)`)
			stmt, err := parseSingle("print(" + l + ")")
			if err != nil {
				return nil, err
			}
			addStmt(stmt)
		case rangeFor.MatchString(l):
			m := rangeFor.FindStringSubmatch(l)
			stmt, err := parseSingle(fmt.Sprintf("for %s in %s {}", m[1], strings.TrimSpace(m[2])))
			if err != nil {
				return nil, err
			}
			addStmt(stmt)
			blk := stmt.Children[len(stmt.Children)-1]
			stack = append(stack, frame{block: blk})
		case cFor.MatchString(l):
			m := cFor.FindStringSubmatch(l)
			stmt, err := parseSingle(fmt.Sprintf("for %s in %s..%s {}", m[1], strings.TrimSpace(m[2]), strings.TrimSpace(m[3])))
			if err != nil {
				return nil, err
			}
			addStmt(stmt)
			blk := stmt.Children[len(stmt.Children)-1]
			stack = append(stack, frame{block: blk})
		case whileRe.MatchString(l):
			m := whileRe.FindStringSubmatch(l)
			cond := strings.TrimSpace(m[1])
			if strings.HasPrefix(cond, "(") && strings.HasSuffix(cond, ")") {
				cond = strings.TrimSuffix(strings.TrimPrefix(cond, "("), ")")
			}
			stmt, err := parseSingle(fmt.Sprintf("while %s {}", cond))
			if err != nil {
				return nil, err
			}
			addStmt(stmt)
			blk := stmt.Children[len(stmt.Children)-1]
			stack = append(stack, frame{block: blk})
		case ifRe.MatchString(l):
			m := ifRe.FindStringSubmatch(l)
			cond := strings.TrimSpace(m[1])
			stmt, err := parseSingle(fmt.Sprintf("if %s {}", cond))
			if err != nil {
				return nil, err
			}
			addStmt(stmt)
			blk := stmt.Children[len(stmt.Children)-1]
			stack = append(stack, frame{block: blk, ifNode: stmt})
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
			negRe := regexp.MustCompile(`([+\-*/])\s*-([A-Za-z0-9_]+)`)
			l = negRe.ReplaceAllString(l, `$1 (-$2)`)
			stmt, err := parseSingle(l)
			if err != nil {
				return nil, err
			}
			addStmt(stmt)
		}
	}
	if len(stack) != 0 {
		return nil, fmt.Errorf("unbalanced braces")
	}
	return out, nil
}

func convertExpression(s string) string {
	s = strings.TrimSpace(s)
	for {
		if len(s) >= 2 && s[0] == '(' && s[len(s)-1] == ')' {
			depth := 0
			balanced := true
			for i, r := range s {
				if r == '(' {
					depth++
				} else if r == ')' {
					depth--
					if depth == 0 && i < len(s)-1 {
						balanced = false
						break
					}
				}
			}
			if balanced && depth == 0 {
				s = strings.TrimSpace(s[1 : len(s)-1])
				continue
			}
		}
		break
	}
	// handle ternary operator
	depth := 0
	q := -1
	colon := -1
	for i, r := range s {
		switch r {
		case '?':
			if depth == 0 && q == -1 {
				q = i
			}
		case ':':
			if depth == 0 && q != -1 {
				if i > 0 && s[i-1] == ':' {
					break
				}
				if i+1 < len(s) && s[i+1] == ':' {
					break
				}
				colon = i
				break
			}
		case '(', '[', '{':
			depth++
		case ')', ']', '}':
			if depth > 0 {
				depth--
			}
		}
	}
	if q != -1 && colon != -1 {
		cond := convertExpression(strings.TrimSpace(s[:q]))
		yes := convertExpression(strings.TrimSpace(s[q+1 : colon]))
		no := convertExpression(strings.TrimSpace(s[colon+1:]))
		return fmt.Sprintf("if %s then %s else %s", cond, yes, no)
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
                inner := s[start+2 : len(s)-2]
                parts := strings.Split(inner, "},")
                var pairs []string
                for _, p := range parts {
                        p = strings.TrimSpace(strings.Trim(p, "{}"))
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
