package c

import (
	"fmt"
	"regexp"
	"strconv"
	"strings"

	"mochi/ast"
)

// Transform converts a parsed Program into a Mochi AST.
func Transform(p *Program) (*ast.Node, error) {
	if p == nil || p.Root == nil {
		return nil, fmt.Errorf("nil program")
	}
	prints := findPrints(p.Root)
	prints = append(prints, parseArrayLoops(p.Source)...)
	root := &ast.Node{Kind: "program"}
	for _, pr := range prints {
		arg := valueNode(pr)
		root.Children = append(root.Children, &ast.Node{
			Kind:     "call",
			Value:    "print",
			Children: []*ast.Node{arg},
		})
	}
	return root, nil
}

func findPrints(n *Node) []string {
	prints := make([]string, 0)
	vars := make(map[string]string)
	walkPrints(n, vars, &prints)
	return prints
}

func walkPrints(n *Node, vars map[string]string, prints *[]string) {
	switch n.Kind {
	case "VarDecl":
		if n.Name != "" {
			if v := valueWithVars(n, vars); v != "" {
				vars[n.Name] = v
			}
		}
	case "BinaryOperator":
		if len(n.Inner) >= 2 && n.Inner[0].Kind == "DeclRefExpr" {
			name := walkName(&n.Inner[0])
			if name != "" {
				if v := valueWithVars(&n.Inner[1], vars); v != "" {
					vars[name] = v
				}
			}
		}
	case "CallExpr":
		if callee := calleeName(n); callee == "printf" || callee == "puts" {
			if arg := firstValueWithVars(n, vars); arg != "" {
				*prints = append(*prints, arg)
			}
		}
	case "WhileStmt":
		if len(n.Inner) >= 2 {
			cond := &n.Inner[0]
			body := &n.Inner[1]
			if len(cond.Inner) == 2 {
				name := walkName(&cond.Inner[0])
				limit := valueWithVars(&cond.Inner[1], vars)
				if name != "" && limit != "" {
					start, err1 := strconv.Atoi(vars[name])
					end, err2 := strconv.Atoi(limit)
					if err1 == nil && err2 == nil {
						for start < end {
							vars[name] = strconv.Itoa(start)
							walkPrints(body, vars, prints)
							val, ok := vars[name]
							if !ok {
								break
							}
							iv, err := strconv.Atoi(val)
							if err != nil {
								break
							}
							start = iv
						}
						return
					}
				}
			}
		}
	}
	for i := range n.Inner {
		walkPrints(&n.Inner[i], vars, prints)
	}
}

func calleeName(n *Node) string {
	if len(n.Inner) == 0 {
		return ""
	}
	return walkName(&n.Inner[0])
}

func walkName(n *Node) string {
	if n.Kind == "DeclRefExpr" {
		if n.Name != "" {
			return n.Name
		}
		if n.Ref != nil && n.Ref.Name != "" {
			return n.Ref.Name
		}
	}
	if len(n.Inner) > 0 {
		return walkName(&n.Inner[0])
	}
	return ""
}

func firstValueWithVars(n *Node, vars map[string]string) string {
	start := 1
	if len(n.Inner) > 2 {
		start = 2
	}
	for i := start; i < len(n.Inner); i++ {
		if v := valueWithVars(&n.Inner[i], vars); v != "" {
			return v
		}
	}
	return ""
}

func valueWithVars(n *Node, vars map[string]string) string {
	switch n.Kind {
	case "IntegerLiteral", "FloatingLiteral":
		return n.Value
	case "StringLiteral":
		return strconv.Quote(strings.Trim(n.Value, "\""))
	case "DeclRefExpr":
		if n.Name != "" {
			if v, ok := vars[n.Name]; ok {
				return v
			}
		}
		if n.Ref != nil && n.Ref.Name != "" {
			if v, ok := vars[n.Ref.Name]; ok {
				return v
			}
		}
		return ""
	case "BinaryOperator":
		if len(n.Inner) == 2 {
			a := valueWithVars(&n.Inner[0], vars)
			b := valueWithVars(&n.Inner[1], vars)
			if a != "" && b != "" {
				ai, err1 := strconv.Atoi(a)
				bi, err2 := strconv.Atoi(b)
				if err1 == nil && err2 == nil {
					return strconv.Itoa(ai + bi)
				}
			}
		}
	}
	for i := range n.Inner {
		if v := valueWithVars(&n.Inner[i], vars); v != "" {
			return v
		}
	}
	return ""
}

func valueNode(v string) *ast.Node {
	if strings.HasPrefix(v, "\"") && strings.HasSuffix(v, "\"") {
		s, err := strconv.Unquote(v)
		if err == nil {
			return &ast.Node{Kind: "string", Value: s}
		}
	}
	if i, err := strconv.Atoi(v); err == nil {
		return &ast.Node{Kind: "int", Value: i}
	}
	if f, err := strconv.ParseFloat(v, 64); err == nil {
		return &ast.Node{Kind: "float", Value: f}
	}
	switch v {
	case "true", "false":
		return &ast.Node{Kind: "bool", Value: v == "true"}
	}
	return &ast.Node{Kind: "unknown", Value: v}
}

func parseArrayLoops(src string) []string {
	arrDecl := regexp.MustCompile(`(?m)(?:int|const char\*|char\s*\*)\s+(\w+)_arr\s*\[]\s*=\s*{([^}]*)}`)
	arrays := make(map[string][]string)
	for _, m := range arrDecl.FindAllStringSubmatch(src, -1) {
		name := m[1]
		elems := strings.Split(m[2], ",")
		vals := make([]string, 0, len(elems))
		for _, e := range elems {
			e = strings.TrimSpace(e)
			if e == "" {
				continue
			}
			vals = append(vals, e)
		}
		arrays[name] = vals
	}

	loopRe := regexp.MustCompile(`(?s)for\s*\(size_t i = 0; i < (\w+)_len; i\+\+\)\s*{([^}]*)}`)
	prints := make([]string, 0)
	for _, m := range loopRe.FindAllStringSubmatch(src, -1) {
		name := m[1]
		arr, ok := arrays[name]
		if !ok || len(arr) == 0 {
			continue
		}
		body := m[2]
		if strings.Contains(body, "printf") || strings.Contains(body, "puts") {
			prints = append(prints, arr...)
		}
	}
	return prints
}
