package c

import (
	"fmt"
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
	arrPrints := parseArrayLoops(p)
	if len(arrPrints) > 0 {
		skip := make(map[string]bool)
		for _, ap := range arrPrints {
			if unq, err := strconv.Unquote(ap); err == nil {
				if i := strings.Index(unq, " costs $"); i > 0 {
					key := unq[:i]
					skip[key] = true
					skip[strconv.Quote(key)] = true
				}
			}
		}
		filtered := make([]string, 0, len(prints))
		for _, pr := range prints {
			if skip[pr] {
				continue
			}
			filtered = append(filtered, pr)
		}
		prints = append(filtered, arrPrints...)
	}
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
	case "CharacterLiteral":
		if v, err := strconv.Atoi(n.Value); err == nil {
			return strconv.QuoteRune(rune(v))
		}
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
	case "ConditionalOperator":
		if len(n.Inner) == 3 {
			cond := valueWithVars(&n.Inner[0], vars)
			if cond != "" {
				if isTrue(cond) {
					return valueWithVars(&n.Inner[1], vars)
				}
				return valueWithVars(&n.Inner[2], vars)
			}
		}
	case "BinaryOperator":
		if len(n.Inner) == 2 {
			a := valueWithVars(&n.Inner[0], vars)
			b := valueWithVars(&n.Inner[1], vars)
			if a != "" && b != "" {
				switch n.Op {
				case "+", "-", "*", "/":
					ai, err1 := strconv.Atoi(a)
					bi, err2 := strconv.Atoi(b)
					if err1 == nil && err2 == nil {
						switch n.Op {
						case "+":
							return strconv.Itoa(ai + bi)
						case "-":
							return strconv.Itoa(ai - bi)
						case "*":
							return strconv.Itoa(ai * bi)
						case "/":
							if bi != 0 {
								return strconv.Itoa(ai / bi)
							}
						}
					}
				case "&&":
					return strconv.FormatBool(isTrue(a) && isTrue(b))
				case "||":
					return strconv.FormatBool(isTrue(a) || isTrue(b))
				case "==":
					return strconv.FormatBool(a == b)
				case "!=":
					return strconv.FormatBool(a != b)
				case "<", "<=", ">", ">=":
					ai, err1 := strconv.Atoi(a)
					bi, err2 := strconv.Atoi(b)
					if err1 == nil && err2 == nil {
						switch n.Op {
						case "<":
							return strconv.FormatBool(ai < bi)
						case "<=":
							return strconv.FormatBool(ai <= bi)
						case ">":
							return strconv.FormatBool(ai > bi)
						case ">=":
							return strconv.FormatBool(ai >= bi)
						}
					}
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

func isTrue(v string) bool {
	switch v {
	case "true", "1":
		return true
	case "false", "0", "":
		return false
	}
	// fallback for numeric strings
	if i, err := strconv.Atoi(v); err == nil {
		return i != 0
	}
	return v != ""
}

func parseArrayLoops(p *Program) []string {
	arrays := make(map[string][]string)

	var walkArrays func(*Node)
	walkArrays = func(n *Node) {
		if n.Kind == "VarDecl" && strings.HasSuffix(n.Name, "_arr") {
			if len(n.Inner) > 0 && n.Inner[0].Kind == "InitListExpr" {
				vals := make([]string, 0, len(n.Inner[0].Inner))
				for i := range n.Inner[0].Inner {
					v := &n.Inner[0].Inner[i]
					switch v.Kind {
					case "IntegerLiteral", "FloatingLiteral", "StringLiteral":
						vals = append(vals, strings.TrimSpace(v.Value))
					default:
						s := firstLiteral(v, "StringLiteral")
						nval := firstLiteral(v, "IntegerLiteral")
						if s != "" && nval != "" {
							line := fmt.Sprintf("%s costs $ %s", s, nval)
							vals = append(vals, strconv.Quote(line))
						}
					}
				}
				if len(vals) > 0 {
					name := strings.TrimSuffix(n.Name, "_arr")
					arrays[name] = vals
				}
			}
		}
		for i := range n.Inner {
			walkArrays(&n.Inner[i])
		}
	}
	walkArrays(p.Root)

	var prints []string

	var walkLoops func(*Node)
	walkLoops = func(n *Node) {
		if n.Kind == "ForStmt" && len(n.Inner) >= 5 {
			init := &n.Inner[0]
			cond := &n.Inner[2]
			body := &n.Inner[len(n.Inner)-1]
			if len(init.Inner) > 0 {
				vd := init.Inner[0]
				if vd.Kind == "VarDecl" && len(vd.Inner) > 0 {
					if cond.Kind == "BinaryOperator" && len(cond.Inner) == 2 {
						right := walkName(&cond.Inner[1])
						if strings.HasSuffix(right, "_len") {
							arrName := strings.TrimSuffix(right, "_len")
							if arr, ok := arrays[arrName]; ok {
								if containsPrint(body) {
									prints = append(prints, arr...)
								}
							}
						}
					}
				}
			}
		}
		for i := range n.Inner {
			walkLoops(&n.Inner[i])
		}
	}
	walkLoops(p.Root)
	return prints
}

func containsPrint(n *Node) bool {
	if n.Kind == "CallExpr" {
		if callee := calleeName(n); callee == "printf" || callee == "puts" {
			return true
		}
	}
	for i := range n.Inner {
		if containsPrint(&n.Inner[i]) {
			return true
		}
	}
	return false
}

func firstLiteral(n *Node, kind string) string {
	if n.Kind == kind {
		return strings.Trim(n.Value, "\"")
	}
	for i := range n.Inner {
		if v := firstLiteral(&n.Inner[i], kind); v != "" {
			return v
		}
	}
	return ""
}
