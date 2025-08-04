package mochi

import (
	"fmt"
	"strings"
)

// Decorate performs a simple type inference on the program and makes all
// variable declarations explicit. It returns a new Program with type
// annotations inserted. If type checking fails an error is returned.
func Decorate(p *Program) (*Program, error) {
	if p == nil || p.File == nil {
		return nil, fmt.Errorf("nil program")
	}
	cp := cloneProgram(p)
	env := map[string]*Node{}
	if err := decorateNode(&cp.File.Node, env); err != nil {
		return nil, err
	}
	return cp, nil
}

func decorateNode(n *Node, env map[string]*Node) error {
	if n == nil {
		return nil
	}
	switch n.Kind {
	case "program", "block":
		for _, c := range n.Children {
			if err := decorateNode(c, env); err != nil {
				return err
			}
		}
	case "let", "var":
		if len(n.Children) == 0 {
			return fmt.Errorf("%s %s missing expression", n.Kind, n.Text)
		}
		if n.Children[0].Kind == "type" {
			// already typed, verify expression matches if present
			if len(n.Children) > 1 {
				want := n.Children[0]
				got, err := inferType(n.Children[1], env)
				if err != nil {
					return err
				}
				if !typeEqual(want, got) {
					return fmt.Errorf("type mismatch for %s: %s vs %s", n.Text, typeString(want), typeString(got))
				}
			}
			env[n.Text] = cloneNode(n.Children[0])
			return nil
		}
		t, err := inferType(n.Children[0], env)
		if err != nil {
			return err
		}
		n.Children = append([]*Node{t}, n.Children...)
		env[n.Text] = cloneNode(t)
	default:
		for _, c := range n.Children {
			if err := decorateNode(c, env); err != nil {
				return err
			}
		}
	}
	return nil
}

func inferType(n *Node, env map[string]*Node) (*Node, error) {
	if n == nil {
		return &Node{Kind: "type", Text: "any"}, nil
	}
	switch n.Kind {
	case "int":
		return &Node{Kind: "type", Text: "int"}, nil
	case "float":
		return &Node{Kind: "type", Text: "float"}, nil
	case "string":
		return &Node{Kind: "type", Text: "string"}, nil
	case "bool":
		return &Node{Kind: "type", Text: "bool"}, nil
	case "selector":
		t, ok := env[n.Text]
		if !ok {
			return nil, fmt.Errorf("undefined variable %s", n.Text)
		}
		return cloneNode(t), nil
	case "group":
		if len(n.Children) != 1 {
			return nil, fmt.Errorf("group expects one child")
		}
		return inferType(n.Children[0], env)
	case "list":
		if len(n.Children) == 0 {
			return &Node{Kind: "type", Text: "list"}, nil
		}
		elem, err := inferType(n.Children[0], env)
		if err != nil {
			return nil, err
		}
		for i := 1; i < len(n.Children); i++ {
			t, err := inferType(n.Children[i], env)
			if err != nil {
				return nil, err
			}
			if !typeEqual(elem, t) {
				return nil, fmt.Errorf("list element type mismatch")
			}
		}
		return &Node{Kind: "type", Text: "list", Children: []*Node{elem}}, nil
	case "map":
		if len(n.Children) == 0 {
			return &Node{Kind: "type", Text: "map", Children: []*Node{{Kind: "type", Text: "any"}, {Kind: "type", Text: "any"}}}, nil
		}
		first := n.Children[0]
		if len(first.Children) < 2 {
			return nil, fmt.Errorf("invalid map entry")
		}
		keyType, err := inferKeyType(first.Children[0], env)
		if err != nil {
			return nil, err
		}
		valType, err := inferType(first.Children[1], env)
		if err != nil {
			return nil, err
		}
		for _, e := range n.Children[1:] {
			if len(e.Children) < 2 {
				return nil, fmt.Errorf("invalid map entry")
			}
			k, err := inferKeyType(e.Children[0], env)
			if err != nil {
				return nil, err
			}
			v, err := inferType(e.Children[1], env)
			if err != nil {
				return nil, err
			}
			if !typeEqual(keyType, k) || !typeEqual(valType, v) {
				return nil, fmt.Errorf("map entry type mismatch")
			}
		}
		return &Node{Kind: "type", Text: "map", Children: []*Node{keyType, valType}}, nil
	case "binary":
		if len(n.Children) != 2 {
			return nil, fmt.Errorf("binary expects two operands")
		}
		left, err := inferType(n.Children[0], env)
		if err != nil {
			return nil, err
		}
		right, err := inferType(n.Children[1], env)
		if err != nil {
			return nil, err
		}
		switch n.Text {
		case "&&", "||":
			if left.Text != "bool" || right.Text != "bool" {
				return nil, fmt.Errorf("boolean operator on non-bool")
			}
			return &Node{Kind: "type", Text: "bool"}, nil
		case "==", "!=", "<", ">", "<=", ">=":
			if !typeEqual(left, right) {
				return nil, fmt.Errorf("comparison type mismatch")
			}
			return &Node{Kind: "type", Text: "bool"}, nil
		case "+", "-", "*", "/", "%":
			if left.Text == "string" || right.Text == "string" {
				if n.Text == "+" && left.Text == "string" && right.Text == "string" {
					return &Node{Kind: "type", Text: "string"}, nil
				}
				return nil, fmt.Errorf("invalid string operation")
			}
			if left.Text == "float" || right.Text == "float" {
				if !isNumeric(left.Text) || !isNumeric(right.Text) {
					return nil, fmt.Errorf("numeric op with non-numeric")
				}
				return &Node{Kind: "type", Text: "float"}, nil
			}
			if left.Text == "int" && right.Text == "int" {
				return &Node{Kind: "type", Text: "int"}, nil
			}
			return nil, fmt.Errorf("numeric op with non-numeric")
		default:
			return nil, fmt.Errorf("unknown binary operator %s", n.Text)
		}
	case "unary":
		if len(n.Children) != 1 {
			return nil, fmt.Errorf("unary expects one operand")
		}
		t, err := inferType(n.Children[0], env)
		if err != nil {
			return nil, err
		}
		switch n.Text {
		case "!":
			if t.Text != "bool" {
				return nil, fmt.Errorf("! on non-bool")
			}
			return &Node{Kind: "type", Text: "bool"}, nil
		case "+", "-":
			if !isNumeric(t.Text) {
				return nil, fmt.Errorf("numeric unary on non-numeric")
			}
			return t, nil
		default:
			return nil, fmt.Errorf("unknown unary operator %s", n.Text)
		}
	default:
		// Fallback to any
		return &Node{Kind: "type", Text: "any"}, nil
	}
}

func inferKeyType(n *Node, env map[string]*Node) (*Node, error) {
	switch n.Kind {
	case "string", "selector":
		return &Node{Kind: "type", Text: "string"}, nil
	default:
		return inferType(n, env)
	}
}

func isNumeric(t string) bool {
	return t == "int" || t == "float"
}

func typeEqual(a, b *Node) bool {
	if a == nil || b == nil {
		return a == nil && b == nil
	}
	if a.Kind != b.Kind || a.Text != b.Text || len(a.Children) != len(b.Children) {
		return false
	}
	for i := range a.Children {
		if !typeEqual(a.Children[i], b.Children[i]) {
			return false
		}
	}
	return true
}

func cloneNode(n *Node) *Node {
	if n == nil {
		return nil
	}
	c := &Node{Kind: n.Kind, Text: n.Text, Start: n.Start, StartCol: n.StartCol, End: n.End, EndCol: n.EndCol}
	if len(n.Children) > 0 {
		c.Children = make([]*Node, len(n.Children))
		for i, ch := range n.Children {
			c.Children[i] = cloneNode(ch)
		}
	}
	return c
}

func cloneProgram(p *Program) *Program {
	if p == nil || p.File == nil {
		return &Program{}
	}
	return &Program{File: &ProgramNode{Node: *cloneNode(&p.File.Node)}}
}

func typeString(n *Node) string {
	if n == nil {
		return "nil"
	}
	if n.Kind != "type" {
		return n.Text
	}
	if len(n.Children) == 0 {
		return n.Text
	}
	switch n.Text {
	case "list":
		return fmt.Sprintf("list<%s>", typeString(n.Children[0]))
	case "map":
		if len(n.Children) == 2 {
			return fmt.Sprintf("map<%s,%s>", typeString(n.Children[0]), typeString(n.Children[1]))
		}
	}
	var sb strings.Builder
	sb.WriteString(n.Text)
	sb.WriteString("<")
	for i, c := range n.Children {
		if i > 0 {
			sb.WriteString(",")
		}
		sb.WriteString(typeString(c))
	}
	sb.WriteString(">")
	return sb.String()
}
