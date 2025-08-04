package mochi

import (
	"fmt"
	"strings"
)

var builtinFns = map[string]string{
	"len":    "int",
	"int":    "int",
	"float":  "float",
	"string": "string",
	"bool":   "bool",
	"now":    "int",
	"str":    "string",
}

// Decorate performs a simple type inference on the program and makes all
// variable declarations explicit. It returns a new Program with type
// annotations inserted. If type checking fails an error is returned.
func Decorate(p *Program) (*Program, error) {
	if p == nil || p.File == nil {
		return nil, errNilProgram(nil)
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
	case "type":
		if len(n.Children) > 0 && n.Children[0].Kind == "field" {
			env[n.Text] = cloneNode(n)
			return nil
		}
		for _, c := range n.Children {
			if err := decorateNode(c, env); err != nil {
				return err
			}
		}
	case "fun":
		idx := 0
		for idx < len(n.Children) && n.Children[idx].Kind == "param" {
			idx++
		}
		var ret *Node
		if idx < len(n.Children) && n.Children[idx].Kind == "type" {
			ret = n.Children[idx]
			idx++
		} else {
			ret = &Node{Kind: "type", Text: "any"}
			n.Children = append(n.Children, nil)
			copy(n.Children[idx+1:], n.Children[idx:])
			n.Children[idx] = ret
			idx++
		}
		env[n.Text] = cloneNode(ret)
		local := map[string]*Node{}
		for k, v := range env {
			local[k] = cloneNode(v)
		}
		for i := 0; i < idx; i++ {
			if n.Children[i].Kind != "param" {
				continue
			}
			param := n.Children[i]
			var pt *Node
			if len(param.Children) > 0 {
				pt = param.Children[0]
			} else {
				pt = &Node{Kind: "type", Text: "any"}
				param.Children = append([]*Node{pt}, param.Children...)
			}
			local[param.Text] = cloneNode(pt)
		}
		for _, c := range n.Children[idx:] {
			if err := decorateNode(c, local); err != nil {
				return err
			}
		}
		return nil
	case "let", "var":
		if len(n.Children) == 0 {
			return errMissingExpr(n.Kind, n.Text, n)
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
					return errVarTypeMismatch(n.Text, want, got, n)
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
		if len(n.Children) == 0 {
			t, ok := env[n.Text]
			if !ok {
				return nil, errUndefinedVar(n.Text, n)
			}
			return cloneNode(t), nil
		}
		recv, err := inferType(n.Children[0], env)
		if err != nil {
			return nil, err
		}
		def, ok := env[recv.Text]
		if !ok || len(def.Children) == 0 || def.Children[0].Kind != "field" {
			return nil, errNotStruct(recv, n)
		}
		for _, f := range def.Children {
			if f.Kind == "field" && f.Text == n.Text {
				if len(f.Children) > 0 {
					return cloneNode(f.Children[0]), nil
				}
				return &Node{Kind: "type", Text: "any"}, nil
			}
		}
		return nil, errUnknownField(n.Text, def, n)
	case "call":
		var fname string
		if n.Text != "" {
			fname = n.Text
		} else if len(n.Children) > 0 && n.Children[0].Kind == "selector" {
			fname = n.Children[0].Text
		}
		if fname != "" {
			if t, ok := env[fname]; ok {
				return cloneNode(t), nil
			}
			if bt, ok := builtinFns[fname]; ok {
				return &Node{Kind: "type", Text: bt}, nil
			}
		}
		return &Node{Kind: "type", Text: "any"}, nil
	case "cast":
		if len(n.Children) < 2 || n.Children[1].Kind != "type" {
			return nil, errCastMissingType(n)
		}
		if _, err := inferType(n.Children[0], env); err != nil {
			return nil, err
		}
		return cloneNode(n.Children[1]), nil
	case "group":
		if len(n.Children) != 1 {
			return nil, errGroupOneChild(n)
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
				return nil, errListElemMismatch(n)
			}
		}
		return &Node{Kind: "type", Text: "list", Children: []*Node{elem}}, nil
	case "map":
		if len(n.Children) == 0 {
			return &Node{Kind: "type", Text: "map", Children: []*Node{{Kind: "type", Text: "any"}, {Kind: "type", Text: "any"}}}, nil
		}
		first := n.Children[0]
		if len(first.Children) < 2 {
			return nil, errInvalidMapEntry(first)
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
				return nil, errInvalidMapEntry(e)
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
				return nil, errMapEntryTypeMismatch(e)
			}
		}
		return &Node{Kind: "type", Text: "map", Children: []*Node{keyType, valType}}, nil
	case "index":
		if len(n.Children) != 2 {
			return &Node{Kind: "type", Text: "any"}, nil
		}
		base, err := inferType(n.Children[0], env)
		if err != nil {
			return nil, err
		}
		idx, err := inferType(n.Children[1], env)
		if err != nil {
			return nil, err
		}
		switch base.Text {
		case "list":
			if idx.Text != "int" {
				return nil, errListIndexNotInt(n)
			}
			if len(base.Children) > 0 {
				return cloneNode(base.Children[0]), nil
			}
			return &Node{Kind: "type", Text: "any"}, nil
		case "map":
			if len(base.Children) < 2 {
				return &Node{Kind: "type", Text: "any"}, nil
			}
			keyT := base.Children[0]
			valT := base.Children[1]
			if !typeEqual(keyT, idx) {
				return nil, errMapIndexTypeMismatch(keyT, idx, n)
			}
			return cloneNode(valT), nil
		default:
			return nil, errIndexNonIndexable(base, n)
		}
	case "binary":
		if len(n.Children) != 2 {
			return nil, errBinaryTwoOperands(n)
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
				return nil, errBoolOpNonBool(n)
			}
			return &Node{Kind: "type", Text: "bool"}, nil
		case "==", "!=", "<", ">", "<=", ">=":
			if !typeEqual(left, right) {
				return nil, errComparisonMismatch(n)
			}
			return &Node{Kind: "type", Text: "bool"}, nil
		case "+", "-", "*", "/", "%":
			if left.Text == "string" || right.Text == "string" {
				if n.Text == "+" && left.Text == "string" && right.Text == "string" {
					return &Node{Kind: "type", Text: "string"}, nil
				}
				return nil, errInvalidStringOp(n)
			}
			if left.Text == "float" || right.Text == "float" {
				if !isNumeric(left.Text) || !isNumeric(right.Text) {
					return nil, errNumericOpNonNumeric(n)
				}
				return &Node{Kind: "type", Text: "float"}, nil
			}
			if left.Text == "int" && right.Text == "int" {
				return &Node{Kind: "type", Text: "int"}, nil
			}
			return nil, errNumericOpNonNumeric(n)
		default:
			return nil, errUnknownBinaryOp(n.Text, n)
		}
	case "unary":
		if len(n.Children) != 1 {
			return nil, errUnaryOneOperand(n)
		}
		t, err := inferType(n.Children[0], env)
		if err != nil {
			return nil, err
		}
		switch n.Text {
		case "!":
			if t.Text != "bool" {
				return nil, errBangNonBool(n)
			}
			return &Node{Kind: "type", Text: "bool"}, nil
		case "+", "-":
			if !isNumeric(t.Text) {
				return nil, errNumericOpNonNumeric(n)
			}
			return t, nil
		default:
			return nil, errUnknownUnaryOp(n.Text, n)
		}
	case "struct":
		def, ok := env[n.Text]
		if ok && len(def.Children) > 0 && def.Children[0].Kind == "field" {
			for _, f := range n.Children {
				if len(f.Children) == 0 {
					continue
				}
				fieldDef := findField(def, f.Text)
				if fieldDef == nil {
					return nil, errUnknownField(f.Text, def, f)
				}
				want := &Node{Kind: "type", Text: "any"}
				if len(fieldDef.Children) > 0 {
					want = fieldDef.Children[0]
				}
				got, err := inferType(f.Children[0], env)
				if err != nil {
					return nil, err
				}
				if !typeEqual(want, got) {
					return nil, errVarTypeMismatch(f.Text, want, got, f)
				}
			}
		}
		return &Node{Kind: "type", Text: n.Text}, nil
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

func findField(def *Node, name string) *Node {
	if def == nil {
		return nil
	}
	for _, f := range def.Children {
		if f.Kind == "field" && f.Text == name {
			return f
		}
	}
	return nil
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
