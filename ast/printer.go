package ast

import (
	"fmt"
	"io"
	"strconv"
	"strings"
)

// Fprint writes Mochi source code representation of n to w.
func Fprint(w io.Writer, n *Node) error {
	var b strings.Builder
	writeNode(&b, n, 0)
	_, err := io.WriteString(w, b.String())
	return err
}

// Source returns Mochi source code for the node.
func (n *Node) Source() string {
	var b strings.Builder
	writeNode(&b, n, 0)
	return b.String()
}

func writeNode(b *strings.Builder, n *Node, indent int) {
	switch n.Kind {
	case "program":
		if n.Value != nil {
			fmt.Fprintf(b, "package %s\n", n.Value)
		}
		for _, c := range n.Children {
			writeStmt(b, c, indent)
		}
	default:
		writeStmt(b, n, indent)
	}
}

func writeStmt(b *strings.Builder, n *Node, indent int) {
	ind := strings.Repeat("  ", indent)
	switch n.Kind {
	case "let", "var":
		fmt.Fprintf(b, "%s%s %s", ind, n.Kind, n.Value)
		i := 0
		if len(n.Children) > 0 && isTypeNode(n.Children[0]) {
			fmt.Fprintf(b, ": %s", typeString(n.Children[0]))
			i++
		}
		if i < len(n.Children) {
			expr := exprString(n.Children[i])
			if strings.Contains(expr, "\n") {
				parts := strings.Split(expr, "\n")
				fmt.Fprintf(b, " = %s\n", parts[0])
				prefix := fmt.Sprintf("%s%s %s = ", ind, n.Kind, n.Value)
				indent2 := strings.Repeat(" ", len(prefix))
				for _, p := range parts[1:] {
					if p == "" {
						continue
					}
					fmt.Fprintf(b, "%s%s\n", indent2, p)
				}
				return
			}
			fmt.Fprintf(b, " = %s", expr)
		}
		b.WriteString("\n")
	case "assign":
		if n.Value != nil {
			fmt.Fprintf(b, "%s%s = %s\n", ind, n.Value, exprString(n.Children[0]))
		} else if len(n.Children) == 2 {
			fmt.Fprintf(b, "%s%s = %s\n", ind, exprString(n.Children[0]), exprString(n.Children[1]))
		}
	case "fun":
		fmt.Fprintf(b, "%sfun %s(", ind, n.Value)
		idx := 0
		for idx < len(n.Children) && n.Children[idx].Kind == "param" {
			if idx > 0 {
				b.WriteString(", ")
			}
			b.WriteString(paramString(n.Children[idx]))
			idx++
		}
		b.WriteString(")")
		if idx < len(n.Children) && isTypeNode(n.Children[idx]) {
			fmt.Fprintf(b, ": %s", typeString(n.Children[idx]))
			idx++
		}
		b.WriteString(" {\n")
		for ; idx < len(n.Children); idx++ {
			writeStmt(b, n.Children[idx], indent+1)
		}
		fmt.Fprintf(b, "%s}\n", ind)
	case "export":
		if len(n.Children) > 0 {
			fmt.Fprintf(b, "%sexport ", ind)
			writeStmt(b, n.Children[0], 0)
		}
	case "return":
		fmt.Fprintf(b, "%sreturn", ind)
		if len(n.Children) > 0 {
			fmt.Fprintf(b, " %s", exprString(n.Children[0]))
		}
		b.WriteString("\n")
	case "break":
		fmt.Fprintf(b, "%sbreak\n", ind)
	case "continue":
		fmt.Fprintf(b, "%scontinue\n", ind)
	case "if":
		if len(n.Children) < 2 {
			return
		}
		fmt.Fprintf(b, "%sif %s {\n", ind, exprString(n.Children[0]))
		writeBlock(b, n.Children[1], indent+1)
		if len(n.Children) > 2 {
			fmt.Fprintf(b, "%s} else ", ind)
			c := n.Children[2]
			if c.Kind == "block" {
				b.WriteString("{\n")
				writeBlock(b, c, indent+1)
				fmt.Fprintf(b, "%s}\n", ind)
			} else {
				writeStmt(b, c, indent)
			}
		} else {
			fmt.Fprintf(b, "%s}\n", ind)
		}
	case "while":
		if len(n.Children) != 2 {
			return
		}
		fmt.Fprintf(b, "%swhile %s {\n", ind, exprString(n.Children[0]))
		writeBlock(b, n.Children[1], indent+1)
		fmt.Fprintf(b, "%s}\n", ind)
	case "for":
		if len(n.Children) < 2 {
			return
		}
		c0 := n.Children[0]
		if c0.Kind == "range" {
			if len(c0.Children) == 1 {
				fmt.Fprintf(b, "%sfor %s in %s {\n", ind, n.Value, exprString(c0.Children[0]))
			} else {
				fmt.Fprintf(b, "%sfor %s in %s..%s {\n", ind, n.Value, exprString(c0.Children[0]), exprString(c0.Children[1]))
			}
		} else {
			fmt.Fprintf(b, "%sfor %s in %s {\n", ind, n.Value, exprString(c0.Children[0]))
		}
		writeBlock(b, n.Children[1], indent+1)
		fmt.Fprintf(b, "%s}\n", ind)
	case "update":
		fmt.Fprintf(b, "%supdate %s\n", ind, n.Value)
		for _, c := range n.Children {
			switch c.Kind {
			case "set":
				fmt.Fprintf(b, "%sset %s\n", ind, exprString(c.Children[0]))
			case "where":
				fmt.Fprintf(b, "%swhere %s\n", ind, exprString(c.Children[0]))
			}
		}
	case "expect":
		if len(n.Children) > 0 {
			fmt.Fprintf(b, "%sexpect %s\n", ind, exprString(n.Children[0]))
		}
	case "test":
		fmt.Fprintf(b, "%stest %q {\n", ind, n.Value)
		for _, c := range n.Children {
			writeStmt(b, c, indent+1)
		}
		fmt.Fprintf(b, "%s}\n", ind)
	case "bench":
		fmt.Fprintf(b, "%sbench %q {\n", ind, n.Value)
		for _, c := range n.Children {
			writeStmt(b, c, indent+1)
		}
		fmt.Fprintf(b, "%s}\n", ind)
	case "type":
		fmt.Fprintf(b, "%stype %s", ind, n.Value)
		if len(n.Children) > 0 {
			b.WriteString(" {\n")
			for _, c := range n.Children {
				if c.Kind == "field" {
					typ := "any"
					if len(c.Children) > 0 {
						typ = typeString(c.Children[0])
					}
					fmt.Fprintf(b, "%s  %s: %s\n", ind, c.Value, typ)
				} else {
					writeStmt(b, c, indent+1)
				}
			}
			fmt.Fprintf(b, "%s}\n", ind)
		} else {
			b.WriteString("\n")
		}
	case "stream":
		fmt.Fprintf(b, "%sstream %s {\n", ind, n.Value)
		for _, f := range n.Children {
			fmt.Fprintf(b, "%s  %s\n", ind, f.Value)
		}
		fmt.Fprintf(b, "%s}\n", ind)
	case "agent":
		fmt.Fprintf(b, "%sagent %s {\n", ind, n.Value)
		for _, c := range n.Children {
			writeStmt(b, c, indent+1)
		}
		fmt.Fprintf(b, "%s}\n", ind)
	case "on":
		fmt.Fprintf(b, "%son %s {\n", ind, n.Value)
		writeBlock(b, n, indent+1)
		fmt.Fprintf(b, "%s}\n", ind)
	case "intent":
		fmt.Fprintf(b, "%sintent %s(", ind, n.Value)
		idx := 0
		for idx < len(n.Children) && n.Children[idx].Kind == "param" {
			if idx > 0 {
				b.WriteString(", ")
			}
			b.WriteString(paramString(n.Children[idx]))
			idx++
		}
		b.WriteString(")")
		if idx < len(n.Children) && isTypeNode(n.Children[idx]) {
			fmt.Fprintf(b, ": %s", typeString(n.Children[idx]))
			idx++
		}
		b.WriteString(" {\n")
		for ; idx < len(n.Children); idx++ {
			writeStmt(b, n.Children[idx], indent+1)
		}
		fmt.Fprintf(b, "%s}\n", ind)
	case "model":
		fmt.Fprintf(b, "%smodel %s {\n", ind, n.Value)
		for _, f := range n.Children {
			fmt.Fprintf(b, "%s  %s: %s\n", ind, f.Kind, exprString(f.Children[0]))
		}
		fmt.Fprintf(b, "%s}\n", ind)
	case "block":
		writeBlock(b, n, indent)
	default:
		// expression statement
		fmt.Fprintf(b, "%s%s\n", ind, exprString(n))
	}
}

func writeBlock(b *strings.Builder, n *Node, indent int) {
	for _, st := range n.Children {
		writeStmt(b, st, indent)
	}
}

func paramString(n *Node) string {
	s := n.Value.(string)
	if len(n.Children) > 0 {
		s += ": " + typeString(n.Children[0])
	}
	return s
}

func exprString(n *Node) string {
	switch n.Kind {
	case "int":
		return fmt.Sprintf("%v", n.Value)
	case "float":
		if v, ok := n.Value.(float64); ok {
			if v == float64(int64(v)) {
				return fmt.Sprintf("%.1f", v)
			}
			return strconv.FormatFloat(v, 'f', -1, 64)
		}
		return fmt.Sprintf("%v", n.Value)
	case "string":
		return fmt.Sprintf("%q", n.Value)
	case "bool":
		if v, ok := n.Value.(bool); ok {
			if v {
				return "true"
			}
			return "false"
		}
		return fmt.Sprintf("%v", n.Value)
	case "selector":
		if len(n.Children) == 0 {
			return fmt.Sprintf("%v", n.Value)
		}
		return exprString(n.Children[0]) + "." + n.Value.(string)
	case "call":
		start := 0
		var callee string
		if n.Value != nil {
			callee = fmt.Sprintf("%v", n.Value)
		} else {
			callee = exprString(n.Children[0])
			start = 1
		}
		args := make([]string, 0, len(n.Children)-start)
		for i := start; i < len(n.Children); i++ {
			args = append(args, exprString(n.Children[i]))
		}
		return fmt.Sprintf("%s(%s)", callee, strings.Join(args, ", "))
	case "unary":
		return fmt.Sprintf("%s%s", n.Value, exprString(n.Children[0]))
	case "binary":
		right := exprString(n.Children[1])
		if len(right) > 0 && (right[0] == '-' || right[0] == '+') {
			if !(strings.HasPrefix(right, "(") && strings.HasSuffix(right, ")")) {
				right = "(" + right + ")"
			}
		}
		return fmt.Sprintf("(%s %s %s)", exprString(n.Children[0]), n.Value, right)
	case "list":
		parts := make([]string, len(n.Children))
		for i, c := range n.Children {
			parts[i] = exprString(c)
		}
		return "[" + strings.Join(parts, ", ") + "]"
	case "map":
		parts := make([]string, len(n.Children))
		for i, c := range n.Children {
			parts[i] = fmt.Sprintf("%s: %s", exprString(c.Children[0]), exprString(c.Children[1]))
		}
		return "{" + strings.Join(parts, ", ") + "}"
	case "struct":
		var prefix string
		if n.Value != nil {
			prefix = fmt.Sprintf("%s ", n.Value)
		}
		parts := make([]string, len(n.Children))
		for i, c := range n.Children {
			parts[i] = fmt.Sprintf("%s: %s", c.Value, exprString(c.Children[0]))
		}
		return prefix + "{" + strings.Join(parts, ", ") + "}"
	case "index":
		tgt := exprString(n.Children[0])
		if len(n.Children) == 2 {
			return fmt.Sprintf("%s[%s]", tgt, exprString(n.Children[1]))
		}
		var start, end string
		for _, c := range n.Children[1:] {
			if c.Kind == "start" && len(c.Children) > 0 {
				start = exprString(c.Children[0])
			}
			if c.Kind == "end" && len(c.Children) > 0 {
				end = exprString(c.Children[0])
			}
		}
		return fmt.Sprintf("%s[%s:%s]", tgt, start, end)
	case "cast":
		return fmt.Sprintf("%s as %s", exprString(n.Children[0]), typeString(n.Children[1]))
	case "group":
		return fmt.Sprintf("(%s)", exprString(n.Children[0]))
	case "funexpr":
		idx := 0
		var sb strings.Builder
		sb.WriteString("fun(")
		for idx < len(n.Children) && n.Children[idx].Kind == "param" {
			if idx > 0 {
				sb.WriteString(", ")
			}
			sb.WriteString(paramString(n.Children[idx]))
			idx++
		}
		sb.WriteString(")")
		if idx < len(n.Children) && isTypeNode(n.Children[idx]) {
			sb.WriteString(": ")
			sb.WriteString(typeString(n.Children[idx]))
			idx++
		}
		if idx < len(n.Children) {
			if n.Children[idx].Kind == "block" {
				sb.WriteString(" {\n")
				var tmp strings.Builder
				writeBlock(&tmp, n.Children[idx], 1)
				sb.WriteString(tmp.String())
				sb.WriteString("}")
			} else {
				sb.WriteString(" => ")
				sb.WriteString(exprString(n.Children[idx]))
			}
		}
		return sb.String()
	case "if_expr":
		s := fmt.Sprintf("if %s then %s", exprString(n.Children[0]), exprString(n.Children[1]))
		if len(n.Children) > 2 {
			s += " else " + exprString(n.Children[2])
		}
		return s
	case "query":
		if len(n.Children) == 0 {
			return ""
		}
		var sb strings.Builder
		src := exprString(n.Children[0].Children[0])
		fmt.Fprintf(&sb, "from %s in %s", n.Value, src)
		indent := "  "
		for _, c := range n.Children[1:] {
			sb.WriteString("\n")
			sb.WriteString(indent)
			switch c.Kind {
			case "from":
				src := exprString(c.Children[0].Children[0])
				fmt.Fprintf(&sb, "from %s in %s", c.Value, src)
			case "join", "left_join", "right_join", "outer_join":
				src := exprString(c.Children[0].Children[0])
				kind := strings.ReplaceAll(c.Kind, "_", " ")
				fmt.Fprintf(&sb, "%s %s in %s", kind, c.Value, src)
				if len(c.Children) > 1 && c.Children[1].Kind == "on" {
					sb.WriteString(" on ")
					sb.WriteString(exprString(c.Children[1].Children[0]))
				}
			case "where", "sort", "skip", "take":
				fmt.Fprintf(&sb, "%s %s", c.Kind, exprString(c.Children[0]))
			case "group_by":
				fmt.Fprintf(&sb, "group by %s into %s", exprString(c.Children[0]), c.Children[1].Value)
			case "select":
				sb.WriteString("select ")
				sb.WriteString(exprString(c.Children[0]))
			}
		}
		return sb.String()
	case "match":
		if len(n.Children) == 0 {
			return "match {}"
		}
		var sb strings.Builder
		sb.WriteString("match ")
		sb.WriteString(exprString(n.Children[0]))
		sb.WriteString(" { ")
		for i := 1; i < len(n.Children); i++ {
			c := n.Children[i]
			if i > 1 {
				sb.WriteString(" ")
			}
			pat := "_"
			if len(c.Children) > 0 && c.Children[0].Kind != "_" {
				pat = exprString(c.Children[0])
			}
			sb.WriteString(pat + " => " + exprString(c.Children[len(c.Children)-1]))
			if i < len(n.Children)-1 {
				sb.WriteString(",")
			}
		}
		sb.WriteString(" }")
		return sb.String()
	case "generate_text":
		parts := make([]string, len(n.Children))
		for i, f := range n.Children {
			parts[i] = fmt.Sprintf("%s: %s", f.Kind, exprString(f.Children[0]))
		}
		return "generate text { " + strings.Join(parts, ", ") + " }"
	case "fetch":
		if len(n.Children) == 0 {
			return "fetch"
		}
		s := "fetch " + exprString(n.Children[0])
		if len(n.Children) > 1 {
			s += " with " + exprString(n.Children[1])
		}
		return s
	case "load":
		var s string
		if len(n.Children) > 0 {
			s = fmt.Sprintf("load %s", exprString(n.Children[0]))
		}
		if len(n.Children) > 1 && isTypeNode(n.Children[1]) {
			s += " as " + typeString(n.Children[1])
		}
		if len(n.Children) > 2 {
			s += " with " + exprString(n.Children[2])
		}
		return s
	case "save":
		if len(n.Children) == 0 {
			return "save"
		}
		s := fmt.Sprintf("save %s", exprString(n.Children[0]))
		if len(n.Children) > 1 {
			s += " to " + exprString(n.Children[1])
		}
		if len(n.Children) > 2 {
			s += " with " + exprString(n.Children[2])
		}
		return s
	default:
		return n.Kind
	}
}

func typeString(n *Node) string {
	switch n.Kind {
	case "typefun":
		parts := make([]string, len(n.Children))
		for i, c := range n.Children {
			parts[i] = typeString(c)
		}
		if len(parts) > 0 {
			last := parts[len(parts)-1]
			parts = parts[:len(parts)-1]
			return "fun(" + strings.Join(parts, ", ") + ")" + ":" + last
		}
		return "fun()"
	case "type":
		if len(n.Children) > 0 && n.Children[0].Kind == "struct" {
			return structString(n.Children[0])
		}
		if len(n.Children) > 0 {
			parts := make([]string, len(n.Children))
			for i, c := range n.Children {
				parts[i] = typeString(c)
			}
			return fmt.Sprintf("%s<%s>", n.Value, strings.Join(parts, ", "))
		}
		return fmt.Sprintf("%v", n.Value)
	case "struct":
		return structString(n)
	default:
		return n.Kind
	}
}

func structString(n *Node) string {
	parts := make([]string, len(n.Children))
	for i, c := range n.Children {
		parts[i] = fmt.Sprintf("%s: %s", c.Value, typeString(c.Children[0]))
	}
	return "{" + strings.Join(parts, ", ") + "}"
}

func isTypeNode(n *Node) bool {
	return n.Kind == "type" || n.Kind == "typefun" || (n.Kind == "struct" && len(n.Children) > 0 && isTypeNode(n.Children[0]))
}
