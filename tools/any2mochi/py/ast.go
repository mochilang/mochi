//go:build archived

package py

import (
	"bytes"
	"context"
	"encoding/json"
	"fmt"
	"os/exec"
	"strings"
	"time"
)

// ASTNode represents a single entry in the Python AST JSON format. It roughly
// mirrors the structure produced by the `ast` module when serialised to JSON
// within the helper script embedded below.
type ASTNode struct {
	Type          string          `json:"_type"`
	Name          string          `json:"name,omitempty"`
	ID            string          `json:"id,omitempty"`
	Arg           string          `json:"arg,omitempty"`
	Body          []*ASTNode      `json:"body,omitempty"`
	Targets       []*ASTNode      `json:"targets,omitempty"`
	Value         json.RawMessage `json:"value,omitempty"`
	Func          *ASTNode        `json:"func,omitempty"`
	Args          json.RawMessage `json:"args,omitempty"`
	Keywords      []*ASTNode      `json:"keywords,omitempty"`
	Annotation    *ASTNode        `json:"annotation,omitempty"`
	Returns       *ASTNode        `json:"returns,omitempty"`
	Test          *ASTNode        `json:"test,omitempty"`
	Target        *ASTNode        `json:"target,omitempty"`
	Iter          *ASTNode        `json:"iter,omitempty"`
	Operand       *ASTNode        `json:"operand,omitempty"`
	Orelse        []*ASTNode      `json:"orelse,omitempty"`
	Op            *ASTNode        `json:"op,omitempty"`
	Left          *ASTNode        `json:"left,omitempty"`
	Right         *ASTNode        `json:"right,omitempty"`
	Attr          string          `json:"attr,omitempty"`
	Elts          []*ASTNode      `json:"elts,omitempty"`
	Keys          []*ASTNode      `json:"keys,omitempty"`
	Values        []*ASTNode      `json:"values,omitempty"`
	Slice         *ASTNode        `json:"slice,omitempty"`
	Lower         *ASTNode        `json:"lower,omitempty"`
	Upper         *ASTNode        `json:"upper,omitempty"`
	Step          *ASTNode        `json:"step,omitempty"`
	Ops           []*ASTNode      `json:"ops,omitempty"`
	Comparators   []*ASTNode      `json:"comparators,omitempty"`
	DecoratorList []*ASTNode      `json:"decorator_list,omitempty"`
	Bases         []*ASTNode      `json:"bases,omitempty"`
	Generators    []*ASTNode      `json:"generators,omitempty"`
	Ifs           []*ASTNode      `json:"ifs,omitempty"`
	Elt           *ASTNode        `json:"elt,omitempty"`
	Line          int             `json:"lineno,omitempty"`
	EndLine       int             `json:"end_lineno,omitempty"`
	Col           int             `json:"col_offset,omitempty"`
	EndCol        int             `json:"end_col_offset,omitempty"`
}

func (n *ASTNode) valueNode() *ASTNode {
	if n == nil || len(n.Value) == 0 || n.Value[0] == '"' || n.Value[0] == '-' || (n.Value[0] >= '0' && n.Value[0] <= '9') {
		return nil
	}
	var out ASTNode
	if err := json.Unmarshal(n.Value, &out); err != nil {
		return nil
	}
	return &out
}

func (n *ASTNode) constValue() any {
	if n == nil || len(n.Value) == 0 {
		return nil
	}
	var v any
	_ = json.Unmarshal(n.Value, &v)
	return v
}

func (n *ASTNode) callArgs() []*ASTNode {
	if n == nil || len(n.Args) == 0 {
		return nil
	}
	var arr []*ASTNode
	if err := json.Unmarshal(n.Args, &arr); err == nil {
		return arr
	}
	var fn struct {
		Args []*ASTNode `json:"args"`
	}
	if err := json.Unmarshal(n.Args, &fn); err == nil {
		return fn.Args
	}
	return nil
}

const astScript = `import ast, json, sys

def node_to_dict(node):
    if isinstance(node, ast.AST):
        fields = {}
        for k, v in ast.iter_fields(node):
            fields[k] = node_to_dict(v)
        for attr in ("lineno", "end_lineno", "col_offset", "end_col_offset"):
            if hasattr(node, attr):
                fields[attr] = getattr(node, attr)
        return {'_type': node.__class__.__name__, **fields}
    elif isinstance(node, list):
        return [node_to_dict(x) for x in node]
    else:
        return node

tree = ast.parse(sys.stdin.read())
json.dump(node_to_dict(tree), sys.stdout)`

func parseAST(src string) (*ASTNode, error) {
	ctx, cancel := context.WithTimeout(context.Background(), 5*time.Second)
	defer cancel()
	cmd := exec.CommandContext(ctx, pythonCmd, "-c", astScript)
	cmd.Stdin = strings.NewReader(src)
	var out bytes.Buffer
	var errBuf bytes.Buffer
	cmd.Stdout = &out
	cmd.Stderr = &errBuf
	if err := cmd.Run(); err != nil {
		msg := strings.TrimSpace(errBuf.String())
		if msg != "" {
			if line := parseErrorLine(msg); line > 0 {
				msg += "\n" + snippetAround(src, line)
			}
			return nil, fmt.Errorf("%v: %s", err, msg)
		}
		return nil, err
	}
	var root ASTNode
	if err := json.Unmarshal(out.Bytes(), &root); err != nil {
		return nil, err
	}
	return &root, nil
}

// ConvertError represents a failure while converting a Python AST to Mochi.
// It includes the line number and surrounding source code for easier debugging.
type ConvertError struct {
	Line int
	Snip string
	Msg  string
}

func (e *ConvertError) Error() string {
	return fmt.Sprintf("line %d: %s\n%s", e.Line, e.Msg, e.Snip)
}

func newConvertError(line int, lines []string, msg string) error {
	start := line - 2
	if start < 0 {
		start = 0
	}
	end := line
	if end >= len(lines) {
		end = len(lines) - 1
	}
	var b strings.Builder
	for i := start; i <= end; i++ {
		prefix := "   "
		if i+1 == line {
			prefix = ">>>"
		}
		fmt.Fprintf(&b, "%s %d: %s\n", prefix, i+1, strings.TrimRight(lines[i], "\n"))
	}
	return &ConvertError{Line: line, Snip: strings.TrimRight(b.String(), "\n"), Msg: msg}
}

func emitAST(b *strings.Builder, n *ASTNode, indent string, lines []string, seen map[string]bool) error {
	if n == nil {
		return nil
	}
	switch n.Type {
	case "Module":
		for _, c := range n.Body {
			if err := emitAST(b, c, indent, lines, seen); err != nil {
				return err
			}
		}
	case "FunctionDef":
		if n.Name == "main" {
			for _, st := range n.Body {
				if err := emitAST(b, st, indent, lines, seen); err != nil {
					return err
				}
			}
			return nil
		}
		b.WriteString("fun ")
		b.WriteString(n.Name)
		b.WriteByte('(')
		args := n.callArgs()
		first := true
		for _, a := range args {
			if a.Arg == "self" {
				continue
			}
			if !first {
				b.WriteString(", ")
			}
			first = false
			b.WriteString(a.Arg)
			if a.Annotation != nil {
				b.WriteString(": ")
				if err := emitExpr(b, a.Annotation, lines); err != nil {
					return err
				}
			}
		}
		b.WriteByte(')')
		if n.Returns != nil {
			b.WriteString(": ")
			if err := emitExpr(b, n.Returns, lines); err != nil {
				return err
			}
		}
		b.WriteString(" {\n")
		for _, st := range n.Body {
			b.WriteString(indent)
			b.WriteString("  ")
			if err := emitAST(b, st, indent+"  ", lines, seen); err != nil {
				return err
			}
		}
		b.WriteString(indent)
		b.WriteString("}\n")
	case "ClassDef":
		b.WriteString("type ")
		b.WriteString(n.Name)
		b.WriteString(" {\n")
		for _, st := range n.Body {
			if st.Type == "AnnAssign" && st.Target != nil && st.Target.Type == "Name" {
				b.WriteString(indent)
				b.WriteString("  ")
				b.WriteString(st.Target.ID)
				if st.Annotation != nil {
					b.WriteString(": ")
					if err := emitExpr(b, st.Annotation, lines); err != nil {
						return err
					}
				}
				b.WriteByte('\n')
			}
		}
		b.WriteString(indent)
		b.WriteString("}\n")
	case "Return":
		b.WriteString("return")
		if v := n.valueNode(); v != nil {
			b.WriteByte(' ')
			if err := emitExpr(b, v, lines); err != nil {
				return err
			}
		}
		b.WriteByte('\n')
	case "Expr":
		if call := n.valueNode(); call != nil && call.Type == "Call" && call.Func != nil && call.Func.Type == "Name" && call.Func.ID == "main" {
			return nil
		}
		if err := emitExpr(b, n.valueNode(), lines); err != nil {
			return err
		}
		b.WriteString("\n")
	case "Assign":
		if len(n.Body) > 0 {
			return newConvertError(n.Line, lines, "complex assignment")
		}
		targets := n.callArgs()
		if len(targets) == 0 && len(n.Targets) > 0 {
			targets = n.Targets
		}
		if len(targets) == 0 {
			return newConvertError(n.Line, lines, "unsupported assignment")
		}
		if strings.TrimSpace(string(n.Value)) == "null" {
			return nil
		}
		if v := n.valueNode(); v != nil && v.Type == "Constant" && strings.TrimSpace(string(v.Value)) == "null" {
			return nil
		}
		nameNode := targets[0]
		if nameNode.Type == "Name" && seen != nil {
			if seen[nameNode.ID] {
				return nil
			}
			seen[nameNode.ID] = true
		}
		b.WriteString("let ")
		if err := emitExpr(b, nameNode, lines); err != nil {
			return err
		}
		b.WriteString(" = ")
		if err := emitExpr(b, n.valueNode(), lines); err != nil {
			return err
		}
		b.WriteString("\n")
	case "For":
		b.WriteString("for ")
		if err := emitExpr(b, n.Target, lines); err != nil {
			return err
		}
		b.WriteString(" in ")
		if err := emitExpr(b, n.Iter, lines); err != nil {
			return err
		}
		b.WriteString(" {\n")
		for _, st := range n.Body {
			b.WriteString(indent)
			b.WriteString("  ")
			if err := emitAST(b, st, indent+"  ", lines, seen); err != nil {
				return err
			}
		}
		b.WriteString(indent)
		b.WriteString("}\n")
	case "While":
		b.WriteString("while ")
		if err := emitExpr(b, n.Test, lines); err != nil {
			return err
		}
		b.WriteString(" {\n")
		for _, st := range n.Body {
			b.WriteString(indent)
			b.WriteString("  ")
			if err := emitAST(b, st, indent+"  ", lines, seen); err != nil {
				return err
			}
		}
		b.WriteString(indent)
		b.WriteString("}\n")
	case "If":
		if n.Test != nil && n.Test.Type == "Compare" && len(n.Test.Ops) == 1 && n.Test.Ops[0].Type == "Eq" && n.Test.Left != nil && n.Test.Left.Type == "Name" && n.Test.Left.ID == "__name__" {
			// skip module entrypoint check
			return nil
		}
		b.WriteString("if ")
		if err := emitExpr(b, n.Test, lines); err != nil {
			return err
		}
		b.WriteString(" {\n")
		for _, st := range n.Body {
			b.WriteString(indent)
			b.WriteString("  ")
			if err := emitAST(b, st, indent+"  ", lines, seen); err != nil {
				return err
			}
		}
		b.WriteString(indent)
		b.WriteString("}")
		if len(n.Orelse) > 0 {
			b.WriteString(" else {\n")
			for _, st := range n.Orelse {
				b.WriteString(indent)
				b.WriteString("  ")
				if err := emitAST(b, st, indent+"  ", lines, seen); err != nil {
					return err
				}
			}
			b.WriteString(indent)
			b.WriteString("}")
		}
		b.WriteByte('\n')
	case "Continue":
		b.WriteString("continue\n")
	case "Break":
		b.WriteString("break\n")
	case "AugAssign":
		if n.Target == nil || n.Op == nil {
			return newConvertError(n.Line, lines, "bad augmented assignment")
		}
		b.WriteString("let ")
		if err := emitExpr(b, n.Target, lines); err != nil {
			return err
		}
		b.WriteString(" = ")
		if err := emitExpr(b, n.Target, lines); err != nil {
			return err
		}
		b.WriteByte(' ')
		switch n.Op.Type {
		case "Add":
			b.WriteByte('+')
		case "Sub":
			b.WriteByte('-')
		case "Mult":
			b.WriteByte('*')
		case "Div":
			b.WriteByte('/')
		case "Mod":
			b.WriteByte('%')
		default:
			return newConvertError(n.Line, lines, "unhandled aug assign operator")
		}
		b.WriteByte(' ')
		if err := emitExpr(b, n.valueNode(), lines); err != nil {
			return err
		}
		b.WriteByte('\n')
	case "Global":
		// ignore
		return nil
	}
	return nil
}

func emitExpr(b *strings.Builder, n *ASTNode, lines []string) error {
	if n == nil {
		return nil
	}
	switch n.Type {
	case "Call":
		fn := n.Func
		if fn != nil && fn.Type == "Name" && fn.ID == "print" {
			b.WriteString("print(")
			args := n.callArgs()
			for i, a := range args {
				if i > 0 {
					b.WriteString(", ")
				}
				if err := emitExpr(b, a, lines); err != nil {
					return err
				}
			}
			b.WriteString(")")
			return nil
		}
		if fn != nil && fn.Type == "Name" {
			if len(n.Keywords) > 0 && len(n.callArgs()) == 0 && strings.Title(fn.ID) == fn.ID {
				b.WriteString(fn.ID)
				b.WriteString(" {")
				for i, kw := range n.Keywords {
					if i > 0 {
						b.WriteString(", ")
					}
					b.WriteString(kw.Arg)
					b.WriteString(": ")
					if err := emitExpr(b, kw.valueNode(), lines); err != nil {
						return err
					}
				}
				b.WriteString("}")
				return nil
			}
			b.WriteString(fn.ID)
		}
		b.WriteString("(")
		args := n.callArgs()
		for i, a := range args {
			if i > 0 {
				b.WriteString(", ")
			}
			if err := emitExpr(b, a, lines); err != nil {
				return err
			}
		}
		for _, kw := range n.Keywords {
			if len(args) > 0 {
				b.WriteString(", ")
			}
			if kw.Arg != "" {
				b.WriteString(kw.Arg)
				b.WriteString(": ")
			}
			if err := emitExpr(b, kw.valueNode(), lines); err != nil {
				return err
			}
			args = append(args, kw)
		}
		b.WriteString(")")
	case "Name":
		id := n.ID
		if id == "str" {
			id = "string"
		}
		b.WriteString(id)
	case "Constant":
		v := n.constValue()
		switch vv := v.(type) {
		case string:
			fmt.Fprintf(b, "%q", vv)
		default:
			fmt.Fprintf(b, "%v", vv)
		}
	case "BinOp":
		if err := emitExpr(b, n.Left, lines); err != nil {
			return err
		}
		b.WriteByte(' ')
		if n.Op != nil {
			switch n.Op.Type {
			case "Add":
				b.WriteByte('+')
			case "Sub":
				b.WriteByte('-')
			case "Mult":
				b.WriteByte('*')
			case "Div":
				b.WriteByte('/')
			case "Mod":
				b.WriteByte('%')
			default:
				return newConvertError(n.Line, lines, "unhandled operator")
			}
		}
		b.WriteByte(' ')
		if err := emitExpr(b, n.Right, lines); err != nil {
			return err
		}
	case "Compare":
		if err := emitExpr(b, n.Left, lines); err != nil {
			return err
		}
		if len(n.Ops) == 0 || len(n.Comparators) == 0 {
			return newConvertError(n.Line, lines, "bad compare")
		}
		b.WriteByte(' ')
		op := n.Ops[0]
		switch op.Type {
		case "Eq":
			b.WriteString("==")
		case "NotEq":
			b.WriteString("!=")
		case "Lt":
			b.WriteString("<")
		case "LtE":
			b.WriteString("<=")
		case "Gt":
			b.WriteString(">")
		case "GtE":
			b.WriteString(">=")
		default:
			return newConvertError(n.Line, lines, "unhandled compare operator")
		}
		b.WriteByte(' ')
		if err := emitExpr(b, n.Comparators[0], lines); err != nil {
			return err
		}
	case "Attribute":
		if err := emitExpr(b, n.valueNode(), lines); err != nil {
			return err
		}
		b.WriteByte('.')
		if n.Attr != "" {
			b.WriteString(n.Attr)
		} else {
			b.WriteString(n.Name)
		}
	case "List", "Tuple":
		open, close := '[', ']'
		if n.Type == "Tuple" {
			open, close = '(', ')'
		}
		b.WriteByte(byte(open))
		for i, e := range n.Elts {
			if i > 0 {
				b.WriteString(", ")
			}
			if err := emitExpr(b, e, lines); err != nil {
				return err
			}
		}
		b.WriteByte(byte(close))
	case "Dict":
		b.WriteByte('{')
		for i := range n.Keys {
			if i > 0 {
				b.WriteString(", ")
			}
			if err := emitExpr(b, n.Keys[i], lines); err != nil {
				return err
			}
			b.WriteString(": ")
			if err := emitExpr(b, n.Values[i], lines); err != nil {
				return err
			}
		}
		b.WriteByte('}')
	case "Lambda":
		snippet := extractSegment(lines, n.Line, n.Col, n.EndLine, n.EndCol)
		b.WriteString(snippet)
	case "ListComp":
		if len(n.Generators) == 1 {
			g := n.Generators[0]
			b.WriteString("from ")
			if err := emitExpr(b, g.Target, lines); err != nil {
				return err
			}
			b.WriteString(" in ")
			if err := emitExpr(b, g.Iter, lines); err != nil {
				return err
			}
			if len(g.Ifs) > 0 {
				b.WriteString(" where ")
				if err := emitExpr(b, g.Ifs[0], lines); err != nil {
					return err
				}
			}
			b.WriteString(" select ")
			if err := emitExpr(b, n.Elt, lines); err != nil {
				return err
			}
		} else {
			return newConvertError(n.Line, lines, "unsupported list comp")
		}
	case "Subscript":
		if err := emitExpr(b, n.valueNode(), lines); err != nil {
			return err
		}
		b.WriteByte('[')
		if n.Slice != nil {
			if n.Slice.Type == "Slice" {
				if n.Slice.Lower != nil {
					if err := emitExpr(b, n.Slice.Lower, lines); err != nil {
						return err
					}
				}
				b.WriteByte(':')
				if n.Slice.Upper != nil {
					if err := emitExpr(b, n.Slice.Upper, lines); err != nil {
						return err
					}
				}
				if n.Slice.Step != nil {
					b.WriteByte(':')
					if err := emitExpr(b, n.Slice.Step, lines); err != nil {
						return err
					}
				}
			} else {
				if err := emitExpr(b, n.Slice, lines); err != nil {
					return err
				}
			}
		}
		b.WriteByte(']')
	case "UnaryOp":
		if n.Op != nil {
			switch n.Op.Type {
			case "USub":
				b.WriteByte('-')
			case "UAdd":
				b.WriteByte('+')
			case "Not":
				b.WriteString("not ")
			default:
				return newConvertError(n.Line, lines, "unhandled unary operator")
			}
		}
		if err := emitExpr(b, n.Operand, lines); err != nil {
			return err
		}
	default:
		return newConvertError(n.Line, lines, "unhandled expression")
	}
	return nil
}

// ConvertAST converts Python code to Mochi using a JSON AST.
func ConvertAST(src string) ([]byte, error) {
	root, err := parseAST(src)
	if err != nil {
		return nil, err
	}
	lines := strings.Split(src, "\n")
	var b strings.Builder
	seen := map[string]bool{}
	if err := emitAST(&b, root, "", lines, seen); err != nil {
		return nil, err
	}
	out := strings.TrimSpace(b.String())
	if out == "" {
		return nil, fmt.Errorf("no output")
	}
	return []byte(out + "\n"), nil
}

func extractSegment(lines []string, startLine, startCol, endLine, endCol int) string {
	if startLine < 1 {
		startLine = 1
	}
	if endLine > len(lines) {
		endLine = len(lines)
	}
	startIdx := startLine - 1
	endIdx := endLine - 1
	var out strings.Builder
	for i := startIdx; i <= endIdx && i < len(lines); i++ {
		line := lines[i]
		if i == startIdx && startCol < len(line) {
			line = line[startCol:]
		}
		if i == endIdx && endCol <= len(line) {
			line = line[:endCol]
		}
		out.WriteString(line)
		if i != endIdx {
			out.WriteByte('\n')
		}
	}
	return out.String()
}
