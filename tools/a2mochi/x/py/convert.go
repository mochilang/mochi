package py

import (
	"bytes"
	"context"
	"encoding/json"
	"fmt"
	"os/exec"
	"regexp"
	"strconv"
	"strings"
	"time"

	"mochi/ast"
	"mochi/parser"
)

// Node represents a Python AST node parsed from the builtin `ast` module.
type Node struct {
	Type          string          `json:"_type"`
	Name          string          `json:"name,omitempty"`
	ID            string          `json:"id,omitempty"`
	Arg           string          `json:"arg,omitempty"`
	Body          []*Node         `json:"body,omitempty"`
	Targets       []*Node         `json:"targets,omitempty"`
	Value         json.RawMessage `json:"value,omitempty"`
	Func          *Node           `json:"func,omitempty"`
	Args          json.RawMessage `json:"args,omitempty"`
	Keywords      []*Node         `json:"keywords,omitempty"`
	Annotation    *Node           `json:"annotation,omitempty"`
	Returns       *Node           `json:"returns,omitempty"`
	Test          *Node           `json:"test,omitempty"`
	Target        *Node           `json:"target,omitempty"`
	Iter          *Node           `json:"iter,omitempty"`
	Operand       *Node           `json:"operand,omitempty"`
	Orelse        []*Node         `json:"orelse,omitempty"`
	Op            *Node           `json:"op,omitempty"`
	Left          *Node           `json:"left,omitempty"`
	Right         *Node           `json:"right,omitempty"`
	Attr          string          `json:"attr,omitempty"`
	Elts          []*Node         `json:"elts,omitempty"`
	Keys          []*Node         `json:"keys,omitempty"`
	Values        []*Node         `json:"values,omitempty"`
	Slice         *Node           `json:"slice,omitempty"`
	Lower         *Node           `json:"lower,omitempty"`
	Upper         *Node           `json:"upper,omitempty"`
	Step          *Node           `json:"step,omitempty"`
	Ops           []*Node         `json:"ops,omitempty"`
	Comparators   []*Node         `json:"comparators,omitempty"`
	DecoratorList []*Node         `json:"decorator_list,omitempty"`
	Bases         []*Node         `json:"bases,omitempty"`
	Generators    []*Node         `json:"generators,omitempty"`
	Ifs           []*Node         `json:"ifs,omitempty"`
	Elt           *Node           `json:"elt,omitempty"`
	Line          int             `json:"lineno,omitempty"`
	EndLine       int             `json:"end_lineno,omitempty"`
	Col           int             `json:"col_offset,omitempty"`
	EndCol        int             `json:"end_col_offset,omitempty"`

	Lines []string `json:"-"` // original source lines, root node only
}

func (n *Node) UnmarshalJSON(data []byte) error {
	type raw Node
	var aux struct {
		Body   json.RawMessage `json:"body,omitempty"`
		Orelse json.RawMessage `json:"orelse,omitempty"`
		*raw
	}
	aux.raw = (*raw)(n)
	if err := json.Unmarshal(data, &aux); err != nil {
		return err
	}
	if len(aux.Body) > 0 {
		switch aux.Body[0] {
		case '[':
			if err := json.Unmarshal(aux.Body, &n.Body); err != nil {
				return err
			}
		case '{':
			var m Node
			if err := json.Unmarshal(aux.Body, &m); err != nil {
				return err
			}
			n.Body = []*Node{&m}
		}
	}
	if len(aux.Orelse) > 0 && n.Orelse == nil {
		switch aux.Orelse[0] {
		case '[':
			if err := json.Unmarshal(aux.Orelse, &n.Orelse); err != nil {
				return err
			}
		case '{':
			var m Node
			if err := json.Unmarshal(aux.Orelse, &m); err != nil {
				return err
			}
			n.Orelse = []*Node{&m}
		}
	}
	return nil
}

func (n *Node) valueNode() *Node {
	if n == nil || len(n.Value) == 0 || n.Value[0] == '"' || n.Value[0] == '-' || (n.Value[0] >= '0' && n.Value[0] <= '9') {
		return nil
	}
	var out Node
	if err := json.Unmarshal(n.Value, &out); err != nil {
		return nil
	}
	return &out
}

func (n *Node) constValue() any {
	if n == nil || len(n.Value) == 0 {
		return nil
	}
	var v any
	_ = json.Unmarshal(n.Value, &v)
	return v
}

func (n *Node) callArgs() []*Node {
	if n == nil || len(n.Args) == 0 {
		return nil
	}
	var raws []json.RawMessage
	if err := json.Unmarshal(n.Args, &raws); err == nil {
		out := make([]*Node, 0, len(raws))
		for _, r := range raws {
			var m Node
			if err := json.Unmarshal(r, &m); err == nil {
				out = append(out, &m)
			}
		}
		return out
	}
	var fn struct {
		Args []json.RawMessage `json:"args"`
	}
	if err := json.Unmarshal(n.Args, &fn); err == nil {
		out := make([]*Node, 0, len(fn.Args))
		for _, r := range fn.Args {
			var m Node
			if err := json.Unmarshal(r, &m); err == nil {
				out = append(out, &m)
			}
		}
		return out
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

func snippetAround(src string, line int) string {
	lines := strings.Split(src, "\n")
	start := line - 2
	if start < 0 {
		start = 0
	}
	end := line + 1
	if end > len(lines) {
		end = len(lines)
	}
	for i := start; i < end; i++ {
		lines[i] = fmt.Sprintf("%3d: %s", i+1, lines[i])
	}
	return strings.Join(lines[start:end], "\n")
}

func parseErrorLine(msg string) int {
	re := regexp.MustCompile(`line (\d+)`)
	matches := re.FindAllStringSubmatch(msg, -1)
	if len(matches) == 0 {
		return 0
	}
	m := matches[len(matches)-1]
	if len(m) == 2 {
		if n, err := strconv.Atoi(m[1]); err == nil {
			return n
		}
	}
	return 0
}

// Parse parses Python source into a Node using the builtin ast module.
func Parse(src string) (*Node, error) {
	ctx, cancel := context.WithTimeout(context.Background(), 5*time.Second)
	defer cancel()
	cmd := exec.CommandContext(ctx, "python3", "-c", astScript)
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
	var root Node
	if err := json.Unmarshal(out.Bytes(), &root); err != nil {
		return nil, err
	}
	root.Lines = strings.Split(src, "\n")
	return &root, nil
}

// Convert converts a parsed Python AST to a Mochi AST node.
func Convert(n *Node) (*ast.Node, error) {
	if n == nil {
		return nil, fmt.Errorf("nil node")
	}
	var b strings.Builder
	if err := emitAST(&b, n, "", n.Lines, map[string]bool{}); err != nil {
		return nil, err
	}
	code := strings.TrimSpace(b.String())
	if code == "" {
		return nil, fmt.Errorf("no output")
	}
	prog, err := parser.ParseString(code)
	if err != nil {
		return nil, err
	}
	return ast.FromProgram(prog), nil
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
	return fmt.Errorf("line %d: %s\n%s", line, msg, strings.TrimRight(b.String(), "\n"))
}

func emitAST(b *strings.Builder, n *Node, indent string, lines []string, seen map[string]bool) error {
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
	case "Import", "ImportFrom", "Global":
		return nil
	case "FunctionDef":
		return emitFuncDef(b, n, indent, lines, seen)
	case "ClassDef":
		return emitClassDef(b, n, indent, lines, seen)
	case "Return":
		return emitReturnStmt(b, n, lines)
	case "Expr":
		return emitExprStmt(b, n, lines, seen)
	case "Assign":
		return emitAssignStmt(b, n, lines, seen)
	case "AugAssign":
		return emitAugAssignStmt(b, n, lines, seen)
	case "For":
		return emitForStmt(b, n, indent, lines, seen)
	case "While":
		return emitWhileStmt(b, n, indent, lines, seen)
	case "If":
		return emitIfStmt(b, n, indent, lines, seen)
	case "Continue":
		b.WriteString("continue\n")
	case "Break":
		b.WriteString("break\n")
	default:
		return newConvertError(n.Line, lines, "unhandled statement")
	}
	return nil
}

func emitFuncDef(b *strings.Builder, n *Node, indent string, lines []string, seen map[string]bool) error {
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
	return nil
}

func emitClassDef(b *strings.Builder, n *Node, indent string, lines []string, seen map[string]bool) error {
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
	return nil
}

func emitReturnStmt(b *strings.Builder, n *Node, lines []string) error {
	b.WriteString("return")
	if v := n.valueNode(); v != nil {
		b.WriteByte(' ')
		if err := emitExpr(b, v, lines); err != nil {
			return err
		}
	}
	b.WriteByte('\n')
	return nil
}

func emitExprStmt(b *strings.Builder, n *Node, lines []string, seen map[string]bool) error {
	if call := n.valueNode(); call != nil && call.Type == "Call" && call.Func != nil && call.Func.Type == "Name" && call.Func.ID == "main" {
		return nil
	}
	if err := emitExpr(b, n.valueNode(), lines); err != nil {
		return err
	}
	b.WriteString("\n")
	return nil
}

func emitAssignStmt(b *strings.Builder, n *Node, lines []string, seen map[string]bool) error {
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
	declared := false
	if nameNode.Type == "Name" && seen != nil {
		declared = seen[nameNode.ID]
		if !declared {
			seen[nameNode.ID] = true
		}
	}
	if !declared {
		b.WriteString("var ")
	}
	if err := emitExpr(b, nameNode, lines); err != nil {
		return err
	}
	b.WriteString(" = ")
	if err := emitExpr(b, n.valueNode(), lines); err != nil {
		return err
	}
	b.WriteString("\n")
	return nil
}

func emitAugAssignStmt(b *strings.Builder, n *Node, lines []string, seen map[string]bool) error {
	if n.Target == nil || n.Op == nil {
		return newConvertError(n.Line, lines, "bad augmented assign")
	}
	declared := false
	if n.Target.Type == "Name" && seen != nil {
		declared = seen[n.Target.ID]
		if !declared {
			seen[n.Target.ID] = true
		}
	}
	if !declared {
		b.WriteString("var ")
	}
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
	return nil
}

func emitForStmt(b *strings.Builder, n *Node, indent string, lines []string, seen map[string]bool) error {
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
	return nil
}

func emitWhileStmt(b *strings.Builder, n *Node, indent string, lines []string, seen map[string]bool) error {
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
	return nil
}

func emitIfStmt(b *strings.Builder, n *Node, indent string, lines []string, seen map[string]bool) error {
	if n.Test != nil && n.Test.Type == "Compare" && len(n.Test.Ops) == 1 && n.Test.Ops[0].Type == "Eq" && n.Test.Left != nil && n.Test.Left.Type == "Name" && n.Test.Left.ID == "__name__" {
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
	return nil
}

func emitExpr(b *strings.Builder, n *Node, lines []string) error {
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
		parts := strings.SplitN(snippet, ":", 2)
		args := ""
		body := ""
		if len(parts) == 2 {
			args = strings.TrimSpace(strings.TrimPrefix(parts[0], "lambda"))
			body = strings.TrimSpace(parts[1])
		}
		b.WriteString("fun (")
		b.WriteString(args)
		b.WriteString(") => ")
		b.WriteString(body)
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
	case "IfExp":
		b.WriteString("if ")
		if err := emitExpr(b, n.Test, lines); err != nil {
			return err
		}
		b.WriteString(" then ")
		var thenExpr *Node
		if len(n.Body) > 0 {
			thenExpr = n.Body[0]
		}
		if err := emitExpr(b, thenExpr, lines); err != nil {
			return err
		}
		if len(n.Orelse) > 0 {
			b.WriteString(" else ")
			if err := emitExpr(b, n.Orelse[0], lines); err != nil {
				return err
			}
		}
	case "BoolOp":
		for i, v := range n.Values {
			if i > 0 {
				if n.Op != nil {
					switch n.Op.Type {
					case "And":
						b.WriteString(" && ")
					case "Or":
						b.WriteString(" || ")
					default:
						return newConvertError(n.Line, lines, "unhandled bool operator")
					}
				}
			}
			if err := emitExpr(b, v, lines); err != nil {
				return err
			}
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
		b.WriteByte('(')
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
		b.WriteByte(')')
	default:
		return newConvertError(n.Line, lines, "unhandled expression")
	}
	return nil
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
