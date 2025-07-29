//go:build slow

package py

import (
	"bytes"
	"context"
	"encoding/json"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
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
	idx := strings.LastIndex(msg, "line ")
	if idx == -1 {
		return 0
	}
	idx += len("line ")
	end := idx
	for end < len(msg) && msg[end] >= '0' && msg[end] <= '9' {
		end++
	}
	if n, err := strconv.Atoi(msg[idx:end]); err == nil {
		return n
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

func repoRoot() (string, error) {
	dir, err := os.Getwd()
	if err != nil {
		return "", err
	}
	for i := 0; i < 10; i++ {
		if _, err := os.Stat(filepath.Join(dir, "go.mod")); err == nil {
			return dir, nil
		}
		parent := filepath.Dir(dir)
		if parent == dir {
			break
		}
		dir = parent
	}
	return "", os.ErrNotExist
}

func metaHeader() string {
	root, err := repoRoot()
	if err != nil {
		return ""
	}
	ver, err := os.ReadFile(filepath.Join(root, "VERSION"))
	if err != nil {
		return ""
	}
	tz := time.FixedZone("GMT+7", 7*3600)
	now := time.Now().In(tz).Format("2006-01-02 15:04:05 MST")
	return fmt.Sprintf("// Mochi %s %s\n", strings.TrimSpace(string(ver)), now)
}

func blockComment(src string) string {
	if !strings.HasSuffix(src, "\n") {
		src += "\n"
	}
	return "/*\n" + src + "*/\n"
}

func ConvertSource(n *Node) (string, error) {
	if n == nil {
		return "", fmt.Errorf("nil node")
	}
	var b strings.Builder
	if err := emitAST(&b, n, "", n.Lines, map[string]bool{}, map[string][]string{}); err != nil {
		return "", err
	}
	code := strings.TrimSpace(b.String())
	if code == "" {
		return "", fmt.Errorf("no output")
	}
	var out strings.Builder
	out.WriteString(metaHeader())
	out.WriteString(blockComment(strings.Join(n.Lines, "\n")))
	out.WriteString(code)
	out.WriteByte('\n')
	return out.String(), nil
}

// Convert converts a parsed Python AST to a Mochi AST node.
func Convert(n *Node) (*ast.Node, error) {
	src, err := ConvertSource(n)
	if err != nil {
		return nil, err
	}
	prog, err := parser.ParseString(src)
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

func emitAST(b *strings.Builder, n *Node, indent string, lines []string, seen map[string]bool, structs map[string][]string) error {
	if n == nil {
		return nil
	}
	switch n.Type {
	case "Module":
		for _, c := range n.Body {
			if err := emitAST(b, c, indent, lines, seen, structs); err != nil {
				return err
			}
		}
	case "Import", "ImportFrom", "Global":
		return nil
	case "FunctionDef":
		return emitFuncDef(b, n, indent, lines, seen, structs)
	case "ClassDef":
		return emitClassDef(b, n, indent, lines, seen, structs)
	case "Return":
		return emitReturnStmt(b, n, lines, structs)
	case "Expr":
		return emitExprStmt(b, n, lines, seen, structs)
	case "Assign":
		return emitAssignStmt(b, n, lines, seen, structs)
	case "AugAssign":
		return emitAugAssignStmt(b, n, lines, seen, structs)
	case "For":
		return emitForStmt(b, n, indent, lines, seen, structs)
	case "While":
		return emitWhileStmt(b, n, indent, lines, seen, structs)
	case "If":
		return emitIfStmt(b, n, indent, lines, seen, structs)
	case "Continue":
		b.WriteString("continue\n")
	case "Break":
		b.WriteString("break\n")
	case "Assert":
		b.WriteString("expect ")
		if err := emitExpr(b, n.Test, lines, structs); err != nil {
			return err
		}
		b.WriteByte('\n')
	default:
		return newConvertError(n.Line, lines, "unhandled statement")
	}
	return nil
}

func emitFuncDef(b *strings.Builder, n *Node, indent string, lines []string, seen map[string]bool, structs map[string][]string) error {
	if n.Name == "main" {
		for _, st := range n.Body {
			if err := emitAST(b, st, indent, lines, seen, structs); err != nil {
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
			if err := emitExpr(b, a.Annotation, lines, structs); err != nil {
				return err
			}
		} else {
			b.WriteString(": int")
		}
	}
	b.WriteByte(')')
	b.WriteString(": ")
	if n.Returns != nil {
		if err := emitExpr(b, n.Returns, lines, structs); err != nil {
			return err
		}
	} else if rt := inferReturnType(n, lines); rt != "" {
		b.WriteString(rt)
	} else {
		b.WriteString("int")
	}
	b.WriteString(" {\n")
	for _, st := range n.Body {
		b.WriteString(indent)
		b.WriteString("  ")
		if err := emitAST(b, st, indent+"  ", lines, seen, structs); err != nil {
			return err
		}
	}
	b.WriteString(indent)
	b.WriteString("}\n")
	return nil
}

func emitClassDef(b *strings.Builder, n *Node, indent string, lines []string, seen map[string]bool, structs map[string][]string) error {
	b.WriteString("type ")
	b.WriteString(n.Name)
	b.WriteString(" {\n")
	var fields []string
	for _, st := range n.Body {
		if st.Type == "AnnAssign" && st.Target != nil && st.Target.Type == "Name" {
			b.WriteString(indent)
			b.WriteString("  ")
			b.WriteString(st.Target.ID)
			fields = append(fields, st.Target.ID)
			if st.Annotation != nil {
				b.WriteString(": ")
				if err := emitExpr(b, st.Annotation, lines, structs); err != nil {
					return err
				}
			}
			b.WriteByte('\n')
		}
	}
	b.WriteString(indent)
	b.WriteString("}\n")
	if structs != nil {
		structs[n.Name] = fields
	}
	return nil
}

func emitReturnStmt(b *strings.Builder, n *Node, lines []string, structs map[string][]string) error {
	b.WriteString("return")
	if v := n.valueNode(); v != nil {
		b.WriteByte(' ')
		if err := emitExpr(b, v, lines, structs); err != nil {
			return err
		}
	}
	b.WriteByte('\n')
	return nil
}

func emitExprStmt(b *strings.Builder, n *Node, lines []string, seen map[string]bool, structs map[string][]string) error {
	if call := n.valueNode(); call != nil && call.Type == "Call" && call.Func != nil && call.Func.Type == "Name" && call.Func.ID == "main" {
		return nil
	}
	if err := emitExpr(b, n.valueNode(), lines, structs); err != nil {
		return err
	}
	b.WriteString("\n")
	return nil
}

func emitAssignStmt(b *strings.Builder, n *Node, lines []string, seen map[string]bool, structs map[string][]string) error {
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
	if seen != nil {
		switch nameNode.Type {
		case "Name":
			declared = seen[nameNode.ID]
			if !declared {
				seen[nameNode.ID] = true
			}
		case "Subscript":
			v := nameNode.valueNode()
			for v != nil && v.Type == "Subscript" {
				v = v.valueNode()
			}
			if v != nil && v.Type == "Name" {
				declared = seen[v.ID]
			}
		}
	}
	if !declared {
		b.WriteString("var ")
	}
	if err := emitExpr(b, nameNode, lines, structs); err != nil {
		return err
	}
	if !declared {
		if t := listStructType(n.valueNode(), structs); t != "" {
			fmt.Fprintf(b, ": list<%s>", t)
		}
	}
	b.WriteString(" = ")
	if err := emitExpr(b, n.valueNode(), lines, structs); err != nil {
		return err
	}
	b.WriteString("\n")
	return nil
}

func emitAugAssignStmt(b *strings.Builder, n *Node, lines []string, seen map[string]bool, structs map[string][]string) error {
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
	if err := emitExpr(b, n.Target, lines, structs); err != nil {
		return err
	}
	b.WriteString(" = ")
	if err := emitExpr(b, n.Target, lines, structs); err != nil {
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
	case "FloorDiv":
		b.WriteByte('/')
	case "Mod":
		b.WriteByte('%')
	default:
		return newConvertError(n.Line, lines, "unhandled aug assign operator")
	}
	b.WriteByte(' ')
	if err := emitExpr(b, n.valueNode(), lines, structs); err != nil {
		return err
	}
	b.WriteByte('\n')
	return nil
}

func emitForStmt(b *strings.Builder, n *Node, indent string, lines []string, seen map[string]bool, structs map[string][]string) error {
	b.WriteString("for ")
	if err := emitExpr(b, n.Target, lines, structs); err != nil {
		return err
	}
	b.WriteString(" in ")
	if n.Iter != nil && n.Iter.Type == "Call" && n.Iter.Func != nil && n.Iter.Func.Type == "Name" && n.Iter.Func.ID == "range" {
		args := n.Iter.callArgs()
		switch len(args) {
		case 1:
			b.WriteString("0..")
			if err := emitExpr(b, args[0], lines, structs); err != nil {
				return err
			}
		case 2:
			if err := emitExpr(b, args[0], lines, structs); err != nil {
				return err
			}
			b.WriteString("..")
			if err := emitExpr(b, args[1], lines, structs); err != nil {
				return err
			}
		default:
			if err := emitExpr(b, n.Iter, lines, structs); err != nil {
				return err
			}
		}
	} else {
		if err := emitExpr(b, n.Iter, lines, structs); err != nil {
			return err
		}
	}
	b.WriteString(" {\n")
	for _, st := range n.Body {
		b.WriteString(indent)
		b.WriteString("  ")
		if err := emitAST(b, st, indent+"  ", lines, seen, structs); err != nil {
			return err
		}
	}
	b.WriteString(indent)
	b.WriteString("}\n")
	return nil
}

func emitWhileStmt(b *strings.Builder, n *Node, indent string, lines []string, seen map[string]bool, structs map[string][]string) error {
	b.WriteString("while ")
	if err := emitExpr(b, n.Test, lines, structs); err != nil {
		return err
	}
	b.WriteString(" {\n")
	for _, st := range n.Body {
		b.WriteString(indent)
		b.WriteString("  ")
		if err := emitAST(b, st, indent+"  ", lines, seen, structs); err != nil {
			return err
		}
	}
	b.WriteString(indent)
	b.WriteString("}\n")
	return nil
}

func emitListCompQuery(b *strings.Builder, n *Node, slice *Node, lines []string, structs map[string][]string) error {
	if len(n.Generators) == 0 {
		return newConvertError(n.Line, lines, "unsupported listcomp")
	}
	g := n.Generators[0]
	b.WriteString("from ")
	if err := emitExpr(b, g.Target, lines, structs); err != nil {
		return err
	}
	b.WriteString(" in ")
	iter := g.Iter
	var sortExpr *Node
	desc := false
	if iter != nil && iter.Type == "Call" && iter.Func != nil && iter.Func.Type == "Name" && iter.Func.ID == "sorted" {
		args := iter.callArgs()
		if len(args) > 0 {
			iter = args[0]
		}
		for _, kw := range iter.Keywords {
			_ = kw
		}
		for _, kw := range g.Iter.Keywords {
			if kw.Arg == "key" {
				if val := kw.valueNode(); val != nil && val.Type == "Lambda" && len(val.Body) > 0 {
					sortExpr = val.Body[0]
				} else {
					sortExpr = kw.valueNode()
				}
			} else if kw.Arg == "reverse" {
				if v, ok := kw.valueNode().constValue().(bool); ok && v {
					desc = true
				}
			}
		}
	}
	if err := emitExpr(b, iter, lines, structs); err != nil {
		return err
	}
	for _, g := range n.Generators[1:] {
		b.WriteString(" from ")
		if err := emitExpr(b, g.Target, lines, structs); err != nil {
			return err
		}
		b.WriteString(" in ")
		if err := emitExpr(b, g.Iter, lines, structs); err != nil {
			return err
		}
	}
	condCount := 0
	for _, g := range n.Generators {
		for _, c := range g.Ifs {
			if condCount == 0 {
				b.WriteString(" where ")
			} else {
				b.WriteString(" && ")
			}
			if err := emitExpr(b, c, lines, structs); err != nil {
				return err
			}
			condCount++
		}
	}
	if sortExpr != nil {
		b.WriteString(" sort by ")
		if desc {
			b.WriteByte('-')
		}
		if err := emitExpr(b, sortExpr, lines, structs); err != nil {
			return err
		}
	}
	if slice != nil {
		if slice.Lower != nil {
			b.WriteString(" skip ")
			if err := emitExpr(b, slice.Lower, lines, structs); err != nil {
				return err
			}
		}
		if slice.Upper != nil {
			b.WriteString(" take ")
			if slice.Lower != nil && slice.Upper.Type == "BinOp" && slice.Upper.Op != nil && slice.Upper.Op.Type == "Add" && nodesEqual(slice.Lower, slice.Upper.Left) {
				if err := emitExpr(b, slice.Upper.Right, lines, structs); err != nil {
					return err
				}
			} else {
				if err := emitExpr(b, slice.Upper, lines, structs); err != nil {
					return err
				}
			}
		}
	}
	b.WriteString(" select ")
	if err := emitExpr(b, n.Elt, lines, structs); err != nil {
		return err
	}
	return nil
}

func emitIfStmt(b *strings.Builder, n *Node, indent string, lines []string, seen map[string]bool, structs map[string][]string) error {
	if n.Test != nil && n.Test.Type == "Compare" && len(n.Test.Ops) == 1 && n.Test.Ops[0].Type == "Eq" && n.Test.Left != nil && n.Test.Left.Type == "Name" && n.Test.Left.ID == "__name__" {
		return nil
	}
	b.WriteString("if ")
	if err := emitExpr(b, n.Test, lines, structs); err != nil {
		return err
	}
	b.WriteString(" {\n")
	for _, st := range n.Body {
		b.WriteString(indent)
		b.WriteString("  ")
		if err := emitAST(b, st, indent+"  ", lines, seen, structs); err != nil {
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
			if err := emitAST(b, st, indent+"  ", lines, seen, structs); err != nil {
				return err
			}
		}
		b.WriteString(indent)
		b.WriteString("}")
	}
	b.WriteByte('\n')
	return nil
}

func emitExpr(b *strings.Builder, n *Node, lines []string, structs map[string][]string) error {
	if n == nil {
		return nil
	}
	switch n.Type {
	case "Call":
		fn := n.Func
		if fn != nil && fn.Type == "Name" && fn.ID == "len" {
			args := n.callArgs()
			if len(args) == 1 {
				if a, bnode, ok := listUnionAllArgs(args[0]); ok {
					b.WriteString("len(")
					if err := emitExpr(b, a, lines, structs); err != nil {
						return err
					}
					b.WriteString(" union all ")
					if err := emitExpr(b, bnode, lines, structs); err != nil {
						return err
					}
					b.WriteByte(')')
					return nil
				}
			}
		}
		if fn != nil && fn.Type == "Name" && fn.ID == "list" {
			args := n.callArgs()
			if len(args) == 1 {
				if op, left, right, ok := setBinOp(args[0]); ok {
					if err := emitExpr(b, left, lines, structs); err != nil {
						return err
					}
					b.WriteString(" ")
					b.WriteString(op)
					b.WriteString(" ")
					if err := emitExpr(b, right, lines, structs); err != nil {
						return err
					}
					return nil
				}
			}
		}
		if fn != nil && fn.Type == "Name" && fn.ID == "print" {
			b.WriteString("print(")
			args := n.callArgs()
			for i, a := range args {
				if i > 0 {
					b.WriteString(", ")
				}
				if err := emitExpr(b, a, lines, structs); err != nil {
					return err
				}
			}
			b.WriteString(")")
			return nil
		}
		if fn != nil && fn.Type == "Name" {
			if fn.ID == "list" {
				args := n.callArgs()
				if len(args) == 1 {
					inner := args[0]
					if inner.Type == "Call" && inner.Func != nil && inner.Func.Type == "Attribute" && inner.Func.Attr == "values" && len(inner.callArgs()) == 0 {
						b.WriteString("values(")
						if err := emitExpr(b, inner.Func.valueNode(), lines, structs); err != nil {
							return err
						}
						b.WriteString(")")
						return nil
					}
				}
			}
			if len(n.Keywords) > 0 && len(n.callArgs()) == 0 && strings.Title(fn.ID) == fn.ID {
				b.WriteString(fn.ID)
				b.WriteString(" {")
				for i, kw := range n.Keywords {
					if i > 0 {
						b.WriteString(", ")
					}
					b.WriteString(kw.Arg)
					b.WriteString(": ")
					if err := emitExpr(b, kw.valueNode(), lines, structs); err != nil {
						return err
					}
				}
				b.WriteString("}")
				return nil
			}
			if len(n.Keywords) == 0 && strings.Title(fn.ID) == fn.ID {
				if fields, ok := structs[fn.ID]; ok && len(fields) == len(n.callArgs()) {
					b.WriteString(fn.ID)
					b.WriteString("{")
					args := n.callArgs()
					for i, a := range args {
						if i > 0 {
							b.WriteString(", ")
						}
						b.WriteString(fields[i])
						b.WriteString(": ")
						if err := emitExpr(b, a, lines, structs); err != nil {
							return err
						}
					}
					b.WriteString("}")
					return nil
				}
			}
			b.WriteString(fn.ID)
		} else if fn != nil && fn.Type == "Attribute" {
			if fn.Attr == "values" && len(n.callArgs()) == 0 && len(n.Keywords) == 0 {
				b.WriteString("values(")
				if err := emitExpr(b, fn.valueNode(), lines, structs); err != nil {
					return err
				}
				b.WriteString(")")
				return nil
			}
			if err := emitExpr(b, fn.valueNode(), lines, structs); err != nil {
				return err
			}
			b.WriteByte('.')
			if fn.Attr != "" {
				b.WriteString(fn.Attr)
			} else {
				b.WriteString(fn.Name)
			}
		}
		b.WriteString("(")
		args := n.callArgs()
		for i, a := range args {
			if i > 0 {
				b.WriteString(", ")
			}
			if err := emitExpr(b, a, lines, structs); err != nil {
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
			if err := emitExpr(b, kw.valueNode(), lines, structs); err != nil {
				return err
			}
			args = append(args, kw)
		}
		b.WriteString(")")
	case "Name":
		switch n.ID {
		case "str":
			b.WriteString("string")
		default:
			b.WriteString(n.ID)
		}
	case "Constant":
		v := n.constValue()
		switch vv := v.(type) {
		case string:
			fmt.Fprintf(b, "%q", vv)
		case nil:
			b.WriteString("nil")
		default:
			fmt.Fprintf(b, "%v", vv)
		}
	case "BinOp":
		if n.Op != nil && n.Op.Type == "Add" && n.Left != nil && n.Right != nil &&
			n.Left.Type == "Name" && n.Right.Type == "List" && len(n.Right.Elts) == 1 {
			b.WriteString("append(")
			if err := emitExpr(b, n.Left, lines, structs); err != nil {
				return err
			}
			b.WriteString(", ")
			if err := emitExpr(b, n.Right.Elts[0], lines, structs); err != nil {
				return err
			}
			b.WriteByte(')')
			return nil
		}
		lp := binOpPrec(n.Left)
		rp := binOpPrec(n.Right)
		cp := binOpPrec(n)
		if lp > 0 && lp < cp {
			b.WriteByte('(')
			if err := emitExpr(b, n.Left, lines, structs); err != nil {
				return err
			}
			b.WriteByte(')')
		} else {
			if err := emitExpr(b, n.Left, lines, structs); err != nil {
				return err
			}
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
			case "FloorDiv":
				b.WriteByte('/')
			case "Mod":
				b.WriteByte('%')
			default:
				return newConvertError(n.Line, lines, "unhandled operator")
			}
		}
		b.WriteByte(' ')
		if rp > 0 && rp < cp {
			b.WriteByte('(')
			if err := emitExpr(b, n.Right, lines, structs); err != nil {
				return err
			}
			b.WriteByte(')')
		} else {
			if err := emitExpr(b, n.Right, lines, structs); err != nil {
				return err
			}
		}
	case "Compare":
		if lc, ok := existsListComp(n); ok {
			b.WriteString("exists(")
			if err := emitExpr(b, lc, lines, structs); err != nil {
				return err
			}
			b.WriteByte(')')
			return nil
		}
		if len(n.Ops) == 1 && n.Ops[0].Type == "NotIn" && len(n.Comparators) == 1 {
			b.WriteString("!(")
			if err := emitExpr(b, n.Left, lines, structs); err != nil {
				return err
			}
			b.WriteString(" in ")
			if err := emitExpr(b, n.Comparators[0], lines, structs); err != nil {
				return err
			}
			b.WriteByte(')')
			return nil
		}
		if err := emitExpr(b, n.Left, lines, structs); err != nil {
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
		case "In":
			b.WriteString("in")
		default:
			return newConvertError(n.Line, lines, "unhandled compare operator")
		}
		b.WriteByte(' ')
		if err := emitExpr(b, n.Comparators[0], lines, structs); err != nil {
			return err
		}
	case "Attribute":
		if err := emitExpr(b, n.valueNode(), lines, structs); err != nil {
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
			if err := emitExpr(b, e, lines, structs); err != nil {
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
			if err := emitExpr(b, n.Keys[i], lines, structs); err != nil {
				return err
			}
			b.WriteString(": ")
			if err := emitExpr(b, n.Values[i], lines, structs); err != nil {
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
		argParts := strings.Split(args, ",")
		for i, p := range argParts {
			p = strings.TrimSpace(p)
			if i > 0 {
				b.WriteString(", ")
			}
			if p != "" {
				b.WriteString(p)
				b.WriteString(": int")
			}
		}
		b.WriteString(") : int => ")
		b.WriteString(body)
	case "ListComp":
		return emitListCompQuery(b, n, nil, lines, structs)
	case "IfExp":
		if c := condBool(n); c != nil {
			return emitExpr(b, c, lines, structs)
		}
		if arg, ok := avgArg(n); ok {
			b.WriteString("avg(")
			if err := emitExpr(b, arg, lines, structs); err != nil {
				return err
			}
			b.WriteByte(')')
			return nil
		}
		b.WriteString("if ")
		if err := emitExpr(b, n.Test, lines, structs); err != nil {
			return err
		}
		b.WriteString(" then ")
		var thenExpr *Node
		if len(n.Body) > 0 {
			thenExpr = n.Body[0]
		}
		if err := emitExpr(b, thenExpr, lines, structs); err != nil {
			return err
		}
		if len(n.Orelse) > 0 {
			b.WriteString(" else ")
			if err := emitExpr(b, n.Orelse[0], lines, structs); err != nil {
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
			if err := emitExpr(b, v, lines, structs); err != nil {
				return err
			}
		}
	case "Subscript":
		if lc := n.valueNode(); lc != nil && lc.Type == "ListComp" && n.Slice != nil && n.Slice.Type == "Slice" {
			return emitListCompQuery(b, lc, n.Slice, lines, structs)
		}
		if err := emitExpr(b, n.valueNode(), lines, structs); err != nil {
			return err
		}
		b.WriteByte('[')
		if n.Slice != nil {
			if n.Slice.Type == "Slice" {
				if n.Slice.Lower != nil {
					if err := emitExpr(b, n.Slice.Lower, lines, structs); err != nil {
						return err
					}
				}
				b.WriteByte(':')
				if n.Slice.Upper != nil {
					if err := emitExpr(b, n.Slice.Upper, lines, structs); err != nil {
						return err
					}
				}
				if n.Slice.Step != nil {
					b.WriteByte(':')
					if err := emitExpr(b, n.Slice.Step, lines, structs); err != nil {
						return err
					}
				}
			} else {
				if err := emitExpr(b, n.Slice, lines, structs); err != nil {
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
				b.WriteByte('!')
				b.WriteByte('(')
				if err := emitExpr(b, n.Operand, lines, structs); err != nil {
					return err
				}
				b.WriteByte(')')
				b.WriteByte(')')
				return nil
			default:
				return newConvertError(n.Line, lines, "unhandled unary operator")
			}
		}
		if err := emitExpr(b, n.Operand, lines, structs); err != nil {
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

func avgArg(n *Node) (*Node, bool) {
	if n == nil || n.Type != "IfExp" || len(n.Body) == 0 || len(n.Orelse) == 0 {
		return nil, false
	}
	thenExpr := n.Body[0]
	elseExpr := n.Orelse[0]
	if thenExpr == nil || thenExpr.Type != "BinOp" || thenExpr.Op == nil || thenExpr.Op.Type != "Div" {
		return nil, false
	}
	left := thenExpr.Left
	right := thenExpr.Right
	if left == nil || right == nil {
		return nil, false
	}
	if left.Type != "Call" || right.Type != "Call" {
		return nil, false
	}
	if left.Func == nil || left.Func.Type != "Name" || left.Func.ID != "sum" {
		return nil, false
	}
	if right.Func == nil || right.Func.Type != "Name" || right.Func.ID != "len" {
		return nil, false
	}
	largs := left.callArgs()
	rargs := right.callArgs()
	if len(largs) != 1 || len(rargs) != 1 {
		return nil, false
	}
	if !nodesEqual(largs[0], rargs[0]) {
		return nil, false
	}
	if elseExpr.Type != "Constant" {
		return nil, false
	}
	val := elseExpr.constValue()
	switch v := val.(type) {
	case float64:
		if v != 0 {
			return nil, false
		}
	case int:
		if v != 0 {
			return nil, false
		}
	default:
		return nil, false
	}
	return largs[0], true
}

func nodesEqual(a, b *Node) bool {
	if a == nil || b == nil {
		return a == nil && b == nil
	}
	if a.Type != b.Type {
		return false
	}
	switch a.Type {
	case "Name":
		return a.ID == b.ID
	case "Constant":
		return string(a.Value) == string(b.Value)
	case "List":
		if len(a.Elts) != len(b.Elts) {
			return false
		}
		for i := range a.Elts {
			if !nodesEqual(a.Elts[i], b.Elts[i]) {
				return false
			}
		}
		return true
	default:
		return false
	}
}

func binOpPrec(n *Node) int {
	if n == nil || n.Type != "BinOp" || n.Op == nil {
		return 0
	}
	switch n.Op.Type {
	case "Add", "Sub":
		return 1
	case "Mult", "Div", "FloorDiv", "Mod":
		return 2
	default:
		return 0
	}
}

func condBool(n *Node) *Node {
	if n == nil || n.Type != "IfExp" || len(n.Body) == 0 || len(n.Orelse) == 0 {
		return nil
	}
	thenExpr := n.Body[0]
	elseExpr := n.Orelse[0]
	if thenExpr.Type == "Constant" && elseExpr.Type == "Constant" {
		t := thenExpr.constValue()
		e := elseExpr.constValue()
		if (t == 1 || t == 1.0) && (e == 0 || e == 0.0) {
			return n.Test
		}
	}
	return nil
}

func existsListComp(n *Node) (*Node, bool) {
	if n == nil || n.Type != "Compare" || len(n.Ops) != 1 || len(n.Comparators) != 1 {
		return nil, false
	}
	if n.Ops[0].Type != "Gt" {
		return nil, false
	}
	right := n.Comparators[0]
	if right == nil || right.Type != "Constant" {
		return nil, false
	}
	if v := right.constValue(); v != 0 && v != 0.0 {
		return nil, false
	}
	call := n.Left
	if call == nil || call.Type != "Call" || call.Func == nil || call.Func.Type != "Name" || call.Func.ID != "len" {
		return nil, false
	}
	args := call.callArgs()
	if len(args) != 1 || args[0].Type != "ListComp" {
		return nil, false
	}
	return args[0], true
}

func listUnionAllArgs(n *Node) (*Node, *Node, bool) {
	if n == nil || n.Type != "BinOp" || n.Op == nil || n.Op.Type != "Add" {
		return nil, nil, false
	}
	if n.Left.Type == "List" && n.Right.Type == "List" {
		return n.Left, n.Right, true
	}
	return nil, nil, false
}

func setBinOp(n *Node) (string, *Node, *Node, bool) {
	if n == nil || n.Type != "BinOp" || n.Op == nil {
		return "", nil, nil, false
	}
	l := n.Left
	r := n.Right
	if l == nil || r == nil {
		return "", nil, nil, false
	}
	if l.Type != "Call" || r.Type != "Call" || l.Func == nil || r.Func == nil {
		return "", nil, nil, false
	}
	if l.Func.Type != "Name" || r.Func.Type != "Name" || l.Func.ID != "set" || r.Func.ID != "set" {
		return "", nil, nil, false
	}
	largs := l.callArgs()
	rargs := r.callArgs()
	if len(largs) != 1 || len(rargs) != 1 {
		return "", nil, nil, false
	}
	var op string
	switch n.Op.Type {
	case "BitOr":
		op = "union"
	case "Sub":
		op = "except"
	case "BitAnd":
		op = "intersect"
	default:
		return "", nil, nil, false
	}
	return op, largs[0], rargs[0], true
}

func inferReturnType(n *Node, lines []string) string {
	if len(n.Body) > 0 {
		ret := n.Body[len(n.Body)-1]
		if ret.Type == "Return" {
			val := ret.valueNode()
			if val != nil && val.Type == "Lambda" {
				return lambdaType(val, lines)
			}
			if val != nil && val.Type == "Constant" {
				v := val.constValue()
				if v == true || v == false {
					return "bool"
				}
			}
		}
	}
	return ""
}

func lambdaType(n *Node, lines []string) string {
	snippet := extractSegment(lines, n.Line, n.Col, n.EndLine, n.EndCol)
	parts := strings.SplitN(snippet, ":", 2)
	argStr := ""
	if len(parts) == 2 {
		argStr = strings.TrimSpace(strings.TrimPrefix(parts[0], "lambda"))
	}
	args := []string{}
	for _, p := range strings.Split(argStr, ",") {
		p = strings.TrimSpace(p)
		if p != "" {
			args = append(args, "int")
		}
	}
	return fmt.Sprintf("fun(%s): int", strings.Join(args, ", "))
}

func listStructType(n *Node, structs map[string][]string) string {
	if n == nil || n.Type != "List" || len(n.Elts) == 0 {
		return ""
	}
	first := n.Elts[0]
	if first.Type != "Call" || first.Func == nil || first.Func.Type != "Name" {
		return ""
	}
	name := first.Func.ID
	if _, ok := structs[name]; !ok {
		return ""
	}
	for _, e := range n.Elts[1:] {
		if e.Type != "Call" || e.Func == nil || e.Func.Type != "Name" || e.Func.ID != name {
			return ""
		}
	}
	return name
}
