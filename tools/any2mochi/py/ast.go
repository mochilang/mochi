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

// Node represents a single entry in the Python AST JSON format.
// It roughly mirrors the structure produced by the `ast` module when
// serialised to JSON within the helper script embedded below.
type Node struct {
	Type    string          `json:"_type"`
	Name    string          `json:"name,omitempty"`
	ID      string          `json:"id,omitempty"`
	Body    []*Node         `json:"body,omitempty"`
	Value   json.RawMessage `json:"value,omitempty"`
	Func    *Node           `json:"func,omitempty"`
	Args    json.RawMessage `json:"args,omitempty"`
	Line    int             `json:"lineno,omitempty"`
	EndLine int             `json:"end_lineno,omitempty"`
	Col     int             `json:"col_offset,omitempty"`
	EndCol  int             `json:"end_col_offset,omitempty"`
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
	var arr []*Node
	if err := json.Unmarshal(n.Args, &arr); err == nil {
		return arr
	}
	var fn struct {
		Args []*Node `json:"args"`
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

func parseAST(src string) (*Node, error) {
	ctx, cancel := context.WithTimeout(context.Background(), 5*time.Second)
	defer cancel()
	cmd := exec.CommandContext(ctx, pythonCmd, "-c", astScript)
	cmd.Stdin = strings.NewReader(src)
	var out bytes.Buffer
	cmd.Stdout = &out
	if err := cmd.Run(); err != nil {
		return nil, err
	}
	var root Node
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
	return fmt.Sprintf("line %d: %s\n  %s", e.Line, e.Msg, e.Snip)
}

func newConvertError(line int, lines []string, msg string) error {
	ctx := ""
	if line-1 < len(lines) && line-1 >= 0 {
		ctx = strings.TrimSpace(lines[line-1])
	}
	return &ConvertError{Line: line, Snip: ctx, Msg: msg}
}

func emitAST(b *strings.Builder, n *Node, indent string, lines []string) error {
	if n == nil {
		return nil
	}
	switch n.Type {
	case "Module":
		for _, c := range n.Body {
			if err := emitAST(b, c, indent, lines); err != nil {
				return err
			}
		}
	case "FunctionDef":
		b.WriteString("fun ")
		b.WriteString(n.Name)
		b.WriteString("() {\n")
		for _, st := range n.Body {
			b.WriteString(indent)
			b.WriteString("  ")
			if err := emitAST(b, st, indent+"  ", lines); err != nil {
				return err
			}
		}
		b.WriteString(indent)
		b.WriteString("}\n")
	case "Expr":
		if err := emitExpr(b, n.valueNode(), lines); err != nil {
			return err
		}
		b.WriteString("\n")
	case "Assign":
		if len(n.Body) > 0 {
			return newConvertError(n.Line, lines, "complex assignment")
		}
		targets := n.callArgs()
		if len(targets) == 0 {
			return newConvertError(n.Line, lines, "unsupported assignment")
		}
		b.WriteString("let ")
		if err := emitExpr(b, targets[0], lines); err != nil {
			return err
		}
		b.WriteString(" = ")
		if err := emitExpr(b, n.valueNode(), lines); err != nil {
			return err
		}
		b.WriteString("\n")
	}
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
		b.WriteString(")")
	case "Name":
		b.WriteString(n.ID)
	case "Constant":
		v := n.constValue()
		switch vv := v.(type) {
		case string:
			fmt.Fprintf(b, "%q", vv)
		default:
			fmt.Fprintf(b, "%v", vv)
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
	if err := emitAST(&b, root, "", lines); err != nil {
		return nil, err
	}
	out := strings.TrimSpace(b.String())
	if out == "" {
		return nil, fmt.Errorf("no output")
	}
	return []byte(out + "\n"), nil
}
