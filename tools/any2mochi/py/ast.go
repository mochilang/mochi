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

type node struct {
	Type  string          `json:"_type"`
	Name  string          `json:"name,omitempty"`
	ID    string          `json:"id,omitempty"`
	Body  []*node         `json:"body,omitempty"`
	Value json.RawMessage `json:"value,omitempty"`
	Func  *node           `json:"func,omitempty"`
	Args  json.RawMessage `json:"args,omitempty"`
}

func (n *node) valueNode() *node {
	if n == nil || len(n.Value) == 0 || n.Value[0] == '"' || n.Value[0] == '-' || (n.Value[0] >= '0' && n.Value[0] <= '9') {
		return nil
	}
	var out node
	if err := json.Unmarshal(n.Value, &out); err != nil {
		return nil
	}
	return &out
}

func (n *node) constValue() any {
	if n == nil || len(n.Value) == 0 {
		return nil
	}
	var v any
	_ = json.Unmarshal(n.Value, &v)
	return v
}

func (n *node) callArgs() []*node {
	if n == nil || len(n.Args) == 0 {
		return nil
	}
	var arr []*node
	if err := json.Unmarshal(n.Args, &arr); err == nil {
		return arr
	}
	var fn struct {
		Args []*node `json:"args"`
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
        return {'_type': node.__class__.__name__, **fields}
    elif isinstance(node, list):
        return [node_to_dict(x) for x in node]
    else:
        return node

tree = ast.parse(sys.stdin.read())
json.dump(node_to_dict(tree), sys.stdout)`

func parseAST(src string) (*node, error) {
	ctx, cancel := context.WithTimeout(context.Background(), 5*time.Second)
	defer cancel()
	cmd := exec.CommandContext(ctx, pythonCmd, "-c", astScript)
	cmd.Stdin = strings.NewReader(src)
	var out bytes.Buffer
	cmd.Stdout = &out
	if err := cmd.Run(); err != nil {
		return nil, err
	}
	var root node
	if err := json.Unmarshal(out.Bytes(), &root); err != nil {
		return nil, err
	}
	return &root, nil
}

func emitAST(b *strings.Builder, n *node, indent string) {
	if n == nil {
		return
	}
	switch n.Type {
	case "Module":
		for _, c := range n.Body {
			emitAST(b, c, indent)
		}
	case "FunctionDef":
		b.WriteString("fun ")
		b.WriteString(n.Name)
		b.WriteString("() {\n")
		for _, st := range n.Body {
			b.WriteString(indent)
			b.WriteString("  ")
			emitAST(b, st, indent+"  ")
		}
		b.WriteString(indent)
		b.WriteString("}\n")
	case "Expr":
		emitExpr(b, n.valueNode())
		b.WriteString("\n")
	}
}

func emitExpr(b *strings.Builder, n *node) {
	if n == nil {
		return
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
				emitExpr(b, a)
			}
			b.WriteString(")")
			return
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
			emitExpr(b, a)
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
	}
}

// ConvertAST converts Python code to Mochi using a JSON AST.
func ConvertAST(src string) ([]byte, error) {
	root, err := parseAST(src)
	if err != nil {
		return nil, err
	}
	var b strings.Builder
	emitAST(&b, root, "")
	out := strings.TrimSpace(b.String())
	if out == "" {
		return nil, fmt.Errorf("no output")
	}
	return []byte(out + "\n"), nil
}
