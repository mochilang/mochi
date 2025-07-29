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

// ASTNode mirrors Python's ast.AST structure as produced by the helper script.
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

// Program represents a parsed Python source file.
type Program struct {
	Module *ASTNode `json:"module"`
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
json.dump(node_to_dict(tree), sys.stdout)
`

var pythonCmd = "python3"

// Inspect parses the given Python source code and returns a Program describing
// its AST.
func Inspect(src string) (*Program, error) {
	ctx, cancel := context.WithTimeout(context.Background(), 5*time.Second)
	defer cancel()
	cmd := exec.CommandContext(ctx, pythonCmd, "-c", astScript)
	cmd.Stdin = strings.NewReader(src)
	var out, errBuf bytes.Buffer
	cmd.Stdout = &out
	cmd.Stderr = &errBuf
	if err := cmd.Run(); err != nil {
		msg := strings.TrimSpace(errBuf.String())
		if msg != "" {
			return nil, fmt.Errorf("%v: %s", err, msg)
		}
		return nil, err
	}
	var root ASTNode
	if err := json.Unmarshal(out.Bytes(), &root); err != nil {
		return nil, err
	}
	return &Program{Module: &root}, nil
}
