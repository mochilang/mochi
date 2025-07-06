package ruby

import (
	"bytes"
	"encoding/json"
	"fmt"
	"os"
	"os/exec"
	"strings"
)

// rbIndent returns a string of two-space indents for the given level.
func indent(level int) string { return strings.Repeat("  ", level) }

type Node struct {
	Type     string
	Value    string
	Children []Node
}

func parseAST(src string) (*Node, error) {
	cmd := exec.Command("ruby", "-e", "require 'json';require 'ripper';puts JSON.generate(Ripper.sexp(ARGF.read))")
	cmd.Stdin = strings.NewReader(src)
	var buf bytes.Buffer
	cmd.Stdout = &buf
	if err := cmd.Run(); err != nil {
		return nil, err
	}
	var data interface{}
	if err := json.Unmarshal(buf.Bytes(), &data); err != nil {
		return nil, err
	}
	node := buildNode(data)
	return &node, nil
}

func buildNode(v interface{}) Node {
	arr, ok := v.([]interface{})
	if !ok || len(arr) == 0 {
		return Node{}
	}
	n := Node{Type: fmt.Sprint(arr[0])}
	for i := 1; i < len(arr); i++ {
		switch val := arr[i].(type) {
		case []interface{}:
			n.Children = append(n.Children, buildNode(val))
		case string:
			if n.Value == "" {
				n.Value = val
			}
		}
	}
	return n
}

func tokens(n Node) []string {
	if strings.HasPrefix(n.Type, "@") {
		if n.Value != "" {
			return []string{n.Value}
		}
		return nil
	}
	var out []string
	for _, c := range n.Children {
		out = append(out, tokens(c)...)
	}
	return out
}

func exprString(n Node) string {
	toks := tokens(n)
	if len(toks) == 0 {
		return ""
	}
	s := strings.Join(toks, " ")
	reps := []struct{ old, new string }{
		{" ( ", "("}, {" )", ")"}, {"( ", "("}, {" , ", ", "},
		{" [ ", "["}, {" ] ", "]"}, {" . ", "."},
	}
	for _, r := range reps {
		s = strings.ReplaceAll(s, r.old, r.new)
	}
	return s
}

func params(n Node) string {
	var names []string
	var walk func(Node)
	walk = func(n Node) {
		if n.Type == "@ident" {
			names = append(names, n.Value)
			return
		}
		for _, c := range n.Children {
			walk(c)
		}
	}
	walk(n)
	return strings.Join(names, ", ")
}

func convertNode(n Node, level int, out *[]string) {
	idt := indent(level)
	switch n.Type {
	case "program":
		for _, c := range n.Children {
			convertNode(c, level, out)
		}
	case "command", "method_add_arg":
		if len(n.Children) > 0 {
			f := n.Children[0]
			if (f.Type == "@ident" && f.Value == "puts") ||
				(f.Type == "fcall" && len(f.Children) > 0 && f.Children[0].Type == "@ident" && f.Children[0].Value == "puts") {
				arg := exprString(n.Children[len(n.Children)-1])
				*out = append(*out, idt+"print("+arg+")")
				return
			}
		}
		*out = append(*out, idt+exprString(n))
	case "assign":
		if len(n.Children) < 2 {
			return
		}
		name := exprString(n.Children[0])
		expr := exprString(n.Children[1])
		kw := "let "
		if level == 0 {
			kw = "var "
		}
		*out = append(*out, idt+kw+name+" = "+expr)
	case "while":
		if len(n.Children) < 2 {
			return
		}
		cond := exprString(n.Children[0])
		*out = append(*out, idt+"while "+cond+" {")
		convertNode(n.Children[1], level+1, out)
		*out = append(*out, idt+"}")
	case "for":
		if len(n.Children) < 3 {
			return
		}
		varName := exprString(n.Children[0])
		iter := exprString(n.Children[1])
		*out = append(*out, idt+"for "+varName+" in "+iter+" {")
		convertNode(n.Children[2], level+1, out)
		*out = append(*out, idt+"}")
	case "if":
		if len(n.Children) < 2 {
			return
		}
		cond := exprString(n.Children[0])
		*out = append(*out, idt+"if "+cond+" {")
		convertNode(n.Children[1], level+1, out)
		if len(n.Children) > 2 {
			e := n.Children[2]
			if e.Type == "else" {
				*out = append(*out, idt+"} else {")
				convertNode(e, level+1, out)
			}
		}
		*out = append(*out, idt+"}")
	case "else", "bodystmt":
		for _, c := range n.Children {
			convertNode(c, level, out)
		}
	case "def":
		if len(n.Children) < 3 {
			return
		}
		name := exprString(n.Children[0])
		params := params(n.Children[1])
		*out = append(*out, idt+"fun "+name+"("+params+") {")
		convertNode(n.Children[2], level+1, out)
		*out = append(*out, idt+"}")
	case "class":
		if len(n.Children) < 3 {
			return
		}
		name := exprString(n.Children[0])
		*out = append(*out, idt+"type "+name+" {")
		convertNode(n.Children[2], level+1, out)
		*out = append(*out, idt+"}")
	default:
		for _, c := range n.Children {
			convertNode(c, level, out)
		}
	}
}

// Convert parses Ruby source code and emits minimal Mochi.
func Convert(src string) ([]byte, error) {
	if _, err := exec.LookPath("ruby"); err != nil {
		return nil, fmt.Errorf("ruby not installed: %w", err)
	}
	ast, err := parseAST(src)
	if err != nil {
		return nil, err
	}
	var lines []string
	convertNode(*ast, 0, &lines)
	if len(lines) == 0 {
		return nil, fmt.Errorf("no convertible statements")
	}
	return []byte(strings.Join(lines, "\n")), nil
}

// ConvertFile reads the Ruby file at path and converts it to Mochi.
func ConvertFile(path string) ([]byte, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	return Convert(string(data))
}
