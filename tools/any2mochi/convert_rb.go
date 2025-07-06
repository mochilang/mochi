package any2mochi

import (
	"bytes"
	"encoding/json"
	"fmt"
	"os"
	"os/exec"
	"strings"
)

// rbIndent returns a string of two-space indents for the given level.
func rbIndent(level int) string { return strings.Repeat("  ", level) }

type rbNode struct {
	Type     string
	Value    string
	Children []rbNode
}

func parseRbAST(src string) (*rbNode, error) {
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
	node := buildRbNode(data)
	return &node, nil
}

func buildRbNode(v interface{}) rbNode {
	arr, ok := v.([]interface{})
	if !ok || len(arr) == 0 {
		return rbNode{}
	}
	n := rbNode{Type: fmt.Sprint(arr[0])}
	for i := 1; i < len(arr); i++ {
		switch val := arr[i].(type) {
		case []interface{}:
			n.Children = append(n.Children, buildRbNode(val))
		case string:
			if n.Value == "" {
				n.Value = val
			}
		}
	}
	return n
}

func rbTokens(n rbNode) []string {
	if strings.HasPrefix(n.Type, "@") {
		if n.Value != "" {
			return []string{n.Value}
		}
		return nil
	}
	var out []string
	for _, c := range n.Children {
		out = append(out, rbTokens(c)...)
	}
	return out
}

func rbExprString(n rbNode) string {
	toks := rbTokens(n)
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

func rbParams(n rbNode) string {
	var names []string
	var walk func(rbNode)
	walk = func(n rbNode) {
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

func convertRbNode(n rbNode, level int, out *[]string) {
	idt := rbIndent(level)
	switch n.Type {
	case "program":
		for _, c := range n.Children {
			convertRbNode(c, level, out)
		}
	case "command", "method_add_arg":
		if len(n.Children) > 0 {
			f := n.Children[0]
			if (f.Type == "@ident" && f.Value == "puts") ||
				(f.Type == "fcall" && len(f.Children) > 0 && f.Children[0].Type == "@ident" && f.Children[0].Value == "puts") {
				arg := rbExprString(n.Children[len(n.Children)-1])
				*out = append(*out, idt+"print("+arg+")")
				return
			}
		}
		*out = append(*out, idt+rbExprString(n))
	case "assign":
		if len(n.Children) < 2 {
			return
		}
		name := rbExprString(n.Children[0])
		expr := rbExprString(n.Children[1])
		kw := "let "
		if level == 0 {
			kw = "var "
		}
		*out = append(*out, idt+kw+name+" = "+expr)
	case "while":
		if len(n.Children) < 2 {
			return
		}
		cond := rbExprString(n.Children[0])
		*out = append(*out, idt+"while "+cond+" {")
		convertRbNode(n.Children[1], level+1, out)
		*out = append(*out, idt+"}")
	case "for":
		if len(n.Children) < 3 {
			return
		}
		varName := rbExprString(n.Children[0])
		iter := rbExprString(n.Children[1])
		*out = append(*out, idt+"for "+varName+" in "+iter+" {")
		convertRbNode(n.Children[2], level+1, out)
		*out = append(*out, idt+"}")
	case "if":
		if len(n.Children) < 2 {
			return
		}
		cond := rbExprString(n.Children[0])
		*out = append(*out, idt+"if "+cond+" {")
		convertRbNode(n.Children[1], level+1, out)
		if len(n.Children) > 2 {
			e := n.Children[2]
			if e.Type == "else" {
				*out = append(*out, idt+"} else {")
				convertRbNode(e, level+1, out)
			}
		}
		*out = append(*out, idt+"}")
	case "else", "bodystmt":
		for _, c := range n.Children {
			convertRbNode(c, level, out)
		}
	case "def":
		if len(n.Children) < 3 {
			return
		}
		name := rbExprString(n.Children[0])
		params := rbParams(n.Children[1])
		*out = append(*out, idt+"fun "+name+"("+params+") {")
		convertRbNode(n.Children[2], level+1, out)
		*out = append(*out, idt+"}")
	case "class":
		if len(n.Children) < 3 {
			return
		}
		name := rbExprString(n.Children[0])
		*out = append(*out, idt+"type "+name+" {")
		convertRbNode(n.Children[2], level+1, out)
		*out = append(*out, idt+"}")
	default:
		for _, c := range n.Children {
			convertRbNode(c, level, out)
		}
	}
}

// ConvertRb converts Ruby source code to Mochi using a small Ruby AST parser.
func ConvertRb(src string) ([]byte, error) {
	if _, err := exec.LookPath("ruby"); err != nil {
		return nil, fmt.Errorf("ruby not installed: %w", err)
	}
	ast, err := parseRbAST(src)
	if err != nil {
		return nil, err
	}
	var lines []string
	convertRbNode(*ast, 0, &lines)
	if len(lines) == 0 {
		return nil, fmt.Errorf("no convertible statements")
	}
	return []byte(strings.Join(lines, "\n")), nil
}

// ConvertRbFile reads the Ruby file at path and converts it to Mochi.
func ConvertRbFile(path string) ([]byte, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	return ConvertRb(string(data))
}
