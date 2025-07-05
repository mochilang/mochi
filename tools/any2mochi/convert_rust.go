package any2mochi

import (
	"bytes"
	"context"
	"fmt"
	"os"
	"os/exec"
	"regexp"
	"strconv"
	"strings"
	"time"
)

// node represents a parsed rust-analyzer syntax tree node.
type node struct {
	kind     string
	start    int
	end      int
	children []*node
}

var lineRe = regexp.MustCompile(`^( *)([A-Z_]+)@(\d+)..(\d+)(?:\s+"(.*)")?$`)

// parseTree parses the rust-analyzer syntax tree output into a hierarchy of nodes.
func parseTree(out string) *node {
	root := &node{kind: "ROOT"}
	stack := []*node{root}
	for _, line := range strings.Split(strings.TrimSpace(out), "\n") {
		if strings.TrimSpace(line) == "" {
			continue
		}
		m := lineRe.FindStringSubmatch(line)
		if m == nil {
			continue
		}
		indent := len(m[1]) / 2
		kind := m[2]
		start, _ := strconv.Atoi(m[3])
		end, _ := strconv.Atoi(m[4])
		n := &node{kind: kind, start: start, end: end}
		for len(stack) > indent+1 {
			stack = stack[:len(stack)-1]
		}
		parent := stack[len(stack)-1]
		parent.children = append(parent.children, n)
		stack = append(stack, n)
	}
	if len(root.children) > 0 {
		return root.children[0]
	}
	return root
}

func findChild(n *node, kind string) *node {
	for _, c := range n.children {
		if c.kind == kind {
			return c
		}
	}
	return nil
}

func indent(level int) string { return strings.Repeat("  ", level) }

func convertParams(src string, n *node) []string {
	var params []string
	if n == nil {
		return params
	}
	for _, c := range n.children {
		if c.kind == "PARAM" {
			id := findChild(findChild(findChild(c, "IDENT_PAT"), "NAME"), "IDENT")
			if id != nil {
				params = append(params, strings.TrimSpace(src[id.start:id.end]))
			}
		}
	}
	return params
}

func convertStmt(src string, n *node, level int) []string {
	idt := indent(level)
	switch n.kind {
	case "LET_STMT":
		text := strings.TrimSpace(src[n.start:n.end])
		text = strings.TrimSuffix(text, ";")
		text = strings.Replace(text, "let ", "var ", 1)
		text = strings.Replace(text, "mut ", "", 1)
		return []string{idt + text}
	case "EXPR_STMT":
		if len(n.children) == 0 {
			return nil
		}
		c := n.children[0]
		switch c.kind {
		case "MACRO_EXPR":
			code := strings.TrimSpace(src[c.start:c.end])
			if strings.HasPrefix(code, "println!") {
				args := strings.TrimPrefix(code, "println!")
				args = strings.TrimSpace(strings.TrimSuffix(strings.TrimPrefix(args, "("), ")"))
				parts := splitArgs(args)
				if len(parts) > 0 && strings.HasPrefix(strings.TrimSpace(parts[0]), "\"") {
					if len(parts) == 1 {
						return []string{idt + fmt.Sprintf("print(%s)", parts[0])}
					}
					parts = parts[1:]
				}
				return []string{idt + fmt.Sprintf("print(%s)", strings.Join(parts, ", "))}
			}
			code = strings.TrimSuffix(code, ";")
			return []string{idt + code}
		case "FOR_EXPR":
			return convertFor(src, c, level)
		case "WHILE_EXPR":
			return convertWhile(src, c, level)
		case "IF_EXPR":
			return convertIf(src, c, level)
		case "RETURN_EXPR":
			code := strings.TrimSuffix(strings.TrimSpace(src[c.start:c.end]), ";")
			return []string{idt + "return " + strings.TrimPrefix(code, "return ")}
		default:
			code := strings.TrimSuffix(strings.TrimSpace(src[c.start:c.end]), ";")
			return []string{idt + code}
		}
	case "FOR_EXPR":
		return convertFor(src, n, level)
	case "WHILE_EXPR":
		return convertWhile(src, n, level)
	case "IF_EXPR":
		return convertIf(src, n, level)
	case "RETURN_EXPR":
		code := strings.TrimSuffix(strings.TrimSpace(src[n.start:n.end]), ";")
		return []string{idt + code}
	}
	return nil
}

func convertFor(src string, n *node, level int) []string {
	block := findChild(n, "BLOCK_EXPR")
	if block == nil {
		return nil
	}
	header := strings.TrimSpace(src[n.start:block.start])
	lines := convertStmts(src, findChild(block, "STMT_LIST"), level+1)
	out := []string{indent(level) + header + " {"}
	out = append(out, lines...)
	out = append(out, indent(level)+"}")
	return out
}

func convertWhile(src string, n *node, level int) []string {
	block := findChild(n, "BLOCK_EXPR")
	if block == nil {
		return nil
	}
	header := strings.TrimSpace(src[n.start:block.start])
	lines := convertStmts(src, findChild(block, "STMT_LIST"), level+1)
	out := []string{indent(level) + header + " {"}
	out = append(out, lines...)
	out = append(out, indent(level)+"}")
	return out
}

func convertIf(src string, n *node, level int) []string {
	var out []string
	var blocks []*node
	for _, c := range n.children {
		if c.kind == "BLOCK_EXPR" {
			blocks = append(blocks, c)
		}
	}
	if len(blocks) == 0 {
		return nil
	}
	cond := strings.TrimSpace(src[n.children[2].start:blocks[0].start])
	out = append(out, indent(level)+"if "+cond+" {")
	out = append(out, convertStmts(src, findChild(blocks[0], "STMT_LIST"), level+1)...)
	out = append(out, indent(level)+"}")
	if len(blocks) > 1 {
		out[len(out)-1] = indent(level) + "else {"
		out = append(out, convertStmts(src, findChild(blocks[1], "STMT_LIST"), level+1)...)
		out = append(out, indent(level)+"}")
	}
	return out
}

func convertStmts(src string, list *node, level int) []string {
	var out []string
	if list == nil {
		return out
	}
	for _, c := range list.children {
		switch c.kind {
		case "LET_STMT", "EXPR_STMT", "RETURN_EXPR", "FOR_EXPR", "WHILE_EXPR", "IF_EXPR":
			out = append(out, convertStmt(src, c, level)...)
		}
	}
	return out
}

func splitArgs(s string) []string {
	var args []string
	depth := 0
	start := 0
	for i, r := range s {
		switch r {
		case '(':
			depth++
		case ')':
			depth--
		case ',':
			if depth == 0 {
				args = append(args, strings.TrimSpace(s[start:i]))
				start = i + 1
			}
		}
	}
	if start < len(s) {
		args = append(args, strings.TrimSpace(s[start:]))
	}
	return args
}

func runRustAnalyzerParse(cmd, src string) (string, error) {
	ctx, cancel := context.WithTimeout(context.Background(), 5*time.Second)
	defer cancel()
	c := exec.CommandContext(ctx, cmd, "parse")
	c.Stdin = strings.NewReader(src)
	var out bytes.Buffer
	c.Stdout = &out
	if err := c.Run(); err != nil {
		return "", err
	}
	return out.String(), nil
}

// ConvertRust converts Rust source code to Mochi using rust-analyzer's parse output.
func ConvertRust(src string) ([]byte, error) {
	ls := Servers["rust"]
	if err := EnsureServer(ls.Command); err != nil {
		return nil, err
	}
	ast, err := runRustAnalyzerParse(ls.Command, src)
	if err != nil {
		return nil, err
	}
	tree := parseTree(ast)
	if tree == nil {
		return nil, fmt.Errorf("parse failed")
	}
	var out []string
	for _, c := range tree.children {
		if c.kind != "FN" {
			continue
		}
		nameNode := findChild(findChild(c, "NAME"), "IDENT")
		if nameNode == nil {
			continue
		}
		params := convertParams(src, findChild(c, "PARAM_LIST"))
		out = append(out, fmt.Sprintf("fun %s(%s) {", strings.TrimSpace(src[nameNode.start:nameNode.end]), strings.Join(params, ", ")))
		body := findChild(findChild(c, "BLOCK_EXPR"), "STMT_LIST")
		out = append(out, convertStmts(src, body, 1)...)
		out = append(out, "}")
	}
	if len(out) == 0 {
		return nil, fmt.Errorf("no convertible symbols found\n\nsource snippet:\n%s", numberedSnippet(src))
	}
	return []byte(strings.Join(out, "\n")), nil
}

// ConvertRustFile reads the rust file and converts it to Mochi.
func ConvertRustFile(path string) ([]byte, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	return ConvertRust(string(data))
}
