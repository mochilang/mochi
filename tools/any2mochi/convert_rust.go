package any2mochi

import (
	"bytes"
	"context"
	"fmt"
	"os"
	"os/exec"
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

// parseLine parses a single line of rust-analyzer parse output. It returns the
// indentation level, node kind and start/end offsets. The format is:
// "  KIND@0..3" or "  KIND@0..3 \"tok\"".
func parseLine(line string) (indent int, kind string, start, end int, ok bool) {
	i := 0
	for i < len(line) && line[i] == ' ' {
		i++
	}
	indent = i / 2
	rest := line[i:]
	at := strings.IndexByte(rest, '@')
	if at < 0 {
		return
	}
	kind = rest[:at]
	rest = rest[at+1:]
	dots := strings.Index(rest, "..")
	if dots < 0 {
		return
	}
	if s, err := strconv.Atoi(rest[:dots]); err == nil {
		start = s
	} else {
		return
	}
	rest = rest[dots+2:]
	endStr := rest
	if sp := strings.IndexByte(rest, ' '); sp >= 0 {
		endStr = rest[:sp]
	}
	if e, err := strconv.Atoi(endStr); err == nil {
		end = e
	} else {
		return
	}
	ok = true
	return
}

// parseTree parses the rust-analyzer syntax tree output into a hierarchy of nodes.
func parseTree(out string) *node {
	root := &node{kind: "ROOT"}
	stack := []*node{root}
	for _, line := range strings.Split(strings.TrimSpace(out), "\n") {
		if strings.TrimSpace(line) == "" {
			continue
		}
		indent, kind, start, end, ok := parseLine(line)
		if !ok {
			continue
		}
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
		if c.kind == "SELF_PARAM" {
			// ignore self parameter in methods
			continue
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
				parts := splitRustArgs(args)
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
		case "MATCH_EXPR":
			return convertMatch(src, c, level)
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
	case "MATCH_EXPR":
		return convertMatch(src, n, level)
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

func convertMatch(src string, n *node, level int) []string {
	arms := findChild(n, "MATCH_ARM_LIST")
	if arms == nil {
		return nil
	}
	header := strings.TrimSpace(src[n.start:arms.start])
	var out []string
	out = append(out, indent(level)+header+" {")
	for _, arm := range arms.children {
		if arm.kind != "MATCH_ARM" {
			continue
		}
		code := strings.TrimSpace(src[arm.start:arm.end])
		out = append(out, indent(level+1)+code)
	}
	out = append(out, indent(level)+"}")
	return out
}

func convertStmts(src string, list *node, level int) []string {
	var out []string
	if list == nil {
		return out
	}
	for _, c := range list.children {
		switch c.kind {
		case "LET_STMT", "EXPR_STMT", "RETURN_EXPR", "FOR_EXPR", "WHILE_EXPR", "IF_EXPR", "MATCH_EXPR":
			out = append(out, convertStmt(src, c, level)...)
		}
	}
	return out
}

func splitRustArgs(s string) []string {
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

func convertRustType(src string, n *node) string {
	if n == nil {
		return "any"
	}
	t := strings.TrimSpace(src[n.start:n.end])
	t = strings.TrimPrefix(t, "&")
	t = strings.TrimPrefix(t, "mut ")
	if strings.HasPrefix(t, "Box<") && strings.HasSuffix(t, ">") {
		t = strings.TrimSuffix(strings.TrimPrefix(t, "Box<"), ">")
	}
	switch t {
	case "i64", "i32", "i16", "i8", "isize", "usize", "u64", "u32", "u16", "u8":
		return "int"
	case "f64", "f32":
		return "float"
	case "bool":
		return "bool"
	case "String", "str":
		return "string"
	default:
		return t
	}
}

func convertStruct(src string, n *node) []string {
	nameNode := findChild(n, "NAME")
	if nameNode == nil {
		return nil
	}
	name := strings.TrimSpace(src[nameNode.start:nameNode.end])
	fields := findChild(n, "RECORD_FIELD_LIST")
	var out []string
	out = append(out, fmt.Sprintf("type %s {", name))
	if fields != nil {
		for _, f := range fields.children {
			if f.kind != "RECORD_FIELD" {
				continue
			}
			fnameNode := findChild(f, "NAME")
			if fnameNode == nil {
				continue
			}
			typNode := findChild(f, "PATH_TYPE")
			if typNode == nil {
				typNode = findChild(f, "REF_TYPE")
			}
			fname := strings.TrimSpace(src[fnameNode.start:fnameNode.end])
			typ := convertRustType(src, typNode)
			out = append(out, fmt.Sprintf("  %s: %s", fname, typ))
		}
	}
	out = append(out, "}")
	return out
}

func convertEnum(src string, n *node) []string {
	nameNode := findChild(n, "NAME")
	if nameNode == nil {
		return nil
	}
	name := strings.TrimSpace(src[nameNode.start:nameNode.end])
	variants := findChild(n, "VARIANT_LIST")
	if variants == nil {
		return nil
	}
	var out []string
	out = append(out, fmt.Sprintf("type %s =", name))
	first := true
	for _, v := range variants.children {
		if v.kind != "VARIANT" {
			continue
		}
		linePrefix := "  "
		if !first {
			linePrefix = "  | "
		}
		first = false
		vnameNode := findChild(v, "NAME")
		if vnameNode == nil {
			continue
		}
		vname := strings.TrimSpace(src[vnameNode.start:vnameNode.end])
		if rfl := findChild(v, "RECORD_FIELD_LIST"); rfl != nil {
			var fields []string
			for _, rf := range rfl.children {
				if rf.kind != "RECORD_FIELD" {
					continue
				}
				fnameNode := findChild(rf, "NAME")
				typNode := findChild(rf, "PATH_TYPE")
				if typNode == nil {
					typNode = findChild(rf, "REF_TYPE")
				}
				if fnameNode != nil {
					fname := strings.TrimSpace(src[fnameNode.start:fnameNode.end])
					typ := convertRustType(src, typNode)
					fields = append(fields, fmt.Sprintf("%s: %s", fname, typ))
				}
			}
			out = append(out, linePrefix+vname+"("+strings.Join(fields, ", ")+")")
		} else {
			out = append(out, linePrefix+vname)
		}
	}
	return out
}

func convertFn(src string, n *node, level int) []string {
	nameNode := findChild(findChild(n, "NAME"), "IDENT")
	if nameNode == nil {
		return nil
	}
	params := convertParams(src, findChild(n, "PARAM_LIST"))
	out := []string{indent(level) + fmt.Sprintf("fun %s(%s) {", strings.TrimSpace(src[nameNode.start:nameNode.end]), strings.Join(params, ", "))}
	body := findChild(findChild(n, "BLOCK_EXPR"), "STMT_LIST")
	out = append(out, convertStmts(src, body, level+1)...)
	out = append(out, indent(level)+"}")
	return out
}

func convertImpl(src string, n *node) []string {
	typNode := findChild(n, "PATH_TYPE")
	if typNode == nil {
		return nil
	}
	typeName := strings.TrimSpace(src[typNode.start:typNode.end])
	itemList := findChild(n, "ASSOC_ITEM_LIST")
	if itemList == nil {
		return nil
	}
	var out []string
	for _, it := range itemList.children {
		if it.kind != "FN" {
			continue
		}
		method := convertFn(src, it, 1)
		if len(method) > 0 {
			out = append(out, method...)
		}
	}
	if len(out) == 0 {
		return nil
	}
	wrapped := []string{fmt.Sprintf("extend %s {", typeName)}
	wrapped = append(wrapped, out...)
	wrapped = append(wrapped, "}")
	return wrapped
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
	structs := make(map[string][]string)
	order := []string{}
	methods := make(map[string][]string)
	for _, c := range tree.children {
		switch c.kind {
		case "STRUCT":
			nameNode := findChild(c, "NAME")
			if nameNode != nil {
				name := strings.TrimSpace(src[nameNode.start:nameNode.end])
				structs[name] = convertStruct(src, c)
				order = append(order, name)
			}
		case "ENUM":
			out = append(out, convertEnum(src, c)...)
		case "IMPL":
			typNode := findChild(c, "PATH_TYPE")
			if typNode != nil {
				name := strings.TrimSpace(src[typNode.start:typNode.end])
				methods[name] = append(methods[name], convertImpl(src, c)...)
			}
		case "FN":
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
	}
	var full []string
	for _, name := range order {
		lines := structs[name]
		if m := methods[name]; len(m) > 0 {
			lines = append(lines[:len(lines)-1], m...)
			lines = append(lines, "}")
		}
		full = append(full, lines...)
	}
	out = append(full, out...)
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
