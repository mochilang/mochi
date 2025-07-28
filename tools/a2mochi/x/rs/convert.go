//go:build slow

package rs

import (
	"bytes"
	"context"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"regexp"
	"strconv"
	"strings"
	"time"

	mochiast "mochi/ast"
	"mochi/parser"
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
			typNode := findChild(c, "PATH_TYPE")
			if typNode == nil {
				typNode = findChild(c, "REF_TYPE")
			}
			if id != nil {
				name := strings.TrimSpace(src[id.start:id.end])
				typ := convertRustType(src, typNode)
				if typ != "any" {
					params = append(params, fmt.Sprintf("%s: %s", name, typ))
				} else {
					params = append(params, name)
				}
			}
		}
		if c.kind == "SELF_PARAM" {
			// ignore self parameter in methods
			continue
		}
	}
	return params
}

// convertLetStmt converts a LET_STMT node into a Mochi variable declaration line.
func convertLetStmt(src string, n *node, level int) string {
	text := strings.TrimSpace(src[n.start:n.end])
	text = strings.TrimSuffix(text, ";")
	text = strings.Replace(text, "let ", "var ", 1)
	text = strings.Replace(text, "mut ", "", 1)
	// convert type annotation if present
	if colon := strings.Index(text, ":"); colon >= 0 {
		eq := strings.Index(text, "=")
		if eq == -1 || colon < eq {
			name := strings.TrimSpace(text[:colon])
			rest := text[colon+1:]
			typStr := strings.TrimSpace(rest)
			val := ""
			if eq >= 0 {
				typStr = strings.TrimSpace(rest[:eq-colon-1])
				val = rest[eq-colon-1:]
			}
			typ := convertRustTypeFromString(typStr)
			text = name + ": " + typ + val
		}
	}
	// sanitize value expression separately to catch vec! and push() usage
	if eq := strings.Index(text, "="); eq >= 0 {
		before := strings.TrimSpace(text[:eq])
		expr := strings.TrimSpace(text[eq+1:])
		expr = sanitizeExpr(expr)
		text = before + " = " + expr
	}
	text = sanitizeExpr(text)
	return indent(level) + text
}

func convertStmt(src string, n *node, level int) []string {
	idt := indent(level)
	switch n.kind {
	case "LET_STMT":
		return []string{convertLetStmt(src, n, level)}
	case "EXPR_STMT":
		if len(n.children) == 0 {
			return nil
		}
		c := n.children[0]
		switch c.kind {
		case "MACRO_EXPR":
			code := strings.TrimSpace(src[c.start:c.end])
			if line, ok := convertPrintMacro(code); ok {
				return []string{idt + sanitizeExpr(line)}
			}
			if line, ok := convertVecMacro(code); ok {
				return []string{idt + sanitizeExpr(line)}
			}
			code = strings.TrimSuffix(code, ";")
			code = sanitizeExpr(code)
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
			code = sanitizeExpr(code)
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
	var lastExpr *node
	for _, c := range list.children {
		switch c.kind {
		case "LET_STMT", "EXPR_STMT", "RETURN_EXPR", "FOR_EXPR", "WHILE_EXPR", "IF_EXPR", "MATCH_EXPR":
			out = append(out, convertStmt(src, c, level)...)
			lastExpr = nil
		default:
			if strings.Contains(c.kind, "EXPR") {
				lastExpr = c
			}
		}
	}
	if lastExpr != nil {
		expr := strings.TrimSpace(src[lastExpr.start:lastExpr.end])
		if expr != "" {
			expr = sanitizeExpr(expr)
			out = append(out, indent(level)+"return "+expr)
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

func convertPrintMacro(code string) (string, bool) {
	macros := []string{"println!", "print!", "eprintln!", "dbg!"}
	for _, m := range macros {
		if strings.HasPrefix(code, m) {
			args := strings.TrimPrefix(code, m)
			args = strings.TrimSpace(strings.TrimSuffix(strings.TrimPrefix(args, "("), ")"))
			parts := splitRustArgs(args)
			for i, p := range parts {
				parts[i] = sanitizeExpr(p)
			}
			if m == "dbg!" {
				return "print(" + strings.Join(parts, ", ") + ")", true
			}
			if len(parts) > 0 && strings.HasPrefix(strings.TrimSpace(parts[0]), "\"") {
				if len(parts) == 1 {
					return fmt.Sprintf("print(%s)", parts[0]), true
				}
				parts = parts[1:]
			}
			return "print(" + strings.Join(parts, ", ") + ")", true
		}
	}
	return "", false
}

func convertVecMacro(code string) (string, bool) {
	c := strings.TrimSpace(code)
	if strings.HasPrefix(c, "vec![") && strings.HasSuffix(c, "]") {
		inner := strings.TrimSuffix(strings.TrimPrefix(c, "vec!["), "]")
		return "[" + inner + "]", true
	}
	if c == "Vec::new()" {
		return "[]", true
	}
	return "", false
}

func convertBoxNew(code string) (string, bool) {
	c := strings.TrimSpace(code)
	if strings.HasPrefix(c, "Box::new(") && strings.HasSuffix(c, ")") {
		inner := c[len("Box::new(") : len(c)-1]
		return inner, true
	}
	return "", false
}

func sanitizeExpr(code string) string {
	code = strings.TrimSpace(code)

	// drop reference/deref prefixes often produced by the compiler
	for {
		changed := false
		if strings.HasPrefix(code, "&") {
			code = strings.TrimSpace(strings.TrimPrefix(code, "&"))
			changed = true
		}
		if strings.HasPrefix(code, "let ") {
			code = strings.TrimSpace(strings.Replace(code, "let ", "var ", 1))
			changed = true
		}
		if strings.HasPrefix(code, "mut ") {
			code = strings.TrimSpace(strings.TrimPrefix(code, "mut "))
			changed = true
		}
		if strings.HasPrefix(code, "*") {
			code = strings.TrimSpace(strings.TrimPrefix(code, "*"))
			changed = true
		}
		if !changed {
			break
		}
	}

	if v, ok := convertVecMacro(code); ok {
		code = v
	} else if v, ok := convertBoxNew(code); ok {
		code = v
	}
	code = strings.ReplaceAll(code, "let ", "var ")
	code = strings.ReplaceAll(code, "mut ", "")
	// convert inline vec! macro usage
	reVec := regexp.MustCompile(`vec!\[(?P<items>[^\]]*)\]`)
	code = reVec.ReplaceAllString(code, `[$items]`)
	// convert push() into append() assignment
	rePush := regexp.MustCompile(`\b([A-Za-z_][A-Za-z0-9_]*)\.push\(([^)]*)\)`)
	code = rePush.ReplaceAllString(code, `$1 = append($1, $2)`)
	if strings.HasPrefix(code, "{") && strings.HasSuffix(code, "}") {
		inner := strings.TrimSpace(code[1 : len(code)-1])
		parts := strings.Split(inner, ";")
		if len(parts) == 3 {
			p1 := strings.TrimSpace(parts[0])
			p2 := strings.TrimSpace(parts[1])
			p3 := strings.TrimSpace(parts[2])
			if strings.HasPrefix(p1, "var ") {
				if eq := strings.Index(p1, "="); eq > 0 {
					name := strings.TrimSpace(p1[4:eq])
					base := strings.TrimSpace(p1[eq+1:])
					pushPrefix := fmt.Sprintf("%s = append(%s,", name, name)
					if strings.HasPrefix(p2, pushPrefix) && p3 == name {
						arg := strings.TrimSpace(strings.TrimSuffix(strings.TrimPrefix(p2, pushPrefix), ")"))
						code = fmt.Sprintf("append(%s, %s)", base, arg)
					}
				}
			}
		}
	}

	code = strings.ReplaceAll(code, ".to_vec()", "")
	code = strings.ReplaceAll(code, ".clone()", "")
	code = strings.ReplaceAll(code, ".to_string()", "")
	// simplify "+ -N" into "- N" for numeric literals
	code = regexp.MustCompile(`\+\s*-([0-9])`).ReplaceAllString(code, " - $1")

	code = strings.ReplaceAll(code, "Default::default()", "0")

	return code
}

func convertRustType(src string, n *node) string {
	if n == nil {
		return "any"
	}
	return convertRustTypeFromString(strings.TrimSpace(src[n.start:n.end]))
}

func convertRustTypeFromString(t string) string {
	t = strings.TrimSpace(t)
	t = strings.TrimPrefix(t, "&")
	t = strings.TrimPrefix(t, "mut ")
	if strings.HasPrefix(t, "Box<") && strings.HasSuffix(t, ">") {
		t = strings.TrimSuffix(strings.TrimPrefix(t, "Box<"), ">")
	}
	if strings.HasPrefix(t, "Vec<") && strings.HasSuffix(t, ">") {
		// list types are not explicitly typed in Mochi
		return "any"
	}
	if strings.HasPrefix(t, "Option<") && strings.HasSuffix(t, ">") {
		inner := t[7 : len(t)-1]
		return convertRustTypeFromString(inner)
	}
	if strings.HasPrefix(t, "[") && strings.HasSuffix(t, "]") {
		inner := t[1 : len(t)-1]
		if idx := strings.Index(inner, ";"); idx >= 0 {
			inner = inner[:idx]
		}
		return "[" + convertRustTypeFromString(inner) + "]"
	}
	if i := strings.Index(t, "<"); i >= 0 {
		t = t[:i]
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
	}
	if len(t) <= 2 && t != "" && strings.ToUpper(t) == t {
		return "any"
	}
	return t
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

func convertConst(src string, n *node) []string {
	nameNode := findChild(n, "NAME")
	valNode := findChild(n, "LITERAL")
	if nameNode == nil || valNode == nil {
		return nil
	}
	name := strings.TrimSpace(src[nameNode.start:nameNode.end])
	val := strings.TrimSpace(src[valNode.start:valNode.end])
	typNode := findChild(n, "PATH_TYPE")
	if typNode == nil {
		typNode = findChild(n, "REF_TYPE")
	}
	typ := convertRustType(src, typNode)
	line := "const " + name
	if typ != "any" {
		line += ": " + typ
	}
	line += " = " + val
	return []string{line}
}

func convertTypeAlias(src string, n *node) []string {
	nameNode := findChild(n, "NAME")
	typNode := findChild(n, "PATH_TYPE")
	if typNode == nil {
		typNode = findChild(n, "REF_TYPE")
	}
	if nameNode == nil || typNode == nil {
		return nil
	}
	name := strings.TrimSpace(src[nameNode.start:nameNode.end])
	typ := convertRustType(src, typNode)
	return []string{fmt.Sprintf("type %s = %s", name, typ)}
}

func convertFn(src string, n *node, level int) []string {
	nameNode := findChild(findChild(n, "NAME"), "IDENT")
	if nameNode == nil {
		return nil
	}
	params := convertParams(src, findChild(n, "PARAM_LIST"))
	ret := ""
	if rt := findChild(n, "RET_TYPE"); rt != nil {
		typNode := findChild(rt, "PATH_TYPE")
		if typNode == nil {
			typNode = findChild(rt, "REF_TYPE")
		}
		ret = convertRustType(src, typNode)
		if ret == "()" {
			ret = ""
		}
	}
	header := fmt.Sprintf("fun %s(%s)", strings.TrimSpace(src[nameNode.start:nameNode.end]), strings.Join(params, ", "))
	if ret != "" {
		header += ": " + ret
	}
	out := []string{indent(level) + header + " {"}
	block := findChild(n, "BLOCK_EXPR")
	body := findChild(block, "STMT_LIST")
	if body != nil {
		out = append(out, convertStmts(src, body, level+1)...)
	} else if block != nil {
		out = append(out, fallbackRustBody(src[block.start:block.end], level+1)...)
	}
	out = append(out, indent(level)+"}")
	return out
}

func fallbackRustBody(body string, level int) []string {
	body = strings.TrimSpace(body)
	if strings.HasPrefix(body, "{") && strings.HasSuffix(body, "}") {
		body = strings.TrimSpace(body[1 : len(body)-1])
	}
	var out []string
	for _, line := range strings.Split(body, "\n") {
		line = strings.TrimSpace(line)
		if line == "" {
			continue
		}
		if strings.HasPrefix(line, "#") {
			// skip attributes and macros like #[derive(...)] that are
			// not valid Mochi syntax
			continue
		}
		line = strings.TrimSuffix(line, ";")
		line = strings.Replace(line, "let ", "var ", 1)
		line = strings.Replace(line, "mut ", "", 1)
		if nl, ok := convertPrintMacro(line); ok {
			line = nl
		}
		line = sanitizeExpr(line)
		out = append(out, indent(level)+line)
	}
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

func convertTrait(src string, n *node) []string {
	nameNode := findChild(n, "NAME")
	if nameNode == nil {
		return nil
	}
	name := strings.TrimSpace(src[nameNode.start:nameNode.end])
	itemList := findChild(n, "ASSOC_ITEM_LIST")
	var methods []string
	if itemList != nil {
		for _, it := range itemList.children {
			if it.kind != "FN" {
				continue
			}
			line := strings.TrimSpace(src[it.start:it.end])
			line = strings.TrimSuffix(line, "{")
			line = strings.TrimSuffix(line, "}")
			line = strings.TrimSuffix(line, ";")
			methods = append(methods, "# "+line)
		}
	}
	out := []string{fmt.Sprintf("# trait %s {", name)}
	out = append(out, methods...)
	out = append(out, "# }")
	return out
}

func runRustAnalyzerParse(cmd, src string) (string, error) {
	ctx, cancel := context.WithTimeout(context.Background(), 5*time.Second)
	defer cancel()
	c := exec.CommandContext(ctx, cmd, "parse")
	c.Stdin = strings.NewReader(src)
	var out, stderr bytes.Buffer
	c.Stdout = &out
	c.Stderr = &stderr
	if err := c.Run(); err != nil {
		if msg := strings.TrimSpace(stderr.String()); msg != "" {
			return "", fmt.Errorf("%v: %s", err, msg)
		}
		return "", err
	}
	return out.String(), nil
}

func convertRustTree(src string, tree *node) ([]byte, error) {
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
		case "CONST":
			out = append(out, convertConst(src, c)...)
		case "TYPE_ALIAS":
			out = append(out, convertTypeAlias(src, c)...)
		case "IMPL":
			typNode := findChild(c, "PATH_TYPE")
			if typNode != nil {
				name := strings.TrimSpace(src[typNode.start:typNode.end])
				methods[name] = append(methods[name], convertImpl(src, c)...)
			}
		case "TRAIT":
			out = append(out, convertTrait(src, c)...)
		case "FN":
			nameNode := findChild(findChild(c, "NAME"), "IDENT")
			if nameNode == nil {
				continue
			}
			params := convertParams(src, findChild(c, "PARAM_LIST"))
			ret := ""
			if rt := findChild(c, "RET_TYPE"); rt != nil {
				typNode := findChild(rt, "PATH_TYPE")
				if typNode == nil {
					typNode = findChild(rt, "REF_TYPE")
				}
				ret = convertRustType(src, typNode)
				if ret == "()" {
					ret = ""
				}
			}
			header := fmt.Sprintf("fun %s(%s)", strings.TrimSpace(src[nameNode.start:nameNode.end]), strings.Join(params, ", "))
			if ret != "" {
				header += ": " + ret
			}
			out = append(out, header+" {")
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
		return nil, fmt.Errorf("no convertible symbols found\n\nsource snippet:\n%s", snippet(src))
	}
	return []byte(strings.Join(out, "\n")), nil
}

// ConvertAST converts Rust source code using an already parsed syntax tree.
func ConvertAST(src string, n *ASTNode) (*mochiast.Node, error) {
	code, err := ConvertSourceAST(src, n)
	if err != nil {
		return nil, err
	}
	prog, err := parser.ParseString(code)
	if err != nil {
		return nil, err
	}
	return mochiast.FromProgram(prog), nil
}

// Convert converts Rust source code to a Mochi AST using rust-analyzer's parse output.
func Convert(src string) (*mochiast.Node, error) {
	code, err := ConvertSource(src)
	if err != nil {
		return nil, err
	}
	prog, err := parser.ParseString(code)
	if err != nil {
		return nil, err
	}
	return mochiast.FromProgram(prog), nil
}

// ConvertFile reads the Rust file and converts it to Mochi.
func ConvertFile(path string) (*mochiast.Node, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	return Convert(string(data))
}

// ConvertASTFile parses the given Rust file and converts it using the parsed AST.
func ConvertASTFile(path string) (*mochiast.Node, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	ast, err := ParseAST(string(data))
	if err != nil {
		return nil, err
	}
	return ConvertAST(string(data), ast)
}

// ConvertASTFileSource parses the Rust file and converts it to Mochi source with
// header and embedded original source code using the parsed AST.
func ConvertASTFileSource(path string) (string, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return "", err
	}
	ast, err := ParseAST(string(data))
	if err != nil {
		return "", err
	}
	return ConvertSourceAST(string(data), ast)
}

func snippet(src string) string {
	lines := strings.Split(src, "\n")
	if len(lines) > 10 {
		lines = lines[:10]
	}
	for i, l := range lines {
		lines[i] = fmt.Sprintf("%3d: %s", i+1, l)
	}
	return strings.Join(lines, "\n")
}

func snippetAt(src string, line, col int) string {
	lines := strings.Split(src, "\n")
	start := line - 1
	if start-1 >= 0 {
		start--
	}
	end := line - 1
	if end+1 < len(lines) {
		end++
	}
	var out strings.Builder
	for i := start; i <= end && i < len(lines); i++ {
		out.WriteString(fmt.Sprintf("%3d | %s\n", i+1, lines[i]))
		if i == line-1 {
			out.WriteString("    | " + strings.Repeat(" ", col-1) + "^\n")
		}
	}
	return strings.TrimSuffix(out.String(), "\n")
}

func findRepoRoot() string {
	dir, _ := os.Getwd()
	for i := 0; i < 10; i++ {
		if _, err := os.Stat(filepath.Join(dir, "go.mod")); err == nil {
			return dir
		}
		parent := filepath.Dir(dir)
		if parent == dir {
			break
		}
		dir = parent
	}
	return "."
}

func readVersion() string {
	path := filepath.Join(findRepoRoot(), "VERSION")
	if b, err := os.ReadFile(path); err == nil {
		return strings.TrimSpace(string(b))
	}
	return "dev"
}

func addHeader(src string, code []byte) string {
	tz := time.FixedZone("GMT+7", 7*3600)
	header := fmt.Sprintf("// Generated by Mochi a2mochi v%s on %s", readVersion(), time.Now().In(tz).Format("2006-01-02 15:04 -0700"))
	var b strings.Builder
	b.WriteString(header)
	b.WriteByte('\n')
	b.WriteString("/*\n")
	b.WriteString(src)
	if !strings.HasSuffix(src, "\n") {
		b.WriteByte('\n')
	}
	b.WriteString("*/\n")
	b.Write(code)
	return b.String()
}

// ConvertSourceAST converts Rust source code and its parsed AST into Mochi source
// with a generated header and the original Rust source as a block comment.
func ConvertSourceAST(src string, n *ASTNode) (string, error) {
	code, err := convertRustTree(src, fromASTNode(n))
	if err != nil {
		return "", err
	}
	return addHeader(src, code), nil
}

// ConvertSource converts Rust source code into Mochi source using rust-analyzer.
// The generated code includes a header comment and the original Rust source.
func ConvertSource(src string) (string, error) {
	if _, err := exec.LookPath("rust-analyzer"); err != nil {
		return "", fmt.Errorf("rust-analyzer not installed")
	}
	out, err := runRustAnalyzerParse("rust-analyzer", src)
	if err != nil {
		return "", err
	}
	tree := parseTree(out)
	code, err := convertRustTree(src, tree)
	if err != nil {
		return "", err
	}
	return addHeader(src, code), nil
}

// ConvertFileSource reads the Rust file and converts it to Mochi source with
// header and embedded original source code.
func ConvertFileSource(path string) (string, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return "", err
	}
	return ConvertSource(string(data))
}
