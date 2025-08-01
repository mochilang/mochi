//go:build slow

package rb

import (
	"bytes"
	"encoding/json"
	"fmt"
	"os"
	"os/exec"
	"regexp"
	"strconv"
	"strings"

	"mochi/transpiler/meta"

	"mochi/ast"
	"mochi/parser"
)

// Node represents a Ruby AST node parsed from the ripper library.
type Node struct {
	Type     string
	Value    string
	Line     int
	Col      int
	Children []Node
	EndLine  int
	EndCol   int
	Source   string
}

// Parse parses Ruby source code into a Node tree using ripper.
func Parse(src string) (*Node, error) {
	if _, err := exec.LookPath("ruby"); err != nil {
		return nil, fmt.Errorf("ruby not installed: %w", err)
	}
	cmd := exec.Command("ruby", "-e", `require 'json';require 'ripper';src=STDIN.read;b=Ripper::SexpBuilder.new(src);ast=b.parse;if b.error?; STDERR.puts b.error; exit 1; end;puts JSON.generate(ast)`)
	cmd.Stdin = strings.NewReader(src)
	var out bytes.Buffer
	var errBuf bytes.Buffer
	cmd.Stdout = &out
	cmd.Stderr = &errBuf
	if err := cmd.Run(); err != nil {
		if errBuf.Len() > 0 {
			msg := strings.TrimSpace(errBuf.String())
			line := 0
			if m := regexp.MustCompile(`(\d+):`).FindStringSubmatch(msg); len(m) > 1 {
				if n, _ := strconv.Atoi(m[1]); n > 0 {
					line = n
				}
			}
			if line > 0 {
				snip := snippetAround(src, line)
				return nil, fmt.Errorf("line %d: %s\n%s", line, msg, snip)
			}
			return nil, fmt.Errorf("%s", msg)
		}
		return nil, err
	}
	var data any
	if err := json.Unmarshal(out.Bytes(), &data); err != nil {
		return nil, err
	}
	node := buildNode(data)
	node.Source = src
	return &node, nil
}

func buildNode(v any) Node {
	if s, ok := v.(string); ok {
		return Node{Type: "@tok", Value: s}
	}
	arr, ok := v.([]any)
	if !ok || len(arr) == 0 {
		return Node{}
	}
	n := Node{Type: fmt.Sprint(arr[0])}
	if strings.HasPrefix(n.Type, "@") {
		if len(arr) > 1 {
			if s, ok := arr[1].(string); ok {
				n.Value = s
			}
		}
		if len(arr) > 2 {
			if pos, ok := arr[2].([]any); ok && len(pos) >= 2 {
				if ln, ok := pos[0].(float64); ok {
					n.Line = int(ln)
				}
				if col, ok := pos[1].(float64); ok {
					n.Col = int(col)
				}
			}
		}
		n.EndLine = n.Line
		n.EndCol = n.Col
		return n
	}
	for i := 1; i < len(arr); i++ {
		child := buildNode(arr[i])
		if n.Line == 0 && child.Line != 0 {
			n.Line = child.Line
			n.Col = child.Col
		}
		if child.Type != "" {
			n.Children = append(n.Children, child)
			n.EndLine = child.EndLine
			n.EndCol = child.EndCol
		}
	}
	return n
}

func tokens(n Node) []string {
	if strings.HasPrefix(n.Type, "@") {
		if n.Value != "" {
			switch n.Type {
			case "@tstring_content":
				return []string{"\"" + n.Value + "\""}
			case "@tok":
				if n.Value == "-@" {
					return []string{"-"}
				}
			}
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
	switch n.Type {
	case "array":
		ts := tokens(n)
		return "[" + strings.Join(ts, ", ") + "]"
	case "hash":
		var parts []string
		var walk func(Node)
		walk = func(x Node) {
			switch x.Type {
			case "assoc_new":
				if len(x.Children) == 2 {
					k := exprString(x.Children[0])
					v := exprString(x.Children[1])
					parts = append(parts, k+": "+v)
				}
			case "args_add", "assoclist_from_args", "args_add_block", "list":
				for _, c := range x.Children {
					walk(c)
				}
			}
		}
		for _, c := range n.Children {
			walk(c)
		}
		return "{" + strings.Join(parts, ", ") + "}"
	case "aref":
		if len(n.Children) == 2 {
			recv := exprString(n.Children[0])
			idx := n.Children[1]
			if idx.Type == "args_add_block" && len(idx.Children) > 0 {
				idx = idx.Children[0]
			}
			return recv + "[" + exprString(idx) + "]"
		}
	}

	if n.Type == "paren" && len(n.Children) == 1 {
		return "(" + exprString(n.Children[0]) + ")"
	}
	if n.Type == "unary" && len(n.Children) == 2 {
		op := exprString(n.Children[0])
		val := exprString(n.Children[1])
		return op + val
	}
	toks := tokens(n)
	if len(toks) == 0 {
		return ""
	}
	s := strings.Join(toks, " ")
	reps := []struct{ old, new string }{
		{" ( ", "("}, {" )", ")"}, {"( ", "("}, {" , ", ", "},
		{" [ ", "["}, {" ] ", "]"}, {" . ", "."}, {"- ", "-"},
	}
	for _, r := range reps {
		s = strings.ReplaceAll(s, r.old, r.new)
	}
	return strings.TrimSpace(s)
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

func structFields(n Node) ([]string, bool) {
	if n.Type != "method_add_arg" || len(n.Children) < 2 {
		return nil, false
	}
	call := n.Children[0]
	if call.Type != "call" || len(call.Children) < 3 {
		return nil, false
	}
	if !(call.Children[0].Type == "var_ref" && len(call.Children[0].Children) > 0 && call.Children[0].Children[0].Type == "@const" && call.Children[0].Children[0].Value == "Struct") {
		return nil, false
	}
	if call.Children[2].Type != "@ident" || call.Children[2].Value != "new" {
		return nil, false
	}
	arg := n.Children[1]
	if arg.Type == "arg_paren" && len(arg.Children) > 0 {
		arg = arg.Children[0]
	}
	var fields []string
	var walk func(Node)
	walk = func(x Node) {
		switch x.Type {
		case "args_add", "args_add_block":
			for _, c := range x.Children {
				walk(c)
			}
		case "symbol_literal":
			if len(x.Children) == 0 {
				return
			}
			sym := x.Children[0]
			if sym.Type == "symbol" && len(sym.Children) > 0 && sym.Children[0].Type == "@ident" {
				fields = append(fields, sym.Children[0].Value)
			}
		}
	}
	walk(arg)
	if len(fields) == 0 {
		return nil, false
	}
	return fields, true
}

func digitsStringValue(n Node) (string, bool) {
	if n.Type == "string_literal" && len(n.Children) > 0 {
		return digitsStringValue(n.Children[len(n.Children)-1])
	}
	if n.Type == "string_add" && len(n.Children) > 0 {
		return digitsStringValue(n.Children[len(n.Children)-1])
	}
	if n.Type == "string_content" && len(n.Children) == 1 {
		return digitsStringValue(n.Children[0])
	}
	if n.Type == "@tstring_content" {
		if regexp.MustCompile(`^\d+$`).MatchString(n.Value) {
			return n.Value, true
		}
	}
	return "", false
}

func firstArg(n Node) Node {
	for {
		switch n.Type {
		case "arg_paren", "args_add_block", "args_new":
			if len(n.Children) > 0 {
				n = n.Children[0]
				continue
			}
		case "args_add":
			if len(n.Children) > 0 {
				n = n.Children[len(n.Children)-1]
				continue
			}
		}
		return n
	}
}

func convertNode(n Node, level int, out *[]string, globals map[string]bool) {
	idt := strings.Repeat("  ", level)
	switch n.Type {
	case "program":
		for _, c := range n.Children {
			convertNode(c, level, out, globals)
		}
	case "command", "method_add_arg":
		if len(n.Children) > 0 {
			f := n.Children[0]
			if (f.Type == "@ident" && f.Value == "puts") ||
				(f.Type == "fcall" && len(f.Children) > 0 && f.Children[0].Type == "@ident" && f.Children[0].Value == "puts") {
				argNode := n.Children[len(n.Children)-1]
				if val, ok := digitsStringValue(argNode); ok {
					*out = append(*out, idt+"print(int(\""+val+"\"))")
				} else if argNode.Type == "call" && len(argNode.Children) == 3 && argNode.Children[2].Type == "@ident" && argNode.Children[2].Value == "length" {
					expr := exprString(argNode.Children[0])
					*out = append(*out, idt+"print(len("+expr+"))")
				} else if argNode.Type == "method_add_arg" {
					call := argNode.Children[0]
					if call.Type == "call" && len(call.Children) == 3 {
						m := call.Children[2]
						if m.Type == "@ident" && (m.Value == "include?" || m.Value == "key?") {
							recv := exprString(call.Children[0])
							arg := exprString(firstArg(argNode.Children[1]))
							*out = append(*out, idt+"print(("+arg+" in "+recv+"))")
							return
						}
					}
					arg := exprString(argNode)
					*out = append(*out, idt+"print("+arg+")")
				} else {
					arg := exprString(argNode)
					*out = append(*out, idt+"print("+arg+")")
				}
				return
			}
		}
		*out = append(*out, idt+exprString(n))
	case "assign":
		if len(n.Children) < 2 {
			return
		}
		name := exprString(n.Children[0])
		if fields, ok := structFields(n.Children[1]); ok {
			*out = append(*out, idt+"type "+name+" {")
			for _, f := range fields {
				*out = append(*out, idt+"  "+f+": any")
			}
			*out = append(*out, idt+"}")
			return
		}
		expr := exprString(n.Children[1])
		kw := ""
		if n.Children[0].Type == "var_field" {
			if level == 0 {
				if !globals[name] {
					kw = "var "
					globals[name] = true
				}
			} else {
				kw = "let "
			}
		}
		*out = append(*out, idt+kw+name+" = "+expr)
	case "while":
		if len(n.Children) < 2 {
			return
		}
		cond := exprString(n.Children[0])
		*out = append(*out, idt+"while "+cond+" {")
		convertNode(n.Children[1], level+1, out, globals)
		*out = append(*out, idt+"}")
	case "return", "break", "next":
		expr := exprString(n)
		if n.Type == "next" {
			expr = strings.Replace(expr, "next", "continue", 1)
		}
		*out = append(*out, idt+expr)
	case "for":
		if len(n.Children) < 3 {
			return
		}
		varName := exprString(n.Children[0])
		iter := exprString(n.Children[1])
		*out = append(*out, idt+"for "+varName+" in "+iter+" {")
		convertNode(n.Children[2], level+1, out, globals)
		*out = append(*out, idt+"}")
	case "method_add_block":
		if len(n.Children) == 2 {
			call := n.Children[0]
			block := n.Children[1]
			if call.Type == "call" && len(call.Children) == 3 && call.Children[2].Type == "@ident" && call.Children[2].Value == "times" {
				count := exprString(call.Children[0])
				varName := "i"
				body := block
				if block.Type == "do_block" && len(block.Children) >= 2 {
					if len(block.Children) > 0 && block.Children[0].Type == "block_var" {
						bv := block.Children[0]
						if len(bv.Children) > 0 {
							params := bv.Children[0]
							if params.Type == "params" && len(params.Children) > 0 && len(params.Children[0].Children) > 0 {
								ident := params.Children[0].Children[0]
								if ident.Type == "@ident" {
									varName = ident.Value
								}
							}
						}
					}
					body = block.Children[len(block.Children)-1]
				}
				*out = append(*out, idt+"for "+varName+" in range("+count+") {")
				convertNode(body, level+1, out, globals)
				*out = append(*out, idt+"}")
				return
			}
		}
	case "if":
		if len(n.Children) < 2 {
			return
		}
		cond := exprString(n.Children[0])
		*out = append(*out, idt+"if "+cond+" {")
		convertNode(n.Children[1], level+1, out, globals)
		if len(n.Children) > 2 {
			handleElse := func(e Node) {}
			handleElse = func(e Node) {
				switch e.Type {
				case "else":
					*out = append(*out, idt+"} else {")
					convertNode(e, level+1, out, globals)
				case "elsif":
					cond := exprString(e.Children[0])
					*out = append(*out, idt+"} else if "+cond+" {")
					convertNode(e.Children[1], level+1, out, globals)
					if len(e.Children) > 2 {
						handleElse(e.Children[2])
					}
				}
			}
			handleElse(n.Children[2])
		}
		*out = append(*out, idt+"}")
	case "case":
		if len(n.Children) < 2 {
			return
		}
		cond := exprString(n.Children[0])
		handleCase := func(w Node, first bool) {}
		handleCase = func(w Node, first bool) {
			switch w.Type {
			case "when":
				if len(w.Children) < 2 {
					return
				}
				var condParts []string
				for _, c := range w.Children[0].Children {
					condParts = append(condParts, cond+" =="+exprString(c))
				}
				if len(condParts) == 0 {
					condParts = append(condParts, cond+" =="+exprString(w.Children[0]))
				}
				cStr := strings.Join(condParts, " || ")
				if first {
					*out = append(*out, idt+"if "+cStr+" {")
				} else {
					*out = append(*out, idt+"} else if "+cStr+" {")
				}
				convertNode(w.Children[1], level+1, out, globals)
				if len(w.Children) > 2 {
					handleCase(w.Children[2], false)
				} else {
					*out = append(*out, idt+"}")
				}
			case "else":
				*out = append(*out, idt+"} else {")
				if len(w.Children) > 0 {
					convertNode(w.Children[0], level+1, out, globals)
				}
				*out = append(*out, idt+"}")
			}
		}
		handleCase(n.Children[1], true)
	case "else", "bodystmt":
		for _, c := range n.Children {
			convertNode(c, level, out, globals)
		}
	case "def":
		if len(n.Children) < 3 {
			return
		}
		name := exprString(n.Children[0])
		params := params(n.Children[1])
		*out = append(*out, idt+"fun "+name+"("+params+") {")
		convertNode(n.Children[2], level+1, out, globals)
		*out = append(*out, idt+"}")
	case "class":
		if len(n.Children) < 3 {
			return
		}
		name := exprString(n.Children[0])
		*out = append(*out, idt+"type "+name+" {")
		convertNode(n.Children[2], level+1, out, globals)
		*out = append(*out, idt+"}")
	default:
		for _, c := range n.Children {
			convertNode(c, level, out, globals)
		}
	}
}

func snippetAround(src string, line int) string {
	lines := strings.Split(src, "\n")
	start := line - 2
	if start < 0 {
		start = 0
	}
	end := line
	if end >= len(lines) {
		end = len(lines) - 1
	}
	for i := start; i <= end; i++ {
		prefix := "   "
		if i+1 == line {
			prefix = ">>>"
		}
		lines[i] = fmt.Sprintf("%s %d: %s", prefix, i+1, lines[i])
	}
	return strings.Join(lines[start:end+1], "\n")
}

// ConvertSource converts a parsed Ruby node into Mochi source code.
func ConvertSource(n *Node) (string, error) {
	if n == nil {
		return "", fmt.Errorf("nil node")
	}
	var lines []string
	globals := make(map[string]bool)
	convertNode(*n, 0, &lines, globals)
	if len(lines) == 0 {
		return "", fmt.Errorf("no convertible statements")
	}
	code := strings.Join(lines, "\n")
	var b strings.Builder
	b.Write(meta.Header("//"))
	if n.Source != "" {
		b.WriteString("/*\n")
		b.WriteString(n.Source)
		if !strings.HasSuffix(n.Source, "\n") {
			b.WriteByte('\n')
		}
		b.WriteString("*/\n")
	}
	b.WriteString(code)
	return b.String(), nil
}

// Convert converts a parsed Ruby node into a Mochi AST node.
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

// ConvertFile reads the Ruby file at path and converts it.
func ConvertFile(path string) (*ast.Node, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	n, err := Parse(string(data))
	if err != nil {
		return nil, err
	}
	return Convert(n)
}
