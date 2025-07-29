//go:build slow

package ruby

import (
        "bytes"
        "encoding/json"
        "fmt"
       "os"
        "os/exec"
        "strconv"
        "strings"
)

// Node represents a Ruby AST node produced by ripper.
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

// Parse converts Ruby source code into a Node tree using ripper.
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
			for i := 0; i < len(msg); i++ {
				if msg[i] < '0' || msg[i] > '9' {
					continue
				}
				j := i
				for j < len(msg) && msg[j] >= '0' && msg[j] <= '9' {
					j++
				}
				if j < len(msg) && msg[j] == ':' {
					if n, err := strconv.Atoi(msg[i:j]); err == nil {
						line = n
					}
					break
				}
				i = j
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

// ParseFile reads Ruby source code from a file and parses it using Parse.
func ParseFile(path string) (*Node, error) {
       data, err := os.ReadFile(path)
       if err != nil {
               return nil, err
       }
       return Parse(string(data))
}

func buildNode(v any) Node {
	if s, ok := v.(string); ok {
		return Node{Type: "@tok", Value: s}
	}
	arr, ok := v.([]any)
	if !ok || len(arr) == 0 {
		return Node{}
	}
	typeStr, ok := arr[0].(string)
	if !ok {
		n := Node{Type: "list"}
		for _, x := range arr {
			c := buildNode(x)
			if c.Type != "" {
				n.Children = append(n.Children, c)
			}
		}
		return n
	}
	n := Node{Type: typeStr}
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
	if (n.Type == "aref" || n.Type == "aref_field") && len(n.Children) >= 2 {
		recv := exprString(n.Children[0])
		idx := n.Children[1]
		if idx.Type == "args_add_block" && len(idx.Children) > 0 {
			idx = idx.Children[0]
		}
		idx = firstArg(idx)
		if (idx.Type == "dot2" || idx.Type == "dot3") && len(idx.Children) == 2 {
			start := exprString(idx.Children[0])
			end := exprString(idx.Children[1])
			return []string{fmt.Sprintf("%s[%s:%s]", recv, start, end)}
		}
		return []string{recv + "[" + exprString(idx) + "]"}
	}
	if n.Type == "unary" && len(n.Children) == 2 {
		op := exprString(n.Children[0])
		val := exprString(n.Children[1])
		if op == "-" {
			return []string{"(" + op + val + ")"}
		}
		return []string{op + val}
	}
	if (n.Type == "dot2" || n.Type == "dot3") && len(n.Children) == 2 {
		left := exprString(n.Children[0])
		right := exprString(n.Children[1])
		return []string{left + ".." + right}
	}
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
			idx = firstArg(idx)
			if (idx.Type == "dot2" || idx.Type == "dot3") && len(idx.Children) == 2 {
				start := exprString(idx.Children[0])
				end := exprString(idx.Children[1])
				return fmt.Sprintf("%s[%s:%s]", recv, start, end)
			}
			return recv + "[" + exprString(idx) + "]"
		}
	case "arg_paren", "args_add_block":
		if len(n.Children) > 0 {
			return exprString(n.Children[0])
		}
	case "args_add":
		var parts []string
		for _, c := range n.Children {
			if c.Type == "args_new" {
				continue
			}
			parts = append(parts, exprString(c))
		}
		return strings.Join(parts, ", ")
	case "dot2", "dot3":
		if len(n.Children) == 2 {
			left := exprString(n.Children[0])
			right := exprString(n.Children[1])
			return left + ".." + right
		}
	case "call":
		if len(n.Children) >= 3 {
			recv := exprString(n.Children[0])
			meth := n.Children[2]
			if meth.Type == "@ident" && meth.Value == "length" {
				return "len(" + recv + ")"
			}
			if meth.Type == "@ident" && meth.Value == "sum" {
				return "sum(" + recv + ")"
			}
		}
	case "method_add_arg":
		if len(n.Children) == 2 {
			call := n.Children[0]
			args := n.Children[1]
			if call.Type == "call" && len(call.Children) >= 3 {
				recv := exprString(call.Children[0])
				meth := call.Children[2]
				if meth.Type == "@ident" {
					if meth.Value == "include?" || meth.Value == "key?" {
						arg := args
						if arg.Type == "arg_paren" && len(arg.Children) > 0 {
							arg = arg.Children[0]
						}
						if arg.Type == "args_add_block" && len(arg.Children) > 0 {
							arg = arg.Children[0]
						}
						return exprString(arg) + " in " + recv
					}
					if meth.Value == "call" {
						a := exprString(args)
						return recv + "(" + a + ")"
					}
				}
			}
			if call.Type == "fcall" && len(call.Children) > 0 {
				nameNode := call.Children[0]
				if nameNode.Type == "@ident" {
					return nameNode.Value + "(" + exprString(args) + ")"
				}
			}
		}
	case "lambda":
		if len(n.Children) >= 2 {
			ps := params(n.Children[0])
			var typed []string
			if ps != "" {
				for _, p := range strings.Split(ps, ", ") {
					typed = append(typed, p+": any")
				}
			}
			body := exprString(n.Children[len(n.Children)-1])
			return fmt.Sprintf("fun(%s): any => (%s)", strings.Join(typed, ", "), body)
		}
	case "ifop":
		if len(n.Children) >= 3 {
			cond := exprString(n.Children[0])
			t := exprString(n.Children[1])
			e := exprString(n.Children[2])
			return fmt.Sprintf("if %s then %s else %s", cond, t, e)
		}
	}
	if n.Type == "paren" && len(n.Children) == 1 {
		return "(" + exprString(n.Children[0]) + ")"
	}
	if n.Type == "unary" && len(n.Children) == 2 {
		op := exprString(n.Children[0])
		val := exprString(n.Children[1])
		if op == "-" {
			return "(" + op + val + ")"
		}
		return op + val
	}
	toks := tokens(n)
	if len(toks) == 0 {
		return ""
	}
	s := strings.Join(toks, " ")
	reps := []struct{ old, new string }{{" ( ", "("}, {" )", ")"}, {"( ", "("}, {" , ", ", "}, {" [ ", "["}, {" ] ", "]"}, {" . ", "."}, {"- ", "-"}}
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
		if n.Value != "" {
			allDigits := true
			for _, r := range n.Value {
				if r < '0' || r > '9' {
					allDigits = false
					break
				}
			}
			if allDigits {
				return n.Value, true
			}
		}
	}
	return "", false
}

// firstArg unwraps nested argument nodes and returns the first real argument.
func firstArg(n Node) Node {
	for {
		switch n.Type {
		case "arg_paren", "args_add_block", "args_new":
			if len(n.Children) > 0 {
				n = n.Children[0]
				continue
			}
		case "args_add", "stmts_add":
			if len(n.Children) > 0 {
				n = n.Children[len(n.Children)-1]
				continue
			}
		case "paren", "stmts_new":
			if len(n.Children) > 0 {
				n = n.Children[0]
				continue
			}
		}
		return n
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
