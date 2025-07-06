package clj

import (
	any2mochi "mochi/tools/any2mochi"

	"bytes"
	"encoding/json"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"

	"mochi/tools/cljast"
)

type Program struct {
	Forms []form `json:"forms"`
}

type form struct {
	Type   string   `json:"type"`
	Name   string   `json:"name,omitempty"`
	Params []string `json:"params,omitempty"`
	Body   []node   `json:"body,omitempty"`
	Value  node     `json:"value,omitempty"`
	Line   int      `json:"line,omitempty"`
}

type node struct {
	Line int    `json:"line,omitempty"`
	Atom string `json:"atom,omitempty"`
	List []node `json:"list,omitempty"`
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

func diagnostics(src string, diags []any2mochi.Diagnostic) string {
	lines := strings.Split(src, "\n")
	var out strings.Builder
	for _, d := range diags {
		ln := int(d.Range.Start.Line)
		col := int(d.Range.Start.Character)
		msg := d.Message
		if ln >= len(lines) {
			out.WriteString(fmt.Sprintf("line %d:%d: %s\n", ln+1, col+1, msg))
			continue
		}
		out.WriteString(fmt.Sprintf("line %d:%d: %s\n", ln+1, col+1, msg))
		start := ln - 1
		if start < 0 {
			start = 0
		}
		end := ln + 1
		if end >= len(lines) {
			end = len(lines) - 1
		}
		for i := start; i <= end; i++ {
			out.WriteString(fmt.Sprintf("%3d | %s\n", i+1, lines[i]))
			if i == ln {
				pointer := strings.Repeat(" ", col) + "^"
				out.WriteString("    | " + pointer + "\n")
			}
		}
	}
	return strings.TrimSpace(out.String())
}

func repoRoot() (string, error) {
	dir, err := os.Getwd()
	if err != nil {
		return "", err
	}
	for i := 0; i < 10; i++ {
		if _, err := os.Stat(filepath.Join(dir, "go.mod")); err == nil {
			return dir, nil
		}
		parent := filepath.Dir(dir)
		if parent == dir {
			break
		}
		dir = parent
	}
	return "", os.ErrNotExist
}

// Convert converts Clojure source code to Mochi using the language server.
func Convert(src string) ([]byte, error) {
	if prog, err := parseCLI(src); err == nil {
		return programToMochi(prog, src)
	}
	ls := any2mochi.Servers["clj"]
	syms, diags, err := any2mochi.EnsureAndParse(ls.Command, ls.Args, ls.LangID, src)
	if err != nil {
		return nil, err
	}
	if len(diags) > 0 {
		return nil, fmt.Errorf("%s", diagnostics(src, diags))
	}
	var out strings.Builder
	writeCljSymbols(&out, nil, syms, ls, src)
	if out.Len() == 0 {
		return nil, fmt.Errorf("no convertible symbols found\n\nsource snippet:\n%s", snippet(src))
	}
	return []byte(out.String()), nil
}

func writeCljSymbols(out *strings.Builder, prefix []string, syms []any2mochi.DocumentSymbol, ls any2mochi.LanguageServer, src string) {
	for _, s := range syms {
		nameParts := prefix
		if s.Name != "" {
			nameParts = append(nameParts, s.Name)
		}
		switch s.Kind {
		case any2mochi.SymbolKindFunction, any2mochi.SymbolKindMethod:
			out.WriteString("fun ")
			out.WriteString(strings.Join(nameParts, "."))
			params, ret := parseParams(s.Detail)
			if len(params) == 0 || ret == "" {
				if hov, err := any2mochi.EnsureAndHover(ls.Command, ls.Args, ls.LangID, src, s.SelectionRange.Start); err == nil {
					p, r := parseHoverSignature(hov)
					if len(p) > 0 {
						params = p
					}
					if r != "" {
						ret = r
					}
				}
			}
			out.WriteByte('(')
			if len(params) > 0 {
				out.WriteString(strings.Join(params, ", "))
			}
			out.WriteByte(')')
			if ret != "" {
				out.WriteString(": ")
				out.WriteString(ret)
			}
			body := parseBody(src, s.Range)
			if len(body) == 0 {
				out.WriteString(" {}\n")
			} else {
				out.WriteString(" {\n")
				for _, stmt := range body {
					out.WriteString("  ")
					out.WriteString(stmt)
					out.WriteByte('\n')
				}
				out.WriteString("}\n")
			}
		case any2mochi.SymbolKindClass, any2mochi.SymbolKindStruct:
			out.WriteString("type ")
			out.WriteString(strings.Join(nameParts, "."))
			out.WriteString(" {}\n")
		case any2mochi.SymbolKindVariable, any2mochi.SymbolKindField, any2mochi.SymbolKindProperty, any2mochi.SymbolKindConstant:
			out.WriteString("var ")
			out.WriteString(strings.Join(nameParts, "."))
			out.WriteString("\n")
		case any2mochi.SymbolKindNamespace, any2mochi.SymbolKindModule, any2mochi.SymbolKindPackage:
			if len(s.Children) > 0 {
				writeCljSymbols(out, nameParts, s.Children, ls, src)
			}
			continue
		}
		if len(s.Children) > 0 {
			writeCljSymbols(out, nameParts, s.Children, ls, src)
		}
	}
}

func parseHoverParams(h any2mochi.Hover) []string {
	var text string
	switch c := h.Contents.(type) {
	case any2mochi.MarkupContent:
		text = c.Value
	case any2mochi.MarkedString:
		if b, err := json.Marshal(c); err == nil {
			var m any2mochi.MarkedStringStruct
			if json.Unmarshal(b, &m) == nil {
				text = m.Value
			} else {
				json.Unmarshal(b, &text)
			}
		}
	case []any2mochi.MarkedString:
		if len(c) > 0 {
			if b, err := json.Marshal(c[0]); err == nil {
				var m any2mochi.MarkedStringStruct
				if json.Unmarshal(b, &m) == nil {
					text = m.Value
				} else {
					json.Unmarshal(b, &text)
				}
			}
		}
	case string:
		text = c
	}
	for _, line := range strings.Split(text, "\n") {
		line = strings.TrimSpace(line)
		o := strings.Index(line, "[")
		cidx := strings.Index(line, "]")
		if o == -1 || cidx == -1 || cidx <= o {
			continue
		}
		list := strings.TrimSpace(line[o+1 : cidx])
		if list == "" {
			continue
		}
		fields := strings.Fields(list)
		params := make([]string, 0, len(fields))
		for _, f := range fields {
			if strings.HasPrefix(f, "&") {
				f = strings.TrimPrefix(f, "&")
			}
			if i := strings.IndexAny(f, ":"); i != -1 {
				f = f[:i]
			}
			params = append(params, f)
		}
		if len(params) > 0 {
			return params
		}
	}
	return nil
}

func parseHoverSignature(h any2mochi.Hover) ([]string, string) {
	var text string
	switch c := h.Contents.(type) {
	case any2mochi.MarkupContent:
		text = c.Value
	case any2mochi.MarkedString:
		if b, err := json.Marshal(c); err == nil {
			var m any2mochi.MarkedStringStruct
			if json.Unmarshal(b, &m) == nil {
				text = m.Value
			} else {
				json.Unmarshal(b, &text)
			}
		}
	case []any2mochi.MarkedString:
		if len(c) > 0 {
			if b, err := json.Marshal(c[0]); err == nil {
				var m any2mochi.MarkedStringStruct
				if json.Unmarshal(b, &m) == nil {
					text = m.Value
				} else {
					json.Unmarshal(b, &text)
				}
			}
		}
	case string:
		text = c
	}
	for _, line := range strings.Split(text, "\n") {
		line = strings.TrimSpace(line)
		o := strings.Index(line, "[")
		cidx := strings.Index(line, "]")
		if o == -1 || cidx == -1 || cidx <= o {
			continue
		}
		list := strings.TrimSpace(line[o+1 : cidx])
		params := []string{}
		if list != "" {
			fields := strings.Fields(list)
			for _, f := range fields {
				if strings.HasPrefix(f, "&") {
					f = strings.TrimPrefix(f, "&")
				}
				if i := strings.IndexAny(f, ":"); i != -1 {
					f = f[:i]
				}
				params = append(params, f)
			}
		}
		ret := strings.TrimSpace(line[cidx+1:])
		if strings.HasPrefix(ret, "->") {
			ret = strings.TrimSpace(strings.TrimPrefix(ret, "->"))
		}
		return params, ret
	}
	return nil, ""
}

func parseParams(detail *string) ([]string, string) {
	if detail == nil {
		return nil, ""
	}
	d := *detail
	start := strings.Index(d, "[")
	end := strings.Index(d, "]")
	if start == -1 || end == -1 || end <= start {
		return nil, ""
	}
	list := strings.TrimSpace(d[start+1 : end])
	if list == "" {
		return nil, ""
	}
	fields := strings.Fields(list)
	params := make([]string, 0, len(fields))
	for _, f := range fields {
		if strings.HasPrefix(f, "&") {
			f = strings.TrimPrefix(f, "&")
		}
		params = append(params, f)
	}
	ret := strings.TrimSpace(d[end+1:])
	if strings.HasPrefix(ret, "->") {
		ret = strings.TrimSpace(strings.TrimPrefix(ret, "->"))
	}
	return params, ret
}

// ConvertFile reads the Clojure file and converts it to Mochi.
func ConvertFile(path string) ([]byte, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	return Convert(string(data))
}

func parseCLI(src string) (*Program, error) {
	root, err := repoRoot()
	if err == nil {
		if _, err := exec.LookPath("clojure"); err == nil {
			tmp, err := os.CreateTemp("", "cljsrc_*.clj")
			if err != nil {
				return nil, err
			}
			if _, err := tmp.WriteString(src); err != nil {
				tmp.Close()
				os.Remove(tmp.Name())
				return nil, err
			}
			tmp.Close()
			defer os.Remove(tmp.Name())

			script := filepath.Join(root, "tools", "any2mochi", "x", "clj", "parse.clj")
			cmd := exec.Command("clojure", script, tmp.Name())
			var out bytes.Buffer
			var stderr bytes.Buffer
			cmd.Stdout = &out
			cmd.Stderr = &stderr
			if err := cmd.Run(); err == nil {
				var ast Program
				if json.Unmarshal(out.Bytes(), &ast) == nil {
					return &ast, nil
				}
			}
		}
	}

	a := cljast.Parse(src)
	prog := &Program{}
	for _, f := range a.Forms {
		prog.Forms = append(prog.Forms, form{
			Type:   f.Type,
			Name:   f.Name,
			Params: f.Params,
			Body:   f.Body,
			Value:  f.Value,
			Line:   f.Line,
		})
	}
	return prog, nil
}

func programToMochi(p *Program, src string) ([]byte, error) {
	var out strings.Builder
	for _, f := range p.Forms {
		switch f.Type {
		case "defn":
			out.WriteString("fun ")
			out.WriteString(f.Name)
			out.WriteByte('(')
			out.WriteString(strings.Join(f.Params, ", "))
			out.WriteByte(')')
			if len(f.Body) == 0 {
				out.WriteString(" {}\n")
			} else {
				out.WriteString(" {\n")
				for _, b := range f.Body {
					if s := cljToMochi(b); s != "" {
						out.WriteString("  ")
						out.WriteString(s)
						out.WriteByte('\n')
					}
				}
				out.WriteString("}\n")
			}
		case "def":
			out.WriteString("let ")
			out.WriteString(f.Name)
			if s := cljToMochi(f.Value); s != "" {
				out.WriteString(" = ")
				out.WriteString(s)
			}
			out.WriteByte('\n')
		case "expr":
			if len(f.Body) == 1 {
				if s := cljToMochi(f.Body[0]); s != "" {
					out.WriteString(s)
					out.WriteByte('\n')
				}
			}
		}
	}
	if out.Len() == 0 {
		return nil, fmt.Errorf("no convertible symbols found\n\nsource snippet:\n%s", snippet(src))
	}
	return []byte(out.String()), nil
}

// parseBody extracts and converts the body of a Clojure function defined
// within the given range of the source code. It performs a very small subset of
// Clojure parsing in pure Go and attempts to translate the forms into Mochi
// statements. Only a handful of constructs generated by the Mochi compiler are
// recognised. The implementation intentionally does not rely on regular
// expressions – it tokenises the source and builds a simple s-expression tree.
func parseBody(src string, r any2mochi.Range) []string {
	lines := strings.Split(src, "\n")
	if int(r.Start.Line) >= len(lines) || int(r.End.Line) >= len(lines) {
		return nil
	}
	var body strings.Builder
	for i := int(r.Start.Line); i <= int(r.End.Line); i++ {
		body.WriteString(lines[i])
		body.WriteByte('\n')
	}
	ast := cljast.Parse(body.String())
	if len(ast.Forms) == 1 && ast.Forms[0].Type == "defn" {
		ast.Forms[0].Body = ast.Forms[0].Body
	}
	var stmts []string
	for _, f := range ast.Forms {
		switch f.Type {
		case "expr":
			if len(f.Body) == 1 {
				if s := cljToMochi(f.Body[0]); s != "" {
					stmts = append(stmts, s)
				}
			}
		case "def":
			if s := cljToMochi(f.Value); s != "" {
				stmts = append(stmts, fmt.Sprintf("let %s = %s", f.Name, s))
			}
		case "defn":
			// ignore nested function definitions
		}
	}
	return stmts
}

// cljToMochi converts a parsed s-expression into a Mochi statement or
// expression. Only a subset of constructs are supported.
func cljToMochi(n node) string {
	if n.Atom != "" && len(n.List) == 0 {
		return n.Atom
	}
	if len(n.List) == 0 {
		return ""
	}
	head := n.List[0].Atom
	switch head {
	case "println":
		var args []string
		for _, a := range n.List[1:] {
			args = append(args, cljToMochi(a))
		}
		return fmt.Sprintf("print(%s)", strings.Join(args, ", "))
	case "def":
		if len(n.List) >= 3 {
			return fmt.Sprintf("let %s = %s", cljToMochi(n.List[1]), cljToMochi(n.List[2]))
		}
	case "+", "-", "*", "/", "mod", "quot":
		if len(n.List) == 3 {
			op := head
			if op == "mod" {
				op = "%"
			} else if op == "quot" {
				op = "/"
			}
			return fmt.Sprintf("%s %s %s", cljToMochi(n.List[1]), op, cljToMochi(n.List[2]))
		}
	case "throw":
		if len(n.List) == 2 {
			l2 := n.List[1]
			if len(l2.List) >= 3 && l2.List[0].Atom == "ex-info" {
				if strings.Trim(l2.List[1].Atom, "\"") == "return" {
					mp := l2.List[2]
					if len(mp.List) >= 2 && mp.List[0].Atom == ":value" {
						return fmt.Sprintf("return %s", cljToMochi(mp.List[1]))
					}
				}
			}
		}
	case "vec":
		if len(n.List) == 2 {
			if s := parseDataset(n.List[1]); s != "" {
				return s
			}
		}
	case "->>":
		if s := parseDataset(n); s != "" {
			return s
		}
	case "loop":
		if s := parseLoop(n); s != "" {
			return s
		}
	default:
		var args []string
		for _, a := range n.List[1:] {
			args = append(args, cljToMochi(a))
		}
		return fmt.Sprintf("%s(%s)", head, strings.Join(args, ", "))
	}
	return ""
}

// parseDataset converts pipeline expressions built with `for` and `->>` into
// Mochi query comprehensions. Only a limited subset of constructs emitted by the
// Mochi compiler are recognised.
func parseDataset(n node) string {
	if len(n.List) == 0 {
		return ""
	}
	if n.List[0].Atom == "->>" {
		parts := n.List[1:]
		if len(parts) == 0 {
			return ""
		}
		forExpr := parts[0]
		if len(forExpr.List) < 3 || forExpr.List[0].Atom != "for" {
			return ""
		}
		bind := forExpr.List[1]
		if len(bind.List) < 2 {
			return ""
		}
		var1 := bind.List[0].Atom
		coll1 := cljToMochi(bind.List[1])
		out := fmt.Sprintf("from %s in %s", var1, coll1)
		idx := 2
		if idx < len(bind.List) && bind.List[idx].Atom == ":when" && idx+1 < len(bind.List) {
			cond := cljToMochi(bind.List[idx+1])
			out += " where " + cond
			idx += 2
		}
		if idx+1 < len(bind.List) {
			var2 := bind.List[idx].Atom
			coll2 := cljToMochi(bind.List[idx+1])
			out += fmt.Sprintf(" join from %s in %s", var2, coll2)
			idx += 2
			if idx < len(bind.List) && bind.List[idx].Atom == ":when" && idx+1 < len(bind.List) {
				cond := cljToMochi(bind.List[idx+1])
				out += " on " + cond
			}
		}
		body := cljToMochi(forExpr.List[2])
		out += " select " + body
		for _, op := range parts[1:] {
			if len(op.List) == 0 {
				continue
			}
			switch op.List[0].Atom {
			case "sort-by":
				if len(op.List) >= 2 {
					out += "\n                sort by " + cljToMochi(op.List[1])
				}
			case "drop":
				if len(op.List) == 2 {
					out += "\n                skip " + cljToMochi(op.List[1])
				}
			case "take":
				if len(op.List) == 2 {
					out += "\n                take " + cljToMochi(op.List[1])
				}
			}
		}
		return out
	}
	return ""
}

// parseLoop converts the compiler-generated loop/try/cond construct into a
// simple 'for' statement.
func parseLoop(n node) string {
	if len(n.List) < 3 || n.List[0].Atom != "loop" {
		return ""
	}
	bind := n.List[1]
	if len(bind.List) != 2 {
		return ""
	}
	seq := bind.List[1]
	if len(seq.List) != 2 || seq.List[0].Atom != "seq" {
		return ""
	}
	coll := cljToMochi(seq.List[1])
	body := n.List[2]
	if len(body.List) < 3 || body.List[0].Atom != "when" {
		return ""
	}
	itemLet := body.List[2]
	if len(itemLet.List) < 3 || itemLet.List[0].Atom != "let" {
		return ""
	}
	itemVar := itemLet.List[1].List[0].Atom
	tryLet := itemLet.List[2]
	if len(tryLet.List) < 3 || tryLet.List[0].Atom != "let" {
		return ""
	}
	tryExpr := tryLet.List[2]
	if len(tryExpr.List) < 2 || tryExpr.List[0].Atom != "try" {
		return ""
	}
	stmt := cljToMochi(tryExpr.List[1])
	return fmt.Sprintf("for %s in %s {\n  %s\n}", itemVar, coll, stmt)
}
