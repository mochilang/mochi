package fs

import (
	"encoding/json"
	"fmt"
	"os"
	"regexp"
	"strings"

	parent "mochi/tools/any2mochi"
)

// Convert translates F# source into Mochi code using the language server when
// available. It falls back to a very small regex based parser when the server
// is unavailable.
func Convert(src string) ([]byte, error) {
	ast, err := Parse(src)
	if err != nil {
		return nil, err
	}
	if ast != nil && len(ast.Stmts) > 0 {
		var out strings.Builder
		writeStmts(&out, ast.Stmts, 0)
		return []byte(out.String()), nil
	}
	if ast != nil && (len(ast.Vars) > 0 || len(ast.Prints) > 0) {
		var out strings.Builder
		for _, v := range ast.Vars {
			out.WriteString("let ")
			out.WriteString(v.Name)
			out.WriteString(" = ")
			out.WriteString(v.Expr)
			out.WriteByte('\n')
		}
		for _, p := range ast.Prints {
			out.WriteString("print(")
			out.WriteString(p)
			out.WriteString(")\n")
		}
		return []byte(out.String()), nil
	}

	// Fallback to simple regex based conversion if parsing failed.
	if out, ferr := fallback(src); ferr == nil {
		return out, nil
	}
	return nil, fmt.Errorf("unsupported")
}

// ConvertFile reads the F# file at path and converts it to Mochi.
func ConvertFile(path string) ([]byte, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	return Convert(string(data))
}

type fsParam struct {
	name string
	typ  string
}

func writeFsSymbols(out *strings.Builder, prefix []string, syms []parent.DocumentSymbol, src string, ls parent.LanguageServer) {
	for _, s := range syms {
		nameParts := prefix
		if s.Name != "" {
			nameParts = append(nameParts, s.Name)
		}
		switch s.Kind {
		case parent.SymbolKindFunction, parent.SymbolKindMethod:
			params, ret := parseDetail(s.Detail)
			if len(params) == 0 && ret == "" {
				if hov, err := parent.EnsureAndHover(ls.Command, ls.Args, ls.LangID, src, s.SelectionRange.Start); err == nil {
					params, ret = parseHover(hov)
				}
			}
			out.WriteString("fun ")
			out.WriteString(strings.Join(nameParts, "."))
			out.WriteByte('(')
			for i, p := range params {
				if i > 0 {
					out.WriteString(", ")
				}
				out.WriteString(p.name)
			}
			out.WriteByte(')')
			if t := mapType(ret); t != "" {
				out.WriteString(": ")
				out.WriteString(t)
			}
			body := functionBody(src, s)
			if len(body) == 0 {
				out.WriteString(" {}\n")
			} else {
				out.WriteString(" {\n")
				for _, ln := range body {
					out.WriteString("  ")
					out.WriteString(ln)
					out.WriteByte('\n')
				}
				out.WriteString("}\n")
			}
		case parent.SymbolKindVariable, parent.SymbolKindConstant, parent.SymbolKindField:
			out.WriteString("let ")
			out.WriteString(strings.Join(nameParts, "."))
			out.WriteByte('\n')
		case parent.SymbolKindStruct, parent.SymbolKindClass, parent.SymbolKindInterface:
			out.WriteString("type ")
			out.WriteString(strings.Join(nameParts, "."))
			out.WriteString(" {}\n")
		}
		if len(s.Children) > 0 {
			writeFsSymbols(out, nameParts, s.Children, src, ls)
		}
	}
}

func parseDetail(detail *string) ([]fsParam, string) {
	if detail == nil {
		return nil, ""
	}
	d := strings.TrimSpace(*detail)
	if d == "" {
		return nil, ""
	}
	if strings.HasPrefix(d, "val") {
		if idx := strings.Index(d, ":"); idx != -1 {
			d = strings.TrimSpace(d[idx+1:])
		}
	}
	parts := strings.Split(d, "->")
	for i := range parts {
		parts[i] = strings.TrimSpace(parts[i])
	}
	ret := ""
	if len(parts) > 0 {
		ret = parts[len(parts)-1]
	}
	params := make([]fsParam, 0, len(parts)-1)
	for _, p := range parts[:len(parts)-1] {
		name := ""
		if idx := strings.Index(p, ":"); idx != -1 {
			name = strings.TrimSpace(p[:idx])
		} else {
			fields := strings.Fields(p)
			if len(fields) > 0 {
				name = fields[len(fields)-1]
			}
		}
		if name == "" {
			name = fmt.Sprintf("p%d", len(params))
		}
		params = append(params, fsParam{name: name})
	}
	return params, ret
}

func parseHover(h parent.Hover) ([]fsParam, string) {
	text := hoverString(h)
	for _, line := range strings.Split(text, "\n") {
		line = strings.TrimSpace(line)
		if strings.Contains(line, "->") {
			if p, r := parseDetail(&line); len(p) > 0 || r != "" {
				return p, r
			}
		}
	}
	return nil, ""
}

func mapType(t string) string {
	t = strings.TrimSpace(t)
	switch t {
	case "", "unit":
		return ""
	case "int", "int32", "int64", "uint32", "uint64":
		return "int"
	case "float", "double", "decimal":
		return "float"
	case "bool":
		return "bool"
	case "string":
		return "string"
	}
	if strings.HasSuffix(t, " list") {
		inner := mapType(strings.TrimSuffix(t, " list"))
		if inner == "" {
			inner = "any"
		}
		return "list<" + inner + ">"
	}
	if strings.HasSuffix(t, " array") {
		inner := mapType(strings.TrimSuffix(t, " array"))
		if inner == "" {
			inner = "any"
		}
		return "list<" + inner + ">"
	}
	return ""
}

var indexRe = regexp.MustCompile(`(\w+)\.\[(.+)\]`)

func fixIndex(expr string) string {
	return indexRe.ReplaceAllString(expr, `$1[$2]`)
}

func functionBody(src string, sym parent.DocumentSymbol) []string {
	lines := strings.Split(src, "\n")
	start := posToOffset(lines, sym.Range.Start)
	end := posToOffset(lines, sym.Range.End)
	if start >= len(src) || end > len(src) || start >= end {
		return nil
	}
	snippet := src[start:end]
	key := "raise (Return_" + sym.Name + " ("
	idx := strings.Index(snippet, key)
	if idx == -1 {
		return nil
	}
	exprStart := idx + len(key)
	depth := 1
	i := exprStart
	for i < len(snippet) && depth > 0 {
		switch snippet[i] {
		case '(':
			depth++
		case ')':
			depth--
		}
		i++
	}
	if depth != 0 {
		return nil
	}
	expr := strings.TrimSpace(snippet[exprStart : i-1])
	return []string{"return " + expr}
}

func posToOffset(lines []string, pos parent.Position) int {
	off := 0
	for i := 0; i < int(pos.Line); i++ {
		if i < len(lines) {
			off += len(lines[i]) + 1
		}
	}
	off += int(pos.Character)
	return off
}

func hoverString(h parent.Hover) string {
	switch v := h.Contents.(type) {
	case parent.MarkupContent:
		return v.Value
	case parent.MarkedString:
		b, err := json.Marshal(v)
		if err == nil {
			var ms parent.MarkedStringStruct
			if err := json.Unmarshal(b, &ms); err == nil {
				if ms.Value != "" {
					return ms.Value
				}
			}
			var s string
			if err := json.Unmarshal(b, &s); err == nil {
				return s
			}
		}
		return ""
	case []parent.MarkedString:
		parts := make([]string, 0, len(v))
		for _, m := range v {
			parts = append(parts, hoverString(parent.Hover{Contents: m}))
		}
		return strings.Join(parts, "\n")
	case string:
		return v
	default:
		return fmt.Sprint(v)
	}
}

func writeStmts(out *strings.Builder, stmts []Stmt, indent int) {
	idt := strings.Repeat("  ", indent)
	for _, s := range stmts {
		switch v := s.(type) {
		case Var:
			kw := "let "
			if v.Mutable {
				kw = "var "
			}
			out.WriteString(idt + kw + v.Name)
			if v.Type != "" {
				if t := mapType(v.Type); t != "" {
					out.WriteString(": " + t)
				}
			}
			if v.Expr != "" {
				out.WriteString(" = " + fixIndex(v.Expr))
			}
			out.WriteByte('\n')
		case Assign:
			lhs := v.Name
			if v.Index != "" {
				lhs += "[" + fixIndex(v.Index) + "]"
			}
			out.WriteString(idt + lhs + " = " + fixIndex(v.Expr) + "\n")
		case Print:
			out.WriteString(idt + "print(" + fixIndex(v.Expr) + ")\n")
		case Expect:
			out.WriteString(idt + "expect(" + v.Cond + ")\n")
		case ForRange:
			out.WriteString(idt + "for " + v.Var + " in " + v.Start + ".." + v.End + " {\n")
			writeStmts(out, v.Body, indent+1)
			out.WriteString(idt + "}\n")
		case ForIn:
			out.WriteString(idt + "for " + v.Var + " in " + v.Expr + " {\n")
			writeStmts(out, v.Body, indent+1)
			out.WriteString(idt + "}\n")
		case While:
			out.WriteString(idt + "while " + v.Cond + " {\n")
			writeStmts(out, v.Body, indent+1)
			out.WriteString(idt + "}\n")
		case TypeDecl:
			if len(v.Fields) > 0 {
				out.WriteString(idt + "type " + v.Name + " {\n")
				for _, f := range v.Fields {
					typ := mapType(f.Type)
					if typ == "" {
						typ = "any"
					}
					out.WriteString(idt + "  " + f.Name + ": " + typ + "\n")
				}
				out.WriteString(idt + "}\n")
			} else if len(v.Variants) > 0 {
				out.WriteString(idt + "type " + v.Name + " =\n")
				for i, vr := range v.Variants {
					sep := "  "
					if i > 0 {
						sep = "  | "
					}
					out.WriteString(idt + sep + vr.Name)
					if len(vr.Fields) > 0 {
						out.WriteByte('(')
						for j, f := range vr.Fields {
							if j > 0 {
								out.WriteString(", ")
							}
							typ := mapType(f.Type)
							if typ == "" {
								typ = "any"
							}
							out.WriteString(f.Name + ": " + typ)
						}
						out.WriteByte(')')
					}
					out.WriteByte('\n')
				}
			} else {
				out.WriteString(idt + "type " + v.Name + " {}\n")
			}
		}
	}
}

func fallback(src string) ([]byte, error) {
	for _, line := range strings.Split(src, "\n") {
		l := strings.TrimSpace(line)
		if strings.HasPrefix(l, "ignore (printfn") {
			open := strings.Index(l, "(")
			close := strings.LastIndex(l, ")")
			if open != -1 && close != -1 && close > open {
				expr := strings.TrimSpace(l[open+1 : close])
				parts := strings.Split(expr, ",")
				if len(parts) > 1 {
					expr = strings.Join(parts[1:], ",")
				}
				return []byte("print(" + strings.TrimSpace(expr) + ")\n"), nil
			}
		}
	}
	return nil, fmt.Errorf("unsupported")
}
