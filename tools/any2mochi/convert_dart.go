package any2mochi

import (
	"bytes"
	"encoding/json"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"regexp"
	"strings"
	"unicode"

	protocol "github.com/tliron/glsp/protocol_3_16"
)

// ConvertDart converts dart source code to Mochi.
func ConvertDart(src string) ([]byte, error) {
	if out, err := convertDartCLI(src); err == nil {
		return out, nil
	}
	return convertDartFallback(src)
}

// ConvertDartFile reads the dart file and converts it to Mochi.
func ConvertDartFile(path string) ([]byte, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	return ConvertDart(string(data))
}

type dartParam struct {
	name string
	typ  string
}

func parseDartDetail(detail string) ([]dartParam, string) {
	open := strings.Index(detail, "(")
	close := strings.LastIndex(detail, ")")
	if open == -1 || close == -1 || close < open {
		return nil, ""
	}
	paramsPart := detail[open+1 : close]
	retPart := strings.TrimSpace(detail[close+1:])
	if i := strings.Index(retPart, "→"); i != -1 {
		retPart = strings.TrimSpace(retPart[i+len("→"):])
	} else if i := strings.Index(retPart, "->"); i != -1 {
		retPart = strings.TrimSpace(retPart[i+2:])
	}
	var params []dartParam
	for _, p := range strings.Split(paramsPart, ",") {
		p = strings.TrimSpace(strings.Trim(p, "{}[]"))
		if p == "" {
			continue
		}
		p = strings.TrimPrefix(p, "required ")
		if eq := strings.Index(p, "="); eq != -1 {
			p = p[:eq]
		}
		fields := strings.FieldsFunc(p, func(r rune) bool { return unicode.IsSpace(r) || r == ':' })
		if len(fields) == 0 {
			continue
		}
		name := fields[len(fields)-1]
		if strings.HasPrefix(name, "this.") {
			name = strings.TrimPrefix(name, "this.")
		}
		typ := ""
		if len(fields) > 1 {
			typ = dartToMochiType(strings.Join(fields[:len(fields)-1], " "))
		}
		params = append(params, dartParam{name: name, typ: typ})
	}
	return params, dartToMochiType(retPart)
}

func parseDartHover(h protocol.Hover) ([]dartParam, string) {
	text := hoverString(h)
	for _, line := range strings.Split(text, "\n") {
		line = strings.TrimSpace(line)
		if strings.Contains(line, "(") && strings.Contains(line, ")") {
			if p, r := parseDartDetail(line); len(p) > 0 || r != "" {
				return p, r
			}
		}
	}
	return nil, ""
}

func writeDartSymbols(out *strings.Builder, prefix []string, syms []protocol.DocumentSymbol, src string, ls LanguageServer) {
	for _, s := range syms {
		nameParts := prefix
		if s.Name != "" {
			nameParts = append(nameParts, s.Name)
		}

		switch s.Kind {
		case protocol.SymbolKindClass, protocol.SymbolKindInterface, protocol.SymbolKindStruct:
			out.WriteString("type ")
			out.WriteString(strings.Join(nameParts, "."))
			out.WriteString(" {\n")
			for _, c := range s.Children {
				if c.Kind == protocol.SymbolKindField || c.Kind == protocol.SymbolKindProperty || c.Kind == protocol.SymbolKindVariable {
					out.WriteString("  ")
					out.WriteString(c.Name)
					if t := dartFieldType(src, c, ls); t != "" {
						out.WriteString(": ")
						out.WriteString(t)
					}
					out.WriteByte('\n')
				}
			}
			out.WriteString("}\n")
			for _, c := range s.Children {
				if c.Kind == protocol.SymbolKindMethod || c.Kind == protocol.SymbolKindConstructor || c.Kind == protocol.SymbolKindFunction {
					writeDartSymbols(out, nameParts, []protocol.DocumentSymbol{c}, src, ls)
				}
			}
			continue
		case protocol.SymbolKindVariable, protocol.SymbolKindConstant:
			if len(prefix) == 0 && s.Name != "" {
				out.WriteString("let ")
				out.WriteString(s.Name)
				if t := dartFieldType(src, s, ls); t != "" {
					out.WriteString(": ")
					out.WriteString(t)
				}
				out.WriteByte('\n')
			}
		case protocol.SymbolKindEnum:
			out.WriteString("type ")
			out.WriteString(strings.Join(nameParts, "."))
			out.WriteString(" {\n")
			for _, c := range s.Children {
				if c.Kind == protocol.SymbolKindEnumMember {
					fmt.Fprintf(out, "  %s\n", c.Name)
				}
			}
			out.WriteString("}\n")
			var rest []protocol.DocumentSymbol
			for _, c := range s.Children {
				if c.Kind != protocol.SymbolKindEnumMember {
					rest = append(rest, c)
				}
			}
			if len(rest) > 0 {
				writeDartSymbols(out, nameParts, rest, src, ls)
			}
			continue
		case protocol.SymbolKindFunction, protocol.SymbolKindMethod, protocol.SymbolKindConstructor:
			out.WriteString("fun ")
			out.WriteString(strings.Join(nameParts, "."))
			detail := ""
			if s.Detail != nil {
				detail = *s.Detail
			}
			params, ret := parseDartDetail(detail)
			if len(params) == 0 && ret == "" {
				if hov, err := EnsureAndHover(ls.Command, ls.Args, ls.LangID, src, s.SelectionRange.Start); err == nil {
					if p, r := parseDartHover(hov); len(p) > 0 || r != "" {
						params, ret = p, r
					}
				}
			}
			out.WriteByte('(')
			for i, p := range params {
				if i > 0 {
					out.WriteString(", ")
				}
				out.WriteString(p.name)
				if p.typ != "" && p.typ != "any" {
					out.WriteString(": ")
					out.WriteString(p.typ)
				}
			}
			out.WriteByte(')')
			if ret != "" && ret != "any" {
				out.WriteString(": ")
				out.WriteString(ret)
			}
			body := parseDartFunctionBody(src, s)
			if len(body) == 0 {
				out.WriteString(" {}\n")
			} else {
				out.WriteString(" {\n")
				for _, line := range body {
					out.WriteString(line)
					out.WriteByte('\n')
				}
				out.WriteString("}\n")
			}
		}

		if len(s.Children) > 0 {
			writeDartSymbols(out, nameParts, s.Children, src, ls)
		}
	}
}

func dartTypeFromDetail(d *string) string {
	if d == nil {
		return ""
	}
	return dartToMochiType(strings.TrimSpace(*d))
}

func dartFieldType(src string, sym protocol.DocumentSymbol, ls LanguageServer) string {
	if t := dartTypeFromDetail(sym.Detail); t != "" && t != "any" {
		return t
	}
	return dartTypeFromHover(src, sym, ls)
}

func dartTypeFromHover(src string, sym protocol.DocumentSymbol, ls LanguageServer) string {
	hov, err := EnsureAndHover(ls.Command, ls.Args, ls.LangID, src, sym.SelectionRange.Start)
	if err != nil {
		return ""
	}
	text := hoverString(hov)
	for _, line := range strings.Split(text, "\n") {
		l := strings.TrimSpace(line)
		if idx := strings.Index(l, ":"); idx != -1 {
			t := strings.TrimSpace(l[idx+1:])
			if t != "" {
				return dartToMochiType(t)
			}
		}
		if idx := strings.Index(l, "→"); idx != -1 {
			t := strings.TrimSpace(l[idx+len("→"):])
			if t != "" {
				return dartToMochiType(t)
			}
		} else if idx := strings.Index(l, "->"); idx != -1 {
			t := strings.TrimSpace(l[idx+2:])
			if t != "" {
				return dartToMochiType(t)
			}
		}
	}
	return ""
}

func splitDartArgs(s string) []string {
	var parts []string
	depth := 0
	start := 0
	for i, r := range s {
		switch r {
		case '<':
			depth++
		case '>':
			if depth > 0 {
				depth--
			}
		case ',':
			if depth == 0 {
				parts = append(parts, strings.TrimSpace(s[start:i]))
				start = i + 1
			}
		}
	}
	if start < len(s) {
		parts = append(parts, strings.TrimSpace(s[start:]))
	}
	return parts
}

func dartToMochiType(t string) string {
	t = strings.TrimSpace(t)
	if strings.HasSuffix(t, "?") {
		t = strings.TrimSuffix(t, "?")
	}
	switch t {
	case "", "dynamic", "Object":
		return "any"
	case "int":
		return "int"
	case "double", "num":
		return "float"
	case "bool":
		return "bool"
	case "String":
		return "string"
	case "void":
		return ""
	}
	if strings.HasSuffix(t, ">") {
		if open := strings.Index(t, "<"); open != -1 {
			outer := strings.TrimSpace(t[:open])
			inner := strings.TrimSuffix(t[open+1:], ">")
			args := splitDartArgs(inner)
			switch outer {
			case "List", "Iterable", "Set":
				a := "any"
				if len(args) > 0 {
					if at := dartToMochiType(args[0]); at != "" {
						a = at
					}
				}
				return "list<" + a + ">"
			case "Map":
				if len(args) == 2 {
					k := dartToMochiType(args[0])
					if k == "" {
						k = "any"
					}
					v := dartToMochiType(args[1])
					if v == "" {
						v = "any"
					}
					return "map<" + k + ", " + v + ">"
				}
			case "Future":
				if len(args) == 1 {
					return dartToMochiType(args[0])
				}
			}
		}
	}
	return t
}

// convertDartFallback performs a best-effort conversion using simple regex
// parsing when no language server is available. Only a small subset of Dart is
// supported.
func convertDartFallback(src string) ([]byte, error) {
	stmts := dartSplitStatements(src)
	var out strings.Builder
	indent := 0
	ind := func() string { return strings.Repeat("  ", indent) }

	funcRE := regexp.MustCompile(`^(?:[A-Za-z0-9_<>,\[\]\?]+\s+)?([A-Za-z_][A-Za-z0-9_]*)\s*\(([^)]*)\)$`)
	fieldRE := regexp.MustCompile(`^(?:final\s+|var\s+)?([A-Za-z0-9_<>,\[\]\?]+)\s+(\w+)`)

	parseParams := func(s string) []string {
		var params []string
		for _, p := range strings.Split(s, ",") {
			p = strings.TrimSpace(p)
			p = strings.TrimPrefix(p, "required ")
			if eq := strings.Index(p, "="); eq != -1 {
				p = p[:eq]
			}
			fields := strings.Fields(p)
			if len(fields) == 0 {
				continue
			}
			name := fields[len(fields)-1]
			if strings.HasPrefix(name, "this.") {
				name = strings.TrimPrefix(name, "this.")
			}
			params = append(params, name)
		}
		return params
	}

	for i := 0; i < len(stmts); i++ {
		s := strings.TrimSpace(stmts[i])
		if s == "" {
			continue
		}
		switch s {
		case "{":
			out.WriteString(ind())
			out.WriteString("{\n")
			indent++
			continue
		case "}":
			if indent > 0 {
				indent--
			}
			out.WriteString(ind())
			out.WriteString("}\n")
			continue
		}

		nextBlock := i+1 < len(stmts) && stmts[i+1] == "{"

		if nextBlock && strings.HasPrefix(s, "class ") {
			name := strings.TrimSpace(strings.TrimPrefix(s, "class "))
			out.WriteString(ind())
			out.WriteString("type ")
			out.WriteString(name)
			out.WriteString(" {\n")
			indent++
			i += 2 // skip "{" token
			for i < len(stmts) {
				line := strings.TrimSpace(stmts[i])
				if line == "}" {
					indent--
					out.WriteString(ind())
					out.WriteString("}\n")
					break
				}
				if m := fieldRE.FindStringSubmatch(line); m != nil {
					out.WriteString(ind())
					out.WriteString(m[2])
					if typ := dartToMochiType(m[1]); typ != "" && typ != "any" {
						out.WriteString(": ")
						out.WriteString(typ)
					}
					out.WriteByte('\n')
				}
				i++
			}
			continue
		}

		if nextBlock && funcRE.MatchString(s) {
			m := funcRE.FindStringSubmatch(s)
			out.WriteString(ind())
			out.WriteString("fun ")
			out.WriteString(m[1])
			params := parseParams(m[2])
			out.WriteByte('(')
			for j, p := range params {
				if j > 0 {
					out.WriteString(", ")
				}
				out.WriteString(p)
			}
			out.WriteString(") {\n")
			indent++
			continue
		}

		line := convertDartStmt(s)
		if line != "" {
			out.WriteString(ind())
			out.WriteString(line)
			out.WriteByte('\n')
		}
	}
	if out.Len() == 0 {
		return nil, fmt.Errorf("no convertible symbols found\n\nsource snippet:\n%s", numberedSnippet(src))
	}
	return []byte(out.String()), nil
}

func convertDartCLI(src string) ([]byte, error) {
	root, err := repoRoot()
	if err != nil {
		return nil, err
	}
	cli := filepath.Join(root, "tools", "any2mochi", "cmd", "dartast")
	cmd := exec.Command("go", "run", cli)
	cmd.Stdin = strings.NewReader(src)
	var out bytes.Buffer
	var stderr bytes.Buffer
	cmd.Stdout = &out
	cmd.Stderr = &stderr
	if err := cmd.Run(); err != nil {
		return nil, fmt.Errorf("dartast: %s", strings.TrimSpace(stderr.String()))
	}
	var prog DartProgram
	if err := json.Unmarshal(out.Bytes(), &prog); err != nil {
		return nil, err
	}
	return convertDartFromAST(&prog, src)
}

func convertDartFromAST(p *DartProgram, src string) ([]byte, error) {
	var out strings.Builder
	for _, d := range p.Decls {
		switch d.Kind {
		case "class":
			out.WriteString("type ")
			out.WriteString(d.Name)
			out.WriteString(" {\n")
			for _, f := range d.Fields {
				out.WriteString("  ")
				out.WriteString(f.Name)
				if f.Type != "" && f.Type != "any" {
					out.WriteString(": ")
					out.WriteString(f.Type)
				}
				out.WriteByte('\n')
			}
			out.WriteString("}\n")
		case "func":
			out.WriteString("fun ")
			out.WriteString(d.Name)
			out.WriteByte('(')
			for i, p := range d.Params {
				if i > 0 {
					out.WriteString(", ")
				}
				out.WriteString(p)
			}
			out.WriteString(") {\n")
			for _, line := range d.Body {
				stmt := convertDartStmt(line)
				if stmt == "" {
					continue
				}
				out.WriteString("  ")
				out.WriteString(stmt)
				out.WriteByte('\n')
			}
			out.WriteString("}\n")
		case "stmt":
			stmt := convertDartStmt(d.Stmt)
			if stmt == "" {
				continue
			}
			out.WriteString(stmt)
			out.WriteByte('\n')
		}
	}
	if out.Len() == 0 {
		return nil, fmt.Errorf("no convertible symbols found\n\nsource snippet:\n%s", numberedSnippet(src))
	}
	return []byte(out.String()), nil
}
