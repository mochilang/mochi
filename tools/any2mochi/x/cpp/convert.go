package cpp

import (
	"errors"
	"fmt"
	any2mochi "mochi/tools/any2mochi"
	"os"
	"os/exec"
	"regexp"
	"strconv"
	"strings"
)

type param struct {
	name string
	typ  string
}

// Convert converts cpp source code to Mochi using the language server.
func Convert(src string) ([]byte, error) {
	ls := any2mochi.Servers["cpp"]
	var syms []any2mochi.DocumentSymbol
	var diags []any2mochi.Diagnostic
	if ls.Command != "" {
		if _, lookErr := exec.LookPath(ls.Command); lookErr == nil {
			syms, diags, _ = any2mochi.ParseText(ls.Command, ls.Args, ls.LangID, src)
		}
	}

	var out strings.Builder
	if syms != nil {
		writeSymbols(&out, nil, syms, src, ls)
	}
	if out.Len() == 0 {
		funcs, enums, structs, err := parseAST(src)
		if err != nil {
			return nil, formatParseErr(src, err)
		}
		for _, st := range structs {
			out.WriteString("type ")
			out.WriteString(st.name)
			out.WriteString(" {\n")
			for _, f := range st.fields {
				out.WriteString("  ")
				out.WriteString(f.name)
				if f.typ != "" {
					out.WriteString(": ")
					out.WriteString(f.typ)
				}
				out.WriteByte('\n')
			}
			out.WriteString("}\n")
		}
		for _, e := range enums {
			out.WriteString("type ")
			out.WriteString(e.name)
			out.WriteString(" {\n")
			for _, v := range e.variants {
				out.WriteString("  ")
				out.WriteString(v)
				out.WriteByte('\n')
			}
			out.WriteString("}\n")
		}
		for _, f := range funcs {
			out.WriteString("fun ")
			out.WriteString(f.name)
			out.WriteByte('(')
			for i, p := range f.params {
				if i > 0 {
					out.WriteString(", ")
				}
				out.WriteString(p.name)
				if p.typ != "" {
					out.WriteString(": ")
					out.WriteString(p.typ)
				}
			}
			out.WriteByte(')')
			if f.ret != "" && f.ret != "void" {
				out.WriteString(": ")
				out.WriteString(f.ret)
			}
			body := convertBodyString(f.body)
			if len(body) == 0 {
				out.WriteString(" {}\n")
			} else {
				out.WriteString(" {\n")
				for _, line := range body {
					out.WriteString("  ")
					out.WriteString(line)
					out.WriteByte('\n')
				}
				out.WriteString("}\n")
			}
		}
	}
	if out.Len() == 0 {
		if len(diags) > 0 {
			return nil, fmt.Errorf("%s", diagnostics(src, diags))
		}
		return nil, fmt.Errorf("no convertible symbols found\n\nsource snippet:\n%s", snippet(src))
	}
	return []byte(out.String()), nil
}

// ConvertFile reads the cpp file and converts it to Mochi.
func ConvertFile(path string) ([]byte, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	out, convErr := Convert(string(data))
	if convErr != nil {
		return nil, fmt.Errorf("%s: %w", path, convErr)
	}
	return out, nil
}

func writeSymbols(out *strings.Builder, prefix []string, syms []any2mochi.DocumentSymbol, src string, ls any2mochi.LanguageServer) {
	for _, s := range syms {
		nameParts := prefix
		if s.Name != "" {
			nameParts = append(nameParts, s.Name)
		}
		switch s.Kind {
		case any2mochi.SymbolKindNamespace, any2mochi.SymbolKindModule, any2mochi.SymbolKindPackage:
			writeSymbols(out, nameParts, s.Children, src, ls)
		case any2mochi.SymbolKindClass, any2mochi.SymbolKindStruct:
			out.WriteString("type ")
			out.WriteString(strings.Join(nameParts, "."))
			out.WriteString(" {\n")
			for _, c := range s.Children {
				if c.Kind != any2mochi.SymbolKindField {
					continue
				}
				out.WriteString("  ")
				out.WriteString(c.Name)
				t := ""
				if c.Detail != nil {
					t = mapType(*c.Detail)
				}
				if t == "" {
					t = fieldType(src, c, ls)
				}
				if t != "" {
					out.WriteString(": ")
					out.WriteString(t)
				}
				out.WriteByte('\n')
			}
			out.WriteString("}\n")
			var childSyms []any2mochi.DocumentSymbol
			for _, c := range s.Children {
				if c.Kind != any2mochi.SymbolKindField {
					childSyms = append(childSyms, c)
				}
			}
			if len(childSyms) > 0 {
				writeSymbols(out, nameParts, childSyms, src, ls)
			}
		case any2mochi.SymbolKindEnum:
			out.WriteString("type ")
			out.WriteString(strings.Join(nameParts, "."))
			out.WriteString(" {\n")
			for _, c := range s.Children {
				if c.Kind == any2mochi.SymbolKindEnumMember || (c.Kind == any2mochi.SymbolKindEnum && len(c.Children) == 0) {
					fmt.Fprintf(out, "  %s\n", c.Name)
				}
			}
			out.WriteString("}\n")
			var rest []any2mochi.DocumentSymbol
			for _, c := range s.Children {
				if !(c.Kind == any2mochi.SymbolKindEnumMember || (c.Kind == any2mochi.SymbolKindEnum && len(c.Children) == 0)) {
					rest = append(rest, c)
				}
			}
			if len(rest) > 0 {
				writeSymbols(out, nameParts, rest, src, ls)
			}
		case any2mochi.SymbolKindFunction, any2mochi.SymbolKindMethod:
			signature := ""
			if hov, err := any2mochi.EnsureAndHover(ls.Command, ls.Args, ls.LangID, src, s.SelectionRange.Start); err == nil {
				if mc, ok := hov.Contents.(any2mochi.MarkupContent); ok {
					lines := strings.Split(mc.Value, "\n")
					for i := len(lines) - 1; i >= 0 && signature == ""; i-- {
						l := strings.TrimSpace(lines[i])
						if strings.Contains(l, "(") && strings.Contains(l, ")") {
							signature = l
						}
					}
				}
			}
			var params []param
			var ret string
			if signature != "" {
				params, ret = parseSignature(signature)
			} else {
				names, r := parseDetail(s.Detail)
				ret = r
				for _, n := range names {
					params = append(params, param{name: n})
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
				if p.typ != "" {
					out.WriteString(": ")
					out.WriteString(p.typ)
				}
			}
			out.WriteByte(')')
			ret = mapType(ret)
			if ret != "" && ret != "void" {
				out.WriteString(": ")
				out.WriteString(ret)
			}
			body := convertBody(src, s.Range)
			if len(body) == 0 {
				out.WriteString(" {}\n")
			} else {
				out.WriteString(" {\n")
				for _, line := range body {
					out.WriteString("  ")
					out.WriteString(line)
					out.WriteByte('\n')
				}
				out.WriteString("}\n")
			}
		case any2mochi.SymbolKindVariable, any2mochi.SymbolKindConstant:
			if strings.HasPrefix(s.Name, "using ") || strings.Contains(s.Name, " ") {
				continue
			}
			out.WriteString("let ")
			out.WriteString(strings.Join(nameParts, "."))
			out.WriteByte('\n')
		}
	}
}

func parseDetail(detail *string) ([]string, string) {
	if detail == nil {
		return nil, ""
	}
	d := strings.TrimSpace(*detail)
	if d == "" {
		return nil, ""
	}
	open := strings.Index(d, "(")
	close := strings.LastIndex(d, ")")
	if open == -1 || close == -1 || close < open {
		return nil, strings.TrimSpace(d)
	}
	ret := strings.TrimSpace(d[:open])
	paramsPart := d[open+1 : close]
	params := []string{}
	for _, p := range strings.Split(paramsPart, ",") {
		p = strings.TrimSpace(p)
		if p == "" {
			continue
		}
		fields := strings.Fields(p)
		if len(fields) == 0 {
			continue
		}
		name := fields[len(fields)-1]
		if eq := strings.Index(name, "="); eq != -1 {
			name = name[:eq]
		}
		name = strings.Trim(name, "*&")
		params = append(params, name)
	}
	return params, ret
}

func parseSignature(sig string) ([]param, string) {
	sig = strings.TrimSpace(sig)
	open := strings.Index(sig, "(")
	close := strings.LastIndex(sig, ")")
	if open == -1 || close == -1 || close < open {
		return nil, mapType(sig)
	}
	header := strings.TrimSpace(sig[:open])
	paramsPart := sig[open+1 : close]
	parts := strings.Fields(header)
	ret := ""
	if len(parts) > 1 {
		ret = mapType(strings.Join(parts[:len(parts)-1], " "))
	}
	paramsSplit := strings.Split(paramsPart, ",")
	params := make([]param, 0, len(paramsSplit))
	for _, ps := range paramsSplit {
		ps = strings.TrimSpace(ps)
		if ps == "" || ps == "void" {
			continue
		}
		f := strings.Fields(ps)
		name := f[len(f)-1]
		typ := ""
		if len(f) > 1 {
			typ = mapType(strings.Join(f[:len(f)-1], " "))
		}
		if eq := strings.Index(name, "="); eq != -1 {
			name = name[:eq]
		}
		name = strings.Trim(name, "*&")
		params = append(params, param{name: name, typ: typ})
	}
	return params, ret
}

func mapType(typ string) string {
	typ = strings.TrimSpace(typ)
	for strings.HasSuffix(typ, "*") || strings.HasSuffix(typ, "&") {
		typ = strings.TrimSpace(typ[:len(typ)-1])
	}
	typ = strings.TrimPrefix(typ, "const ")
	typ = strings.TrimPrefix(typ, "unsigned ")
	switch typ {
	case "", "void":
		return ""
	case "int", "size_t", "long", "short":
		return "int"
	case "float", "double":
		return "float"
	case "bool":
		return "bool"
	case "char", "char16_t", "char32_t", "std::string", "string":
		return "string"
	}
	if strings.HasPrefix(typ, "std::vector<") && strings.HasSuffix(typ, ">") {
		inner := mapType(typ[len("std::vector<") : len(typ)-1])
		if inner == "" {
			inner = "any"
		}
		return "list<" + inner + ">"
	}
	return typ
}

// fieldType attempts to determine the type of a field using hover information
// from the language server when the document symbol does not include it.
func fieldType(src string, sym any2mochi.DocumentSymbol, ls any2mochi.LanguageServer) string {
	hov, err := any2mochi.EnsureAndHover(ls.Command, ls.Args, ls.LangID, src, sym.SelectionRange.Start)
	if err != nil {
		return ""
	}
	if mc, ok := hov.Contents.(any2mochi.MarkupContent); ok {
		for _, line := range strings.Split(mc.Value, "\n") {
			if idx := strings.Index(line, ":"); idx != -1 {
				t := strings.TrimSpace(line[idx+1:])
				if t != "" {
					return mapType(t)
				}
			}
		}
	}
	return ""
}

// convertBody converts the body of a function defined by r in src into a slice
// of Mochi statements. Only very basic constructs like prints, returns and
// simple assignments are handled.
func convertBody(src string, r any2mochi.Range) []string {
	lines := strings.Split(src, "\n")
	start := int(r.Start.Line)
	end := int(r.End.Line)
	if start >= len(lines) || end >= len(lines) {
		return nil
	}
	bodyLines := lines[start : end+1]
	if len(bodyLines) > 0 {
		bodyLines = bodyLines[1:]
	}
	if len(bodyLines) > 0 {
		bodyLines = bodyLines[:len(bodyLines)-1]
	}
	var out []string
	for _, l := range bodyLines {
		l = strings.TrimSpace(l)
		l = strings.TrimSuffix(l, ";")
		if l == "" {
			continue
		}
		switch {
		case strings.HasPrefix(l, "return"):
			out = append(out, l)
		case strings.Contains(l, "std::cout") || strings.HasPrefix(l, "cout <<"):
			l = strings.TrimPrefix(l, "std::cout <<")
			l = strings.TrimPrefix(l, "cout <<")
			l = strings.TrimSuffix(l, "<< std::endl")
			l = strings.TrimSuffix(l, "<< endl")
			l = strings.TrimSpace(l)
			out = append(out, "print("+l+")")
		default:
			decl := false
			for _, pre := range []string{"int ", "float ", "double ", "bool ", "std::string ", "string ", "auto "} {
				if strings.HasPrefix(l, pre) {
					l = strings.TrimPrefix(l, pre)
					decl = true
					break
				}
			}
			if decl {
				l = "let " + l
			}
			out = append(out, l)
		}
	}
	return out
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
		start := int(d.Range.Start.Line)
		msg := d.Message
		line := ""
		if start < len(lines) {
			line = strings.TrimSpace(lines[start])
		}
		out.WriteString(fmt.Sprintf("line %d: %s\n  %s\n", start+1, msg, line))
	}
	return strings.TrimSpace(out.String())
}

func formatParseErr(src string, err error) error {
	msg := err.Error()
	re := regexp.MustCompile(`:(\d+):(\d+):`)
	lines := strings.Split(src, "\n")
	matches := re.FindAllStringSubmatch(msg, -1)
	if matches == nil {
		return err
	}
	var b strings.Builder
	for _, m := range matches {
		ln, _ := strconv.Atoi(m[1])
		col, _ := strconv.Atoi(m[2])
		start := ln - 2
		if start < 0 {
			start = 0
		}
		end := ln + 1
		if end > len(lines) {
			end = len(lines)
		}
		fmt.Fprintf(&b, "line %d:%d: %s\n", ln, col, msg)
		for i := start; i < end && i < len(lines); i++ {
			prefix := "   "
			if i+1 == ln {
				prefix = ">>>"
			}
			fmt.Fprintf(&b, "%d:%s %s\n", i+1, prefix, lines[i])
			if i+1 == ln {
				pointer := strings.Repeat(" ", col-1) + "^"
				fmt.Fprintf(&b, "    %s\n", pointer)
			}
		}
	}
	return errors.New(strings.TrimSpace(b.String()))
}
