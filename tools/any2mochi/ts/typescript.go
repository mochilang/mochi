package ts

import (
	"encoding/json"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"regexp"
	"runtime"
	"strings"

	tscode "mochi/compile/ts"
	a2m "mochi/tools/any2mochi"
)

// TypeScriptUseLanguageServer controls whether the TypeScript language server is
// used when converting. It defaults to false and can be enabled by consumers.
var TypeScriptUseLanguageServer = false

// ConvertTypeScript converts TypeScript source code to a minimal Mochi representation.
// If TypeScriptUseLanguageServer is false the conversion relies solely on the
// regex based fallback parser. When enabled, the language server is attempted
// after the fallback if no symbols were discovered.
func ConvertTypeScript(src string) ([]byte, error) {
	var out strings.Builder

	if decls, err := parseTypeScriptAST(src); err == nil && len(decls) > 0 {
		writeTypeScriptDecls(&out, decls)
	} else if data, err := parseTypeScriptDeno(src); err == nil && len(data) > 0 {
		out.Write(data)
	} else {
		parseTypeScriptFallback(&out, src)
	}

	var syms []a2m.DocumentSymbol
	var diags []a2m.Diagnostic
	var err error

	if out.Len() == 0 && TypeScriptUseLanguageServer {
		_ = tscode.EnsureTSLanguageServer()
		ls := a2m.Servers["typescript"]
		syms, diags, err = a2m.EnsureAndParse(ls.Command, ls.Args, ls.LangID, src)
		if err == nil && len(diags) == 0 {
			writeTypeScriptSymbols(&out, nil, syms, src, ls)
		}
	}

	if out.Len() == 0 {
		if err != nil {
			return nil, err
		}
		if len(diags) > 0 {
			return nil, fmt.Errorf("%s", formatDiagnostics(src, diags))
		}
		return nil, fmt.Errorf("no convertible symbols found\n\nsource snippet:\n%s", numberedSnippet(src))
	}
	return []byte(out.String()), nil
}

// ConvertTypeScriptFile reads the TS file and converts it to Mochi.
func ConvertTypeScriptFile(path string) ([]byte, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	return ConvertTypeScript(string(data))
}

// ConvertTypeScriptWithJSON converts the source and also returns the parsed
// symbols encoded as JSON.
func ConvertTypeScriptWithJSON(src string) ([]byte, []byte, error) {
	if !TypeScriptUseLanguageServer {
		if data, err := parseTypeScriptDeno(src); err == nil && len(data) > 0 {
			return data, nil, nil
		}
		var b strings.Builder
		parseTypeScriptFallback(&b, src)
		if b.Len() == 0 {
			return nil, nil, fmt.Errorf("no convertible symbols found\n\nsource snippet:\n%s", numberedSnippet(src))
		}
		return []byte(b.String()), nil, nil
	}

	ls := a2m.Servers["typescript"]
	syms, diags, err := a2m.EnsureAndParse(ls.Command, ls.Args, ls.LangID, src)
	if err != nil {
		return nil, nil, err
	}
	if len(diags) > 0 {
		return nil, nil, fmt.Errorf("%s", formatDiagnostics(src, diags))
	}
	var out strings.Builder
	writeTypeScriptSymbols(&out, nil, syms, src, ls)
	if out.Len() == 0 {
		return nil, nil, fmt.Errorf("no convertible symbols found\n\nsource snippet:\n%s", numberedSnippet(src))
	}
	js, _ := json.MarshalIndent(syms, "", "  ")
	return []byte(out.String()), js, nil
}

// ConvertTypeScriptFileWithJSON reads the TS file and converts it to Mochi
// while also returning the parsed symbols as JSON.
func ConvertTypeScriptFileWithJSON(path string) ([]byte, []byte, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, nil, err
	}
	return ConvertTypeScriptWithJSON(string(data))
}

func writeTypeScriptSymbols(out *strings.Builder, prefix []string, syms []a2m.DocumentSymbol, src string, ls a2m.LanguageServer) {
	for _, s := range syms {
		nameParts := prefix
		if s.Name != "" {
			nameParts = append(nameParts, s.Name)
		}
		switch s.Kind {
		case a2m.SymbolKindClass, a2m.SymbolKindInterface, a2m.SymbolKindStruct:
			writeTypeScriptClass(out, nameParts, s, src, ls)
		case a2m.SymbolKindEnum:
			writeTypeScriptEnum(out, nameParts, s, src, ls)
		case a2m.SymbolKindVariable, a2m.SymbolKindConstant, a2m.SymbolKindField, a2m.SymbolKindProperty:
			if s.Name != "" && len(prefix) == 0 {
				if fields, alias := typeScriptAliasDef(src, s, ls); len(fields) > 0 || alias != "" {
					writeTypeScriptAlias(out, s.Name, fields, alias)
				} else {
					typ := typeScriptFieldType(src, s, ls)
					out.WriteString("let ")
					out.WriteString(s.Name)
					if typ != "" {
						out.WriteString(": ")
						out.WriteString(typ)
					}
					out.WriteByte('\n')
				}
			}
		case a2m.SymbolKindFunction, a2m.SymbolKindMethod, a2m.SymbolKindConstructor:
			writeTypeScriptFunc(out, strings.Join(nameParts, "."), s, src, ls)
		}
		if len(s.Children) > 0 && s.Kind != a2m.SymbolKindClass && s.Kind != a2m.SymbolKindInterface && s.Kind != a2m.SymbolKindStruct {
			writeTypeScriptSymbols(out, nameParts, s.Children, src, ls)
		}
	}
}

func writeTypeScriptClass(out *strings.Builder, nameParts []string, sym a2m.DocumentSymbol, src string, ls a2m.LanguageServer) {
	out.WriteString("type ")
	out.WriteString(strings.Join(nameParts, "."))
	fields := []a2m.DocumentSymbol{}
	methods := []a2m.DocumentSymbol{}
	for _, c := range sym.Children {
		switch c.Kind {
		case a2m.SymbolKindField, a2m.SymbolKindProperty:
			fields = append(fields, c)
		case a2m.SymbolKindFunction, a2m.SymbolKindMethod, a2m.SymbolKindConstructor:
			methods = append(methods, c)
		}
	}
	if len(fields) == 0 && len(methods) == 0 {
		out.WriteString(" {}\n")
		return
	}
	out.WriteString(" {\n")
	for _, f := range fields {
		out.WriteString("  ")
		out.WriteString(f.Name)
		if typ := typeScriptFieldType(src, f, ls); typ != "" {
			out.WriteString(": ")
			out.WriteString(typ)
		}
		out.WriteByte('\n')
	}
	for _, m := range methods {
		var b strings.Builder
		writeTypeScriptFunc(&b, m.Name, m, src, ls)
		for _, line := range strings.Split(strings.TrimSuffix(b.String(), "\n"), "\n") {
			out.WriteString("  ")
			out.WriteString(line)
			out.WriteByte('\n')
		}
	}
	out.WriteString("}\n")
}

func writeTypeScriptFunc(out *strings.Builder, name string, sym a2m.DocumentSymbol, src string, ls a2m.LanguageServer) {
	params, ret := typeScriptHoverSignature(src, sym, ls)
	out.WriteString("fun ")
	out.WriteString(name)
	out.WriteByte('(')
	for i, p := range params {
		if i > 0 {
			out.WriteString(", ")
		}
		out.WriteString(p.Name)
		if p.Typ != "" {
			out.WriteString(": ")
			out.WriteString(p.Typ)
		}
	}
	out.WriteByte(')')
	if ret != "" && ret != "void" {
		out.WriteString(": ")
		out.WriteString(ret)
	}
	start := indexForPosition(src, sym.Range.Start)
	end := indexForPosition(src, sym.Range.End)
	body := ""
	if start < end && end <= len(src) {
		snippet := src[start:end]
		if open := strings.Index(snippet, "{"); open != -1 {
			if close := strings.LastIndex(snippet, "}"); close != -1 && close > open {
				body = snippet[open+1 : close]
			}
		}
	}
	stmts := typeScriptFunctionBody(body)
	if len(stmts) == 0 {
		out.WriteString(" {}\n")
		return
	}
	out.WriteString(" {\n")
	for _, line := range stmts {
		out.WriteString("  ")
		out.WriteString(line)
		out.WriteByte('\n')
	}
	out.WriteString("}\n")
}

func typeScriptHoverSignature(src string, sym a2m.DocumentSymbol, ls a2m.LanguageServer) ([]typeScriptParam, string) {
	hov, err := a2m.EnsureAndHover(ls.Command, ls.Args, ls.LangID, src, sym.SelectionRange.Start)
	if err != nil {
		return nil, ""
	}
	if mc, ok := hov.Contents.(a2m.MarkupContent); ok {
		lines := strings.Split(mc.Value, "\n")
		for i := len(lines) - 1; i >= 0; i-- {
			l := strings.TrimSpace(lines[i])
			if strings.Contains(l, "(") && strings.Contains(l, ")") {
				return parseTypeScriptSignature(l)
			}
		}
	}
	return nil, ""
}

func typeScriptFieldType(src string, sym a2m.DocumentSymbol, ls a2m.LanguageServer) string {
	hov, err := a2m.EnsureAndHover(ls.Command, ls.Args, ls.LangID, src, sym.SelectionRange.Start)
	if err != nil {
		return ""
	}
	if mc, ok := hov.Contents.(a2m.MarkupContent); ok {
		lines := strings.Split(mc.Value, "\n")
		for _, l := range lines {
			l = strings.TrimSpace(l)
			if idx := strings.Index(l, ":"); idx != -1 {
				typ := strings.TrimSpace(l[idx+1:])
				if typ != "" {
					return typeScriptToMochiType(typ)
				}
			}
		}
	}
	return ""
}

type typeScriptField struct {
	Name string `json:"name"`
	Typ  string `json:"typ"`
}

// typeScriptAliasDef returns fields for a type alias object or the aliased type.
// When neither can be determined it returns nil and empty string.
func typeScriptAliasDef(src string, sym a2m.DocumentSymbol, ls a2m.LanguageServer) ([]typeScriptField, string) {
	hov, err := a2m.EnsureAndHover(ls.Command, ls.Args, ls.LangID, src, sym.SelectionRange.Start)
	if err != nil {
		return nil, ""
	}
	if mc, ok := hov.Contents.(a2m.MarkupContent); ok {
		lines := strings.Split(mc.Value, "\n")
		reading := false
		var fields []typeScriptField
		for _, l := range lines {
			l = strings.TrimSpace(l)
			if !reading {
				if strings.HasPrefix(l, "type "+sym.Name) {
					if idx := strings.Index(l, "="); idx != -1 {
						after := strings.TrimSpace(l[idx+1:])
						if strings.HasPrefix(after, "{") {
							reading = true
							continue
						}
						return nil, typeScriptToMochiType(strings.TrimSuffix(after, ";"))
					}
				}
			} else {
				if strings.HasPrefix(l, "}") {
					break
				}
				l = strings.TrimSuffix(l, ";")
				if idx := strings.Index(l, ":"); idx != -1 {
					name := strings.TrimSpace(l[:idx])
					typ := typeScriptToMochiType(strings.TrimSpace(l[idx+1:]))
					fields = append(fields, typeScriptField{Name: name, Typ: typ})
				}
			}
		}
		if len(fields) > 0 {
			return fields, ""
		}
	}
	return nil, ""
}

func writeTypeScriptAlias(out *strings.Builder, name string, fields []typeScriptField, alias string) {
	out.WriteString("type ")
	out.WriteString(name)
	if len(fields) == 0 {
		out.WriteString(" = ")
		out.WriteString(alias)
		out.WriteByte('\n')
		return
	}
	out.WriteString(" {\n")
	for _, f := range fields {
		out.WriteString("  ")
		out.WriteString(f.Name)
		if f.Typ != "" {
			out.WriteString(": ")
			out.WriteString(f.Typ)
		}
		out.WriteByte('\n')
	}
	out.WriteString("}\n")
}

func writeTypeScriptEnum(out *strings.Builder, nameParts []string, sym a2m.DocumentSymbol, src string, ls a2m.LanguageServer) {
	out.WriteString("type ")
	out.WriteString(strings.Join(nameParts, "."))
	out.WriteString(" {\n")
	for _, c := range sym.Children {
		if c.Kind == a2m.SymbolKindEnumMember {
			out.WriteString("  ")
			out.WriteString(c.Name)
			out.WriteByte('\n')
		}
	}
	out.WriteString("}\n")
	var rest []a2m.DocumentSymbol
	for _, c := range sym.Children {
		if c.Kind != a2m.SymbolKindEnumMember {
			rest = append(rest, c)
		}
	}
	if len(rest) > 0 {
		writeTypeScriptSymbols(out, nameParts, rest, src, ls)
	}
}

type typeScriptParam struct {
	Name string `json:"name"`
	Typ  string `json:"typ"`
}

func parseTypeScriptSignature(sig string) ([]typeScriptParam, string) {
	sig = strings.TrimSpace(sig)
	open := strings.Index(sig, "(")
	close := strings.LastIndex(sig, ")")
	if open == -1 || close == -1 || close < open {
		return nil, typeScriptToMochiType(strings.TrimSpace(sig))
	}
	paramsPart := sig[open+1 : close]
	var params []typeScriptParam
	for _, p := range splitTypeScriptParams(paramsPart) {
		p = strings.TrimSpace(p)
		if p == "" {
			continue
		}
		name := p
		typ := ""
		if colon := strings.Index(p, ":"); colon != -1 {
			name = strings.TrimSpace(p[:colon])
			typ = strings.TrimSpace(p[colon+1:])
		}
		params = append(params, typeScriptParam{Name: name, Typ: typeScriptToMochiType(typ)})
	}
	rest := strings.TrimSpace(sig[close+1:])
	if strings.HasPrefix(rest, ":") {
		rest = strings.TrimSpace(rest[1:])
	} else if strings.HasPrefix(rest, "=>") {
		rest = strings.TrimSpace(rest[2:])
	}
	return params, typeScriptToMochiType(rest)
}

func splitTypeScriptParams(s string) []string {
	var parts []string
	depth := 0
	start := 0
	for i, r := range s {
		switch r {
		case '<', '(', '[':
			depth++
		case '>', ')', ']':
			if depth > 0 {
				depth--
			}
		case ',':
			if depth == 0 {
				parts = append(parts, s[start:i])
				start = i + 1
			}
		}
	}
	if start < len(s) {
		parts = append(parts, s[start:])
	}
	return parts
}

func typeScriptToMochiType(t string) string {
	t = strings.TrimSpace(t)
	if strings.Contains(t, "|") {
		parts := strings.Split(t, "|")
		var keep []string
		for _, p := range parts {
			p = strings.TrimSpace(p)
			if p == "null" || p == "undefined" {
				continue
			}
			mp := typeScriptToMochiType(p)
			if mp != "" {
				keep = append(keep, mp)
			}
		}
		if len(keep) == 1 {
			return keep[0]
		}
		if len(keep) > 1 {
			return "any"
		}
		return ""
	}
	switch t {
	case "", "any", "unknown", "object":
		return ""
	case "number":
		return "int"
	case "string":
		return "string"
	case "boolean":
		return "bool"
	case "void", "undefined", "null":
		return ""
	}
	if strings.HasSuffix(t, "[]") {
		inner := typeScriptToMochiType(t[:len(t)-2])
		if inner == "" {
			inner = "any"
		}
		return "list<" + inner + ">"
	}
	if strings.HasPrefix(t, "Array<") && strings.HasSuffix(t, ">") {
		inner := typeScriptToMochiType(t[len("Array<") : len(t)-1])
		if inner == "" {
			inner = "any"
		}
		return "list<" + inner + ">"
	}
	if strings.HasPrefix(t, "Record<") && strings.HasSuffix(t, ">") {
		parts := splitTypeScriptParams(t[len("Record<") : len(t)-1])
		key := "any"
		val := "any"
		if len(parts) > 0 {
			k := typeScriptToMochiType(parts[0])
			if k != "" {
				key = k
			}
		}
		if len(parts) > 1 {
			v := typeScriptToMochiType(parts[1])
			if v != "" {
				val = v
			}
		}
		return "map<" + key + "," + val + ">"
	}
	return t
}

func parseTypeScriptDeno(src string) ([]byte, error) {
	if err := tscode.EnsureDeno(); err != nil {
		return nil, err
	}
	_, file, _, _ := runtime.Caller(0)
	script := filepath.Join(filepath.Dir(file), "convert_deno.ts")
	temp, err := os.CreateTemp("", "tsinput-*.ts")
	if err != nil {
		return nil, err
	}
	if _, err := temp.WriteString(src); err != nil {
		os.Remove(temp.Name())
		return nil, err
	}
	temp.Close()
	defer os.Remove(temp.Name())
	cmd := exec.Command("deno", "run", "--quiet", "--allow-read", script, temp.Name())
	cmd.Env = append(os.Environ(), "DENO_TLS_CA_STORE=system")
	out, err := cmd.CombinedOutput()
	if err != nil {
		return nil, fmt.Errorf("deno error: %w\n%s", err, out)
	}
	return out, nil
}

func parseTypeScriptFallback(out *strings.Builder, src string) {
	typeRe := regexp.MustCompile(`(?ms)type\s+([A-Za-z_][A-Za-z0-9_]*)\s*=\s*{`)
	for _, idx := range typeRe.FindAllStringSubmatchIndex(src, -1) {
		name := src[idx[2]:idx[3]]
		open := strings.Index(src[idx[0]:idx[1]], "{")
		if open == -1 {
			continue
		}
		open += idx[0]
		close := findMatch(src, open, '{', '}')
		if close <= open {
			continue
		}
		body := src[open+1 : close]
		out.WriteString("type ")
		out.WriteString(name)
		out.WriteString(" {\n")
		for _, line := range strings.Split(body, "\n") {
			l := strings.TrimSpace(strings.TrimSuffix(line, ";"))
			if l == "" {
				continue
			}
			if colon := strings.Index(l, ":"); colon != -1 {
				field := strings.TrimSpace(l[:colon])
				typ := typeScriptToMochiType(strings.TrimSpace(l[colon+1:]))
				out.WriteString("  ")
				out.WriteString(field)
				if typ != "" {
					out.WriteString(": ")
					out.WriteString(typ)
				}
				out.WriteByte('\n')
			}
		}
		out.WriteString("}\n")
	}

	varRe := regexp.MustCompile(`(?m)^(?:let|const|var)\s+([A-Za-z_][A-Za-z0-9_]*)\s*(?::\s*([^=;]+))?`)
	for _, m := range varRe.FindAllStringSubmatch(src, -1) {
		name := m[1]
		typ := typeScriptToMochiType(strings.TrimSpace(m[2]))
		if name == "" {
			continue
		}
		out.WriteString("let ")
		out.WriteString(name)
		if typ != "" {
			out.WriteString(": ")
			out.WriteString(typ)
		}
		out.WriteByte('\n')
	}

	funcRe := regexp.MustCompile(`(?ms)function\s+([A-Za-z_][A-Za-z0-9_]*)\s*\(([^)]*)\)\s*(?::\s*([^{\n]+))?\s*\{`)
	for _, idx := range funcRe.FindAllStringSubmatchIndex(src, -1) {
		name := src[idx[2]:idx[3]]
		paramsPart := src[idx[4]:idx[5]]
		ret := strings.TrimSpace(src[idx[6]:idx[7]])
		open := idx[1] - 1
		close := findMatch(src, open, '{', '}')
		body := ""
		if close > open {
			body = src[open+1 : close]
		}
		out.WriteString("fun ")
		out.WriteString(name)
		out.WriteByte('(')
		params, _ := parseTypeScriptSignature("(" + paramsPart + ")" + funcReturnSig(ret))
		for i, p := range params {
			if i > 0 {
				out.WriteString(", ")
			}
			out.WriteString(p.Name)
			if p.Typ != "" {
				out.WriteString(": ")
				out.WriteString(p.Typ)
			}
		}
		out.WriteByte(')')
		mappedRet := typeScriptToMochiType(ret)
		if mappedRet != "" && mappedRet != "void" {
			out.WriteString(": ")
			out.WriteString(mappedRet)
		}
		stmts := typeScriptFunctionBody(body)
		if len(stmts) == 0 {
			out.WriteString(" {}\n")
		} else {
			out.WriteString(" {\n")
			for _, l := range stmts {
				out.WriteString("  ")
				out.WriteString(l)
				out.WriteByte('\n')
			}
			out.WriteString("}\n")
		}
	}
}

func funcReturnSig(ret string) string {
	if ret == "" {
		return ""
	}
	return ": " + ret
}

// writeTypeScriptDecls converts parsed AST declarations into Mochi source code.
func writeTypeScriptDecls(out *strings.Builder, decls []typeScriptDecl) {
	for _, d := range decls {
		switch d.Kind {
		case "var":
			out.WriteString("let ")
			out.WriteString(d.Name)
			if d.Ret != "" {
				out.WriteString(": ")
				out.WriteString(d.Ret)
			}
			out.WriteByte('\n')
		case "func":
			out.WriteString("fun ")
			out.WriteString(d.Name)
			out.WriteByte('(')
			for i, p := range d.Params {
				if i > 0 {
					out.WriteString(", ")
				}
				out.WriteString(p.Name)
				if p.Typ != "" {
					out.WriteString(": ")
					out.WriteString(p.Typ)
				}
			}
			out.WriteByte(')')
			if d.Ret != "" && d.Ret != "void" {
				out.WriteString(": ")
				out.WriteString(d.Ret)
			}
			stmts := typeScriptFunctionBody(d.Body)
			if len(stmts) == 0 {
				out.WriteString(" {}\n")
			} else {
				out.WriteString(" {\n")
				for _, l := range stmts {
					out.WriteString("  ")
					out.WriteString(l)
					out.WriteByte('\n')
				}
				out.WriteString("}\n")
			}
		case "enum":
			out.WriteString("type ")
			out.WriteString(d.Name)
			out.WriteString(" {\n")
			for _, v := range d.Variants {
				out.WriteString("  ")
				out.WriteString(v)
				out.WriteByte('\n')
			}
			out.WriteString("}\n")
		case "type":
			out.WriteString("type ")
			out.WriteString(d.Name)
			out.WriteString(" {\n")
			for _, f := range d.Fields {
				out.WriteString("  ")
				out.WriteString(f.Name)
				if f.Typ != "" {
					out.WriteString(": ")
					out.WriteString(f.Typ)
				}
				out.WriteByte('\n')
			}
			out.WriteString("}\n")
		case "alias":
			out.WriteString("type ")
			out.WriteString(d.Name)
			out.WriteString(" = ")
			out.WriteString(d.Alias)
			out.WriteByte('\n')
		}
	}
}
