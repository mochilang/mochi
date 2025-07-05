package any2mochi

import (
	"fmt"
	"os"
	"path/filepath"
	"strings"

	protocol "github.com/tliron/glsp/protocol_3_16"
)

// ConvertPhp converts php source code to Mochi using the language server.
func ConvertPhp(src string) ([]byte, error) {
	return convertPhp(src, "")
}

func convertPhp(src, root string) ([]byte, error) {
	ls := Servers["php"]
	syms, _, err := EnsureAndParseWithRoot(ls.Command, ls.Args, ls.LangID, src, root)
	if err != nil {
		return nil, err
	}
	// Many PHP language servers emit diagnostics for standard library
	// functions unless a full project is loaded. Ignore diagnostics so we
	// can still extract symbols from standalone files.
	var out strings.Builder
	appendPhpSymbols(&out, nil, syms, src, ls)
	if out.Len() == 0 {
		return nil, fmt.Errorf("no convertible symbols found\n\nsource snippet:\n%s", numberedSnippet(src))
	}
	return []byte(out.String()), nil
}

func appendPhpSymbols(out *strings.Builder, prefix []string, syms []protocol.DocumentSymbol, src string, ls LanguageServer) {
	for _, s := range syms {
		nameParts := prefix
		if s.Name != "" {
			nameParts = append(nameParts, s.Name)
		}
		switch s.Kind {
		case protocol.SymbolKindFunction, protocol.SymbolKindMethod, protocol.SymbolKindConstructor:
			var params []phpParam
			var ret string
			params, ret = phpHoverSignature(src, s.SelectionRange.Start, ls)
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
			if ret != "" && ret != "void" {
				out.WriteString(": ")
				out.WriteString(ret)
			}
			out.WriteString(" {}\n")
			if len(s.Children) > 0 {
				appendPhpSymbols(out, nameParts, s.Children, src, ls)
			}
		case protocol.SymbolKindClass:
			out.WriteString("type ")
			out.WriteString(strings.Join(nameParts, "."))
			fields := []protocol.DocumentSymbol{}
			methods := []protocol.DocumentSymbol{}
			for _, c := range s.Children {
				switch c.Kind {
				case protocol.SymbolKindField, protocol.SymbolKindProperty:
					fields = append(fields, c)
				case protocol.SymbolKindFunction, protocol.SymbolKindMethod, protocol.SymbolKindConstructor:
					methods = append(methods, c)
				}
			}
			if len(fields) == 0 && len(methods) == 0 {
				out.WriteString(" {}\n")
			} else {
				out.WriteString(" {\n")
				for _, f := range fields {
					name := strings.TrimPrefix(f.Name, "$")
					typ := phpFieldType(src, f.SelectionRange.Start, ls)
					if typ == "" {
						typ = "any"
					}
					fmt.Fprintf(out, "  %s: %s\n", name, typ)
				}
				for _, m := range methods {
					var b strings.Builder
					appendPhpSymbols(&b, []string{m.Name}, []protocol.DocumentSymbol{m}, src, ls)
					for _, line := range strings.Split(strings.TrimSuffix(b.String(), "\n"), "\n") {
						out.WriteString("  ")
						out.WriteString(line)
						out.WriteByte('\n')
					}
				}
				out.WriteString("}\n")
			}
			// recurse on nested symbols that aren't part of the struct body
			for _, c := range s.Children {
				if c.Kind != protocol.SymbolKindField && c.Kind != protocol.SymbolKindProperty && c.Kind != protocol.SymbolKindFunction && c.Kind != protocol.SymbolKindMethod && c.Kind != protocol.SymbolKindConstructor {
					appendPhpSymbols(out, nameParts, []protocol.DocumentSymbol{c}, src, ls)
				}
			}
		case protocol.SymbolKindVariable, protocol.SymbolKindConstant:
			if len(prefix) == 0 {
				name := strings.TrimPrefix(s.Name, "$")
				typ := phpFieldType(src, s.SelectionRange.Start, ls)
				if typ == "" {
					typ = "any"
				}
				fmt.Fprintf(out, "let %s: %s\n", name, typ)
			}
		default:
			if len(s.Children) > 0 {
				appendPhpSymbols(out, nameParts, s.Children, src, ls)
			}
		}
	}
}

// ConvertPhpFile reads the php file and converts it to Mochi.
func ConvertPhpFile(path string) ([]byte, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	return convertPhp(string(data), filepath.Dir(path))
}

type phpParam struct {
	name string
	typ  string
}

func phpHoverSignature(src string, pos protocol.Position, ls LanguageServer) ([]phpParam, string) {
	hov, err := EnsureAndHover(ls.Command, ls.Args, ls.LangID, src, pos)
	if err != nil {
		return nil, ""
	}
	if mc, ok := hov.Contents.(protocol.MarkupContent); ok {
		return parsePhpSignature(mc.Value)
	}
	return nil, ""
}

func phpFieldType(src string, pos protocol.Position, ls LanguageServer) string {
	hov, err := EnsureAndHover(ls.Command, ls.Args, ls.LangID, src, pos)
	if err != nil {
		return ""
	}
	if mc, ok := hov.Contents.(protocol.MarkupContent); ok {
		return parsePhpVarType(mc.Value)
	}
	return ""
}

func parsePhpSignature(s string) ([]phpParam, string) {
	lines := strings.Split(s, "\n")
	var params []phpParam
	ret := ""
	for _, l := range lines {
		l = strings.TrimSpace(l)
		if strings.HasPrefix(l, "@param") || strings.HasPrefix(l, "_@param_") {
			l = strings.TrimPrefix(l, "@param")
			l = strings.TrimPrefix(l, "_@param_")
			l = strings.TrimSpace(strings.Trim(l, "`"))
			fields := strings.Fields(l)
			if len(fields) >= 2 {
				typ := mapPhpType(strings.Trim(fields[0], "`"))
				name := strings.TrimPrefix(fields[1], "$")
				params = append(params, phpParam{name: name, typ: typ})
			}
		} else if strings.HasPrefix(l, "@return") || strings.HasPrefix(l, "_@return_") {
			l = strings.TrimPrefix(l, "@return")
			l = strings.TrimPrefix(l, "_@return_")
			l = strings.TrimSpace(strings.Trim(l, "`"))
			ret = mapPhpType(strings.Trim(l, "`"))
		}
	}
	return params, ret
}

func parsePhpVarType(s string) string {
	lines := strings.Split(s, "\n")
	for _, l := range lines {
		l = strings.TrimSpace(l)
		if strings.HasPrefix(l, "@var") || strings.HasPrefix(l, "_@var_") {
			l = strings.TrimPrefix(l, "@var")
			l = strings.TrimPrefix(l, "_@var_")
			l = strings.TrimSpace(strings.Trim(l, "`"))
			fields := strings.Fields(l)
			if len(fields) >= 1 {
				return mapPhpType(strings.Trim(fields[0], "`"))
			}
		}
	}
	return ""
}

func mapPhpType(t string) string {
	t = strings.TrimSpace(t)
	if strings.HasSuffix(t, "[]") {
		elem := mapPhpType(strings.TrimSuffix(t, "[]"))
		if elem == "" {
			elem = "any"
		}
		return "list<" + elem + ">"
	}
	if strings.HasPrefix(t, "array<") && strings.HasSuffix(t, ">") {
		elem := mapPhpType(strings.TrimSuffix(strings.TrimPrefix(t, "array<"), ">"))
		if elem == "" {
			elem = "any"
		}
		return "list<" + elem + ">"
	}
	switch t {
	case "int":
		return "int"
	case "float":
		return "float"
	case "string":
		return "string"
	case "bool", "boolean":
		return "bool"
	case "array":
		return "list<any>"
	case "void", "mixed":
		return ""
	default:
		return t
	}
}
