//go:build slow

package hs

import (
	"fmt"
	"strings"
)

// Field describes a record field in a data declaration.
type Field struct {
	Name string `json:"name"`
	Type string `json:"type"`
}

// Item represents a minimal Haskell AST item parsed from source code.
type Item struct {
	Kind       string   `json:"kind"`
	Name       string   `json:"name,omitempty"`
	Params     []string `json:"params,omitempty"`
	Body       string   `json:"body,omitempty"`
	Type       string   `json:"type,omitempty"`
	Fields     []Field  `json:"fields,omitempty"`
	Collection string   `json:"collection,omitempty"`
	Start      string   `json:"start,omitempty"`
	End        string   `json:"end,omitempty"`
	Line       int      `json:"line"`
	Doc        string   `json:"doc,omitempty"`
	Derives    []string `json:"derives,omitempty"`
}

// Parse parses a very small subset of Haskell source and returns a slice
// of Items describing top level declarations. Unsupported lines cause an
// error that includes the line number and snippet for easier debugging.
func Parse(src string) ([]Item, error) {
	lines := strings.Split(src, "\n")
	var items []Item
	var parseErr error
	sigs := make(map[string]string)
	for i := 0; i < len(lines); i++ {
		line := lines[i]
		l := strings.TrimSpace(line)
		if strings.HasPrefix(line, " ") || strings.HasPrefix(line, "\t") {
			continue
		}
		if l == "" || strings.HasPrefix(l, "--") || strings.HasPrefix(l, "module ") || strings.HasPrefix(l, "import ") || strings.HasPrefix(l, "{-") {
			continue
		}
		if parts := strings.SplitN(l, "::", 2); len(parts) == 2 {
			name := strings.TrimSpace(parts[0])
			sigs[name] = strings.TrimSpace(parts[1])
			continue
		}
		if strings.HasPrefix(l, "main =") {
			body := strings.TrimSpace(strings.TrimPrefix(l, "main ="))
			if body == "do" {
				j := i + 1
				for j < len(lines) {
					ln := lines[j]
					if strings.TrimSpace(ln) == "" {
						j++
						continue
					}
					if !strings.HasPrefix(ln, " ") && !strings.HasPrefix(ln, "\t") {
						break
					}
					t := strings.TrimSpace(ln)
					if strings.HasPrefix(t, "print") {
						arg := strings.TrimSpace(strings.TrimPrefix(t, "print"))
						arg = strings.Trim(arg, "()")
						items = append(items, Item{Kind: "print", Body: arg, Line: j + 1})
					} else if strings.HasPrefix(t, "putStrLn") {
						arg := strings.TrimSpace(strings.TrimPrefix(t, "putStrLn"))
						arg = strings.Trim(arg, "()")
						if strings.HasPrefix(arg, "show ") {
							arg = "str(" + strings.TrimSpace(strings.TrimPrefix(arg, "show ")) + ")"
						}
						items = append(items, Item{Kind: "print", Body: arg, Line: j + 1})
					} else if strings.HasPrefix(t, "_json") {
						arg := strings.TrimSpace(strings.TrimPrefix(t, "_json"))
						arg = strings.Trim(arg, "()")
						items = append(items, Item{Kind: "json", Body: arg, Line: j + 1})
					} else if strings.HasPrefix(t, "mapM_") {
						if it, ok := parseMapM(t, j+1); ok {
							items = append(items, it)
						}
					}
					j++
				}
				i = j - 1
				continue
			}
			if strings.HasPrefix(body, "putStrLn") {
				arg := strings.TrimSpace(strings.TrimPrefix(body, "putStrLn"))
				arg = strings.Trim(arg, "()")
				items = append(items, Item{Kind: "print", Body: arg, Line: i + 1})
			}
			continue
		}
		if strings.HasPrefix(l, "data ") && strings.Contains(l, "{") {
			parts := strings.Fields(strings.TrimSpace(strings.TrimPrefix(l, "data ")))
			if len(parts) > 0 {
				name := parts[0]
				// capture lines until closing brace
				j := i
				buf := l
				for j+1 < len(lines) && !strings.Contains(lines[j], "}") {
					j++
					buf += "\n" + strings.TrimSpace(lines[j])
				}
				if end := strings.Index(buf, "}"); end != -1 {
					start := strings.Index(buf, "{")
					fields := strings.Split(buf[start+1:end], ",")
					var fs []Field
					for _, f := range fields {
						f = strings.TrimSpace(f)
						if f == "" {
							continue
						}
						parts := strings.Fields(f)
						if len(parts) >= 3 && parts[1] == "::" {
							fs = append(fs, Field{Name: parts[0], Type: parts[2]})
						}
					}
					var derives []string
					rest := strings.TrimSpace(buf[end+1:])
					if rest == "" && j+1 < len(lines) {
						next := strings.TrimSpace(lines[j+1])
						if strings.HasPrefix(next, "deriving") {
							rest = next
							j++
						}
					}
					if strings.HasPrefix(rest, "deriving") {
						list := rest[len("deriving"):]
						list = strings.TrimSpace(strings.Trim(list, "()"))
						parts := strings.Split(list, ",")
						for _, p := range parts {
							p = strings.TrimSpace(p)
							if p != "" {
								derives = append(derives, p)
							}
						}
					}
					items = append(items, Item{Kind: "struct", Name: name, Fields: fs, Line: i + 1, Derives: derives})
					i = j
					continue
				}
			}
		}
		if strings.HasPrefix(l, "data ") || strings.HasPrefix(l, "newtype ") || strings.HasPrefix(l, "type ") || strings.HasPrefix(l, "instance ") {
			continue
		}
		if strings.HasPrefix(l, "|") || strings.HasPrefix(l, "case ") || strings.HasPrefix(l, "of ") || l == "where" {
			continue
		}
		if parts := strings.SplitN(l, "=", 2); len(parts) == 2 {
			left := strings.Fields(strings.TrimSpace(parts[0]))
			if len(left) == 0 {
				continue
			}
			name := left[0]
			switch name {
			case "forLoop", "whileLoop", "avg", "_group_by", "_indexString", "_input", "_now", "_json", "_readInput", "_writeOutput", "_split", "_parseCSV", "_load", "_save",
				"_asInt", "_asDouble", "_asString", "_asBool", "_parseJSON", "_valueToMap", "_valueToString", "_mapToValue":
				continue
			}
			params := left[1:]
			body := strings.TrimSpace(parts[1])
			if strings.HasPrefix(body, "do") {
				continue
			}
			kind := "func"
			if len(params) == 0 {
				kind = "var"
			}
			it := Item{Kind: kind, Name: name, Params: params, Body: body, Line: i + 1}
			if t, ok := sigs[name]; ok {
				it.Type = t
			}
			items = append(items, it)
			continue
		}
		if parseErr == nil {
			start := i - 1
			if start < 0 {
				start = 0
			}
			end := i + 1
			if end >= len(lines) {
				end = len(lines) - 1
			}
			var snippet []string
			for j := start; j <= end; j++ {
				prefix := "   "
				if j == i {
					prefix = "-> "
				}
				snippet = append(snippet, fmt.Sprintf("%s%3d | %s", prefix, j+1, lines[j]))
			}
			parseErr = fmt.Errorf("line %d: unsupported syntax: %s\n%s", i+1, strings.TrimSpace(lines[i]), strings.Join(snippet, "\n"))
		}
	}
	return items, parseErr
}

// parseMapM parses a line using mapM_ loop syntax and returns an Item representing a for loop.
func parseMapM(line string, lineNum int) (Item, bool) {
	l := strings.TrimSpace(strings.TrimPrefix(line, "mapM_"))
	if !strings.HasPrefix(l, "(\\") {
		return Item{}, false
	}
	idx := strings.Index(l, "->")
	if idx == -1 {
		return Item{}, false
	}
	varName := strings.TrimSpace(l[2:idx])
	rest := strings.TrimSpace(l[idx+2:])
	close := -1
	for i := 0; i < len(rest); i++ {
		if rest[i] == ')' {
			if i+1 >= len(rest) || rest[i+1] == ' ' || rest[i+1] == '(' || rest[i+1] == '[' {
				close = i
				break
			}
		}
	}
	if close == -1 {
		close = strings.LastIndex(rest, ")")
	}
	if close == -1 {
		return Item{}, false
	}
	body := strings.TrimSpace(rest[:close])
	src := strings.TrimSpace(rest[close+1:])
	if strings.HasPrefix(src, "(") && strings.HasSuffix(src, ")") {
		src = strings.TrimSuffix(strings.TrimPrefix(src, "("), ")")
	}
	// handle range form [a .. b - 1]
	if strings.HasPrefix(src, "[") && strings.HasSuffix(src, "]") {
		s := strings.TrimSuffix(strings.TrimPrefix(src, "["), "]")
		if parts := strings.SplitN(s, "..", 2); len(parts) == 2 {
			start := strings.TrimSpace(parts[0])
			end := strings.TrimSpace(parts[1])
			end = strings.TrimSuffix(end, "- 1")
			end = strings.TrimSpace(end)
			return Item{Kind: "for", Name: varName, Start: start, End: end, Body: convertBody(body), Line: lineNum}, true
		}
	}
	if strings.HasPrefix(src, "Map.keys ") {
		src = strings.TrimPrefix(src, "Map.keys ")
	}
	return Item{Kind: "for", Name: varName, Collection: src, Body: convertBody(body), Line: lineNum}, true
}

func convertBody(body string) string {
	if strings.HasPrefix(body, "print") {
		arg := strings.TrimSpace(strings.TrimPrefix(body, "print"))
		arg = strings.Trim(arg, "()")
		return "print(" + convertExpr(arg) + ")"
	}
	if strings.HasPrefix(body, "putStrLn") {
		arg := strings.TrimSpace(strings.TrimPrefix(body, "putStrLn"))
		arg = strings.Trim(arg, "()")
		if strings.HasPrefix(arg, "show ") {
			arg = "str(" + strings.TrimSpace(strings.TrimPrefix(arg, "show ")) + ")"
		}
		return "print(" + convertExpr(arg) + ")"
	}
	return convertExpr(body)
}

func convertExpr(expr string) string {
	expr = strings.TrimSpace(expr)
	if strings.HasPrefix(expr, "[") && strings.HasSuffix(expr, "]") {
		return expr
	}
	if strings.HasPrefix(expr, "div ") {
		parts := strings.Fields(expr)
		if len(parts) == 3 {
			return parts[1] + " / " + parts[2]
		}
	}
	if strings.Contains(expr, "`mod`") {
		parts := strings.Split(expr, "`mod`")
		if len(parts) == 2 {
			return strings.TrimSpace(parts[0]) + " % " + strings.TrimSpace(parts[1])
		}
	}
	expr = strings.ReplaceAll(expr, "True", "true")
	expr = strings.ReplaceAll(expr, "False", "false")
	parts := strings.Fields(expr)
	if len(parts) > 1 {
		ops := map[string]bool{"+": true, "-": true, "*": true, "/": true, "%": true, "&&": true, "||": true, "==": true, "<": true, ">": true, "<=": true, ">=": true}
		if !ops[parts[1]] {
			for i := 1; i < len(parts); i++ {
				parts[i] = strings.TrimSuffix(parts[i], ",")
			}
			return parts[0] + "(" + strings.Join(parts[1:], ", ") + ")"
		}
	}
	return expr
}

// convertItems converts the Haskell AST items to Mochi source code.
func convertItems(items []Item) []byte {
	var out strings.Builder
	for _, it := range items {
		switch it.Kind {
		case "print":
			out.WriteString("print(")
			out.WriteString(convertExpr(it.Body))
			out.WriteString(")\n")
		case "json":
			out.WriteString("json(")
			out.WriteString(convertExpr(it.Body))
			out.WriteString(")\n")
		case "func":
			out.WriteString("fun ")
			out.WriteString(it.Name)
			var paramTypes []string
			var retType string
			if it.Type != "" {
				paramTypes, retType = parseSigTypes(it.Type)
			}
			out.WriteByte('(')
			for i, p := range it.Params {
				if i > 0 {
					out.WriteString(", ")
				}
				out.WriteString(p)
				if i < len(paramTypes) && paramTypes[i] != "" {
					out.WriteString(": ")
					out.WriteString(paramTypes[i])
				}
			}
			out.WriteByte(')')
			if retType != "" {
				out.WriteString(": ")
				out.WriteString(retType)
			}
			if it.Body == "" {
				out.WriteString(" {}\n")
			} else {
				out.WriteString(" { ")
				out.WriteString(convertExpr(it.Body))
				out.WriteString(" }\n")
			}
		case "var":
			out.WriteString("let ")
			out.WriteString(it.Name)
			if it.Type != "" {
				if typ, ok := parseVarSig(it.Type); ok {
					out.WriteString(": ")
					out.WriteString(typ)
				}
			}
			if it.Body != "" {
				out.WriteString(" = ")
				out.WriteString(convertExpr(it.Body))
			}
			out.WriteByte('\n')
		case "for":
			out.WriteString("for ")
			out.WriteString(it.Name)
			out.WriteString(" in ")
			if it.Collection != "" {
				out.WriteString(it.Collection)
			} else {
				out.WriteString(it.Start)
				out.WriteString("..")
				out.WriteString(it.End)
			}
			out.WriteString(" { ")
			out.WriteString(convertExpr(it.Body))
			out.WriteString(" }\n")
		case "struct":
			out.WriteString("type ")
			out.WriteString(it.Name)
			if len(it.Fields) == 0 {
				out.WriteString(" {}\n")
				break
			}
			out.WriteString(" {\n")
			for _, f := range it.Fields {
				out.WriteString("  ")
				out.WriteString(f.Name)
				if f.Type != "" {
					out.WriteString(": ")
					out.WriteString(mapType(f.Type))
				}
				out.WriteByte('\n')
			}
			out.WriteString("}\n")
		}
	}
	if out.Len() == 0 {
		return nil
	}
	return []byte(out.String())
}
