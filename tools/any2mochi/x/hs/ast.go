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
	Kind   string   `json:"kind"`
	Name   string   `json:"name,omitempty"`
	Params []string `json:"params,omitempty"`
	Body   string   `json:"body,omitempty"`
	Fields []Field  `json:"fields,omitempty"`
	Line   int      `json:"line"`
	Doc    string   `json:"doc,omitempty"`
}

// Parse parses a very small subset of Haskell source and returns a slice
// of Items describing top level declarations. Unsupported lines cause an
// error that includes the line number and snippet for easier debugging.
func Parse(src string) ([]Item, error) {
	lines := strings.Split(src, "\n")
	var items []Item
	var parseErr error
	for i := 0; i < len(lines); i++ {
		line := lines[i]
		l := strings.TrimSpace(line)
		if l == "" || strings.HasPrefix(l, "--") || strings.HasPrefix(l, "module ") || strings.HasPrefix(l, "import ") || strings.HasPrefix(l, "{-") {
			continue
		}
		if strings.HasPrefix(l, "main =") {
			body := strings.TrimSpace(strings.TrimPrefix(l, "main ="))
			if body == "do" && i+1 < len(lines) {
				next := strings.TrimSpace(lines[i+1])
				if strings.HasPrefix(next, "putStrLn") {
					arg := strings.TrimSpace(strings.TrimPrefix(next, "putStrLn"))
					arg = strings.Trim(arg, "()")
					items = append(items, Item{Kind: "print", Body: arg, Line: i + 1})
				}
				i++
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
					items = append(items, Item{Kind: "struct", Name: name, Fields: fs, Line: i + 1})
					i = j
					continue
				}
			}
		}
		if parts := strings.SplitN(l, "=", 2); len(parts) == 2 {
			left := strings.Fields(strings.TrimSpace(parts[0]))
			if len(left) == 0 {
				continue
			}
			name := left[0]
			params := left[1:]
			body := strings.TrimSpace(parts[1])
			if strings.HasPrefix(body, "do") {
				continue
			}
			kind := "func"
			if len(params) == 0 {
				kind = "var"
			}
			items = append(items, Item{Kind: kind, Name: name, Params: params, Body: body, Line: i + 1})
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
			parseErr = fmt.Errorf("line %d: unsupported syntax\n%s", i+1, strings.Join(snippet, "\n"))
		}
	}
	return items, parseErr
}

// convertItems converts the Haskell AST items to Mochi source code.
func convertItems(items []Item) []byte {
	var out strings.Builder
	for _, it := range items {
		switch it.Kind {
		case "print":
			out.WriteString("print(")
			out.WriteString(it.Body)
			out.WriteString(")\n")
		case "func":
			out.WriteString("fun ")
			out.WriteString(it.Name)
			out.WriteByte('(')
			for i, p := range it.Params {
				if i > 0 {
					out.WriteString(", ")
				}
				out.WriteString(p)
			}
			out.WriteByte(')')
			if it.Body == "" {
				out.WriteString(" {}\n")
			} else {
				out.WriteString(" { ")
				out.WriteString(it.Body)
				out.WriteString(" }\n")
			}
		case "var":
			out.WriteString("let ")
			out.WriteString(it.Name)
			if it.Body != "" {
				out.WriteString(" = ")
				out.WriteString(it.Body)
			}
			out.WriteByte('\n')
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
