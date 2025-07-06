package hs

import "strings"

// Item represents a minimal Haskell AST item parsed from source code.
type Item struct {
	Kind   string   `json:"kind"`
	Name   string   `json:"name,omitempty"`
	Params []string `json:"params,omitempty"`
	Body   string   `json:"body,omitempty"`
}

// Parse parses a very small subset of Haskell source and returns a slice
// of Items describing top level declarations.
func Parse(src string) []Item {
	lines := strings.Split(src, "\n")
	var items []Item
	for i, line := range lines {
		l := strings.TrimSpace(line)
		if strings.HasPrefix(l, "main =") {
			body := strings.TrimSpace(strings.TrimPrefix(l, "main ="))
			if body == "do" && i+1 < len(lines) {
				next := strings.TrimSpace(lines[i+1])
				if strings.HasPrefix(next, "putStrLn") {
					arg := strings.TrimSpace(strings.TrimPrefix(next, "putStrLn"))
					arg = strings.Trim(arg, "()")
					items = append(items, Item{Kind: "print", Body: arg})
				}
				continue
			}
			if strings.HasPrefix(body, "putStrLn") {
				arg := strings.TrimSpace(strings.TrimPrefix(body, "putStrLn"))
				arg = strings.Trim(arg, "()")
				items = append(items, Item{Kind: "print", Body: arg})
			}
			continue
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
			items = append(items, Item{Kind: "func", Name: name, Params: params, Body: body})
		}
	}
	return items
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
		}
	}
	if out.Len() == 0 {
		return nil
	}
	return []byte(out.String())
}
