package any2mochi

import (
	"fmt"
	"strings"
)

// PasEnum represents a simple Pascal enum type.
type PasEnum struct {
	Name    string   `json:"name"`
	Members []string `json:"members"`
}

// PasVar represents a variable declaration.
type PasVar struct {
	Name string `json:"name"`
	Type string `json:"type,omitempty"`
}

// PasStmt represents a very small subset of Pascal statements.
type PasStmt struct {
	Kind  string `json:"kind"`
	Name  string `json:"name,omitempty"`
	Expr  string `json:"expr,omitempty"`
	Value string `json:"value,omitempty"`
}

// PasProg is a simplified AST used for tests and fallback conversion.
type PasProg struct {
	Types []PasEnum `json:"types,omitempty"`
	Vars  []PasVar  `json:"vars,omitempty"`
	Body  []PasStmt `json:"body,omitempty"`
}

// ParsePasSimple parses a very small subset of Pascal needed for the
// any2mochi fallback converter. It understands enum and variable
// declarations as well as writeln and assignment statements inside the
// main program block.
func ParsePasSimple(src string) (*PasProg, error) {
	lines := strings.Split(src, "\n")
	p := &PasProg{}
	inBody := false
	for i := 0; i < len(lines); i++ {
		l := strings.TrimSpace(lines[i])
		lower := strings.ToLower(l)
		if !inBody {
			switch {
			case strings.HasPrefix(lower, "type") && strings.Contains(l, "("):
				name := ""
				if idx := strings.Index(lower, "type"); idx != -1 {
					rest := strings.TrimSpace(l[idx+len("type"):])
					eq := strings.Index(rest, "=")
					if eq != -1 {
						name = strings.TrimSpace(rest[:eq])
						rest = rest[eq+1:]
					}
					vals := rest
					for !strings.Contains(vals, ")") && i+1 < len(lines) {
						i++
						vals += strings.TrimSpace(lines[i])
					}
					if cIdx := strings.Index(vals, "("); cIdx != -1 {
						vals = vals[cIdx+1:]
					}
					if end := strings.Index(vals, ")"); end != -1 {
						vals = vals[:end]
					}
					var members []string
					for _, part := range strings.Split(vals, ",") {
						v := strings.TrimSpace(strings.TrimSuffix(part, ";"))
						if v != "" {
							members = append(members, v)
						}
					}
					if name != "" && len(members) > 0 {
						p.Types = append(p.Types, PasEnum{Name: name, Members: members})
					}
				}
			case strings.HasPrefix(lower, "var") && strings.Contains(l, ":"):
				rest := strings.TrimSpace(strings.TrimPrefix(l, "var"))
				rest = strings.TrimSuffix(rest, ";")
				if idx := strings.Index(rest, ":"); idx != -1 {
					name := strings.TrimSpace(rest[:idx])
					typ := pasToMochiType(strings.TrimSpace(rest[idx+1:]))
					if name != "" {
						p.Vars = append(p.Vars, PasVar{Name: name, Type: typ})
					}
				}
			case lower == "begin":
				inBody = true
			}
			continue
		}
		if lower == "end." || lower == "end;" {
			inBody = false
			continue
		}
		if l == "" {
			continue
		}
		switch {
		case strings.HasPrefix(lower, "writeln("):
			expr := strings.TrimSuffix(strings.TrimPrefix(l, "writeln("), ");")
			p.Body = append(p.Body, PasStmt{Kind: "writeln", Value: expr})
		case strings.Contains(l, ":="):
			parts := strings.SplitN(l, ":=", 2)
			name := strings.TrimSpace(parts[0])
			expr := strings.TrimSpace(strings.TrimSuffix(parts[1], ";"))
			p.Body = append(p.Body, PasStmt{Kind: "assign", Name: name, Expr: expr})
		default:
			// ignore other statements
		}
	}
	if len(p.Types) == 0 && len(p.Vars) == 0 && len(p.Body) == 0 {
		return nil, fmt.Errorf("convert failure: no convertible content")
	}
	return p, nil
}

// pasProgToMochi converts a parsed Pascal program to Mochi source.
func pasProgToMochi(p *PasProg) []byte {
	var out []string
	for _, t := range p.Types {
		out = append(out, "type "+t.Name+" {")
		for _, m := range t.Members {
			out = append(out, "  "+m)
		}
		out = append(out, "}")
	}
	if len(p.Body) > 0 {
		out = append(out, "fun main() {")
		for _, v := range p.Vars {
			line := "  var " + v.Name
			if v.Type != "" {
				line += ": " + v.Type
			}
			out = append(out, line)
		}
		for _, s := range p.Body {
			switch s.Kind {
			case "writeln":
				val := s.Value
				if strings.HasPrefix(val, "'") && strings.HasSuffix(val, "'") {
					val = "\"" + strings.Trim(val, "'") + "\""
				}
				out = append(out, "  print("+val+")")
			case "assign":
				out = append(out, "  "+s.Name+" = "+s.Expr)
			}
		}
		out = append(out, "}")
	}
	res := strings.Join(out, "\n")
	if !strings.HasSuffix(res, "\n") {
		res += "\n"
	}
	return []byte(res)
}
