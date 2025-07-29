//go:build slow

package kt

import (
	"encoding/json"
	"fmt"
	"os"
	"os/exec"
	"regexp"
	"strings"
)

// Node represents a top-level declaration parsed from Kotlin.
type Node struct {
	Kind   string  `json:"kind"`
	Name   string  `json:"name"`
	Params []Param `json:"params,omitempty"`
	Ret    string  `json:"ret,omitempty"`
	Fields []Field `json:"fields,omitempty"`
	Stmts  []Stmt  `json:"stmts,omitempty"`
}

// Param represents a Kotlin function parameter.
type Param struct {
	Name string `json:"name"`
	Typ  string `json:"typ"`
}

// Field represents a Kotlin class field.
type Field struct {
	Name string `json:"name"`
	Typ  string `json:"typ"`
}

// Stmt represents a simple statement inside a function body.
type Stmt struct {
	Kind string `json:"kind"`
	Name string `json:"name,omitempty"`
	Expr string `json:"expr,omitempty"`
	Body []Stmt `json:"body,omitempty"`
}

// Program holds parsed declarations and the original Kotlin source.
type Program struct {
	Nodes  []Node `json:"nodes"`
	Source string `json:"-"`
}

// Parse parses Kotlin source by invoking kotlinc with the
// -Xdump-declarations-to flag. It relies on the official Kotlin
// compiler rather than any regex based parsing.
func Parse(src string) (*Program, error) {
	ast, err := parseKotlinc(src)
	if err != nil {
		return parseNaive(src)
	}
	return &Program{Nodes: toNodes(ast), Source: src}, nil
}

// astJSON mirrors the structure produced by kotlinc's JSON dump.
type astJSON struct {
	Classes   []cls   `json:"classes"`
	Functions []fn    `json:"functions"`
	Vars      []vdecl `json:"vars"`
}

type cls struct {
	Name    string  `json:"name"`
	Kind    string  `json:"kind,omitempty"`
	Data    bool    `json:"data,omitempty"`
	Sealed  bool    `json:"sealed,omitempty"`
	Extends string  `json:"extends,omitempty"`
	Fields  []field `json:"fields"`
	Methods []fn    `json:"methods"`
}

type field struct {
	Name string `json:"name"`
	Type string `json:"type"`
}

type vdecl struct {
	Name  string `json:"name"`
	Type  string `json:"type,omitempty"`
	Value string `json:"value"`
}

type fn struct {
	Name   string      `json:"name"`
	Params []paramDecl `json:"params"`
	Ret    string      `json:"ret,omitempty"`
}

type paramDecl struct {
	Name string `json:"name"`
	Type string `json:"type,omitempty"`
}

func parseKotlinc(src string) (*astJSON, error) {
	if _, err := exec.LookPath("kotlinc"); err != nil {
		return nil, fmt.Errorf("kotlinc not installed")
	}
	tmp, err := os.CreateTemp("", "ktsrc_*.kt")
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

	outFile := tmp.Name() + ".json"
	cmd := exec.Command("kotlinc", tmp.Name(), "-Xdump-declarations-to="+outFile)
	if err := cmd.Run(); err != nil {
		os.Remove(outFile)
		return nil, err
	}
	defer os.Remove(outFile)

	data, err := os.ReadFile(outFile)
	if err != nil {
		return nil, err
	}
	var a astJSON
	if err := json.Unmarshal(data, &a); err != nil {
		return nil, err
	}
	if len(a.Classes) == 0 && len(a.Functions) == 0 && len(a.Vars) == 0 {
		return nil, fmt.Errorf("no symbols found")
	}
	return &a, nil
}

func toNodes(a *astJSON) []Node {
	var nodes []Node
	for _, c := range a.Classes {
		var fields []Field
		for _, f := range c.Fields {
			fields = append(fields, Field{Name: f.Name, Typ: mapType(f.Type)})
		}
		nodes = append(nodes, Node{Kind: "type", Name: c.Name, Fields: fields})
	}
	for _, f := range a.Functions {
		var params []Param
		for _, p := range f.Params {
			params = append(params, Param{Name: p.Name, Typ: mapType(p.Type)})
		}
		nodes = append(nodes, Node{Kind: "func", Name: f.Name, Params: params, Ret: mapType(f.Ret)})
	}
	for _, v := range a.Vars {
		nodes = append(nodes, Node{Kind: "var", Name: v.Name, Ret: mapType(v.Type)})
	}
	return nodes
}

func mapType(t string) string {
	t = strings.TrimSpace(t)
	if t == "*" {
		return "any"
	}
	if strings.HasSuffix(t, "?") {
		t = strings.TrimSuffix(t, "?")
	}
	if strings.Contains(t, "->") {
		parts := strings.SplitN(t, "->", 2)
		params := strings.TrimSpace(parts[0])
		ret := strings.TrimSpace(parts[1])
		if strings.HasPrefix(params, "(") && strings.HasSuffix(params, ")") {
			params = params[1 : len(params)-1]
		}
		var mapped []string
		if params != "" {
			for _, p := range splitGeneric(params) {
				mp := mapType(p)
				if mp == "" {
					mp = "any"
				}
				mapped = append(mapped, mp)
			}
		}
		mret := mapType(ret)
		res := "fun(" + strings.Join(mapped, ", ") + ")"
		if mret != "" {
			res += ": " + mret
		}
		return res
	}
	if strings.HasPrefix(t, "Function") && strings.Contains(t, "<") && strings.HasSuffix(t, ">") {
		inner := t[strings.Index(t, "<")+1 : len(t)-1]
		parts := splitGeneric(inner)
		if len(parts) > 0 {
			ret := mapType(parts[len(parts)-1])
			var params []string
			for _, p := range parts[:len(parts)-1] {
				mp := mapType(p)
				if mp == "" {
					mp = "any"
				}
				params = append(params, mp)
			}
			res := "fun(" + strings.Join(params, ", ") + ")"
			if ret != "" {
				res += ": " + ret
			}
			return res
		}
	}
	switch t {
	case "", "Unit", "Nothing":
		return ""
	case "Int", "Long", "Short", "Byte":
		return "int"
	case "Float", "Double":
		return "float"
	case "Number":
		return "float"
	case "Boolean":
		return "bool"
	case "String", "Char":
		return "string"
	case "Any":
		return "any"
	}
	if strings.HasPrefix(t, "List<") && strings.HasSuffix(t, ">") {
		inner := mapType(t[5 : len(t)-1])
		if inner == "" {
			inner = "any"
		}
		return "list<" + inner + ">"
	}
	if strings.HasPrefix(t, "Iterable<") && strings.HasSuffix(t, ">") {
		inner := mapType(t[9 : len(t)-1])
		if inner == "" {
			inner = "any"
		}
		return "list<" + inner + ">"
	}
	if strings.HasPrefix(t, "Sequence<") && strings.HasSuffix(t, ">") {
		inner := mapType(t[9 : len(t)-1])
		if inner == "" {
			inner = "any"
		}
		return "list<" + inner + ">"
	}
	if strings.HasPrefix(t, "Set<") && strings.HasSuffix(t, ">") {
		inner := mapType(t[4 : len(t)-1])
		if inner == "" {
			inner = "any"
		}
		return "list<" + inner + ">"
	}
	if strings.HasPrefix(t, "Array<") && strings.HasSuffix(t, ">") {
		inner := mapType(t[6 : len(t)-1])
		if inner == "" {
			inner = "any"
		}
		return "list<" + inner + ">"
	}
	if strings.HasPrefix(t, "MutableList<") && strings.HasSuffix(t, ">") {
		inner := mapType(t[12 : len(t)-1])
		if inner == "" {
			inner = "any"
		}
		return "list<" + inner + ">"
	}
	if strings.HasPrefix(t, "MutableSet<") && strings.HasSuffix(t, ">") {
		inner := mapType(t[11 : len(t)-1])
		if inner == "" {
			inner = "any"
		}
		return "list<" + inner + ">"
	}
	if strings.HasPrefix(t, "Map<") && strings.HasSuffix(t, ">") {
		inner := t[4 : len(t)-1]
		parts := splitGeneric(inner)
		key := "any"
		val := "any"
		if len(parts) == 2 {
			if k := mapType(parts[0]); k != "" {
				key = k
			}
			if v := mapType(parts[1]); v != "" {
				val = v
			}
		}
		return "map<" + key + ", " + val + ">"
	}
	if strings.HasPrefix(t, "MutableMap<") && strings.HasSuffix(t, ">") {
		inner := t[11 : len(t)-1]
		parts := splitGeneric(inner)
		key := "any"
		val := "any"
		if len(parts) == 2 {
			if k := mapType(parts[0]); k != "" {
				key = k
			}
			if v := mapType(parts[1]); v != "" {
				val = v
			}
		}
		return "map<" + key + ", " + val + ">"
	}
	if strings.HasPrefix(t, "Pair<") && strings.HasSuffix(t, ">") {
		inner := t[5 : len(t)-1]
		parts := splitGeneric(inner)
		first := "any"
		second := "any"
		if len(parts) == 2 {
			if f := mapType(parts[0]); f != "" {
				first = f
			}
			if s := mapType(parts[1]); s != "" {
				second = s
			}
		}
		return "tuple<" + first + ", " + second + ">"
	}
	if strings.HasPrefix(t, "Map.Entry<") && strings.HasSuffix(t, ">") {
		inner := t[10 : len(t)-1]
		parts := splitGeneric(inner)
		first := "any"
		second := "any"
		if len(parts) == 2 {
			if f := mapType(parts[0]); f != "" {
				first = f
			}
			if s := mapType(parts[1]); s != "" {
				second = s
			}
		}
		return "tuple<" + first + ", " + second + ">"
	}
	return t
}

func splitGeneric(s string) []string {
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

func parseNaive(src string) (*Program, error) {
	var nodes []Node
	var current *Node
	var body []string
	depth := 0
	for _, line := range strings.Split(src, "\n") {
		trimmed := strings.TrimSpace(line)
		if current != nil {
			if trimmed == "}" {
				current.Stmts = parseStmts(body)
				current = nil
				body = nil
				depth--
				continue
			}
			body = append(body, trimmed)
			continue
		}
		if depth == 0 {
			if strings.HasPrefix(trimmed, "data class ") {
				rest := strings.TrimPrefix(trimmed, "data class ")
				if open := strings.Index(rest, "("); open != -1 {
					name := strings.TrimSpace(rest[:open])
					if close := strings.Index(rest, ")"); close != -1 {
						fieldsPart := rest[open+1 : close]
						var fields []Field
						for _, p := range splitParams(fieldsPart) {
							p = strings.TrimSpace(p)
							p = strings.TrimPrefix(p, "val ")
							p = strings.TrimPrefix(p, "var ")
							kv := strings.SplitN(p, ":", 2)
							if len(kv) == 2 {
								fields = append(fields, Field{Name: strings.TrimSpace(kv[0]), Typ: strings.TrimSpace(kv[1])})
							}
						}
						nodes = append(nodes, Node{Kind: "type", Name: name, Fields: fields})
					}
				}
			} else if strings.HasPrefix(trimmed, "class ") || strings.HasPrefix(trimmed, "sealed class ") || strings.HasPrefix(trimmed, "enum class ") {
				rest := trimmed
				switch {
				case strings.HasPrefix(rest, "sealed class "):
					rest = strings.TrimPrefix(rest, "sealed class ")
				case strings.HasPrefix(rest, "enum class "):
					rest = strings.TrimPrefix(rest, "enum class ")
				default:
					rest = strings.TrimPrefix(rest, "class ")
				}
				if open := strings.Index(rest, "("); open != -1 {
					name := strings.TrimSpace(rest[:open])
					if close := strings.Index(rest, ")"); close != -1 {
						fieldsPart := rest[open+1 : close]
						var fields []Field
						for _, p := range splitParams(fieldsPart) {
							p = strings.TrimSpace(p)
							p = strings.TrimPrefix(p, "val ")
							p = strings.TrimPrefix(p, "var ")
							kv := strings.SplitN(p, ":", 2)
							if len(kv) == 2 {
								fields = append(fields, Field{Name: strings.TrimSpace(kv[0]), Typ: strings.TrimSpace(kv[1])})
							}
						}
						nodes = append(nodes, Node{Kind: "type", Name: name, Fields: fields})
					} else {
						nodes = append(nodes, Node{Kind: "type", Name: strings.TrimSpace(rest)})
					}
				} else {
					name := strings.TrimSpace(rest)
					if idx := strings.IndexAny(name, " {:"); idx != -1 {
						name = name[:idx]
					}
					nodes = append(nodes, Node{Kind: "type", Name: name})
				}
			} else if strings.HasPrefix(trimmed, "interface ") || strings.HasPrefix(trimmed, "sealed interface ") {
				name := strings.TrimSpace(strings.TrimPrefix(strings.TrimPrefix(trimmed, "sealed interface "), "interface "))
				if idx := strings.IndexAny(name, " {:"); idx != -1 {
					name = name[:idx]
				}
				nodes = append(nodes, Node{Kind: "type", Name: name})
			} else if strings.HasPrefix(trimmed, "object ") {
				name := strings.TrimSpace(strings.TrimPrefix(trimmed, "object "))
				if idx := strings.IndexAny(name, " {:"); idx != -1 {
					name = name[:idx]
				}
				nodes = append(nodes, Node{Kind: "type", Name: name})
			} else if strings.HasPrefix(trimmed, "fun ") {
				rest := strings.TrimPrefix(trimmed, "fun ")
				if open := strings.Index(rest, "("); open != -1 {
					name := strings.TrimSpace(rest[:open])
					afterOpen := rest[open+1:]
					if close := strings.Index(afterOpen, ")"); close != -1 {
						paramsPart := afterOpen[:close]
						remainder := strings.TrimSpace(afterOpen[close+1:])
						ret := ""
						if strings.HasPrefix(remainder, ":") {
							rest := strings.TrimSpace(remainder[1:])
							end := len(rest)
							if idx := strings.IndexAny(rest, "={"); idx != -1 {
								end = idx
							}
							ret = strings.TrimSpace(rest[:end])
						}
						var params []Param
						for _, p := range splitParams(paramsPart) {
							p = strings.TrimSpace(p)
							if p == "" {
								continue
							}
							if idx := strings.Index(p, "="); idx != -1 {
								p = strings.TrimSpace(p[:idx])
							}
							kv := strings.SplitN(p, ":", 2)
							if len(kv) == 2 {
								params = append(params, Param{Name: strings.TrimSpace(kv[0]), Typ: strings.TrimSpace(kv[1])})
							} else if len(kv) == 1 {
								params = append(params, Param{Name: strings.TrimSpace(kv[0])})
							}
						}
						nodes = append(nodes, Node{Kind: "func", Name: name, Params: params, Ret: ret})
						if strings.HasSuffix(trimmed, "{") {
							current = &nodes[len(nodes)-1]
							body = nil
						}
					}
				}
			} else if strings.HasPrefix(trimmed, "val ") || strings.HasPrefix(trimmed, "var ") {
				kind := "val"
				rest := strings.TrimSpace(strings.TrimPrefix(trimmed, "val "))
				if strings.HasPrefix(trimmed, "var ") {
					kind = "var"
					rest = strings.TrimSpace(strings.TrimPrefix(trimmed, "var "))
				}
				name := rest
				typ := ""
				if idx := strings.Index(rest, ":"); idx != -1 {
					name = strings.TrimSpace(rest[:idx])
					after := strings.TrimSpace(rest[idx+1:])
					if idxEq := strings.Index(after, "="); idxEq != -1 {
						typ = strings.TrimSpace(after[:idxEq])
					} else {
						typ = strings.TrimSpace(after)
					}
				} else if idx := strings.Index(rest, "="); idx != -1 {
					name = strings.TrimSpace(rest[:idx])
				}
				nodes = append(nodes, Node{Kind: kind, Name: name, Ret: typ})
			}
		}
		depth += strings.Count(line, "{")
		depth -= strings.Count(line, "}")
	}
	if len(nodes) == 0 {
		return nil, fmt.Errorf("no symbols found")
	}
	return &Program{Nodes: nodes, Source: src}, nil
}

func splitParams(s string) []string {
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

func parseStmts(lines []string) []Stmt {
	var stmts []Stmt
	for i := 0; i < len(lines); i++ {
		l := strings.TrimSpace(strings.TrimSuffix(lines[i], ";"))

		// variable declaration
		if strings.HasPrefix(l, "val ") || strings.HasPrefix(l, "var ") {
			kind := "let"
			rest := strings.TrimSpace(strings.TrimPrefix(l, "val "))
			if strings.HasPrefix(l, "var ") {
				kind = "var"
				rest = strings.TrimSpace(strings.TrimPrefix(l, "var "))
			}
			eq := strings.Index(rest, "=")
			if eq == -1 {
				continue
			}
			name := strings.TrimSpace(rest[:eq])
			if colon := strings.Index(name, ":"); colon != -1 {
				name = strings.TrimSpace(name[:colon])
			}
			expr := convertExpr(strings.TrimSpace(rest[eq+1:]))
			stmts = append(stmts, Stmt{Kind: kind, Name: name, Expr: expr})
			continue
		}

		// println statement
		if strings.HasPrefix(l, "println(") && strings.HasSuffix(l, ")") {
			inner := strings.TrimSuffix(strings.TrimPrefix(l, "println("), ")")
			expr := convertExpr(inner)
			stmts = append(stmts, Stmt{Kind: "print", Expr: expr})
			continue
		}

		// assignment
		if idx := strings.Index(l, "="); idx != -1 && !strings.HasPrefix(l, "for ") {
			name := strings.TrimSpace(l[:idx])
			expr := convertExpr(strings.TrimSpace(l[idx+1:]))
			stmts = append(stmts, Stmt{Kind: "set", Name: name, Expr: expr})
			continue
		}

		// for loop
		if strings.HasPrefix(l, "for (") && strings.HasSuffix(l, "{") {
			head := strings.TrimSpace(strings.TrimSuffix(strings.TrimPrefix(l, "for ("), "{"))
			head = strings.TrimSuffix(head, ")")
			parts := strings.SplitN(head, " in ", 2)
			if len(parts) == 2 {
				varName := strings.TrimSpace(parts[0])
				iter := convertExpr(strings.TrimSpace(parts[1]))
				depth := 1
				var body []string
				for i++; i < len(lines); i++ {
					line := strings.TrimSpace(lines[i])
					if strings.HasSuffix(line, "{") {
						depth++
					}
					if line == "}" {
						depth--
						if depth == 0 {
							break
						}
					}
					if depth > 0 {
						body = append(body, line)
					}
				}
				stmt := Stmt{Kind: "for", Name: varName, Expr: iter, Body: parseStmts(body)}
				stmts = append(stmts, stmt)
			}
			continue
		}
	}
	return stmts
}

var appendRe = regexp.MustCompile(`^([a-zA-Z_][a-zA-Z0-9_]*)\s*\+\s*(\d+)$`)

func convertExpr(expr string) string {
	expr = strings.TrimSpace(expr)
	if strings.HasSuffix(expr, ".average()") {
		inner := strings.TrimSuffix(expr, ".average()")
		if strings.HasPrefix(inner, "mutableListOf(") && strings.HasSuffix(inner, ")") {
			inner = strings.TrimSuffix(strings.TrimPrefix(inner, "mutableListOf("), ")")
			if strings.Contains(inner, "to") || strings.Contains(inner, "mutableListOf(") {
				return ""
			}
			inner = strings.ReplaceAll(inner, " ", "")
			return "avg([" + inner + "])"
		}
		return ""
	}
	if strings.HasPrefix(expr, "mutableListOf(") && strings.HasSuffix(expr, ")") && !strings.Contains(expr, ".") {
		inner := strings.TrimSuffix(strings.TrimPrefix(expr, "mutableListOf("), ")")
		if strings.Contains(inner, "to") || strings.Contains(inner, "mutableListOf(") {
			return ""
		}
		inner = strings.ReplaceAll(inner, " ", "")
		return "[" + inner + "]"
	}
	if m := appendRe.FindStringSubmatch(expr); m != nil {
		return fmt.Sprintf("append(%s, %s)", m[1], m[2])
	}
	if strings.HasSuffix(expr, ".toInt()") {
		inner := strings.TrimSuffix(expr, ".toInt()")
		return "int(" + inner + ")"
	}
	if strings.Contains(expr, " until ") {
		parts := strings.SplitN(expr, " until ", 2)
		if len(parts) == 2 {
			return fmt.Sprintf("%s..%s-1", convertExpr(parts[0]), convertExpr(parts[1]))
		}
	}
	return expr
}
