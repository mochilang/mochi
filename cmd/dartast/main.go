package main

import (
	"encoding/json"
	"fmt"
	"io/ioutil"
	"os"
	"regexp"
	"strings"
)

var typedVarRe = regexp.MustCompile(`^(?:final|const)?\s*([A-Za-z_][A-Za-z0-9_<>,\[\]\? ]*)\s+([A-Za-z_][A-Za-z0-9_]*)\s*(=.*)?$`)

type dartParam struct {
	Name string `json:"name"`
	Type string `json:"type"`
}

type dartFunc struct {
	Name   string      `json:"name"`
	Params []dartParam `json:"params"`
	Ret    string      `json:"ret"`
	Body   []string    `json:"body"`
	Start  int         `json:"start"`
	End    int         `json:"end"`
	Doc    string      `json:"doc,omitempty"`
}

type ast struct {
	Functions []dartFunc `json:"functions"`
}

func parseParams(s string) []dartParam {
	s = strings.TrimSpace(s)
	if s == "" {
		return nil
	}
	parts := strings.Split(s, ",")
	params := make([]dartParam, 0, len(parts))
	for _, p := range parts {
		p = strings.TrimSpace(p)
		p = strings.TrimPrefix(p, "var ")
		fields := strings.Fields(p)
		if len(fields) == 0 {
			continue
		}
		name := fields[len(fields)-1]
		typ := strings.Join(fields[:len(fields)-1], " ")
		params = append(params, dartParam{Name: name, Type: typ})
	}
	return params
}

func parseStatements(body string) []string {
	lines := strings.Split(body, "\n")
	var out []string
	indent := 1
	for _, line := range lines {
		l := strings.TrimSpace(line)
		l = strings.TrimSuffix(l, ";")
		if l == "" {
			continue
		}
		switch {
		case l == "}":
			if indent > 0 {
				indent--
			}
			out = append(out, strings.Repeat("  ", indent)+"}")
		case strings.HasPrefix(l, "if ") && strings.HasSuffix(l, "{"):
			cond := strings.TrimSpace(strings.TrimSuffix(l[3:], "{"))
			out = append(out, strings.Repeat("  ", indent)+"if "+cond+" {")
			indent++
		case strings.HasPrefix(l, "for ") && strings.HasSuffix(l, "{"):
			head := strings.TrimSpace(strings.TrimSuffix(l[4:], "{"))
			rep := regexp.MustCompile(`var (\w+) = ([^;]+); \w+ < ([^;]+); \w+\+\+`)
			if m := rep.FindStringSubmatch(head); m != nil {
				out = append(out, strings.Repeat("  ", indent)+"for "+m[1]+" in "+m[2]+".."+m[3]+" {")
			} else {
				out = append(out, strings.Repeat("  ", indent)+"for "+head+" {")
			}
			indent++
		case !strings.HasPrefix(l, "var ") && !strings.HasPrefix(l, "return ") && typedVarRe.MatchString(l):
			m := typedVarRe.FindStringSubmatch(l)
			typ := strings.TrimSpace(m[1])
			name := m[2]
			val := strings.TrimSpace(strings.TrimPrefix(m[3], "="))
			stmt := "let " + name
			mt := toMochiType(typ)
			if mt != "" && mt != "any" {
				stmt += ": " + mt
			}
			if strings.Contains(val, ".length") {
				parts := strings.SplitN(val, ".length", 2)
				val = "len(" + strings.TrimSpace(parts[0]) + ")"
			}
			if val != "" {
				stmt += " = " + val
			}
			out = append(out, strings.Repeat("  ", indent)+stmt)
		case strings.HasPrefix(l, "var "):
			stmt := strings.TrimSpace(strings.TrimPrefix(l, "var "))
			if strings.Contains(stmt, "=") && strings.Contains(stmt, ".length") {
				parts := strings.SplitN(stmt, "=", 2)
				name := strings.TrimSpace(parts[0])
				rhs := strings.TrimSpace(strings.TrimSuffix(parts[1], ".length"))
				l = "let " + name + " = len(" + rhs + ")"
			} else {
				l = "let " + stmt
			}
			out = append(out, strings.Repeat("  ", indent)+l)
		case strings.HasPrefix(l, "while ") && strings.HasSuffix(l, "{"):
			cond := strings.TrimSpace(strings.TrimSuffix(l[6:], "{"))
			out = append(out, strings.Repeat("  ", indent)+"while "+cond+" {")
			indent++
		case l == "else {":
			if indent > 0 {
				indent--
			}
			out = append(out, strings.Repeat("  ", indent)+"else {")
			indent++
		case strings.HasPrefix(l, "return "):
			expr := strings.TrimSpace(l[len("return "):])
			out = append(out, strings.Repeat("  ", indent)+"return "+expr)
		default:
			out = append(out, strings.Repeat("  ", indent)+l)
		}
	}
	return out
}

func parse(src string) []dartFunc {
	re := regexp.MustCompile(`(?m)^\s*([A-Za-z_][\w<>,\[\]\s]*)\s+([A-Za-z_][\w]*)\s*\(([^)]*)\)\s*(?:async\*?\s*)?\{`)
	matches := re.FindAllStringSubmatchIndex(src, -1)
	subs := re.FindAllStringSubmatch(src, -1)
	var funcs []dartFunc
	for i, m := range matches {
		if len(subs[i]) < 4 {
			continue
		}
		ret := strings.TrimSpace(subs[i][1])
		name := subs[i][2]
		params := subs[i][3]
		if name == "for" || name == "while" || name == "if" || name == "switch" {
			continue
		}
		start := m[1]
		end := len(src)
		if i+1 < len(matches) {
			end = matches[i+1][0]
		}
		depth := 1
		bodyEnd := end
		for j := start; j < end; j++ {
			if src[j] == '{' {
				depth++
			} else if src[j] == '}' {
				depth--
				if depth == 0 {
					bodyEnd = j
					break
				}
			}
		}
		body := src[start:bodyEnd]
		doc := docBefore(src, m[0])
		startLine := strings.Count(src[:start], "\n") + 1
		endLine := strings.Count(src[:bodyEnd], "\n") + 1
		funcs = append(funcs, dartFunc{
			Name:   name,
			Params: parseParams(params),
			Ret:    strings.TrimSpace(ret),
			Body:   parseStatements(body[1:]),
			Start:  startLine,
			End:    endLine,
			Doc:    doc,
		})
	}
	return funcs
}

func docBefore(src string, idx int) string {
	lines := strings.Split(src[:idx], "\n")
	var doc []string
	for i := len(lines) - 1; i >= 0; i-- {
		l := strings.TrimSpace(lines[i])
		if strings.HasPrefix(l, "///") {
			doc = append([]string{strings.TrimSpace(strings.TrimPrefix(l, "///"))}, doc...)
			continue
		}
		if strings.HasPrefix(l, "//") {
			doc = append([]string{strings.TrimSpace(strings.TrimPrefix(l, "//"))}, doc...)
			continue
		}
		if l == "" {
			continue
		}
		break
	}
	return strings.Join(doc, "\n")
}

func splitArgs(s string) []string {
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

func toMochiType(t string) string {
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
			args := splitArgs(inner)
			switch outer {
			case "List", "Iterable", "Set":
				a := "any"
				if len(args) > 0 {
					if at := toMochiType(args[0]); at != "" {
						a = at
					}
				}
				return "list<" + a + ">"
			case "Map":
				if len(args) == 2 {
					k := toMochiType(args[0])
					if k == "" {
						k = "any"
					}
					v := toMochiType(args[1])
					if v == "" {
						v = "any"
					}
					return "map<" + k + ", " + v + ">"
				}
			case "Future":
				if len(args) == 1 {
					return toMochiType(args[0])
				}
			}
		}
	}
	return t
}

func main() {
	data, err := ioutil.ReadAll(os.Stdin)
	if err != nil {
		fmt.Fprintln(os.Stderr, err)
		os.Exit(1)
	}
	a := ast{Functions: parse(string(data))}
	enc := json.NewEncoder(os.Stdout)
	enc.SetIndent("", "  ")
	if err := enc.Encode(a); err != nil {
		fmt.Fprintln(os.Stderr, err)
		os.Exit(1)
	}
}
