package dart

import (
	"regexp"
	"strings"
)

type param struct{ name, typ string }

type Func struct {
	Name   string
	Params []param
	Ret    string
	Body   []string
}

func parseParams(s string) []param {
	s = strings.TrimSpace(s)
	if s == "" {
		return nil
	}
	parts := strings.Split(s, ",")
	params := make([]param, 0, len(parts))
	for _, p := range parts {
		p = strings.TrimSpace(p)
		p = strings.TrimPrefix(p, "var ")
		fields := strings.Fields(p)
		if len(fields) == 0 {
			continue
		}
		name := fields[len(fields)-1]
		typ := strings.Join(fields[:len(fields)-1], " ")
		params = append(params, param{name: name, typ: toMochiType(typ)})
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

// parse returns the functions found in src.
func parse(src string) ([]Func, error) {
	re := regexp.MustCompile(`(?m)^\s*([A-Za-z_][\w<>,\[\]\s]*)\s+([A-Za-z_][\w]*)\s*\(([^)]*)\)\s*\{`)
	matches := re.FindAllStringSubmatchIndex(src, -1)
	subs := re.FindAllStringSubmatch(src, -1)
	var funcs []Func
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
		funcs = append(funcs, Func{
			Name:   name,
			Params: parseParams(params),
			Ret:    toMochiType(ret),
			Body:   parseStatements(body[1:]),
		})
	}
	return funcs, nil
}
