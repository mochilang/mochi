//go:build slow

package c

import (
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"regexp"
	"strings"
	"time"

	"mochi/ast"
	"mochi/parser"
)

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

// ConvertSource converts a C source string to Mochi source using clang.
func ConvertSource(src string) (string, error) {
	if _, err := exec.LookPath("clang"); err != nil {
		return "", fmt.Errorf("clang not installed")
	}
	out, err := convertSimple(src)
	if err != nil {
		return "", err
	}

	var b strings.Builder
	b.WriteString(header())
	b.WriteString("/*\n")
	b.WriteString(src)
	if !strings.HasSuffix(src, "\n") {
		b.WriteByte('\n')
	}
	b.WriteString("*/\n")
	b.Write(out)
	return b.String(), nil
}

// Convert parses the generated Mochi source into an AST node.
func Convert(src string) (*ast.Node, error) {
	code, err := ConvertSource(src)
	if err != nil {
		return nil, err
	}
	prog, err := parser.ParseString(code)
	if err != nil {
		return nil, err
	}
	return ast.FromProgram(prog), nil
}

// ConvertFile reads a C file and converts it to Mochi source.
func ConvertFile(path string) (string, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return "", err
	}
	return ConvertSource(string(data))
}

// --- Internal helpers below ---

// convertSimple converts C code by invoking clang and translating a limited AST.
func convertSimple(src string) ([]byte, error) {
	structs := parseStructs(src)
	funcs, err := parseClangFile(src)
	if err != nil {
		return nil, fmt.Errorf("%v\n\nsource snippet:\n%s", err, snippet(src))
	}
	if len(funcs) == 0 {
		return nil, fmt.Errorf("no convertible symbols found\n\nsource snippet:\n%s", snippet(src))
	}
	var out strings.Builder
	for _, s := range structs {
		out.WriteString(s)
		out.WriteByte('\n')
	}
	for _, fn := range funcs {
		if fn.name == "main" {
			for _, ln := range fn.body {
				trimmed := strings.TrimSpace(ln)
				if strings.HasPrefix(trimmed, "return ") || trimmed == "return" {
					continue
				}
				out.WriteString(trimmed)
				out.WriteByte('\n')
			}
			continue
		}
		out.WriteString("fun ")
		out.WriteString(fn.name)
		out.WriteByte('(')
		for i, p := range fn.params {
			if i > 0 {
				out.WriteString(", ")
			}
			name := p.name
			if name == "" {
				name = fmt.Sprintf("a%d", i)
			}
			out.WriteString(name)
			if p.typ != "" {
				out.WriteString(": ")
				out.WriteString(p.typ)
			}
		}
		out.WriteByte(')')
		if fn.ret != "" && fn.ret != "void" {
			out.WriteString(": ")
			out.WriteString(fn.ret)
		}
		if len(fn.body) == 0 {
			out.WriteString(" {}\n")
		} else {
			out.WriteString(" {\n")
			for _, ln := range fn.body {
				out.WriteString(ln)
				out.WriteByte('\n')
			}
			out.WriteString("}\n")
		}
	}
	return []byte(out.String()), nil
}

var castRE = regexp.MustCompile(`\([a-zA-Z_][a-zA-Z0-9_\s]*\*\)`) // matches C casts like (int *)
var functionPtrRE = regexp.MustCompile(`^.*\(\*\s*([A-Za-z_][A-Za-z0-9_]*)\s*\)\s*\([^)]*\)\s*=\s*(.*)$`)
var funcPtrTypeAnonRE = regexp.MustCompile(`^(.+)\(\*\)\s*\((.*)\)$`)
var funcPtrTypeNamedRE = regexp.MustCompile(`^(.+)\(\*\s*[A-Za-z_][A-Za-z0-9_]*\s*\)\s*\((.*)\)$`)
var forRangeRE = regexp.MustCompile(`^for\s*\((?:[A-Za-z_][A-Za-z0-9_]*\s+)?([A-Za-z_][A-Za-z0-9_]*)\s*=\s*([^;]+);\s*([A-Za-z_][A-Za-z0-9_]*)\s*(<|<=)\s*([^;]+);\s*([A-Za-z_][A-Za-z0-9_]*)\+\+\s*\)$`)

func stripCasts(s string) string { return castRE.ReplaceAllString(s, "") }

func parseParam(p string) (string, string) {
	if p == "" || p == "..." {
		return "", ""
	}
	fields := strings.Fields(p)
	if len(fields) == 1 {
		return "", mapType(fields[0])
	}
	name := fields[len(fields)-1]
	typ := strings.Join(fields[:len(fields)-1], " ")
	if strings.ContainsAny(name, "*[]") || strings.Contains(name, "[") {
		return "", mapType(p)
	}
	return name, mapType(typ)
}

func parseParamTypes(params string) []string {
	params = strings.TrimSpace(params)
	if params == "" || params == "void" {
		return nil
	}
	parts := strings.Split(params, ",")
	out := make([]string, 0, len(parts))
	for _, p := range parts {
		_, t := parseParam(strings.TrimSpace(p))
		if t == "" {
			t = "any"
		}
		out = append(out, t)
	}
	return out
}

func mapType(typ string) string {
	typ = strings.TrimSpace(typ)
	if m := funcPtrTypeNamedRE.FindStringSubmatch(typ); m != nil {
		ret := mapType(strings.TrimSpace(m[1]))
		params := parseParamTypes(m[2])
		if len(params) == 0 {
			if ret == "" {
				return "fun()"
			}
			return fmt.Sprintf("fun(): %s", ret)
		}
		if ret == "" {
			return fmt.Sprintf("fun(%s)", strings.Join(params, ", "))
		}
		return fmt.Sprintf("fun(%s): %s", strings.Join(params, ", "), ret)
	}
	if m := funcPtrTypeAnonRE.FindStringSubmatch(typ); m != nil {
		ret := mapType(strings.TrimSpace(m[1]))
		params := parseParamTypes(m[2])
		if len(params) == 0 {
			if ret == "" {
				return "fun()"
			}
			return fmt.Sprintf("fun(): %s", ret)
		}
		if ret == "" {
			return fmt.Sprintf("fun(%s)", strings.Join(params, ", "))
		}
		return fmt.Sprintf("fun(%s): %s", strings.Join(params, ", "), ret)
	}
	if open := strings.Index(typ, "["); open != -1 && strings.HasSuffix(typ, "]") {
		base := strings.TrimSpace(typ[:open])
		inner := mapType(base)
		if inner == "" {
			inner = "any"
		}
		return "list<" + inner + ">"
	}
	for strings.HasSuffix(typ, "*") {
		typ = strings.TrimSpace(strings.TrimSuffix(typ, "*"))
	}
	typ = strings.TrimPrefix(typ, "static")
	typ = strings.TrimPrefix(strings.TrimSpace(typ), "const")
	typ = strings.TrimPrefix(strings.TrimSpace(typ), "unsigned")
	typ = strings.TrimPrefix(strings.TrimSpace(typ), "signed")
	typ = strings.TrimPrefix(strings.TrimSpace(typ), "struct")
	typ = strings.TrimSpace(typ)
	switch typ {
	case "", "void":
		return ""
	case "bool":
		return "bool"
	case "int", "size_t", "long", "short":
		return "int"
	case "long long":
		return "int"
	case "float", "double":
		return "float"
	case "char":
		return "string"
	default:
		if strings.HasPrefix(typ, "list_") {
			inner := mapType(strings.TrimPrefix(typ, "list_"))
			if inner == "" {
				inner = "any"
			}
			return "list<" + inner + ">"
		}
		if strings.HasPrefix(typ, "map_") {
			inner := strings.TrimPrefix(typ, "map_")
			parts := strings.SplitN(inner, "_", 2)
			if len(parts) == 2 {
				k := mapType(parts[0])
				v := mapType(parts[1])
				if k == "" {
					k = "any"
				}
				if v == "" {
					v = "any"
				}
				return "map<" + k + ", " + v + ">"
			}
		}
		return typ
	}
}

var structRE = regexp.MustCompile(`(?s)typedef\s+(struct|union)\s*\{([^}]*)\}\s*([A-Za-z_][A-Za-z0-9_]*)\s*;`)

func parseStructs(src string) []string {
	matches := structRE.FindAllStringSubmatch(src, -1)
	var out []string
	for _, m := range matches {
		kind := m[1]
		body := m[2]
		name := strings.TrimSpace(m[3])
		var b strings.Builder
		b.WriteString("type ")
		b.WriteString(name)
		if kind == "union" {
			b.WriteString(" union {")
		} else {
			b.WriteString(" {")
		}
		b.WriteByte('\n')
		for _, line := range strings.Split(body, "\n") {
			line = strings.TrimSpace(line)
			if line == "" {
				continue
			}
			fields := strings.Fields(strings.TrimSuffix(line, ";"))
			if len(fields) < 2 {
				continue
			}
			fieldName := fields[len(fields)-1]
			fieldName = strings.TrimLeft(fieldName, "*")
			fieldType := mapType(strings.Join(fields[:len(fields)-1], " "))
			b.WriteString("  ")
			b.WriteString(fieldName)
			if fieldType != "" {
				b.WriteString(": ")
				b.WriteString(fieldType)
			}
			b.WriteByte('\n')
		}
		b.WriteString("}\n")
		out = append(out, b.String())
	}
	return out
}

func parseStatements(body string) []string {
	var out []string
	indent := 1
	for _, line := range strings.Split(body, "\n") {
		l := strings.TrimSpace(line)
		if l == "" {
			continue
		}
		switch {
		case l == "{":
			out = append(out, strings.Repeat("  ", indent)+"{")
			indent++
		case l == "}":
			indent--
			if indent < 0 {
				indent = 0
			}
			out = append(out, strings.Repeat("  ", indent)+"}")
		case strings.HasPrefix(l, "for ") || strings.HasPrefix(l, "for("):
			hasBrace := strings.HasSuffix(l, "{")
			header := strings.TrimSpace(l)
			if hasBrace {
				header = strings.TrimSpace(strings.TrimSuffix(header, "{"))
			}
			if m := forRangeRE.FindStringSubmatch(header); m != nil {
				if m[1] == m[3] && m[1] == m[6] {
					varName := m[1]
					start := strings.TrimSpace(m[2])
					op := m[4]
					end := strings.TrimSpace(m[5])
					if op == "<=" {
						end = end + " + 1"
					}
					stmt := fmt.Sprintf("for %s in range(%s, %s) {", varName, start, end)
					out = append(out, strings.Repeat("  ", indent)+stmt)
					indent++
				} else if hasBrace {
					out = append(out, strings.Repeat("  ", indent)+header+" {")
					indent++
				} else {
					out = append(out, strings.Repeat("  ", indent)+l)
				}
			} else if hasBrace {
				out = append(out, strings.Repeat("  ", indent)+header+" {")
				indent++
			} else {
				out = append(out, strings.Repeat("  ", indent)+l)
			}
		case strings.HasPrefix(l, "while ") || strings.HasPrefix(l, "while("):
			if strings.HasSuffix(l, "{") {
				h := strings.TrimSpace(strings.TrimSuffix(l, "{"))
				h = strings.TrimPrefix(h, "while")
				h = strings.TrimSpace(h)
				for strings.HasPrefix(h, "(") && strings.HasSuffix(h, ")") {
					h = strings.TrimPrefix(h, "(")
					h = strings.TrimSuffix(h, ")")
					h = strings.TrimSpace(h)
				}
				out = append(out, strings.Repeat("  ", indent)+"while "+h+" {")
				indent++
			} else {
				out = append(out, strings.Repeat("  ", indent)+l)
			}
		case strings.HasPrefix(l, "if ") || strings.HasPrefix(l, "if("):
			if strings.HasSuffix(l, "{") {
				h := strings.TrimSpace(strings.TrimSuffix(l, "{"))
				h = strings.TrimPrefix(h, "if")
				h = strings.TrimSpace(h)
				for strings.HasPrefix(h, "(") && strings.HasSuffix(h, ")") {
					h = strings.TrimPrefix(h, "(")
					h = strings.TrimSuffix(h, ")")
					h = strings.TrimSpace(h)
				}
				if strings.HasPrefix(h, "!(") && strings.HasSuffix(h, ")") {
					inner := strings.TrimSuffix(strings.TrimPrefix(h, "!("), ")")
					h = "!" + strings.TrimSpace(inner)
				}
				out = append(out, strings.Repeat("  ", indent)+"if "+h+" {")
				indent++
			} else {
				out = append(out, strings.Repeat("  ", indent)+l)
			}
		case strings.HasPrefix(l, "else {"):
			indent--
			if indent < 0 {
				indent = 0
			}
			out = append(out, strings.Repeat("  ", indent)+"else {")
			indent++
		case strings.HasPrefix(l, "return "):
			expr := strings.TrimSuffix(strings.TrimPrefix(l, "return "), ";")
			out = append(out, strings.Repeat("  ", indent)+"return "+expr)
		case l == "continue;":
			out = append(out, strings.Repeat("  ", indent)+"continue")
		case l == "break;":
			out = append(out, strings.Repeat("  ", indent)+"break")
		case strings.HasPrefix(l, "printf("):
			args := strings.TrimSuffix(strings.TrimPrefix(l, "printf("), ");")
			parts := strings.SplitN(args, ",", 2)
			arg := strings.TrimSpace(args)
			if len(parts) == 2 {
				arg = strings.TrimSpace(parts[1])
			}
			out = append(out, strings.Repeat("  ", indent)+"print("+arg+")")
		case strings.HasPrefix(l, "puts("):
			arg := strings.TrimSuffix(strings.TrimPrefix(l, "puts("), ");")
			out = append(out, strings.Repeat("  ", indent)+"print("+strings.TrimSpace(arg)+")")
		default:
			if strings.HasSuffix(l, ";") {
				l = strings.TrimSuffix(l, ";")
			}
			l = stripCasts(l)
			switch {
			case strings.HasSuffix(l, "++"):
				v := strings.TrimSpace(strings.TrimSuffix(l, "++"))
				out = append(out, strings.Repeat("  ", indent)+v+" = "+v+" + 1")
			case strings.HasSuffix(l, "--"):
				v := strings.TrimSpace(strings.TrimSuffix(l, "--"))
				out = append(out, strings.Repeat("  ", indent)+v+" = "+v+" - 1")
			case strings.Contains(l, "+="):
				parts := strings.SplitN(l, "+=", 2)
				v := strings.TrimSpace(parts[0])
				val := strings.TrimSpace(parts[1])
				out = append(out, strings.Repeat("  ", indent)+v+" = "+v+" + "+val)
			case strings.Contains(l, "-="):
				parts := strings.SplitN(l, "-=", 2)
				v := strings.TrimSpace(parts[0])
				val := strings.TrimSpace(parts[1])
				out = append(out, strings.Repeat("  ", indent)+v+" = "+v+" - "+val)
			case strings.Contains(l, "*="):
				parts := strings.SplitN(l, "*=", 2)
				v := strings.TrimSpace(parts[0])
				val := strings.TrimSpace(parts[1])
				out = append(out, strings.Repeat("  ", indent)+v+" = "+v+" * "+val)
			case strings.Contains(l, "/="):
				parts := strings.SplitN(l, "/=", 2)
				v := strings.TrimSpace(parts[0])
				val := strings.TrimSpace(parts[1])
				out = append(out, strings.Repeat("  ", indent)+v+" = "+v+" / "+val)
			case strings.Contains(l, "%="):
				parts := strings.SplitN(l, "%=", 2)
				v := strings.TrimSpace(parts[0])
				val := strings.TrimSpace(parts[1])
				out = append(out, strings.Repeat("  ", indent)+v+" = "+v+" % "+val)
			case strings.Contains(l, "&="):
				parts := strings.SplitN(l, "&=", 2)
				v := strings.TrimSpace(parts[0])
				val := strings.TrimSpace(parts[1])
				out = append(out, strings.Repeat("  ", indent)+v+" = "+v+" & "+val)
			case strings.Contains(l, "|="):
				parts := strings.SplitN(l, "|=", 2)
				v := strings.TrimSpace(parts[0])
				val := strings.TrimSpace(parts[1])
				out = append(out, strings.Repeat("  ", indent)+v+" = "+v+" | "+val)
			case strings.Contains(l, "^="):
				parts := strings.SplitN(l, "^=", 2)
				v := strings.TrimSpace(parts[0])
				val := strings.TrimSpace(parts[1])
				out = append(out, strings.Repeat("  ", indent)+v+" = "+v+" ^ "+val)
			case strings.HasPrefix(l, "int "):
				out = append(out, strings.Repeat("  ", indent)+"var "+strings.TrimSpace(l[4:]))
			case strings.HasPrefix(l, "size_t "):
				out = append(out, strings.Repeat("  ", indent)+"var "+strings.TrimSpace(l[7:]))
			case strings.HasPrefix(l, "long "):
				out = append(out, strings.Repeat("  ", indent)+"var "+strings.TrimSpace(l[5:]))
			case strings.HasPrefix(l, "short "):
				out = append(out, strings.Repeat("  ", indent)+"var "+strings.TrimSpace(l[6:]))
			case strings.HasPrefix(l, "float "):
				out = append(out, strings.Repeat("  ", indent)+"var "+strings.TrimSpace(l[6:]))
			case strings.HasPrefix(l, "double "):
				out = append(out, strings.Repeat("  ", indent)+"var "+strings.TrimSpace(l[7:]))
			case strings.HasPrefix(l, "char "):
				out = append(out, strings.Repeat("  ", indent)+"var "+strings.TrimSpace(l[5:]))
			case functionPtrRE.MatchString(l):
				m := functionPtrRE.FindStringSubmatch(l)
				name := strings.TrimSpace(m[1])
				val := strings.TrimSpace(m[2])
				if strings.HasSuffix(val, ";") {
					val = strings.TrimSuffix(val, ";")
				}
				out = append(out, strings.Repeat("  ", indent)+"var "+name+" = "+val)
			default:
				out = append(out, strings.Repeat("  ", indent)+l)
			}
		}
	}
	return out
}

func header() string {
	loc := time.FixedZone("GMT+7", 7*3600)
	t := time.Now().In(loc)
	return fmt.Sprintf("// Generated by a2mochi v%s on %s\n", version(), t.Format("2006-01-02 15:04:05 MST"))
}

func version() string {
	root := repoRoot()
	if root == "" {
		return "dev"
	}
	data, err := os.ReadFile(filepath.Join(root, "VERSION"))
	if err != nil {
		return "dev"
	}
	return strings.TrimSpace(string(data))
}

func repoRoot() string {
	dir, err := os.Getwd()
	if err != nil {
		return ""
	}
	for i := 0; i < 10; i++ {
		if _, err := os.Stat(filepath.Join(dir, "go.mod")); err == nil {
			return dir
		}
		parent := filepath.Dir(dir)
		if parent == dir {
			break
		}
		dir = parent
	}
	return ""
}
