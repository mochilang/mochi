package java

import (
	"bytes"
	"encoding/json"
	"fmt"
	"os/exec"
	"regexp"
	"strings"
)

type structField struct {
	Name string `json:"name"`
	Type string `json:"type"`
	Line int    `json:"line"`
}

type structDef struct {
	Name   string        `json:"name"`
	Line   int           `json:"line"`
	Fields []structField `json:"fields"`
}

type funcParam struct {
	Name string `json:"name"`
	Type string `json:"type"`
}

type funcDef struct {
	Name   string      `json:"name"`
	Line   int         `json:"line"`
	Ret    string      `json:"ret"`
	Params []funcParam `json:"params"`
	Body   []string    `json:"body"`
}

type varDef struct {
	Name string `json:"name"`
	Expr string `json:"expr"`
	Line int    `json:"line"`
}

type javaAST struct {
	Package string      `json:"package"`
	Structs []structDef `json:"structs"`
	Vars    []varDef    `json:"vars"`
	Funcs   []funcDef   `json:"funcs"`
}

func parse(src string) ([]string, error) {
	pkg := ""
	for _, line := range strings.Split(src, "\n") {
		t := strings.TrimSpace(line)
		if strings.HasPrefix(t, "package ") {
			pkg = strings.TrimSuffix(strings.TrimPrefix(t, "package "), ";")
			break
		}
	}

	cmd := exec.Command("mochi-javaast")
	cmd.Stdin = strings.NewReader(src)
	var out bytes.Buffer
	cmd.Stdout = &out
	if err := cmd.Run(); err == nil {
		var ast javaAST
		if err := json.Unmarshal(out.Bytes(), &ast); err != nil {
			return nil, err
		}
		return astToLines(&ast, pkg)
	}
	return parseLegacy(src, pkg)
}

func astToLines(ast *javaAST, pkg string) ([]string, error) {
	var lines []string
	structs := make(map[string][]string)
	for _, st := range ast.Structs {
		lines = append(lines, fmt.Sprintf("type %s {", st.Name))
		var fields []string
		for _, f := range st.Fields {
			lines = append(lines, fmt.Sprintf("  %s: %s", f.Name, mapJavaType(f.Type)))
			fields = append(fields, f.Name)
		}
		lines = append(lines, "}")
		structs[st.Name] = fields
	}
	for _, v := range ast.Vars {
		expr := convertJavaExpr(v.Expr, structs)
		lines = append(lines, fmt.Sprintf("var %s = %s", v.Name, expr))
	}
	for _, fn := range ast.Funcs {
		var params []string
		for _, p := range fn.Params {
			name := p.Name
			if p.Type != "" {
				name += ": " + mapJavaType(p.Type)
			}
			params = append(params, name)
		}
		if fn.Name == "main" && len(params) == 1 && strings.HasPrefix(fn.Params[0].Type, "String") {
			params = nil
		}
		header := fmt.Sprintf("fun %s(%s)", fn.Name, strings.Join(params, ", "))
		rt := mapJavaType(fn.Ret)
		isTest := strings.HasPrefix(fn.Name, "test_") && rt == "void"
		if rt != "" && rt != "void" {
			header += ": " + rt
		}
		if isTest {
			tname := strings.ReplaceAll(strings.TrimPrefix(fn.Name, "test_"), "_", " ")
			lines = append(lines, fmt.Sprintf("test \"%s\" {", tname))
		} else {
			lines = append(lines, header+" {")
		}
		body, err := javaBodyLines(fn.Body, structs)
		if err != nil {
			return nil, fmt.Errorf("%s", err)
		}
		for _, b := range body {
			lines = append(lines, "  "+b)
		}
		lines = append(lines, "}")
	}
	if pkg != "" {
		lines = append([]string{fmt.Sprintf("package %s", pkg)}, lines...)
	}
	return lines, nil
}

func parseLegacy(src, pkg string) ([]string, error) {
	lines := strings.Split(src, "\n")
	var ast javaAST
	ast.Package = pkg
	funcRe := regexp.MustCompile(`(?i)(?:public\s+)?static\s+([^\s]+)\s+([A-Za-z_][A-Za-z0-9_]*)\(([^)]*)\)\s*{`)
	for i := 0; i < len(lines); i++ {
		l := strings.TrimSpace(lines[i])
		switch {
		case isVarAssign(l):
			name, expr := parseVarAssign(l)
			ast.Vars = append(ast.Vars, varDef{Name: name, Expr: expr, Line: i + 1})
		case strings.HasPrefix(l, "static class "):
			stName := strings.TrimSpace(strings.TrimSuffix(strings.TrimPrefix(l, "static class "), "{"))
			st := structDef{Name: stName, Line: i + 1}
			i++
			for i < len(lines) {
				t := strings.TrimSpace(lines[i])
				if t == "}" {
					break
				}
				if isFieldLine(t) {
					typ, name := parseFieldLine(t)
					st.Fields = append(st.Fields, structField{Name: name, Type: typ, Line: i + 1})
					i++
					continue
				}
				if strings.HasPrefix(t, stName+"(") {
					// skip constructor definitions
					depth := strings.Count(t, "{") - strings.Count(t, "}")
					for depth > 0 && i+1 < len(lines) {
						i++
						t = strings.TrimSpace(lines[i])
						depth += strings.Count(t, "{") - strings.Count(t, "}")
					}
					i++
					continue
				}
				i++
			}
			ast.Structs = append(ast.Structs, st)
		case funcRe.MatchString(l):
			m := funcRe.FindStringSubmatch(l)
			fn := funcDef{Name: m[2], Line: i + 1, Ret: m[1]}
			if strings.HasPrefix(fn.Name, "_") {
				// skip helper implementations
				i++
				depth := 1
				for i < len(lines) {
					line := lines[i]
					depth += strings.Count(line, "{") - strings.Count(line, "}")
					i++
					if depth == 0 {
						break
					}
				}
				continue
			}
			params := splitArgs(m[3])
			for _, p := range params {
				f := strings.Fields(strings.TrimSpace(p))
				if len(f) == 0 {
					continue
				}
				name := f[len(f)-1]
				typ := strings.Join(f[:len(f)-1], " ")
				fn.Params = append(fn.Params, funcParam{Name: name, Type: typ})
			}
			i++
			depth := 1
			for i < len(lines) {
				line := lines[i]
				if strings.Contains(line, "{") {
					depth++
				}
				if strings.Contains(line, "}") {
					depth--
					if depth == 0 {
						break
					}
				}
				fn.Body = append(fn.Body, line)
				i++
			}
			ast.Funcs = append(ast.Funcs, fn)
		default:
			if l != "" && l != "{" && l != "}" && !strings.HasPrefix(l, "public class") {
				snippet := numberedBodySnippet(lines, i)
				return nil, fmt.Errorf("line %d: unsupported line\n%s", i+1, snippet)
			}
		}
	}
	return astToLines(&ast, pkg)
}

func javaBodyLines(body []string, structs map[string][]string) ([]string, error) {
	var out []string
	for i := 0; i < len(body); i++ {
		l := strings.TrimSpace(body[i])
		if l == "" {
			continue
		}
		// join multi-line statements ending with '='
		if strings.HasSuffix(l, "=") {
			depth := 0
			for i+1 < len(body) {
				next := strings.TrimSpace(body[i+1])
				l += " " + next
				i++
				depth += strings.Count(next, "{") - strings.Count(next, "}")
				if depth <= 0 && strings.HasSuffix(next, ";") {
					break
				}
			}
		}
		if strings.HasPrefix(l, "System.out.println(") && strings.HasSuffix(l, ");") {
			expr := strings.TrimSuffix(strings.TrimPrefix(l, "System.out.println("), ");")
			out = append(out, "print("+convertJavaExpr(expr, structs)+")")
			continue
		}
		if strings.HasPrefix(l, "return") {
			expr := strings.TrimSpace(strings.TrimSuffix(strings.TrimPrefix(l, "return"), ";"))
			if expr == "" {
				out = append(out, "return")
			} else {
				out = append(out, "return "+convertJavaExpr(expr, structs))
			}
			continue
		}
		if strings.HasPrefix(l, "expect(") && strings.HasSuffix(l, ");") {
			expr := strings.TrimSuffix(strings.TrimPrefix(l, "expect("), ");")
			out = append(out, "expect "+convertJavaExpr(expr, structs))
			continue
		}
		if isVarAssign(l + ";") {
			name, expr := parseVarAssign(l + ";")
			out = append(out, "var "+name+" = "+convertJavaExpr(expr, structs))
			continue
		}
		if left, right, ok := parseSimpleAssign(l + ";"); ok {
			out = append(out, left+" = "+convertJavaExpr(right, structs))
			continue
		}
		snippet := numberedBodySnippet(body, i)
		return nil, fmt.Errorf("line %d: unsupported line\n%s", i+1, snippet)
	}
	return out, nil
}

func numberedBodySnippet(lines []string, idx int) string {
	start := idx - 2
	if start < 0 {
		start = 0
	}
	end := idx + 2
	if end >= len(lines) {
		end = len(lines) - 1
	}
	var b strings.Builder
	for i := start; i <= end; i++ {
		prefix := "   "
		if i == idx {
			prefix = ">>>"
		}
		b.WriteString(fmt.Sprintf("%s %3d| %s\n", prefix, i+1, strings.TrimSpace(lines[i])))
	}
	return b.String()
}
