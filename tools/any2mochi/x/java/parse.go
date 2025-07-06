package java

import (
	"bytes"
	"encoding/json"
	"fmt"
	"os/exec"
	"strings"
)

type javaAST struct {
	Package string `json:"package"`
	Structs []struct {
		Name   string `json:"name"`
		Fields []struct {
			Name string `json:"name"`
			Type string `json:"type"`
		} `json:"fields"`
	} `json:"structs"`
	Funcs []struct {
		Name   string `json:"name"`
		Ret    string `json:"ret"`
		Params []struct {
			Name string `json:"name"`
			Type string `json:"type"`
		} `json:"params"`
		Body []string `json:"body"`
	} `json:"funcs"`
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
	if err := cmd.Run(); err != nil {
		return nil, err
	}
	var ast javaAST
	if err := json.Unmarshal(out.Bytes(), &ast); err != nil {
		return nil, err
	}
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
	for _, fn := range ast.Funcs {
		var params []string
		for _, p := range fn.Params {
			name := p.Name
			if p.Type != "" {
				name += ": " + mapJavaType(p.Type)
			}
			params = append(params, name)
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

func javaBodyLines(body []string, structs map[string][]string) ([]string, error) {
	var out []string
	for i := 0; i < len(body); i++ {
		l := strings.TrimSpace(body[i])
		if l == "" {
			continue
		}
		// join multi-line statements ending with '='
		if strings.HasSuffix(l, "=") {
			for i+1 < len(body) {
				l += " " + strings.TrimSpace(body[i+1])
				i++
				if strings.HasSuffix(strings.TrimSpace(body[i]), ";") {
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
		return nil, fmt.Errorf("line %d: unsupported line: %s\n%s", i+1, l, snippet)
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
		b.WriteString(fmt.Sprintf("%3d| %s\n", i+1, strings.TrimSpace(lines[i])))
	}
	return b.String()
}
