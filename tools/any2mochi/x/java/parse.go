package java

import (
	"bytes"
	"encoding/json"
	"fmt"
	"os/exec"
	"strings"
)

type AST struct {
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
	cmd := exec.Command("mochi-javaast")
	cmd.Stdin = strings.NewReader(src)
	var out bytes.Buffer
	cmd.Stdout = &out
	if err := cmd.Run(); err != nil {
		return nil, err
	}
	var ast AST
	if err := json.Unmarshal(out.Bytes(), &ast); err != nil {
		return nil, err
	}
	var lines []string
	structs := make(map[string][]string)
	for _, st := range ast.Structs {
		lines = append(lines, fmt.Sprintf("type %s {", st.Name))
		var fields []string
		for _, f := range st.Fields {
			lines = append(lines, fmt.Sprintf("  %s: %s", f.Name, mapType(f.Type)))
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
				name += ": " + mapType(p.Type)
			}
			params = append(params, name)
		}
		header := fmt.Sprintf("fun %s(%s)", fn.Name, strings.Join(params, ", "))
		rt := mapType(fn.Ret)
		if rt != "" && rt != "void" {
			header += ": " + rt
		}
		lines = append(lines, header+" {")
		body, err := bodyLines(fn.Body, structs)
		if err != nil {
			return nil, fmt.Errorf("%s", err)
		}
		for _, b := range body {
			lines = append(lines, "  "+b)
		}
		lines = append(lines, "}")
	}
	return lines, nil
}

func bodyLines(body []string, structs map[string][]string) ([]string, error) {
	var out []string
	for _, line := range body {
		l := strings.TrimSpace(line)
		if l == "" {
			continue
		}
		if strings.HasPrefix(l, "System.out.println(") && strings.HasSuffix(l, ");") {
			expr := strings.TrimSuffix(strings.TrimPrefix(l, "System.out.println("), ");")
			out = append(out, "print("+convertExpr(expr, structs)+")")
			continue
		}
		if strings.HasPrefix(l, "return") {
			expr := strings.TrimSpace(strings.TrimSuffix(strings.TrimPrefix(l, "return"), ";"))
			if expr == "" {
				out = append(out, "return")
			} else {
				out = append(out, "return "+convertExpr(expr, structs))
			}
			continue
		}
		if isVarAssign(l + ";") {
			name, expr := parseVarAssign(l + ";")
			out = append(out, "var "+name+" = "+convertExpr(expr, structs))
			continue
		}
		if left, right, ok := parseSimpleAssign(l + ";"); ok {
			out = append(out, left+" = "+convertExpr(right, structs))
			continue
		}
		return nil, fmt.Errorf("unsupported line: %s", l)
	}
	return out, nil
}
