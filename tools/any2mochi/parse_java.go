package any2mochi

import (
	"bytes"
	"encoding/json"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
)

// javaAST mirrors the JSON structure produced by the javaast CLI.
type javaAST struct {
	Structs []javaStruct `json:"structs"`
	Funcs   []javaFunc   `json:"funcs"`
}

type javaStruct struct {
	Name   string   `json:"name"`
	Fields []string `json:"fields"`
}

type javaFunc struct {
	Name   string   `json:"name"`
	Params []string `json:"params"`
	Lines  []string `json:"lines"`
}

// parseJava invokes the javaast CLI to convert Java source to Mochi statements.
func parseJava(src string) ([]string, error) {
	ast, err := parseJavaCLI(src)
	if err != nil {
		return nil, err
	}
	return convertJavaAST(ast)
}

func parseJavaCLI(src string) (*javaAST, error) {
	root, err := repoRoot()
	if err != nil {
		return nil, err
	}
	tmp, err := os.CreateTemp("", "javasrc_*.java")
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
	cmd := exec.Command("go", "run", filepath.Join(root, "tools", "javaast"), tmp.Name())
	var out bytes.Buffer
	var errBuf bytes.Buffer
	cmd.Stdout = &out
	cmd.Stderr = &errBuf
	if err := cmd.Run(); err != nil {
		msg := strings.TrimSpace(errBuf.String())
		if msg == "" {
			msg = err.Error()
		}
		return nil, fmt.Errorf("javaast: %s", msg)
	}
	var ast javaAST
	if err := json.Unmarshal(out.Bytes(), &ast); err != nil {
		return nil, err
	}
	return &ast, nil
}

func convertJavaAST(ast *javaAST) ([]string, error) {
	var out []string
	structs := make(map[string][]string)
	for _, st := range ast.Structs {
		structs[st.Name] = st.Fields
		out = append(out, "type "+st.Name+" {")
		for _, f := range st.Fields {
			out = append(out, "  "+f+": any")
		}
		out = append(out, "}")
	}
	for _, fn := range ast.Funcs {
		out = append(out, fmt.Sprintf("fun %s(%s) {", fn.Name, strings.Join(fn.Params, ", ")))
		indent := 1
		for _, line := range fn.Lines {
			conv, err := convertJavaLine(strings.TrimSpace(line), structs, &indent)
			if err != nil {
				return nil, err
			}
			for _, l := range conv {
				out = append(out, strings.Repeat("  ", indent)+l)
			}
		}
		if indent > 0 {
			indent--
		}
		out = append(out, "}")
	}
	return out, nil
}

func convertJavaLine(line string, structs map[string][]string, indent *int) ([]string, error) {
	if line == "" {
		return nil, nil
	}
	switch {
	case line == "}":
		if *indent > 0 {
			(*indent)--
		}
		return []string{"}"}, nil
	case line == "else {":
		if *indent > 0 {
			(*indent)--
		}
		(*indent)++
		return []string{"} else {"}, nil
	case strings.HasPrefix(line, "if "):
		cond := strings.Trim(line[len("if "):], "() {")
		cond = convertJavaExpr(cond, structs)
		(*indent)++
		return []string{"if " + cond + " {"}, nil
	case strings.HasPrefix(line, "while "):
		cond := strings.Trim(line[len("while "):], "() {")
		cond = convertJavaExpr(cond, structs)
		(*indent)++
		return []string{"while " + cond + " {"}, nil
	case strings.HasPrefix(line, "for ("):
		inner := strings.TrimSuffix(strings.TrimPrefix(line, "for ("), ") {")
		// foreach with ':'
		if strings.Contains(inner, ":") {
			parts := strings.SplitN(inner, ":", 2)
			left := strings.TrimSpace(parts[0])
			fields := strings.Fields(left)
			name := fields[len(fields)-1]
			expr := convertJavaExpr(strings.TrimSpace(parts[1]), structs)
			(*indent)++
			return []string{"for " + name + " in " + expr + " {"}, nil
		}
		parts := strings.Split(inner, ";")
		if len(parts) == 3 {
			name, start := parseVarAssign(strings.TrimSpace(parts[0]) + ";")
			cond := strings.TrimSpace(parts[1])
			cmp := strings.Fields(cond)
			if len(cmp) == 3 && cmp[0] == name && cmp[1] == "<" {
				end := convertJavaExpr(cmp[2], structs)
				startExpr := convertJavaExpr(start, structs)
				(*indent)++
				return []string{fmt.Sprintf("for %s in %s .. %s {", name, startExpr, end)}, nil
			}
		}
		return nil, fmt.Errorf("unsupported line: %s", line)
	case line == "break;" || line == "continue;":
		return []string{strings.TrimSuffix(line, ";")}, nil
	case strings.HasPrefix(line, "return "):
		expr := convertJavaExpr(strings.TrimSuffix(strings.TrimPrefix(line, "return "), ";"), structs)
		return []string{"return " + expr}, nil
	default:
		if before, key, val, ok := parseMapPut(line); ok {
			return []string{fmt.Sprintf("%s[%s] = %s", before, convertJavaExpr(key, structs), convertJavaExpr(val, structs))}, nil
		}
		if isVarAssign(line) {
			name, expr := parseVarAssign(line)
			return []string{"var " + name + " = " + convertJavaExpr(expr, structs)}, nil
		}
		if left, right, ok := parseSimpleAssign(line); ok {
			return []string{left + " = " + convertJavaExpr(right, structs)}, nil
		}
		if strings.HasPrefix(line, "System.out.println(") {
			inner := strings.TrimSuffix(strings.TrimPrefix(line, "System.out.println("), ");")
			return []string{"print(" + convertJavaExpr(inner, structs) + ")"}, nil
		}
		if strings.HasSuffix(line, ";") {
			expr := strings.TrimSuffix(line, ";")
			return []string{convertJavaExpr(expr, structs)}, nil
		}
	}
	return nil, fmt.Errorf("unsupported line: %s", line)
}
