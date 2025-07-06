package ocaml

import (
	"bytes"
	"encoding/json"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
)

type Program struct {
	Funcs  []Func   `json:"funcs"`
	Prints []string `json:"prints"`
}

type Func struct {
	Name   string   `json:"name"`
	Params []string `json:"params"`
	Body   string   `json:"body"`
}

func parse(src string) (*Program, error) {
	tmp, err := os.CreateTemp("", "ocaml-*.ml")
	if err != nil {
		return nil, err
	}
	defer os.Remove(tmp.Name())
	if _, err := tmp.WriteString(src); err != nil {
		return nil, err
	}
	tmp.Close()
	root, err := repoRoot()
	if err != nil {
		return nil, err
	}
	script := filepath.Join(root, "tools", "any2mochi", "x", "ocaml", "ocaml_ast.js")
	cmd := exec.Command("node", script, tmp.Name())
	var out bytes.Buffer
	var stderr bytes.Buffer
	cmd.Stdout = &out
	cmd.Stderr = &stderr
	if err := cmd.Run(); err != nil {
		msg := stderr.String()
		if msg == "" {
			msg = err.Error()
		}
		return nil, fmt.Errorf("node: %s", msg)
	}
	var prog Program
	if err := json.Unmarshal(out.Bytes(), &prog); err != nil {
		return nil, err
	}
	return &prog, nil
}

func formatProgram(p *Program) []byte {
	var out bytes.Buffer
	for _, fn := range p.Funcs {
		out.WriteString("fun ")
		out.WriteString(fn.Name)
		out.WriteByte('(')
		for i, n := range fn.Params {
			if i > 0 {
				out.WriteString(", ")
			}
			out.WriteString(n)
		}
		out.WriteString(") {\n  ")
		body := fn.Body
		body = strings.ReplaceAll(body, "print_endline", "print")
		body = strings.ReplaceAll(body, "string_of_int", "str")
		body = strings.ReplaceAll(body, "string_of_float", "str")
		body = strings.ReplaceAll(body, "string_of_bool", "str")
		body = strings.ReplaceAll(body, "str (", "str(")
		body = strings.ReplaceAll(body, "print (", "print(")
		body = strings.ReplaceAll(body, "not ", "!")
		out.WriteString(body)
		out.WriteString("\n}\n")
	}
	for _, pstr := range p.Prints {
		line := strings.TrimSpace(pstr)
		line = strings.ReplaceAll(line, "string_of_int", "str")
		line = strings.ReplaceAll(line, "string_of_float", "str")
		line = strings.ReplaceAll(line, "string_of_bool", "str")
		line = strings.ReplaceAll(line, "str (", "str(")
		line = strings.ReplaceAll(line, "print (", "print(")
		line = strings.ReplaceAll(line, "not ", "!")
		if strings.HasPrefix(line, "print_endline") {
			line = strings.TrimSpace(strings.TrimPrefix(line, "print_endline"))
			if strings.HasPrefix(line, "(") && strings.HasSuffix(line, ")") {
				line = strings.TrimSuffix(strings.TrimPrefix(line, "("), ")")
			}
			out.WriteString("print(")
			out.WriteString(strings.TrimSpace(line))
			out.WriteString(")\n")
		} else {
			out.WriteString("print(")
			out.WriteString(line)
			out.WriteString(")\n")
		}
	}
	return out.Bytes()
}

func repoRoot() (string, error) {
	dir, err := os.Getwd()
	if err != nil {
		return "", err
	}
	for i := 0; i < 10; i++ {
		if _, err := os.Stat(filepath.Join(dir, "go.mod")); err == nil {
			return dir, nil
		}
		parent := filepath.Dir(dir)
		if parent == dir {
			break
		}
		dir = parent
	}
	return "", os.ErrNotExist
}
