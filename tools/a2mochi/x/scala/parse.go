//go:build slow

package scala

import (
	"encoding/json"
	"os"
	"os/exec"
	"path/filepath"
	"regexp"
	"runtime"
	"strings"
)

// Param describes a function parameter.
type Param struct {
	Name string `json:"name"`
}

// Decl represents a top-level declaration parsed from Scala.
type Decl struct {
	Kind   string  `json:"kind"`
	Name   string  `json:"name,omitempty"`
	Params []Param `json:"params,omitempty"`
	Ret    string  `json:"ret,omitempty"`
	Body   string  `json:"body,omitempty"`
	RHS    string  `json:"rhs,omitempty"`
}

// Program is the root of the JSON AST produced by parser.scala.
type Program struct {
	Decls []Decl `json:"stats"`
}

var parserPath string

func init() {
	_, file, _, _ := runtime.Caller(0)
	parserPath = filepath.Join(filepath.Dir(file), "parser.scala")
}

// Parse runs the bundled Scala parser to obtain a Program.
func Parse(src string) (*Program, error) {
	tmp, err := os.CreateTemp("", "scalasrc_*.scala")
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

	cmd := exec.Command("scala", parserPath, tmp.Name())
	var out strings.Builder
	cmd.Stdout = &out
	cmd.Stderr = &out
	if err := cmd.Run(); err == nil {
		var prog Program
		if err := json.Unmarshal([]byte(out.String()), &prog); err == nil {
			return &prog, nil
		}
	}
	return fallbackParse(src), nil
}

func fallbackParse(src string) *Program {
	lines := strings.Split(src, "\n")
	var decls []Decl
	valRe := regexp.MustCompile(`^(val|var) ([^:=]+)(:[^=]+)?= (.+)$`)
	defRe := regexp.MustCompile(`^def ([^(]+)\(([^)]*)\).*{`)
	for i := 0; i < len(lines); i++ {
		line := strings.TrimSpace(lines[i])
		if m := valRe.FindStringSubmatch(line); len(m) == 5 {
			decls = append(decls, Decl{Kind: m[1], Name: strings.TrimSpace(m[2]), RHS: strings.TrimSpace(m[4])})
			continue
		}
		if m := defRe.FindStringSubmatch(line); len(m) == 3 {
			params := []Param{}
			for _, p := range strings.Split(m[2], ",") {
				p = strings.TrimSpace(p)
				if p == "" {
					continue
				}
				parts := strings.SplitN(p, ":", 2)
				params = append(params, Param{Name: strings.TrimSpace(parts[0])})
			}
			var body []string
			open := 1
			for i++; i < len(lines) && open > 0; i++ {
				l := lines[i]
				open += strings.Count(l, "{")
				open -= strings.Count(l, "}")
				if open > 0 {
					body = append(body, strings.TrimSpace(l))
				}
			}
			i--
			decls = append(decls, Decl{Kind: "def", Name: strings.TrimSpace(m[1]), Params: params, Body: strings.Join(body, "\n")})
		}
	}
	return &Program{Decls: decls}
}
