package scala

import (
	"encoding/json"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
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
	if err := cmd.Run(); err != nil {
		return nil, fmt.Errorf("scala parser: %v\n%s", err, out.String())
	}
	var prog Program
	if err := json.Unmarshal([]byte(out.String()), &prog); err != nil {
		return nil, err
	}
	return &prog, nil
}
