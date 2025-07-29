//go:build slow

package dart

import (
	"bytes"
	_ "embed"
	"encoding/json"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
)

//go:embed parser.dart
var parserDart string

// Param represents a Dart function parameter.
type Param struct {
	Name string `json:"name"`
	Type string `json:"type"`
}

// Function represents a parsed Dart function.
type Function struct {
	Name   string   `json:"name"`
	Params []Param  `json:"params"`
	Ret    string   `json:"ret"`
	Body   []string `json:"body"`
	Start  int      `json:"start"`
	End    int      `json:"end"`
	Doc    string   `json:"doc,omitempty"`
}

// Field represents a Dart class field.
type Field struct {
	Name string `json:"name"`
	Type string `json:"type"`
}

// Class represents a Dart class definition.
type Class struct {
	Name   string  `json:"name"`
	Fields []Field `json:"fields"`
	Start  int     `json:"start"`
	End    int     `json:"end"`
	Doc    string  `json:"doc,omitempty"`
}

// Program represents a parsed Dart file.
type Program struct {
	Functions []Function `json:"functions"`
	Classes   []Class    `json:"classes"`
	Src       string     `json:"-"`
}

// Parse parses Dart source into a Program using the official Dart parser.
func Parse(src string) (*Program, error) {
	funcs, classes, err := runParser(src)
	if err != nil {
		return nil, err
	}
	return &Program{Functions: funcs, Classes: classes, Src: src}, nil
}

func runParser(src string) ([]Function, []Class, error) {
	if dartPath, err := exec.LookPath("dart"); err == nil {
		f, err := os.CreateTemp("", "parser-*.dart")
		if err != nil {
			return nil, nil, err
		}
		if _, err := f.WriteString(parserDart); err != nil {
			f.Close()
			os.Remove(f.Name())
			return nil, nil, err
		}
		f.Close()
		defer os.Remove(f.Name())

		cmd := exec.Command(dartPath, f.Name())
		cmd.Stdin = strings.NewReader(src)
		var out bytes.Buffer
		var errBuf bytes.Buffer
		cmd.Stdout = &out
		cmd.Stderr = &errBuf
		if err := cmd.Run(); err == nil {
			return decode(out.Bytes())
		}
	}

	exe, err := exec.LookPath("dartast")
	if err != nil {
		root, rErr := repoRoot()
		if rErr != nil {
			return nil, nil, fmt.Errorf("dart not found and dartast missing: %w", err)
		}
		exe = filepath.Join(root, "cmd", "dartast")
		cmd := exec.Command("go", "run", exe)
		cmd.Stdin = strings.NewReader(src)
		var out bytes.Buffer
		var errBuf bytes.Buffer
		cmd.Stdout = &out
		cmd.Stderr = &errBuf
		if runErr := cmd.Run(); runErr != nil {
			if errBuf.Len() > 0 {
				return nil, nil, fmt.Errorf("%v: %s", runErr, errBuf.String())
			}
			return nil, nil, runErr
		}
		return decode(out.Bytes())
	}

	cmd := exec.Command(exe)
	cmd.Stdin = strings.NewReader(src)
	var out bytes.Buffer
	var errBuf bytes.Buffer
	cmd.Stdout = &out
	cmd.Stderr = &errBuf
	if err := cmd.Run(); err != nil {
		if errBuf.Len() > 0 {
			return nil, nil, fmt.Errorf("%v: %s", err, errBuf.String())
		}
		return nil, nil, err
	}
	return decode(out.Bytes())
}

func decode(data []byte) ([]Function, []Class, error) {
	var a struct {
		Functions []Function `json:"functions"`
		Classes   []Class    `json:"classes"`
	}
	if err := json.Unmarshal(data, &a); err != nil {
		return nil, nil, err
	}
	return a.Functions, a.Classes, nil
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
