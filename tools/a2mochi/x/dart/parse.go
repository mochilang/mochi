package dart

import (
	"bytes"
	_ "embed"
	"encoding/json"
	"fmt"
	"os"
	"os/exec"
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
	dartPath, err := exec.LookPath("dart")
	if err != nil {
		return nil, nil, fmt.Errorf("dart not found: %w", err)
	}
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
