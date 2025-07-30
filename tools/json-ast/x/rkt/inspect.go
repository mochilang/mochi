//go:build slow

package rkt

import (
	"bytes"
	_ "embed"
	"encoding/json"
	"fmt"
	"os"
	"os/exec"
	"strings"
)

//go:embed parse.rkt
var racketParser string

// Form represents a top-level form parsed from Racket source.
type Form struct {
	Datum any `json:"datum"`
	Line  int `json:"line"`
	Col   int `json:"col"`
}

// Program represents a parsed Racket source file.
type Program struct {
	Forms []Form `json:"forms"`
}

// Inspect parses the provided Racket source code and returns a Program
// describing its structure using the official Racket parser.
func Inspect(src string) (*Program, error) {
	prog, err := parse(src)
	if err != nil {
		return nil, err
	}
	return prog, nil
}

func parse(src string) (*Program, error) {
	if _, err := exec.LookPath("racket"); err != nil {
		return nil, fmt.Errorf("racket not installed")
	}
	script, err := os.CreateTemp("", "parse-*.rkt")
	if err != nil {
		return nil, err
	}
	defer os.Remove(script.Name())
	if _, err := script.WriteString(racketParser); err != nil {
		script.Close()
		return nil, err
	}
	script.Close()
	cmd := exec.Command("racket", script.Name())
	cmd.Stdin = strings.NewReader(src)
	var out bytes.Buffer
	var errBuf bytes.Buffer
	cmd.Stdout = &out
	cmd.Stderr = &errBuf
	if err := cmd.Run(); err != nil {
		if errBuf.Len() > 0 {
			return nil, fmt.Errorf("racket: %s", errBuf.String())
		}
		return nil, err
	}
	var forms []Form
	if err := json.Unmarshal(out.Bytes(), &forms); err != nil {
		return nil, err
	}
	return &Program{Forms: forms}, nil
}
