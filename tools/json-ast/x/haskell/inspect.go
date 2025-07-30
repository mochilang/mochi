package haskell

import (
	"bytes"
	_ "embed"
	"encoding/json"
	"fmt"
	"os"
	"os/exec"
	"strings"
)

//go:embed parse.hs
var parseScript string

type Program struct {
	AST json.RawMessage `json:"ast"`
}

// Inspect parses the given Haskell source code using runghc and
// returns a Program describing its structure.
func Inspect(src string) (*Program, error) {
	if _, err := exec.LookPath("runghc"); err != nil {
		return nil, fmt.Errorf("runghc not installed")
	}
	tmp, err := os.CreateTemp("", "hs-src-*.hs")
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

	script, err := os.CreateTemp("", "parse-*.hs")
	if err != nil {
		return nil, err
	}
	if _, err := script.WriteString(parseScript); err != nil {
		script.Close()
		os.Remove(script.Name())
		return nil, err
	}
	script.Close()
	defer os.Remove(script.Name())

	cmd := exec.Command("runghc", script.Name(), tmp.Name())
	var out bytes.Buffer
	var errBuf bytes.Buffer
	cmd.Stdout = &out
	cmd.Stderr = &errBuf
	if err := cmd.Run(); err != nil {
		msg := strings.TrimSpace(errBuf.String())
		if msg != "" {
			return nil, fmt.Errorf("%v: %s", err, msg)
		}
		return nil, err
	}
	var raw json.RawMessage
	if err := json.Unmarshal(out.Bytes(), &raw); err != nil {
		return nil, err
	}
	return &Program{AST: raw}, nil
}
