package prolog

import (
	"bytes"
	_ "embed"
	"encoding/json"
	"fmt"
	"os"
	"os/exec"
	"strings"
)

//go:embed pl_ast.pl
var plScript string

// Inspect parses Prolog source code using SWI-Prolog. It returns a Program
// describing the file's clauses. Options may be nil to accept the defaults.
func Inspect(src string) (*Program, error) {
	return InspectWithOption(src, Option{})
}

// InspectWithOption behaves like Inspect but allows the caller to specify
// whether position information should be included in the resulting AST.
func InspectWithOption(src string, opt Option) (*Program, error) {
	exe := os.Getenv("SWIPL")
	if exe == "" {
		exe = "swipl"
	}
	if _, err := exec.LookPath(exe); err != nil {
		return nil, fmt.Errorf("%s not installed", exe)
	}

	tmp, err := os.CreateTemp("", "pl-*.pl")
	if err != nil {
		return nil, err
	}
	if _, err := tmp.WriteString(plScript); err != nil {
		tmp.Close()
		os.Remove(tmp.Name())
		return nil, err
	}
	tmp.Close()
	defer os.Remove(tmp.Name())

	cmd := exec.Command(exe, "-q", "-f", tmp.Name(), "-t", "main")
	cmd.Stdin = strings.NewReader(src)
	var out, errBuf bytes.Buffer
	cmd.Stdout = &out
	cmd.Stderr = &errBuf
	if err := cmd.Run(); err != nil {
		if errBuf.Len() > 0 {
			return nil, fmt.Errorf("%s", strings.TrimSpace(errBuf.String()))
		}
		return nil, err
	}

	var prog rawProgram
	if err := json.Unmarshal(out.Bytes(), &prog); err != nil {
		return nil, err
	}
	return programToNode(&prog, []byte(src), opt), nil
}
