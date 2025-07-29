package rb

import (
	"bytes"
	"encoding/json"
	"fmt"
	"os/exec"
	"strings"
)

// Program represents a parsed Ruby source file.
type Program struct {
	Program any `json:"program"`
}

const ripperScript = `require 'json';require 'ripper';src=STDIN.read;b=Ripper::SexpBuilder.new(src);ast=b.parse;if b.error?; STDERR.puts b.error; exit 1; end;puts JSON.generate(ast)`

// Inspect parses the given Ruby source code and returns a Program describing its AST.
func Inspect(src string) (*Program, error) {
	if _, err := exec.LookPath("ruby"); err != nil {
		return nil, fmt.Errorf("ruby not installed: %w", err)
	}
	cmd := exec.Command("ruby", "-e", ripperScript)
	cmd.Stdin = strings.NewReader(src)
	var out bytes.Buffer
	var errBuf bytes.Buffer
	cmd.Stdout = &out
	cmd.Stderr = &errBuf
	if err := cmd.Run(); err != nil {
		if errBuf.Len() > 0 {
			return nil, fmt.Errorf("%s", strings.TrimSpace(errBuf.String()))
		}
		return nil, err
	}
	var data any
	if err := json.Unmarshal(out.Bytes(), &data); err != nil {
		return nil, err
	}
	return &Program{Program: data}, nil
}
