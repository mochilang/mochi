package rkt

import (
	"bytes"
	_ "embed"
	"encoding/json"
	"fmt"
	"os/exec"
	"strings"
)

//go:embed parse.rkt
var racketParser string

type RawAST interface{}

// ParseAST parses Racket source using the official parser and returns the raw AST as JSON.
func ParseAST(src string) (RawAST, error) {
	if _, err := exec.LookPath("racket"); err != nil {
		return nil, fmt.Errorf("racket not installed")
	}
	if idx := strings.Index(src, "#lang"); idx >= 0 && idx < 100 {
		if i := strings.Index(src[idx:], "\n"); i >= 0 {
			src = src[:idx] + src[idx+i+1:]
		} else {
			src = src[:idx]
		}
	}
	cmd := exec.Command("racket", "-e", racketParser)
	cmd.Stdin = strings.NewReader(src)
	var out bytes.Buffer
	var errBuf bytes.Buffer
	cmd.Stdout = &out
	cmd.Stderr = &errBuf
	if err := cmd.Run(); err != nil {
		return nil, fmt.Errorf("racket parse error: %v: %s", err, errBuf.String())
	}
	var ast RawAST
	if err := json.Unmarshal(out.Bytes(), &ast); err != nil {
		return nil, err
	}
	return ast, nil
}
