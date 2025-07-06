package scala

import (
	"bytes"
	"context"
	"encoding/json"
	"os/exec"
	"time"
)

// parseCmd is the CLI used to parse Scala source into a simple AST.
var parseCmd = "scalaast"

type Func struct {
	Name   string   `json:"name"`
	Params []string `json:"params"`
	Body   []string `json:"body"`
}

// runParse invokes the scalaast CLI to obtain the AST in JSON form.
func runParse(src string) ([]Func, error) {
	ctx, cancel := context.WithTimeout(context.Background(), 5*time.Second)
	defer cancel()
	cmd := exec.CommandContext(ctx, parseCmd)
	cmd.Stdin = bytes.NewBufferString(src)
	var out bytes.Buffer
	cmd.Stdout = &out
	if err := cmd.Run(); err != nil {
		return nil, err
	}
	var funcs []Func
	if err := json.Unmarshal(out.Bytes(), &funcs); err != nil {
		return nil, err
	}
	return funcs, nil
}
