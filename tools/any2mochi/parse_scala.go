package any2mochi

import (
	"bytes"
	"context"
	"encoding/json"
	"os/exec"
	"time"
)

// scalaParseCmd is the CLI used to parse Scala source into a simple AST.
var scalaParseCmd = "scalaast"

type scalaFunc struct {
	Name   string   `json:"name"`
	Params []string `json:"params"`
	Body   []string `json:"body"`
}

// runScalaParse invokes the scalaast CLI to obtain the AST in JSON form.
func runScalaParse(src string) ([]scalaFunc, error) {
	ctx, cancel := context.WithTimeout(context.Background(), 5*time.Second)
	defer cancel()
	cmd := exec.CommandContext(ctx, scalaParseCmd)
	cmd.Stdin = bytes.NewBufferString(src)
	var out bytes.Buffer
	cmd.Stdout = &out
	if err := cmd.Run(); err != nil {
		return nil, err
	}
	var funcs []scalaFunc
	if err := json.Unmarshal(out.Bytes(), &funcs); err != nil {
		return nil, err
	}
	return funcs, nil
}
