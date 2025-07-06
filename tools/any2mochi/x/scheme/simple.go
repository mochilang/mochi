package scheme

import (
	"bytes"
	"context"
	"encoding/json"
	"os/exec"
	"strings"
	"time"
)

// convertSimple converts Scheme source code by invoking the schemeast CLI
// to obtain a minimal AST. Only top level function definitions are translated
// into empty Mochi stubs.
func convertSimple(src string) ([]byte, error) {
	items, err := runParse(src)
	if err != nil {
		return nil, err
	}
	var out strings.Builder
	for _, it := range items {
		if it.Kind != "func" {
			continue
		}
		out.WriteString("fun ")
		out.WriteString(it.Name)
		out.WriteByte('(')
		for i, p := range it.Params {
			if i > 0 {
				out.WriteString(", ")
			}
			out.WriteString(p)
		}
		out.WriteString(") {}\n")
	}
	if out.Len() == 0 {
		return nil, nil
	}
	return []byte(out.String()), nil
}

type cliItem struct {
	Kind   string   `json:"kind"`
	Name   string   `json:"name"`
	Params []string `json:"params,omitempty"`
}

func runParse(src string) ([]cliItem, error) {
	ctx, cancel := context.WithTimeout(context.Background(), 5*time.Second)
	defer cancel()
	// prefer a pre-built schemeast binary if available
	path, err := exec.LookPath("schemeast")
	if err != nil {
		// fall back to 'go run' which builds the CLI on the fly
		cmd := exec.CommandContext(ctx, "go", "run", "./tools/any2mochi/x/scheme/cmd/schemeast")
		cmd.Stdin = strings.NewReader(src)
		var out bytes.Buffer
		cmd.Stdout = &out
		if err := cmd.Run(); err != nil {
			return nil, err
		}
		var items []cliItem
		if err := json.Unmarshal(out.Bytes(), &items); err != nil {
			return nil, err
		}
		return items, nil
	}
	cmd := exec.CommandContext(ctx, path)
	cmd.Stdin = strings.NewReader(src)
	var out bytes.Buffer
	cmd.Stdout = &out
	if err := cmd.Run(); err != nil {
		return nil, err
	}
	var items []cliItem
	if err := json.Unmarshal(out.Bytes(), &items); err != nil {
		return nil, err
	}
	return items, nil
}
