//go:build slow

package any2mochi

import (
	"bytes"
	"context"
	"encoding/json"
	"os/exec"
	"time"
)

// ParseText invokes cmdName with args and feeds src on stdin. The command
// is expected to output a JSON object containing "symbols" and
// "diagnostics" fields that match the protocol types.
func ParseText(cmdName string, args []string, langID string, src string) ([]DocumentSymbol, []Diagnostic, error) {
	return ParseTextWithRoot(cmdName, args, langID, src, "")
}

// ParseTextWithRoot behaves like ParseText but provides a workspace root
// directory to the called CLI via the WORKSPACE_ROOT environment variable.
func ParseTextWithRoot(cmdName string, args []string, langID, src, root string) ([]DocumentSymbol, []Diagnostic, error) {
	ctx, cancel := context.WithTimeout(context.Background(), 5*time.Second)
	defer cancel()
	cmd := exec.CommandContext(ctx, cmdName, args...)
	if root != "" {
		cmd.Env = append(cmd.Env, "WORKSPACE_ROOT="+root)
	}
	cmd.Stdin = bytes.NewBufferString(src)
	var out bytes.Buffer
	cmd.Stdout = &out
	if err := cmd.Run(); err != nil {
		return nil, nil, err
	}
	var res struct {
		Symbols     []DocumentSymbol `json:"symbols"`
		Diagnostics []Diagnostic     `json:"diagnostics"`
	}
	if err := json.Unmarshal(out.Bytes(), &res); err != nil {
		return nil, nil, err
	}
	return res.Symbols, res.Diagnostics, nil
}
