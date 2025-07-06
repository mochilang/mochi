package scheme

import (
	"bytes"
	"context"
	"encoding/json"
	"os"
	"os/exec"
	"path/filepath"
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
	funcs := false
	for _, it := range items {
		if it.Kind != "func" {
			continue
		}
		funcs = true
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
	if !funcs {
		for _, it := range items {
			if it.Kind != "var" {
				continue
			}
			out.WriteString("let ")
			out.WriteString(it.Name)
			if it.Expr != "" {
				out.WriteString(" = ")
				out.WriteString(convertExpr(it.Expr))
			}
			out.WriteByte('\n')
		}
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
	Expr   string   `json:"expr,omitempty"`
}

// repoRoot walks up parent directories to locate go.mod. It mirrors the helper
// used in other converters so that running tests from subdirectories works.
func repoRoot() (string, error) {
	dir, err := os.Getwd()
	if err != nil {
		return "", err
	}
	for i := 0; i < 10; i++ {
		if _, err := os.Stat(filepath.Join(dir, "go.mod")); err == nil {
			return dir, nil
		}
		parent := filepath.Dir(dir)
		if parent == dir {
			break
		}
		dir = parent
	}
	return "", os.ErrNotExist
}

func runParse(src string) ([]cliItem, error) {
	ctx, cancel := context.WithTimeout(context.Background(), 5*time.Second)
	defer cancel()
	// prefer a pre-built schemeast binary if available
	path, err := exec.LookPath("schemeast")
	if err != nil {
		// fall back to 'go run' which builds the CLI on the fly
		root, rErr := repoRoot()
		if rErr != nil {
			return nil, rErr
		}
		cmd := exec.CommandContext(ctx, "go", "run", filepath.Join(root, "tools/any2mochi/x/scheme/cmd/schemeast"))
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
