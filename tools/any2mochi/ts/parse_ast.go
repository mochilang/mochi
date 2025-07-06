package ts

import (
	"encoding/json"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"runtime"

	tscode "mochi/compile/ts"
)

// TSAstDecl represents a top-level declaration parsed from a TypeScript source
// file. Line numbers are 1-based and refer to the original TypeScript file.
type TSAstDecl struct {
	Kind      string    `json:"kind"`
	Name      string    `json:"name"`
	Params    []TSParam `json:"params,omitempty"`
	Ret       string    `json:"ret,omitempty"`
	Body      string    `json:"body,omitempty"`
	Fields    []TSField `json:"fields,omitempty"`
	Alias     string    `json:"alias,omitempty"`
	Variants  []string  `json:"variants,omitempty"`
	StartLine int       `json:"start,omitempty"`
	EndLine   int       `json:"end,omitempty"`
}

// parseTSAST parses src using a Deno helper and returns the AST.
func parseTSAST(src string) ([]TSAstDecl, error) {
	if err := tscode.EnsureDeno(); err != nil {
		return nil, err
	}
	_, file, _, _ := runtime.Caller(0)
	script := filepath.Join(filepath.Dir(file), "parse_ts_ast.ts")
	temp, err := os.CreateTemp("", "tsinput-*.ts")
	if err != nil {
		return nil, err
	}
	if _, err := temp.WriteString(src); err != nil {
		os.Remove(temp.Name())
		return nil, err
	}
	temp.Close()
	defer os.Remove(temp.Name())
	cmd := exec.Command("deno", "run", "--quiet", "--allow-read", script, temp.Name())
	cmd.Env = append(os.Environ(), "DENO_TLS_CA_STORE=system")
	out, err := cmd.CombinedOutput()
	if err != nil {
		return nil, fmt.Errorf("deno error: %w\n%s", err, out)
	}
	var decls []TSAstDecl
	if err := json.Unmarshal(out, &decls); err != nil {
		return nil, err
	}
	return decls, nil
}
