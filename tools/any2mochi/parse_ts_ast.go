package any2mochi

import (
	"encoding/json"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"runtime"

	tscode "mochi/compile/ts"
)

type tsASTDecl struct {
	Kind     string    `json:"kind"`
	Name     string    `json:"name"`
	Params   []tsParam `json:"params,omitempty"`
	Ret      string    `json:"ret,omitempty"`
	Body     string    `json:"body,omitempty"`
	Fields   []tsField `json:"fields,omitempty"`
	Alias    string    `json:"alias,omitempty"`
	Variants []string  `json:"variants,omitempty"`
}

// parseTSAST parses src using a Deno helper and returns the AST.
func parseTSAST(src string) ([]tsASTDecl, error) {
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
	var decls []tsASTDecl
	if err := json.Unmarshal(out, &decls); err != nil {
		return nil, err
	}
	return decls, nil
}
