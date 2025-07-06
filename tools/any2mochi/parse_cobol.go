package any2mochi

import (
	"bytes"
	"encoding/json"
	"os/exec"
	"strings"
)

type cobolFunc struct {
	Name  string   `json:"name"`
	Lines []string `json:"lines"`
}

type cobolAST struct {
	Functions []cobolFunc `json:"functions"`
}

// parseCobolCLI invokes the cobolast CLI to obtain a Cobol AST in JSON form.
func parseCobolCLI(src string) (cobolAST, error) {
	var ast cobolAST
	cmd := exec.Command("cobolast")
	cmd.Stdin = strings.NewReader(src)
	var out bytes.Buffer
	cmd.Stdout = &out
	if err := cmd.Run(); err != nil {
		return ast, err
	}
	if err := json.Unmarshal(out.Bytes(), &ast); err != nil {
		return ast, err
	}
	return ast, nil
}
