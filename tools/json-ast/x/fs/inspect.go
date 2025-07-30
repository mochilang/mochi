//go:build slow

package fs

import (
	"encoding/json"
	"os"
	"os/exec"
	"strings"

	_ "embed"
)

// Program describes a simplified F# program structure extracted using the
// official compiler service.
type Program struct {
	Vars   []Var    `json:"vars"`
	Prints []string `json:"prints"`
	Stmts  []Stmt   `json:"stmts"`
}

//go:embed parse.fsx
var parseScript string

// Inspect parses F# code using the official F# compiler service via
// a helper F# script executed with `fsharpi`.
func Inspect(src string) (*Program, error) {
	tmp, err := os.CreateTemp("", "parse-*.fsx")
	if err != nil {
		return nil, err
	}
	if _, err := tmp.WriteString(parseScript); err != nil {
		tmp.Close()
		os.Remove(tmp.Name())
		return nil, err
	}
	tmp.Close()
	defer os.Remove(tmp.Name())

	cmd := exec.Command("dotnet", "fsi", "--quiet", tmp.Name())
	cmd.Stdin = strings.NewReader(src)
	out, err := cmd.Output()
	if err != nil {
		return nil, err
	}
	var p Program
	if err := json.Unmarshal(out, &p); err != nil {
		return nil, err
	}
	return &p, nil
}
