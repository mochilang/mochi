//go:build slow

package ocaml

import (
	"fmt"
	"os"
	"os/exec"
	"strings"
)

// Program represents a parsed OCaml source file.
type Program struct {
	Prints []Print
	Source string
}

type Print struct {
	Expr string
}

// Parse parses OCaml source code using the ocamlc compiler.
// It relies on `ocamlc -dparsetree` and performs a very small extraction of
// print_endline statements. Regular expressions are intentionally avoided.
func Parse(src string) (*Program, error) {
	if _, err := exec.LookPath("ocamlc"); err != nil {
		return nil, fmt.Errorf("ocamlc not installed")
	}
	tmp, err := os.CreateTemp("", "ocaml-src-*.ml")
	if err != nil {
		return nil, err
	}
	if _, err := tmp.WriteString(src); err != nil {
		os.Remove(tmp.Name())
		return nil, err
	}
	tmp.Close()
	defer os.Remove(tmp.Name())

	cmd := exec.Command("ocamlc", "-dparsetree", tmp.Name())
	out, err := cmd.CombinedOutput()
	if err != nil {
		return nil, fmt.Errorf("ocamlc error: %w\n%s", err, out)
	}
	prog := parseParsetree(string(out))
	prog.Source = src
	return prog, nil
}

// parseParsetree performs a minimal extraction from the textual parsetree
// produced by `ocamlc -dparsetree`. Only constant string arguments of
// print_endline calls are recognized.
func parseParsetree(out string) *Program {
	p := &Program{}
	lines := strings.Split(out, "\n")
	for i := 0; i < len(lines); i++ {
		line := strings.TrimSpace(lines[i])
		if strings.Contains(line, "Pexp_ident \"print_endline\"") {
			for j := i + 1; j < len(lines); j++ {
				arg := strings.TrimSpace(lines[j])
				if strings.Contains(arg, "PConst_string(") {
					start := strings.Index(arg, "\"")
					end := strings.LastIndex(arg, "\"")
					if start >= 0 && end > start {
						p.Prints = append(p.Prints, Print{Expr: arg[start+1 : end]})
					}
					break
				}
			}
		}
	}
	return p
}
