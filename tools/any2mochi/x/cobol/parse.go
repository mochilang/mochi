package cobol

import "strings"

type Func struct {
	Name  string   `json:"name"`
	Lines []string `json:"lines"`
}

type AST struct {
	Functions []Func `json:"functions"`
}

// Parse extracts a minimal AST from COBOL source code.
func Parse(src string) (*AST, error) {
	var ast AST
	lines := strings.Split(src, "\n")
	inProc := false
	for i := 0; i < len(lines); i++ {
		l := strings.TrimSpace(lines[i])
		upper := strings.ToUpper(l)
		if strings.HasPrefix(upper, "PROCEDURE DIVISION") {
			inProc = true
			continue
		}
		if !inProc {
			continue
		}
		if strings.HasPrefix(upper, "STOP RUN") {
			if len(ast.Functions) == 0 {
				ast.Functions = append(ast.Functions, Func{Name: "main"})
			}
			break
		}
		if len(ast.Functions) == 0 {
			ast.Functions = append(ast.Functions, Func{Name: "main"})
		}
		if l != "" {
			ast.Functions[0].Lines = append(ast.Functions[0].Lines, l)
		}
	}
	return &ast, nil
}
