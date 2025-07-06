package cobol

import (
	"strings"

	"github.com/kaisawind/cobol-go/document"
)

type function struct {
	Name  string   `json:"name"`
	Lines []string `json:"lines"`
}

type ast struct {
	Functions []function `json:"functions"`
}

// parse converts COBOL source to a minimal AST using the cobol-go library for preprocessing.
func parse(src string) (ast, error) {
	processed := document.Parse(src)
	return simpleParse(processed), nil
}

func simpleParse(src string) ast {
	var a ast
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
			if len(a.Functions) == 0 {
				a.Functions = append(a.Functions, function{Name: "main"})
			}
			break
		}
		if len(a.Functions) == 0 {
			a.Functions = append(a.Functions, function{Name: "main"})
		}
		if l != "" {
			a.Functions[0].Lines = append(a.Functions[0].Lines, l)
		}
	}
	return a
}
