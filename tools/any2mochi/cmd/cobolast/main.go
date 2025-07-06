package main

import (
	"encoding/json"
	"io"
	"os"
	"strings"
)

type CobolFunction struct {
	Name  string   `json:"name"`
	Lines []string `json:"lines"`
}

type CobolAST struct {
	Functions []CobolFunction `json:"functions"`
}

func parseCobol(src string) CobolAST {
	var ast CobolAST
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
				ast.Functions = append(ast.Functions, CobolFunction{Name: "main"})
			}
			break
		}
		if len(ast.Functions) == 0 {
			ast.Functions = append(ast.Functions, CobolFunction{Name: "main"})
		}
		if l != "" {
			ast.Functions[0].Lines = append(ast.Functions[0].Lines, l)
		}
	}
	return ast
}

func main() {
	data, err := io.ReadAll(os.Stdin)
	if err != nil {
		panic(err)
	}
	ast := parseCobol(string(data))
	enc := json.NewEncoder(os.Stdout)
	enc.SetIndent("", "  ")
	if err := enc.Encode(ast); err != nil {
		panic(err)
	}
}
