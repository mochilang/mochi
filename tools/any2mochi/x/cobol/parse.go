package cobol

import (
	"fmt"
	"os"
	"os/exec"
	"strconv"
	"strings"
)

type Func struct {
	Name  string   `json:"name"`
	Lines []string `json:"lines"`
}

// Var represents a WORKING-STORAGE declaration.
type Var struct {
	Name    string `json:"name"`
	Picture string `json:"picture"`
	Occurs  int    `json:"occurs,omitempty"`
}

type AST struct {
	Vars      []Var  `json:"vars"`
	Functions []Func `json:"functions"`
}

// Parse extracts a minimal AST from COBOL source code.
func Parse(src string) (*AST, error) {
	// syntax check with cobc if available
	if _, err := exec.LookPath("cobc"); err == nil {
		f, err := os.CreateTemp("", "mochi_*.cob")
		if err != nil {
			return nil, err
		}
		defer os.Remove(f.Name())
		if _, err := f.WriteString(src); err != nil {
			return nil, err
		}
		f.Close()
		cmd := exec.Command("cobc", "-free", "-fsyntax-only", f.Name())
		out, err := cmd.CombinedOutput()
		if err != nil {
			msg := strings.TrimSpace(string(out))
			lineNum := 0
			text := ""
			// best effort parse of first error line: file:line: error: msg
			for _, l := range strings.Split(msg, "\n") {
				if !strings.Contains(l, ": error:") {
					continue
				}
				parts := strings.SplitN(l, ":", 4)
				if len(parts) >= 3 {
					if n, e := strconv.Atoi(strings.TrimSpace(parts[1])); e == nil {
						lineNum = n
						if len(parts) >= 4 {
							text = strings.TrimSpace(parts[3])
						}
					}
				}
				break
			}
			if lineNum > 0 {
				srcLines := strings.Split(src, "\n")
				start := lineNum - 2
				if start < 0 {
					start = 0
				}
				end := lineNum + 1
				if end > len(srcLines) {
					end = len(srcLines)
				}
				var snippet []string
				for i := start; i < end; i++ {
					snippet = append(snippet, fmt.Sprintf("%4d | %s", i+1, srcLines[i]))
				}
				return nil, fmt.Errorf("syntax error at line %d: %s\n%s", lineNum, text, strings.Join(snippet, "\n"))
			}
			return nil, fmt.Errorf("cobc parse error:\n%s", msg)
		}
	}

	var ast AST
	lines := strings.Split(src, "\n")
	inVars := false
	inProc := false
	for i := 0; i < len(lines); i++ {
		l := strings.TrimSpace(lines[i])
		upper := strings.ToUpper(l)
		switch {
		case strings.HasPrefix(upper, "WORKING-STORAGE SECTION"):
			inVars = true
		case strings.HasPrefix(upper, "PROCEDURE DIVISION"):
			inVars = false
			inProc = true
			continue
		}
		if inVars {
			if strings.HasPrefix(l, "01 ") || strings.HasPrefix(l, "77 ") {
				parts := strings.Fields(strings.TrimSuffix(l, "."))
				if len(parts) >= 3 {
					name := strings.ToLower(parts[1])
					var occurs int
					var pic string
					for j := 2; j < len(parts); j++ {
						if parts[j] == "OCCURS" && j+2 < len(parts) {
							occurs, _ = strconv.Atoi(parts[j+1])
							j += 2
						} else if parts[j] == "PIC" && j+1 < len(parts) {
							pic = parts[j+1]
							j++
						}
					}
					ast.Vars = append(ast.Vars, Var{Name: name, Picture: pic, Occurs: occurs})
				}
			}
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
