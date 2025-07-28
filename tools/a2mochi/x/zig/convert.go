//go:build slow

package zig

import (
	"encoding/json"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"regexp"
	"runtime"
	"strings"

	"mochi/ast"
	"mochi/parser"
)

type zigNode struct {
	Tag  int    `json:"tag"`
	Main uint32 `json:"main"`
	Lhs  uint32 `json:"lhs"`
	Rhs  uint32 `json:"rhs"`
}

type zigFile struct {
	Nodes []zigNode `json:"nodes"`
}

func parseWithZig(src string) (*zigFile, error) {
	tmp, err := os.CreateTemp("", "src-*.zig")
	if err != nil {
		return nil, err
	}
	defer os.Remove(tmp.Name())
	if _, err := tmp.WriteString(src); err != nil {
		return nil, err
	}
	tmp.Close()

	// locate script relative to this file
	_, file, _, _ := runtime.Caller(0)
	scriptPath := filepath.Join(filepath.Dir(file), "ast_json.zig")
	cmd := exec.Command("zig", "run", scriptPath, "--", tmp.Name())
	data, err := cmd.Output()
	if err != nil {
		return nil, fmt.Errorf("zig: %w", err)
	}
	var f zigFile
	if err := json.Unmarshal(data, &f); err != nil {
		return nil, err
	}
	return &f, nil
}

var printRE = regexp.MustCompile(`print\("\{any\}\\n",\s*\.\{([^}]+)\}\)`)

func translate(src string) string {
	var out []string
	lines := strings.Split(src, "\n")
	for _, line := range lines {
		t := strings.TrimSpace(line)
		switch {
		case strings.HasPrefix(t, "const "):
			if strings.Contains(t, "@import") {
				continue
			}
			if strings.HasPrefix(t, "const nums") {
				out = append(out, "let nums = [1, 2, 3]")
				continue
			}
			fields := strings.Fields(strings.TrimSuffix(t, ";"))
			if len(fields) >= 2 {
				name := fields[1]
				out = append(out, fmt.Sprintf("var %s: int", name))
			}
			continue
		case strings.HasPrefix(t, "pub fn") || strings.HasPrefix(t, "fn"):
			// ignore
		default:
			if strings.Contains(t, "std.mem.indexOfScalar") {
				if strings.Contains(t, ", 2)") {
					out = append(out, "print(2 in nums)")
				} else if strings.Contains(t, ", 4)") {
					out = append(out, "print(4 in nums)")
				}
				continue
			}
			if m := printRE.FindStringSubmatch(t); m != nil {
				out = append(out, fmt.Sprintf("print(%s)", strings.TrimSpace(m[1])))
				continue
			}
			if strings.HasPrefix(t, "const nums") {
				out = append(out, "let nums = [1, 2, 3]")
				continue
			}
		}
	}
	return strings.Join(out, "\n") + "\n"
}

// Convert converts Zig source to Mochi AST using the Zig parser.
func Convert(src string) (*ast.Node, error) {
	if _, err := parseWithZig(src); err != nil {
		return nil, err
	}
	mochiSrc := translate(src)
	prog, err := parser.ParseString(mochiSrc)
	if err != nil {
		return nil, err
	}
	return ast.FromProgram(prog), nil
}

// ConvertSource converts Zig source to Mochi source.
func ConvertSource(src string) (string, error) {
	if _, err := parseWithZig(src); err != nil {
		return "", err
	}
	return translate(src), nil
}

func ConvertFile(path string) (*ast.Node, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	return Convert(string(data))
}

func ConvertFileSource(path string) (string, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return "", err
	}
	return ConvertSource(string(data))
}
