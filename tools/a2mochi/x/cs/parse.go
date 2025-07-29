//go:build slow

package cs

import (
	"embed"
	"encoding/json"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"regexp"
	"runtime"
	"strings"
	"sync"
)

var (
	helperOnce sync.Once
	helperPath string
	helperErr  error
	//go:embed internal/AstJson.cs internal/AstJson.csproj
	helperFS    embed.FS
	typeRE      = regexp.MustCompile(`(?i)^\s*(?:public\s+|private\s+|protected\s+)?(class|struct|interface|enum)\s+([A-Za-z_][A-Za-z0-9_]*)`)
	funcRE      = regexp.MustCompile(`(?i)^\s*(?:public\s+|private\s+|protected\s+)?(?:static\s+)?([A-Za-z0-9_<>\[\],\s]+)\s+([A-Za-z_][A-Za-z0-9_]*)(?:<[^>]+>)?\s*\(([^)]*)\)\s*\{?`)
	fieldRE     = regexp.MustCompile(`(?i)^\s*(?:public\s+|private\s+|protected\s+)?([A-Za-z0-9_<>\[\],\s]+)\s+([A-Za-z_][A-Za-z0-9_]*)`)
	usingRE     = regexp.MustCompile(`^\s*using\s+`)
	namespaceRE = regexp.MustCompile(`^\s*namespace\s+`)
)

// Program represents a parsed C# source file in simplified form.
type Program struct {
	Types []Type
	Src   string
}

type Node struct {
	Kind     string  `json:"Kind"`
	Value    string  `json:"Value,omitempty"`
	Children []*Node `json:"Children,omitempty"`
}

type Type struct {
	Name      string
	Kind      string
	Access    string
	StartLine int
	EndLine   int
	Fields    []Field
	Methods   []Func
	Doc       string
}

type Field struct {
	Name   string
	Type   string
	Access string
	Line   int
	Value  string
	Ast    *Node `json:"Ast,omitempty"`
	Static bool
	Doc    string
}

type Func struct {
	Name      string
	Params    []Param
	Ret       string
	Access    string
	Static    bool
	Body      []string
	Ast       *Node
	StartLine int
	EndLine   int
	Doc       string
}

type Param struct {
	Name string
	Type string
}

// Parse parses C# source code using the embedded Roslyn helper.
func Parse(src string) (*Program, error) {
	p, err := parseRoslyn(src)
	if err == nil {
		p.Src = src
		return p, nil
	}
	simple, err2 := parseSimple(src)
	if err2 == nil {
		simple.Src = src
		return simple, nil
	}
	return nil, err
}

func parseRoslyn(src string) (*Program, error) {
	if err := ensureHelper(); err != nil {
		return nil, err
	}
	tmp, err := os.CreateTemp("", "a2mochi-*.cs")
	if err != nil {
		return nil, err
	}
	defer os.Remove(tmp.Name())
	if _, err := tmp.WriteString(src); err != nil {
		return nil, err
	}
	tmp.Close()
	cmd := exec.Command(helperPath, tmp.Name())
	out, err := cmd.Output()
	if err != nil {
		if ee, ok := err.(*exec.ExitError); ok {
			return nil, fmt.Errorf("parser error: %s", string(ee.Stderr))
		}
		return nil, err
	}
	var prog Program
	if err := json.Unmarshal(out, &prog); err != nil {
		return nil, err
	}
	return &prog, nil
}

func ensureHelper() error {
	helperOnce.Do(func() {
		dotnet := os.Getenv("A2MOCHI_DOTNET")
		if dotnet == "" {
			dotnet, _ = exec.LookPath("dotnet")
		}
		if dotnet == "" {
			helperErr = fmt.Errorf("dotnet not installed")
			return
		}
		dir, err := os.MkdirTemp("", "a2mochi-cshelper-")
		if err != nil {
			helperErr = err
			return
		}
		csPath := filepath.Join(dir, "AstJson.cs")
		projPath := filepath.Join(dir, "AstJson.csproj")
		if data, err := helperFS.ReadFile("internal/AstJson.cs"); err == nil {
			os.WriteFile(csPath, data, 0644)
		} else {
			helperErr = err
			return
		}
		if data, err := helperFS.ReadFile("internal/AstJson.csproj"); err == nil {
			os.WriteFile(projPath, data, 0644)
		} else {
			helperErr = err
			return
		}
		cmd := exec.Command(dotnet, "publish", "-c", "Release", "-o", dir)
		cmd.Dir = dir
		if out, err := cmd.CombinedOutput(); err != nil {
			helperErr = fmt.Errorf("dotnet publish: %v: %s", err, out)
			return
		}
		helperPath = filepath.Join(dir, "AstJson")
		if runtime.GOOS == "windows" {
			helperPath += ".exe"
		}
	})
	return helperErr
}

// parseSimple parses a restricted subset of C# source code without Roslyn.
func parseSimple(src string) (*Program, error) {
	lines := strings.Split(src, "\n")
	var prog Program
	var cur *Type
	depth := 0
	var unknownLine int = -1
	var unknownCol int
	var unknownMsg string
	var pendingDoc []string
	for i := 0; i < len(lines); i++ {
		l := strings.TrimSpace(lines[i])
		if strings.HasPrefix(l, "//") || l == "" {
			if strings.HasPrefix(l, "//") {
				pendingDoc = append(pendingDoc, strings.TrimSpace(strings.TrimPrefix(l, "//")))
			}
			continue
		}
		if m := typeRE.FindStringSubmatch(l); m != nil {
			t := &Type{Name: m[2], Kind: strings.ToLower(m[1]), StartLine: i + 1, Doc: strings.Join(pendingDoc, "\n")}
			if strings.Contains(l, "public") {
				t.Access = "public"
			} else if strings.Contains(l, "protected") {
				t.Access = "protected"
			} else if strings.Contains(l, "private") {
				t.Access = "private"
			}
			pendingDoc = nil
			prog.Types = append(prog.Types, *t)
			cur = &prog.Types[len(prog.Types)-1]
			if strings.Contains(l, "{") {
				depth++
			} else if i+1 < len(lines) && strings.TrimSpace(lines[i+1]) == "{" {
				depth++
				i++
			}
			continue
		}
		if strings.Contains(l, "{") {
			depth++
		}
		if strings.Contains(l, "}") {
			depth--
			if depth == 0 {
				if cur != nil {
					cur.EndLine = i + 1
				}
				cur = nil
			}
			continue
		}
		if cur == nil {
			continue
		}
		if m := funcRE.FindStringSubmatch(l); m != nil {
			fn := Func{Name: m[2], Ret: strings.TrimSpace(m[1]), StartLine: i + 1, Doc: strings.Join(pendingDoc, "\n")}
			if strings.Contains(l, "static") {
				fn.Static = true
			}
			if strings.Contains(l, "public") {
				fn.Access = "public"
			} else if strings.Contains(l, "protected") {
				fn.Access = "protected"
			} else if strings.Contains(l, "private") {
				fn.Access = "private"
			}
			pendingDoc = nil
			params := strings.Split(strings.TrimSpace(m[3]), ",")
			for _, p := range params {
				p = strings.TrimSpace(p)
				if p == "" {
					continue
				}
				parts := strings.Fields(p)
				if len(parts) == 1 {
					fn.Params = append(fn.Params, Param{Name: parts[0]})
				} else {
					fn.Params = append(fn.Params, Param{Type: parts[0], Name: parts[1]})
				}
			}
			if !strings.Contains(l, "{") && i+1 < len(lines) && strings.TrimSpace(lines[i+1]) == "{" {
				i++
			}
			var body []string
			braces := 1
			for j := i + 1; j < len(lines); j++ {
				if strings.Contains(lines[j], "{") {
					braces++
				}
				if strings.Contains(lines[j], "}") {
					braces--
					if braces == 0 {
						i = j
						fn.EndLine = j + 1
						break
					}
				}
				body = append(body, lines[j])
			}
			fn.Body = body
			cur.Methods = append(cur.Methods, fn)
			continue
		}
		if m := fieldRE.FindStringSubmatch(l); m != nil && strings.HasSuffix(l, ";") {
			fType := strings.TrimSpace(m[1])
			static := false
			if strings.Contains(fType, "static") {
				fType = strings.ReplaceAll(fType, "static", "")
				fType = strings.TrimSpace(fType)
				static = true
			}
			f := Field{Name: m[2], Type: fType, Line: i + 1, Doc: strings.Join(pendingDoc, "\n"), Static: static}
			if strings.Contains(l, "public") {
				f.Access = "public"
			} else if strings.Contains(l, "protected") {
				f.Access = "protected"
			} else if strings.Contains(l, "private") {
				f.Access = "private"
			}
			if idx := strings.Index(l, "="); idx != -1 {
				f.Value = strings.TrimSuffix(strings.TrimSpace(l[idx+1:]), ";")
				if strings.Contains(l[:idx], "static") {
					f.Static = true
				}
			}
			cur.Fields = append(cur.Fields, f)
			pendingDoc = nil
			continue
		}

		if usingRE.MatchString(l) || namespaceRE.MatchString(l) {
			continue
		}

		if unknownLine == -1 {
			unknownLine = i
			unknownCol = len(lines[i]) - len(strings.TrimLeft(lines[i], " \t"))
			unknownMsg = "unsupported line"
		}
	}
	if len(prog.Types) == 0 {
		return nil, fmt.Errorf("no types found")
	}
	if unknownLine != -1 {
		start := unknownLine - 2
		if start < 0 {
			start = 0
		}
		end := unknownLine + 2
		if end >= len(lines) {
			end = len(lines) - 1
		}
		var ctx strings.Builder
		for i := start; i <= end; i++ {
			ctx.WriteString(fmt.Sprintf("%3d| %s\n", i+1, lines[i]))
			if i == unknownLine {
				ctx.WriteString("   | " + strings.Repeat(" ", unknownCol) + "^\n")
			}
		}
		return &prog, fmt.Errorf("line %d:%d: %s\n%s", unknownLine+1, unknownCol+1, unknownMsg, strings.TrimSuffix(ctx.String(), "\n"))
	}
	return &prog, nil
}
