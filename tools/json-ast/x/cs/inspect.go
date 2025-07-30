package cs

import (
	"embed"
	"encoding/json"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"runtime"
	"sync"
)

var (
	helperOnce sync.Once
	helperPath string
	helperErr  error
	//go:embed internal/AstJson.cs internal/AstJson.csproj
	helperFS embed.FS
)

// Node represents a simplified AST node produced by the helper.
type Node struct {
	Kind     string  `json:"kind"`
	Value    string  `json:"value,omitempty"`
	Children []*Node `json:"children,omitempty"`
}

// Param describes a method parameter.
type Param struct {
	Name string `json:"name"`
	Type string `json:"type"`
}

// Method represents a method in a type declaration.
type Method struct {
	Name      string  `json:"name"`
	Params    []Param `json:"params"`
	Ret       string  `json:"ret"`
	Access    string  `json:"access"`
	Static    bool    `json:"static"`
	Ast       *Node   `json:"ast,omitempty"`
	StartLine int     `json:"startline"`
	EndLine   int     `json:"endline"`
	Doc       string  `json:"doc"`
}

// Field represents a field or property in a type declaration.
type Field struct {
	Name   string `json:"name"`
	Type   string `json:"type"`
	Access string `json:"access"`
	Line   int    `json:"line"`
	Value  string `json:"value"`
	Ast    *Node  `json:"ast,omitempty"`
	Static bool   `json:"static"`
	Doc    string `json:"doc"`
}

// Type describes a type declaration.
type Type struct {
	Name      string   `json:"name"`
	Kind      string   `json:"kind"`
	Access    string   `json:"access"`
	StartLine int      `json:"startline"`
	EndLine   int      `json:"endline"`
	Fields    []Field  `json:"fields"`
	Methods   []Method `json:"methods"`
	Doc       string   `json:"doc"`
}

// Program represents a parsed C# program.
type Program struct {
	Types []Type `json:"types"`
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
		dir, err := os.MkdirTemp("", "json-ast-cs-")
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

// Inspect parses the given C# source code and returns its AST as a Program.
func Inspect(src string) (*Program, error) {
	if err := ensureHelper(); err != nil {
		return nil, err
	}
	tmp, err := os.CreateTemp("", "csast-*.cs")
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
			return nil, fmt.Errorf("dotnet: %s", string(ee.Stderr))
		}
		return nil, err
	}
	var prog Program
	if err := json.Unmarshal(out, &prog); err != nil {
		return nil, err
	}
	return &prog, nil
}
