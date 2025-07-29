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
	Kind     string  `json:"Kind"`
	Value    string  `json:"Value,omitempty"`
	Children []*Node `json:"Children,omitempty"`
}

// Param describes a method parameter.
type Param struct {
	Name string `json:"Name"`
	Type string `json:"Type"`
}

// Method represents a method in a type declaration.
type Method struct {
	Name      string   `json:"Name"`
	Params    []Param  `json:"Params"`
	Ret       string   `json:"Ret"`
	Access    string   `json:"Access"`
	Static    bool     `json:"Static"`
	Body      []string `json:"Body"`
	Ast       *Node    `json:"Ast,omitempty"`
	StartLine int      `json:"StartLine"`
	EndLine   int      `json:"EndLine"`
	Doc       string   `json:"Doc"`
}

// Field represents a field or property in a type declaration.
type Field struct {
	Name   string `json:"Name"`
	Type   string `json:"Type"`
	Access string `json:"Access"`
	Line   int    `json:"Line"`
	Value  string `json:"Value"`
	Ast    *Node  `json:"Ast,omitempty"`
	Static bool   `json:"Static"`
	Doc    string `json:"Doc"`
}

// Type describes a type declaration.
type Type struct {
	Name      string   `json:"Name"`
	Kind      string   `json:"Kind"`
	Access    string   `json:"Access"`
	StartLine int      `json:"StartLine"`
	EndLine   int      `json:"EndLine"`
	Fields    []Field  `json:"Fields"`
	Methods   []Method `json:"Methods"`
	Doc       string   `json:"Doc"`
}

// Program represents a parsed C# program.
type Program struct {
	Types []Type `json:"Types"`
	Src   string `json:"Src"`
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
