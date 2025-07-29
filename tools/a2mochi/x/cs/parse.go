//go:build slow

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

// Program represents a parsed C# source file in simplified form.
type Program struct {
	Types []Type
	Src   string
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
	Doc    string
}

type Func struct {
	Name      string
	Params    []Param
	Ret       string
	Access    string
	Static    bool
	Body      []string
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
	return parseRoslyn(src)
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
		if _, err := exec.LookPath("dotnet"); err != nil {
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
		cmd := exec.Command("dotnet", "publish", "-c", "Release", "-o", dir)
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
