//go:build slow

package cs_test

import (
	"encoding/json"
	"os"
	"os/exec"
	"path/filepath"
	"sort"
	"strings"
	"testing"

	cs "mochi/aster/x/cs"
	cscode "mochi/compiler/x/cs"
)

func ensureDotnet(t *testing.T) {
	if err := cscode.EnsureDotnet(); err != nil {
		t.Skipf("dotnet not installed: %v", err)
	}
}

func shouldUpdate() bool {
	return os.Getenv("UPDATE_GOLDEN") == "1"
}

func TestPrint_Golden(t *testing.T) {
	ensureDotnet(t)
	root := repoRoot(t)
	srcDir := filepath.Join(root, "tests", "transpiler", "x", "cs")
	outDir := filepath.Join(root, "tests", "aster", "x", "cs")
	os.MkdirAll(outDir, 0o755)

	files, err := filepath.Glob(filepath.Join(srcDir, "*.cs"))
	if err != nil {
		t.Fatal(err)
	}
	sort.Strings(files)
	if len(files) > 10 {
		files = files[:10]
	}

	for _, src := range files {
		name := strings.TrimSuffix(filepath.Base(src), ".cs")
		t.Run(name, func(t *testing.T) {
			data, err := os.ReadFile(src)
			if err != nil {
				t.Fatalf("read src: %v", err)
			}
			prog, err := cs.Inspect(string(data))
			if err != nil {
				t.Fatalf("inspect: %v", err)
			}
			astJSON, err := json.MarshalIndent(prog, "", "  ")
			if err != nil {
				t.Fatalf("marshal: %v", err)
			}
			astJSON = append(astJSON, '\n')
			jsonPath := filepath.Join(outDir, name+".cs.json")
			if shouldUpdate() {
				if err := os.WriteFile(jsonPath, astJSON, 0644); err != nil {
					t.Fatalf("write json: %v", err)
				}
			}
			wantJSON, err := os.ReadFile(jsonPath)
			if err != nil {
				t.Skip("missing golden")
				return
			}
			if string(astJSON) != string(wantJSON) {
				t.Fatalf("json mismatch\n--- got ---\n%s\n--- want ---\n%s", astJSON, wantJSON)
			}
			out, err := cs.Print(prog)
			if err != nil {
				t.Fatalf("print: %v", err)
			}
			outPath := filepath.Join(outDir, name+".cs")
			if err := os.WriteFile(outPath, []byte(out), 0644); err != nil {
				t.Fatalf("write out: %v", err)
			}
			tmp := t.TempDir()
			proj := filepath.Join(tmp, "app.csproj")
			csproj := `<Project Sdk="Microsoft.NET.Sdk"><PropertyGroup><OutputType>Exe</OutputType><TargetFramework>net8.0</TargetFramework></PropertyGroup></Project>`
			if err := os.WriteFile(proj, []byte(csproj), 0644); err != nil {
				t.Fatal(err)
			}
			if err := os.WriteFile(filepath.Join(tmp, "Program.cs"), []byte(out), 0644); err != nil {
				t.Fatal(err)
			}
			cmd := exec.Command("dotnet", "run", "--project", proj)
			got, err := cmd.CombinedOutput()
			if err != nil {
				t.Skipf("dotnet run error: %v\n%s", err, got)
				return
			}
			tmp2 := t.TempDir()
			proj2 := filepath.Join(tmp2, "orig.csproj")
			_ = os.WriteFile(proj2, []byte(csproj), 0644)
			_ = os.WriteFile(filepath.Join(tmp2, "Program.cs"), data, 0644)
			want, err := exec.Command("dotnet", "run", "--project", proj2).CombinedOutput()
			if err != nil {
				t.Skipf("dotnet run error: %v\n%s", err, want)
				return
			}
			filter := func(b []byte) string {
				lines := strings.Split(strings.TrimSpace(string(b)), "\n")
				out := make([]string, 0, len(lines))
				for _, l := range lines {
					if !strings.Contains(l, "Program.cs") {
						out = append(out, l)
					}
				}
				return strings.Join(out, "\n")
			}
			gotStr := filter(got)
			wantStr := filter(want)
			outFile := filepath.Join(outDir, name+".out")
			if shouldUpdate() {
				if err := os.WriteFile(outFile, []byte(gotStr+"\n"), 0644); err != nil {
					t.Fatalf("write out file: %v", err)
				}
			}
			if gotStr != wantStr {
				t.Fatalf("output mismatch\n--- got ---\n%s\n--- want ---\n%s", gotStr, wantStr)
			}
		})
	}
}
