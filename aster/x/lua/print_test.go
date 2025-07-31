//go:build slow

package lua_test

import (
	"encoding/json"
	"flag"
	"os"
	"os/exec"
	"path/filepath"
	"regexp"
	"sort"
	"strings"
	"testing"

	lua "mochi/aster/x/lua"
)

func shouldUpdate() bool {
	f := flag.Lookup("update")
	return f != nil && f.Value.String() == "true"
}

func sanitizeTables(b []byte) []byte {
	re := regexp.MustCompile(`table: 0x[0-9a-fA-F]+`)
	return re.ReplaceAll(b, []byte("table"))
}

func ensureLua(t *testing.T) {
	if _, err := exec.LookPath("lua"); err != nil {
		t.Skip("lua not installed")
	}
}

func TestPrint_Golden(t *testing.T) {
	ensureLua(t)
	root := repoRoot(t)
	srcDir := filepath.Join(root, "tests", "transpiler", "x", "lua")
	outDir := filepath.Join(root, "tests", "aster", "x", "lua")
	os.MkdirAll(outDir, 0o755)

	files, err := filepath.Glob(filepath.Join(srcDir, "*.lua"))
	if err != nil {
		t.Fatal(err)
	}
	sort.Strings(files)
	if len(files) > 50 {
		files = files[:50]
	}

	for _, src := range files {
		name := strings.TrimSuffix(filepath.Base(src), ".lua")
		t.Run(name, func(t *testing.T) {
			data, err := os.ReadFile(src)
			if err != nil {
				t.Fatalf("read src: %v", err)
			}
			prog, err := lua.InspectWithOption(string(data), lua.Option{Positions: true})
			if err != nil {
				t.Fatalf("inspect: %v", err)
			}
			astJSON, err := json.MarshalIndent(prog, "", "  ")
			if err != nil {
				t.Fatalf("marshal: %v", err)
			}
			astJSON = append(astJSON, '\n')
			jsonPath := filepath.Join(outDir, name+".lua.json")
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
			out, err := lua.Print(prog)
			if err != nil {
				t.Fatalf("print: %v", err)
			}
			outPath := filepath.Join(outDir, name+".lua")
			if shouldUpdate() {
				if err := os.WriteFile(outPath, []byte(out), 0644); err != nil {
					t.Fatalf("write out: %v", err)
				}
			}
			cmd := exec.Command("lua", outPath)
			got, err := cmd.CombinedOutput()
			if err != nil {
				t.Fatalf("run printed: %v\n%s", err, got)
			}
			want, err := exec.Command("lua", src).CombinedOutput()
			if err != nil {
				t.Fatalf("run original: %v\n%s", err, want)
			}
			outFile := filepath.Join(outDir, name+".out")
			if shouldUpdate() {
				if err := os.WriteFile(outFile, got, 0644); err != nil {
					t.Fatalf("write out file: %v", err)
				}
			}
			if name == "for_map_collection" {
				gLines := strings.Split(strings.TrimSpace(string(got)), "\n")
				wLines := strings.Split(strings.TrimSpace(string(want)), "\n")
				sort.Strings(gLines)
				sort.Strings(wLines)
				got = []byte(strings.Join(gLines, "\n"))
				want = []byte(strings.Join(wLines, "\n"))
			}
			got = sanitizeTables(got)
			want = sanitizeTables(want)
			if string(got) != string(want) {
				if strings.HasPrefix(string(got), "table:") && strings.HasPrefix(string(want), "table:") {
					// table addresses differ; ignore
				} else {
					t.Fatalf("output mismatch\n--- got ---\n%s\n--- want ---\n%s", got, want)
				}
			}
		})
	}
}
