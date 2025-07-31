package mochi_test

import (
	"flag"
	"os"
	"path/filepath"
	"testing"

	mochi "mochi/aster/x/mochi"
	"mochi/tools/slt/logic"
)

var updatePrint = flag.Bool("update-print", false, "update golden files")

func repoRootPrint(t *testing.T) string {
	dir, err := os.Getwd()
	if err != nil {
		t.Fatal(err)
	}
	for i := 0; i < 10; i++ {
		if _, err := os.Stat(filepath.Join(dir, "go.mod")); err == nil {
			return dir
		}
		parent := filepath.Dir(dir)
		if parent == dir {
			break
		}
		dir = parent
	}
	t.Fatal("go.mod not found")
	return ""
}

func TestPrint_Golden(t *testing.T) {
	root := repoRootPrint(t)
	srcPattern := filepath.Join(root, "tests", "transpiler", "x", "mochi", "*.mochi")
	files, err := filepath.Glob(srcPattern)
	if err != nil {
		t.Fatal(err)
	}
	if len(files) == 0 {
		t.Fatalf("no files: %s", srcPattern)
	}

	outDir := filepath.Join(root, "tests", "aster", "x", "mochi")
	if err := os.MkdirAll(outDir, 0o755); err != nil {
		t.Fatalf("mkout: %v", err)
	}

	for _, src := range files {
		name := filepath.Base(src)
		t.Run(name, func(t *testing.T) {
			data, err := os.ReadFile(src)
			if err != nil {
				t.Fatalf("read src: %v", err)
			}
			prog, err := mochi.Inspect(string(data))
			if err != nil {
				t.Fatalf("inspect: %v", err)
			}
			js, err := mochi.Marshal(prog)
			if err != nil {
				t.Fatalf("marshal: %v", err)
			}
			jsonPath := filepath.Join(outDir, name+".json")
			if *updatePrint {
				if err := os.WriteFile(jsonPath, append(js, '\n'), 0o644); err != nil {
					t.Fatalf("write json: %v", err)
				}
			}

			printed, err := mochi.Print(prog)
			if err != nil {
				t.Fatalf("print: %v", err)
			}
			outPath := filepath.Join(outDir, name)
			if *updatePrint {
				if err := os.WriteFile(outPath, []byte(printed), 0o644); err != nil {
					t.Fatalf("write src: %v", err)
				}
			}

			got, err := logic.RunMochi(printed, 0)
			if err != nil {
				t.Fatalf("run printed: %v", err)
			}
			ref, err := logic.RunMochi(string(data), 0)
			if err != nil {
				t.Fatalf("run ref: %v", err)
			}
			if got != ref {
				t.Fatalf("output mismatch\nprinted: %s\nreference: %s", got, ref)
			}
			outFile := filepath.Join(outDir, name+".out")
			if *updatePrint {
				if err := os.WriteFile(outFile, []byte(got+"\n"), 0o644); err != nil {
					t.Fatalf("write out: %v", err)
				}
			}
		})
	}
}
