package mochi_test

import (
	"bufio"
	"bytes"
	"flag"
	"fmt"
	"os"
	"path/filepath"
	"strconv"
	"strings"
	"testing"

	mochi "mochi/aster/x/mochi"
)

func readIndex(path string) ([]string, error) {
	f, err := os.Open(path)
	if err != nil {
		return nil, err
	}
	defer f.Close()
	var names []string
	scanner := bufio.NewScanner(f)
	for scanner.Scan() {
		parts := strings.Fields(scanner.Text())
		if len(parts) == 2 {
			names = append(names, parts[1])
		}
	}
	if err := scanner.Err(); err != nil {
		return nil, err
	}
	return names, nil
}

func findRepoRoot(t *testing.T) string {
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

func TestDecorator_Rosetta_Golden(t *testing.T) {
	root := findRepoRoot(t)
	srcDir := filepath.Join(root, "tests", "rosetta", "x", "Mochi")
	outDir := filepath.Join(root, "tests", "aster", "x", "mochi")
	os.MkdirAll(outDir, 0o755)

	names, err := readIndex(filepath.Join(srcDir, "index.txt"))
	if err != nil {
		t.Fatalf("read index: %v", err)
	}
	if len(names) == 0 {
		t.Fatalf("no mochi files in %s", srcDir)
	}

	files := make([]string, len(names))
	for i, n := range names {
		files[i] = filepath.Join(srcDir, n)
	}

	if idxStr := os.Getenv("MOCHI_ROSETTA_INDEX"); idxStr != "" {
		idx, err := strconv.Atoi(idxStr)
		if err != nil || idx < 1 || idx > len(files) {
			t.Fatalf("invalid MOCHI_ROSETTA_INDEX: %s", idxStr)
		}
		files = []string{files[idx-1]}
	}

	for i, src := range files {
		name := strings.TrimSuffix(filepath.Base(src), ".mochi")
		t.Run(fmt.Sprintf("%03d_%s", i+1, name), func(t *testing.T) {
			outPath := filepath.Join(outDir, name+".out.mochi")
			errPath := filepath.Join(outDir, name+".error")

			data, err := os.ReadFile(src)
			if err != nil {
				t.Fatalf("read: %v", err)
			}
			prog, err := mochi.Inspect(string(data), mochi.Option{Filename: src})
			if err != nil {
				_ = os.WriteFile(errPath, []byte(err.Error()), 0o644)
				t.Fatalf("inspect: %v", err)
			}
			decorated, err := mochi.Decorate(prog)
			if err != nil {
				_ = os.WriteFile(errPath, []byte(err.Error()), 0o644)
				t.Fatalf("decorate: %v", err)
			}
			printed, err := mochi.Print(decorated)
			if err != nil {
				_ = os.WriteFile(errPath, []byte(err.Error()), 0o644)
				t.Fatalf("print: %v", err)
			}
			_ = os.Remove(errPath)

			got := bytes.TrimSpace([]byte(printed))
			want, _ := os.ReadFile(outPath)
			want = bytes.TrimSpace(want)

			if updating() || len(want) == 0 {
				_ = os.WriteFile(outPath, got, 0o644)
				return
			}
			if !bytes.Equal(got, want) {
				t.Errorf("output mismatch\nGot: %s\nWant: %s", got, want)
			}
		})
	}
}

func updating() bool {
	f := flag.Lookup("update")
	if f == nil {
		return false
	}
	if getter, ok := f.Value.(interface{ Get() any }); ok {
		if v, ok2 := getter.Get().(bool); ok2 {
			return v
		}
	}
	return false
}
