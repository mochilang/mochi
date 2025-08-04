//go:build slow

package mochi_test

import (
	"bytes"
	"flag"
	"fmt"
	"os"
	"path/filepath"
	"sort"
	"strconv"
	"strings"
	"testing"

	mochi "mochi/aster/x/mochi"
	"mochi/diagnostic"
)

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

func TestRosettaDecorate(t *testing.T) {
	root := findRepoRoot(t)
	srcDir := filepath.Join(root, "tests", "rosetta", "x", "Mochi")
	outDir := filepath.Join(root, "tests", "aster", "x", "mochi", "rosetta")
	if err := os.MkdirAll(outDir, 0o755); err != nil {
		t.Fatalf("mkdir: %v", err)
	}

	files, err := filepath.Glob(filepath.Join(srcDir, "*.mochi"))
	if err != nil {
		t.Fatalf("glob: %v", err)
	}
	sort.Strings(files)
	if len(files) == 0 {
		t.Fatalf("no mochi files in %s", srcDir)
		return
	}
	startIdx := 0
	if idxStr := os.Getenv("MOCHI_ROSETTA_INDEX"); idxStr != "" {
		idx, err := strconv.Atoi(idxStr)
		if err != nil || idx < 1 || idx > len(files) {
			t.Fatalf("invalid MOCHI_ROSETTA_INDEX: %s", idxStr)
		}
		startIdx = idx - 1
		files = []string{files[startIdx]}
	}

	var passed, failed int
	var firstFail string
	for i, src := range files {
		name := strings.TrimSuffix(filepath.Base(src), ".mochi")
		idx := startIdx + i + 1
		testName := fmt.Sprintf("%03d_%s", idx, name)
		ok := t.Run(testName, func(t *testing.T) {
			outPath := filepath.Join(outDir, fmt.Sprintf("%d.out.mochi", idx))
			errPath := filepath.Join(outDir, fmt.Sprintf("%d.error", idx))

			defer func() {
				if r := recover(); r != nil {
					msg := fmt.Sprintf("panic: %v", r)
					_ = os.WriteFile(errPath, []byte(msg+"\n"), 0o644)
					_ = os.Remove(outPath)
					t.Errorf("%s", msg)
				}
			}()

			data, err := os.ReadFile(src)
			if err != nil {
				t.Fatalf("read: %v", err)
			}
			prog, err := mochi.Inspect(string(data), mochi.Option{WithPositions: true})
			if err != nil {
				writeError(errPath, src, err)
				_ = os.Remove(outPath)
				t.Errorf("inspect: %v", err)
				return
			}
			dec, err := mochi.Decorate(prog)
			if err != nil {
				writeError(errPath, src, err)
				_ = os.Remove(outPath)
				t.Errorf("decorate: %v", err)
				return
			}
			printed, err := mochi.Print(dec)
			if err != nil {
				t.Fatalf("print: %v", err)
			}
			_ = os.Remove(errPath)
			got := bytes.TrimSpace([]byte(printed))
			want, _ := os.ReadFile(outPath)
			want = bytes.TrimSpace(want)
			if err := os.WriteFile(outPath, append(got, '\n'), 0o644); err != nil {
				t.Fatalf("write: %v", err)
			}
			if !updating() && len(want) > 0 && !bytes.Equal(got, want) {
				t.Errorf("output mismatch\nGot: %s\nWant: %s", got, want)
			}
		})
		if ok {
			passed++
		} else {
			failed++
			if firstFail == "" {
				firstFail = name
			}
		}
	}
	t.Logf("Summary: %d passed, %d failed", passed, failed)
	if firstFail != "" {
		t.Fatalf("first failing program: %s", firstFail)
	}
}

func writeError(errPath, src string, err error) {
	msg := err.Error()
	if d, ok := err.(diagnostic.Diagnostic); ok {
		d.Pos.Filename = src
		msg = d.Format()
	}
	_ = os.WriteFile(errPath, []byte(msg+"\n"), 0o644)
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
