package swift_test

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"

	swift "mochi/compiler/x/swift"
)

func TestCompileValidPrograms(t *testing.T) {
	root := filepath.Join("..", "..", "..")
	files, err := filepath.Glob(filepath.Join(root, "tests", "vm", "valid", "*.mochi"))
	if err != nil {
		t.Fatalf("glob: %v", err)
	}
	outDir := filepath.Join(root, "tests", "machine", "x", "swift")
	if err := os.MkdirAll(outDir, 0o755); err != nil {
		t.Fatalf("mkdir: %v", err)
	}
	for _, srcPath := range files {
		name := strings.TrimSuffix(filepath.Base(srcPath), filepath.Ext(srcPath))
		t.Run(name, func(t *testing.T) {
			srcBytes, err := os.ReadFile(srcPath)
			if err != nil {
				t.Fatalf("read: %v", err)
			}
			code, err := swift.Compile(string(srcBytes))
			if err != nil {
				writeError(outDir, name, srcPath, err.Error(), nil)
				t.Fatalf("compile: %v", err)
			}
			swiftFile := filepath.Join(outDir, name+".swift")
			if err := os.WriteFile(swiftFile, code, 0o644); err != nil {
				t.Fatalf("write swift: %v", err)
			}
			cmd := exec.Command("swift", swiftFile)
			out, err := cmd.CombinedOutput()
			if err != nil {
				writeError(outDir, name, swiftFile, err.Error(), out)
				t.Fatalf("swift run: %v", err)
			}
			if err := os.WriteFile(filepath.Join(outDir, name+".out"), out, 0o644); err != nil {
				t.Fatalf("write out: %v", err)
			}
		})
	}
}

func writeError(outDir, name, file, msg string, output []byte) {
	buf := &bytes.Buffer{}
	fmt.Fprintf(buf, "Error processing %s:\n%s\n\n", file, msg)
	if len(output) > 0 {
		fmt.Fprintf(buf, "Compiler output:\n%s\n\n", output)
	}
	src, err := os.ReadFile(file)
	if err == nil {
		lines := strings.Split(string(src), "\n")
		for i, line := range lines {
			fmt.Fprintf(buf, "%4d: %s\n", i+1, line)
		}
	}
	os.WriteFile(filepath.Join(outDir, name+".error"), buf.Bytes(), 0o644)
}
