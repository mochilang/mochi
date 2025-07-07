package dart

import (
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"

	"mochi/parser"
)

// TestCompileValid compiles programs in tests/vm/valid using the Dart compiler.
func TestCompileValid(t *testing.T) {
	if _, err := exec.LookPath("dart"); err != nil {
		t.Skip("dart not installed")
	}

	files, err := filepath.Glob(filepath.Join("tests", "vm", "valid", "*.mochi"))
	if err != nil {
		t.Fatalf("glob: %v", err)
	}
	outDir := filepath.Join("tests", "machine", "x", "dart")
	os.MkdirAll(outDir, 0755)

	for _, src := range files {
		name := strings.TrimSuffix(filepath.Base(src), ".mochi")
		t.Run(name, func(t *testing.T) {
			prog, err := parser.Parse(src)
			if err != nil {
				os.WriteFile(filepath.Join(outDir, name+".error"), []byte(err.Error()), 0644)
				t.Skip("parse error")
				return
			}
			code, err := New().Compile(prog)
			codePath := filepath.Join(outDir, name+".dart")
			os.WriteFile(codePath, code, 0644)
			if err != nil {
				os.WriteFile(filepath.Join(outDir, name+".error"), []byte(err.Error()), 0644)
				return
			}
			cmd := exec.Command("dart", codePath)
			out, err := cmd.CombinedOutput()
			if err != nil {
				os.WriteFile(filepath.Join(outDir, name+".error"), out, 0644)
			} else {
				os.WriteFile(filepath.Join(outDir, name+".out"), out, 0644)
			}
		})
	}
}
