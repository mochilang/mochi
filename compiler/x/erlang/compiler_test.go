//go:build slow

package erlang_test

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strconv"
	"strings"
	"testing"

	erlang "mochi/compiler/x/erlang"
)

func TestCompilePrograms(t *testing.T) {
	if _, err := exec.LookPath("escript"); err != nil {
		t.Skip("escript not installed")
	}

	progs, err := erlang.ListPrograms()
	if err != nil {
		t.Fatal(err)
	}

	outDir := filepath.Join("tests", "machine", "x", "erlang")
	os.MkdirAll(outDir, 0o755)
	erlang.RemoveOldArtifacts(outDir)

	for _, src := range progs {
		name := strings.TrimSuffix(filepath.Base(src), filepath.Ext(src))
		t.Run(name, func(t *testing.T) {
			erlPath := filepath.Join(outDir, name+".erl")
			if err := erlang.WriteFile(src, erlPath); err != nil {
				t.Fatalf("compile error: %v", err)
			}
			cmd := exec.Command("escript", erlPath)
			var buf bytes.Buffer
			cmd.Stdout = &buf
			cmd.Stderr = &buf
			if err := cmd.Run(); err != nil {
				errFile := filepath.Join(outDir, name+".error")
				line := extractLineNumber(buf.String())
				ctx := contextLines(erlPath, line, 2)
				msg := fmt.Sprintf("%v\n%s\n%s", err, buf.String(), ctx)
				os.WriteFile(errFile, []byte(msg), 0644)
				t.Fatalf("execution error: %v", err)
			}
			outFile := filepath.Join(outDir, name+".out")
			os.WriteFile(outFile, bytes.TrimSpace(buf.Bytes()), 0644)
		})
	}
}

func extractLineNumber(out string) int {
	parts := strings.Split(out, ":")
	if len(parts) >= 2 {
		if n, err := strconv.Atoi(parts[1]); err == nil {
			return n
		}
	}
	return 0
}

func contextLines(file string, line, n int) string {
	data, err := os.ReadFile(file)
	if err != nil || line <= 0 {
		return ""
	}
	lines := strings.Split(string(data), "\n")
	start := line - n - 1
	if start < 0 {
		start = 0
	}
	end := line + n
	if end > len(lines) {
		end = len(lines)
	}
	var b strings.Builder
	for i := start; i < end; i++ {
		fmt.Fprintf(&b, "%4d: %s\n", i+1, lines[i])
	}
	return b.String()
}
