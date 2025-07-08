//go:build slow

package erlang_test

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"regexp"
	"strconv"
	"strings"
	"testing"

	erlang "mochi/compiler/x/erlang"
	"mochi/compiler/x/testutil"
)

func TestCompilePrograms(t *testing.T) {
	if _, err := exec.LookPath("escript"); err != nil {
		t.Skip("escript not installed")
	}

	progs, err := erlang.ListPrograms()
	if err != nil {
		t.Fatal(err)
	}

	root := testutil.FindRepoRoot(t)
	outDir := filepath.Join(root, "tests", "machine", "x", "erlang")
	os.MkdirAll(outDir, 0o755)
	erlang.RemoveOldArtifacts(outDir)

	for _, src := range progs {
		name := strings.TrimSuffix(filepath.Base(src), filepath.Ext(src))
		t.Run(name, func(t *testing.T) {
			data, _ := os.ReadFile(src)
			code, err := erlang.CompileFile(src)
			if err != nil {
				writeError(outDir, name, data, err)
				return
			}
			erlPath := filepath.Join(outDir, name+".erl")
			os.WriteFile(erlPath, code, 0o755)

			cmd := exec.Command("escript", erlPath)
			var buf bytes.Buffer
			cmd.Stdout = &buf
			cmd.Stderr = &buf
			if err := cmd.Run(); err != nil {
				line := extractLineNumber(buf.String())
				ctx := contextLines(string(code), line, 2)
				msg := fmt.Sprintf("%v\n%s\n%s", err, buf.String(), ctx)
				os.WriteFile(filepath.Join(outDir, name+".error"), []byte(msg), 0644)
				return
			}
			outFile := filepath.Join(outDir, name+".out")
			os.WriteFile(outFile, bytes.TrimSpace(buf.Bytes()), 0644)
			os.Remove(filepath.Join(outDir, name+".error"))
		})
	}
}

func writeError(dir, name string, src []byte, err error) {
	line := extractLine(err.Error())
	ctx := contextLines(string(src), line, 2)
	msg := fmt.Sprintf("line %d: %v\n%s", line, err, ctx)
	os.WriteFile(filepath.Join(dir, name+".error"), []byte(msg), 0644)
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

func contextLines(src string, line, n int) string {
	if line <= 0 {
		return ""
	}
	lines := strings.Split(src, "\n")
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

func extractLine(msg string) int {
	re := regexp.MustCompile(`line (\d+)`)
	if m := re.FindStringSubmatch(msg); m != nil {
		if n, err := strconv.Atoi(m[1]); err == nil {
			return n
		}
	}
	return 0
}
