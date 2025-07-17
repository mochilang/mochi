//go:build slow

package scalacode_test

import (
	"bytes"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"
	"unicode"

	scalacode "mochi/compiler/x/scala"
	"mochi/golden"
	"mochi/parser"
	"mochi/types"
)

func repoRootVM(t *testing.T) string {
	dir, err := os.Getwd()
	if err != nil {
		t.Fatal("cannot determine working directory")
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

func sanitizeName(name string) string {
	if name == "" {
		return "Main"
	}
	var b strings.Builder
	for i, r := range name {
		if i == 0 && unicode.IsDigit(r) {
			b.WriteByte('_')
		}
		if unicode.IsLetter(r) || unicode.IsDigit(r) || r == '_' {
			b.WriteRune(r)
		} else {
			b.WriteByte('_')
		}
	}
	return b.String()
}

func TestScalaCompilerVMValid(t *testing.T) {
	if _, err := exec.LookPath("scalac"); err != nil {
		t.Skip("scalac not installed")
	}
	root := repoRootVM(t)
	outDir := filepath.Join(root, "tests", "machine", "x", "scala")
	os.MkdirAll(outDir, 0o755)

	golden.RunWithSummary(t, "tests/vm/valid", ".mochi", ".out", func(src string) ([]byte, error) {
		base := strings.TrimSuffix(filepath.Base(src), ".mochi")
		runName := sanitizeName(base)
		codePath := filepath.Join(outDir, base+".scala")
		outPath := filepath.Join(outDir, base+".out")
		errPath := filepath.Join(outDir, base+".error")

		prog, err := parser.Parse(src)
		if err != nil {
			os.WriteFile(errPath, []byte(err.Error()), 0o644)
			return nil, err
		}
		env := types.NewEnv(nil)
		if errs := types.Check(prog, env); len(errs) > 0 {
			os.WriteFile(errPath, []byte(errs[0].Error()), 0o644)
			return nil, errs[0]
		}
		os.Setenv("MOCHI_HEADER_TIME", "2006-01-02T15:04:05Z")
		os.Setenv("SOURCE_DATE_EPOCH", "0")
		code, err := scalacode.New(env).Compile(prog)
		os.Unsetenv("MOCHI_HEADER_TIME")
		os.Unsetenv("SOURCE_DATE_EPOCH")
		if err != nil {
			os.WriteFile(errPath, []byte(err.Error()), 0o644)
			return nil, err
		}
		if err := os.WriteFile(codePath, code, 0o644); err != nil {
			return nil, err
		}
		tmp := t.TempDir()
		if out, err := exec.Command("scalac", "-d", tmp, codePath).CombinedOutput(); err != nil {
			os.WriteFile(errPath, append([]byte(err.Error()+"\n"), out...), 0o644)
			return nil, err
		}
		cmd := exec.Command("scala", "-cp", tmp, runName)
		if data, err := os.ReadFile(strings.TrimSuffix(src, ".mochi") + ".in"); err == nil {
			cmd.Stdin = bytes.NewReader(data)
		}
		var runOut bytes.Buffer
		cmd.Stdout = &runOut
		cmd.Stderr = &runOut
		if err := cmd.Run(); err != nil {
			os.WriteFile(errPath, append([]byte(err.Error()+"\n"), runOut.Bytes()...), 0o644)
			return nil, err
		}
		outBytes := bytes.TrimSpace(runOut.Bytes())
		os.WriteFile(outPath, outBytes, 0o644)
		os.Remove(errPath)
		return outBytes, nil
	})
}
