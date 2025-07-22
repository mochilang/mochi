//go:build slow

package swifttrans_test

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"
	"time"

	"mochi/compiler/x/testutil"
	"mochi/golden"
	"mochi/parser"
	swifttrans "mochi/transpiler/x/swift"
	"mochi/types"
)

func TestSwiftTranspiler_Rosetta_Golden(t *testing.T) {
	swiftExe := ensureSwift(t)
	root := testutil.FindRepoRoot(t)
	outDir := filepath.Join(root, "tests", "rosetta", "transpiler", "Swift")
	os.MkdirAll(outDir, 0o755)

	golden.RunWithSummary(t, "tests/rosetta/x/Mochi", ".mochi", ".out", func(src string) ([]byte, error) {
		base := strings.TrimSuffix(filepath.Base(src), ".mochi")
		codePath := filepath.Join(outDir, base+".swift")
		outPath := filepath.Join(outDir, base+".out")
		errPath := filepath.Join(outDir, base+".error")

		prog, err := parser.Parse(src)
		if err != nil {
			_ = os.WriteFile(errPath, []byte(err.Error()), 0o644)
			return nil, err
		}
		env := types.NewEnv(nil)
		if errs := types.Check(prog, env); len(errs) > 0 {
			_ = os.WriteFile(errPath, []byte(errs[0].Error()), 0o644)
			return nil, errs[0]
		}
		ast, err := swifttrans.Transpile(env, prog)
		if err != nil {
			_ = os.WriteFile(errPath, []byte(err.Error()), 0o644)
			return nil, err
		}
		code := ast.Emit()
		if err := os.WriteFile(codePath, code, 0o644); err != nil {
			return nil, err
		}
		out, err := compileAndRunSwiftSrc(t, swiftExe, code)
		if err != nil {
			_ = os.WriteFile(errPath, append([]byte(err.Error()+"\n"), out...), 0o644)
			return nil, err
		}
		outBytes := bytes.TrimSpace(out)
		_ = os.WriteFile(outPath, outBytes, 0o644)
		_ = os.Remove(errPath)
		return outBytes, nil
	})
}

func updateRosetta() {
	root := repoRoot()
	srcDir := filepath.Join(root, "tests", "rosetta", "x", "Mochi")
	binDir := filepath.Join(root, "tests", "rosetta", "transpiler", "Swift")
	docPath := filepath.Join(root, "transpiler", "x", "swift", "ROSETTA.md")
	files, _ := filepath.Glob(filepath.Join(srcDir, "*.mochi"))
	total := len(files)
	compiled := 0
	var lines []string
	for _, f := range files {
		name := strings.TrimSuffix(filepath.Base(f), ".mochi")
		mark := "[ ]"
		if _, err := os.Stat(filepath.Join(binDir, name+".swift")); err == nil {
			compiled++
			mark = "[x]"
		}
		lines = append(lines, "- "+mark+" "+name)
	}
	tsRaw, _ := exec.Command("git", "log", "-1", "--format=%cI").Output()
	tsStr := strings.TrimSpace(string(tsRaw))
	if t, err := time.Parse(time.RFC3339, tsStr); err == nil {
		if loc, lerr := time.LoadLocation("Asia/Bangkok"); lerr == nil {
			tsStr = t.In(loc).Format("2006-01-02 15:04 -0700")
		} else {
			tsStr = t.Format("2006-01-02 15:04 MST")
		}
	} else {
		tsStr = time.Now().Format("2006-01-02 15:04 MST")
	}

	var buf bytes.Buffer
	buf.WriteString("# Swift Rosetta Transpiler\n\n")
	buf.WriteString("Generated Swift code for Mochi Rosetta programs in `tests/rosetta/x/Mochi`. Outputs are stored in `tests/rosetta/transpiler/Swift`. Errors are captured in `.error` files.\n\n")
	buf.WriteString(fmt.Sprintf("Completed: %d/%d\n", compiled, total))
	buf.WriteString(fmt.Sprintf("Last updated: %s\n\n", tsStr))
	buf.WriteString("Checklist:\n")
	buf.WriteString(strings.Join(lines, "\n"))
	buf.WriteString("\n")
	_ = os.WriteFile(docPath, buf.Bytes(), 0o644)
}
