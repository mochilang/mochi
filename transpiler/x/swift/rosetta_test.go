//go:build slow

package swifttrans_test

import (
	"bufio"
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strconv"
	"strings"
	"testing"
	"time"

	"mochi/compiler/x/testutil"
	"mochi/parser"
	swifttrans "mochi/transpiler/x/swift"
	"mochi/types"
)

func TestSwiftTranspiler_Rosetta_Golden(t *testing.T) {
	swiftExe := ensureSwift(t)
	root := testutil.FindRepoRoot(t)
	outDir := filepath.Join(root, "tests", "rosetta", "transpiler", "Swift")
	os.MkdirAll(outDir, 0o755)

	srcDir := filepath.Join(root, "tests", "rosetta", "x", "Mochi")
	idxPath := filepath.Join(srcDir, "index.txt")
	names, err := readIndex(idxPath)
	if err != nil {
		t.Fatalf("read index: %v", err)
	}

	idx := 1
	if v := os.Getenv("ROSETTA_INDEX"); v != "" {
		if n, err := strconv.Atoi(v); err == nil && n > 0 {
			idx = n
		} else {
			t.Fatalf("invalid ROSETTA_INDEX %s", v)
		}
	}
	if idx > len(names) {
		t.Fatalf("index %d out of range", idx)
	}

	files := []string{filepath.Join(srcDir, names[idx-1])}

	runCase := func(src string) error {
		base := strings.TrimSuffix(filepath.Base(src), ".mochi")
		codePath := filepath.Join(outDir, base+".swift")
		outPath := filepath.Join(outDir, base+".out")
		errPath := filepath.Join(outDir, base+".error")

		prog, err := parser.Parse(src)
		if err != nil {
			_ = os.WriteFile(errPath, []byte(err.Error()), 0o644)
			return fmt.Errorf("parse: %w", err)
		}
		env := types.NewEnv(nil)
		if errs := types.Check(prog, env); len(errs) > 0 {
			_ = os.WriteFile(errPath, []byte(errs[0].Error()), 0o644)
			return fmt.Errorf("type: %v", errs[0])
		}
		ast, err := swifttrans.Transpile(env, prog)
		if err != nil {
			_ = os.WriteFile(errPath, []byte(err.Error()), 0o644)
			return fmt.Errorf("transpile: %w", err)
		}
		code := ast.Emit()
		if err := os.WriteFile(codePath, code, 0o644); err != nil {
			return fmt.Errorf("write code: %w", err)
		}
		out, err := compileAndRunSwiftSrc(t, swiftExe, code)
		if err != nil {
			_ = os.WriteFile(errPath, append([]byte(err.Error()+"\n"), out...), 0o644)
			return fmt.Errorf("run: %v", err)
		}
		outBytes := bytes.TrimSpace(out)
		_ = os.WriteFile(outPath, outBytes, 0o644)
		_ = os.Remove(errPath)
		return nil
	}

	for _, src := range files {
		name := strings.TrimSuffix(filepath.Base(src), ".mochi")
		testName := fmt.Sprintf("%03d_%s", idx, name)
		ok := t.Run(testName, func(t *testing.T) {
			if err := runCase(src); err != nil {
				t.Fatal(err)
			}
		})
		if !ok {
			break
		}
	}
}

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

func updateRosetta() {
	root := repoRoot()
	srcDir := filepath.Join(root, "tests", "rosetta", "x", "Mochi")
	binDir := filepath.Join(root, "tests", "rosetta", "transpiler", "Swift")
	docPath := filepath.Join(root, "transpiler", "x", "swift", "ROSETTA.md")
	names, _ := readIndex(filepath.Join(srcDir, "index.txt"))
	total := len(names)
	compiled := 0
	var lines []string
	for i, f := range names {
		name := strings.TrimSuffix(f, ".mochi")
		mark := "[ ]"
		if _, err := os.Stat(filepath.Join(binDir, name+".swift")); err == nil {
			if _, err2 := os.Stat(filepath.Join(binDir, name+".error")); os.IsNotExist(err2) {
				compiled++
				mark = "[x]"
			}
		}
		lines = append(lines, fmt.Sprintf("%d. %s %s", i+1, mark, name))
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
