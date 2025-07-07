//go:build archived && slow

package cljcode_test

import (
	"bytes"
	"encoding/json"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"reflect"
	"testing"

	cljcode "mochi/archived/x/clj"
	"mochi/archived/x/testutil"
	"mochi/parser"
	"mochi/types"
)

func TestClojureCompiler_JOB(t *testing.T) {
	if err := cljcode.EnsureClojure(); err != nil {
		t.Skipf("clojure not installed: %v", err)
	}
	root := testutil.FindRepoRoot(t)
	for i := 1; i <= 10; i++ {
		name := fmt.Sprintf("q%d", i)
		src := filepath.Join(root, "tests", "dataset", "job", name+".mochi")
		prog, err := parser.Parse(src)
		if err != nil {
			t.Fatalf("parse %s: %v", name, err)
		}
		env := types.NewEnv(nil)
		if errs := types.Check(prog, env); len(errs) > 0 {
			t.Fatalf("type %s: %v", name, errs[0])
		}
		code, err := cljcode.New(env).Compile(prog)
		if err != nil {
			t.Fatalf("compile %s: %v", name, err)
		}
		wantCodePath := filepath.Join(root, "tests", "dataset", "job", "compiler", "clj", name+".clj.out")
		wantCode, err := os.ReadFile(wantCodePath)
		if err != nil {
			t.Fatalf("read golden %s: %v", name, err)
		}
		if got := bytes.TrimSpace(code); !bytes.Equal(got, bytes.TrimSpace(wantCode)) {
			t.Errorf("%s code mismatch\n\n--- Got ---\n%s\n\n--- Want ---\n%s", name, got, bytes.TrimSpace(wantCode))
		}
		dir := t.TempDir()
		file := filepath.Join(dir, "main.clj")
		if err := os.WriteFile(file, code, 0644); err != nil {
			t.Fatalf("write %s: %v", name, err)
		}
		cmd := exec.Command("clojure", file)
		cmd.Env = append(os.Environ(), "CLASSPATH=/usr/share/java/data.json.jar:/usr/share/java/snakeyaml-engine.jar")
		out, err := cmd.CombinedOutput()
		if err != nil {
			t.Logf("%s failed: %v", name, err)
			continue
		}
		lines := bytes.Split(bytes.TrimSpace(out), []byte("\n"))
		gotLines := [][]byte{}
		for _, ln := range lines {
			if bytes.HasPrefix(ln, []byte("WARNING")) {
				continue
			}
			if len(ln) > 0 {
				gotLines = append(gotLines, ln)
			}
		}
		if len(gotLines) == 0 {
			t.Fatalf("no output for %s", name)
		}
		gotJSON := gotLines[0]
		wantOutPath := filepath.Join(root, "tests", "dataset", "job", "compiler", "clj", name+".out")
		wantOut, err := os.ReadFile(wantOutPath)
		if err != nil {
			t.Fatalf("read golden %s: %v", name, err)
		}
		wantJSON := bytes.Split(bytes.TrimSpace(wantOut), []byte("\n"))[0]
		var gv, wv any
		if err := json.Unmarshal(gotJSON, &gv); err != nil {
			t.Fatalf("parse got json %s: %v", name, err)
		}
		if err := json.Unmarshal(wantJSON, &wv); err != nil {
			t.Fatalf("parse want json %s: %v", name, err)
		}
		if !reflect.DeepEqual(gv, wv) {
			t.Errorf("%s output mismatch\n\n--- Got ---\n%s\n\n--- Want ---\n%s", name, gotJSON, wantJSON)
		}
	}
}
