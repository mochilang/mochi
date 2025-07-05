package sandbox_test

import (
	"bytes"
	"encoding/json"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"reflect"
	"sort"
	"strings"
	"testing"

	"mochi/tools/sandbox"
	"mochi/tools/sandbox/runner"
)

func TestListLanguages(t *testing.T) {
	root := findRepoRoot(t)
	want := map[string]bool{}
	entries, err := os.ReadDir(filepath.Join(root, "compile"))
	if err != nil {
		t.Fatalf("read compile dir: %v", err)
	}
	for _, e := range entries {
		if !e.IsDir() || e.Name() == "x" {
			continue
		}
		want[e.Name()] = true
	}
	entries, err = os.ReadDir(filepath.Join(root, "compile", "x"))
	if err == nil {
		for _, e := range entries {
			if !e.IsDir() || e.Name() == "testutil" {
				continue
			}
			want[e.Name()] = true
		}
	}

	cwd, _ := os.Getwd()
	if err := os.Chdir(root); err != nil {
		t.Fatal(err)
	}
	defer os.Chdir(cwd)

	langs, err := sandbox.ListLanguages()
	if err != nil {
		t.Fatalf("ListLanguages: %v", err)
	}
	got := map[string]bool{}
	for _, l := range langs {
		got[l] = true
	}
	if !reflect.DeepEqual(got, want) {
		t.Fatalf("languages mismatch\n got: %v\nwant: %v", keys(got), keys(want))
	}
}

func TestDockerfileFor(t *testing.T) {
	df := sandbox.DockerfileFor("py")
	img := sandbox.Images["py"]
	if !strings.Contains(df, fmt.Sprintf("FROM %s", img)) {
		t.Fatalf("dockerfile missing base image: %s", df)
	}
	if !strings.Contains(df, "./tools/sandbox/runner/py") {
		t.Fatalf("dockerfile missing runner path: %s", df)
	}
	if !strings.Contains(df, "CMD [\"/usr/local/bin/runner\"]") {
		t.Fatalf("dockerfile missing CMD: %s", df)
	}
}

func TestRunnerProcess(t *testing.T) {
	root := findRepoRoot(t)
	tmp := t.TempDir()
	cmd := exec.Command("go", "run", "-C", root, "./tools/sandbox/runner/py")
	cmd.Dir = tmp
	req := runner.Request{Files: map[string]string{"hello.py": "print(\"hello\")"}}
	payload, _ := json.Marshal(req)
	cmd.Stdin = bytes.NewReader(payload)
	out, err := cmd.CombinedOutput()
	if err != nil {
		t.Fatalf("runner process: %v\n%s", err, out)
	}
	var resp runner.Response
	if err := json.Unmarshal(out, &resp); err != nil {
		t.Fatalf("unmarshal: %v\n%s", err, out)
	}
	if strings.TrimSpace(resp.Output) != "hello" {
		t.Fatalf("unexpected output: %q", resp.Output)
	}
	if resp.Error != "" {
		t.Fatalf("unexpected error: %s", resp.Error)
	}
}

func keys(m map[string]bool) []string {
	s := make([]string, 0, len(m))
	for k := range m {
		s = append(s, k)
	}
	sort.Strings(s)
	return s
}
