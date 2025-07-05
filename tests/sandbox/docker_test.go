package sandbox_test

import (
	"encoding/json"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"

	"mochi/tools/sandbox"
	"mochi/tools/sandbox/runner"
)

func TestDockerfiles(t *testing.T) {
	if _, err := exec.LookPath("docker"); err != nil {
		t.Skip("docker not installed")
	}

	root := findRepoRoot(t)
	dfDir := filepath.Join(root, "tools", "sandbox", "dockerfiles")
	exDir := filepath.Join(root, "tools", "sandbox", "examples")

	files, err := filepath.Glob(filepath.Join(dfDir, "*.Dockerfile"))
	if err != nil {
		t.Fatalf("list dockerfiles: %v", err)
	}

	for _, df := range files {
		lang := strings.TrimSuffix(filepath.Base(df), ".Dockerfile")
		ext := sandbox.Extensions[lang]
		src := filepath.Join(exDir, "hello."+ext)
		if _, err := os.Stat(src); err != nil {
			t.Fatalf("missing hello example for %s", lang)
		}
		tag := "mochi_sandbox_" + lang
		cmd := exec.Command("docker", "build", "-f", df, "-t", tag, root)
		if out, err := cmd.CombinedOutput(); err != nil {
			t.Fatalf("build %s: %v\n%s", lang, err, out)
		}
		data, err := os.ReadFile(filepath.Join(exDir, "hello."+ext))
		if err != nil {
			t.Fatalf("read example: %v", err)
		}
		req := runner.Request{Files: map[string]string{"hello." + ext: string(data)}}
		payload, _ := json.Marshal(req)
		runArgs := []string{"run", "--rm", "-i", tag}
		cmd = exec.Command("docker", runArgs...)
		cmd.Stdin = strings.NewReader(string(payload))
		out, err := cmd.CombinedOutput()
		if err != nil {
			t.Fatalf("run %s: %v\n%s", lang, err, out)
		}
		var resp runner.Response
		if err := json.Unmarshal(out, &resp); err != nil {
			t.Fatalf("parse response %s: %v\n%s", lang, err, out)
		}
		got := strings.TrimSpace(resp.Output)
		if got != "hello" {
			t.Fatalf("%s output: %q", lang, got)
		}
	}
}

func findRepoRoot(t *testing.T) string {
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
