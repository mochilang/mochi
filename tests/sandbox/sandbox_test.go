//go:build slow

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
	exDir := filepath.Join(root, "tools", "sandbox", "examples")

	var missing []string
	var failed []string

	for lang, cmdArgs := range sandbox.RunCmds {
		lang := lang
		cmdArgs := cmdArgs
		t.Run(lang, func(t *testing.T) {
			prog := firstProg(cmdArgs)
			if prog != "" {
				if _, err := exec.LookPath(prog); err != nil {
					missing = append(missing, fmt.Sprintf("%s (%s)", lang, prog))
					return
				}
			}

			tmp := t.TempDir()
			ext := sandbox.Extensions[lang]
			srcPath := filepath.Join(exDir, "hello."+ext)
			data, err := os.ReadFile(srcPath)
			if err != nil {
				t.Fatalf("read example: %v", err)
			}
			pkg := filepath.Join(root, "tools", "sandbox", "runner", lang)
			bin := filepath.Join(tmp, "runner")
			build := exec.Command("go", "build", "-o", bin, pkg)
			build.Dir = root
			if out, err := build.CombinedOutput(); err != nil {
				t.Fatalf("build runner: %v\n%s", err, out)
			}
			cmd := exec.Command(bin)
			cmd.Dir = tmp
			req := runner.Request{Files: map[string]string{"hello." + ext: string(data)}}
			payload, _ := json.Marshal(req)
			cmd.Stdin = bytes.NewReader(payload)
			out, err := cmd.CombinedOutput()
			if err != nil {
				failed = append(failed, fmt.Sprintf("%s: %v\n%s", lang, err, out))
				return
			}
			var resp runner.Response
			if err := json.Unmarshal(out, &resp); err != nil {
				t.Fatalf("unmarshal: %v\n%s", err, out)
			}
			if !strings.Contains(resp.Output, "hello") {
				failed = append(failed, fmt.Sprintf("%s: output %q", lang, resp.Output))
				return
			}
			if resp.Error != "" {
				failed = append(failed, fmt.Sprintf("%s: error %s", lang, resp.Error))
				return
			}
		})
	}

	if err := writeErrorsFile(root, missing, failed); err != nil {
		t.Fatalf("write errors file: %v", err)
	}
	if len(missing) > 0 || len(failed) > 0 {
		t.Logf("missing compilers: %v\nfailed languages: %v", strings.Join(missing, ", "), strings.Join(failed, "; "))
	}
}

func firstProg(args []string) string {
	if len(args) == 0 {
		return ""
	}
	if args[0] == "bash" && len(args) >= 3 && args[1] == "-c" {
		fields := strings.Fields(args[2])
		if len(fields) > 0 {
			return fields[0]
		}
		return ""
	}
	return args[0]
}

func keys(m map[string]bool) []string {
	s := make([]string, 0, len(m))
	for k := range m {
		s = append(s, k)
	}
	sort.Strings(s)
	return s
}

func writeErrorsFile(root string, missing, failed []string) error {
	path := filepath.Join(root, "tools", "sandbox", "ERRORS.md")
	var b strings.Builder
	if len(missing) == 0 && len(failed) == 0 {
		b.WriteString("All languages passed\n")
	} else {
		if len(missing) > 0 {
			sort.Strings(missing)
			b.WriteString("## Missing compilers\n\n")
			for _, m := range missing {
				b.WriteString("- " + m + "\n")
			}
		}
		if len(failed) > 0 {
			sort.Strings(failed)
			if len(missing) > 0 {
				b.WriteString("\n")
			}
			b.WriteString("## Failed languages\n\n")
			for _, f := range failed {
				b.WriteString("- " + f + "\n")
			}
		}
	}
	return os.WriteFile(path, []byte(b.String()), 0644)
}
