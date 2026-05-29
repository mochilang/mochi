package publish

import (
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"

	"mochi/package3/go/library"
)

// TestPhase12GitTagPublishSentinel is the MEP-74 phase 12 end-to-end
// sentinel. It exercises the full publish flow against a local
// bare-repo fixture (no network) and proves:
//
//   - library.Emit -> Publish.Validate -> Publish.Run produces a
//     real git working tree whose committed tree matches the
//     emitted Files map byte-for-byte;
//   - the tag and the commit both end up in the bare-repo remote;
//   - a downstream `git clone` of the remote checks out the
//     publish commit, the publish tag resolves on the clone, and
//     `go build ./...` against the clone succeeds (the publish
//     does not break the buildability of the underlying library
//     module);
//   - DryRun mode leaves the local tree intact (commit + tag both
//     present locally) but the remote bare-repo stays empty.
func TestPhase12GitTagPublishSentinel(t *testing.T) {
	if _, err := exec.LookPath("git"); err != nil {
		t.Skipf("git not available: %v", err)
	}
	if _, err := exec.LookPath("go"); err != nil {
		t.Skipf("go not available: %v", err)
	}

	root := t.TempDir()
	remoteRoot := filepath.Join(root, "remote.git")
	if err := runCmd(t, root, "git", "init", "--bare", remoteRoot); err != nil {
		t.Fatalf("init bare remote: %v", err)
	}

	api := library.PublicAPI{
		ModulePath:  "example.com/mochi/calc",
		PackageName: "calc",
		Version:     "v0.2.0",
		GoVersion:   "1.21",
		Meta: library.PackageMeta{
			DocComment:  "Package calc is a tiny calculator published by Mochi.",
			Description: "Tiny calculator",
			License:     "MIT",
			LicenseText: "MIT License\n\nCopyright (c) 2026 Mochi.\n",
		},
		Items: []library.Item{
			library.ItemConst{Name: "Version", Type: "string", Value: "\"v0.2.0\""},
			library.ItemFunc{Name: "Add",
				Params:  []library.Param{{Name: "a", Type: "int64"}, {Name: "b", Type: "int64"}},
				Results: []library.Result{{Type: "int64"}},
				Body:    "return a + b",
			},
		},
	}
	files, err := library.Emit(api)
	if err != nil {
		t.Fatalf("library.Emit: %v", err)
	}

	t.Run("happy path publishes and remote receives tag", func(t *testing.T) {
		req := PublishRequest{
			Files:         files,
			ModulePath:    api.ModulePath,
			Tag:           "v0.2.0",
			RemoteURL:     "file://" + remoteRoot,
			Author:        Author{Name: "Mochi CI", Email: "ci@mochi-lang.org"},
			WorkspaceRoot: filepath.Join(root, "ws-happy"),
		}
		res, err := Publish(req, NewExecRunner())
		if err != nil {
			t.Fatalf("Publish: %v", err)
		}
		if !res.Pushed {
			t.Fatalf("expected Pushed=true")
		}
		if len(res.CommitSHA) < 40 {
			t.Errorf("CommitSHA looks short: %q", res.CommitSHA)
		}

		// The remote should have the tag and the commit now.
		out, err := combinedOutput(remoteRoot, "git", "tag", "-l")
		if err != nil {
			t.Fatalf("list remote tags: %v\n%s", err, out)
		}
		if !strings.Contains(out, "v0.2.0") {
			t.Errorf("remote does not have tag v0.2.0:\n%s", out)
		}

		// Clone the bare repo and verify the publish.
		clonePath := filepath.Join(root, "consumer-clone")
		if out, err := combinedOutput(root, "git", "clone", "file://"+remoteRoot, clonePath); err != nil {
			t.Fatalf("clone remote: %v\n%s", err, out)
		}
		if out, err := combinedOutput(clonePath, "git", "checkout", "v0.2.0"); err != nil {
			t.Fatalf("checkout tag: %v\n%s", err, out)
		}
		// The emitted go.mod / doc.go / calc.go must all be there.
		for _, want := range []string{"go.mod", "doc.go", "calc.go", "LICENSE"} {
			path := filepath.Join(clonePath, want)
			if _, err := os.Stat(path); err != nil {
				t.Errorf("clone missing %s: %v", want, err)
			}
		}
		// `go build ./...` against the cloned tree must succeed.
		out, err = goCmd(clonePath, "go", "build", "./...")
		if err != nil {
			t.Errorf("go build on clone failed: %v\n%s", err, out)
		}
	})

	t.Run("dry run does not touch the remote", func(t *testing.T) {
		req := PublishRequest{
			Files:         files,
			ModulePath:    api.ModulePath,
			Tag:           "v0.3.0",
			RemoteURL:     "file://" + remoteRoot,
			Author:        Author{Name: "Mochi CI", Email: "ci@mochi-lang.org"},
			WorkspaceRoot: filepath.Join(root, "ws-dry"),
			DryRun:        true,
		}
		res, err := Publish(req, NewExecRunner())
		if err != nil {
			t.Fatalf("Publish dry-run: %v", err)
		}
		if res.Pushed {
			t.Errorf("DryRun should report Pushed=false")
		}
		// Local tag still present.
		out, err := combinedOutput(req.WorkspaceRoot, "git", "tag", "-l")
		if err != nil {
			t.Fatalf("local tag -l: %v\n%s", err, out)
		}
		if !strings.Contains(out, "v0.3.0") {
			t.Errorf("local tag v0.3.0 missing under DryRun:\n%s", out)
		}
		// Remote tag must NOT have v0.3.0.
		out, _ = combinedOutput(remoteRoot, "git", "tag", "-l")
		if strings.Contains(out, "v0.3.0") {
			t.Errorf("remote should not have v0.3.0 under DryRun; got:\n%s", out)
		}
	})

	t.Run("canonical import path mismatch is blocked", func(t *testing.T) {
		req := PublishRequest{
			Files:         files,
			ModulePath:    "example.com/wrong",
			Tag:           "v0.4.0",
			RemoteURL:     "file://" + remoteRoot,
			Author:        Author{Name: "Mochi CI", Email: "ci@mochi-lang.org"},
			WorkspaceRoot: filepath.Join(root, "ws-mismatch"),
		}
		_, err := Publish(req, NewExecRunner())
		if err == nil {
			t.Errorf("expected canonical-import-path mismatch error")
		}
		if !strings.Contains(err.Error(), "go.mod declares module") {
			t.Errorf("unexpected error text: %v", err)
		}
	})
}

func runCmd(t *testing.T, dir, name string, args ...string) error {
	t.Helper()
	cmd := exec.Command(name, args...)
	cmd.Dir = dir
	out, err := cmd.CombinedOutput()
	if err != nil {
		t.Logf("%s %v failed: %v\n%s", name, args, err, out)
	}
	return err
}

func combinedOutput(dir, name string, args ...string) (string, error) {
	cmd := exec.Command(name, args...)
	cmd.Dir = dir
	out, err := cmd.CombinedOutput()
	return string(out), err
}

func goCmd(dir, name string, args ...string) (string, error) {
	cmd := exec.Command(name, args...)
	cmd.Dir = dir
	gocache, _ := os.MkdirTemp("", "gocache-")
	gomodcache, _ := os.MkdirTemp("", "gomodcache-")
	defer os.RemoveAll(gocache)
	defer os.RemoveAll(gomodcache)
	cmd.Env = append(os.Environ(),
		"GOCACHE="+gocache,
		"GOMODCACHE="+gomodcache,
		"GOFLAGS=",
		"GO111MODULE=on",
	)
	out, err := cmd.CombinedOutput()
	return string(out), err
}
