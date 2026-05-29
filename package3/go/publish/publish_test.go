package publish

import (
	"errors"
	"strings"
	"testing"

	"mochi/package3/go/library"
)

func goodAPI() library.EmitResult {
	return library.EmitResult{
		Files: map[string]string{
			"go.mod":  "module example.com/x\n\ngo 1.21\n",
			"doc.go":  "// Package x is a stub.\npackage x\n",
			"x.go":    "package x\n",
		},
	}
}

func goodRequest() PublishRequest {
	return PublishRequest{
		Files:      goodAPI(),
		ModulePath: "example.com/x",
		Tag:        "v0.1.0",
		RemoteURL:  "file:///tmp/fake.git",
		Author:     Author{Name: "Mochi", Email: "ci@mochi-lang.org"},
	}
}

func TestValidateRejectsEmptyFiles(t *testing.T) {
	req := goodRequest()
	req.Files.Files = nil
	if err := req.Validate(); err == nil || !errors.Is(err, ErrPublish) {
		t.Errorf("expected ErrPublish, got %v", err)
	}
}

func TestValidateRejectsMissingGoMod(t *testing.T) {
	req := goodRequest()
	req.Files = library.EmitResult{Files: map[string]string{"x.go": "package x\n"}}
	if err := req.Validate(); err == nil {
		t.Errorf("expected error for missing go.mod")
	}
}

func TestValidateRejectsBadTag(t *testing.T) {
	req := goodRequest()
	req.Tag = "0.1.0" // missing v
	if err := req.Validate(); err == nil {
		t.Errorf("expected error for non-semver tag")
	}
	req.Tag = "v1.2"
	if err := req.Validate(); err == nil {
		t.Errorf("expected error for incomplete semver tag")
	}
	req.Tag = "latest"
	if err := req.Validate(); err == nil {
		t.Errorf("expected error for non-semver tag")
	}
}

func TestValidateRejectsMissingRemoteURL(t *testing.T) {
	req := goodRequest()
	req.RemoteURL = ""
	if err := req.Validate(); err == nil {
		t.Errorf("expected error for empty remote URL")
	}
}

func TestValidateRejectsMissingAuthor(t *testing.T) {
	req := goodRequest()
	req.Author.Name = ""
	if err := req.Validate(); err == nil {
		t.Errorf("expected error for missing author name")
	}
	req.Author.Name = "x"
	req.Author.Email = ""
	if err := req.Validate(); err == nil {
		t.Errorf("expected error for missing author email")
	}
}

func TestValidateRejectsCanonicalImportPathMismatch(t *testing.T) {
	req := goodRequest()
	req.ModulePath = "example.com/other"
	err := req.Validate()
	if err == nil || !strings.Contains(err.Error(), "go.mod declares module") {
		t.Errorf("expected canonical-import-path mismatch, got %v", err)
	}
}

func TestValidateAccepts(t *testing.T) {
	if err := goodRequest().Validate(); err != nil {
		t.Errorf("unexpected error: %v", err)
	}
}

func TestCanonicalImportPathHappy(t *testing.T) {
	got, err := CanonicalImportPath("module example.com/x\n\ngo 1.21\n")
	if err != nil {
		t.Fatalf("CanonicalImportPath: %v", err)
	}
	if got != "example.com/x" {
		t.Errorf("got %q; want example.com/x", got)
	}
}

func TestCanonicalImportPathSkipsLeadingComments(t *testing.T) {
	body := "// auto-generated\n\nmodule example.com/x\n"
	got, err := CanonicalImportPath(body)
	if err != nil {
		t.Fatalf("CanonicalImportPath: %v", err)
	}
	if got != "example.com/x" {
		t.Errorf("got %q; want example.com/x", got)
	}
}

func TestCanonicalImportPathRejectsMissingDirective(t *testing.T) {
	if _, err := CanonicalImportPath("go 1.21\n"); err == nil {
		t.Errorf("expected error when module directive is missing")
	}
}

func TestCanonicalImportPathRejectsEmpty(t *testing.T) {
	if _, err := CanonicalImportPath(""); err == nil {
		t.Errorf("expected error on empty go.mod")
	}
}

// RecordingRunner is a Runner that records every Run / Output call
// in order. It surfaces a synthetic SHA for `git rev-parse HEAD` so
// the publish driver can be exercised without spawning real git.
type RecordingRunner struct {
	Cmds []string
	// SHA is the value returned for `git rev-parse HEAD`.
	SHA string
	// ErrAt, if set, makes the runner fail the Nth call.
	ErrAt int
	// callCount counts every Run+Output dispatched.
	callCount int
}

func (r *RecordingRunner) record(name string, args ...string) {
	r.Cmds = append(r.Cmds, name+" "+strings.Join(args, " "))
}

func (r *RecordingRunner) failNow() bool {
	r.callCount++
	return r.ErrAt > 0 && r.callCount == r.ErrAt
}

func (r *RecordingRunner) Run(dir, name string, args ...string) error {
	r.record(name, args...)
	if r.failNow() {
		return errors.New("synthetic error")
	}
	return nil
}

func (r *RecordingRunner) Output(dir, name string, args ...string) (string, error) {
	r.record(name, args...)
	if r.failNow() {
		return "", errors.New("synthetic error")
	}
	if name == "git" && len(args) > 0 && args[0] == "rev-parse" {
		if r.SHA == "" {
			return "fa11111111111111111111111111111111111111\n", nil
		}
		return r.SHA + "\n", nil
	}
	return "", nil
}

func TestPublishHappyPath(t *testing.T) {
	req := goodRequest()
	req.WorkspaceRoot = t.TempDir()
	runner := &RecordingRunner{SHA: "deadbeefcafebabe1234567890abcdefdeadbeef"}
	res, err := Publish(req, runner)
	if err != nil {
		t.Fatalf("Publish: %v", err)
	}
	if !res.Pushed {
		t.Errorf("expected Pushed=true")
	}
	if res.CommitSHA != "deadbeefcafebabe1234567890abcdefdeadbeef" {
		t.Errorf("CommitSHA = %q; want the recorder's SHA", res.CommitSHA)
	}
	if res.Tag != "v0.1.0" {
		t.Errorf("Tag = %q; want v0.1.0", res.Tag)
	}
	if res.WorkspaceRoot != req.WorkspaceRoot {
		t.Errorf("WorkspaceRoot = %q; want %q", res.WorkspaceRoot, req.WorkspaceRoot)
	}
	// Verify the expected git invocations happened in order.
	wantSequence := []string{
		"git init --initial-branch=main",
		"git config user.name Mochi",
		"git config user.email ci@mochi-lang.org",
		"git config commit.gpgsign false",
		"git config tag.gpgsign false",
		"git remote add origin file:///tmp/fake.git",
		"git add .",
		"git commit -m publish v0.1.0",
		"git tag -a v0.1.0 -m release v0.1.0",
		"git rev-parse HEAD",
		"git push origin main",
		"git push origin v0.1.0",
	}
	if len(runner.Cmds) != len(wantSequence) {
		t.Fatalf("got %d commands, want %d:\n%v", len(runner.Cmds), len(wantSequence), runner.Cmds)
	}
	for i, want := range wantSequence {
		if runner.Cmds[i] != want {
			t.Errorf("cmd[%d] = %q; want %q", i, runner.Cmds[i], want)
		}
	}
}

func TestPublishDryRunSkipsPush(t *testing.T) {
	req := goodRequest()
	req.WorkspaceRoot = t.TempDir()
	req.DryRun = true
	runner := &RecordingRunner{}
	res, err := Publish(req, runner)
	if err != nil {
		t.Fatalf("Publish: %v", err)
	}
	if res.Pushed {
		t.Errorf("expected Pushed=false under DryRun")
	}
	for _, cmd := range runner.Cmds {
		if strings.HasPrefix(cmd, "git push") {
			t.Errorf("DryRun should not push; saw %q", cmd)
		}
	}
}

func TestPublishUsesCustomBranch(t *testing.T) {
	req := goodRequest()
	req.WorkspaceRoot = t.TempDir()
	req.DefaultBranch = "release"
	runner := &RecordingRunner{}
	if _, err := Publish(req, runner); err != nil {
		t.Fatalf("Publish: %v", err)
	}
	foundInit := false
	foundPush := false
	for _, c := range runner.Cmds {
		if c == "git init --initial-branch=release" {
			foundInit = true
		}
		if c == "git push origin release" {
			foundPush = true
		}
	}
	if !foundInit || !foundPush {
		t.Errorf("custom branch not propagated; cmds=%v", runner.Cmds)
	}
}

func TestPublishUsesCustomCommitMessage(t *testing.T) {
	req := goodRequest()
	req.WorkspaceRoot = t.TempDir()
	req.CommitMessage = "release: v0.1.0 publish"
	runner := &RecordingRunner{}
	if _, err := Publish(req, runner); err != nil {
		t.Fatalf("Publish: %v", err)
	}
	found := false
	for _, c := range runner.Cmds {
		if c == "git commit -m release: v0.1.0 publish" {
			found = true
		}
	}
	if !found {
		t.Errorf("custom commit message not propagated: %v", runner.Cmds)
	}
}

func TestPublishMaterialisesFiles(t *testing.T) {
	req := goodRequest()
	req.WorkspaceRoot = t.TempDir()
	runner := &RecordingRunner{}
	res, err := Publish(req, runner)
	if err != nil {
		t.Fatalf("Publish: %v", err)
	}
	wantFiles := []string{"doc.go", "go.mod", "x.go"}
	if len(res.FilesWritten) != len(wantFiles) {
		t.Fatalf("FilesWritten = %v; want %v", res.FilesWritten, wantFiles)
	}
	for i, w := range wantFiles {
		if res.FilesWritten[i] != w {
			t.Errorf("FilesWritten[%d] = %q; want %q", i, res.FilesWritten[i], w)
		}
	}
}

func TestPublishValidateFailureSkipsAllRunner(t *testing.T) {
	req := goodRequest()
	req.Tag = "invalid"
	runner := &RecordingRunner{}
	if _, err := Publish(req, runner); err == nil {
		t.Errorf("expected validation error")
	}
	if len(runner.Cmds) != 0 {
		t.Errorf("validation failure should not call runner; cmds=%v", runner.Cmds)
	}
}

func TestPublishRunnerErrorSurfaces(t *testing.T) {
	req := goodRequest()
	req.WorkspaceRoot = t.TempDir()
	runner := &RecordingRunner{ErrAt: 1} // fail on `git init`
	if _, err := Publish(req, runner); err == nil {
		t.Errorf("expected error from runner")
	}
}

func TestPublishGeneratesTempWorkspaceWhenEmpty(t *testing.T) {
	req := goodRequest()
	runner := &RecordingRunner{}
	res, err := Publish(req, runner)
	if err != nil {
		t.Fatalf("Publish: %v", err)
	}
	if res.WorkspaceRoot == "" {
		t.Errorf("WorkspaceRoot was empty after auto-temp")
	}
	if !strings.Contains(res.WorkspaceRoot, "mochi-publish-") {
		t.Errorf("temp dir name unexpected: %q", res.WorkspaceRoot)
	}
}
