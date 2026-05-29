package cosign

import (
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"

	"mochi/package3/go/library"
	"mochi/package3/go/publish"
)

// TestPhase13CosignSiblingTagSentinel is the MEP-74 phase 13
// end-to-end sentinel. It exercises the full chain
//
//	library.Emit -> publish.Publish -> cosign.Sign
//
// against a local bare-repo fixture (no network, fake signer) and
// proves:
//
//   - Sign attaches the sibling tag `<tag>.sig` to the publish
//     commit in the publish workspace and pushes it to the same
//     bare remote the publish commit went to;
//   - a downstream `git clone` of the remote sees the sibling tag,
//     and `git cat-file -p <tag>.sig` round-trips the cosign bundle
//     bytes through ParseSiblingMessage;
//   - the sibling tag does not perturb the original publish tag
//     (the publish commit SHA the sibling points at matches the
//     publish-flow SHA).
func TestPhase13CosignSiblingTagSentinel(t *testing.T) {
	if _, err := exec.LookPath("git"); err != nil {
		t.Skipf("git not available: %v", err)
	}

	root := t.TempDir()
	remoteRoot := filepath.Join(root, "remote.git")
	if err := runCmd(t, root, "git", "init", "--bare", remoteRoot); err != nil {
		t.Fatalf("init bare remote: %v", err)
	}

	api := library.PublicAPI{
		ModulePath:  "example.com/mochi/calc",
		PackageName: "calc",
		Version:     "v0.3.0",
		GoVersion:   "1.21",
		Meta: library.PackageMeta{
			DocComment:  "Package calc is a tiny calculator published by Mochi.",
			Description: "Tiny calculator",
			License:     "MIT",
			LicenseText: "MIT License\n\nCopyright (c) 2026 Mochi.\n",
		},
		Items: []library.Item{
			library.ItemConst{Name: "Version", Type: "string", Value: "\"v0.3.0\""},
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

	publishReq := publish.PublishRequest{
		Files:         files,
		ModulePath:    api.ModulePath,
		Tag:           "v0.3.0",
		RemoteURL:     "file://" + remoteRoot,
		Author:        publish.Author{Name: "Mochi CI", Email: "ci@mochi-lang.org"},
		WorkspaceRoot: filepath.Join(root, "ws"),
	}
	pubRes, err := publish.Publish(publishReq, publish.NewExecRunner())
	if err != nil {
		t.Fatalf("publish.Publish: %v", err)
	}
	if !pubRes.Pushed {
		t.Fatalf("expected publish Pushed=true")
	}
	if len(pubRes.CommitSHA) != 40 {
		t.Fatalf("expected 40-char publish commit SHA, got %q", pubRes.CommitSHA)
	}

	bundle := []byte("{\"signature\":\"AAA\",\"cert\":\"BBB\",\"rekorLogIndex\":42}")
	signer := &fakeSigner{bundle: bundle}
	signReq := SignRequest{
		CommitSHA:     pubRes.CommitSHA,
		Tag:           "v0.3.0",
		WorkspaceRoot: pubRes.WorkspaceRoot,
		Author:        publish.Author{Name: "Mochi CI", Email: "ci@mochi-lang.org"},
		OIDCToken:     "fake.oidc.token",
	}
	signRes, err := Sign(signReq, signer, publish.NewExecRunner())
	if err != nil {
		t.Fatalf("cosign.Sign: %v", err)
	}
	if !signRes.Pushed {
		t.Fatalf("expected cosign Pushed=true")
	}
	if signRes.SiblingTag != "v0.3.0.sig" {
		t.Errorf("SiblingTag = %q; want v0.3.0.sig", signRes.SiblingTag)
	}

	// The remote bare repo should now carry both the publish tag
	// and the sibling tag.
	out, err := combinedOutput(remoteRoot, "git", "tag", "-l")
	if err != nil {
		t.Fatalf("list remote tags: %v\n%s", err, out)
	}
	if !strings.Contains(out, "v0.3.0\n") && !strings.HasSuffix(strings.TrimSpace(out), "v0.3.0") && !strings.Contains(out, "v0.3.0 ") {
		t.Errorf("remote missing v0.3.0:\n%s", out)
	}
	if !strings.Contains(out, "v0.3.0.sig") {
		t.Errorf("remote missing v0.3.0.sig:\n%s", out)
	}

	// Clone the bare repo, then read the sibling-tag message back
	// through `git cat-file -p` and verify the bundle round-trips.
	clonePath := filepath.Join(root, "consumer-clone")
	if out, err := combinedOutput(root, "git", "clone", "file://"+remoteRoot, clonePath); err != nil {
		t.Fatalf("clone remote: %v\n%s", err, out)
	}

	// Resolve the sibling-tag object to its annotated commit SHA and
	// confirm it matches the publish commit (sibling tag must point
	// to the publish commit, not to a separate commit).
	pointedSHA, err := combinedOutput(clonePath, "git", "rev-list", "-n", "1", "v0.3.0.sig")
	if err != nil {
		t.Fatalf("git rev-list v0.3.0.sig: %v\n%s", err, pointedSHA)
	}
	if got := strings.TrimSpace(pointedSHA); got != pubRes.CommitSHA {
		t.Errorf("sibling tag points to %q; want publish commit %q", got, pubRes.CommitSHA)
	}

	// `git cat-file -p <annotated-tag>` outputs the tag header lines
	// followed by a blank line and then the annotated message body.
	tagBlob, err := combinedOutput(clonePath, "git", "cat-file", "-p", "v0.3.0.sig")
	if err != nil {
		t.Fatalf("git cat-file -p v0.3.0.sig: %v\n%s", err, tagBlob)
	}
	body := tagBlobBody(tagBlob)
	if body == "" {
		t.Fatalf("annotated tag body is empty; full blob:\n%s", tagBlob)
	}

	gotTag, gotCommit, gotBundle, err := ParseSiblingMessage(body)
	if err != nil {
		t.Fatalf("ParseSiblingMessage: %v\nbody=%q", err, body)
	}
	if gotTag != "v0.3.0" {
		t.Errorf("parsed tag = %q; want v0.3.0", gotTag)
	}
	if gotCommit != pubRes.CommitSHA {
		t.Errorf("parsed commit = %q; want %q", gotCommit, pubRes.CommitSHA)
	}
	if string(gotBundle) != string(bundle) {
		t.Errorf("bundle did not round-trip through annotated tag:\ngot:  %q\nwant: %q", gotBundle, bundle)
	}

	// And the fake signer must have seen the publish commit SHA,
	// the publish tag, and the default Sigstore endpoints.
	if signer.gotCommitSHA != pubRes.CommitSHA {
		t.Errorf("signer received commit %q; want %q", signer.gotCommitSHA, pubRes.CommitSHA)
	}
	if signer.gotTag != "v0.3.0" {
		t.Errorf("signer received tag %q; want v0.3.0", signer.gotTag)
	}
	if signer.gotFulcio != DefaultFulcioURL {
		t.Errorf("signer received fulcio %q; want %q", signer.gotFulcio, DefaultFulcioURL)
	}
	if signer.gotRekor != DefaultRekorURL {
		t.Errorf("signer received rekor %q; want %q", signer.gotRekor, DefaultRekorURL)
	}
	if signer.gotAudience != DefaultAudience {
		t.Errorf("signer received audience %q; want %q", signer.gotAudience, DefaultAudience)
	}
}

// tagBlobBody strips the `object/type/tag/tagger` header from the
// output of `git cat-file -p <annotated-tag>` and returns the
// annotated message body. The header is separated from the body by
// a blank line per the git object format.
func tagBlobBody(blob string) string {
	idx := strings.Index(blob, "\n\n")
	if idx < 0 {
		return ""
	}
	return blob[idx+2:]
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
	// Scrub GIT_* envs so a parent test harness cannot leak its
	// index/working-tree into a child git invocation that targets
	// a different repo.
	clean := []string{}
	for _, e := range os.Environ() {
		if strings.HasPrefix(e, "GIT_DIR=") ||
			strings.HasPrefix(e, "GIT_WORK_TREE=") ||
			strings.HasPrefix(e, "GIT_INDEX_FILE=") ||
			strings.HasPrefix(e, "GIT_OBJECT_DIRECTORY=") {
			continue
		}
		clean = append(clean, e)
	}
	cmd.Env = clean
	out, err := cmd.CombinedOutput()
	return string(out), err
}
