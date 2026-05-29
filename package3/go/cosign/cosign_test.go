package cosign

import (
	"errors"
	"strings"
	"testing"

	"mochi/package3/go/publish"
)

const sampleSHA = "deadbeefcafebabe1234567890abcdefdeadbeef"

func goodReq() SignRequest {
	return SignRequest{
		CommitSHA:     sampleSHA,
		Tag:           "v0.1.0",
		WorkspaceRoot: "/tmp/x",
		Author:        publish.Author{Name: "Mochi", Email: "ci@mochi-lang.org"},
		OIDCToken:     "fake.oidc.token",
	}
}

// fakeSigner records its inputs and returns a deterministic bundle.
type fakeSigner struct {
	gotCommitSHA, gotTag string
	gotAudience          string
	gotOIDC              string
	gotFulcio, gotRekor  string
	bundle               []byte
	err                  error
}

func (f *fakeSigner) Sign(commitSHA, tag, audience, oidc, fulcio, rekor string) ([]byte, error) {
	f.gotCommitSHA = commitSHA
	f.gotTag = tag
	f.gotAudience = audience
	f.gotOIDC = oidc
	f.gotFulcio = fulcio
	f.gotRekor = rekor
	if f.err != nil {
		return nil, f.err
	}
	if f.bundle != nil {
		return f.bundle, nil
	}
	return []byte("{\"signature\":\"AAA\",\"cert\":\"BBB\"}"), nil
}

// recordingRunner mirrors publish.Runner without the network bits.
type recordingRunner struct {
	Cmds []string
}

func (r *recordingRunner) Run(dir, name string, args ...string) error {
	r.Cmds = append(r.Cmds, name+" "+strings.Join(args, " "))
	return nil
}

func (r *recordingRunner) Output(dir, name string, args ...string) (string, error) {
	r.Cmds = append(r.Cmds, name+" "+strings.Join(args, " "))
	return "", nil
}

func TestValidateAccepts(t *testing.T) {
	if err := goodReq().Validate(); err != nil {
		t.Errorf("unexpected error: %v", err)
	}
}

func TestValidateRejectsShortSHA(t *testing.T) {
	r := goodReq()
	r.CommitSHA = "abc123"
	if err := r.Validate(); err == nil || !errors.Is(err, ErrCosign) {
		t.Errorf("expected ErrCosign for short SHA, got %v", err)
	}
}

func TestValidateRejectsNonHexSHA(t *testing.T) {
	r := goodReq()
	r.CommitSHA = strings.Repeat("z", 40)
	if err := r.Validate(); err == nil {
		t.Errorf("expected error for non-hex SHA")
	}
}

func TestValidateRejectsUppercaseSHA(t *testing.T) {
	r := goodReq()
	r.CommitSHA = strings.ToUpper(sampleSHA)
	if err := r.Validate(); err == nil {
		t.Errorf("expected error for uppercase SHA")
	}
}

func TestValidateRejectsEmptyTag(t *testing.T) {
	r := goodReq()
	r.Tag = ""
	if err := r.Validate(); err == nil {
		t.Errorf("expected error for empty tag")
	}
}

func TestValidateRejectsAlreadySigTag(t *testing.T) {
	r := goodReq()
	r.Tag = "v0.1.0.sig"
	err := r.Validate()
	if err == nil || !strings.Contains(err.Error(), "already ends in .sig") {
		t.Errorf("expected sibling-tag rejection, got %v", err)
	}
}

func TestValidateRejectsEmptyWorkspaceRoot(t *testing.T) {
	r := goodReq()
	r.WorkspaceRoot = ""
	if err := r.Validate(); err == nil {
		t.Errorf("expected error for empty workspace")
	}
}

func TestValidateRejectsEmptyOIDC(t *testing.T) {
	r := goodReq()
	r.OIDCToken = ""
	if err := r.Validate(); err == nil {
		t.Errorf("expected error for empty OIDC token")
	}
}

func TestValidateRejectsEmptyAuthor(t *testing.T) {
	r := goodReq()
	r.Author.Name = ""
	if err := r.Validate(); err == nil {
		t.Errorf("expected error for empty author name")
	}
	r.Author.Name = "x"
	r.Author.Email = ""
	if err := r.Validate(); err == nil {
		t.Errorf("expected error for empty author email")
	}
}

func TestSignHappyPath(t *testing.T) {
	req := goodReq()
	signer := &fakeSigner{}
	runner := &recordingRunner{}
	res, err := Sign(req, signer, runner)
	if err != nil {
		t.Fatalf("Sign: %v", err)
	}
	if res.SiblingTag != "v0.1.0.sig" {
		t.Errorf("SiblingTag = %q; want v0.1.0.sig", res.SiblingTag)
	}
	if !res.Pushed {
		t.Errorf("expected Pushed=true")
	}
	if string(res.Bundle) != "{\"signature\":\"AAA\",\"cert\":\"BBB\"}" {
		t.Errorf("Bundle = %q; want the fake signer's bundle", res.Bundle)
	}

	if signer.gotCommitSHA != sampleSHA {
		t.Errorf("signer received CommitSHA %q; want %q", signer.gotCommitSHA, sampleSHA)
	}
	if signer.gotTag != "v0.1.0" {
		t.Errorf("signer received Tag %q; want v0.1.0", signer.gotTag)
	}
	if signer.gotAudience != "sigstore" {
		t.Errorf("audience = %q; want sigstore", signer.gotAudience)
	}
	if signer.gotFulcio != DefaultFulcioURL {
		t.Errorf("fulcio = %q; want %q", signer.gotFulcio, DefaultFulcioURL)
	}
	if signer.gotRekor != DefaultRekorURL {
		t.Errorf("rekor = %q; want %q", signer.gotRekor, DefaultRekorURL)
	}

	wantCmds := []string{
		"git config user.name Mochi",
		"git config user.email ci@mochi-lang.org",
	}
	for i, want := range wantCmds {
		if runner.Cmds[i] != want {
			t.Errorf("cmd[%d] = %q; want %q", i, runner.Cmds[i], want)
		}
	}
	tagCmd := runner.Cmds[2]
	if !strings.HasPrefix(tagCmd, "git tag -a v0.1.0.sig "+sampleSHA+" -m cosign-sig:") {
		t.Errorf("tag cmd unexpected: %q", tagCmd)
	}
	if runner.Cmds[3] != "git push origin v0.1.0.sig" {
		t.Errorf("push cmd unexpected: %q", runner.Cmds[3])
	}
}

func TestSignDryRunSkipsPush(t *testing.T) {
	req := goodReq()
	req.DryRun = true
	runner := &recordingRunner{}
	res, err := Sign(req, &fakeSigner{}, runner)
	if err != nil {
		t.Fatalf("Sign: %v", err)
	}
	if res.Pushed {
		t.Errorf("Pushed should be false under DryRun")
	}
	for _, c := range runner.Cmds {
		if strings.HasPrefix(c, "git push") {
			t.Errorf("DryRun should not push; saw %q", c)
		}
	}
}

func TestSignSurfacesSignerError(t *testing.T) {
	req := goodReq()
	signer := &fakeSigner{err: errors.New("synthetic signer failure")}
	runner := &recordingRunner{}
	_, err := Sign(req, signer, runner)
	if err == nil || !errors.Is(err, ErrCosign) {
		t.Errorf("expected ErrCosign on signer error, got %v", err)
	}
	if len(runner.Cmds) != 0 {
		t.Errorf("signer failure should not call runner; cmds=%v", runner.Cmds)
	}
}

func TestSignRejectsEmptyBundle(t *testing.T) {
	req := goodReq()
	signer := &fakeSigner{bundle: []byte{}}
	runner := &recordingRunner{}
	_, err := Sign(req, signer, runner)
	if err == nil || !strings.Contains(err.Error(), "empty bundle") {
		t.Errorf("expected empty-bundle rejection, got %v", err)
	}
}

func TestSignValidationFailureSkipsSigner(t *testing.T) {
	req := goodReq()
	req.CommitSHA = "shortbad"
	signer := &fakeSigner{}
	runner := &recordingRunner{}
	_, err := Sign(req, signer, runner)
	if err == nil {
		t.Errorf("expected validation error")
	}
	if signer.gotCommitSHA != "" {
		t.Errorf("validation failure should not call signer")
	}
}

func TestBuildAndParseSiblingMessageRoundtrip(t *testing.T) {
	bundle := []byte("\x00\x01\x02bundle\xff\xfe")
	msg := buildSiblingMessage("v1.2.3", sampleSHA, bundle)
	tag, commit, gotBundle, err := ParseSiblingMessage(msg)
	if err != nil {
		t.Fatalf("ParseSiblingMessage: %v", err)
	}
	if tag != "v1.2.3" {
		t.Errorf("tag = %q; want v1.2.3", tag)
	}
	if commit != sampleSHA {
		t.Errorf("commit = %q; want %q", commit, sampleSHA)
	}
	if string(gotBundle) != string(bundle) {
		t.Errorf("bundle roundtrip drift: %x vs %x", gotBundle, bundle)
	}
}

func TestParseSiblingMessageRejectsUnknownScheme(t *testing.T) {
	msg := "cosign-sig: future-scheme-v99\ntag: v0.1.0\ncommit: " + sampleSHA + "\nbundle-base64: AA==\n"
	if _, _, _, err := ParseSiblingMessage(msg); err == nil {
		t.Errorf("expected unknown-scheme rejection")
	}
}

func TestParseSiblingMessageRejectsMissingFields(t *testing.T) {
	msg := "cosign-sig: mochi-mep74-cosign-v1\ntag: v0.1.0\n"
	if _, _, _, err := ParseSiblingMessage(msg); err == nil {
		t.Errorf("expected missing-fields rejection")
	}
}

func TestParseSiblingMessageRejectsBadBase64(t *testing.T) {
	msg := "cosign-sig: mochi-mep74-cosign-v1\ntag: v0.1.0\ncommit: " + sampleSHA + "\nbundle-base64: ***not-base64***\n"
	if _, _, _, err := ParseSiblingMessage(msg); err == nil {
		t.Errorf("expected base64 decode error")
	}
}

func TestSignUsesCustomAudienceAndEndpoints(t *testing.T) {
	req := goodReq()
	req.Audience = "custom-aud"
	req.FulcioURL = "https://staging.fulcio"
	req.RekorURL = "https://staging.rekor"
	signer := &fakeSigner{}
	runner := &recordingRunner{}
	if _, err := Sign(req, signer, runner); err != nil {
		t.Fatalf("Sign: %v", err)
	}
	if signer.gotAudience != "custom-aud" {
		t.Errorf("audience = %q; want custom-aud", signer.gotAudience)
	}
	if signer.gotFulcio != "https://staging.fulcio" {
		t.Errorf("fulcio = %q; want staging.fulcio", signer.gotFulcio)
	}
	if signer.gotRekor != "https://staging.rekor" {
		t.Errorf("rekor = %q; want staging.rekor", signer.gotRekor)
	}
}

func TestIsFullSHA(t *testing.T) {
	if !isFullSHA(sampleSHA) {
		t.Errorf("sampleSHA should be valid")
	}
	if isFullSHA("") {
		t.Errorf("empty should not be valid")
	}
	if isFullSHA(sampleSHA + "0") {
		t.Errorf("41 chars should not be valid")
	}
	if isFullSHA(sampleSHA[:39]) {
		t.Errorf("39 chars should not be valid")
	}
}
