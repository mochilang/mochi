package publish

import (
	"errors"
	"strings"
	"testing"

	"mochi/package3/rust/library"
)

type fakeTransport struct {
	calls   int
	lastURL string
	lastHdr map[string]string
	lastBod []byte
	retCode int
	retErr  error
}

func (f *fakeTransport) Do(url string, headers map[string]string, body []byte) (int, []byte, error) {
	f.calls++
	f.lastURL = url
	f.lastHdr = headers
	f.lastBod = body
	if f.retCode == 0 {
		f.retCode = 200
	}
	return f.retCode, []byte("ok"), f.retErr
}

func validRequest() PublishRequest {
	files := library.Files{
		"Cargo.toml": "[package]\nname = \"demo\"\nversion = \"1.0.0\"\nedition = \"2021\"\n[lib]\ncrate-type = [\"rlib\", \"cdylib\"]\n",
		"src/lib.rs": "pub fn x() {}\n",
	}
	return PublishRequest{
		CrateName: "demo",
		Version:   "1.0.0",
		Files:     files,
		OIDCToken: "header.payload.sig",
	}
}

func TestPublishRequestValidate(t *testing.T) {
	if err := validRequest().Validate(); err != nil {
		t.Errorf("valid request rejected: %v", err)
	}
}

func TestPublishRequestRejectsEmptyFields(t *testing.T) {
	cases := []struct {
		name string
		mut  func(r *PublishRequest)
	}{
		{"empty crate", func(r *PublishRequest) { r.CrateName = "" }},
		{"empty version", func(r *PublishRequest) { r.Version = "" }},
		{"empty oidc", func(r *PublishRequest) { r.OIDCToken = "" }},
		{"missing manifest", func(r *PublishRequest) { delete(r.Files, "Cargo.toml") }},
		{"missing lib.rs", func(r *PublishRequest) { delete(r.Files, "src/lib.rs") }},
	}
	for _, c := range cases {
		t.Run(c.name, func(t *testing.T) {
			r := validRequest()
			c.mut(&r)
			if err := r.Validate(); err == nil {
				t.Errorf("expected error for %s", c.name)
			}
		})
	}
}

func TestPublishRequestDetectsNameMismatch(t *testing.T) {
	r := validRequest()
	r.CrateName = "other"
	err := r.Validate()
	if err == nil {
		t.Fatal("expected name mismatch error")
	}
	if !strings.Contains(err.Error(), "name") {
		t.Errorf("error should mention name: %v", err)
	}
}

func TestPublishRequestDetectsVersionMismatch(t *testing.T) {
	r := validRequest()
	r.Version = "9.9.9"
	if err := r.Validate(); err == nil {
		t.Fatal("expected version mismatch error")
	}
}

func TestPublishDryRunSkipsTransport(t *testing.T) {
	r := validRequest()
	r.DryRun = true
	tr := &fakeTransport{}
	res, err := Publish(r, tr)
	if err != nil {
		t.Fatal(err)
	}
	if tr.calls != 0 {
		t.Errorf("dry-run should not call transport, got %d calls", tr.calls)
	}
	if len(res.CrateBytes) == 0 {
		t.Error("expected crate bytes in result")
	}
	if len(res.SignedBundle) == 0 {
		t.Error("expected sigstore bundle in result")
	}
	if res.StatusCode != 0 {
		t.Errorf("dry-run StatusCode should be 0, got %d", res.StatusCode)
	}
}

func TestPublishSendsBearerToken(t *testing.T) {
	r := validRequest()
	tr := &fakeTransport{}
	if _, err := Publish(r, tr); err != nil {
		t.Fatal(err)
	}
	if got := tr.lastHdr["Authorization"]; got != "Bearer "+r.OIDCToken {
		t.Errorf("Authorization header = %q, want Bearer prefix", got)
	}
}

func TestPublishSendsSigstoreBundleHeader(t *testing.T) {
	r := validRequest()
	tr := &fakeTransport{}
	if _, err := Publish(r, tr); err != nil {
		t.Fatal(err)
	}
	if tr.lastHdr["X-Mochi-Sigstore-Bundle"] == "" {
		t.Error("missing X-Mochi-Sigstore-Bundle header")
	}
	if tr.lastHdr["X-Mochi-Publish-Audience"] != Audience {
		t.Errorf("audience header = %q want %q", tr.lastHdr["X-Mochi-Publish-Audience"], Audience)
	}
}

func TestPublishTargetsCorrectURL(t *testing.T) {
	r := validRequest()
	tr := &fakeTransport{}
	if _, err := Publish(r, tr); err != nil {
		t.Fatal(err)
	}
	if !strings.HasSuffix(tr.lastURL, "/api/v1/crates/new") {
		t.Errorf("URL should target /api/v1/crates/new: %q", tr.lastURL)
	}
	if !strings.HasPrefix(tr.lastURL, DefaultRegistryURL) {
		t.Errorf("URL should default to %q: %q", DefaultRegistryURL, tr.lastURL)
	}
}

func TestPublishHonorsCustomRegistryURL(t *testing.T) {
	r := validRequest()
	r.RegistryURL = "https://staging.example.com/"
	tr := &fakeTransport{}
	if _, err := Publish(r, tr); err != nil {
		t.Fatal(err)
	}
	if tr.lastURL != "https://staging.example.com/api/v1/crates/new" {
		t.Errorf("got URL %q", tr.lastURL)
	}
}

func TestPublishFailsOnNon2xx(t *testing.T) {
	r := validRequest()
	tr := &fakeTransport{retCode: 422}
	if _, err := Publish(r, tr); err == nil {
		t.Error("expected error for non-2xx")
	}
}

func TestPublishFailsOnTransportError(t *testing.T) {
	r := validRequest()
	tr := &fakeTransport{retErr: errors.New("network down")}
	if _, err := Publish(r, tr); err == nil {
		t.Error("expected error for transport failure")
	}
}

func TestPublishFailsOnNilTransport(t *testing.T) {
	r := validRequest()
	if _, err := Publish(r, nil); err == nil {
		t.Error("expected error for nil transport on non-dry-run")
	}
}

func TestPublishUploadBodyFormat(t *testing.T) {
	r := validRequest()
	tr := &fakeTransport{}
	if _, err := Publish(r, tr); err != nil {
		t.Fatal(err)
	}
	body := tr.lastBod
	if len(body) < 8 {
		t.Fatalf("body too short: %d bytes", len(body))
	}
	// first u32: metadata length = 2 ("{}")
	if body[0] != 2 || body[1] != 0 || body[2] != 0 || body[3] != 0 {
		t.Errorf("bad metadata length prefix: %v", body[:4])
	}
	if string(body[4:6]) != "{}" {
		t.Errorf("bad metadata: %q", body[4:6])
	}
}

func TestPublishErrorString(t *testing.T) {
	e := PublishError{Reason: "test reason"}
	if e.Error() != "publish: test reason" {
		t.Errorf("got %q", e.Error())
	}
}

func TestRegistryURLDefault(t *testing.T) {
	r := PublishRequest{}
	if r.registryURL() != DefaultRegistryURL {
		t.Errorf("expected default URL, got %q", r.registryURL())
	}
	r.RegistryURL = "https://x/"
	if r.registryURL() != "https://x" {
		t.Errorf("expected trailing slash trimmed: got %q", r.registryURL())
	}
}
