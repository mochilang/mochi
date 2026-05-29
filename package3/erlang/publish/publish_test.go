package publish

import (
	"encoding/json"
	"fmt"
	"strings"
	"testing"
	"time"
)

// ── OIDC helpers ──────────────────────────────────────────────────────────

func validClaims(exp int64) OIDCClaims {
	return OIDCClaims{
		Issuer:   "https://token.actions.githubusercontent.com",
		Subject:  "repo:mochilang/mochi:ref:refs/heads/main",
		Audience: HexAudience,
		Expiry:   exp,
		IssuedAt: exp - 300,
	}
}

func futureJWT() string {
	return EncodeUnverifiedJWT(validClaims(time.Now().Add(1 * time.Hour).Unix()))
}

func expiredJWT() string {
	return EncodeUnverifiedJWT(validClaims(time.Now().Add(-1 * time.Hour).Unix()))
}

// ── ParseOIDCToken ────────────────────────────────────────────────────────

func TestParseOIDCToken_ValidJWT(t *testing.T) {
	jwt := futureJWT()
	claims, err := ParseOIDCToken(jwt)
	if err != nil {
		t.Fatalf("ParseOIDCToken: %v", err)
	}
	if claims.Audience != HexAudience {
		t.Errorf("Audience = %q, want %q", claims.Audience, HexAudience)
	}
	if claims.Issuer == "" {
		t.Error("Issuer should not be empty")
	}
}

func TestParseOIDCToken_NotJWT(t *testing.T) {
	_, err := ParseOIDCToken("not.a.valid.jwt.with.too.many.parts")
	if err == nil {
		t.Error("expected error for malformed JWT")
	}
}

func TestParseOIDCToken_TwoPartJWT(t *testing.T) {
	_, err := ParseOIDCToken("header.payload")
	if err == nil {
		t.Error("expected error for 2-part JWT")
	}
}

func TestParseOIDCToken_BadBase64(t *testing.T) {
	_, err := ParseOIDCToken("header.!!!notbase64!!!.sig")
	if err == nil {
		t.Error("expected error for bad base64 payload")
	}
}

func TestParseOIDCToken_RoundTrip(t *testing.T) {
	orig := validClaims(9999999999)
	orig.Repository = "mochilang/mochi"
	jwt := EncodeUnverifiedJWT(orig)
	claims, err := ParseOIDCToken(jwt)
	if err != nil {
		t.Fatalf("ParseOIDCToken: %v", err)
	}
	if claims.Repository != "mochilang/mochi" {
		t.Errorf("Repository = %q, want mochilang/mochi", claims.Repository)
	}
}

// ── ValidateClaims ────────────────────────────────────────────────────────

func TestValidateClaims_Valid(t *testing.T) {
	c := validClaims(time.Now().Add(1 * time.Hour).Unix())
	if err := ValidateClaims(c, time.Now()); err != nil {
		t.Errorf("unexpected error: %v", err)
	}
}

func TestValidateClaims_WrongAudience(t *testing.T) {
	c := validClaims(time.Now().Add(1 * time.Hour).Unix())
	c.Audience = "crates.io"
	err := ValidateClaims(c, time.Now())
	if err == nil || !strings.Contains(err.Error(), "audience") {
		t.Errorf("expected audience error, got: %v", err)
	}
}

func TestValidateClaims_Expired(t *testing.T) {
	c := validClaims(time.Now().Add(-1 * time.Hour).Unix())
	err := ValidateClaims(c, time.Now())
	if err == nil || !strings.Contains(err.Error(), "expired") {
		t.Errorf("expected expired error, got: %v", err)
	}
}

func TestValidateClaims_ZeroExpiry(t *testing.T) {
	c := validClaims(0)
	err := ValidateClaims(c, time.Now())
	if err == nil {
		t.Error("expected error for zero exp")
	}
}

func TestValidateClaims_EmptyIssuer(t *testing.T) {
	c := validClaims(time.Now().Add(1 * time.Hour).Unix())
	c.Issuer = ""
	err := ValidateClaims(c, time.Now())
	if err == nil {
		t.Error("expected error for empty iss")
	}
}

func TestValidateClaims_EmptySubject(t *testing.T) {
	c := validClaims(time.Now().Add(1 * time.Hour).Unix())
	c.Subject = ""
	err := ValidateClaims(c, time.Now())
	if err == nil {
		t.Error("expected error for empty sub")
	}
}

// ── FromEnv ───────────────────────────────────────────────────────────────

func TestFromEnv_Valid(t *testing.T) {
	env := map[string]string{
		"ACTIONS_ID_TOKEN_REQUEST_URL":   "https://example.com/token",
		"ACTIONS_ID_TOKEN_REQUEST_TOKEN": "bearer-token",
	}
	gha, err := FromEnv(env)
	if err != nil {
		t.Fatalf("FromEnv: %v", err)
	}
	if gha.RequestURL != "https://example.com/token" {
		t.Errorf("RequestURL = %q", gha.RequestURL)
	}
}

func TestFromEnv_MissingURL(t *testing.T) {
	env := map[string]string{"ACTIONS_ID_TOKEN_REQUEST_TOKEN": "tok"}
	_, err := FromEnv(env)
	if err == nil {
		t.Error("expected ErrOIDCEnvMissing for missing URL")
	}
}

func TestFromEnv_MissingToken(t *testing.T) {
	env := map[string]string{"ACTIONS_ID_TOKEN_REQUEST_URL": "https://x.com"}
	_, err := FromEnv(env)
	if err == nil {
		t.Error("expected ErrOIDCEnvMissing for missing token")
	}
}

func TestFromEnv_EmptyValues(t *testing.T) {
	env := map[string]string{
		"ACTIONS_ID_TOKEN_REQUEST_URL":   "",
		"ACTIONS_ID_TOKEN_REQUEST_TOKEN": "tok",
	}
	_, err := FromEnv(env)
	if err == nil {
		t.Error("expected error for empty URL")
	}
}

func TestErrOIDCEnvMissing_MentionsHexAPIKey(t *testing.T) {
	msg := ErrOIDCEnvMissing.Error()
	if !strings.Contains(msg, "HEX_API_KEY") {
		t.Error("error message should mention HEX_API_KEY is not accepted")
	}
}

// ── ExchangeOIDCToken ─────────────────────────────────────────────────────

type fakeTransport struct {
	responses []fakeResponse
	idx       int
	calls     []fakeCall
}
type fakeResponse struct{ status int; body []byte }
type fakeCall struct{ url string; headers map[string]string; body []byte }

func (f *fakeTransport) Do(url string, headers map[string]string, body []byte) (int, []byte, error) {
	f.calls = append(f.calls, fakeCall{url: url, headers: headers, body: body})
	if f.idx >= len(f.responses) {
		return 500, nil, fmt.Errorf("unexpected call %d", f.idx)
	}
	r := f.responses[f.idx]
	f.idx++
	return r.status, r.body, nil
}

func hexAuthResponse(key string) []byte {
	b, _ := json.Marshal(map[string]string{"key": key})
	return b
}

func TestExchangeOIDCToken_Success(t *testing.T) {
	tr := &fakeTransport{responses: []fakeResponse{
		{200, hexAuthResponse("short-lived-key-abc123")},
	}}
	key, err := ExchangeOIDCToken(futureJWT(), "", tr)
	if err != nil {
		t.Fatalf("ExchangeOIDCToken: %v", err)
	}
	if key != "short-lived-key-abc123" {
		t.Errorf("key = %q", key)
	}
}

func TestExchangeOIDCToken_UsesDefaultHexURL(t *testing.T) {
	tr := &fakeTransport{responses: []fakeResponse{
		{200, hexAuthResponse("k")},
	}}
	_, _ = ExchangeOIDCToken(futureJWT(), "", tr)
	if len(tr.calls) == 0 || !strings.Contains(tr.calls[0].url, DefaultHexURL) {
		t.Errorf("expected call to DefaultHexURL, got: %v", tr.calls)
	}
}

func TestExchangeOIDCToken_AuthEndpoint(t *testing.T) {
	tr := &fakeTransport{responses: []fakeResponse{
		{200, hexAuthResponse("k")},
	}}
	_, _ = ExchangeOIDCToken("a.b.c", DefaultHexURL, tr)
	if len(tr.calls) > 0 && !strings.HasSuffix(tr.calls[0].url, "/api/auth") {
		t.Errorf("expected /api/auth endpoint, got: %s", tr.calls[0].url)
	}
}

func TestExchangeOIDCToken_Non200(t *testing.T) {
	tr := &fakeTransport{responses: []fakeResponse{{401, []byte(`{"error":"unauthorized"}`)}}}
	_, err := ExchangeOIDCToken(futureJWT(), "", tr)
	if err == nil {
		t.Error("expected error for 401 response")
	}
}

func TestExchangeOIDCToken_MissingKey(t *testing.T) {
	tr := &fakeTransport{responses: []fakeResponse{{200, []byte(`{}`)}}}
	_, err := ExchangeOIDCToken(futureJWT(), "", tr)
	if err == nil {
		t.Error("expected error when response has no 'key' field")
	}
}

// ── PublishRequest.Validate ───────────────────────────────────────────────

func TestValidate_Valid(t *testing.T) {
	r := PublishRequest{
		PackageName:  "cowboy",
		Version:      "2.12.0",
		OIDCToken:    futureJWT(),
		TarballBytes: []byte("tarball"),
	}
	if err := r.Validate(); err != nil {
		t.Errorf("unexpected error: %v", err)
	}
}

func TestValidate_EmptyPackageName(t *testing.T) {
	r := PublishRequest{Version: "1.0", OIDCToken: "t", TarballBytes: []byte("x")}
	if err := r.Validate(); err == nil {
		t.Error("expected error for empty package name")
	}
}

func TestValidate_EmptyVersion(t *testing.T) {
	r := PublishRequest{PackageName: "cowboy", OIDCToken: "t", TarballBytes: []byte("x")}
	if err := r.Validate(); err == nil {
		t.Error("expected error for empty version")
	}
}

func TestValidate_EmptyOIDCToken(t *testing.T) {
	r := PublishRequest{PackageName: "cowboy", Version: "1.0", TarballBytes: []byte("x")}
	if err := r.Validate(); err == nil {
		t.Error("expected error for empty OIDC token")
	}
}

func TestValidate_EmptyTarball(t *testing.T) {
	r := PublishRequest{PackageName: "cowboy", Version: "1.0", OIDCToken: "t"}
	if err := r.Validate(); err == nil {
		t.Error("expected error for empty tarball")
	}
}

func TestValidate_MentionsHexAPIKey(t *testing.T) {
	r := PublishRequest{PackageName: "cowboy", Version: "1.0", TarballBytes: []byte("x")}
	err := r.Validate()
	if err != nil && !strings.Contains(err.Error(), "HEX_API_KEY") {
		t.Errorf("empty OIDC token error should mention HEX_API_KEY: %v", err)
	}
}

// ── Publish ───────────────────────────────────────────────────────────────

func TestPublish_DryRun(t *testing.T) {
	tr := &fakeTransport{responses: []fakeResponse{
		{200, hexAuthResponse("dry-run-key")},
	}}
	req := PublishRequest{
		PackageName:  "cowboy",
		Version:      "2.12.0",
		OIDCToken:    futureJWT(),
		TarballBytes: []byte("tarball"),
		DryRun:       true,
	}
	res, err := Publish(req, tr)
	if err != nil {
		t.Fatalf("Publish dry-run: %v", err)
	}
	if res.APIKey == "" {
		t.Error("DryRun should still return API key")
	}
	if res.UploadedURL != "" {
		t.Error("DryRun should not set UploadedURL")
	}
	if res.StatusCode != 0 {
		t.Error("DryRun should not set StatusCode")
	}
	// Must NOT have called the upload endpoint.
	for _, call := range tr.calls {
		if strings.Contains(call.url, "/releases") {
			t.Error("DryRun must not call upload endpoint")
		}
	}
}

func TestPublish_Success(t *testing.T) {
	tr := &fakeTransport{responses: []fakeResponse{
		{200, hexAuthResponse("live-key-xyz")},
		{201, []byte(`{"status":"ok"}`)},
	}}
	req := PublishRequest{
		PackageName:  "cowboy",
		Version:      "2.12.0",
		OIDCToken:    futureJWT(),
		TarballBytes: []byte("tarball content"),
	}
	res, err := Publish(req, tr)
	if err != nil {
		t.Fatalf("Publish: %v", err)
	}
	if res.StatusCode != 201 {
		t.Errorf("StatusCode = %d, want 201", res.StatusCode)
	}
	if !strings.Contains(res.UploadedURL, "/packages/cowboy/releases") {
		t.Errorf("UploadedURL = %q", res.UploadedURL)
	}
}

func TestPublish_UploadUsesAPIKeyAuth(t *testing.T) {
	tr := &fakeTransport{responses: []fakeResponse{
		{200, hexAuthResponse("the-api-key")},
		{201, []byte(`{}`)},
	}}
	req := PublishRequest{
		PackageName:  "ranch",
		Version:      "2.1.0",
		OIDCToken:    futureJWT(),
		TarballBytes: []byte("tgz"),
	}
	_, _ = Publish(req, tr)
	if len(tr.calls) < 2 {
		t.Fatal("expected 2 transport calls")
	}
	authHdr := tr.calls[1].headers["Authorization"]
	if authHdr != "key the-api-key" {
		t.Errorf("Authorization header = %q, want 'key the-api-key'", authHdr)
	}
}

func TestPublish_ExpiredToken(t *testing.T) {
	tr := &fakeTransport{}
	req := PublishRequest{
		PackageName:  "cowboy",
		Version:      "2.12.0",
		OIDCToken:    expiredJWT(),
		TarballBytes: []byte("tgz"),
	}
	_, err := Publish(req, tr)
	if err == nil || !strings.Contains(err.Error(), "expired") {
		t.Errorf("expected expired error, got: %v", err)
	}
	if len(tr.calls) > 0 {
		t.Error("expired token must not trigger any network call")
	}
}

func TestPublish_UploadFailure(t *testing.T) {
	tr := &fakeTransport{responses: []fakeResponse{
		{200, hexAuthResponse("key")},
		{409, []byte(`{"message":"version already published"}`)},
	}}
	req := PublishRequest{
		PackageName:  "cowboy",
		Version:      "2.12.0",
		OIDCToken:    futureJWT(),
		TarballBytes: []byte("tgz"),
	}
	_, err := Publish(req, tr)
	if err == nil {
		t.Error("expected error for 409 upload response")
	}
}

func TestPublish_NilTransportDryRun(t *testing.T) {
	// With DryRun=true and a nil transport, only the OIDC exchange call
	// is needed; but we have no transport here. Actually with DryRun
	// the upload is skipped but the OIDC exchange still needs a transport.
	// This test verifies the nil-transport guard on non-dry-run path.
	req := PublishRequest{
		PackageName:  "cowboy",
		Version:      "1.0",
		OIDCToken:    futureJWT(),
		TarballBytes: []byte("x"),
	}
	// Non-dry-run with nil transport: should fail at exchange call, not panic.
	// (ExchangeOIDCToken returns error when transport is nil via Do call)
	_, err := Publish(req, nil)
	if err == nil {
		t.Error("expected error with nil transport on non-dry-run")
	}
}
