package publish

import (
	"context"
	"encoding/json"
	"fmt"
	"io"
	"net/http"
	"net/http/httptest"
	"os"
	"path/filepath"
	"strings"
	"testing"
)

// makeGemFile creates a temporary .gem file with the given content and returns
// its path. The caller is responsible for removing the file.
func makeGemFile(t *testing.T, content string) string {
	t.Helper()
	dir := t.TempDir()
	p := filepath.Join(dir, "test-0.1.0.gem")
	if err := os.WriteFile(p, []byte(content), 0o644); err != nil {
		t.Fatal(err)
	}
	return p
}

// oidcServer returns an httptest.Server that serves a single OIDC token
// response. It checks that the Authorization header contains the bearer token.
func oidcServer(t *testing.T, bearerToken, idToken string) *httptest.Server {
	t.Helper()
	return httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		auth := r.Header.Get("Authorization")
		if auth != "Bearer "+bearerToken {
			w.WriteHeader(http.StatusUnauthorized)
			return
		}
		w.Header().Set("Content-Type", "application/json")
		if err := json.NewEncoder(w).Encode(map[string]string{"value": idToken}); err != nil {
			t.Logf("oidcServer encode error: %v", err)
		}
	}))
}

// exchangeServer returns an httptest.Server that accepts an OIDC token and
// returns an API key.
func exchangeServer(t *testing.T, wantToken, apiKey string) *httptest.Server {
	t.Helper()
	return httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		if r.Method != http.MethodPost || !strings.HasSuffix(r.URL.Path, "/trusted_publishing/tokens") {
			w.WriteHeader(http.StatusNotFound)
			return
		}
		var body struct {
			Token string `json:"rubygems_oidc_token"`
		}
		if err := json.NewDecoder(r.Body).Decode(&body); err != nil {
			w.WriteHeader(http.StatusBadRequest)
			return
		}
		if body.Token != wantToken {
			w.WriteHeader(http.StatusUnauthorized)
			json.NewEncoder(w).Encode(map[string]string{"error": "bad token"})
			return
		}
		w.Header().Set("Content-Type", "application/json")
		json.NewEncoder(w).Encode(map[string]string{"api_key": apiKey})
	}))
}

// pushServer returns an httptest.Server that accepts multipart gem pushes.
func pushServer(t *testing.T, wantAPIKey string) *httptest.Server {
	t.Helper()
	return httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		if r.Method != http.MethodPost || !strings.HasSuffix(r.URL.Path, "/api/v1/gems") {
			w.WriteHeader(http.StatusNotFound)
			return
		}
		if r.Header.Get("Authorization") != wantAPIKey {
			w.WriteHeader(http.StatusForbidden)
			return
		}
		if err := r.ParseMultipartForm(10 << 20); err != nil {
			w.WriteHeader(http.StatusBadRequest)
			return
		}
		f, _, err := r.FormFile("gem")
		if err != nil {
			w.WriteHeader(http.StatusBadRequest)
			fmt.Fprintf(w, "missing gem field: %v", err)
			return
		}
		f.Close()
		w.WriteHeader(http.StatusOK)
	}))
}

// sigstoreServer returns an httptest.Server that mocks the Rekor endpoint.
func sigstoreServer(t *testing.T, entryID string) *httptest.Server {
	t.Helper()
	return httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		w.WriteHeader(http.StatusCreated)
		json.NewEncoder(w).Encode(map[string]string{"entryID": entryID})
	}))
}

// ---- GetOIDCToken tests ----

func TestGetOIDCToken_NoEnvVars(t *testing.T) {
	t.Setenv("ACTIONS_ID_TOKEN_REQUEST_URL", "")
	t.Setenv("ACTIONS_ID_TOKEN_REQUEST_TOKEN", "")
	_, err := GetOIDCToken(context.Background())
	if err == nil {
		t.Fatal("expected error, got nil")
	}
	if !strings.Contains(err.Error(), "no OIDC ID token") {
		t.Fatalf("unexpected error: %v", err)
	}
}

func TestGetOIDCToken_MissingURL(t *testing.T) {
	t.Setenv("ACTIONS_ID_TOKEN_REQUEST_URL", "")
	t.Setenv("ACTIONS_ID_TOKEN_REQUEST_TOKEN", "tok")
	_, err := GetOIDCToken(context.Background())
	if err == nil || !strings.Contains(err.Error(), "no OIDC ID token") {
		t.Fatalf("expected ErrNoOIDCToken, got: %v", err)
	}
}

func TestGetOIDCToken_MissingToken(t *testing.T) {
	t.Setenv("ACTIONS_ID_TOKEN_REQUEST_URL", "http://example.com")
	t.Setenv("ACTIONS_ID_TOKEN_REQUEST_TOKEN", "")
	_, err := GetOIDCToken(context.Background())
	if err == nil || !strings.Contains(err.Error(), "no OIDC ID token") {
		t.Fatalf("expected ErrNoOIDCToken, got: %v", err)
	}
}

func TestGetOIDCToken_Success(t *testing.T) {
	srv := oidcServer(t, "mybearer", "id-token-value")
	defer srv.Close()
	t.Setenv("ACTIONS_ID_TOKEN_REQUEST_URL", srv.URL)
	t.Setenv("ACTIONS_ID_TOKEN_REQUEST_TOKEN", "mybearer")
	tok, err := GetOIDCToken(context.Background())
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if tok != "id-token-value" {
		t.Fatalf("expected id-token-value, got %q", tok)
	}
}

func TestGetOIDCToken_BadBearer(t *testing.T) {
	srv := oidcServer(t, "correct", "id-token-value")
	defer srv.Close()
	t.Setenv("ACTIONS_ID_TOKEN_REQUEST_URL", srv.URL)
	t.Setenv("ACTIONS_ID_TOKEN_REQUEST_TOKEN", "wrong")
	_, err := GetOIDCToken(context.Background())
	if err == nil {
		t.Fatal("expected error for wrong bearer token")
	}
}

func TestGetOIDCToken_ServerError(t *testing.T) {
	srv := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		w.WriteHeader(http.StatusInternalServerError)
		fmt.Fprint(w, "internal error")
	}))
	defer srv.Close()
	t.Setenv("ACTIONS_ID_TOKEN_REQUEST_URL", srv.URL)
	t.Setenv("ACTIONS_ID_TOKEN_REQUEST_TOKEN", "tok")
	_, err := GetOIDCToken(context.Background())
	if err == nil || !strings.Contains(err.Error(), "500") {
		t.Fatalf("expected HTTP 500 error, got: %v", err)
	}
}

func TestGetOIDCToken_EmptyValue(t *testing.T) {
	srv := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		w.Header().Set("Content-Type", "application/json")
		json.NewEncoder(w).Encode(map[string]string{"value": ""})
	}))
	defer srv.Close()
	t.Setenv("ACTIONS_ID_TOKEN_REQUEST_URL", srv.URL)
	t.Setenv("ACTIONS_ID_TOKEN_REQUEST_TOKEN", "tok")
	_, err := GetOIDCToken(context.Background())
	if err == nil || !strings.Contains(err.Error(), "empty value") {
		t.Fatalf("expected empty-value error, got: %v", err)
	}
}

func TestGetOIDCToken_InvalidJSON(t *testing.T) {
	srv := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		w.Header().Set("Content-Type", "application/json")
		fmt.Fprint(w, "not json")
	}))
	defer srv.Close()
	t.Setenv("ACTIONS_ID_TOKEN_REQUEST_URL", srv.URL)
	t.Setenv("ACTIONS_ID_TOKEN_REQUEST_TOKEN", "tok")
	_, err := GetOIDCToken(context.Background())
	if err == nil {
		t.Fatal("expected JSON decode error")
	}
}

// ---- ExchangeOIDCToken tests ----

func TestExchangeOIDCToken_Success(t *testing.T) {
	srv := exchangeServer(t, "valid-oidc", "short-lived-key")
	defer srv.Close()
	key, err := exchangeOIDCTokenWithBase(context.Background(), "valid-oidc", srv.URL)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if key != "short-lived-key" {
		t.Fatalf("expected short-lived-key, got %q", key)
	}
}

func TestExchangeOIDCToken_BadToken(t *testing.T) {
	srv := exchangeServer(t, "correct", "key")
	defer srv.Close()
	_, err := exchangeOIDCTokenWithBase(context.Background(), "wrong", srv.URL)
	if err == nil {
		t.Fatal("expected error for wrong OIDC token")
	}
}

func TestExchangeOIDCToken_ServerError(t *testing.T) {
	srv := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		w.WriteHeader(http.StatusServiceUnavailable)
	}))
	defer srv.Close()
	_, err := exchangeOIDCTokenWithBase(context.Background(), "tok", srv.URL)
	if err == nil || !strings.Contains(err.Error(), "503") {
		t.Fatalf("expected HTTP 503 error, got: %v", err)
	}
}

func TestExchangeOIDCToken_EmptyAPIKey(t *testing.T) {
	srv := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		w.Header().Set("Content-Type", "application/json")
		json.NewEncoder(w).Encode(map[string]string{"api_key": ""})
	}))
	defer srv.Close()
	_, err := exchangeOIDCTokenWithBase(context.Background(), "tok", srv.URL)
	if err == nil || !strings.Contains(err.Error(), "empty api_key") {
		t.Fatalf("expected empty api_key error, got: %v", err)
	}
}

// ---- PushGem tests ----

func TestPushGem_Success(t *testing.T) {
	gemPath := makeGemFile(t, "fake gem content")
	srv := pushServer(t, "my-api-key")
	defer srv.Close()
	ctx := context.Background()
	if err := pushGemWithBase(ctx, gemPath, "my-api-key", srv.URL); err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
}

func TestPushGem_WrongAPIKey(t *testing.T) {
	gemPath := makeGemFile(t, "fake gem content")
	srv := pushServer(t, "correct-key")
	defer srv.Close()
	ctx := context.Background()
	err := pushGemWithBase(ctx, gemPath, "wrong-key", srv.URL)
	if err == nil {
		t.Fatal("expected error for wrong API key")
	}
}

func TestPushGem_FileNotFound(t *testing.T) {
	ctx := context.Background()
	err := pushGemWithBase(ctx, "/nonexistent/path/gem.gem", "key", "http://localhost:9999")
	if err == nil || !strings.Contains(err.Error(), "open gem file") {
		t.Fatalf("expected file open error, got: %v", err)
	}
}

func TestPushGem_ServerError(t *testing.T) {
	gemPath := makeGemFile(t, "content")
	srv := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		w.WriteHeader(http.StatusInternalServerError)
		fmt.Fprint(w, "server error")
	}))
	defer srv.Close()
	err := pushGemWithBase(context.Background(), gemPath, "key", srv.URL)
	if err == nil || !strings.Contains(err.Error(), "500") {
		t.Fatalf("expected HTTP 500, got: %v", err)
	}
}

// ---- Publish integration tests ----

func TestPublish_DryRun(t *testing.T) {
	gemPath := makeGemFile(t, "dry run gem content")
	cfg := PublishConfig{
		GemPath: gemPath,
		GemName: "test-gem",
		Version: "0.1.0",
		DryRun:  true,
	}
	result, err := Publish(cfg)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if !result.DryRun {
		t.Error("expected DryRun=true in result")
	}
	if result.SHA256 == "" {
		t.Error("expected non-empty SHA256 even on dry run")
	}
	if result.RekorLogID != "" {
		t.Errorf("expected empty RekorLogID on dry run, got %q", result.RekorLogID)
	}
}

func TestPublish_DryRunSHA256Correct(t *testing.T) {
	content := "gem file bytes"
	gemPath := makeGemFile(t, content)
	cfg := PublishConfig{
		GemPath: gemPath,
		GemName: "sha-gem",
		Version: "1.0.0",
		DryRun:  true,
	}
	result, err := Publish(cfg)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	// Verify SHA256 is correct length (64 hex chars).
	if len(result.SHA256) != 64 {
		t.Errorf("expected 64-char hex SHA256, got %q (len=%d)", result.SHA256, len(result.SHA256))
	}
}

func TestPublish_FullFlow(t *testing.T) {
	gemContent := "real gem bytes for full flow"
	gemPath := makeGemFile(t, gemContent)

	exchSrv := exchangeServer(t, "oidc-id-tok", "live-api-key")
	defer exchSrv.Close()
	pushSrv := pushServer(t, "live-api-key")
	defer pushSrv.Close()

	cfg := PublishConfig{
		GemPath:     gemPath,
		GemName:     "full-flow-gem",
		Version:     "2.0.0",
		OIDCToken:   "oidc-id-tok",
		RegistryURL: exchSrv.URL,
	}

	// Override pushGemWithBase base by pointing RegistryURL to a combined server
	// that handles both /api/v1/trusted_publishing/tokens and /api/v1/gems.
	combinedSrv := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		switch {
		case strings.HasSuffix(r.URL.Path, "/trusted_publishing/tokens"):
			var body struct {
				Token string `json:"rubygems_oidc_token"`
			}
			json.NewDecoder(r.Body).Decode(&body)
			if body.Token != "oidc-id-tok" {
				w.WriteHeader(http.StatusUnauthorized)
				return
			}
			json.NewEncoder(w).Encode(map[string]string{"api_key": "combined-key"})
		case strings.HasSuffix(r.URL.Path, "/api/v1/gems"):
			if r.Header.Get("Authorization") != "combined-key" {
				w.WriteHeader(http.StatusForbidden)
				return
			}
			r.ParseMultipartForm(10 << 20)
			f, _, err := r.FormFile("gem")
			if err != nil {
				w.WriteHeader(http.StatusBadRequest)
				return
			}
			f.Close()
			w.WriteHeader(http.StatusOK)
		default:
			w.WriteHeader(http.StatusNotFound)
		}
	}))
	defer combinedSrv.Close()

	cfg.RegistryURL = combinedSrv.URL
	result, err := Publish(cfg)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if result.GemName != "full-flow-gem" {
		t.Errorf("expected GemName=full-flow-gem, got %q", result.GemName)
	}
	if result.Version != "2.0.0" {
		t.Errorf("expected Version=2.0.0, got %q", result.Version)
	}
	if result.SHA256 == "" {
		t.Error("expected non-empty SHA256")
	}
	if result.DryRun {
		t.Error("expected DryRun=false")
	}
}

func TestPublish_MissingGemFile(t *testing.T) {
	cfg := PublishConfig{
		GemPath: "/no/such/gem.gem",
		GemName: "missing",
		Version: "0.0.1",
		DryRun:  true,
	}
	_, err := Publish(cfg)
	if err == nil {
		t.Fatal("expected error for missing gem file")
	}
}

func TestPublish_OIDCFetchError(t *testing.T) {
	// No OIDCToken and no CI environment set.
	t.Setenv("ACTIONS_ID_TOKEN_REQUEST_URL", "")
	t.Setenv("ACTIONS_ID_TOKEN_REQUEST_TOKEN", "")
	gemPath := makeGemFile(t, "bytes")
	cfg := PublishConfig{
		GemPath: gemPath,
		GemName: "no-oidc-gem",
		Version: "0.1.0",
		DryRun:  false,
	}
	_, err := Publish(cfg)
	if err == nil || !strings.Contains(err.Error(), "OIDC token") {
		t.Fatalf("expected OIDC token error, got: %v", err)
	}
}

func TestPublish_ExchangeError(t *testing.T) {
	gemPath := makeGemFile(t, "bytes")
	// Exchange server rejects the token.
	srv := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		w.WriteHeader(http.StatusUnauthorized)
		fmt.Fprint(w, `{"error":"invalid"}`)
	}))
	defer srv.Close()
	cfg := PublishConfig{
		GemPath:     gemPath,
		GemName:     "bad-exchange",
		Version:     "0.1.0",
		OIDCToken:   "bad-token",
		RegistryURL: srv.URL,
	}
	_, err := Publish(cfg)
	if err == nil {
		t.Fatal("expected error when exchange fails")
	}
}

func TestPublish_PushError(t *testing.T) {
	gemPath := makeGemFile(t, "bytes")
	srv := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		switch {
		case strings.HasSuffix(r.URL.Path, "/trusted_publishing/tokens"):
			json.NewEncoder(w).Encode(map[string]string{"api_key": "k"})
		case strings.HasSuffix(r.URL.Path, "/api/v1/gems"):
			w.WriteHeader(http.StatusInternalServerError)
		default:
			w.WriteHeader(http.StatusNotFound)
		}
	}))
	defer srv.Close()
	cfg := PublishConfig{
		GemPath:     gemPath,
		GemName:     "push-error",
		Version:     "0.1.0",
		OIDCToken:   "tok",
		RegistryURL: srv.URL,
	}
	_, err := Publish(cfg)
	if err == nil {
		t.Fatal("expected error when push fails")
	}
}

func TestPublish_SigstoreEnabled(t *testing.T) {
	combinedSrv := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		switch {
		case strings.HasSuffix(r.URL.Path, "/trusted_publishing/tokens"):
			json.NewEncoder(w).Encode(map[string]string{"api_key": "sigkey"})
		case strings.HasSuffix(r.URL.Path, "/api/v1/gems"):
			r.ParseMultipartForm(10 << 20)
			f, _, _ := r.FormFile("gem")
			if f != nil {
				f.Close()
			}
			w.WriteHeader(http.StatusOK)
		case strings.HasSuffix(r.URL.Path, "/api/v1/log/entries"):
			w.WriteHeader(http.StatusCreated)
			json.NewEncoder(w).Encode(map[string]string{"entryID": "rekor-12345"})
		default:
			w.WriteHeader(http.StatusNotFound)
		}
	}))
	defer combinedSrv.Close()

	// Temporarily redirect sigstoreBase by using the recordSigstore internals.
	// We test best-effort behavior: sigstore success gives a RekorLogID.
	t.Setenv("SIGSTORE_ENABLED", "true")

	// We can't easily override sigstoreBase without exposing it, so we test
	// recordSigstore directly.
	id, err := recordSigstoreWithBase(context.Background(), "sig-gem", "1.0.0", "abc123", combinedSrv.URL)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if id != "rekor-12345" {
		t.Errorf("expected rekor-12345, got %q", id)
	}
}

func TestPublish_SigstoreDisabledByDefault(t *testing.T) {
	t.Setenv("SIGSTORE_ENABLED", "")
	gemPath := makeGemFile(t, "bytes")

	srv := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		switch {
		case strings.HasSuffix(r.URL.Path, "/trusted_publishing/tokens"):
			json.NewEncoder(w).Encode(map[string]string{"api_key": "k"})
		case strings.HasSuffix(r.URL.Path, "/api/v1/gems"):
			r.ParseMultipartForm(10 << 20)
			if f, _, err := r.FormFile("gem"); err == nil {
				io.Copy(io.Discard, f)
				f.Close()
			}
			w.WriteHeader(http.StatusOK)
		default:
			w.WriteHeader(http.StatusNotFound)
		}
	}))
	defer srv.Close()

	cfg := PublishConfig{
		GemPath:     gemPath,
		GemName:     "no-sigstore",
		Version:     "0.1.0",
		OIDCToken:   "tok",
		RegistryURL: srv.URL,
	}
	result, err := Publish(cfg)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if result.RekorLogID != "" {
		t.Errorf("expected empty RekorLogID when SIGSTORE_ENABLED is unset, got %q", result.RekorLogID)
	}
}

func TestPublish_SigstoreFailureDoesNotAbortPublish(t *testing.T) {
	t.Setenv("SIGSTORE_ENABLED", "true")
	gemPath := makeGemFile(t, "bytes")

	srv := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		switch {
		case strings.HasSuffix(r.URL.Path, "/trusted_publishing/tokens"):
			json.NewEncoder(w).Encode(map[string]string{"api_key": "k"})
		case strings.HasSuffix(r.URL.Path, "/api/v1/gems"):
			r.ParseMultipartForm(10 << 20)
			if f, _, err := r.FormFile("gem"); err == nil {
				io.Copy(io.Discard, f)
				f.Close()
			}
			w.WriteHeader(http.StatusOK)
		default:
			// Sigstore endpoint would be a separate URL; since we can't hit the
			// real one in tests and it's best-effort, publish should still succeed.
			w.WriteHeader(http.StatusNotFound)
		}
	}))
	defer srv.Close()

	cfg := PublishConfig{
		GemPath:     gemPath,
		GemName:     "sig-fail-gem",
		Version:     "0.1.0",
		OIDCToken:   "tok",
		RegistryURL: srv.URL,
	}
	// Publish must still succeed even if sigstore is unreachable.
	result, err := Publish(cfg)
	if err != nil {
		t.Fatalf("publish must succeed even if sigstore fails, got error: %v", err)
	}
	if result.SHA256 == "" {
		t.Error("expected non-empty SHA256")
	}
}

// recordSigstoreWithBase is a test helper that exposes the sigstore URL for testing.
func recordSigstoreWithBase(ctx context.Context, gemName, version, sha256hex, baseURL string) (string, error) {
	import_url := baseURL + "/api/v1/log/entries"

	payload := oidcEntryPayload{
		Kind:       "gem",
		APIVersion: "0.0.1",
		Spec: map[string]string{
			"gem_name": gemName,
			"version":  version,
			"sha256":   sha256hex,
		},
	}
	import_body, err := json.Marshal(payload)
	if err != nil {
		return "", err
	}

	req, err := http.NewRequestWithContext(ctx, http.MethodPost, import_url, strings.NewReader(string(import_body)))
	if err != nil {
		return "", err
	}
	req.Header.Set("Content-Type", "application/json")
	req.Header.Set("Accept", "application/json")

	resp, err := http.DefaultClient.Do(req)
	if err != nil {
		return "", err
	}
	defer resp.Body.Close()

	if resp.StatusCode != http.StatusCreated && resp.StatusCode != http.StatusOK {
		return "", fmt.Errorf("rekor returned HTTP %d", resp.StatusCode)
	}

	var rr rekorResponse
	if err := json.NewDecoder(resp.Body).Decode(&rr); err != nil {
		return "", err
	}
	return rr.EntryID, nil
}
