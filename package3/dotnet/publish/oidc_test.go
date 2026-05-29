package publish_test

import (
	"context"
	"encoding/json"
	"net/http"
	"net/http/httptest"
	"os"
	"path/filepath"
	"strings"
	"testing"

	"mochi/package3/dotnet/publish"
)

// ---------- FetchOIDCToken tests ----------

func TestFetchOIDCToken_emptyURL(t *testing.T) {
	_, err := publish.FetchOIDCToken(context.Background(), "", "tok")
	if err == nil {
		t.Fatal("expected error for empty URL")
	}
}

func TestFetchOIDCToken_success(t *testing.T) {
	srv := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		if r.Header.Get("Authorization") != "Bearer mytoken" {
			http.Error(w, "unauthorized", http.StatusUnauthorized)
			return
		}
		w.Header().Set("Content-Type", "application/json")
		_ = json.NewEncoder(w).Encode(publish.OIDCTokenResponse{Value: "jwt-value-abc"})
	}))
	defer srv.Close()

	got, err := publish.FetchOIDCToken(context.Background(), srv.URL, "mytoken")
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if got != "jwt-value-abc" {
		t.Errorf("expected jwt-value-abc, got %s", got)
	}
}

func TestFetchOIDCToken_non200(t *testing.T) {
	srv := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		http.Error(w, "forbidden", http.StatusForbidden)
	}))
	defer srv.Close()

	_, err := publish.FetchOIDCToken(context.Background(), srv.URL, "")
	if err == nil {
		t.Fatal("expected error for 403")
	}
	if !strings.Contains(err.Error(), "403") {
		t.Errorf("expected 403 in error, got: %v", err)
	}
}

func TestFetchOIDCToken_malformedJSON(t *testing.T) {
	srv := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		w.Header().Set("Content-Type", "application/json")
		_, _ = w.Write([]byte("not-json{"))
	}))
	defer srv.Close()

	_, err := publish.FetchOIDCToken(context.Background(), srv.URL, "")
	if err == nil {
		t.Fatal("expected error for malformed JSON")
	}
}

func TestFetchOIDCToken_emptyValue(t *testing.T) {
	srv := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		w.Header().Set("Content-Type", "application/json")
		_ = json.NewEncoder(w).Encode(publish.OIDCTokenResponse{Value: ""})
	}))
	defer srv.Close()

	_, err := publish.FetchOIDCToken(context.Background(), srv.URL, "")
	if err == nil {
		t.Fatal("expected error for empty value")
	}
}

// ---------- ExchangeForNuGetToken tests ----------

func TestExchangeForNuGetToken_success(t *testing.T) {
	srv := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		if r.Method != http.MethodPost {
			http.Error(w, "method not allowed", http.StatusMethodNotAllowed)
			return
		}
		w.Header().Set("Content-Type", "application/json")
		_, _ = w.Write([]byte(`{"token":"nuget-secret","expiresAt":"2026-06-01T00:00:00Z"}`))
	}))
	defer srv.Close()

	cfg := publish.OIDCConfig{MockFulcioURL: srv.URL}
	tok, err := publish.ExchangeForNuGetToken(context.Background(), cfg, "my-jwt")
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if tok.Token != "nuget-secret" {
		t.Errorf("expected nuget-secret, got %s", tok.Token)
	}
	if tok.ExpiresAt.IsZero() {
		t.Error("expected non-zero ExpiresAt")
	}
}

func TestExchangeForNuGetToken_non200(t *testing.T) {
	srv := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		http.Error(w, "service unavailable", http.StatusServiceUnavailable)
	}))
	defer srv.Close()

	cfg := publish.OIDCConfig{MockFulcioURL: srv.URL}
	_, err := publish.ExchangeForNuGetToken(context.Background(), cfg, "jwt")
	if err == nil {
		t.Fatal("expected error for 503")
	}
}

func TestExchangeForNuGetToken_malformedJSON(t *testing.T) {
	srv := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		w.WriteHeader(http.StatusOK)
		_, _ = w.Write([]byte("{{bad"))
	}))
	defer srv.Close()

	cfg := publish.OIDCConfig{MockFulcioURL: srv.URL}
	_, err := publish.ExchangeForNuGetToken(context.Background(), cfg, "jwt")
	if err == nil {
		t.Fatal("expected error for malformed JSON")
	}
}

func TestExchangeForNuGetToken_emptyToken(t *testing.T) {
	srv := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		w.Header().Set("Content-Type", "application/json")
		_, _ = w.Write([]byte(`{"token":"","expiresAt":"2026-06-01T00:00:00Z"}`))
	}))
	defer srv.Close()

	cfg := publish.OIDCConfig{MockFulcioURL: srv.URL}
	_, err := publish.ExchangeForNuGetToken(context.Background(), cfg, "jwt")
	if err == nil {
		t.Fatal("expected error for empty token")
	}
}

func TestExchangeForNuGetToken_noExpiresAt(t *testing.T) {
	srv := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		w.Header().Set("Content-Type", "application/json")
		_, _ = w.Write([]byte(`{"token":"tok123"}`))
	}))
	defer srv.Close()

	cfg := publish.OIDCConfig{MockFulcioURL: srv.URL}
	tok, err := publish.ExchangeForNuGetToken(context.Background(), cfg, "jwt")
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if tok.Token != "tok123" {
		t.Errorf("expected tok123, got %s", tok.Token)
	}
}

// ---------- PublishNupkg tests ----------

func TestPublishNupkg_success(t *testing.T) {
	var gotKey string
	var gotMethod string
	srv := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		gotMethod = r.Method
		gotKey = r.Header.Get("X-NuGet-ApiKey")
		w.WriteHeader(http.StatusCreated)
	}))
	defer srv.Close()

	// Write a temp nupkg file.
	f, err := os.CreateTemp(t.TempDir(), "*.nupkg")
	if err != nil {
		t.Fatalf("create temp: %v", err)
	}
	_ = f.Close()

	err = publish.PublishNupkg(context.Background(), srv.URL, "the-api-key", f.Name())
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if gotMethod != http.MethodPut {
		t.Errorf("expected PUT, got %s", gotMethod)
	}
	if gotKey != "the-api-key" {
		t.Errorf("expected the-api-key, got %s", gotKey)
	}
}

func TestPublishNupkg_non2xx(t *testing.T) {
	srv := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		http.Error(w, "conflict", http.StatusConflict)
	}))
	defer srv.Close()

	f, err := os.CreateTemp(t.TempDir(), "*.nupkg")
	if err != nil {
		t.Fatalf("create temp: %v", err)
	}
	_ = f.Close()

	err = publish.PublishNupkg(context.Background(), srv.URL, "key", f.Name())
	if err == nil {
		t.Fatal("expected error for 409")
	}
}

func TestPublishNupkg_missingFile(t *testing.T) {
	err := publish.PublishNupkg(context.Background(), "http://nowhere", "key", "/nonexistent/path.nupkg")
	if err == nil {
		t.Fatal("expected error for missing file")
	}
}

// ---------- Run (dry-run) tests ----------

func TestRun_dryRun_success(t *testing.T) {
	// Mock OIDC endpoint.
	oidcSrv := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		w.Header().Set("Content-Type", "application/json")
		_ = json.NewEncoder(w).Encode(publish.OIDCTokenResponse{Value: "jwt-from-gha"})
	}))
	defer oidcSrv.Close()

	// Mock exchange endpoint.
	exchangeSrv := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		w.Header().Set("Content-Type", "application/json")
		_, _ = w.Write([]byte(`{"token":"nuget-dry","expiresAt":"2026-06-01T00:00:00Z"}`))
	}))
	defer exchangeSrv.Close()

	cfg := publish.OIDCConfig{
		PackageID:      "My.Package",
		PackageVersion: "1.0.0",
		OIDCTokenURL:   oidcSrv.URL,
		MockFulcioURL:  exchangeSrv.URL,
		DryRun:         true,
	}

	result, err := publish.Run(context.Background(), cfg, "/nonexistent.nupkg")
	if err != nil {
		t.Fatalf("unexpected error in dry run: %v", err)
	}
	if !result.OIDCTokenFetched {
		t.Error("expected OIDCTokenFetched to be true")
	}
	if !result.NuGetTokenExchanged {
		t.Error("expected NuGetTokenExchanged to be true")
	}
	if result.Uploaded {
		t.Error("expected Uploaded to be false in dry-run mode")
	}
	if result.DryRun != true {
		t.Error("expected DryRun to be true")
	}
}

func TestRun_fullMode_success(t *testing.T) {
	oidcSrv := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		w.Header().Set("Content-Type", "application/json")
		_ = json.NewEncoder(w).Encode(publish.OIDCTokenResponse{Value: "jwt-value"})
	}))
	defer oidcSrv.Close()

	exchangeSrv := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		w.Header().Set("Content-Type", "application/json")
		_, _ = w.Write([]byte(`{"token":"upload-token","expiresAt":"2026-06-01T00:00:00Z"}`))
	}))
	defer exchangeSrv.Close()

	uploadSrv := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		w.WriteHeader(http.StatusCreated)
	}))
	defer uploadSrv.Close()

	// Create temp nupkg.
	nupkgPath := filepath.Join(t.TempDir(), "test.nupkg")
	if err := os.WriteFile(nupkgPath, []byte("fake-nupkg"), 0o644); err != nil {
		t.Fatalf("write nupkg: %v", err)
	}

	cfg := publish.OIDCConfig{
		PackageID:        "My.Package",
		PackageVersion:   "1.0.0",
		NuGetRegistryURL: uploadSrv.URL,
		OIDCTokenURL:     oidcSrv.URL,
		MockFulcioURL:    exchangeSrv.URL,
		DryRun:           false,
	}

	result, err := publish.Run(context.Background(), cfg, nupkgPath)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if !result.OIDCTokenFetched {
		t.Error("expected OIDCTokenFetched true")
	}
	if !result.NuGetTokenExchanged {
		t.Error("expected NuGetTokenExchanged true")
	}
	if !result.Uploaded {
		t.Error("expected Uploaded true")
	}
}

func TestRun_oidcFetchFails(t *testing.T) {
	// Use a server that returns 500.
	srv := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		http.Error(w, "internal error", http.StatusInternalServerError)
	}))
	defer srv.Close()

	cfg := publish.OIDCConfig{
		OIDCTokenURL:  srv.URL,
		MockFulcioURL: srv.URL,
	}
	result, err := publish.Run(context.Background(), cfg, "/tmp/x.nupkg")
	if err == nil {
		t.Fatal("expected error when OIDC fetch fails")
	}
	if result.OIDCTokenFetched {
		t.Error("OIDCTokenFetched should be false")
	}
}

func TestRun_exchangeFails(t *testing.T) {
	oidcSrv := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		w.Header().Set("Content-Type", "application/json")
		_ = json.NewEncoder(w).Encode(publish.OIDCTokenResponse{Value: "jwt"})
	}))
	defer oidcSrv.Close()

	exchangeSrv := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		http.Error(w, "bad gateway", http.StatusBadGateway)
	}))
	defer exchangeSrv.Close()

	cfg := publish.OIDCConfig{
		OIDCTokenURL:  oidcSrv.URL,
		MockFulcioURL: exchangeSrv.URL,
	}
	result, err := publish.Run(context.Background(), cfg, "/tmp/x.nupkg")
	if err == nil {
		t.Fatal("expected error when exchange fails")
	}
	if !result.OIDCTokenFetched {
		t.Error("expected OIDCTokenFetched true after OIDC success")
	}
	if result.NuGetTokenExchanged {
		t.Error("NuGetTokenExchanged should be false")
	}
}

func TestRun_resultContainsPackageInfo(t *testing.T) {
	oidcSrv := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		w.Header().Set("Content-Type", "application/json")
		_ = json.NewEncoder(w).Encode(publish.OIDCTokenResponse{Value: "jwt"})
	}))
	defer oidcSrv.Close()

	exchangeSrv := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		w.Header().Set("Content-Type", "application/json")
		_, _ = w.Write([]byte(`{"token":"t","expiresAt":"2026-06-01T00:00:00Z"}`))
	}))
	defer exchangeSrv.Close()

	cfg := publish.OIDCConfig{
		PackageID:      "Foo.Bar",
		PackageVersion: "2.3.4",
		OIDCTokenURL:   oidcSrv.URL,
		MockFulcioURL:  exchangeSrv.URL,
		DryRun:         true,
	}
	result, err := publish.Run(context.Background(), cfg, "")
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if result.PackageID != "Foo.Bar" {
		t.Errorf("expected Foo.Bar, got %s", result.PackageID)
	}
	if result.PackageVersion != "2.3.4" {
		t.Errorf("expected 2.3.4, got %s", result.PackageVersion)
	}
}

func TestRun_defaultRegistryURL(t *testing.T) {
	cfg := publish.OIDCConfig{
		PackageID:    "X",
		OIDCTokenURL: "http://bad-host",
	}
	result, _ := publish.Run(context.Background(), cfg, "")
	if !strings.HasPrefix(result.RegistryURL, "https://") {
		t.Errorf("expected https:// registry URL, got %s", result.RegistryURL)
	}
}
