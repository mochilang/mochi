package publish

import (
	"context"
	"encoding/json"
	"net/http"
	"net/http/httptest"
	"slices"
	"strings"
	"testing"
	"time"
)

func defaultConfig() Config {
	return Config{
		PackagistName:    "acme/my-lib",
		Version:          "1.0.0",
		RepoURL:          "https://github.com/acme/my-lib",
		PackagistUsername: "acme-user",
		PackagistToken:   "token123",
		Remote:           "origin",
	}
}

func TestValidateOK(t *testing.T) {
	if err := Validate(defaultConfig()); err != nil {
		t.Errorf("expected no error for valid config; got %v", err)
	}
}

func TestValidateMissingName(t *testing.T) {
	cfg := defaultConfig()
	cfg.PackagistName = ""
	if err := Validate(cfg); err == nil {
		t.Error("expected error for missing PackagistName")
	}
}

func TestValidateMissingVersion(t *testing.T) {
	cfg := defaultConfig()
	cfg.Version = ""
	if err := Validate(cfg); err == nil {
		t.Error("expected error for missing Version")
	}
}

func TestValidateMissingRepoURL(t *testing.T) {
	cfg := defaultConfig()
	cfg.RepoURL = ""
	if err := Validate(cfg); err == nil {
		t.Error("expected error for missing RepoURL")
	}
}

func TestValidateVPrefix(t *testing.T) {
	cfg := defaultConfig()
	cfg.Version = "v1.0.0"
	if err := Validate(cfg); err == nil {
		t.Error("expected error for v-prefixed version")
	}
}

func TestValidateInvalidName(t *testing.T) {
	cfg := defaultConfig()
	cfg.PackagistName = "nodash"
	if err := Validate(cfg); err == nil {
		t.Error("expected error for package name without slash")
	}
}

func TestPlanSteps(t *testing.T) {
	steps := Plan(defaultConfig())
	if len(steps) < 4 {
		t.Errorf("expected at least 4 steps; got %d: %v", len(steps), steps)
	}
	names := make([]string, len(steps))
	for i, s := range steps {
		names[i] = s.Name
	}
	for _, required := range []string{"validate", "tag", "push-tag", "update-api"} {
		if !slices.Contains(names, required) {
			t.Errorf("expected step %q; got %v", required, names)
		}
	}
}

func TestPlanNoVerifySkipsVerify(t *testing.T) {
	cfg := defaultConfig()
	cfg.NoVerify = true
	steps := Plan(cfg)
	for _, s := range steps {
		if s.Name == "verify" {
			t.Error("NoVerify=true should omit verify step")
		}
	}
}

func TestPlanWithVerify(t *testing.T) {
	cfg := defaultConfig()
	cfg.NoVerify = false
	steps := Plan(cfg)
	found := false
	for _, s := range steps {
		if s.Name == "verify" {
			found = true
		}
	}
	if !found {
		t.Error("expected verify step when NoVerify=false")
	}
}

func TestPingUpdateAPISuccess(t *testing.T) {
	var capturedBody []byte
	srv := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		if r.Method != http.MethodPost {
			t.Errorf("expected POST; got %s", r.Method)
		}
		var err error
		capturedBody, err = stdioReadAll(r.Body)
		if err != nil {
			t.Errorf("read body: %v", err)
		}
		w.WriteHeader(http.StatusOK)
	}))
	defer srv.Close()

	cfg := defaultConfig()
	cfg.PackagistBaseURL = srv.URL

	if err := PingUpdateAPI(context.Background(), cfg); err != nil {
		t.Errorf("unexpected error: %v", err)
	}

	// Verify the request body contains the repo URL.
	var body map[string]any
	if err := json.Unmarshal(capturedBody, &body); err != nil {
		t.Fatalf("invalid request body: %v", err)
	}
	repo, ok := body["repository"].(map[string]any)
	if !ok {
		t.Fatalf("expected repository object; got %v", body)
	}
	if repo["url"] != cfg.RepoURL {
		t.Errorf("repo url = %v; want %v", repo["url"], cfg.RepoURL)
	}
}

func TestPingUpdateAPIError(t *testing.T) {
	srv := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		w.WriteHeader(http.StatusUnauthorized)
		w.Write([]byte("bad token"))
	}))
	defer srv.Close()

	cfg := defaultConfig()
	cfg.PackagistBaseURL = srv.URL

	err := PingUpdateAPI(context.Background(), cfg)
	if err == nil {
		t.Error("expected error for 401 response")
	}
	if !strings.Contains(err.Error(), "401") {
		t.Errorf("error should mention 401; got %v", err)
	}
}

func TestWaitForIndexSuccess(t *testing.T) {
	cfg := defaultConfig()
	srv := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		json.NewEncoder(w).Encode(map[string]any{
			"package": map[string]any{
				"versions": map[string]any{
					"1.0.0": map[string]any{"name": "acme/my-lib"},
				},
			},
		})
	}))
	defer srv.Close()

	cfg.PackagistBaseURL = srv.URL
	if err := WaitForIndex(context.Background(), cfg, 5*time.Second); err != nil {
		t.Errorf("expected success; got %v", err)
	}
}

func TestWaitForIndexTimeout(t *testing.T) {
	srv := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		// Return package without the requested version.
		json.NewEncoder(w).Encode(map[string]any{
			"package": map[string]any{
				"versions": map[string]any{},
			},
		})
	}))
	defer srv.Close()

	cfg := defaultConfig()
	cfg.PackagistBaseURL = srv.URL
	err := WaitForIndex(context.Background(), cfg, 50*time.Millisecond)
	if err == nil {
		t.Error("expected timeout error")
	}
	if !strings.Contains(err.Error(), "timed out") {
		t.Errorf("error should mention timed out; got %v", err)
	}
}

// stdioReadAll reads all bytes from a reader.
func stdioReadAll(r interface{ Read(p []byte) (n int, err error) }) ([]byte, error) {
	var buf []byte
	tmp := make([]byte, 512)
	for {
		n, err := r.Read(tmp)
		buf = append(buf, tmp[:n]...)
		if err != nil {
			break
		}
	}
	return buf, nil
}
