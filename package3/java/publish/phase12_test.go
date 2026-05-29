package publish_test

import (
	"archive/zip"
	"bytes"
	"encoding/json"
	"net/http"
	"net/http/httptest"
	"os"
	"path/filepath"
	"strings"
	"testing"
	"time"

	"mochi/package3/java/publish"
)

func buildFakeArtifacts(t *testing.T, dir string) (jarPath, pomPath string) {
	t.Helper()
	var buf bytes.Buffer
	zw := zip.NewWriter(&buf)
	_ = zw.Close()
	jarPath = filepath.Join(dir, "mylib-jni-wrapper-1.0.0.jar")
	if err := os.WriteFile(jarPath, buf.Bytes(), 0o644); err != nil {
		t.Fatalf("write jar: %v", err)
	}
	pomPath = filepath.Join(dir, "mylib-jni-wrapper-1.0.0.pom")
	pom := `<?xml version="1.0"?><project><groupId>dev.mochi.java-bridge</groupId><artifactId>mylib-jni-wrapper</artifactId><version>1.0.0</version></project>`
	if err := os.WriteFile(pomPath, []byte(pom), 0o644); err != nil {
		t.Fatalf("write pom: %v", err)
	}
	return
}

func TestBuildBundle_ValidSpec(t *testing.T) {
	dir := t.TempDir()
	jar, pom := buildFakeArtifacts(t, dir)
	spec := publish.BundleSpec{
		GroupID:    "dev.mochi.java-bridge",
		ArtifactID: "mylib-jni-wrapper",
		Version:    "1.0.0",
		JARPath:    jar,
		POMPath:    pom,
	}
	res, err := publish.BuildBundle(spec, filepath.Join(dir, "out"))
	if err != nil {
		t.Fatalf("BuildBundle: %v", err)
	}
	if _, err := os.Stat(res.BundlePath); err != nil {
		t.Errorf("bundle not on disk: %v", err)
	}
}

func TestBuildBundle_MissingGroup(t *testing.T) {
	_, err := publish.BuildBundle(publish.BundleSpec{
		ArtifactID: "mylib", Version: "1.0", JARPath: "j", POMPath: "p",
	}, t.TempDir())
	if err == nil {
		t.Error("expected error for missing GroupID")
	}
}

func TestDryRun_ValidBundle(t *testing.T) {
	dir := t.TempDir()
	jar, pom := buildFakeArtifacts(t, dir)
	spec := publish.BundleSpec{
		GroupID:    "dev.mochi.java-bridge",
		ArtifactID: "mylib-jni-wrapper",
		Version:    "1.0.0",
		JARPath:    jar,
		POMPath:    pom,
	}
	res, err := publish.BuildBundle(spec, filepath.Join(dir, "out"))
	if err != nil {
		t.Fatalf("BuildBundle: %v", err)
	}
	if err := publish.DryRun(res.BundlePath, spec.GroupID, spec.ArtifactID, spec.Version); err != nil {
		t.Errorf("DryRun: %v", err)
	}
}

func TestClientUpload_DryRun(t *testing.T) {
	client := &publish.Client{Token: "test-token"}
	id, err := client.Upload("irrelevant.zip", true, "test-bundle")
	if err != nil {
		t.Fatalf("Upload dry-run: %v", err)
	}
	if id == "" {
		t.Error("expected non-empty deployment ID for dry-run")
	}
}

func TestClientUpload_MockServer(t *testing.T) {
	srv := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		if r.Method != http.MethodPost {
			t.Errorf("unexpected method %q", r.Method)
		}
		w.WriteHeader(http.StatusCreated)
		w.Write([]byte("deploy-abc-123"))
	}))
	defer srv.Close()

	dir := t.TempDir()
	jar, pom := buildFakeArtifacts(t, dir)
	spec := publish.BundleSpec{
		GroupID:    "dev.mochi.java-bridge",
		ArtifactID: "mylib-jni-wrapper",
		Version:    "1.0.0",
		JARPath:    jar,
		POMPath:    pom,
	}
	res, err := publish.BuildBundle(spec, filepath.Join(dir, "out"))
	if err != nil {
		t.Fatalf("BuildBundle: %v", err)
	}

	client := &publish.Client{
		BaseURL: srv.URL,
		Token:   "test-token",
	}
	id, err := client.Upload(res.BundlePath, false, "test-bundle")
	if err != nil {
		t.Fatalf("Upload: %v", err)
	}
	if id != "deploy-abc-123" {
		t.Errorf("id = %q; want deploy-abc-123", id)
	}
}

func TestClientPollUntilPublished_Mock(t *testing.T) {
	calls := 0
	srv := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		calls++
		state := "PENDING"
		if calls >= 2 {
			state = "PUBLISHED"
		}
		w.Header().Set("Content-Type", "application/json")
		json.NewEncoder(w).Encode(map[string]string{"deploymentState": state})
	}))
	defer srv.Close()

	client := &publish.Client{
		BaseURL:      srv.URL,
		Token:        "t",
		PollInterval: 1 * time.Millisecond,
		PollTimeout:  5 * time.Second,
	}
	state, err := client.PollUntilPublished("dep-xyz")
	if err != nil {
		t.Fatalf("PollUntilPublished: %v", err)
	}
	if state != publish.DeploymentPublished {
		t.Errorf("state = %q; want PUBLISHED", state)
	}
}

func TestDeploymentStateConstants(t *testing.T) {
	for _, s := range []publish.DeploymentState{
		publish.DeploymentPending,
		publish.DeploymentValidated,
		publish.DeploymentPublished,
		publish.DeploymentFailed,
	} {
		if strings.TrimSpace(string(s)) == "" {
			t.Errorf("empty deployment state constant: %q", s)
		}
	}
}
