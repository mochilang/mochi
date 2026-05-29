package publish

import (
	"archive/zip"
	"bytes"
	"encoding/json"
	"fmt"
	"io"
	"net/http"
	"net/http/httptest"
	"os"
	"path/filepath"
	"strings"
	"testing"
	"time"
)

// ─── validateBundleSpec ───────────────────────────────────────────────────────

func TestValidateBundleSpec_OK(t *testing.T) {
	spec := BundleSpec{
		GroupID: "com.example", ArtifactID: "mylib", Version: "1.0",
		JARPath: "/some/mylib.jar", POMPath: "/some/mylib.pom",
	}
	if err := validateBundleSpec(spec); err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
}

func TestValidateBundleSpec_Missing(t *testing.T) {
	cases := []BundleSpec{
		{ArtifactID: "a", Version: "1", JARPath: "j", POMPath: "p"},
		{GroupID: "g", Version: "1", JARPath: "j", POMPath: "p"},
		{GroupID: "g", ArtifactID: "a", JARPath: "j", POMPath: "p"},
		{GroupID: "g", ArtifactID: "a", Version: "1", POMPath: "p"},
		{GroupID: "g", ArtifactID: "a", Version: "1", JARPath: "j"},
	}
	for _, spec := range cases {
		if err := validateBundleSpec(spec); err == nil {
			t.Errorf("want error for spec %+v, got nil", spec)
		}
	}
}

// ─── checksum helpers ─────────────────────────────────────────────────────────

func TestSHA1Hex(t *testing.T) {
	// SHA-1 of empty string is well-known.
	got := sha1hex([]byte{})
	if got != "da39a3ee5e6b4b0d3255bfef95601890afd80709" {
		t.Errorf("sha1hex of empty: %q", got)
	}
}

func TestMD5Hex(t *testing.T) {
	got := md5hex([]byte{})
	if got != "d41d8cd98f00b204e9800998ecf8427e" {
		t.Errorf("md5hex of empty: %q", got)
	}
}

// ─── BuildBundle ──────────────────────────────────────────────────────────────

// buildFakeArtifacts writes minimal JAR + POM files and returns their paths.
func buildFakeArtifacts(t *testing.T, dir string) (jarPath, pomPath string) {
	t.Helper()
	// minimal valid ZIP (empty JAR)
	var buf bytes.Buffer
	zw := zip.NewWriter(&buf)
	_ = zw.Close()
	jarPath = filepath.Join(dir, "mylib-1.0.jar")
	if err := os.WriteFile(jarPath, buf.Bytes(), 0o644); err != nil {
		t.Fatalf("write jar: %v", err)
	}
	pomPath = filepath.Join(dir, "mylib-1.0.pom")
	pom := `<?xml version="1.0"?><project><groupId>com.example</groupId><artifactId>mylib</artifactId><version>1.0</version></project>`
	if err := os.WriteFile(pomPath, []byte(pom), 0o644); err != nil {
		t.Fatalf("write pom: %v", err)
	}
	return
}

func TestBuildBundle_Basic(t *testing.T) {
	dir := t.TempDir()
	jarPath, pomPath := buildFakeArtifacts(t, dir)

	spec := BundleSpec{
		GroupID: "com.example", ArtifactID: "mylib", Version: "1.0",
		JARPath: jarPath, POMPath: pomPath,
	}
	res, err := BuildBundle(spec, dir)
	if err != nil {
		t.Fatalf("BuildBundle: %v", err)
	}
	if _, err := os.Stat(res.BundlePath); err != nil {
		t.Fatalf("bundle not created: %v", err)
	}

	// Validate the bundle passes DryRun.
	if err := DryRun(res.BundlePath, "com.example", "mylib", "1.0"); err != nil {
		t.Errorf("DryRun failed: %v", err)
	}
}

func TestBuildBundle_ContainsRequiredEntries(t *testing.T) {
	dir := t.TempDir()
	jarPath, pomPath := buildFakeArtifacts(t, dir)
	spec := BundleSpec{
		GroupID: "org.test", ArtifactID: "cool-lib", Version: "2.3.4",
		JARPath: jarPath, POMPath: pomPath,
	}
	// Override artifact name in spec but use the files we already wrote.
	res, err := BuildBundle(spec, dir)
	if err != nil {
		t.Fatalf("BuildBundle: %v", err)
	}

	r, err := zip.OpenReader(res.BundlePath)
	if err != nil {
		t.Fatalf("open zip: %v", err)
	}
	defer r.Close()

	want := map[string]bool{
		"org/test/cool-lib/2.3.4/cool-lib-2.3.4.jar":      true,
		"org/test/cool-lib/2.3.4/cool-lib-2.3.4.pom":      true,
		"org/test/cool-lib/2.3.4/cool-lib-2.3.4.jar.sha1": true,
		"org/test/cool-lib/2.3.4/cool-lib-2.3.4.pom.sha1": true,
		"org/test/cool-lib/2.3.4/cool-lib-2.3.4.jar.md5":  true,
		"org/test/cool-lib/2.3.4/cool-lib-2.3.4.pom.md5":  true,
	}
	for _, f := range r.File {
		delete(want, f.Name)
	}
	for missing := range want {
		t.Errorf("bundle missing required entry: %s", missing)
	}
}

func TestBuildBundle_GroupPathConversion(t *testing.T) {
	dir := t.TempDir()
	jarPath, pomPath := buildFakeArtifacts(t, dir)
	spec := BundleSpec{
		GroupID: "com.example.deep.package", ArtifactID: "lib", Version: "0.1",
		JARPath: jarPath, POMPath: pomPath,
	}
	res, err := BuildBundle(spec, dir)
	if err != nil {
		t.Fatalf("BuildBundle: %v", err)
	}
	r, _ := zip.OpenReader(res.BundlePath)
	defer r.Close()
	for _, f := range r.File {
		if strings.Contains(f.Name, "com.example") {
			t.Errorf("group dots not converted to slashes: %s", f.Name)
		}
		if strings.HasPrefix(f.Name, "com/example/deep/package/") {
			// Good.
			return
		}
	}
	t.Error("expected com/example/deep/package/ prefix in bundle entry")
}

// ─── DryRun ───────────────────────────────────────────────────────────────────

func TestDryRun_MissingEntry(t *testing.T) {
	dir := t.TempDir()
	// Create a bundle that's missing required entries.
	bundlePath := filepath.Join(dir, "bad.zip")
	f, _ := os.Create(bundlePath)
	zw := zip.NewWriter(f)
	// Add only the JAR, not the POM or checksums.
	w, _ := zw.Create("com/example/lib/1.0/lib-1.0.jar")
	_, _ = w.Write([]byte("fake jar"))
	_ = zw.Close()
	f.Close()

	err := DryRun(bundlePath, "com.example", "lib", "1.0")
	if err == nil {
		t.Fatal("want error for missing entries, got nil")
	}
	if !strings.Contains(err.Error(), "missing") {
		t.Errorf("error should mention 'missing': %v", err)
	}
}

func TestDryRun_ChecksumMismatch(t *testing.T) {
	dir := t.TempDir()
	bundlePath := filepath.Join(dir, "mismatch.zip")
	f, _ := os.Create(bundlePath)
	zw := zip.NewWriter(f)
	jarData := []byte("fake jar bytes")
	pomData := []byte("fake pom")
	addZipEntry := func(name string, data []byte) {
		w, _ := zw.Create(name)
		_, _ = w.Write(data)
	}
	addZipEntry("com/example/lib/1.0/lib-1.0.jar", jarData)
	addZipEntry("com/example/lib/1.0/lib-1.0.jar.sha1", []byte("wronghash"))
	addZipEntry("com/example/lib/1.0/lib-1.0.pom", pomData)
	addZipEntry("com/example/lib/1.0/lib-1.0.pom.sha1", []byte(sha1hex(pomData)))
	_ = zw.Close()
	f.Close()

	err := DryRun(bundlePath, "com.example", "lib", "1.0")
	if err == nil {
		t.Fatal("want checksum error, got nil")
	}
	if !strings.Contains(err.Error(), "SHA-1 mismatch") {
		t.Errorf("expected SHA-1 mismatch error: %v", err)
	}
}

// ─── Client.Upload (dry-run mode) ────────────────────────────────────────────

func TestClient_Upload_DryRun(t *testing.T) {
	c := &Client{Token: "tok"}
	id, err := c.Upload("/any/path.zip", true, "test-bundle")
	if err != nil {
		t.Fatalf("dry-run upload: %v", err)
	}
	if id != "dry-run-deployment-id" {
		t.Errorf("unexpected id: %q", id)
	}
}

// ─── Client.Upload (mock server) ─────────────────────────────────────────────

func TestClient_Upload_MockServer(t *testing.T) {
	dir := t.TempDir()
	// Build a minimal bundle to upload.
	jarPath, pomPath := buildFakeArtifacts(t, dir)
	spec := BundleSpec{
		GroupID: "com.example", ArtifactID: "mylib", Version: "1.0",
		JARPath: jarPath, POMPath: pomPath,
	}
	bundle, err := BuildBundle(spec, dir)
	if err != nil {
		t.Fatalf("BuildBundle: %v", err)
	}

	var receivedAuth string
	srv := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		switch {
		case strings.HasPrefix(r.URL.Path, "/api/v1/publisher/upload"):
			receivedAuth = r.Header.Get("Authorization")
			w.WriteHeader(http.StatusCreated)
			fmt.Fprint(w, "test-deployment-123")
		case strings.HasPrefix(r.URL.Path, "/api/v1/publisher/status"):
			w.WriteHeader(http.StatusOK)
			_ = json.NewEncoder(w).Encode(statusResponse{DeploymentState: DeploymentPublished})
		default:
			w.WriteHeader(http.StatusNotFound)
		}
	}))
	defer srv.Close()

	c := &Client{
		BaseURL:      srv.URL,
		Token:        "bearer-token",
		HTTPClient:   srv.Client(),
		PollInterval: 10 * time.Millisecond,
		PollTimeout:  5 * time.Second,
	}

	deployID, err := c.Upload(bundle.BundlePath, false, "mylib-bundle")
	if err != nil {
		t.Fatalf("Upload: %v", err)
	}
	if deployID != "test-deployment-123" {
		t.Errorf("deploymentID = %q", deployID)
	}
	if receivedAuth != "Bearer bearer-token" {
		t.Errorf("Authorization = %q; want %q", receivedAuth, "Bearer bearer-token")
	}

	state, err := c.PollUntilPublished(deployID)
	if err != nil {
		t.Fatalf("PollUntilPublished: %v", err)
	}
	if state != DeploymentPublished {
		t.Errorf("state = %q; want PUBLISHED", state)
	}
}

func TestClient_PollUntilPublished_Failure(t *testing.T) {
	srv := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		w.WriteHeader(http.StatusOK)
		_ = json.NewEncoder(w).Encode(statusResponse{DeploymentState: DeploymentFailed})
	}))
	defer srv.Close()

	c := &Client{
		BaseURL:      srv.URL,
		Token:        "tok",
		HTTPClient:   srv.Client(),
		PollInterval: 10 * time.Millisecond,
		PollTimeout:  time.Second,
	}
	_, err := c.PollUntilPublished("some-id")
	if err == nil {
		t.Fatal("want error for FAILED deployment")
	}
}

// addZipEntry is a test helper for building custom ZIPs.
func addZipEntry(zw *zip.Writer, name string, data []byte) {
	w, _ := zw.Create(name)
	_, _ = io.Copy(w, bytes.NewReader(data))
}

