package publish

import (
	"context"
	"errors"
	"strings"
	"testing"
)

func TestRecordingUploaderCaptures(t *testing.T) {
	u := &RecordingUploader{}
	call := UploadCall{
		Target:   validTarget(),
		Registry: RegistryPyPI,
		Token:    "tok",
	}
	res, err := u.Upload(context.Background(), call)
	if err != nil {
		t.Fatalf("Upload: %v", err)
	}
	if !res.Skipped {
		t.Fatal("RecordingUploader must set Skipped")
	}
	if res.URL != "https://pypi.org/project/mochi-sample/0.1.0/" {
		t.Fatalf("URL = %q", res.URL)
	}
	if len(u.Calls) != 1 {
		t.Fatalf("Calls = %d", len(u.Calls))
	}
}

func TestUVUploaderInvokesUVPublish(t *testing.T) {
	var seenName string
	var seenArgs []string
	var seenEnv []string
	u := UVUploader{Run: func(_ context.Context, env []string, name string, args ...string) ([]byte, error) {
		seenName = name
		seenArgs = args
		seenEnv = env
		return []byte("ok"), nil
	}}
	call := UploadCall{
		Target:   validTarget(),
		Registry: RegistryTestPyPI,
		Token:    "minted-tok",
	}
	res, err := u.Upload(context.Background(), call)
	if err != nil {
		t.Fatalf("Upload: %v", err)
	}
	if seenName != "uv" {
		t.Fatalf("uv binary = %q", seenName)
	}
	if seenArgs[0] != "publish" {
		t.Fatalf("first arg %q", seenArgs[0])
	}
	joined := strings.Join(seenArgs, " ")
	for _, want := range []string{"--trusted-publishing always", "--publish-url https://test.pypi.org/legacy/", "/tmp/mochi-sample-0.1.0.tar.gz", "/tmp/mochi-sample-0.1.0-py3-none-any.whl"} {
		if !strings.Contains(joined, want) {
			t.Fatalf("args missing %q: %s", want, joined)
		}
	}
	if len(seenEnv) != 1 || !strings.HasPrefix(seenEnv[0], "UV_PUBLISH_TOKEN=minted-tok") {
		t.Fatalf("env = %v", seenEnv)
	}
	if res.URL != "https://test.pypi.org/project/mochi-sample/0.1.0/" {
		t.Fatalf("URL = %q", res.URL)
	}
}

func TestUVUploaderSurfacesSubprocessError(t *testing.T) {
	u := UVUploader{Run: func(context.Context, []string, string, ...string) ([]byte, error) {
		return []byte("upload failed: 403"), errors.New("exit 1")
	}}
	call := UploadCall{Target: validTarget(), Registry: RegistryPyPI, Token: "tok"}
	if _, err := u.Upload(context.Background(), call); err == nil || !strings.Contains(err.Error(), "uv publish") {
		t.Fatalf("expected uv publish error, got %v", err)
	}
}

func TestUVUploaderHonoursBinaryOverride(t *testing.T) {
	var seen string
	u := UVUploader{Binary: "/opt/bin/uv", Run: func(_ context.Context, _ []string, name string, _ ...string) ([]byte, error) {
		seen = name
		return nil, nil
	}}
	if _, err := u.Upload(context.Background(), UploadCall{Target: validTarget(), Registry: RegistryPyPI, Token: "tok"}); err != nil {
		t.Fatalf("Upload: %v", err)
	}
	if seen != "/opt/bin/uv" {
		t.Fatalf("binary override = %q", seen)
	}
}
