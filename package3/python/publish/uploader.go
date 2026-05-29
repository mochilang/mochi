package publish

import (
	"context"
	"fmt"
	"os/exec"
	"strings"
)

// UploadCall is the per-target invocation the orchestrator hands to an
// Uploader. The Token is the short-lived PyPI API token returned from the
// mint endpoint; AttestationJSON is the canonical-JSON in-toto statement
// the orchestrator built (the Sigstore signing pass that wraps it in an
// in-toto envelope is owned by the uploader because uv runs the actual
// `sigstore-python` invocation inline).
type UploadCall struct {
	Target          PublishTarget
	Registry        RegistryKind
	Token           string
	AttestationJSON []byte
}

// Uploader drives the actual artifact upload + Sigstore signing. The default
// implementation shells to `uv publish`; tests use RecordingUploader to
// capture calls without touching the network.
type Uploader interface {
	Upload(ctx context.Context, call UploadCall) (UploadResult, error)
}

// UploadResult captures the outcome of a single upload. URL is the
// PyPI artifact URL (e.g. https://pypi.org/project/mochi-httpx/0.1.0/);
// AttestationURL is the PEP 740 attestation pointer if the registry
// returned one, empty otherwise.
type UploadResult struct {
	URL            string
	AttestationURL string
	Skipped        bool // true when DryRun bypassed the upload.
}

// UVUploader shells to `uv publish --trusted-publishing always` for each
// target. UV reads the API token from the UV_PUBLISH_TOKEN env var and
// the artifacts from the positional path args.
type UVUploader struct {
	// Binary overrides the uv binary path. Empty means "uv" on PATH.
	Binary string
	// Run overrides exec.CommandContext; nil means real subprocess.
	Run func(ctx context.Context, env []string, name string, args ...string) ([]byte, error)
}

// Upload invokes `uv publish` for the target. Returns the rendered
// upload URL on success.
func (u UVUploader) Upload(ctx context.Context, call UploadCall) (UploadResult, error) {
	bin := u.Binary
	if bin == "" {
		bin = "uv"
	}
	args := []string{
		"publish",
		"--trusted-publishing", "always",
		"--publish-url", call.Registry.URL() + "/legacy/",
		call.Target.SdistPath,
		call.Target.WheelPath,
	}
	env := []string{"UV_PUBLISH_TOKEN=" + call.Token}
	run := u.Run
	if run == nil {
		run = defaultRun
	}
	out, err := run(ctx, env, bin, args...)
	if err != nil {
		return UploadResult{}, fmt.Errorf("publish: uv publish: %w: %s", err, strings.TrimSpace(string(out)))
	}
	return UploadResult{
		URL: fmt.Sprintf("%s/project/%s/%s/", call.Registry.URL(), call.Target.Distribution, call.Target.Version),
	}, nil
}

func defaultRun(ctx context.Context, env []string, name string, args ...string) ([]byte, error) {
	cmd := exec.CommandContext(ctx, name, args...)
	cmd.Env = append(cmd.Environ(), env...)
	return cmd.CombinedOutput()
}

// RecordingUploader captures every Upload call without executing anything.
// Tests assert against Calls; the orchestrator uses it as the default when
// DryRun is set and no uploader is supplied.
type RecordingUploader struct {
	Calls []UploadCall
}

// Upload appends the call and returns a synthetic "would-upload" URL.
func (r *RecordingUploader) Upload(_ context.Context, call UploadCall) (UploadResult, error) {
	r.Calls = append(r.Calls, call)
	return UploadResult{
		URL:     fmt.Sprintf("%s/project/%s/%s/", call.Registry.URL(), call.Target.Distribution, call.Target.Version),
		Skipped: true,
	}, nil
}
