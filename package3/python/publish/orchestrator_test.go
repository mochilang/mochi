package publish

import (
	"context"
	"errors"
	"strings"
	"testing"
)

type fakeOIDC struct {
	tok string
	err error
}

func (f fakeOIDC) Token(string) (string, error) {
	if f.err != nil {
		return "", f.err
	}
	return f.tok, nil
}

type fakeUploader struct {
	calls   []UploadCall
	err     error
	results []UploadResult
}

func (f *fakeUploader) Upload(_ context.Context, c UploadCall) (UploadResult, error) {
	f.calls = append(f.calls, c)
	if f.err != nil {
		return UploadResult{}, f.err
	}
	if len(f.results) == 0 {
		return UploadResult{URL: "https://example.test/" + c.Target.Distribution}, nil
	}
	r := f.results[0]
	f.results = f.results[1:]
	return r, nil
}

func TestOrchestratorRejectsInvalidRequest(t *testing.T) {
	o := &Orchestrator{}
	if _, err := o.Publish(context.Background(), PublishRequest{}); err == nil {
		t.Fatal("expected validate error")
	}
}

func TestOrchestratorDryRunSkipsOIDC(t *testing.T) {
	o := &Orchestrator{}
	rec := &RecordingUploader{}
	res, err := o.Publish(context.Background(), PublishRequest{
		Registry: RegistryTestPyPI,
		Targets:  []PublishTarget{validTarget()},
		DryRun:   true,
		Uploader: rec,
	})
	if err != nil {
		t.Fatalf("Publish: %v", err)
	}
	if !res.DryRun {
		t.Fatal("result must mark DryRun")
	}
	if len(rec.Calls) != 1 || rec.Calls[0].Token != "" {
		t.Fatalf("dry-run must not pass a token: %+v", rec.Calls)
	}
	if len(res.Attestation) == 0 {
		t.Fatal("attestation must still be built in dry-run")
	}
	if len(res.Artifacts) != 1 || !res.Artifacts[0].Skipped {
		t.Fatalf("artifact not marked Skipped: %+v", res.Artifacts)
	}
}

func TestOrchestratorDryRunUsesRecordingUploaderByDefault(t *testing.T) {
	o := &Orchestrator{}
	res, err := o.Publish(context.Background(), PublishRequest{
		Registry: RegistryPyPI,
		Targets:  []PublishTarget{validTarget()},
		DryRun:   true,
	})
	if err != nil {
		t.Fatalf("Publish: %v", err)
	}
	if !res.Artifacts[0].Skipped {
		t.Fatal("default dry-run uploader must Skip")
	}
}

func TestOrchestratorPropagatesOIDCError(t *testing.T) {
	o := &Orchestrator{}
	_, err := o.Publish(context.Background(), PublishRequest{
		Registry:     RegistryPyPI,
		Targets:      []PublishTarget{validTarget()},
		OIDCProvider: fakeOIDC{err: errors.New("no runner")},
	})
	if err == nil || !strings.Contains(err.Error(), "no runner") {
		t.Fatalf("expected OIDC error, got %v", err)
	}
}

func TestOrchestratorAttestationContainsAllTargets(t *testing.T) {
	o := &Orchestrator{}
	tgs := []PublishTarget{validTarget(), {
		Distribution: "mochi-other",
		Version:      "0.0.1",
		SdistPath:    "/x.tar.gz",
		SdistSHA256:  "x",
		SdistSize:    1,
		WheelPath:    "/x.whl",
		WheelSHA256:  "y",
		WheelSize:    2,
	}}
	res, err := o.Publish(context.Background(), PublishRequest{
		Registry: RegistryPyPI,
		Targets:  tgs,
		DryRun:   true,
	})
	if err != nil {
		t.Fatalf("Publish: %v", err)
	}
	body := string(res.Attestation)
	for _, want := range []string{"mochi-sample-0.1.0.tar.gz", "mochi-other-0.0.1-py3-none-any.whl"} {
		if !strings.Contains(body, want) {
			t.Fatalf("attestation missing %q\n%s", want, body)
		}
	}
}

func TestOrchestratorRejectsUploadFailure(t *testing.T) {
	o := &Orchestrator{}
	u := &fakeUploader{err: errors.New("network down")}
	_, err := o.Publish(context.Background(), PublishRequest{
		Registry: RegistryPyPI,
		Targets:  []PublishTarget{validTarget()},
		DryRun:   true,
		Uploader: u,
	})
	if err == nil || !strings.Contains(err.Error(), "network down") {
		t.Fatalf("expected uploader error, got %v", err)
	}
}

func TestOrchestratorPropagatesBuilderID(t *testing.T) {
	o := &Orchestrator{}
	res, err := o.Publish(context.Background(), PublishRequest{
		Registry: RegistryPyPI,
		Targets:  []PublishTarget{validTarget()},
		DryRun:   true,
		Builder:  "https://my.builder",
	})
	if err != nil {
		t.Fatalf("Publish: %v", err)
	}
	if !strings.Contains(string(res.Attestation), "https://my.builder") {
		t.Fatalf("builder id not propagated: %s", res.Attestation)
	}
}
