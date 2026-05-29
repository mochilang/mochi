package publish

import (
	"strings"
	"testing"
)

func validTarget() PublishTarget {
	return PublishTarget{
		Distribution: "mochi-sample",
		Version:      "0.1.0",
		SdistPath:    "/tmp/mochi-sample-0.1.0.tar.gz",
		SdistSHA256:  "abc",
		SdistSize:    100,
		WheelPath:    "/tmp/mochi-sample-0.1.0-py3-none-any.whl",
		WheelSHA256:  "def",
		WheelSize:    200,
	}
}

func TestRegistryKindString(t *testing.T) {
	for kind, want := range map[RegistryKind]string{
		RegistryPyPI:     "pypi",
		RegistryTestPyPI: "testpypi",
		RegistryKind(99): "unknown",
	} {
		if got := kind.String(); got != want {
			t.Fatalf("RegistryKind(%d) = %q, want %q", kind, got, want)
		}
	}
}

func TestRegistryKindURL(t *testing.T) {
	if got := RegistryPyPI.URL(); got != "https://pypi.org" {
		t.Fatalf("PyPI URL = %q", got)
	}
	if got := RegistryTestPyPI.URL(); got != "https://test.pypi.org" {
		t.Fatalf("TestPyPI URL = %q", got)
	}
	if got := RegistryKind(99).URL(); got != "" {
		t.Fatalf("unknown URL = %q", got)
	}
}

func TestRegistryKindOIDCAudience(t *testing.T) {
	if got := RegistryPyPI.OIDCAudience(); got != "pypi" {
		t.Fatalf("PyPI audience = %q", got)
	}
	if got := RegistryTestPyPI.OIDCAudience(); got != "testpypi" {
		t.Fatalf("TestPyPI audience = %q", got)
	}
	if got := RegistryKind(99).OIDCAudience(); got != "" {
		t.Fatalf("unknown audience = %q", got)
	}
}

func TestValidateRejectsUnknownRegistry(t *testing.T) {
	req := PublishRequest{Registry: RegistryKind(99), Targets: []PublishTarget{validTarget()}, DryRun: true}
	if err := req.Validate(); err == nil || !strings.Contains(err.Error(), "Registry") {
		t.Fatalf("expected Registry error, got %v", err)
	}
}

func TestValidateRejectsEmptyTargets(t *testing.T) {
	req := PublishRequest{Registry: RegistryPyPI, DryRun: true}
	if err := req.Validate(); err == nil || !strings.Contains(err.Error(), "Targets") {
		t.Fatalf("expected Targets error, got %v", err)
	}
}

func TestValidateRejectsBadTargetFields(t *testing.T) {
	cases := map[string]func(*PublishTarget){
		"Distribution": func(t *PublishTarget) { t.Distribution = "" },
		"Version":      func(t *PublishTarget) { t.Version = "" },
		"artifact path": func(t *PublishTarget) {
			t.SdistPath = ""
		},
		"sha256": func(t *PublishTarget) {
			t.SdistSHA256 = ""
		},
		"zero size": func(t *PublishTarget) {
			t.SdistSize = 0
		},
		"tar.gz": func(t *PublishTarget) {
			t.SdistPath = "/tmp/x.zip"
		},
		"whl": func(t *PublishTarget) {
			t.WheelPath = "/tmp/x.zip"
		},
	}
	for label, mutate := range cases {
		tg := validTarget()
		mutate(&tg)
		req := PublishRequest{Registry: RegistryPyPI, Targets: []PublishTarget{tg}, DryRun: true}
		if err := req.Validate(); err == nil || !strings.Contains(err.Error(), label) {
			t.Fatalf("%s: expected error containing %q, got %v", label, label, err)
		}
	}
}

func TestValidateRequiresOIDCWhenNotDryRun(t *testing.T) {
	req := PublishRequest{Registry: RegistryPyPI, Targets: []PublishTarget{validTarget()}, DryRun: false}
	if err := req.Validate(); err == nil || !strings.Contains(err.Error(), "OIDCProvider") {
		t.Fatalf("expected OIDCProvider error, got %v", err)
	}
}

func TestValidateDryRunAllowsNoOIDC(t *testing.T) {
	req := PublishRequest{Registry: RegistryPyPI, Targets: []PublishTarget{validTarget()}, DryRun: true}
	if err := req.Validate(); err != nil {
		t.Fatalf("dry-run without OIDC must validate: %v", err)
	}
}
