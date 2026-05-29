package publish

import (
	"fmt"
	"strings"
)

// RegistryKind selects between production PyPI and the TestPyPI staging
// registry. Both share the trusted-publishing OIDC mint endpoint shape;
// only the host and the OIDC audience differ.
type RegistryKind int

const (
	// RegistryPyPI is the production registry at https://pypi.org/.
	RegistryPyPI RegistryKind = iota
	// RegistryTestPyPI is the staging registry at https://test.pypi.org/.
	RegistryTestPyPI
)

// String renders the registry kind as a stable token.
func (r RegistryKind) String() string {
	switch r {
	case RegistryPyPI:
		return "pypi"
	case RegistryTestPyPI:
		return "testpypi"
	}
	return "unknown"
}

// URL returns the canonical https:// host for the registry.
func (r RegistryKind) URL() string {
	switch r {
	case RegistryPyPI:
		return "https://pypi.org"
	case RegistryTestPyPI:
		return "https://test.pypi.org"
	}
	return ""
}

// OIDCAudience returns the audience claim the OIDC token must carry for the
// registry's mint endpoint to accept it.
func (r RegistryKind) OIDCAudience() string {
	switch r {
	case RegistryPyPI:
		return "pypi"
	case RegistryTestPyPI:
		return "testpypi"
	}
	return ""
}

// PublishTarget is one artifact pair (sdist + wheel) to upload. The hash
// fields are sha256 base64-urlsafe-no-padding per PEP 376 / PEP 740 and
// surface in the attestation's in-toto subject list.
type PublishTarget struct {
	// Distribution is the PEP 503 distribution name (e.g. "mochi-httpx").
	Distribution string
	// Version is the PEP 440 release identifier.
	Version string
	// SdistPath is the absolute path to the sdist .tar.gz.
	SdistPath string
	// SdistSHA256 is the sha256 hash of the sdist body (base64-urlsafe,
	// no padding).
	SdistSHA256 string
	// SdistSize is the byte size of the sdist body.
	SdistSize int
	// WheelPath is the absolute path to the wheel .whl.
	WheelPath string
	// WheelSHA256 is the sha256 hash of the wheel body.
	WheelSHA256 string
	// WheelSize is the byte size of the wheel body.
	WheelSize int
}

// PublishRequest is the input to Orchestrator.Publish. Every required field is
// validated; see Validate.
type PublishRequest struct {
	// Registry selects PyPI vs TestPyPI.
	Registry RegistryKind
	// Targets is the artifact set to upload. Empty is a validation error.
	Targets []PublishTarget
	// DryRun skips the real upload but still exercises OIDC token
	// minting + attestation envelope construction; useful in CI to
	// verify the publish path before a release branch is cut.
	DryRun bool
	// OIDCProvider supplies the OIDC token. Required unless DryRun is
	// true AND OIDCProvider is nil, in which case a synthetic dry-run
	// token is used.
	OIDCProvider OIDCProvider
	// Uploader drives the actual upload subprocess. When nil the
	// orchestrator constructs the default uv-shell uploader.
	Uploader Uploader
	// Builder identifies who produced the artifact; surfaces in the
	// PEP 740 in-toto statement as the predicate's `builder.id`.
	Builder string
}

// Validate enforces the well-formedness invariants. The orchestrator runs
// this before any external call.
func (r PublishRequest) Validate() error {
	switch r.Registry {
	case RegistryPyPI, RegistryTestPyPI:
	default:
		return fmt.Errorf("publish: unknown Registry %d", r.Registry)
	}
	if len(r.Targets) == 0 {
		return fmt.Errorf("publish: empty Targets")
	}
	for i, t := range r.Targets {
		if t.Distribution == "" {
			return fmt.Errorf("publish: target %d empty Distribution", i)
		}
		if t.Version == "" {
			return fmt.Errorf("publish: target %d empty Version", i)
		}
		if t.SdistPath == "" || t.WheelPath == "" {
			return fmt.Errorf("publish: target %d missing artifact path", i)
		}
		if t.SdistSHA256 == "" || t.WheelSHA256 == "" {
			return fmt.Errorf("publish: target %d missing sha256", i)
		}
		if t.SdistSize <= 0 || t.WheelSize <= 0 {
			return fmt.Errorf("publish: target %d zero size", i)
		}
		if !strings.HasSuffix(t.SdistPath, ".tar.gz") {
			return fmt.Errorf("publish: target %d sdist %q missing .tar.gz", i, t.SdistPath)
		}
		if !strings.HasSuffix(t.WheelPath, ".whl") {
			return fmt.Errorf("publish: target %d wheel %q missing .whl", i, t.WheelPath)
		}
	}
	if !r.DryRun && r.OIDCProvider == nil {
		return fmt.Errorf("publish: OIDCProvider required for non-dry-run")
	}
	return nil
}
