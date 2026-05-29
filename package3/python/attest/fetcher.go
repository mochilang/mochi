package attest

import (
	"context"
	"errors"
	"fmt"
	"strings"
)

// Fetcher pulls the attestation bundle for a wheel. Production wires
// HTTPFetcher; tests wire StaticFetcher.
type Fetcher interface {
	Fetch(ctx context.Context, target WheelTarget) ([]byte, error)
}

// ErrNoAttestation is the sentinel a Fetcher returns when PyPI does
// not publish an attestation for the wheel. The verifier maps it to
// ReasonMissingAttestation when Policy.Required is true.
var ErrNoAttestation = errors.New("attest: no attestation available")

// StaticFetcher returns Bundle verbatim. Bundle may be nil to
// simulate ErrNoAttestation.
type StaticFetcher struct {
	Bundle []byte
}

// Fetch returns the configured bundle or ErrNoAttestation when empty.
func (s StaticFetcher) Fetch(context.Context, WheelTarget) ([]byte, error) {
	if len(s.Bundle) == 0 {
		return nil, ErrNoAttestation
	}
	return s.Bundle, nil
}

// HTTPFetcher constructs the PEP 740 attestation URL from the wheel
// URL. The actual HTTP fetch is sub-phase 15.2; this type ships
// AttestationURL today so the install orchestrator can stage URL
// construction without taking on a net dependency.
type HTTPFetcher struct{}

// AttestationURL returns the `<wheel-url>.provenance` companion URL
// per PEP 740 § "Fetching attestations".
func (HTTPFetcher) AttestationURL(target WheelTarget) (string, error) {
	if !strings.HasSuffix(target.URL, ".whl") {
		return "", fmt.Errorf("attest: wheel URL %q does not end in .whl", target.URL)
	}
	return target.URL + ".provenance", nil
}

// Fetch is sub-phase 15.2; for now return ErrNoAttestation so the
// caller's flow exercises the Required gate without a network call.
func (HTTPFetcher) Fetch(context.Context, WheelTarget) ([]byte, error) {
	return nil, ErrNoAttestation
}
