package attest

import (
	"context"
	"errors"
	"fmt"
)

// Verifier wires a Fetcher (network or static) to a Policy. Install
// orchestration calls Verify once per resolved wheel and gates the
// install on the returned Report + error per Policy.Required.
type Verifier struct {
	Policy  Policy
	Fetcher Fetcher
}

// Verify pulls the attestation bundle for target, parses it, and runs
// it through the policy. When the fetcher signals ErrNoAttestation the
// verifier still calls Policy.Verify(nil, target) so the policy can
// emit ReasonMissingAttestation when Required and stay silent when not.
func (v Verifier) Verify(ctx context.Context, target WheelTarget) (*Report, error) {
	if v.Fetcher == nil {
		return nil, fmt.Errorf("attest: verifier missing Fetcher")
	}

	raw, err := v.Fetcher.Fetch(ctx, target)
	if errors.Is(err, ErrNoAttestation) {
		return v.Policy.Verify(nil, target)
	}
	if err != nil {
		r := &Report{
			WheelURL:     target.URL,
			Distribution: target.Distribution,
			Version:      target.Version,
		}
		r.addViolation(ReasonParseFailure, fmt.Sprintf("fetch attestation: %v", err))
		if v.Policy.Required {
			return r, fmt.Errorf("attest: fetch attestation: %w", err)
		}
		return r, nil
	}

	bundle, err := ParseBundle(raw)
	if err != nil {
		r := &Report{
			WheelURL:     target.URL,
			Distribution: target.Distribution,
			Version:      target.Version,
		}
		r.addViolation(ReasonParseFailure, err.Error())
		if v.Policy.Required {
			return r, fmt.Errorf("attest: %w", err)
		}
		return r, nil
	}

	return v.Policy.Verify(bundle, target)
}
