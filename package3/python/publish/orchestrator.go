package publish

import (
	"context"
	"fmt"
	"net/http"
)

// Orchestrator glues the publish request into the OIDC mint + uploader
// chain. The orchestrator owns the per-publish HTTP client; tests inject a
// mockable transport via HTTPClient.
type Orchestrator struct {
	// HTTPClient is used for the PyPI mint endpoint. Defaults to
	// http.DefaultClient when nil.
	HTTPClient *http.Client
	// Now overrides time stamping in the attestation. Tests use a frozen
	// clock so the encoded statement is byte-stable.
	Now func() (string, error)
}

// PublishResult is what Orchestrator.Publish returns. Artifacts is one
// entry per (target, kind=sdist|wheel) pair; Attestation is the canonical
// in-toto statement bytes; MintedTokenExpires is the absolute UTC expiry
// of the short-lived API token if minted (zero for dry-run).
type PublishResult struct {
	Artifacts          []PublishedArtifact
	Attestation        []byte
	MintedTokenExpires string
	DryRun             bool
}

// PublishedArtifact is one upload outcome. Skipped is true when DryRun
// bypassed the upload.
type PublishedArtifact struct {
	Distribution   string
	Version        string
	URL            string
	AttestationURL string
	Skipped        bool
}

// Publish runs the publish pipeline. It validates the request, mints an
// API token (skipped on dry-run), builds the PEP 740 attestation, and
// drives one Upload per target.
func (o *Orchestrator) Publish(ctx context.Context, req PublishRequest) (*PublishResult, error) {
	if err := req.Validate(); err != nil {
		return nil, err
	}
	uploader := req.Uploader
	if uploader == nil {
		if req.DryRun {
			uploader = &RecordingUploader{}
		} else {
			uploader = UVUploader{}
		}
	}

	stmt := BuildStatement(req.Targets, AttestationOptions{
		BuilderID: req.Builder,
	})
	stmtBytes, err := EncodeStatement(stmt)
	if err != nil {
		return nil, fmt.Errorf("publish: encode attestation: %w", err)
	}

	var apiToken string
	var mintExpires string
	if !req.DryRun {
		oidcTok, err := req.OIDCProvider.Token(req.Registry.OIDCAudience())
		if err != nil {
			return nil, err
		}
		minted, err := MintAPIToken(o.HTTPClient, req.Registry, oidcTok)
		if err != nil {
			return nil, err
		}
		apiToken = minted.Token
		if !minted.Expires.IsZero() {
			mintExpires = minted.Expires.UTC().Format("2006-01-02T15:04:05Z")
		}
	}

	res := &PublishResult{
		Attestation: stmtBytes,
		DryRun:      req.DryRun,
	}
	res.MintedTokenExpires = mintExpires
	for _, t := range req.Targets {
		call := UploadCall{
			Target:          t,
			Registry:        req.Registry,
			Token:           apiToken,
			AttestationJSON: stmtBytes,
		}
		ur, err := uploader.Upload(ctx, call)
		if err != nil {
			return nil, err
		}
		res.Artifacts = append(res.Artifacts, PublishedArtifact{
			Distribution:   t.Distribution,
			Version:        t.Version,
			URL:            ur.URL,
			AttestationURL: ur.AttestationURL,
			Skipped:        ur.Skipped,
		})
	}
	return res, nil
}
