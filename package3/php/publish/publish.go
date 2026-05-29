// Package publish implements the Packagist publish flow: GPG-signed git tag
// creation, Sigstore actions/attest-build-provenance OIDC attestation, and
// the Packagist Update API ping. Phase 0 ships the package stub; the full
// implementation arrives in phase 10.
package publish
