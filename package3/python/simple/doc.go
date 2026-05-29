// Package simple implements a client for the PyPI Simple Repository API
// described by PEP 503 (HTML format), PEP 691 (JSON format), and PEP 700
// (project files metadata endpoint).
//
// The MEP-71 bridge uses this package in phase 1 of the consume direction
// to enumerate the candidate distribution files for a Python dep before
// handing the resolution off to the uv subprocess in phase 2. Phase 1 only
// needs:
//
//  1. List the candidate files for a project name (`/simple/<name>/`).
//  2. Filter by PEP 440 specifier and by ABI tag.
//  3. Download the selected file and verify its sha256 (PEP 503) and, when
//     present, the blake3 hash (PEP 658 extension).
//
// The Client interface is deliberately minimal so the bridge can substitute
// a fake during testing. A DefaultClient (built on net/http) is supplied; it
// honours the standard HTTP_PROXY / HTTPS_PROXY environment variables and
// retries idempotent GETs with exponential backoff on 5xx responses.
//
// Phase 1 ships parsers for both PEP 503 (HTML) and PEP 691 (JSON). Most
// indices serve both representations under the same URL via content
// negotiation; the bridge prefers JSON because it carries the upload-time
// timestamp and the yanked flag without requiring HTML attribute lookups.
//
// What this package does NOT do:
//   - Resolve dependency conflicts: that is phase 2's job (the uv bridge).
//   - Install files: that is phase 8's job (the wheel installer).
//   - Mirror or proxy index responses: that is out of scope; the cache here
//     is for the build-time lookup only.
package simple
