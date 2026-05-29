---
title: "Phase 13. Sigstore + OIDC trusted publishing"
sidebar_position: 14
sidebar_label: "Phase 13. Sigstore + OIDC"
description: "MEP-57 Phase 13 — keyless signing via GitHub Actions / GitLab CI OIDC, Fulcio certificate request, Rekor inclusion proof, Sigstore bundle assembly, registry-side verification, `mochi pkg audit signatures`."
---

# Phase 13. Sigstore + OIDC trusted publishing

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-57 §Phases · Phase 13](/docs/mep/mep-0057#phase-13-sigstore) |
| Status         | NOT STARTED |
| Started        | — |
| Landed         | — |
| Tracking issue | — |
| Tracking PR    | — |

## Gate

`TestPhase13Sigstore`: publish round-trip against a sigstore-mock Fulcio + Rekor; bundle verifies; a flipped byte in the artifact fails verification; an unregistered publisher binding fails with `M057_SIG_E006`.

Pass criteria:

1. OIDC token issuance. In a GitHub Actions runner, the publisher obtains a workload identity JWT scoped to the workflow's `repository`, `ref`, and `job_workflow_ref` claims. Token never written to disk; lifetime under 5 minutes.
2. Fulcio cert. The ephemeral keypair plus the OIDC token round-trips through Fulcio and returns a short-lived (10-minute) X.509 cert binding the public key to the OIDC identity. The cert chain is verified against the Fulcio root.
3. Rekor inclusion. The signed in-toto Statement is submitted to Rekor; the response includes an inclusion proof against the current SET (Signed Entry Timestamp). The client persists the proof in the bundle.
4. Bundle round-trip. The Sigstore bundle (Protobuf v0.3, research note 09 §6) is uploaded with the blob; consumer-side verification reads the bundle, verifies cert + signature + Rekor inclusion against pinned roots, and the bundle from disk re-verifies identically.
5. Tamper detection. A flipped byte anywhere in the tarball causes signature verification to fail with `M057_SIG_E004`.
6. Wrong-publisher rejection. A bundle whose Fulcio cert binds to `github.com/attacker/strings` for a package whose registered publisher is `github.com/mochilang/strings` is rejected with `M057_SIG_E006`. The registered binding lives in the index entry's `pr.sig` field.
7. Mock infrastructure. The CI test uses `sigstore-mock` Fulcio + Rekor (research note 09 §9); no live calls to Sigstore public infrastructure are required for tests to pass.

## Goal-alignment audit

Sigstore + OIDC is the *only* publish surface in v1, so this phase is where the publish flow actually becomes usable. The user-facing goal moved: "I push a `v0.1.0` tag in GitHub Actions; `mochi pkg publish` signs and uploads without any long-lived secrets".

The decision to skip password-protected GPG keys (or any long-lived key) eliminates one of the largest classes of supply-chain compromise: stolen publisher keys (Codecov 2021, log4shell-adjacent NPM tokens 2022). Workload identity tokens are scoped to a single workflow execution; even if exfiltrated, they expire before being usable.

The publisher binding (research note 09 §7) is the answer to "but what stops attacker from spinning up `github.com/attacker/strings` and signing a fake `@mochi/strings`?" The first publisher of a name registers a binding (a regex against the OIDC subject claim); subsequent publishes must match the binding. Stealing a name therefore requires either compromising the original publisher's GitHub org or convincing the registry to rebind, both of which are auditable.

## Sub-phases

| # | Scope | Status | Commit |
|---|-------|--------|--------|
| 13.0 | OIDC token request to GitHub Actions, GitLab CI providers | NOT STARTED | — |
| 13.1 | Fulcio certificate request with ephemeral keypair | NOT STARTED | — |
| 13.2 | In-toto Statement v1 + SLSA Provenance v1 predicate | NOT STARTED | — |
| 13.3 | Signature creation; bundle assembly | NOT STARTED | — |
| 13.4 | Rekor entry submission; inclusion proof capture | NOT STARTED | — |
| 13.5 | Bundle upload to registry alongside blob | NOT STARTED | — |
| 13.6 | Registry-side bundle verification against publisher binding | NOT STARTED | — |
| 13.7 | Consumer-side `mochi pkg audit signatures` | NOT STARTED | — |
| 13.8 | Sigstore-mock Fulcio + Rekor for CI tests | NOT STARTED | — |
| 13.9 | `mochi pkg publish register` binding flow | NOT STARTED | — |
| 13.10 | TUF root distribution + pinning | NOT STARTED | — |

## Sub-phase 13.0 — OIDC token request

```go
// pkg/pkgsign/oidc/provider.go
type Provider interface {
    Name() string
    AvailableInEnv() bool
    GetToken(ctx context.Context, audience string) (string, error)
}

// pkg/pkgsign/oidc/github.go
type GitHubActions struct{}

func (g GitHubActions) AvailableInEnv() bool {
    return os.Getenv("ACTIONS_ID_TOKEN_REQUEST_URL") != "" &&
           os.Getenv("ACTIONS_ID_TOKEN_REQUEST_TOKEN") != ""
}

func (g GitHubActions) GetToken(ctx context.Context, audience string) (string, error) {
    url := os.Getenv("ACTIONS_ID_TOKEN_REQUEST_URL") + "&audience=" + audience
    req, _ := http.NewRequestWithContext(ctx, "GET", url, nil)
    req.Header.Set("Authorization", "Bearer "+os.Getenv("ACTIONS_ID_TOKEN_REQUEST_TOKEN"))
    resp, err := http.DefaultClient.Do(req)
    if err != nil { return "", err }
    defer resp.Body.Close()
    var r struct{ Value string `json:"value"` }
    json.NewDecoder(resp.Body).Decode(&r)
    return r.Value, nil
}
```

Providers in order of preference:

1. `GitHubActions` (when run in Actions).
2. `GitLabCI` (when run in GitLab CI).
3. `CircleCI` (when run in Circle).
4. `Buildkite`.
5. Interactive browser (developer machine): opens `sigstore.dev/oauth` for local signing; only available when stdin is a TTY.

Audience: always `sigstore` (per Sigstore convention).

## Sub-phase 13.1 — Fulcio cert request

```go
// pkg/pkgsign/fulcio/fulcio.go
type Client struct {
    URL string  // https://fulcio.sigstore.dev (default)
    HTTP *http.Client
}

type SigningCert struct {
    Cert        *x509.Certificate    // ephemeral, ~10 min
    Chain       []*x509.Certificate  // up to Fulcio root
    PublicKey   crypto.PublicKey
    PrivateKey  crypto.PrivateKey   // ephemeral, never persisted
}

func (c *Client) GetCert(ctx context.Context, idToken string) (*SigningCert, error) {
    priv, _ := ecdsa.GenerateKey(elliptic.P256(), rand.Reader)
    proof := signOIDCSubject(priv, idToken)
    body, _ := json.Marshal(map[string]any{
        "publicKey": map[string]any{
            "content":   base64.StdEncoding.EncodeToString(elliptic.Marshal(priv.PublicKey.Curve, priv.PublicKey.X, priv.PublicKey.Y)),
            "algorithm": "ecdsa",
        },
        "signedEmailAddress": proof,
    })
    req, _ := http.NewRequestWithContext(ctx, "POST", c.URL+"/api/v2/signingCert", bytes.NewReader(body))
    req.Header.Set("Authorization", "Bearer "+idToken)
    req.Header.Set("Content-Type", "application/json")
    resp, err := c.HTTP.Do(req)
    /* parse PEM chain; return SigningCert */
}
```

Key material lifecycle: the ECDSA private key is generated in-process, used to sign the in-toto Statement, then discarded. It never touches disk. The Fulcio cert binding it to the OIDC identity is what makes the signature usable.

Determinism carve-out: the Sigstore bundle is **not** byte-deterministic.
The ephemeral key, the wall-clock-tied Fulcio cert NotBefore/NotAfter, the
Rekor log index, and the inclusion proof all differ between two publishes
of the same source. Phase 17's reproducibility contract therefore applies
to the *tarball* (Phase 12) and the *in-toto Statement payload* (Phase
13.2), not to the bundle's `signatures[].sig` or `verificationMaterial`.
Bundle verification asserts the payload hash matches the expected blob;
two reproducing publishes produce different bundles that both validate
against the same blob. Documented as a known and intentional gap in
[research note 12 §A.14](/docs/research/0057/risks-and-alternatives).

## Sub-phase 13.2 — In-toto Statement v1 + SLSA Build L3

The signed payload is an in-toto Statement (research note 09 §5):

```json
{
  "_type": "https://in-toto.io/Statement/v1",
  "subject": [{
    "name": "@mochi/strings@0.4.7",
    "digest": {
      "blake3": "e2d1a4...",
      "sha256": "abf3e1..."
    }
  }],
  "predicateType": "https://slsa.dev/provenance/v1",
  "predicate": {
    "buildDefinition": {
      "buildType": "https://mochi-lang.org/buildtype/v1",
      "externalParameters": {
        "source": "git+https://github.com/mochilang/strings@refs/tags/v0.4.7",
        "configPath": "mochi.toml"
      },
      "internalParameters": {
        "mochiVersion": "0.7.0",
        "targets": ["typescript", "python", "jvm"]
      },
      "resolvedDependencies": [
        {"uri": "git+https://github.com/mochilang/strings", "digest": {"gitCommit": "1a2b3c..."}},
        {"uri": "pkg:mochi/@mochi/json@1.2.5", "digest": {"blake3": "..."}}
      ]
    },
    "runDetails": {
      "builder": {"id": "https://github.com/actions/runner"},
      "metadata": {
        "invocationId": "https://github.com/mochilang/strings/actions/runs/9876543",
        "startedOn": "2026-05-29T14:00:00Z",
        "finishedOn": "2026-05-29T14:02:33Z"
      }
    }
  }
}
```

`resolvedDependencies` is the lockfile's package list, materialised. This is what makes the provenance SLSA Build L3 conformant (research note 09 §5.3): the producer can prove what went into the build.

```go
type Statement struct {
    Type           string             `json:"_type"`
    Subject        []Subject          `json:"subject"`
    PredicateType  string             `json:"predicateType"`
    Predicate      SLSAProvenanceV1   `json:"predicate"`
}

func BuildStatement(art *Artefact, m *Manifest, lock *Lockfile, env *RuntimeEnv) *Statement {
    return &Statement{
        Type: "https://in-toto.io/Statement/v1",
        Subject: []Subject{{
            Name: fmt.Sprintf("%s@%s", m.Package.Name, m.Package.Version),
            Digest: map[string]string{
                "blake3": art.BLAKE3, "sha256": art.SHA256,
            },
        }},
        PredicateType: "https://slsa.dev/provenance/v1",
        Predicate: buildProvenance(m, lock, env),
    }
}
```

## Sub-phase 13.3 — Sign + bundle

The signature uses DSSE envelope (Dead Simple Signing Envelope, RFC):

```go
// pkg/pkgsign/dsse/envelope.go
type Envelope struct {
    Payload     string       `json:"payload"`         // base64 of Statement JSON
    PayloadType string       `json:"payloadType"`     // "application/vnd.in-toto+json"
    Signatures  []Signature  `json:"signatures"`
}

type Signature struct {
    Sig  string `json:"sig"`              // base64 ECDSA signature over PAE(payloadType, payload)
    Cert string `json:"cert,omitempty"`   // PEM of Fulcio cert (chain stored elsewhere)
}

func SignDSSE(stmt *Statement, cert *SigningCert) (*Envelope, error) {
    payload, _ := json.Marshal(stmt)
    pae := paeEncode("application/vnd.in-toto+json", payload)
    sig, _ := ecdsa.SignASN1(rand.Reader, cert.PrivateKey.(*ecdsa.PrivateKey), sha256Sum(pae))
    return &Envelope{
        Payload:     base64.StdEncoding.EncodeToString(payload),
        PayloadType: "application/vnd.in-toto+json",
        Signatures: []Signature{{
            Sig:  base64.StdEncoding.EncodeToString(sig),
            Cert: pemEncode(cert.Cert),
        }},
    }, nil
}
```

The PAE encoding (Pre-Authentication Encoding):

```
"DSSEv1 " + len(payloadType) + " " + payloadType + " " + len(payload) + " " + payload
```

Defends against payload-type swap attacks.

The Sigstore bundle (Protobuf, schema at `sigstore_bundle.proto`):

```protobuf
message Bundle {
  string media_type = 1;            // "application/vnd.dev.sigstore.bundle+json;version=0.3"
  VerificationMaterial verification_material = 2;
  oneof content {
    MessageSignature message_signature = 3;
    DsseEnvelope dsse_envelope = 4;
  }
}

message VerificationMaterial {
  oneof content { X509CertificateChain x509_chain = 1; PublicKey public_key = 2; }
  repeated TransparencyLogEntry tlog_entries = 3;
  repeated TimestampVerificationData timestamp_verification_data = 4;
}
```

The bundle assembles cert chain, Rekor inclusion proof, and DSSE envelope into a single artefact uploaded with the tarball.

## Sub-phase 13.4 — Rekor submission

```go
// pkg/pkgsign/rekor/rekor.go
type Client struct {
    URL  string  // https://rekor.sigstore.dev
    HTTP *http.Client
}

type LogEntry struct {
    UUID         string
    LogIndex     int64
    IntegratedTime int64
    Body         string  // base64 canonicalised entry
    Verification InclusionProof
}

type InclusionProof struct {
    TreeSize int64
    LogIndex int64
    RootHash string
    Hashes   []string  // Merkle path
    SET      string    // Signed Entry Timestamp (Ed25519 over body+timestamp+treesize)
}

func (c *Client) Submit(env *dsse.Envelope, cert *x509.Certificate) (*LogEntry, error) {
    body := buildIntotoEntry(env, cert)
    resp, err := c.HTTP.Post(c.URL+"/api/v1/log/entries", "application/json", bytes.NewReader(body))
    /* parse Rekor LogEntry response */
}
```

The inclusion proof anchors the entry to a Merkle tree root that Rekor publicly attests; the client verifies the proof immediately upon return (without trusting Rekor blindly) by recomputing the path.

Submission round-trip target: under 2s (Rekor's published p99 is ~1s as of 2025).

## Sub-phase 13.5 — Bundle upload

The upload from Phase 12.6 grows to include the bundle:

```
POST https://upload.mochi.dev/packages
Content-Type: multipart/form-data; boundary=mochi-boundary

--mochi-boundary
Content-Disposition: form-data; name="tarball"; filename="strings-0.4.7.tar.zst"
Content-Type: application/vnd.mochi.tarball+zstd

<bytes>
--mochi-boundary
Content-Disposition: form-data; name="bundle"; filename="strings-0.4.7.sigstore.bundle"
Content-Type: application/vnd.dev.sigstore.bundle+json;version=0.3

<bytes>
--mochi-boundary--
```

The registry stores the bundle alongside the blob at `blobs.mochi.dev/<bb>/<aa>/<hex>.sigstore.bundle`, sibling of `<hex>.tar.zst`.

## Sub-phase 13.6 — Registry-side verification

The registry MUST verify the bundle before accepting the upload (research note 09 §8):

1. Parse the DSSE envelope.
2. Verify the cert chain against the pinned Fulcio root.
3. Verify the ECDSA signature over PAE.
4. Verify the Rekor inclusion proof against the SET.
5. Extract the OIDC subject from the cert; match against the package's publisher binding (`pr.sig` field in the index entry).
6. Parse the in-toto Statement; check `subject[0].digest.blake3` matches the tarball's hash.

A binding looks like:

```toml
[[package.publisher_binding]]
provider     = "github-actions"
subject_regex = "^https://github\\.com/mochilang/strings/\\.github/workflows/[^@]+@refs/tags/.+$"
```

The regex anchors the workflow file path (job_workflow_ref claim) and the tag-ref pattern. A workflow living elsewhere in the same repo cannot publish.

```go
func (s *Server) VerifyBundle(b *Bundle, expectedBlake3 string, binding []PublisherBinding) error {
    if err := verifyCertChain(b.X509Chain, s.FulcioRoot); err != nil { return E007(err) }
    if err := verifyDSSESignature(b.Envelope, b.X509Chain[0]); err != nil { return E007(err) }
    if err := verifyRekor(b.TlogEntries, s.RekorPublicKey); err != nil { return E007(err) }
    subject := extractSubject(b.X509Chain[0])
    if !matchAnyBinding(subject, binding) { return E006() }
    stmt, _ := decodeStatement(b.Envelope.Payload)
    if stmt.Subject[0].Digest["blake3"] != expectedBlake3 { return E007("subject digest mismatch") }
    return nil
}
```

## Sub-phase 13.7 — Consumer `mochi pkg audit signatures`

```
mochi pkg audit signatures             # verify every locked package's bundle
mochi pkg audit signatures --since=v0.5
mochi pkg audit signatures --offline   # use the bundle cached at install time
```

```go
func cmdAuditSignatures(c *cli.Context) error {
    lock, _ := pkglock.ParseFile("mochi.lock")
    trust, _ := pkgsign.LoadTUFRoot()
    var failures []string
    for _, p := range lock.Packages {
        bundle, err := loadBundle(p.BLAKE3)
        if err != nil { failures = append(failures, fmt.Sprintf("%s: %v", p.Name, err)); continue }
        if err := pkgsign.Verify(bundle, p.BLAKE3, trust); err != nil {
            failures = append(failures, fmt.Sprintf("%s: %v", p.Name, err))
        }
    }
    if len(failures) > 0 { return cli.Exit(strings.Join(failures, "\n"), 1) }
    fmt.Println("OK")
    return nil
}
```

Failure modes surface the specific cause: signature invalid, Rekor proof invalid, cert chain invalid, expired cert, binding mismatch.

## Sub-phase 13.8 — Sigstore mock for CI

`tests/pkgsystem/sigstore/mockinfra/`:

```go
// tests/pkgsystem/sigstore/mockinfra/fulcio.go
type MockFulcio struct {
    *httptest.Server
    Root *x509.Certificate
    Key  *ecdsa.PrivateKey  // signs ephemeral certs
}

func (m *MockFulcio) IssueCert(pub crypto.PublicKey, subject string) []byte { /* ... */ }
```

```go
// tests/pkgsystem/sigstore/mockinfra/rekor.go
type MockRekor struct {
    *httptest.Server
    Entries []LogEntry
    Root    []byte
}

func (m *MockRekor) Append(body []byte) *LogEntry { /* compute Merkle path, return inclusion proof */ }
```

CI tests against the mock are end-to-end: publish flow signs against mock Fulcio, mock Rekor returns a verifiable proof, registry-side code verifies. No live Sigstore traffic.

## Sub-phase 13.9 — `mochi pkg publish register`

The first publish of a name registers the binding:

```
mochi pkg publish register \
  --name @scope/name \
  --provider github-actions \
  --subject-regex "^https://github\\.com/myorg/mypkg/\\.github/workflows/release\\.yml@refs/tags/.+$"
```

The CLI POSTs to `https://api.mochi.dev/packages/<name>/bindings`. The first POST succeeds; subsequent POSTs require an OIDC token whose subject already matches an existing binding (rotation flow).

Outputs the binding as a manifest snippet for the user to commit:

```toml
[[package.publisher_binding]]
provider      = "github-actions"
subject_regex = "^https://github\\.com/myorg/mypkg/\\.github/workflows/release\\.yml@refs/tags/.+$"
```

This is the published authoritative record; the registry's view is derived from publishes that match.

## Sub-phase 13.10 — TUF root distribution

The Fulcio + Rekor root keys are distributed via TUF (The Update Framework), pinned in the client:

```go
// pkg/pkgsign/tuf/tuf.go
type Root struct {
    FulcioPublicKey *x509.Certificate
    RekorPublicKey  ed25519.PublicKey
    CTLogPublicKey  *ecdsa.PublicKey
    Expires         time.Time
}

func LoadTUFRoot() (*Root, error) {
    // 1. Read pinned root.json (embedded in binary at build time).
    // 2. Walk delegated metadata from https://tuf-repo-cdn.sigstore.dev/.
    // 3. Verify timestamp.json against root keys; verify targets.json.
    // 4. Fetch and verify Fulcio + Rekor public keys.
}
```

Embedded pinning at build time avoids a TOFU window on first run.

## Files changed

| File | Purpose | Owner |
|------|---------|-------|
| `pkg/pkgsign/oidc/provider.go` | OIDC abstraction | Owner |
| `pkg/pkgsign/oidc/github.go` | GitHub Actions provider | Owner |
| `pkg/pkgsign/oidc/gitlab.go` | GitLab CI provider | Owner |
| `pkg/pkgsign/oidc/browser.go` | Interactive flow | Owner |
| `pkg/pkgsign/fulcio/fulcio.go` | Fulcio client | Owner |
| `pkg/pkgsign/dsse/envelope.go` | DSSE encode/decode | Owner |
| `pkg/pkgsign/intoto/statement.go` | In-toto Statement v1 | Owner |
| `pkg/pkgsign/slsa/provenance.go` | SLSA v1 predicate | Owner |
| `pkg/pkgsign/rekor/rekor.go` | Rekor client | Owner |
| `pkg/pkgsign/bundle/bundle.go` | Sigstore bundle protobuf | Owner |
| `pkg/pkgsign/verify/verify.go` | Bundle verification | Owner |
| `pkg/pkgsign/tuf/tuf.go` | TUF root loader | Owner |
| `pkg/pkgpublish/sign.go` | Wires signing into publish (extended by Phase 17 SDE) | Owner |
| `cmd/mochi/publish_register.go` | `mochi pkg publish register` handler | Owner |
| `cmd/mochi/audit_signatures.go` | `mochi pkg audit signatures` handler | Owner |
| `tests/pkgsystem/sigstore/roundtrip/*` | End-to-end against mock | Owner |
| `tests/pkgsystem/sigstore/tampered/*` | Verify catches flipped byte | Owner |
| `tests/pkgsystem/sigstore/wrong-publisher/*` | E006 on binding miss | Owner |
| `tests/pkgsystem/sigstore/mockinfra/*` | Mock Fulcio + Rekor | Owner |

## Error code surface

| Code | Trigger |
|------|---------|
| `M057_SIG_E006` | OIDC subject does not match any publisher binding. |
| `M057_SIG_E004` | Sigstore verification failed (cert, sig, or Rekor proof). |
| `M057_SIG_E001` | OIDC token issuance failed (no provider, network, scope mismatch). |
| `M057_TUF_E001` | TUF metadata expired or invalid. |
| `M057_TUF_E002` | TUF target verification failed. |

## Test set

- `TestPhase13OIDCGitHub` — mock GitHub OIDC env returns token.
- `TestPhase13FulcioRoundTrip` — mock Fulcio issues cert; chain verifies.
- `TestPhase13Statement` — in-toto Statement matches golden JSON.
- `TestPhase13Sign` — DSSE signature verifies.
- `TestPhase13Rekor` — mock Rekor returns valid inclusion proof; client verifies.
- `TestPhase13BundleRoundtrip` — bundle serialised, parsed, verified.
- `TestPhase13Tampered` — byte flip raises E007.
- `TestPhase13WrongPublisher` — mismatched subject raises E006.
- `TestPhase13AuditOffline` — cached bundle verifies without network.
- `TestPhase13Register` — first publish creates binding; second non-matching publish refused.
- `TestPhase13TUF` — root expiry rejected.

## Open questions

- Whether to also support keyful (cosign-style) signing for niche cases like air-gapped publishers; current plan: no at v1, OIDC-only.
- Whether to verify Rekor's witness co-signatures (research note 09 §10); current plan: yes, post v1.0 once Sigstore witnesses are stable.
- Whether registry storage of bundles requires checksum-addressed storage too (so a different bundle cannot be substituted later); current plan: yes, bundle is content-addressed by SHA-256 alongside the blob.

## Cross-references

- Trusted publishing details: [research note 09](/docs/research/0057/trusted-publishing).
- Rationale: [research note 02 §5](/docs/research/0057/design-philosophy).
- Publisher binding format: [research note 09 §7](/docs/research/0057/trusted-publishing).
- SLSA Build L3 mapping: [research note 09 §5.3](/docs/research/0057/trusted-publishing).
- Index entry `pr.sig` field: [research note 07 §3](/docs/research/0057/registry-index).
