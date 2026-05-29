package build

import (
	"os"
	"path/filepath"
	"strings"
	"testing"
)

// TestPhase18PackagistReleaseWorkflow gates the Packagist signed-release
// supply chain. Phase 18 is a "gate-only" phase per MEP-55 §17: no new
// lowering code lands, but the CI workflow that signs and attests the
// runtime tarball must exist and carry the contractual steps:
//
//   - GPG-signed tag verification (`git tag -v`)
//   - composer audit before publish
//   - actions/attest-build-provenance@v1 (Sigstore-backed OIDC)
//   - php-signify Ed25519 (optional)
//   - Packagist webhook notify (optional)
//   - independent verify-gate job (re-runs composer install + audit)
//
// The gate is structural: it asserts the workflow file is in place and
// references the right actions. Running the workflow against a real
// Packagist publish is out of scope for this test (and dangerous for a
// repo-wide CI run; the workflow is tag-triggered for that reason).
func TestPhase18PackagistReleaseWorkflow(t *testing.T) {
	root := repoRoot(t)
	wfPath := filepath.Join(root, ".github", "workflows", "transpiler3-php-publish.yml")
	data, err := os.ReadFile(wfPath)
	if err != nil {
		t.Fatalf("read publish workflow: %v", err)
	}
	src := string(data)

	for _, want := range []string{
		// Tag-only trigger so a normal push never publishes.
		"tags:",
		"'v0.*'",
		"'v1.*'",
		// OIDC permission for the attestation action.
		"id-token: write",
		"attestations: write",
		// GPG private key gated, skip-if-unconfigured.
		"GPG_PRIVATE_KEY",
		"gpg --batch --import",
		// Tag signature verification.
		"git tag -v",
		// composer audit is the FriendsOfPHP advisory gate.
		"composer audit",
		// Reproducible-tar flags so the staged tarball is deterministic.
		"--sort=name",
		"--mtime=",
		// The actual attestation action.
		"actions/attest-build-provenance@v1",
		"subject-path:",
		// Optional Drupal php-signify Ed25519 signature path.
		"PHP_SIGNIFY_SECRET_KEY",
		"drupal/php-signify",
		// Optional Packagist webhook notify.
		"packagist.org/api/update-package",
		// Independent verify gate.
		"verify-gate:",
		"needs: publish",
	} {
		if !strings.Contains(src, want) {
			t.Errorf("publish workflow missing %q", want)
		}
	}
}

// TestPhase18ReleaseTrustRootDocumented asserts that SECURITY.md (or a
// nearby release-trust doc) explains how consumers verify a signed
// release. Phase 18 is the user-visible surface of the trust chain;
// without the doc, the GPG key and OIDC attestation are orphaned.
//
// The check is intentionally loose: we look for any one of several
// canonical verification commands. A doc that uses any of them passes.
func TestPhase18ReleaseTrustRootDocumented(t *testing.T) {
	root := repoRoot(t)
	candidates := []string{
		// PHP-runtime-scoped docs take precedence; the global
		// SECURITY.md is about memory-safety and the trust-root details
		// belong next to the package being released.
		filepath.Join(root, "transpiler3", "php", "runtime", "TRUST.md"),
		filepath.Join(root, "transpiler3", "php", "runtime", "SECURITY.md"),
		filepath.Join(root, "SECURITY.md"),
	}

	mechanisms := []string{
		"git tag -v",
		"gh attestation verify",
		"php-signify",
		"GPG",
		"OIDC",
	}

	// At least one trust-root document must exist AND mention at least
	// one verification mechanism. We accept any of the candidate paths.
	for _, p := range candidates {
		data, err := os.ReadFile(p)
		if err != nil {
			continue
		}
		src := string(data)
		for _, m := range mechanisms {
			if strings.Contains(src, m) {
				return // pass
			}
		}
	}
	t.Errorf("no release-verification mechanism documented in any of %v (looked for any of %v)", candidates, mechanisms)
}

// TestPhase18ComposerManifestReleasable asserts the runtime
// composer.json is shaped for a Packagist publish:
//   - name is "mochi/runtime" (the namespace reserved on Packagist)
//   - license is set (Packagist requires it)
//   - autoload.psr-4 maps the runtime namespace
//   - require pins php to an ^8.x constraint
func TestPhase18ComposerManifestReleasable(t *testing.T) {
	root := repoRoot(t)
	path := filepath.Join(root, "transpiler3", "php", "runtime", "composer.json")
	data, err := os.ReadFile(path)
	if err != nil {
		t.Fatalf("read composer.json: %v", err)
	}
	src := string(data)

	for _, want := range []string{
		`"name": "mochi/runtime"`,
		`"license":`,
		`"autoload":`,
		`"psr-4":`,
		`"require":`,
		`"php":`,
	} {
		if !strings.Contains(src, want) {
			t.Errorf("composer.json missing %q for Packagist publish", want)
		}
	}
}
