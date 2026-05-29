package build

import (
	"os"
	"os/exec"
	"path/filepath"
	"regexp"
	"strings"
	"testing"
)

// TestPhase18WorkflowEmit asserts the release.yml file lands at
// the conventional path `.github/workflows/release.yml` (where
// GitHub Actions picks it up).
func TestPhase18WorkflowEmit(t *testing.T) {
	root := repoRoot(t)
	mochiPath := filepath.Join(root, "tests", "transpiler3", "typescript", "fixtures", "phase01-hello", "hello_int.mochi")
	outDir := t.TempDir()
	d := &Driver{CacheDir: t.TempDir(), NoCache: true}
	workflowPath, err := d.Build(mochiPath, outDir, TargetReleaseWorkflow)
	if err != nil {
		t.Fatalf("Build TargetReleaseWorkflow: %v", err)
	}
	wantSuffix := filepath.Join(".github", "workflows", "release.yml")
	if !strings.HasSuffix(workflowPath, wantSuffix) {
		t.Errorf("workflow path = %s, want suffix %s", workflowPath, wantSuffix)
	}
	if _, err := os.Stat(workflowPath); err != nil {
		t.Fatalf("workflow file missing: %v", err)
	}
}

// TestPhase18OidcPermissions asserts the workflow declares the
// permissions GitHub OIDC needs for Trusted Publishing:
//
//   - `id-token: write`: required for the workflow to mint an
//     OIDC token (without it, the publish step fails with "no
//     identity token available").
//   - `attestations: write`: required by
//     `actions/attest-build-provenance@v2` (the action that
//     records the build attestation in GitHub Attestations).
//
// Missing either permission means the action fails at runtime.
// This test catches that regression at emit time.
func TestPhase18OidcPermissions(t *testing.T) {
	body := readEmittedWorkflow(t)
	if !regexp.MustCompile(`(?m)^\s*id-token:\s*write`).MatchString(body) {
		t.Errorf("workflow missing `id-token: write` permission (required for OIDC):\n%s", body)
	}
	if !regexp.MustCompile(`(?m)^\s*attestations:\s*write`).MatchString(body) {
		t.Errorf("workflow missing `attestations: write` permission (required for actions/attest-build-provenance):\n%s", body)
	}
}

// TestPhase18NoLongLivedTokens greps the emitted workflow for any
// reference to a long-lived publishing secret. Trusted Publishing
// is incompatible with `NPM_TOKEN` / `JSR_TOKEN` in CI; if the
// emitter accidentally embeds one, an attacker who exfiltrates
// the token has a long window to publish malicious versions.
//
// The forbidden patterns:
//
//   - `NPM_TOKEN`, `JSR_TOKEN`: legacy publish-credential env
//     vars.
//   - `secrets.<X>_TOKEN`: any GHA-secret reference to a token.
//   - `npm publish` WITHOUT `--provenance`: silently downgrades
//     to the legacy authenticated-token path.
func TestPhase18NoLongLivedTokens(t *testing.T) {
	body := readEmittedWorkflow(t)
	forbidden := []string{
		"NPM_TOKEN",
		"JSR_TOKEN",
		"NODE_AUTH_TOKEN",
	}
	for _, bad := range forbidden {
		if strings.Contains(body, bad) {
			t.Errorf("workflow references forbidden long-lived token %q:\n%s", bad, body)
		}
	}
	if regexp.MustCompile(`\$\{\{\s*secrets\.\w+_TOKEN`).MatchString(body) {
		t.Errorf("workflow references secrets.*_TOKEN (incompatible with Trusted Publishing):\n%s", body)
	}
	for _, line := range strings.Split(body, "\n") {
		line = strings.TrimSpace(line)
		if !strings.HasPrefix(line, "run:") {
			continue
		}
		// Only npm publish lines (skip dry-run probes etc.)
		if !strings.Contains(line, "npm publish") {
			continue
		}
		if !strings.Contains(line, "--provenance") {
			t.Errorf("workflow has `npm publish` line without --provenance flag:\n%s", line)
		}
	}
}

// TestPhase18PublishCommands asserts the workflow uses the
// Trusted-Publishing CLI flags on both registries:
//
//   - `npm publish --provenance --access=public`: provenance
//     mode is what triggers the Sigstore signing path; without
//     it, the publish succeeds but produces no attestation.
//     `--access=public` is required for scoped packages
//     (defaults to private).
//   - `deno publish --token-source=github-actions`: instructs
//     Deno to fetch the OIDC token from the Actions environment.
//     Without it, deno publish prompts for an interactive token
//     and fails non-interactively.
func TestPhase18PublishCommands(t *testing.T) {
	body := readEmittedWorkflow(t)
	if !strings.Contains(body, "npm publish --provenance --access=public") {
		t.Errorf("workflow missing `npm publish --provenance --access=public`:\n%s", body)
	}
	if !strings.Contains(body, "deno publish --token-source=github-actions") {
		t.Errorf("workflow missing `deno publish --token-source=github-actions`:\n%s", body)
	}
}

// TestPhase18TagTrigger asserts the workflow runs on tag push
// (`v*.*.*`) and on PR (the dry-run path). Tag-triggered release
// is the canonical pattern for Trusted Publishing: the workflow
// runs only on a signed-off Git tag, and the registry-side
// publisher binding can be scoped to `refs/tags/*` so that PRs
// from forks cannot trigger a real publish.
func TestPhase18TagTrigger(t *testing.T) {
	body := readEmittedWorkflow(t)
	if !regexp.MustCompile(`(?m)^\s*-\s*"v\*\.\*\.\*"`).MatchString(body) {
		t.Errorf("workflow missing tag trigger pattern `v*.*.*`:\n%s", body)
	}
	if !strings.Contains(body, "pull_request") {
		t.Errorf("workflow missing pull_request trigger (PRs should run dry-run path):\n%s", body)
	}
	// The publish step must guard against a real publish from a
	// PR; otherwise a forked PR could trigger a malicious upload.
	if !strings.Contains(body, "startsWith(github.ref, 'refs/tags/')") {
		t.Errorf("workflow missing tag-only guard on real publish step:\n%s", body)
	}
}

// TestPhase18ProvenanceDryRun runs `npm publish --dry-run
// --provenance --access=public` against the Phase 15 npm package
// directory. The dry-run validates the manifest + computes the
// tarball SHA + exercises the publish code path; the only thing
// it skips is the registry-side commit (and the actual Sigstore
// signing, which requires the OIDC env vars only present in CI).
//
// This catches manifest regressions that would fail at the real
// publish time. Skipped when npm is not on PATH.
func TestPhase18ProvenanceDryRun(t *testing.T) {
	if _, err := resolveNpm(); err != nil {
		t.Skipf("npm not on PATH: %v", err)
	}
	root := repoRoot(t)
	mochiPath := filepath.Join(root, "tests", "transpiler3", "typescript", "fixtures", "phase01-hello", "hello_int.mochi")
	pkgDir := t.TempDir()
	d := &Driver{CacheDir: t.TempDir(), NoCache: true}
	if _, err := d.Build(mochiPath, pkgDir, TargetNpmPackage); err != nil {
		t.Fatalf("Build TargetNpmPackage: %v", err)
	}
	npm, _ := resolveNpm()
	cmd := exec.Command(npm, "publish", "--dry-run", "--provenance", "--access=public")
	cmd.Dir = pkgDir
	cmd.Env = os.Environ()
	out, err := cmd.CombinedOutput()
	if err != nil {
		t.Fatalf("npm publish --dry-run --provenance failed: %v\noutput:\n%s", err, string(out))
	}
}

// TestPhase18PinnedActions asserts every third-party action is
// pinned to a major version (`@v4`, `@v2`), never `@main` or
// `@latest`. An unpinned action is a supply-chain hole: the
// upstream repo can ship a malicious release that the workflow
// would silently consume on the next run.
//
// The pin policy is "major version tag" rather than "commit SHA"
// because the workflow is emitted as a starter template and the
// downstream user should be able to update across minor versions
// without re-running mochi. SHA pinning is a downstream hardening
// step; this gate catches the most common foot-gun (untagged
// references) at emit time.
func TestPhase18PinnedActions(t *testing.T) {
	body := readEmittedWorkflow(t)
	usesRE := regexp.MustCompile(`uses:\s*([^\s]+)`)
	matches := usesRE.FindAllStringSubmatch(body, -1)
	if len(matches) == 0 {
		t.Fatalf("workflow has no `uses:` lines:\n%s", body)
	}
	for _, m := range matches {
		ref := m[1]
		if !strings.Contains(ref, "@") {
			t.Errorf("workflow `uses: %s` has no @version pin", ref)
			continue
		}
		parts := strings.SplitN(ref, "@", 2)
		version := parts[1]
		if version == "main" || version == "master" || version == "latest" {
			t.Errorf("workflow `uses: %s` is pinned to mutable ref %q (supply-chain risk)", ref, version)
		}
	}
}

// TestPhase18ScaffoldReuse asserts the workflow target also
// produces the Phase 15 package scaffold (package.json + src/ +
// dist/). The release workflow refers to `mochi build
// --target=npm-package` which needs the .mochi source nearby;
// the scaffold is the artifact that's checked into the user's
// downstream repo.
func TestPhase18ScaffoldReuse(t *testing.T) {
	root := repoRoot(t)
	mochiPath := filepath.Join(root, "tests", "transpiler3", "typescript", "fixtures", "phase01-hello", "hello_int.mochi")
	outDir := t.TempDir()
	d := &Driver{CacheDir: t.TempDir(), NoCache: true}
	if _, err := d.Build(mochiPath, outDir, TargetReleaseWorkflow); err != nil {
		t.Fatalf("Build TargetReleaseWorkflow: %v", err)
	}
	for _, rel := range []string{
		"package.json",
		"src/index.ts",
		filepath.Join(".github", "workflows", "release.yml"),
	} {
		if _, err := os.Stat(filepath.Join(outDir, rel)); err != nil {
			t.Errorf("missing scaffold file %s: %v", rel, err)
		}
	}
}

// readEmittedWorkflow emits the release workflow for the hello
// fixture and returns its body as a string.
func readEmittedWorkflow(t *testing.T) string {
	t.Helper()
	root := repoRoot(t)
	mochiPath := filepath.Join(root, "tests", "transpiler3", "typescript", "fixtures", "phase01-hello", "hello_int.mochi")
	outDir := t.TempDir()
	d := &Driver{CacheDir: t.TempDir(), NoCache: true}
	workflowPath, err := d.Build(mochiPath, outDir, TargetReleaseWorkflow)
	if err != nil {
		t.Fatalf("Build TargetReleaseWorkflow: %v", err)
	}
	body, err := os.ReadFile(workflowPath)
	if err != nil {
		t.Fatalf("read workflow: %v", err)
	}
	return string(body)
}
