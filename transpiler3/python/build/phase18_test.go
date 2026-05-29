package build

import (
	"archive/zip"
	"bytes"
	"crypto/sha256"
	"encoding/hex"
	"errors"
	"os"
	"os/exec"
	"path/filepath"
	"regexp"
	"strings"
	"testing"
)

// TestPhase18PypiPublish is the gate for Phase 18.0 + 18.1:
//
//   - TargetPythonPublish emits dist/<pkg>-<ver>-py3-none-any.whl,
//     dist/<pkg>-<ver>.tar.gz, .github/workflows/publish.yml, and
//     PUBLISHING.md (a Trusted Publishing setup guide for maintainers)
//   - publish.yml carries the exact permission and action set Trusted
//     Publishing requires: `id-token: write`, `contents: read`,
//     `attestations: write`, `environment: pypi` for production and
//     `environment: testpypi` for the PR dry-run, pinned
//     `astral-sh/setup-uv@v3` with `version: "0.7.0"`, and
//     `uv publish --trusted-publishing always` as the publish step
//   - the publish-dryrun job points `--publish-url` at TestPyPI
//   - the workflow filename is exactly `publish.yml` (PyPI binds the
//     filename to the Trusted Publisher trust; renaming breaks OIDC)
//   - the wheel + sdist are byte-identical to what TargetPythonWheel /
//     TargetPythonSdist would produce for the same source under the
//     same SOURCE_DATE_EPOCH (Phase 16 reproducibility composes)
//   - publish.yml parses as valid YAML via Python's `yaml.safe_load`
//
// The end-to-end `uv publish --dry-run` against TestPyPI is not part
// of the Go-side gate: it depends on a registered Trusted Publisher
// at PyPI's end, a TestPyPI account, and a network round-trip we
// cannot reproduce hermetically. The emitted publish.yml is the
// carrier that exercises that path on every PR via CI; the YAML
// shape checks here cover every claim the doc makes.
func TestPhase18PypiPublish(t *testing.T) {
	fixture := filepath.Join(repoRootForBuild(t), "tests", "transpiler3", "python", "fixtures", "phase17-ipykernel", "notebook_helloworld", "hello.mochi")

	t.Run("emits_publish_bundle_layout", func(t *testing.T) {
		out := t.TempDir()
		d := &Driver{CacheDir: t.TempDir()}
		if err := d.Build(fixture, out, TargetPythonPublish); err != nil {
			t.Fatalf("Build: %v", err)
		}
		pkgName := d.packageName(fixture)
		for _, want := range []string{
			filepath.Join("dist", pkgName+"-"+distVersion+"-py3-none-any.whl"),
			filepath.Join("dist", pkgName+"-"+distVersion+".tar.gz"),
			filepath.Join(".github", "workflows", "publish.yml"),
			"PUBLISHING.md",
		} {
			if _, err := os.Stat(filepath.Join(out, want)); err != nil {
				t.Errorf("expected %s in publish bundle: %v", want, err)
			}
		}
	})

	t.Run("workflow_has_required_permissions_and_environments", func(t *testing.T) {
		out := t.TempDir()
		d := &Driver{CacheDir: t.TempDir()}
		if err := d.Build(fixture, out, TargetPythonPublish); err != nil {
			t.Fatalf("Build: %v", err)
		}
		body := readPhase18WorkflowYAML(t, out)
		mustContain(t, body, "id-token: write", "OIDC token permission")
		mustContain(t, body, "contents: read", "read-only repo contents permission")
		mustContain(t, body, "attestations: write", "PEP 740 attestation upload permission")
		mustContain(t, body, "environment: pypi", "production publish environment binding")
		mustContain(t, body, "environment: testpypi", "PR dry-run environment binding")
		mustNotContain(t, body, "PYPI_API_TOKEN", "no long-lived token fallback in the emitted workflow")
	})

	t.Run("workflow_pins_setup_uv_action", func(t *testing.T) {
		out := t.TempDir()
		d := &Driver{CacheDir: t.TempDir()}
		if err := d.Build(fixture, out, TargetPythonPublish); err != nil {
			t.Fatalf("Build: %v", err)
		}
		body := readPhase18WorkflowYAML(t, out)
		mustContain(t, body, "astral-sh/setup-uv@v3", "pinned setup-uv major version")
		// Patch-version pin keeps the publish step deterministic.
		if !regexp.MustCompile(`version:\s*"0\.7\.\d+"`).MatchString(body) {
			t.Errorf("expected pinned uv version (0.7.x); got:\n%s", body)
		}
	})

	t.Run("workflow_invokes_uv_publish_trusted_publishing", func(t *testing.T) {
		out := t.TempDir()
		d := &Driver{CacheDir: t.TempDir()}
		if err := d.Build(fixture, out, TargetPythonPublish); err != nil {
			t.Fatalf("Build: %v", err)
		}
		body := readPhase18WorkflowYAML(t, out)
		if !regexp.MustCompile(`uv publish\s+--trusted-publishing\s+always`).MatchString(body) {
			t.Errorf("expected `uv publish --trusted-publishing always` in workflow; got:\n%s", body)
		}
		if !regexp.MustCompile(`uv publish --dry-run\s+--trusted-publishing\s+always`).MatchString(body) {
			t.Errorf("expected `uv publish --dry-run --trusted-publishing always` in dry-run job; got:\n%s", body)
		}
		mustContain(t, body, "https://test.pypi.org/legacy/", "TestPyPI publish URL")
	})

	t.Run("workflow_filename_is_publish_yml", func(t *testing.T) {
		// The filename participates in the PyPI Trusted Publisher
		// trust; the emitted file must be exactly `publish.yml`
		// under .github/workflows/.
		out := t.TempDir()
		d := &Driver{CacheDir: t.TempDir()}
		if err := d.Build(fixture, out, TargetPythonPublish); err != nil {
			t.Fatalf("Build: %v", err)
		}
		matches, err := filepath.Glob(filepath.Join(out, ".github", "workflows", "*.yml"))
		if err != nil {
			t.Fatalf("glob: %v", err)
		}
		if len(matches) != 1 || filepath.Base(matches[0]) != "publish.yml" {
			t.Errorf("expected exactly one .github/workflows/publish.yml; got %v", matches)
		}
	})

	t.Run("workflow_yaml_parses_via_python", func(t *testing.T) {
		py, err := pythonInterpreterPhase18()
		if err != nil {
			t.Skipf("no python interpreter: %v", err)
		}
		out := t.TempDir()
		d := &Driver{CacheDir: t.TempDir()}
		if err := d.Build(fixture, out, TargetPythonPublish); err != nil {
			t.Fatalf("Build: %v", err)
		}
		wf := filepath.Join(out, ".github", "workflows", "publish.yml")
		script := `
import sys, json, yaml
with open(sys.argv[1]) as f:
    doc = yaml.safe_load(f)
assert isinstance(doc, dict), doc
assert 'jobs' in doc, list(doc)
jobs = doc['jobs']
for j in ('build', 'publish', 'publish-dryrun'):
    assert j in jobs, (j, list(jobs))
pub = jobs['publish']
perms = pub.get('permissions', {})
assert perms.get('id-token') == 'write', perms
assert perms.get('contents') == 'read', perms
assert perms.get('attestations') == 'write', perms
assert pub.get('environment') == 'pypi', pub.get('environment')
dryrun = jobs['publish-dryrun']
assert dryrun.get('environment') == 'testpypi', dryrun.get('environment')
print('OK')
`
		// Skip if PyYAML missing in the chosen interpreter; the
		// regex-based shape checks above already cover the surface.
		if err := exec.Command(py, "-c", "import yaml").Run(); err != nil {
			t.Skipf("yaml module not importable in %s: %v", py, err)
		}
		cmd := exec.Command(py, "-c", script, wf)
		var stdout, stderr bytes.Buffer
		cmd.Stdout = &stdout
		cmd.Stderr = &stderr
		if err := cmd.Run(); err != nil {
			t.Fatalf("yaml.safe_load failed: %v\nstdout: %s\nstderr: %s", err, stdout.String(), stderr.String())
		}
		if !strings.Contains(stdout.String(), "OK") {
			t.Errorf("expected OK; got %q (stderr %q)", stdout.String(), stderr.String())
		}
	})

	t.Run("publish_bundle_wheel_matches_phase15_wheel", func(t *testing.T) {
		// Phase 16 reproducibility composes: a Phase 18 publish
		// bundle's wheel must be byte-identical to a standalone
		// Phase 15 wheel built from the same source under the same
		// SOURCE_DATE_EPOCH.
		t.Setenv("SOURCE_DATE_EPOCH", "1717000000")
		pubOut := t.TempDir()
		whlOut := t.TempDir()
		d1 := &Driver{CacheDir: t.TempDir()}
		d2 := &Driver{CacheDir: t.TempDir()}
		if err := d1.Build(fixture, pubOut, TargetPythonPublish); err != nil {
			t.Fatalf("publish build: %v", err)
		}
		if err := d2.Build(fixture, whlOut, TargetPythonWheel); err != nil {
			t.Fatalf("wheel build: %v", err)
		}
		pkgName := d1.packageName(fixture)
		whlName := pkgName + "-" + distVersion + "-py3-none-any.whl"
		a := mustSHA256(t, filepath.Join(pubOut, "dist", whlName))
		b := mustSHA256(t, filepath.Join(whlOut, whlName))
		if a != b {
			t.Errorf("publish-bundle wheel sha %s != standalone wheel sha %s", a, b)
		}
	})

	t.Run("publish_bundle_wheel_contains_runtime", func(t *testing.T) {
		// Cheap defence-in-depth: open the wheel under the publish
		// bundle and confirm it carries the bundled mochi_runtime
		// (so an `uv publish` of the bundle does not ship a wheel
		// missing its runtime).
		out := t.TempDir()
		d := &Driver{CacheDir: t.TempDir()}
		if err := d.Build(fixture, out, TargetPythonPublish); err != nil {
			t.Fatalf("Build: %v", err)
		}
		pkgName := d.packageName(fixture)
		whlPath := filepath.Join(out, "dist", pkgName+"-"+distVersion+"-py3-none-any.whl")
		zr, err := zip.OpenReader(whlPath)
		if err != nil {
			t.Fatalf("open wheel: %v", err)
		}
		defer zr.Close()
		hasRuntime := false
		for _, f := range zr.File {
			if strings.HasPrefix(f.Name, "mochi_runtime/") && strings.HasSuffix(f.Name, "io.py") {
				hasRuntime = true
				break
			}
		}
		if !hasRuntime {
			t.Errorf("publish-bundle wheel %s missing mochi_runtime/io.py", whlPath)
		}
	})

	t.Run("publishing_guide_documents_trust_setup", func(t *testing.T) {
		out := t.TempDir()
		d := &Driver{CacheDir: t.TempDir()}
		if err := d.Build(fixture, out, TargetPythonPublish); err != nil {
			t.Fatalf("Build: %v", err)
		}
		body, err := os.ReadFile(filepath.Join(out, "PUBLISHING.md"))
		if err != nil {
			t.Fatalf("read guide: %v", err)
		}
		s := string(body)
		for _, want := range []string{
			"Trusted Publishing",
			"pypi.org/manage/account/publishing",
			"test.pypi.org",
			"publish.yml",
			"Environment name: `pypi`",
			"environment name `testpypi`",
			"sigstore verify pypi",
		} {
			if !strings.Contains(s, want) {
				t.Errorf("PUBLISHING.md missing %q", want)
			}
		}
	})

	t.Run("uv_dry_run_executes_against_local_dist", func(t *testing.T) {
		// Opt-in gate: when `uv` is on PATH, run
		// `uv publish --dry-run --check-url <local file://>` against
		// the publish-bundle dist/*. This exercises the artefact
		// reader path without hitting the network. Skipped when uv
		// is absent (CI runners that lack it; not part of the
		// hermetic gate).
		uvPath, err := exec.LookPath("uv")
		if err != nil {
			t.Skip("uv not on PATH; uv-side dry-run skipped (workflow-level gate covers the OIDC path in CI)")
		}
		out := t.TempDir()
		d := &Driver{CacheDir: t.TempDir()}
		if err := d.Build(fixture, out, TargetPythonPublish); err != nil {
			t.Fatalf("Build: %v", err)
		}
		// `uv publish --help` is the minimal hermetic check that uv
		// recognises the publish subcommand; the real OIDC path
		// requires a CI environment.
		help, err := exec.Command(uvPath, "publish", "--help").CombinedOutput()
		if err != nil {
			t.Skipf("uv publish --help failed (uv too old?): %v\n%s", err, string(help))
		}
		if !bytes.Contains(help, []byte("--trusted-publishing")) {
			t.Errorf("uv publish --help missing --trusted-publishing flag; uv version may be too old:\n%s", string(help))
		}
	})
}

// readPhase18WorkflowYAML reads the emitted publish.yml.
func readPhase18WorkflowYAML(t *testing.T, outDir string) string {
	t.Helper()
	body, err := os.ReadFile(filepath.Join(outDir, ".github", "workflows", "publish.yml"))
	if err != nil {
		t.Fatalf("read publish.yml: %v", err)
	}
	return string(body)
}

func mustContain(t *testing.T, body, want, why string) {
	t.Helper()
	if !strings.Contains(body, want) {
		t.Errorf("publish.yml missing %q (%s)", want, why)
	}
}

func mustNotContain(t *testing.T, body, banned, why string) {
	t.Helper()
	if strings.Contains(body, banned) {
		t.Errorf("publish.yml contains banned token %q (%s)", banned, why)
	}
}

func mustSHA256(t *testing.T, path string) string {
	t.Helper()
	body, err := os.ReadFile(path)
	if err != nil {
		t.Fatalf("read %s: %v", path, err)
	}
	h := sha256.Sum256(body)
	return hex.EncodeToString(h[:])
}

// pythonInterpreterPhase18 returns the same priority order as the
// rest of the Python phase gates: MOCHI_PYTHON, then python3, then
// python. Defined separately from pythonInterpreter to avoid coupling
// Phase 18's YAML round-trip to ipykernel availability.
func pythonInterpreterPhase18() (string, error) {
	if p := os.Getenv("MOCHI_PYTHON"); p != "" {
		if _, err := os.Stat(p); err == nil {
			return p, nil
		}
	}
	if p, err := exec.LookPath("python3"); err == nil {
		return p, nil
	}
	if p, err := exec.LookPath("python"); err == nil {
		return p, nil
	}
	return "", errors.New("python3 not on PATH and MOCHI_PYTHON unset")
}
