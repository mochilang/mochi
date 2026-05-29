// Phase 15: install-from-tarball execution gate.
//
// installAndRunBun runs `npm install <tarball>` into a fresh temp dir,
// then executes the installed package's main entry under Bun and
// captures stdout. Bun is the chosen primary runtime for the
// install-from-tarball gate because Bun executes `.ts` files inside
// `node_modules/` natively, with no compilation step required.
//
// Node 22 / 23+ has a hard restriction (ERR_UNSUPPORTED_NODE_MODULES_
// TYPE_STRIPPING) that forbids stripping TS types from files under
// `node_modules/`, so the bare-.ts dist that Phase 15.0 ships only
// runs from node_modules on Bun (and on Deno via the `npm:` specifier
// path; see sub-phase 15.2). The Node install-from-tarball gate
// lands in sub-phase 15.1 once `tsc --build` is wired into the
// pipeline and dist/node/index.js is real JavaScript.
//
// The install is offline-friendly (no registry lookup) because the
// tarball is a local path. `--no-save` avoids creating package-lock.
// `--no-audit` + `--no-fund` keep stdout/stderr quiet.
package build

import (
	"bytes"
	"encoding/json"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
)

// installAndRunBun unpacks tarballPath into a fresh node_modules
// under workDir, then runs `bun <pkg.main>` capturing stdout.
// Returns the captured stdout bytes.
//
// workDir is expected to be empty (caller uses t.TempDir()).
//
// Why Bun and not Node here: Node 23+ enforces ERR_UNSUPPORTED_NODE_
// MODULES_TYPE_STRIPPING for `.ts` under node_modules. Phase 15.0
// dist files are `.ts`. Bun has no such restriction; it parses and
// executes `.ts` from node_modules natively. The Node + Deno
// variants land with sub-phase 15.1 (tsc step) and 15.2 (`deno run
// npm:...` entry).
func installAndRunBun(workDir, tarballPath string) ([]byte, error) {
	if err := writeStubPackageJSON(workDir); err != nil {
		return nil, err
	}
	npm, err := resolveNpm()
	if err != nil {
		return nil, err
	}
	cmd := exec.Command(npm, "install", tarballPath,
		"--no-save", "--silent", "--no-audit", "--no-fund")
	cmd.Dir = workDir
	cmd.Env = os.Environ()
	if out, err := cmd.CombinedOutput(); err != nil {
		return nil, fmt.Errorf("npm install %s in %s failed: %w\n%s",
			tarballPath, workDir, err, string(out))
	}
	pkgName, pkgMain, err := readInstalledPackageMain(workDir)
	if err != nil {
		return nil, err
	}
	bun, err := resolveBun()
	if err != nil {
		return nil, err
	}
	entry := filepath.Join(workDir, "node_modules", pkgName, pkgMain)
	if _, err := os.Stat(entry); err != nil {
		return nil, fmt.Errorf("install gate: entry %s missing: %w", entry, err)
	}
	run := exec.Command(bun, entry)
	run.Env = os.Environ()
	var stdout, stderr bytes.Buffer
	run.Stdout = &stdout
	run.Stderr = &stderr
	if err := run.Run(); err != nil {
		return stdout.Bytes(), fmt.Errorf("bun %s failed: %w\nstderr:\n%s",
			entry, err, stderr.String())
	}
	return stdout.Bytes(), nil
}

// writeStubPackageJSON drops a minimal `{"type":"module"}` package.json
// in workDir so `npm install` doesn't complain about missing manifest
// and so `node` resolves the temp dir as an ESM module root.
func writeStubPackageJSON(workDir string) error {
	stub := []byte(`{"name":"mochi-install-gate","version":"0.0.0","type":"module","private":true}` + "\n")
	return os.WriteFile(filepath.Join(workDir, "package.json"), stub, 0o644)
}

// readInstalledPackageMain inspects workDir/node_modules/* to find
// the single installed mochi-* package and returns its name + main
// entry path (relative to the package root).
//
// The gate installs exactly one tarball into a fresh dir, so the
// node_modules tree has exactly one `mochi-*` subdir. If npm's
// resolver adds peer dep transitively, those siblings are skipped
// by the `mochi-` prefix filter.
func readInstalledPackageMain(workDir string) (string, string, error) {
	nm := filepath.Join(workDir, "node_modules")
	entries, err := os.ReadDir(nm)
	if err != nil {
		return "", "", fmt.Errorf("install gate: read %s: %w", nm, err)
	}
	for _, e := range entries {
		if !e.IsDir() {
			continue
		}
		name := e.Name()
		if len(name) < 6 || name[:6] != "mochi-" {
			continue
		}
		pkgPath := filepath.Join(nm, name, "package.json")
		raw, err := os.ReadFile(pkgPath)
		if err != nil {
			return "", "", fmt.Errorf("install gate: read %s: %w", pkgPath, err)
		}
		var manifest struct {
			Main string `json:"main"`
		}
		if err := json.Unmarshal(raw, &manifest); err != nil {
			return "", "", fmt.Errorf("install gate: parse %s: %w", pkgPath, err)
		}
		if manifest.Main == "" {
			return "", "", fmt.Errorf("install gate: %s has no main field", pkgPath)
		}
		return name, manifest.Main, nil
	}
	return "", "", fmt.Errorf("install gate: no mochi-* package found under %s", nm)
}
