package build

import (
	"archive/tar"
	"archive/zip"
	"compress/gzip"
	"crypto/sha256"
	"encoding/base64"
	"fmt"
	"os"
	"path/filepath"
	"sort"
	"strings"
	"time"
)

// Phase 15.0: build a PEP 427 wheel and a PEP 517 sdist from the same
// generated `src/<pkg>/` tree the source target produces. Both formats
// are built by walking the tree and packaging into zip / tar.gz in
// Go; no `python -m build`, no `pip wheel`, and no third-party build
// backend (hatchling / setuptools) are required at build or test
// time. The runtime support package (`mochi_runtime`) is bundled
// into the wheel as a sibling top-level package so a fresh install
// has no runtime dependency to resolve.
//
// Wheel structure (PEP 427):
//
//	<pkg>/...           user package
//	mochi_runtime/...   bundled runtime
//	<pkg>-0.1.0.dist-info/
//	  METADATA          PEP 621 + PEP 345 metadata
//	  WHEEL             wheel format marker
//	  RECORD            file digest + size manifest
//
// Sdist structure (PEP 517):
//
//	<pkg>-0.1.0/
//	  pyproject.toml
//	  PKG-INFO          mirrors METADATA
//	  src/<pkg>/...
//	  src/mochi_runtime/...
//
// Naming follows PEP 427 (`<distribution>-<version>-py3-none-any.whl`)
// and PEP 625 (`<distribution>-<version>.tar.gz`).

const wheelVersion = "1.0"
const distVersion = "0.1.0"

// buildWheel produces `<pkg>-<version>-py3-none-any.whl` under outDir
// from the populated workDir source tree. The runtime is copied in
// from rtDir so the wheel ships self-contained.
func buildWheel(outDir, workDir, rtDir, pkgName string) (string, error) {
	stageDir, err := os.MkdirTemp("", "mochi-wheel-stage-*")
	if err != nil {
		return "", err
	}
	defer os.RemoveAll(stageDir)

	// User package: src/<pkg>/  ->  <pkg>/  inside the zip.
	srcPkg := filepath.Join(workDir, "src", pkgName)
	if err := copyTreeFiltered(filepath.Join(stageDir, pkgName), srcPkg, nil); err != nil {
		return "", fmt.Errorf("stage user package: %w", err)
	}

	// Phase 12 sidecar: src/<pkg>_externs.py (if present) ships
	// alongside the user package at the same zip root.
	externs := filepath.Join(workDir, "src", pkgName+"_externs.py")
	if _, err := os.Stat(externs); err == nil {
		if err := copyFile(filepath.Join(stageDir, pkgName+"_externs.py"), externs); err != nil {
			return "", fmt.Errorf("stage externs sidecar: %w", err)
		}
	}

	// Bundled runtime: runtime/python/mochi_runtime/  ->  mochi_runtime/.
	if err := copyTreeFiltered(filepath.Join(stageDir, "mochi_runtime"), filepath.Join(rtDir, "mochi_runtime"), pyOnlyFilter); err != nil {
		return "", fmt.Errorf("stage mochi_runtime: %w", err)
	}

	distInfo := fmt.Sprintf("%s-%s.dist-info", pkgName, distVersion)
	if err := os.MkdirAll(filepath.Join(stageDir, distInfo), 0o755); err != nil {
		return "", err
	}
	metadata := wheelMetadata(pkgName, distVersion)
	wheelFile := wheelFormatFile()
	if err := os.WriteFile(filepath.Join(stageDir, distInfo, "METADATA"), []byte(metadata), 0o644); err != nil {
		return "", err
	}
	if err := os.WriteFile(filepath.Join(stageDir, distInfo, "WHEEL"), []byte(wheelFile), 0o644); err != nil {
		return "", err
	}

	// RECORD enumerates every file with sha256 digest + byte size.
	// PEP 376 § Record format.
	record, err := buildRecord(stageDir, distInfo+"/RECORD")
	if err != nil {
		return "", fmt.Errorf("build RECORD: %w", err)
	}
	if err := os.WriteFile(filepath.Join(stageDir, distInfo, "RECORD"), []byte(record), 0o644); err != nil {
		return "", err
	}

	if err := os.MkdirAll(outDir, 0o755); err != nil {
		return "", err
	}
	wheelName := fmt.Sprintf("%s-%s-py3-none-any.whl", pkgName, distVersion)
	wheelPath := filepath.Join(outDir, wheelName)
	if err := zipDir(wheelPath, stageDir); err != nil {
		return "", fmt.Errorf("zip wheel: %w", err)
	}
	return wheelPath, nil
}

// buildSdist produces `<pkg>-<version>.tar.gz` under outDir from the
// populated workDir source tree. PEP 517 / PEP 625 layout.
func buildSdist(outDir, workDir, rtDir, pkgName string) (string, error) {
	stageDir, err := os.MkdirTemp("", "mochi-sdist-stage-*")
	if err != nil {
		return "", err
	}
	defer os.RemoveAll(stageDir)

	top := fmt.Sprintf("%s-%s", pkgName, distVersion)
	root := filepath.Join(stageDir, top)
	if err := os.MkdirAll(filepath.Join(root, "src"), 0o755); err != nil {
		return "", err
	}

	if err := copyTreeFiltered(filepath.Join(root, "src", pkgName), filepath.Join(workDir, "src", pkgName), nil); err != nil {
		return "", err
	}
	externs := filepath.Join(workDir, "src", pkgName+"_externs.py")
	if _, err := os.Stat(externs); err == nil {
		if err := copyFile(filepath.Join(root, "src", pkgName+"_externs.py"), externs); err != nil {
			return "", err
		}
	}
	if err := copyTreeFiltered(filepath.Join(root, "src", "mochi_runtime"), filepath.Join(rtDir, "mochi_runtime"), pyOnlyFilter); err != nil {
		return "", err
	}
	if err := copyFile(filepath.Join(root, "pyproject.toml"), filepath.Join(workDir, "pyproject.toml")); err != nil {
		return "", err
	}
	pkgInfo := wheelMetadata(pkgName, distVersion)
	if err := os.WriteFile(filepath.Join(root, "PKG-INFO"), []byte(pkgInfo), 0o644); err != nil {
		return "", err
	}

	if err := os.MkdirAll(outDir, 0o755); err != nil {
		return "", err
	}
	sdistName := fmt.Sprintf("%s-%s.tar.gz", pkgName, distVersion)
	sdistPath := filepath.Join(outDir, sdistName)
	if err := tarGzDir(sdistPath, stageDir); err != nil {
		return "", fmt.Errorf("tar.gz sdist: %w", err)
	}
	return sdistPath, nil
}

// wheelMetadata renders a PEP 621 + PEP 345 METADATA file. Fields kept
// minimal: name, version, python requirement, summary. Mochi v1 does
// not surface the user-program description through to the metadata.
func wheelMetadata(pkgName, version string) string {
	return strings.Join([]string{
		"Metadata-Version: 2.3",
		"Name: " + pkgName,
		"Version: " + version,
		"Summary: Mochi program packaged for Python (MEP-51 Phase 15).",
		"Requires-Python: >=3.12",
		"",
		"",
	}, "\n")
}

// wheelFormatFile renders the PEP 427 WHEEL marker. The tag
// `py3-none-any` declares pure-Python, no ABI, any platform; matches
// the wheel filename suffix.
func wheelFormatFile() string {
	return strings.Join([]string{
		"Wheel-Version: " + wheelVersion,
		"Generator: mochi-python-mep51 0.1.0",
		"Root-Is-Purelib: true",
		"Tag: py3-none-any",
		"",
		"",
	}, "\n")
}

// buildRecord computes the PEP 376 RECORD manifest: for each file in
// stageDir (sorted, posix path), `<path>,sha256=<base64>,<size>`. The
// RECORD file itself appears with empty digest + size to break the
// self-reference cycle, exactly as `pip wheel` produces.
func buildRecord(stageDir, recordPosixPath string) (string, error) {
	var paths []string
	err := filepath.Walk(stageDir, func(p string, info os.FileInfo, err error) error {
		if err != nil {
			return err
		}
		if info.IsDir() {
			return nil
		}
		rel, err := filepath.Rel(stageDir, p)
		if err != nil {
			return err
		}
		rel = filepath.ToSlash(rel)
		paths = append(paths, rel)
		return nil
	})
	if err != nil {
		return "", err
	}
	// The RECORD self-line must be present even though the file does
	// not exist on disk at compute time. Inject the path into the
	// list so the sort interleaves it lex-correctly with the rest;
	// PEP 376 plus reproducibility tooling (hatchling, pip wheel) all
	// emit RECORD lines in lex order, and the Phase 16 gate enforces
	// the same.
	hasRecord := false
	for _, p := range paths {
		if p == recordPosixPath {
			hasRecord = true
			break
		}
	}
	if !hasRecord {
		paths = append(paths, recordPosixPath)
	}
	sort.Strings(paths)

	var b strings.Builder
	for _, rel := range paths {
		if rel == recordPosixPath {
			fmt.Fprintf(&b, "%s,,\n", rel)
			continue
		}
		data, err := os.ReadFile(filepath.Join(stageDir, filepath.FromSlash(rel)))
		if err != nil {
			return "", err
		}
		sum := sha256.Sum256(data)
		digest := strings.TrimRight(base64.URLEncoding.EncodeToString(sum[:]), "=")
		fmt.Fprintf(&b, "%s,sha256=%s,%d\n", rel, digest, len(data))
	}
	return b.String(), nil
}

// zipDir writes a deterministic zip archive from stageDir at outPath.
// Files are sorted, mtimes are zeroed, mode bits are normalized; the
// resulting archive is suitable for the Phase 16 reproducible-build
// gate (byte-equal across runs).
func zipDir(outPath, stageDir string) error {
	out, err := os.Create(outPath)
	if err != nil {
		return err
	}
	defer out.Close()

	zw := zip.NewWriter(out)
	defer zw.Close()

	var paths []string
	err = filepath.Walk(stageDir, func(p string, info os.FileInfo, err error) error {
		if err != nil {
			return err
		}
		if info.IsDir() {
			return nil
		}
		rel, err := filepath.Rel(stageDir, p)
		if err != nil {
			return err
		}
		paths = append(paths, filepath.ToSlash(rel))
		return nil
	})
	if err != nil {
		return err
	}
	sort.Strings(paths)

	for _, rel := range paths {
		data, err := os.ReadFile(filepath.Join(stageDir, filepath.FromSlash(rel)))
		if err != nil {
			return err
		}
		hdr := &zip.FileHeader{Name: rel, Method: zip.Deflate}
		// Zero out mtime for reproducibility (Phase 16 prep).
		hdr.SetModTime(zeroTime())
		hdr.SetMode(0o644)
		w, err := zw.CreateHeader(hdr)
		if err != nil {
			return err
		}
		if _, err := w.Write(data); err != nil {
			return err
		}
	}
	return nil
}

// tarGzDir writes a deterministic gzip-compressed tar archive from
// stageDir at outPath. Same reproducibility properties as zipDir.
func tarGzDir(outPath, stageDir string) error {
	out, err := os.Create(outPath)
	if err != nil {
		return err
	}
	defer out.Close()

	gz := gzip.NewWriter(out)
	// Zero the gzip header's mtime so two runs produce the same first
	// 10 bytes (Phase 16). gzip.Writer.ModTime defaults to time.Now()
	// only when Header is not explicitly set.
	gz.ModTime = zeroTime()
	defer gz.Close()

	tw := tar.NewWriter(gz)
	defer tw.Close()

	var paths []string
	err = filepath.Walk(stageDir, func(p string, info os.FileInfo, err error) error {
		if err != nil {
			return err
		}
		if info.IsDir() {
			return nil
		}
		rel, err := filepath.Rel(stageDir, p)
		if err != nil {
			return err
		}
		paths = append(paths, filepath.ToSlash(rel))
		return nil
	})
	if err != nil {
		return err
	}
	sort.Strings(paths)

	for _, rel := range paths {
		data, err := os.ReadFile(filepath.Join(stageDir, filepath.FromSlash(rel)))
		if err != nil {
			return err
		}
		hdr := &tar.Header{
			Name:    rel,
			Mode:    0o644,
			Size:    int64(len(data)),
			ModTime: zeroTime(),
			Format:  tar.FormatUSTAR,
		}
		if err := tw.WriteHeader(hdr); err != nil {
			return err
		}
		if _, err := tw.Write(data); err != nil {
			return err
		}
	}
	return nil
}

// pyOnlyFilter keeps .py files and py.typed; drops __pycache__ and
// .pyc so the wheel does not ship build-host bytecode (which is the
// reproducibility-killer for vanilla `pip install`).
func pyOnlyFilter(rel string) bool {
	rel = filepath.ToSlash(rel)
	if strings.Contains(rel, "/__pycache__/") || strings.HasPrefix(rel, "__pycache__/") {
		return false
	}
	if strings.HasSuffix(rel, ".pyc") {
		return false
	}
	return true
}

// copyTreeFiltered copies srcDir into dstDir. Files where filter(rel)
// returns false are skipped. A nil filter copies every file.
func copyTreeFiltered(dstDir, srcDir string, filter func(rel string) bool) error {
	return filepath.Walk(srcDir, func(p string, info os.FileInfo, err error) error {
		if err != nil {
			return err
		}
		rel, err := filepath.Rel(srcDir, p)
		if err != nil {
			return err
		}
		if filter != nil && !filter(rel) {
			return nil
		}
		target := filepath.Join(dstDir, rel)
		if info.IsDir() {
			return os.MkdirAll(target, info.Mode())
		}
		return copyFile(target, p)
	})
}

// zeroTime is the reproducible mtime stamped on every wheel/sdist
// entry. Phase 16.0: honours `$SOURCE_DATE_EPOCH` per the
// reproducible-builds.org spec; falls back to 1980-01-01 (the zip
// format's DOS-encoded mtime epoch floor) when unset or malformed.
// The Phase 16 gate compares wheel bytes across two builds; any
// source-of-non-determinism (mtime, file order, gzip header) must
// be pinned here, not at the call site.
func zeroTime() time.Time {
	return sourceDateEpoch()
}
