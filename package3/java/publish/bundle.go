package publish

import (
	"archive/zip"
	"bytes"
	"crypto/md5"
	"crypto/sha1"
	"encoding/hex"
	"fmt"
	"io"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
)

// BundleSpec describes the artifact files to bundle for upload.
type BundleSpec struct {
	// GroupID is the Maven groupId (e.g. "dev.mochi.java-bridge").
	GroupID string
	// ArtifactID is the Maven artifactId.
	ArtifactID string
	// Version is the release version.
	Version string
	// JARPath is the absolute path to the compiled wrapper JAR.
	JARPath string
	// POMPath is the absolute path to the POM XML.
	POMPath string
	// SourcesJARPath is the optional -sources.jar (empty = skip).
	SourcesJARPath string
	// GPGKeyID is the key fingerprint to sign with (requires gpg on PATH).
	// If empty, signing is skipped.
	GPGKeyID string
}

// BundleResult is the path to the produced ZIP bundle.
type BundleResult struct {
	BundlePath string
}

// BuildBundle assembles the Sonatype Central Portal upload ZIP.
// It includes the JAR, POM, their SHA-1/MD5 checksums, and GPG .asc
// signatures when GPGKeyID is set.
func BuildBundle(spec BundleSpec, outDir string) (*BundleResult, error) {
	if err := validateBundleSpec(spec); err != nil {
		return nil, err
	}
	if err := os.MkdirAll(outDir, 0o755); err != nil {
		return nil, fmt.Errorf("bundle: mkdir: %w", err)
	}

	bundleName := fmt.Sprintf("%s-%s-bundle.zip", spec.ArtifactID, spec.Version)
	bundlePath := filepath.Join(outDir, bundleName)

	f, err := os.Create(bundlePath)
	if err != nil {
		return nil, fmt.Errorf("bundle: create zip: %w", err)
	}
	defer f.Close()

	zw := zip.NewWriter(f)

	groupPath := strings.ReplaceAll(spec.GroupID, ".", "/")
	prefix := fmt.Sprintf("%s/%s/%s", groupPath, spec.ArtifactID, spec.Version)
	base := fmt.Sprintf("%s-%s", spec.ArtifactID, spec.Version)

	type entry struct {
		src  string
		name string
	}
	entries := []entry{
		{spec.JARPath, base + ".jar"},
		{spec.POMPath, base + ".pom"},
	}
	if spec.SourcesJARPath != "" {
		if _, err := os.Stat(spec.SourcesJARPath); err == nil {
			entries = append(entries, entry{spec.SourcesJARPath, base + "-sources.jar"})
		}
	}

	for _, e := range entries {
		data, err := os.ReadFile(e.src)
		if err != nil {
			return nil, fmt.Errorf("bundle: read %s: %w", e.src, err)
		}
		if err := addToZip(zw, prefix+"/"+e.name, data); err != nil {
			return nil, err
		}
		sha1sum := sha1hex(data)
		if err := addToZip(zw, prefix+"/"+e.name+".sha1", []byte(sha1sum)); err != nil {
			return nil, err
		}
		md5sum := md5hex(data)
		if err := addToZip(zw, prefix+"/"+e.name+".md5", []byte(md5sum)); err != nil {
			return nil, err
		}
		if spec.GPGKeyID != "" {
			asc, err := gpgSign(data, spec.GPGKeyID)
			if err != nil {
				return nil, fmt.Errorf("bundle: gpg sign %s: %w", e.name, err)
			}
			if err := addToZip(zw, prefix+"/"+e.name+".asc", asc); err != nil {
				return nil, err
			}
		}
	}

	if err := zw.Close(); err != nil {
		return nil, fmt.Errorf("bundle: close zip: %w", err)
	}
	return &BundleResult{BundlePath: bundlePath}, nil
}

// DryRun validates a bundle ZIP without uploading: checks it can be opened,
// all required entries are present, and checksums match.
func DryRun(bundlePath, groupID, artifactID, version string) error {
	r, err := zip.OpenReader(bundlePath)
	if err != nil {
		return fmt.Errorf("dry-run: open zip: %w", err)
	}
	defer r.Close()

	groupPath := strings.ReplaceAll(groupID, ".", "/")
	prefix := fmt.Sprintf("%s/%s/%s", groupPath, artifactID, version)
	base := fmt.Sprintf("%s-%s", artifactID, version)

	required := []string{
		prefix + "/" + base + ".jar",
		prefix + "/" + base + ".pom",
		prefix + "/" + base + ".jar.sha1",
		prefix + "/" + base + ".pom.sha1",
	}

	index := map[string]*zip.File{}
	for _, f := range r.File {
		index[f.Name] = f
	}

	for _, name := range required {
		if _, ok := index[name]; !ok {
			return fmt.Errorf("dry-run: missing required entry %q", name)
		}
	}

	for _, suffix := range []string{".jar", ".pom"} {
		dataFile := index[prefix+"/"+base+suffix]
		sha1File := index[prefix+"/"+base+suffix+".sha1"]
		if dataFile == nil || sha1File == nil {
			continue
		}
		data, err := readZipEntry(dataFile)
		if err != nil {
			return fmt.Errorf("dry-run: read %s: %w", dataFile.Name, err)
		}
		sha1Stored, err := readZipEntry(sha1File)
		if err != nil {
			return fmt.Errorf("dry-run: read %s: %w", sha1File.Name, err)
		}
		if sha1hex(data) != strings.TrimSpace(string(sha1Stored)) {
			return fmt.Errorf("dry-run: SHA-1 mismatch for %s%s", base, suffix)
		}
	}
	return nil
}

func addToZip(zw *zip.Writer, name string, data []byte) error {
	w, err := zw.Create(name)
	if err != nil {
		return fmt.Errorf("zip add %s: %w", name, err)
	}
	_, err = w.Write(data)
	return err
}

func readZipEntry(f *zip.File) ([]byte, error) {
	rc, err := f.Open()
	if err != nil {
		return nil, err
	}
	defer rc.Close()
	return io.ReadAll(rc)
}

func sha1hex(data []byte) string {
	h := sha1.Sum(data)
	return hex.EncodeToString(h[:])
}

func md5hex(data []byte) string {
	h := md5.Sum(data)
	return hex.EncodeToString(h[:])
}

// execGPG returns an exec.Cmd for running gpg with args.
var execGPG = func(args ...string) *exec.Cmd {
	return exec.Command("gpg", args...)
}

func gpgSign(data []byte, keyID string) ([]byte, error) {
	args := []string{
		"--batch", "--yes", "--armor", "--detach-sign",
		"--local-user", keyID,
		"--output", "-",
		"-",
	}
	cmd := execGPG(args...)
	cmd.Stdin = bytes.NewReader(data)
	out, err := cmd.Output()
	if err != nil {
		return nil, fmt.Errorf("gpg sign: %w", err)
	}
	return out, nil
}

func validateBundleSpec(spec BundleSpec) error {
	if spec.GroupID == "" {
		return fmt.Errorf("bundle: GroupID is required")
	}
	if spec.ArtifactID == "" {
		return fmt.Errorf("bundle: ArtifactID is required")
	}
	if spec.Version == "" {
		return fmt.Errorf("bundle: Version is required")
	}
	if spec.JARPath == "" {
		return fmt.Errorf("bundle: JARPath is required")
	}
	if spec.POMPath == "" {
		return fmt.Errorf("bundle: POMPath is required")
	}
	return nil
}
