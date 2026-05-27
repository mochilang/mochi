package build

import (
	"archive/zip"
	"fmt"
	"io"
	"io/fs"
	"os"
	"path/filepath"
	"strings"
)

// PackUberJar creates a fat jar at outJar by merging:
//   - all .class files under classDir
//   - all .class files from runtimeJar (extracted)
//
// with the given mainClass as the Main-Class manifest entry.
func PackUberJar(classDir, runtimeJar, outJar, mainClass string) error {
	// Create output jar
	f, err := os.Create(outJar)
	if err != nil {
		return fmt.Errorf("uberjar: create %s: %w", outJar, err)
	}
	defer f.Close()

	w := zip.NewWriter(f)
	defer w.Close()

	// Write MANIFEST.MF
	manifest := "Manifest-Version: 1.0\nMain-Class: " + mainClass + "\nImplementation-Version: 0.10.0\nBuilt-By: Mochi Transpiler\n"
	mf, err := w.Create("META-INF/MANIFEST.MF")
	if err != nil {
		return fmt.Errorf("uberjar: manifest: %w", err)
	}
	if _, err := io.WriteString(mf, manifest); err != nil {
		return fmt.Errorf("uberjar: write manifest: %w", err)
	}

	seen := map[string]bool{"META-INF/MANIFEST.MF": true}

	// Extract runtime jar classes first
	if runtimeJar != "" {
		if _, err := os.Stat(runtimeJar); err == nil {
			if err := extractJarTo(w, runtimeJar, seen); err != nil {
				return fmt.Errorf("uberjar: extract runtime: %w", err)
			}
		}
	}

	// Add user class files
	if err := filepath.WalkDir(classDir, func(path string, d fs.DirEntry, err error) error {
		if err != nil || d.IsDir() {
			return err
		}
		rel, _ := filepath.Rel(classDir, path)
		rel = filepath.ToSlash(rel)
		if seen[rel] {
			return nil
		}
		seen[rel] = true
		data, err := os.ReadFile(path)
		if err != nil {
			return err
		}
		entry, err := w.Create(rel)
		if err != nil {
			return err
		}
		_, err = entry.Write(data)
		return err
	}); err != nil {
		return fmt.Errorf("uberjar: walk classDir: %w", err)
	}

	return nil
}

func extractJarTo(w *zip.Writer, jarPath string, seen map[string]bool) error {
	r, err := zip.OpenReader(jarPath)
	if err != nil {
		return err
	}
	defer r.Close()

	for _, f := range r.File {
		name := f.Name
		if strings.HasSuffix(name, "/") || name == "META-INF/MANIFEST.MF" {
			continue
		}
		if seen[name] {
			continue
		}
		seen[name] = true

		rc, err := f.Open()
		if err != nil {
			return err
		}
		data, err := io.ReadAll(rc)
		rc.Close()
		if err != nil {
			return err
		}
		entry, err := w.Create(name)
		if err != nil {
			return err
		}
		if _, err := entry.Write(data); err != nil {
			return err
		}
	}
	return nil
}
