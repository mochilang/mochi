package build

import (
	"archive/zip"
	"fmt"
	"io"
	"io/fs"
	"os"
	"path/filepath"
	"sort"
	"strconv"
	"strings"
	"time"
)

// sourceDateEpoch reads SOURCE_DATE_EPOCH from the environment and returns the
// corresponding UTC time. If the variable is unset or invalid, returns the
// zero time (causing zip entries to use the zero timestamp, which is still
// deterministic across repeated builds on the same machine).
func sourceDateEpoch() time.Time {
	val := os.Getenv("SOURCE_DATE_EPOCH")
	if val == "" {
		return time.Time{}
	}
	sec, err := strconv.ParseInt(strings.TrimSpace(val), 10, 64)
	if err != nil {
		return time.Time{}
	}
	return time.Unix(sec, 0).UTC()
}

type jarEntry struct {
	name string
	data []byte
}

// PackUberJar creates a fat jar at outJar by merging:
//   - all .class files under classDir
//   - all .class files from runtimeJar (extracted)
//
// with the given mainClass as the Main-Class manifest entry.
// Entries are written in sorted order with timestamps from SOURCE_DATE_EPOCH
// to produce reproducible (bit-identical) output across repeated builds.
func PackUberJar(classDir, runtimeJar, outJar, mainClass string) error {
	ts := sourceDateEpoch()

	f, err := os.Create(outJar)
	if err != nil {
		return fmt.Errorf("uberjar: create %s: %w", outJar, err)
	}
	defer f.Close()

	w := zip.NewWriter(f)
	defer w.Close()

	// Write MANIFEST.MF with fixed timestamp.
	manifest := "Manifest-Version: 1.0\nMain-Class: " + mainClass + "\nImplementation-Version: 0.10.0\nBuilt-By: Mochi Transpiler\n"
	mfHeader := &zip.FileHeader{
		Name:     "META-INF/MANIFEST.MF",
		Method:   zip.Deflate,
		Modified: ts,
	}
	mf, err := w.CreateHeader(mfHeader)
	if err != nil {
		return fmt.Errorf("uberjar: manifest: %w", err)
	}
	if _, err := io.WriteString(mf, manifest); err != nil {
		return fmt.Errorf("uberjar: write manifest: %w", err)
	}

	seen := map[string]bool{"META-INF/MANIFEST.MF": true}
	var entries []jarEntry

	// Collect runtime jar classes.
	if runtimeJar != "" {
		if _, err := os.Stat(runtimeJar); err == nil {
			if err := collectJarEntries(runtimeJar, seen, &entries); err != nil {
				return fmt.Errorf("uberjar: extract runtime: %w", err)
			}
		}
	}

	// Collect user class files.
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
		entries = append(entries, jarEntry{name: rel, data: data})
		return nil
	}); err != nil {
		return fmt.Errorf("uberjar: walk classDir: %w", err)
	}

	// Sort entries for deterministic ordering.
	sort.Slice(entries, func(i, j int) bool {
		return entries[i].name < entries[j].name
	})

	// Write sorted entries with fixed timestamp.
	for _, e := range entries {
		hdr := &zip.FileHeader{
			Name:     e.name,
			Method:   zip.Deflate,
			Modified: ts,
		}
		ew, err := w.CreateHeader(hdr)
		if err != nil {
			return fmt.Errorf("uberjar: create entry %s: %w", e.name, err)
		}
		if _, err := ew.Write(e.data); err != nil {
			return fmt.Errorf("uberjar: write entry %s: %w", e.name, err)
		}
	}

	return nil
}

func collectJarEntries(jarPath string, seen map[string]bool, entries *[]jarEntry) error {
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
		*entries = append(*entries, jarEntry{name: name, data: data})
	}
	return nil
}
