package abi3

import (
	"fmt"
	"sort"
	"strings"
)

// LinkedLib is one entry from a C extension's dynamic-link table: the
// soname the linker recorded (e.g. "libc.so.6" on Linux,
// "/usr/lib/libSystem.B.dylib" on macOS, "KERNEL32.dll" on Windows) +
// the resolved on-disk path the loader would use (empty when not
// resolved). External is true when the lib is provided by the host
// system rather than vendored into the wheel; the auditor only judges
// external libs against the profile allow-list.
type LinkedLib struct {
	SoName   string
	Path     string
	External bool
}

// SymbolReader is the IO seam the auditor uses to walk a native
// extension's dynamic-link table. Production callers wire an ELF /
// Mach-O / PE reader (sub-phase 13.1); tests inject a fake.
type SymbolReader interface {
	Read(path string) ([]LinkedLib, error)
}

// AuditOptions bundles the profile to audit against + the reader to
// extract link information through.
type AuditOptions struct {
	Profile ManylinuxProfile
	Reader  SymbolReader
}

// AuditReport is the auditor's verdict on a single C extension.
// External holds the external lib set the auditor saw; Allowed and
// Disallowed partition them by the profile allow-list. Violations is
// the human-readable summary the CLI surfaces; OK is true iff
// Disallowed is empty and the profile was recognised.
type AuditReport struct {
	Path       string
	External   []LinkedLib
	Allowed    []LinkedLib
	Disallowed []LinkedLib
	Violations []string
	OK         bool
}

// AuditExtension reads the linked libs at path through opts.Reader and
// classifies them against opts.Profile. Internal libs (External=false)
// are recorded but never violate; external libs not in the profile's
// allow-list become violations.
//
// AuditExtension never inspects the filesystem itself — that is the
// SymbolReader's job. It returns an error only when Reader returns an
// error (missing file, unreadable header, ...) or when the profile is
// the zero value (caller forgot to look it up).
func AuditExtension(path string, opts AuditOptions) (*AuditReport, error) {
	if opts.Reader == nil {
		return nil, fmt.Errorf("abi3: AuditExtension requires a SymbolReader")
	}
	if opts.Profile.Tag == "" {
		return nil, fmt.Errorf("abi3: AuditExtension requires a non-empty profile")
	}
	libs, err := opts.Reader.Read(path)
	if err != nil {
		return nil, fmt.Errorf("abi3: read %q: %w", path, err)
	}
	rep := &AuditReport{Path: path}
	for _, l := range libs {
		if !l.External {
			continue
		}
		rep.External = append(rep.External, l)
		if opts.Profile.Allows(l.SoName) {
			rep.Allowed = append(rep.Allowed, l)
			continue
		}
		rep.Disallowed = append(rep.Disallowed, l)
		rep.Violations = append(rep.Violations,
			fmt.Sprintf("%s: external lib %q is not in the %s allow-list",
				path, l.SoName, opts.Profile.Tag))
	}
	sortLibsByName(rep.External)
	sortLibsByName(rep.Allowed)
	sortLibsByName(rep.Disallowed)
	sort.Strings(rep.Violations)
	rep.OK = len(rep.Disallowed) == 0
	return rep, nil
}

// AuditWheel audits every extension file in a wheel's name table. The
// caller supplies the file list (typically from wheel RECORD), and the
// auditor invokes AuditExtension against each .so / .dylib / .pyd. The
// aggregate report's OK flag is true iff every per-extension report
// was OK.
func AuditWheel(extensions []string, opts AuditOptions) ([]*AuditReport, bool, error) {
	reports := make([]*AuditReport, 0, len(extensions))
	allOK := true
	for _, ext := range extensions {
		if !IsExtensionFilename(ext) {
			continue
		}
		r, err := AuditExtension(ext, opts)
		if err != nil {
			return nil, false, err
		}
		reports = append(reports, r)
		if !r.OK {
			allOK = false
		}
	}
	return reports, allOK, nil
}

// IsExtensionFilename reports whether name has a C-extension suffix.
// The check is purely lexical; the auditor never opens the file.
func IsExtensionFilename(name string) bool {
	lower := strings.ToLower(name)
	for _, ext := range []string{".so", ".dylib", ".pyd"} {
		if strings.HasSuffix(lower, ext) {
			return true
		}
	}
	return false
}

func sortLibsByName(s []LinkedLib) {
	sort.Slice(s, func(i, j int) bool { return s[i].SoName < s[j].SoName })
}
