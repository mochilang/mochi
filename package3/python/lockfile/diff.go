package lockfile

import (
	"fmt"
	"sort"
	"strings"
)

// DiffKind classifies how two manifest entries differ.
type DiffKind int

const (
	// DiffAdded marks an entry present in the new manifest only.
	DiffAdded DiffKind = iota
	// DiffRemoved marks an entry present in the old manifest only.
	DiffRemoved
	// DiffChanged marks an entry whose fields changed between manifests.
	DiffChanged
)

func (k DiffKind) String() string {
	switch k {
	case DiffAdded:
		return "added"
	case DiffRemoved:
		return "removed"
	case DiffChanged:
		return "changed"
	}
	return "unknown"
}

// DiffEntry is one delta line in a manifest comparison. Old is nil for added
// entries; New is nil for removed entries; both are set for changed entries
// and the Fields slice lists the keys that diverged.
type DiffEntry struct {
	Kind   DiffKind
	Key    string
	Old    *Entry
	New    *Entry
	Fields []string
}

// String renders the DiffEntry in a single-line `kind key (fields)` form
// suitable for the `mochi pkg lock --check` exit summary.
func (de DiffEntry) String() string {
	if len(de.Fields) > 0 {
		return fmt.Sprintf("%s %s (%s)", de.Kind, de.Key, strings.Join(de.Fields, ","))
	}
	return fmt.Sprintf("%s %s", de.Kind, de.Key)
}

// Diff is the ordered list of differences between two manifests.
type Diff struct {
	Entries []DiffEntry
}

// Empty reports whether the two manifests are identical.
func (d Diff) Empty() bool { return len(d.Entries) == 0 }

// String renders the diff as a newline-joined block.
func (d Diff) String() string {
	parts := make([]string, len(d.Entries))
	for i, e := range d.Entries {
		parts[i] = e.String()
	}
	return strings.Join(parts, "\n")
}

// CompareManifests returns the Diff between old (the on-disk lockfile) and
// new (the freshly-Planned manifest). The comparison key is
// `<name>:<alias>`.
func CompareManifests(old, new Manifest) Diff {
	keyOld := indexByKey(old)
	keyNew := indexByKey(new)
	allKeys := mergedKeys(keyOld, keyNew)
	d := Diff{}
	for _, k := range allKeys {
		o, oOK := keyOld[k]
		n, nOK := keyNew[k]
		switch {
		case oOK && nOK:
			if fields := entryFieldDiff(o, n); len(fields) > 0 {
				oCopy, nCopy := o, n
				d.Entries = append(d.Entries, DiffEntry{Kind: DiffChanged, Key: k, Old: &oCopy, New: &nCopy, Fields: fields})
			}
		case nOK:
			nCopy := n
			d.Entries = append(d.Entries, DiffEntry{Kind: DiffAdded, Key: k, New: &nCopy})
		case oOK:
			oCopy := o
			d.Entries = append(d.Entries, DiffEntry{Kind: DiffRemoved, Key: k, Old: &oCopy})
		}
	}
	return d
}

// Check returns nil when the two manifests are identical, an error containing
// the rendered Diff otherwise. This is the kernel of `mochi pkg lock --check`.
func Check(old, new Manifest) error {
	d := CompareManifests(old, new)
	if d.Empty() {
		return nil
	}
	return fmt.Errorf("lockfile drift:\n%s", d.String())
}

func indexByKey(m Manifest) map[string]Entry {
	idx := make(map[string]Entry, len(m.Entries))
	for _, e := range m.Entries {
		idx[e.Name+":"+e.Alias] = e
	}
	return idx
}

func mergedKeys(a, b map[string]Entry) []string {
	set := map[string]struct{}{}
	for k := range a {
		set[k] = struct{}{}
	}
	for k := range b {
		set[k] = struct{}{}
	}
	out := make([]string, 0, len(set))
	for k := range set {
		out = append(out, k)
	}
	sort.Strings(out)
	return out
}

func entryFieldDiff(a, b Entry) []string {
	var f []string
	if a.Source != b.Source {
		f = append(f, "source")
	}
	if a.Version != b.Version {
		f = append(f, "version")
	}
	if a.Specifier != b.Specifier {
		f = append(f, "specifier")
	}
	if a.IndexURL != b.IndexURL {
		f = append(f, "index-url")
	}
	if a.GitURL != b.GitURL {
		f = append(f, "git-url")
	}
	if a.GitRev != b.GitRev {
		f = append(f, "git-rev")
	}
	if a.LocalPath != b.LocalPath {
		f = append(f, "local-path")
	}
	if a.WrapperSHA256 != b.WrapperSHA256 {
		f = append(f, "wrapper-sha256")
	}
	if !stringSliceEqual(a.Capabilities, b.Capabilities) {
		f = append(f, "capabilities")
	}
	return f
}

func stringSliceEqual(a, b []string) bool {
	if len(a) != len(b) {
		return false
	}
	for i := range a {
		if a[i] != b[i] {
			return false
		}
	}
	return true
}
