package lockfile

import "sort"

// SchemaVersion is the currently emitted lockfile schema number. Bump when
// the entry shape changes; Parse refuses any version it does not know.
const SchemaVersion = 1

// Source classifies how the entry was resolved. Mirrors importspec.Source
// but uses string tokens so the TOML on-disk form is human-readable.
type Source string

const (
	SourceRegistry Source = "registry"
	SourceIndex    Source = "index"
	SourceGit      Source = "git"
	SourcePath     Source = "path"
)

// Valid reports whether s is one of the four known sources.
func (s Source) Valid() bool {
	switch s {
	case SourceRegistry, SourceIndex, SourceGit, SourcePath:
		return true
	}
	return false
}

// Entry is one `[[python-package]]` table.
type Entry struct {
	// Name is the PEP 503-normalised distribution name.
	Name string
	// Alias is the Mochi alias the user picked in `import python ... as alias`.
	Alias string
	// Source is the resolution source.
	Source Source
	// Version is the exact resolved version for SourceRegistry / SourceIndex.
	// Empty for SourceGit (use GitRev) and SourcePath.
	Version string
	// Specifier preserves the original PEP 440 spec from the import body.
	Specifier string
	// IndexURL is set when Source = SourceIndex.
	IndexURL string
	// GitURL is set when Source = SourceGit.
	GitURL string
	// GitRev is set when Source = SourceGit.
	GitRev string
	// LocalPath is set when Source = SourcePath.
	LocalPath string
	// WrapperSHA256 is the Phase 6 emit.Shim.SHA256, hex-encoded.
	WrapperSHA256 string
	// Capabilities is the sorted, deduped list of capability tokens used by
	// the wrapped surface. The list shape is documented in
	// capabilities.go::ExtractCapabilities.
	Capabilities []string
}

// Manifest is the full `[[python-package]]` array for a workspace.
type Manifest struct {
	// Version is the schema version. Always SchemaVersion when produced by
	// this package; preserved when round-tripped through Parse for forward
	// compatibility reporting.
	Version int
	// Entries is sorted by (Name, Alias) for determinism.
	Entries []Entry
}

// Sort sorts the entries in canonical order: (Name, Alias) ascending.
func (m *Manifest) Sort() {
	sort.SliceStable(m.Entries, func(i, j int) bool {
		if m.Entries[i].Name != m.Entries[j].Name {
			return m.Entries[i].Name < m.Entries[j].Name
		}
		return m.Entries[i].Alias < m.Entries[j].Alias
	})
}
