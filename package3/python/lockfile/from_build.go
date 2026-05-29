package lockfile

import (
	"fmt"

	"mochi/package3/python/build"
	"mochi/package3/python/importspec"
)

// FromBuildResult converts a build.Result + the originating Request into a
// canonical Manifest. The Manifest is sorted on return.
//
// The Version field on each Entry holds the resolved version. Phase 9 does
// not perform resolution itself; it forwards what the Request's spec already
// carries (a pinned `==N.N.N` becomes Version=N.N.N, an open `>=N` keeps
// Version empty). Sub-phase 8.1 will populate Version from the uv resolver
// output once the install loop lands.
func FromBuildResult(req build.Request, res *build.Result) (Manifest, error) {
	if res == nil {
		return Manifest{}, fmt.Errorf("lockfile: nil build.Result")
	}
	if len(req.Targets) != len(res.Wrappers) || len(req.Targets) != len(res.Shims) {
		return Manifest{}, fmt.Errorf("lockfile: Request/Result target count mismatch (req=%d wrappers=%d shims=%d)",
			len(req.Targets), len(res.Wrappers), len(res.Shims))
	}
	m := Manifest{Version: SchemaVersion}
	for i, t := range req.Targets {
		entry := Entry{
			Name:          t.Spec.Name,
			Alias:         t.Alias,
			Source:        toSource(t.Spec.Source),
			Specifier:     t.Spec.Specifier.String(),
			IndexURL:      t.Spec.IndexURL,
			GitURL:        t.Spec.GitURL,
			GitRev:        t.Spec.GitRev,
			LocalPath:     t.Spec.LocalPath,
			WrapperSHA256: res.Shims[i].SHA256,
			Capabilities:  ExtractCapabilities(res.Wrappers[i]),
		}
		entry.Version = pinnedVersion(t.Spec)
		m.Entries = append(m.Entries, entry)
	}
	m.Sort()
	return m, nil
}

func toSource(s importspec.Source) Source {
	switch s {
	case importspec.SourceRegistry:
		return SourceRegistry
	case importspec.SourceIndex:
		return SourceIndex
	case importspec.SourceGit:
		return SourceGit
	case importspec.SourcePath:
		return SourcePath
	}
	return ""
}

// pinnedVersion returns the exact version a SourceRegistry / SourceIndex spec
// pins to (`==N.N.N`), or empty when the spec is open-ended or a non-registry
// source.
func pinnedVersion(s importspec.Spec) string {
	if s.Source != importspec.SourceRegistry && s.Source != importspec.SourceIndex {
		return ""
	}
	if len(s.Specifier.Clauses) == 1 {
		c := s.Specifier.Clauses[0]
		if c.Op.String() == "==" {
			return c.Version.String()
		}
	}
	return ""
}
