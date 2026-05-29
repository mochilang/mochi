package lockfile

import (
	"fmt"

	"mochi/package3/python/toml"
)

// ParseTOML reads a serialised manifest back into structured form. Unknown
// schema versions are rejected so a future writer cannot silently change the
// shape under an older reader.
func ParseTOML(src string) (Manifest, error) {
	tree, err := toml.Parse(src)
	if err != nil {
		return Manifest{}, fmt.Errorf("lockfile: parse: %w", err)
	}
	dec := toml.NewDecoder(tree)
	version, present, err := dec.Int("schema-version")
	if err != nil {
		return Manifest{}, err
	}
	if !present {
		return Manifest{}, fmt.Errorf("lockfile: missing required key %q", "schema-version")
	}
	if int(version) != SchemaVersion {
		return Manifest{}, fmt.Errorf("lockfile: unsupported schema-version %d (this build understands %d)", version, SchemaVersion)
	}
	m := Manifest{Version: int(version)}
	arr, _, err := dec.TableArray("python-package")
	if err != nil {
		return Manifest{}, err
	}
	for i, sub := range arr {
		e, err := decodeEntry(sub)
		if err != nil {
			return Manifest{}, fmt.Errorf("lockfile: entry %d: %w", i, err)
		}
		m.Entries = append(m.Entries, e)
	}
	m.Sort()
	return m, nil
}

func decodeEntry(d *toml.Decoder) (Entry, error) {
	name, err := d.StringRequired("name")
	if err != nil {
		return Entry{}, err
	}
	alias, err := d.StringRequired("alias")
	if err != nil {
		return Entry{}, err
	}
	source, err := d.StringRequired("source")
	if err != nil {
		return Entry{}, err
	}
	e := Entry{Name: name, Alias: alias, Source: Source(source)}
	if !e.Source.Valid() {
		return Entry{}, fmt.Errorf("invalid source %q", source)
	}
	for _, opt := range []struct {
		key string
		ptr *string
	}{
		{"version", &e.Version},
		{"specifier", &e.Specifier},
		{"index-url", &e.IndexURL},
		{"git-url", &e.GitURL},
		{"git-rev", &e.GitRev},
		{"local-path", &e.LocalPath},
		{"wrapper-sha256", &e.WrapperSHA256},
	} {
		s, _, err := d.String(opt.key)
		if err != nil {
			return Entry{}, err
		}
		*opt.ptr = s
	}
	caps, _, err := d.StringArray("capabilities")
	if err != nil {
		return Entry{}, err
	}
	e.Capabilities = caps
	return e, nil
}
