package lockfile

import (
	"fmt"

	"mochi/package3/python/toml"
)

// ParseTOML reads an [[erlang-package]] manifest from a TOML string.
// It expects the top-level key "erlang-lockfile-schema" to be present and
// equal to SchemaVersion.
func ParseTOML(src string) (Manifest, error) {
	tree, err := toml.Parse(src)
	if err != nil {
		return Manifest{}, fmt.Errorf("erlang lockfile: parse: %w", err)
	}
	dec := toml.NewDecoder(tree)

	version, present, err := dec.Int("erlang-lockfile-schema")
	if err != nil {
		return Manifest{}, fmt.Errorf("erlang lockfile: %w", err)
	}
	if !present {
		return Manifest{}, fmt.Errorf("erlang lockfile: missing required key %q", "erlang-lockfile-schema")
	}
	if int(version) != SchemaVersion {
		return Manifest{}, fmt.Errorf("erlang lockfile: unsupported schema %d (this build understands %d)", version, SchemaVersion)
	}

	m := Manifest{Version: int(version)}
	arr, _, err := dec.TableArray("erlang-package")
	if err != nil {
		return Manifest{}, fmt.Errorf("erlang lockfile: %w", err)
	}
	for i, sub := range arr {
		e, err := decodeEntry(sub)
		if err != nil {
			return Manifest{}, fmt.Errorf("erlang lockfile: entry %d: %w", i, err)
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
	version, err := d.StringRequired("version")
	if err != nil {
		return Entry{}, err
	}
	e := Entry{Name: name, Version: version}

	for _, opt := range []struct {
		key string
		ptr *string
	}{
		{"registry", &e.Registry},
		{"outer-sha256", &e.OuterSHA256},
		{"inner-sha256", &e.InnerSHA256},
		{"inner-sha512", &e.InnerSHA512},
		{"beam-ingest-sha256", &e.BeamIngestSHA256},
		{"shim-sha256", &e.ShimSHA256},
		{"otp-app", &e.OTPApp},
	} {
		s, _, err := d.String(opt.key)
		if err != nil {
			return Entry{}, err
		}
		*opt.ptr = s
	}

	caps, _, err := d.StringArray("capabilities")
	if err != nil {
		return Entry{}, fmt.Errorf("capabilities: %w", err)
	}
	e.Capabilities = caps

	deps, _, err := d.StringArray("dependencies")
	if err != nil {
		return Entry{}, fmt.Errorf("dependencies: %w", err)
	}
	e.Dependencies = deps

	mods, _, err := d.StringArray("modules")
	if err != nil {
		return Entry{}, fmt.Errorf("modules: %w", err)
	}
	e.Modules = mods

	return e, nil
}
