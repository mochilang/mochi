// Package embedded owns the MEP-73 Phase 13 no_std subset: the
// deterministic text shape the wrapper crate adopts when the user
// has declared `[rust] profile = "embedded"` in mochi.toml. The
// embedded profile narrows the wrapper to a `no_std + alloc` build
// (suitable for thumbv7em-none-eabihf, riscv32-imc-none-elf, and
// similar bare-metal targets), refuses async-fn surfaces because
// tokio requires std, and pins the upstream dep to
// `default-features = false` so std-only features cannot leak in
// silently.
//
// The package intentionally only owns the bridge-side encoding of
// the profile. It does NOT walk upstream Cargo.toml to verify
// no_std compatibility (that lives in the lockfile drift checker)
// and does NOT pick the target triple (the user does, via the host
// build driver). Phase 13's contract is: given the profile flag,
// produce the right wrapper text and refuse the right async items.
package embedded

import (
	"fmt"
	"sort"
	"strings"
)

// Profile is the wrapper-crate build profile. The default
// ProfileHosted produces the full-std wrapper Phases 0-12 emit;
// ProfileEmbedded produces the no_std + alloc subset.
type Profile int

const (
	// ProfileHosted is the default. Wrapper uses std; tokio,
	// HashMap, HashSet, and everything else in Phases 0-12 work.
	ProfileHosted Profile = iota

	// ProfileEmbedded is the no_std + alloc subset. The wrapper
	// crate's src/lib.rs gains `#![no_std]` plus
	// `extern crate alloc;`, the upstream Cargo dep is pinned to
	// `default-features = false`, and async fns are refused at
	// synth time because tokio requires std.
	ProfileEmbedded
)

// String reports the profile name as it appears in mochi.toml. The
// returned name is stable and round-trips through ParseProfile.
func (p Profile) String() string {
	switch p {
	case ProfileHosted:
		return "hosted"
	case ProfileEmbedded:
		return "embedded"
	}
	return fmt.Sprintf("unknown-profile-%d", int(p))
}

// ParseProfile parses the `[rust] profile = "..."` value from
// mochi.toml. The empty string defaults to ProfileHosted (the
// pre-Phase-13 behaviour). Unknown values are rejected to keep the
// surface closed.
func ParseProfile(s string) (Profile, error) {
	switch strings.TrimSpace(s) {
	case "", "hosted":
		return ProfileHosted, nil
	case "embedded":
		return ProfileEmbedded, nil
	}
	return ProfileHosted, fmt.Errorf("embedded: unknown profile %q (want \"hosted\" or \"embedded\")", s)
}

// LibRSHeader returns the prologue lines the emitter prepends to
// src/lib.rs. ProfileHosted returns the empty string (the wrapper
// runs under std as before). ProfileEmbedded returns the verbatim
// no_std attribute plus `extern crate alloc;` so allocator-using
// types (String, Vec, BTreeMap from alloc) remain available.
//
// The output is deterministic and ends with a trailing newline iff
// non-empty so callers can concatenate it directly.
func LibRSHeader(p Profile) string {
	if p == ProfileEmbedded {
		return "#![no_std]\nextern crate alloc;\n"
	}
	return ""
}

// CargoUpstreamDepRow renders the `[dependencies] <name> = ...`
// line for the upstream crate. For ProfileEmbedded, the inline
// table flips `default-features = false` so the wrapper does not
// silently inherit `std` from the upstream's default feature set.
// For ProfileHosted, the row keeps the simple `name = "=version"`
// shape Phases 0-12 produce.
func CargoUpstreamDepRow(profile Profile, name, version string) string {
	if profile == ProfileEmbedded {
		return fmt.Sprintf("%s = { version = \"=%s\", default-features = false }\n", name, version)
	}
	return fmt.Sprintf("%s = \"=%s\"\n", name, version)
}

// RefuseAsync reports whether async fns must be refused at synth
// time under the given profile. ProfileEmbedded returns true: the
// tokio runtime singleton Phase 11 prepends to the wrapper requires
// std, so any async fn under an embedded build would fail at link
// time with a clearer-up-front refusal.
func RefuseAsync(p Profile) bool {
	return p == ProfileEmbedded
}

// AsyncRefusalReason is the human-readable diagnostic the wrapper
// uses when refusing an async fn under ProfileEmbedded. The phrase
// is fixed so test fixtures can match it byte-stable.
const AsyncRefusalReason = "async fn requires tokio (std); embedded profile is no_std + alloc"

// ParseTOMLBody parses the body of a `[rust] profile = "..."` table
// fragment. The accepted shapes are:
//
//	profile = "embedded"
//	profile = "hosted"
//
// plus blank-line / whitespace tolerance. The empty body defaults
// to ProfileHosted. The parser is intentionally narrow; it rejects
// anything that does not match the literal shape so the manifest
// stays a closed surface.
func ParseTOMLBody(body string) (Profile, error) {
	for _, raw := range strings.Split(body, "\n") {
		line := strings.TrimSpace(raw)
		if line == "" || strings.HasPrefix(line, "#") {
			continue
		}
		k, v, ok := strings.Cut(line, "=")
		if !ok {
			return ProfileHosted, fmt.Errorf("embedded: malformed row %q", raw)
		}
		k = strings.TrimSpace(k)
		v = strings.TrimSpace(v)
		if k != "profile" {
			return ProfileHosted, fmt.Errorf("embedded: unknown key %q (want \"profile\")", k)
		}
		if !strings.HasPrefix(v, `"`) || !strings.HasSuffix(v, `"`) {
			return ProfileHosted, fmt.Errorf("embedded: profile value %q is not a quoted string", v)
		}
		return ParseProfile(v[1 : len(v)-1])
	}
	return ProfileHosted, nil
}

// AllowedTriples returns the closed set of target triples the
// embedded profile is expected to compile against. The list is
// sorted alphabetically for byte stability; callers display it in
// diagnostics when the user picks a non-matching triple. The list
// follows MEP-73 §13 (embedded subset target matrix).
func AllowedTriples(p Profile) []string {
	if p != ProfileEmbedded {
		return nil
	}
	out := []string{
		"riscv32imac-unknown-none-elf",
		"riscv32imc-unknown-none-elf",
		"thumbv6m-none-eabi",
		"thumbv7em-none-eabi",
		"thumbv7em-none-eabihf",
		"thumbv7m-none-eabi",
		"thumbv8m.main-none-eabihf",
	}
	sort.Strings(out)
	return out
}
