// Package semver implements PEP 440 version parsing, comparison, and
// version-specifier matching. PEP 440 is Python's canonical version-format
// PEP; it is the language of distribution version strings produced by
// `setup.py sdist` / `pyproject.toml`, returned by the PEP 503 / 691 simple
// index, and consumed by the uv resolver.
//
// The MEP-71 bridge needs PEP 440 in three places:
//
//  1. Phase 1: filter and sort the list of distribution files returned by
//     the simple index. The newest non-yanked release that matches the
//     user's `import python "<pkg>@<spec>"` specifier wins.
//  2. Phase 9: serialise the resolved version into the `mochi.lock`
//     `[[python-package]] version` field. The format is the canonical PEP
//     440 normalised form.
//  3. Phase 10: emit a `requires-dist` specifier into the wheel metadata
//     when re-publishing a Mochi package as a Python sdist / wheel.
//
// PEP 440 version grammar (informal):
//
//	[N!]N(.N)*[{a|b|c|rc|alpha|beta|pre|preview}N][.postN][.devN][+local]
//
// where:
//   - N! is an optional epoch (default 0)
//   - N(.N)* is the release segment (one or more numeric components)
//   - {a|b|c|rc|...}N is an optional pre-release segment (alpha / beta /
//     release-candidate); the spelling is normalised to "a" / "b" / "rc"
//   - .postN is an optional post-release counter
//   - .devN is an optional dev-release counter
//   - +local is an optional local version label
//
// The package does not implement PEP 440's full lenient parser. The bridge
// rejects ambiguous or non-normalised inputs at lock time so the `mochi.lock`
// hash key is stable. Specifically, the package rejects:
//   - leading "v" prefixes (e.g. "v1.2.3")
//   - case-insensitive pre-release spellings except the canonical "a", "b",
//     "rc", "post", "dev"
//   - implicit-zero release segments shorter than one component
//
// Inputs that pass `Parse` are guaranteed to round-trip through `String`
// byte-for-byte.
package semver
