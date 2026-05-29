// Package pypackage implements MEP-71 Phase 10: the TargetPythonPackage
// emit. It composes the file layout a Mochi-side library ships as a Python
// distribution and the PEP 517 backend (`mochi_build`) that drives the
// actual sdist / wheel build.
//
// The package surfaces three building blocks:
//
//   - Package: the in-memory representation of a Mochi-as-Python
//     distribution (name, version, summary, requires-python, public
//     surface).
//   - Layout: the flat path -> bytes map a generated tree lays out.
//   - RenderSdist / RenderWheel: the per-format renderers, layered on
//     the closed type table from Phase 4 and the importspec name
//     normalisation rules from Phase 7.
//
// The pyproject.toml the renderer emits sets `build-backend = "mochi_build"`
// and `build-requires` to the Mochi runtime so a downstream Python consumer
// can install the package through standard pip / uv flows. The `.pyi` file
// the renderer ships gives downstream typing tools (mypy, pyright) full
// surface visibility without dynamic introspection.
//
// See MEP-71 §10 "Publish direction" for the normative schema. The actual
// PyPI upload + Sigstore attestation flow lives in Phase 11.
package pypackage
