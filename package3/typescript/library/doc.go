// Package library emits the publish-side TargetNpmLibrary and TargetJsrLibrary
// artefacts. TargetNpmLibrary produces package.json + dist/*.mjs + dist/*.d.ts
// via `tsc --declaration --emitDeclarationOnly`. TargetJsrLibrary produces
// jsr.json + mod.ts under the source-not-dist invariant. Lands across MEP-72
// Phase 10 (npm) and Phase 11 (jsr).
//
// See website/docs/implementation/0072/phase-10-npm-library-emit.md and
// website/docs/implementation/0072/phase-11-jsr-library-emit.md.
package library
