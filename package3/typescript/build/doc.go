// Package build carries the bridge's Driver and Workspace types: it synthesises
// the per-build package.json + import-map.json + tsconfig.json, materialises
// the consumed package tree under node_modules/ + jsr_cache/, and invokes the
// host runtime / bundler. Lands across MEP-72 Phase 0 (scaffolding) and Phase 8
// (orchestration).
//
// See website/docs/implementation/0072/phase-00-skeleton.md and
// website/docs/implementation/0072/phase-08-build.md.
package build
