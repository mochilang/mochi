// Package browser runs the browser-bundle pre-flight pass and the bundle-size
// gate. The bundle pipeline itself runs via `bun build` (primary) or `esbuild`
// (fallback) under MEP-52 Phase 17's TargetBrowserBundle. MEP-72 adds the
// per-consumed-package CJS-rejection + node:*-import rejection passes plus the
// `[ts.publish] browser-bundle-budget` override. Shared between MEP-72 Phase
// 11 (TargetJsrLibrary browser sub-corpus) and Phase 17 (final gate).
//
// See website/docs/implementation/0072/phase-11-jsr-library-emit.md and
// website/docs/implementation/0072/phase-17-edge-jupyter.md.
package browser
