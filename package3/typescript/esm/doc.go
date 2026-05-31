// Package esm carries the ESM vs CJS interop pass: the exports-map conditional
// resolver (per-runtime condition orders), the dual-package-hazard detector,
// the CJS-only-browser refusal, the `node:*` import surface detector. Lands in
// MEP-72 Phase 16.
//
// See website/docs/implementation/0072/phase-16-esm-cjs.md.
package esm
