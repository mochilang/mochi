// Package publish wires the npm and JSR Trusted-Publishing flows into the
// MEP-52 Phase 18 emitted GitHub Actions workflow. The bridge emits the
// workflow files; the actual publish runs in GitHub Actions under OIDC token
// exchange, with no long-lived NPM_TOKEN or JSR_TOKEN secret. Lands across
// MEP-72 Phase 12 (npm) and Phase 13 (jsr).
//
// See website/docs/implementation/0072/phase-12-npm-publish.md and
// website/docs/implementation/0072/phase-13-jsr-publish.md.
package publish
