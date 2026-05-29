// Package runtime is the Mochi-to-Go runtime support library.
//
// Module path: dev.mochilang/runtime/go. Apache-2.0. Zero
// third-party deps in the default build. LLM provider plugins
// live under dev.mochilang/runtime/go/llm/<provider>.
//
// Design contract: MEP-54. See website/docs/mep/mep-0054.md.
//
// Per-program use: the transpiler3/go build driver vendors
// this module into each output module so the produced binary
// has no dependency outside the Go stdlib.
package runtime

// Version is the runtime library version, stamped at release
// time. Phase 0 leaves it at the dev sentinel; Phase 18 wires
// release-tagging.
const Version = "0.0.0-dev"
