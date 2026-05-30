// Package emit writes the per-consumed-package shim.mochi file under
// target/ts_shims/<pkg>/. The file carries extern fn declarations, extern type
// declarations, and alias bindings. Lands in MEP-72 Phase 6.
//
// See website/docs/implementation/0072/phase-06-extern-emit.md.
package emit
