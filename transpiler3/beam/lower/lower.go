package lower

// Lower is the entry point for the aotir -> cerl lowering pass.
// Phase 0 ships the stub; the pass is implemented incrementally
// starting in Phase 1.
//
// The function signature is intentionally left as a forward
// declaration here so that callers in beam/build can import this
// package without a circular dependency. The actual aotir and cerl
// types are wired up in Phase 1.
func Lower() error {
	return errNotImplemented("Lower not yet implemented (Phase 1)")
}

type notImplementedError string

func errNotImplemented(msg string) error { return notImplementedError(msg) }
func (e notImplementedError) Error() string { return string(e) }
