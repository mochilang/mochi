package emit

// BeamFile holds the result of compiling one cerl.Module.
type BeamFile struct {
	// ModName is the Erlang module name (e.g. "mochi_user__main").
	ModName string
	// Path is the absolute path to the generated .beam file.
	Path string
}

// Emit serialises mod to ETF, drives compile:forms/2 via an
// erl -noshell subprocess, and writes the resulting .beam file
// to outDir/<ModName>.beam.
//
// Phase 0 ships the stub. Phase 1 implements the full pipeline.
func Emit() ([]BeamFile, error) {
	return nil, errNotImplemented("Emit not yet implemented (Phase 1)")
}

type notImplementedError string

func errNotImplemented(msg string) error { return notImplementedError(msg) }
func (e notImplementedError) Error() string { return string(e) }
