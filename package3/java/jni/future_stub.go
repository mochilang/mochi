//go:build !java_jni || !cgo

package jni

import "errors"

// ErrFutureUnavailable is returned by all FutureRuntime methods when the
// package was compiled without java_jni + cgo.
var ErrFutureUnavailable = errors.New("jni: FutureRuntime not available (build with -tags java_jni and CGO enabled)")

// FutureCallback is a Go function invoked when a Java CompletableFuture resolves.
// The resolved value is delivered as a serialised string (see EmitFutureWrapper).
type FutureCallback func(value string, err error)

// FutureRuntime manages the Go-side callback registry for async Java futures.
// In stub mode all methods return ErrFutureUnavailable.
type FutureRuntime struct{}

// NewFutureRuntime always returns ErrFutureUnavailable in stub mode.
func NewFutureRuntime(_ *Runtime) (*FutureRuntime, error) {
	return nil, ErrFutureUnavailable
}

// Register always returns ErrFutureUnavailable in stub mode.
func (r *FutureRuntime) Register(_ FutureCallback) (int64, error) {
	return 0, ErrFutureUnavailable
}

// Unregister is a no-op in stub mode.
func (r *FutureRuntime) Unregister(_ int64) {}
