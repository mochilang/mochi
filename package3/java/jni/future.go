//go:build java_jni && cgo

package jni

import (
	"fmt"
	"sync"
	"sync/atomic"
)

// ErrFutureUnavailable satisfies callers that import this symbol in either build.
// In the CGO build, callbacks are real; the error is returned only for
// programming mistakes (like calling Register with a nil callback).
var ErrFutureUnavailable = fmt.Errorf("jni: FutureRuntime: nil callback")

// FutureCallback is a Go function invoked when a Java CompletableFuture resolves.
// The resolved value is delivered as a serialised string.
type FutureCallback func(value string, err error)

// FutureRuntime manages the Go-side callback registry for async Java futures.
// One FutureRuntime is shared across all JNI calls in a process; it is
// safe for concurrent use.
type FutureRuntime struct {
	mu      sync.Mutex
	next    int64
	pending map[int64]FutureCallback
}

// NewFutureRuntime creates a FutureRuntime bound to an active JVM Runtime.
func NewFutureRuntime(_ *Runtime) (*FutureRuntime, error) {
	return &FutureRuntime{pending: make(map[int64]FutureCallback)}, nil
}

// Register stores cb in the registry and returns a handle that Java passes
// back to MochiRuntime.callback. Register is safe to call from any goroutine.
func (r *FutureRuntime) Register(cb FutureCallback) (int64, error) {
	if cb == nil {
		return 0, ErrFutureUnavailable
	}
	id := atomic.AddInt64(&r.next, 1)
	r.mu.Lock()
	r.pending[id] = cb
	r.mu.Unlock()
	return id, nil
}

// Unregister removes the callback with the given handle. It is called after
// the Java side invokes the callback to release the Go closure.
func (r *FutureRuntime) Unregister(handle int64) {
	r.mu.Lock()
	delete(r.pending, handle)
	r.mu.Unlock()
}

// Invoke is the Go entry point called from the JNI MochiRuntime.callback
// native method. It looks up the registered callback by handle and dispatches
// it in a new goroutine so the JNI thread is not blocked.
func (r *FutureRuntime) Invoke(handle int64, value string, javaErr string) {
	r.mu.Lock()
	cb, ok := r.pending[handle]
	if ok {
		delete(r.pending, handle)
	}
	r.mu.Unlock()
	if !ok {
		return
	}
	var err error
	if javaErr != "" {
		err = fmt.Errorf("java: %s", javaErr)
	}
	go cb(value, err)
}
