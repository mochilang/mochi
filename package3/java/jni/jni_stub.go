//go:build !java_jni || !cgo

package jni

import "errors"

// ErrJNIUnavailable is returned by all Runtime methods when the package was
// compiled without the java_jni build tag or without CGO.
var ErrJNIUnavailable = errors.New("jni: JNI runtime not available (build with -tags java_jni and CGO enabled)")

// Runtime is a placeholder type used when JNI support is compiled out.
type Runtime struct{}

// New always returns ErrJNIUnavailable in stub mode.
func New(_ []string) (*Runtime, error) {
	return nil, ErrJNIUnavailable
}

// CallStaticString always returns ErrJNIUnavailable in stub mode.
func (r *Runtime) CallStaticString(_, _, _ string) (string, error) {
	return "", ErrJNIUnavailable
}

// Close is a no-op in stub mode.
func (r *Runtime) Close() {}
