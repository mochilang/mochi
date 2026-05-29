package jni_test

import (
	"errors"
	"testing"

	"mochi/package3/java/jni"
)

// TestJNIStub verifies that the stub (non-CGO / non-java_jni) build compiles
// and returns ErrJNIUnavailable from every entry point.
// When built with -tags java_jni the tests are skipped because a live JVM
// is required and not available in CI.
func TestNew_StubReturnsError(t *testing.T) {
	rt, err := jni.New([]string{"/nonexistent.jar"})
	if err == nil {
		// Built with java_jni + cgo: we cannot test without a real JVM.
		t.Skip("built with java_jni; live JVM required for full test")
	}
	if rt != nil {
		t.Errorf("expected nil Runtime on error; got %v", rt)
	}
	if !errors.Is(err, jni.ErrJNIUnavailable) {
		t.Errorf("err = %v; want ErrJNIUnavailable", err)
	}
}

func TestCallStaticString_StubReturnsError(t *testing.T) {
	rt := &jni.Runtime{}
	_, err := rt.CallStaticString("java/lang/System", "lineSeparator", "()Ljava/lang/String;")
	if err == nil {
		t.Skip("built with java_jni; live JVM required for full test")
	}
	if !errors.Is(err, jni.ErrJNIUnavailable) {
		t.Errorf("err = %v; want ErrJNIUnavailable", err)
	}
}

func TestClose_StubIsNoop(t *testing.T) {
	rt := &jni.Runtime{}
	rt.Close() // must not panic
}
