package jni_test

import (
	"errors"
	"testing"

	"mochi/package3/java/jni"
)

func TestNewFutureRuntime_StubReturnsError(t *testing.T) {
	fr, err := jni.NewFutureRuntime(&jni.Runtime{})
	if err == nil {
		t.Skip("built with java_jni; live JVM required for full test")
	}
	if fr != nil {
		t.Error("expected nil FutureRuntime on error")
	}
	if !errors.Is(err, jni.ErrFutureUnavailable) {
		t.Errorf("err = %v; want ErrFutureUnavailable", err)
	}
}

func TestFutureRuntime_Register_StubReturnsError(t *testing.T) {
	fr := &jni.FutureRuntime{}
	_, err := fr.Register(func(string, error) {})
	if err == nil {
		t.Skip("built with java_jni; skipping stub test")
	}
	if !errors.Is(err, jni.ErrFutureUnavailable) {
		t.Errorf("err = %v; want ErrFutureUnavailable", err)
	}
}

func TestFutureRuntime_Unregister_Noop(t *testing.T) {
	fr := &jni.FutureRuntime{}
	fr.Unregister(999) // must not panic
}
