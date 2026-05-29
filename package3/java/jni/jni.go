//go:build java_jni && cgo

package jni

/*
#cgo CFLAGS: -I${SRCDIR}/include
#cgo LDFLAGS: -ljvm

#include <jni.h>
#include <stdlib.h>

static jint createJVM(JavaVM **jvm, JNIEnv **env, char *classpath) {
    JavaVMInitArgs args;
    JavaVMOption opts[1];
    opts[0].optionString = classpath;
    args.version = JNI_VERSION_1_8;
    args.nOptions = 1;
    args.options = opts;
    args.ignoreUnrecognized = JNI_FALSE;
    return JNI_CreateJavaVM(jvm, (void**)env, &args);
}

static jclass findClass(JNIEnv *env, const char *name) {
    return (*env)->FindClass(env, name);
}

static jmethodID getStaticMethodID(JNIEnv *env, jclass cls, const char *name, const char *sig) {
    return (*env)->GetStaticMethodID(env, cls, name, sig);
}

static jobject callStaticObjectMethod(JNIEnv *env, jclass cls, jmethodID mid) {
    return (*env)->CallStaticObjectMethod(env, cls, mid);
}

static const char *getStringUTF(JNIEnv *env, jstring s) {
    return (*env)->GetStringUTFChars(env, s, NULL);
}

static void releaseStringUTF(JNIEnv *env, jstring s, const char *c) {
    (*env)->ReleaseStringUTFChars(env, s, c);
}

static jboolean exceptionCheck(JNIEnv *env) {
    return (*env)->ExceptionCheck(env);
}

static void exceptionDescribe(JNIEnv *env) {
    (*env)->ExceptionDescribe(env);
}

static void exceptionClear(JNIEnv *env) {
    (*env)->ExceptionClear(env);
}

static void destroyJVM(JavaVM *jvm) {
    (*jvm)->DestroyJavaVM(jvm);
}
*/
import "C"
import (
	"fmt"
	"unsafe"
)

// Runtime holds a live JVM instance created via JNI_CreateJavaVM.
// Only one JVM may exist per process (JNI constraint).
type Runtime struct {
	jvm *C.JavaVM
	env *C.JNIEnv
}

// New creates a JVM with the given JAR paths added to the class path.
// The JVM is initialised with JNI version 1.8.
func New(jarPaths []string) (*Runtime, error) {
	cp := "-Djava.class.path="
	for i, p := range jarPaths {
		if i > 0 {
			cp += ":"
		}
		cp += p
	}
	ccp := C.CString(cp)
	defer C.free(unsafe.Pointer(ccp))

	var jvm *C.JavaVM
	var env *C.JNIEnv
	rc := C.createJVM(&jvm, &env, ccp)
	if rc != C.JNI_OK {
		return nil, fmt.Errorf("jni: JNI_CreateJavaVM returned %d", int(rc))
	}
	return &Runtime{jvm: jvm, env: env}, nil
}

// CallStaticString invokes a public static method with the given descriptor
// that returns a java.lang.String, and returns the UTF-8 result.
func (r *Runtime) CallStaticString(className, methodName, descriptor string) (string, error) {
	cname := C.CString(className)
	defer C.free(unsafe.Pointer(cname))
	cmeth := C.CString(methodName)
	defer C.free(unsafe.Pointer(cmeth))
	cdesc := C.CString(descriptor)
	defer C.free(unsafe.Pointer(cdesc))

	cls := C.findClass(r.env, cname)
	if cls == nil {
		return "", fmt.Errorf("jni: class not found: %s", className)
	}
	mid := C.getStaticMethodID(r.env, cls, cmeth, cdesc)
	if mid == nil {
		if C.exceptionCheck(r.env) == C.JNI_TRUE {
			C.exceptionDescribe(r.env)
			C.exceptionClear(r.env)
		}
		return "", fmt.Errorf("jni: method not found: %s.%s%s", className, methodName, descriptor)
	}
	obj := C.callStaticObjectMethod(r.env, cls, mid)
	if C.exceptionCheck(r.env) == C.JNI_TRUE {
		C.exceptionDescribe(r.env)
		C.exceptionClear(r.env)
		return "", fmt.Errorf("jni: exception in %s.%s", className, methodName)
	}
	if obj == nil {
		return "", nil
	}
	cstr := C.getStringUTF(r.env, C.jstring(obj))
	defer C.releaseStringUTF(r.env, C.jstring(obj), cstr)
	return C.GoString(cstr), nil
}

// Close destroys the JVM. After Close the Runtime must not be used.
func (r *Runtime) Close() {
	if r.jvm != nil {
		C.destroyJVM(r.jvm)
		r.jvm = nil
		r.env = nil
	}
}
