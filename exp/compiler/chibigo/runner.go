package main

import (
	"fmt"
	"os"
	"os/exec"
	"unsafe"

	"modernc.org/libc"
)

func run_subprocess_go(tls *libc.TLS, argv uintptr) {
	if argv == 0 {
		return
	}
	args := argvToSlice(argv)
	if len(args) == 0 {
		return
	}
	cmd := exec.Command(args[0], args[1:]...)
	cmd.Stdin = os.Stdin
	cmd.Stdout = os.Stdout
	cmd.Stderr = os.Stderr
	if err := cmd.Run(); err != nil {
		if exitErr, ok := err.(*exec.ExitError); ok {
			libc.Xexit(tls, int32(exitErr.ExitCode()))
			return
		}
		fmt.Fprintln(os.Stderr, err)
		libc.Xexit(tls, 1)
	}
}

func argvToSlice(argv uintptr) []string {
	ptrSize := unsafe.Sizeof(uintptr(0))
	count := 0
	for i := uintptr(0); ; i++ {
		ptr := *(*uintptr)(unsafe.Pointer(argv + i*ptrSize))
		if ptr == 0 {
			break
		}
		count++
	}

	args := make([]string, count)
	for i := 0; i < count; i++ {
		ptr := *(*uintptr)(unsafe.Pointer(argv + uintptr(i)*ptrSize))
		args[i] = libc.GoString(ptr)
	}
	return args
}
