//go:build !windows

package main

import (
	"os"
	"runtime"
	"syscall"
)

func processMaxRSS(ps *os.ProcessState) int64 {
	if ps == nil {
		return 0
	}
	ru, ok := ps.SysUsage().(*syscall.Rusage)
	if !ok || ru == nil {
		return 0
	}
	raw := int64(ru.Maxrss)
	if runtime.GOOS == "darwin" {
		return raw // bytes on macOS
	}
	return raw * 1024 // kilobytes on Linux/BSD
}
