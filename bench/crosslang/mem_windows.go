//go:build windows

package main

import "os"

func processMaxRSS(_ *os.ProcessState) int64 { return 0 }
