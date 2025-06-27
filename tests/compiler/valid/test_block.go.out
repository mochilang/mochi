package main

import (
	"fmt"
	"time"
)

func expect(cond bool) {
	if !cond {
		panic("expect failed")
	}
}

func formatDuration(d time.Duration) string {
	switch {
	case d < time.Microsecond:
		return fmt.Sprintf("%dns", d.Nanoseconds())
	case d < time.Millisecond:
		return fmt.Sprintf("%.1fµs", float64(d.Microseconds()))
	case d < time.Second:
		return fmt.Sprintf("%.1fms", float64(d.Milliseconds()))
	default:
		return fmt.Sprintf("%.2fs", d.Seconds())
	}
}

func printTestStart(name string) {
	fmt.Printf("   test %-30s ...", name)
}

func printTestPass(d time.Duration) {
	fmt.Printf(" ok (%s)\n", formatDuration(d))
}

func printTestFail(err error, d time.Duration) {
	fmt.Printf(" fail %v (%s)\n", err, formatDuration(d))
}

func test_addition_works() {
	var x int = (1 + 2)
	_ = x
	expect((x == 3))
}

func main() {
	failures := 0
	fmt.Println("ok")
	{
		printTestStart("addition works")
		start := time.Now()
		var failed error
		func() {
			defer func() {
				if r := recover(); r != nil {
					failed = fmt.Errorf("%v", r)
				}
			}()
			test_addition_works()
		}()
		if failed != nil {
			failures++
			printTestFail(failed, time.Since(start))
		} else {
			printTestPass(time.Since(start))
		}
	}
	if failures > 0 {
		fmt.Printf("\n[FAIL] %d test(s) failed.\n", failures)
	}
}
