//go:build ignore

package main

import (
	"fmt"
)

// line 4
func writeTwo() []string {
	return []string{"jsmith:x:1001:1000:Joe Smith,Room 1007,(234)555-8917,(234)555-0077,jsmith@rosettacode.org:/home/jsmith:/bin/bash", "jdoe:x:1002:1000:Jane Doe,Room 1004,(234)555-8914,(234)555-0044,jdoe@rosettacode.org:/home/jsmith:/bin/bash"}
}

// line 11
func appendOneMore(lines []string) []string {
	return _convSlice[any, string](append(_convSlice[string, any](lines), "xyz:x:1003:1000:X Yz,Room 1003,(234)555-8913,(234)555-0033,xyz@rosettacode.org:/home/xyz:/bin/bash"))
}

// line 15
func main() {
	var lines []string = writeTwo()
	lines = appendOneMore(lines)
	if (len(lines) >= 3) && (lines[2] == "xyz:x:1003:1000:X Yz,Room 1003,(234)555-8913,(234)555-0033,xyz@rosettacode.org:/home/xyz:/bin/bash") {
		fmt.Println("append okay")
	} else {
		fmt.Println("it didn't work")
	}
}

func main() {
	main()
}

func _convSlice[T any, U any](s []T) []U {
	out := []U{}
	for _, v := range s {
		out = append(out, any(v).(U))
	}
	return out
}
