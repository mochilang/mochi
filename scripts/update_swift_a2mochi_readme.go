//go:build archive && slow

package main

import swift "mochi/tools/a2mochi/x/swift"

func main() {
	swift.UpdateReadmeForTests()
}
