//go:build slow

package main

import cpp "mochi/tools/a2mochi/x/cpp"

func main() {
	cpp.UpdateReadmeForTests()
}
