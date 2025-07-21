//go:build archive && slow

package main

import pl "mochi/transpiler/x/pl"

func main() {
	pl.UpdateReadmeForTests()
	pl.UpdateTasksForTests()
}
