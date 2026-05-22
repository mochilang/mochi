//go:build archive && slow

package main

import pl "mochi/archived/transpiler/x/pl"

func main() {
	pl.UpdateReadmeForTests()
	pl.UpdateTasksForTests()
}
