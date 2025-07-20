//go:build archive && slow

package main

import kt "mochi/transpiler/x/kt"

func main() {
	kt.UpdateReadmeForTests()
	kt.UpdateTasksForTests()
}
