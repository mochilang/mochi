package main

import (
	"mochi/tools/sandbox"
	"mochi/tools/sandbox/runner"
)

func main() {
	runner.Run(sandbox.RunCmds["py"])
}
