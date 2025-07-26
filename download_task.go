package main

import (
	"fmt"
	"mochi/tools/rosetta"
)

func main() {
	task, err := rosetta.DownloadTaskByNumber(267, "Go", true)
	if err != nil {
		panic(err)
	}
	fmt.Println("Downloaded task:", task)
}
