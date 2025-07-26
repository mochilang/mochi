//go:build slow

package main

import (
	"flag"
	"fmt"
	"mochi/tools/rosetta"
)

func main() {
	n := flag.Int("n", 1, "task number")
	refresh := flag.Bool("refresh", false, "force download even if cached")
	flag.Parse()

	task, err := rosetta.DownloadTaskByNumber(*n, "Go", *refresh)
	if err != nil {
		panic(err)
	}
	fmt.Println("downloaded task:", task)
}
