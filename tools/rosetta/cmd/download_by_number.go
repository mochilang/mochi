//go:build slow

package main

import (
	"flag"
	"fmt"

	"mochi/tools/rosetta"
)

func main() {
	n := flag.Int("n", 1, "task number to download")
	refresh := flag.Bool("refresh", false, "download even if cached")
	flag.Parse()

	task, err := rosetta.DownloadTaskByNumber(*n, "Go", *refresh)
	if err != nil {
		fmt.Println("error:", err)
		return
	}
	fmt.Println("downloaded task:", task)
}
