//go:build ignore
// +build ignore

package main

// Files required to build supporting package raster are found in:
// * This task (immediately above)
// * Bitmap task

import (
	"fmt"
	"raster"
)

func main() {
	b := raster.NewBitmap(400, 300)
	b.FillRgb(0x240008) // a dark red
	err := b.WritePpmFile("write.ppm")
	if err != nil {
		fmt.Println(err)
	}
}
