//go:build ignore

// Generated by Mochi v0.10.40 on 2025-07-26 12:08:22 GMT+7
package main

import (
	"encoding/json"
	"fmt"
	"os"
	"runtime"
	"strconv"
	"time"
)

var seededNow bool
var nowSeed int64

func init() {
	if s := os.Getenv("MOCHI_NOW_SEED"); s != "" {
		if v, err := strconv.ParseInt(s, 10, 64); err == nil {
			nowSeed = v
			seededNow = true
		}
	}
}
func _now() int {
	if seededNow {
		nowSeed = (nowSeed*1664525 + 1013904223) % 2147483647
		return int(nowSeed)
	}
	return int(time.Now().UnixNano())
}

func mochiMain() {
	var pkg_dog string = "Salt"
	_ = pkg_dog
	var Dog string = "Pepper"
	_ = Dog
	var pkg_DOG string = "Mustard"
	_ = pkg_DOG
	var packageSees func(string, string, string) map[string]bool
	packageSees = func(d1 string, d2 string, d3 string) map[string]bool {
		fmt.Println(((((("Package sees: " + d1) + " ") + d2) + " ") + d3))
		return map[string]bool{"pkg_dog": true, "Dog": true, "pkg_DOG": true}
	}
	var d map[string]bool = packageSees(pkg_dog, Dog, pkg_DOG)
	_ = d
	fmt.Println((("There are " + fmt.Sprint(len(d))) + " dogs.\n"))
	var dog string = "Benjamin"
	_ = dog
	d = packageSees(pkg_dog, Dog, pkg_DOG)
	fmt.Println(((((("Main sees:   " + dog) + " ") + Dog) + " ") + pkg_DOG))
	d["dog"] = true
	d["Dog"] = true
	d["pkg_DOG"] = true
	fmt.Println((("There are " + fmt.Sprint(len(d))) + " dogs.\n"))
	Dog = "Samba"
	d = packageSees(pkg_dog, Dog, pkg_DOG)
	fmt.Println(((((("Main sees:   " + dog) + " ") + Dog) + " ") + pkg_DOG))
	d["dog"] = true
	d["Dog"] = true
	d["pkg_DOG"] = true
	fmt.Println((("There are " + fmt.Sprint(len(d))) + " dogs.\n"))
	var DOG string = "Bernie"
	_ = DOG
	d = packageSees(pkg_dog, Dog, pkg_DOG)
	fmt.Println(((((("Main sees:   " + dog) + " ") + Dog) + " ") + DOG))
	d["dog"] = true
	d["Dog"] = true
	d["pkg_DOG"] = true
	d["DOG"] = true
	fmt.Println((("There are " + fmt.Sprint(len(d))) + " dogs."))
}

func main() {
	func() {
		var ms runtime.MemStats
		runtime.ReadMemStats(&ms)
		startMem := ms.Alloc
		benchStart := time.Now().UnixNano()
		mochiMain()
		runtime.ReadMemStats(&ms)
		endMem := ms.Alloc
		benchEnd := time.Now().UnixNano()
		data := map[string]any{"name": "main", "duration_us": (benchEnd - benchStart) / 1000, "memory_bytes": endMem - startMem}
		out, _ := json.MarshalIndent(data, "", "  ")
		fmt.Println(string(out))
	}()
}
