//go:build ignore

package main

import (
    "fmt"
)

func main() {
    data := map[string]interface{}{"outer": map[string]interface{}{"inner": 1}}
    data["outer"].(map[string]interface{})["inner"] = 2
    fmt.Println(data["outer"].(map[interface{}]interface{})["inner"])
}
