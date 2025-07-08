//go:build ignore

package main

import (
    "fmt"
    "encoding/json"
)

func main() {
    m := map[string]interface{}{"a": 1, "b": 2}
    func() { b, _ := json.Marshal(m); fmt.Println(string(b)) }()
}
