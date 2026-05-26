// Go companion for go_add_ints fixture (Phase 10.2).
// Reads JSON requests from stdin, dispatches to Go functions, writes results to stdout.
package main

import (
	"bufio"
	"encoding/json"
	"fmt"
	"os"
)

func add_ints(a, b int64) int64 { return a + b }

func main() {
	scanner := bufio.NewScanner(os.Stdin)
	for scanner.Scan() {
		var req struct {
			Fn   string        `json:"fn"`
			Args []interface{} `json:"args"`
		}
		if err := json.Unmarshal(scanner.Bytes(), &req); err != nil {
			fmt.Println(`{"error":"parse error"}`)
			continue
		}
		var result interface{}
		switch req.Fn {
		case "add_ints":
			a := int64(req.Args[0].(float64))
			b := int64(req.Args[1].(float64))
			result = add_ints(a, b)
		default:
			fmt.Printf("{\"error\":\"unknown function %s\"}\n", req.Fn)
			continue
		}
		resp, _ := json.Marshal(map[string]interface{}{"result": result})
		fmt.Println(string(resp))
	}
}
