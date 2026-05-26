// Go companion for go_str_upper fixture (Phase 10.2).
package main

import (
	"bufio"
	"encoding/json"
	"fmt"
	"os"
	"strings"
)

func go_toupper(s string) string { return strings.ToUpper(s) }

func main() {
	scanner := bufio.NewScanner(os.Stdin)
	for scanner.Scan() {
		var req struct {
			Fn   string   `json:"fn"`
			Args []string `json:"args"`
		}
		if err := json.Unmarshal(scanner.Bytes(), &req); err != nil {
			fmt.Println(`{"error":"parse error"}`)
			continue
		}
		var result interface{}
		switch req.Fn {
		case "go_toupper":
			result = go_toupper(req.Args[0])
		default:
			fmt.Printf("{\"error\":\"unknown function %s\"}\n", req.Fn)
			continue
		}
		resp, _ := json.Marshal(map[string]interface{}{"result": result})
		fmt.Println(string(resp))
	}
}
