//go:build ignore

// Generated by Mochi v0.10.52 on 2025-08-02 01:03:37 GMT+7
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

func Node(data int) map[string]any {
	return func(v any) map[string]any {
		if v == nil {
			return nil
		}
		if vv, ok := v.(map[string]any); ok {
			return vv
		}
		return nil
	}(map[string]any{"Data": data, "Balance": 0, "Link": []any{nil, nil}})
}

func getLink(n map[string]any, dir int) any {
	return func(v any) []any {
		if v == nil {
			return nil
		}
		if vv, ok := v.([]any); ok {
			return vv
		}
		if arr, ok := v.([]any); ok {
			if len(arr) == 0 {
				return []any{}
			}
			out := make([]any, len(arr))
			for i, x := range arr {
				out[i] = x.(any)
			}
			return out
		}
		return v.([]any)
	}(func(v any) []any {
		if v == nil {
			return nil
		}
		if vv, ok := v.([]any); ok {
			return vv
		}
		if arr, ok := v.([]any); ok {
			if len(arr) == 0 {
				return []any{}
			}
			out := make([]any, len(arr))
			for i, x := range arr {
				out[i] = x.(any)
			}
			return out
		}
		return v.([]any)
	}(n["Link"]))[dir]
}

func setLink(n map[string]any, dir int, v any) {
	var links []any = func(v any) []any {
		if v == nil {
			return nil
		}
		if vv, ok := v.([]any); ok {
			return vv
		}
		if arr, ok := v.([]any); ok {
			if len(arr) == 0 {
				return []any{}
			}
			out := make([]any, len(arr))
			for i, x := range arr {
				out[i] = x.(any)
			}
			return out
		}
		return v.([]any)
	}(n["Link"])
	_ = links
	links[dir] = v
	n["Link"] = links
}

func opp(dir int) int {
	return (1 - dir)
}

func single(root map[string]any, dir int) map[string]any {
	tmp := getLink(root, opp(dir))
	_ = tmp
	setLink(root, opp(dir), getLink(func(v any) map[string]any {
		if v == nil {
			return nil
		}
		if vv, ok := v.(map[string]any); ok {
			return vv
		}
		return nil
	}(tmp), dir))
	setLink(func(v any) map[string]any {
		if v == nil {
			return nil
		}
		if vv, ok := v.(map[string]any); ok {
			return vv
		}
		return nil
	}(tmp), dir, root)
	return func(v any) map[string]any {
		if v == nil {
			return nil
		}
		if vv, ok := v.(map[string]any); ok {
			return vv
		}
		return nil
	}(tmp)
}

func double(root map[string]any, dir int) map[string]any {
	var tmp any = getLink(func(v any) map[string]any {
		if v == nil {
			return nil
		}
		if vv, ok := v.(map[string]any); ok {
			return vv
		}
		return nil
	}(getLink(root, opp(dir))), dir)
	_ = tmp
	setLink(func(v any) map[string]any {
		if v == nil {
			return nil
		}
		if vv, ok := v.(map[string]any); ok {
			return vv
		}
		return nil
	}(getLink(root, opp(dir))), dir, getLink(func(v any) map[string]any {
		if v == nil {
			return nil
		}
		if vv, ok := v.(map[string]any); ok {
			return vv
		}
		return nil
	}(tmp), opp(dir)))
	setLink(func(v any) map[string]any {
		if v == nil {
			return nil
		}
		if vv, ok := v.(map[string]any); ok {
			return vv
		}
		return nil
	}(tmp), opp(dir), getLink(root, opp(dir)))
	setLink(root, opp(dir), tmp)
	tmp = getLink(root, opp(dir))
	setLink(root, opp(dir), getLink(func(v any) map[string]any {
		if v == nil {
			return nil
		}
		if vv, ok := v.(map[string]any); ok {
			return vv
		}
		return nil
	}(tmp), dir))
	setLink(func(v any) map[string]any {
		if v == nil {
			return nil
		}
		if vv, ok := v.(map[string]any); ok {
			return vv
		}
		return nil
	}(tmp), dir, root)
	return func(v any) map[string]any {
		if v == nil {
			return nil
		}
		if vv, ok := v.(map[string]any); ok {
			return vv
		}
		return nil
	}(tmp)
}

func adjustBalance(root map[string]any, dir int, bal int) {
	var n map[string]any = func(v any) map[string]any {
		if v == nil {
			return nil
		}
		if vv, ok := v.(map[string]any); ok {
			return vv
		}
		return nil
	}(getLink(root, dir))
	_ = n
	var nn map[string]any = func(v any) map[string]any {
		if v == nil {
			return nil
		}
		if vv, ok := v.(map[string]any); ok {
			return vv
		}
		return nil
	}(getLink(n, opp(dir)))
	_ = nn
	if nn["Balance"].(int) == 0 {
		root["Balance"] = 0
		n["Balance"] = 0
	} else {
		if nn["Balance"].(int) == bal {
			root["Balance"] = (0 - bal)
			n["Balance"] = 0
		} else {
			root["Balance"] = 0
			n["Balance"] = bal
		}
	}
	nn["Balance"] = 0
}

func insertBalance(root map[string]any, dir int) map[string]any {
	var n map[string]any = func(v any) map[string]any {
		if v == nil {
			return nil
		}
		if vv, ok := v.(map[string]any); ok {
			return vv
		}
		return nil
	}(getLink(root, dir))
	_ = n
	var bal int = ((2 * dir) - 1)
	_ = bal
	if n["Balance"].(int) == bal {
		root["Balance"] = 0
		n["Balance"] = 0
		return single(root, opp(dir))
	}
	adjustBalance(root, dir, bal)
	return double(root, opp(dir))
}

func insertR(root any, data int) map[string]any {
	if root == nil {
		return func(v any) map[string]any {
			if v == nil {
				return nil
			}
			if vv, ok := v.(map[string]any); ok {
				return vv
			}
			return nil
		}(map[string]any{"node": Node(data), "done": false})
	}
	var node map[string]any = func(v any) map[string]any {
		if v == nil {
			return nil
		}
		if vv, ok := v.(map[string]any); ok {
			return vv
		}
		return nil
	}(root)
	_ = node
	var dir int = 0
	_ = dir
	if node["Data"].(int) < data {
		dir = 1
	}
	var r map[string]any = insertR(getLink(node, dir), data)
	_ = r
	setLink(node, dir, r["node"])
	if r["done"].(bool) {
		return func(v any) map[string]any {
			if v == nil {
				return nil
			}
			if vv, ok := v.(map[string]any); ok {
				return vv
			}
			return nil
		}(map[string]any{"node": node, "done": true})
	}
	node["Balance"] = (node["Balance"].(int) + ((2 * dir) - 1))
	if node["Balance"].(int) == 0 {
		return func(v any) map[string]any {
			if v == nil {
				return nil
			}
			if vv, ok := v.(map[string]any); ok {
				return vv
			}
			return nil
		}(map[string]any{"node": node, "done": true})
	}
	if (node["Balance"].(int) == 1) || (node["Balance"].(int) == (0 - 1)) {
		return func(v any) map[string]any {
			if v == nil {
				return nil
			}
			if vv, ok := v.(map[string]any); ok {
				return vv
			}
			return nil
		}(map[string]any{"node": node, "done": false})
	}
	return func(v any) map[string]any {
		if v == nil {
			return nil
		}
		if vv, ok := v.(map[string]any); ok {
			return vv
		}
		return nil
	}(map[string]any{"node": insertBalance(node, dir), "done": true})
}

func Insert(tree any, data int) any {
	var r map[string]any = insertR(tree, data)
	_ = r
	_ = r
	return r["node"]
}

func removeBalance(root map[string]any, dir int) map[string]any {
	var n map[string]any = func(v any) map[string]any {
		if v == nil {
			return nil
		}
		if vv, ok := v.(map[string]any); ok {
			return vv
		}
		return nil
	}(getLink(root, opp(dir)))
	_ = n
	var bal int = ((2 * dir) - 1)
	_ = bal
	if n["Balance"].(int) == (0 - bal) {
		root["Balance"] = 0
		n["Balance"] = 0
		return func(v any) map[string]any {
			if v == nil {
				return nil
			}
			if vv, ok := v.(map[string]any); ok {
				return vv
			}
			return nil
		}(map[string]any{"node": single(root, dir), "done": false})
	}
	if n["Balance"].(int) == bal {
		adjustBalance(root, opp(dir), (0 - bal))
		return func(v any) map[string]any {
			if v == nil {
				return nil
			}
			if vv, ok := v.(map[string]any); ok {
				return vv
			}
			return nil
		}(map[string]any{"node": double(root, dir), "done": false})
	}
	root["Balance"] = (0 - bal)
	n["Balance"] = bal
	return func(v any) map[string]any {
		if v == nil {
			return nil
		}
		if vv, ok := v.(map[string]any); ok {
			return vv
		}
		return nil
	}(map[string]any{"node": single(root, dir), "done": true})
}

func removeR(root any, data int) map[string]any {
	if root == nil {
		return func(v any) map[string]any {
			if v == nil {
				return nil
			}
			if vv, ok := v.(map[string]any); ok {
				return vv
			}
			return nil
		}(map[string]any{"node": nil, "done": false})
	}
	var node map[string]any = func(v any) map[string]any {
		if v == nil {
			return nil
		}
		if vv, ok := v.(map[string]any); ok {
			return vv
		}
		return nil
	}(root)
	_ = node
	if node["Data"].(int) == data {
		if getLink(node, 0) == nil {
			return func(v any) map[string]any {
				if v == nil {
					return nil
				}
				if vv, ok := v.(map[string]any); ok {
					return vv
				}
				return nil
			}(map[string]any{"node": getLink(node, 1), "done": false})
		}
		if getLink(node, 1) == nil {
			return func(v any) map[string]any {
				if v == nil {
					return nil
				}
				if vv, ok := v.(map[string]any); ok {
					return vv
				}
				return nil
			}(map[string]any{"node": getLink(node, 0), "done": false})
		}
		var heir any = getLink(node, 0)
		_ = heir
		for getLink(func(v any) map[string]any {
			if v == nil {
				return nil
			}
			if vv, ok := v.(map[string]any); ok {
				return vv
			}
			return nil
		}(heir), 1) != nil {
			heir = getLink(func(v any) map[string]any {
				if v == nil {
					return nil
				}
				if vv, ok := v.(map[string]any); ok {
					return vv
				}
				return nil
			}(heir), 1)
		}
		node["Data"] = func(v any) map[string]any {
			if v == nil {
				return nil
			}
			if vv, ok := v.(map[string]any); ok {
				return vv
			}
			return nil
		}(heir)["Data"].(int)
		data = int(func(v any) map[string]any {
			if v == nil {
				return nil
			}
			if vv, ok := v.(map[string]any); ok {
				return vv
			}
			return nil
		}(heir)["Data"].(int))
	}
	var dir int = 0
	_ = dir
	if node["Data"].(int) < data {
		dir = 1
	}
	var r map[string]any = removeR(getLink(node, dir), data)
	_ = r
	setLink(node, dir, r["node"])
	if r["done"].(bool) {
		return func(v any) map[string]any {
			if v == nil {
				return nil
			}
			if vv, ok := v.(map[string]any); ok {
				return vv
			}
			return nil
		}(map[string]any{"node": node, "done": true})
	}
	node["Balance"] = ((node["Balance"].(int) + 1) - (2 * dir))
	if (node["Balance"].(int) == 1) || (node["Balance"].(int) == (0 - 1)) {
		return func(v any) map[string]any {
			if v == nil {
				return nil
			}
			if vv, ok := v.(map[string]any); ok {
				return vv
			}
			return nil
		}(map[string]any{"node": node, "done": true})
	}
	if node["Balance"].(int) == 0 {
		return func(v any) map[string]any {
			if v == nil {
				return nil
			}
			if vv, ok := v.(map[string]any); ok {
				return vv
			}
			return nil
		}(map[string]any{"node": node, "done": false})
	}
	return removeBalance(node, dir)
}

func Remove(tree any, data int) any {
	var r map[string]any = removeR(tree, data)
	_ = r
	_ = r
	return r["node"]
}

func indentStr(n int) string {
	var s string = ""
	_ = s
	var i int = 0
	_ = i
	for i < n {
		s = (s + " ")
		i = (i + 1)
	}
	return s
}

func dumpNode(node any, indent int, comma bool) {
	var sp string = indentStr(indent)
	_ = sp
	if node == nil {
		var line string = (sp + "null")
		_ = line
		if comma {
			line = (line + ",")
		}
		fmt.Println(line)
	} else {
		fmt.Println((sp + "{"))
		fmt.Println((((indentStr((indent + 3)) + "\"Data\": ") + fmt.Sprint(func(v any) map[string]any {
			if v == nil {
				return nil
			}
			if vv, ok := v.(map[string]any); ok {
				return vv
			}
			return nil
		}(node)["Data"].(int))) + ","))
		fmt.Println((((indentStr((indent + 3)) + "\"Balance\": ") + fmt.Sprint(func(v any) map[string]any {
			if v == nil {
				return nil
			}
			if vv, ok := v.(map[string]any); ok {
				return vv
			}
			return nil
		}(node)["Balance"].(int))) + ","))
		fmt.Println((indentStr((indent + 3)) + "\"Link\": ["))
		dumpNode(getLink(func(v any) map[string]any {
			if v == nil {
				return nil
			}
			if vv, ok := v.(map[string]any); ok {
				return vv
			}
			return nil
		}(node), 0), (indent + 6), true)
		dumpNode(getLink(func(v any) map[string]any {
			if v == nil {
				return nil
			}
			if vv, ok := v.(map[string]any); ok {
				return vv
			}
			return nil
		}(node), 1), (indent + 6), false)
		fmt.Println((indentStr((indent + 3)) + "]"))
		var end string = (sp + "}")
		_ = end
		if comma {
			end = (end + ",")
		}
		fmt.Println(end)
	}
}

func dump(node any, indent int) {
	dumpNode(node, indent, false)
}

func mochiMain() {
	var tree any = nil
	_ = tree
	fmt.Println("Empty tree:")
	dump(tree, 0)
	fmt.Println("")
	fmt.Println("Insert test:")
	tree = Insert(tree, 3)
	tree = Insert(tree, 1)
	tree = Insert(tree, 4)
	tree = Insert(tree, 1)
	tree = Insert(tree, 5)
	dump(tree, 0)
	fmt.Println("")
	fmt.Println("Remove test:")
	tree = Remove(tree, 3)
	tree = Remove(tree, 1)
	var t map[string]any = func(v any) map[string]any {
		if v == nil {
			return nil
		}
		if vv, ok := v.(map[string]any); ok {
			return vv
		}
		return nil
	}(tree)
	_ = t
	t["Balance"] = 0
	tree = t
	dump(tree, 0)
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
