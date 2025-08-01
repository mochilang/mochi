//go:build ignore

// Generated by Mochi v0.10.42 on 2025-07-27 22:51:02 GMT+7
package main

import (
	"encoding/json"
	"fmt"
	"os"
	"runtime"
	"sort"
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

var EVEN_ROOT int

var ODD_ROOT int

func newNode(len int) map[string]any {
	return map[string]any{"length": len, "edges": map[string]any{}, "suffix": 0}
}

func eertree(s string) []map[string]any {
	var tree []map[string]any = []map[string]any{}
	_ = tree
	tree = append(tree, map[string]any{"length": 0, "suffix": ODD_ROOT, "edges": map[string]any{}})
	tree = append(tree, map[string]any{"length": (0 - 1), "suffix": ODD_ROOT, "edges": map[string]any{}})
	var suffix int = ODD_ROOT
	_ = suffix
	var i int = 0
	_ = i
	for i < len(s) {
		var c string = string([]rune(s)[i:(i + 1)])
		_ = c
		var n int = suffix
		_ = n
		var k int = 0
		_ = k
		for {
			k = tree[n]["length"].(int)
			var b int = ((i - k) - 1)
			_ = b
			if (b >= 0) && (string([]rune(s)[b:(b+1)]) == c) {
				break
			}
			n = tree[n]["suffix"].(int)
		}
		var edges map[string]int = func(v any) map[string]int {
			if v == nil {
				return map[string]int{}
			}
			if vv, ok := v.(map[string]int); ok {
				return vv
			}
			out := make(map[string]int)
			if m, ok := v.(map[string]any); ok {
				for k, vv := range m {
					if vi, ok2 := vv.(int); ok2 {
						out[k] = vi
					}
				}
			}
			return out
		}(tree[n]["edges"])
		_ = edges
		if func() bool { _, ok := edges[c]; return ok }() {
			suffix = edges[c]
			i = (i + 1)
			continue
		}
		suffix = len(tree)
		tree = append(tree, newNode((k + 2)))
		edges[c] = suffix
		tree[n]["edges"] = edges
		if tree[suffix]["length"].(int) == 1 {
			tree[suffix]["suffix"] = 0
			i = (i + 1)
			continue
		}
		for {
			n = tree[n]["suffix"].(int)
			var b int = ((i - tree[n]["length"].(int)) - 1)
			_ = b
			if (b >= 0) && (string([]rune(s)[b:(b+1)]) == c) {
				break
			}
		}
		var en map[string]int = func(v any) map[string]int {
			if v == nil {
				return map[string]int{}
			}
			if vv, ok := v.(map[string]int); ok {
				return vv
			}
			out := make(map[string]int)
			if m, ok := v.(map[string]any); ok {
				for k, vv := range m {
					if vi, ok2 := vv.(int); ok2 {
						out[k] = vi
					}
				}
			}
			return out
		}(tree[n]["edges"])
		_ = en
		tree[suffix]["suffix"] = en[c]
		i = (i + 1)
	}
	return tree
}

func child(tree []map[string]any, idx int, p string, acc []string) []string {
	var edges map[string]int = func(v any) map[string]int {
		if v == nil {
			return map[string]int{}
		}
		if vv, ok := v.(map[string]int); ok {
			return vv
		}
		out := make(map[string]int)
		if m, ok := v.(map[string]any); ok {
			for k, vv := range m {
				if vi, ok2 := vv.(int); ok2 {
					out[k] = vi
				}
			}
		}
		return out
	}(tree[idx]["edges"])
	_ = edges
	for _, ch := range func() []string {
		keys := make([]string, 0, len(edges))
		for k := range edges {
			keys = append(keys, k)
		}
		sort.Slice(keys, func(i, j int) bool { return fmt.Sprint(keys[i]) < fmt.Sprint(keys[j]) })
		return keys
	}() {
		var nxt int = edges[ch]
		_ = nxt
		var pal string = ((ch + p) + ch)
		_ = pal
		acc = append(acc, pal)
		acc = child(func(v any) []map[string]any {
			if v == nil {
				return nil
			}
			if vv, ok := v.([]map[string]any); ok {
				return vv
			}
			if arr, ok := v.([]any); ok {
				if len(arr) == 0 {
					return []map[string]any{}
				}
				out := make([]map[string]any, len(arr))
				for i, x := range arr {
					out[i] = x.(map[string]any)
				}
				return out
			}
			return v.([]map[string]any)
		}(tree), nxt, pal, func(v any) []string {
			if v == nil {
				return nil
			}
			if vv, ok := v.([]string); ok {
				return vv
			}
			if arr, ok := v.([]any); ok {
				if len(arr) == 0 {
					return []string{}
				}
				out := make([]string, len(arr))
				for i, x := range arr {
					out[i] = x.(string)
				}
				return out
			}
			return v.([]string)
		}(acc))
	}
	return acc
}

func subPalindromes(tree []map[string]any) []string {
	var res []string = []string{}
	_ = res
	res = child(func(v any) []map[string]any {
		if v == nil {
			return nil
		}
		if vv, ok := v.([]map[string]any); ok {
			return vv
		}
		if arr, ok := v.([]any); ok {
			if len(arr) == 0 {
				return []map[string]any{}
			}
			out := make([]map[string]any, len(arr))
			for i, x := range arr {
				out[i] = x.(map[string]any)
			}
			return out
		}
		return v.([]map[string]any)
	}(tree), EVEN_ROOT, "", func(v any) []string {
		if v == nil {
			return nil
		}
		if vv, ok := v.([]string); ok {
			return vv
		}
		if arr, ok := v.([]any); ok {
			if len(arr) == 0 {
				return []string{}
			}
			out := make([]string, len(arr))
			for i, x := range arr {
				out[i] = x.(string)
			}
			return out
		}
		return v.([]string)
	}(res))
	var oEdges map[string]int = func(v any) map[string]int {
		if v == nil {
			return map[string]int{}
		}
		if vv, ok := v.(map[string]int); ok {
			return vv
		}
		out := make(map[string]int)
		if m, ok := v.(map[string]any); ok {
			for k, vv := range m {
				if vi, ok2 := vv.(int); ok2 {
					out[k] = vi
				}
			}
		}
		return out
	}(tree[ODD_ROOT]["edges"])
	_ = oEdges
	for _, ch := range func() []string {
		keys := make([]string, 0, len(oEdges))
		for k := range oEdges {
			keys = append(keys, k)
		}
		sort.Slice(keys, func(i, j int) bool { return fmt.Sprint(keys[i]) < fmt.Sprint(keys[j]) })
		return keys
	}() {
		res = append(res, ch)
		res = child(func(v any) []map[string]any {
			if v == nil {
				return nil
			}
			if vv, ok := v.([]map[string]any); ok {
				return vv
			}
			if arr, ok := v.([]any); ok {
				if len(arr) == 0 {
					return []map[string]any{}
				}
				out := make([]map[string]any, len(arr))
				for i, x := range arr {
					out[i] = x.(map[string]any)
				}
				return out
			}
			return v.([]map[string]any)
		}(tree), oEdges[ch], ch, func(v any) []string {
			if v == nil {
				return nil
			}
			if vv, ok := v.([]string); ok {
				return vv
			}
			if arr, ok := v.([]any); ok {
				if len(arr) == 0 {
					return []string{}
				}
				out := make([]string, len(arr))
				for i, x := range arr {
					out[i] = x.(string)
				}
				return out
			}
			return v.([]string)
		}(res))
	}
	return res
}

func mochiMain() {
	var tree []map[string]any = eertree("eertree")
	_ = tree
	var subs []string = subPalindromes(func(v any) []map[string]any {
		if v == nil {
			return nil
		}
		if vv, ok := v.([]map[string]any); ok {
			return vv
		}
		if arr, ok := v.([]any); ok {
			if len(arr) == 0 {
				return []map[string]any{}
			}
			out := make([]map[string]any, len(arr))
			for i, x := range arr {
				out[i] = x.(map[string]any)
			}
			return out
		}
		return v.([]map[string]any)
	}(tree))
	_ = subs
	fmt.Println(fmt.Sprint(subs))
}

func main() {
	func() {
		var ms runtime.MemStats
		runtime.ReadMemStats(&ms)
		startMem := ms.Alloc
		benchStart := time.Now().UnixNano()
		EVEN_ROOT = 0
		ODD_ROOT = 1
		mochiMain()
		runtime.ReadMemStats(&ms)
		endMem := ms.Alloc
		benchEnd := time.Now().UnixNano()
		data := map[string]any{"name": "main", "duration_us": (benchEnd - benchStart) / 1000, "memory_bytes": endMem - startMem}
		out, _ := json.MarshalIndent(data, "", "  ")
		fmt.Println(string(out))
	}()
}
