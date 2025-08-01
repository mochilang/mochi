//go:build ignore

// Generated by Mochi v0.10.42 on 2025-07-28 10:18:22 GMT+7
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

var INF int

var graph map[string]map[string]int

func addEdge(u string, v string, w int) {
	if !func() bool { _, ok := graph[u]; return ok }() {
		graph[u] = map[string]int{}
	}
	graph[u][v] = w
	if !func() bool { _, ok := graph[v]; return ok }() {
		graph[v] = map[string]int{}
	}
}

func removeAt(xs []string, idx int) []string {
	var out []string = []string{}
	_ = out
	var i int = 0
	_ = i
	for _, _ch := range xs {
		x := string(_ch)
		if i != idx {
			out = append(out, x)
		}
		i = (i + 1)
	}
	return out
}

func dijkstra(source string) map[string]any {
	var dist map[string]int = map[string]int{}
	_ = dist
	var prev map[string]string = map[string]string{}
	_ = prev
	for _, v := range func() []string {
		keys := make([]string, 0, len(graph))
		for k := range graph {
			keys = append(keys, k)
		}
		sort.Slice(keys, func(i, j int) bool { return fmt.Sprint(keys[i]) < fmt.Sprint(keys[j]) })
		return keys
	}() {
		dist[v] = INF
		prev[v] = ""
	}
	dist[source] = 0
	var q []string = []string{}
	_ = q
	for _, v := range func() []string {
		keys := make([]string, 0, len(graph))
		for k := range graph {
			keys = append(keys, k)
		}
		sort.Slice(keys, func(i, j int) bool { return fmt.Sprint(keys[i]) < fmt.Sprint(keys[j]) })
		return keys
	}() {
		q = append(q, v)
	}
	for len(q) > 0 {
		var bestIdx int = 0
		_ = bestIdx
		var u string = q[0]
		_ = u
		var i int = 1
		_ = i
		for i < len(q) {
			var v string = q[i]
			_ = v
			if dist[v] < dist[u] {
				u = v
				bestIdx = i
			}
			i = (i + 1)
		}
		q = removeAt(func(v any) []string {
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
		}(q), bestIdx)
		for _, v := range func() []string {
			keys := make([]string, 0, len(graph[u]))
			for k := range graph[u] {
				keys = append(keys, k)
			}
			sort.Slice(keys, func(i, j int) bool { return fmt.Sprint(keys[i]) < fmt.Sprint(keys[j]) })
			return keys
		}() {
			var alt int = (dist[u] + graph[u][v])
			_ = alt
			if alt < dist[v] {
				dist[v] = alt
				prev[v] = u
			}
		}
	}
	return map[string]any{"dist": dist, "prev": prev}
}

func path(prev map[string]string, v string) string {
	var s string = v
	_ = s
	var cur string = v
	_ = cur
	for prev[cur] != "" {
		cur = prev[cur]
		s = (cur + s)
	}
	return s
}

func mochiMain() {
	addEdge("a", "b", 7)
	addEdge("a", "c", 9)
	addEdge("a", "f", 14)
	addEdge("b", "c", 10)
	addEdge("b", "d", 15)
	addEdge("c", "d", 11)
	addEdge("c", "f", 2)
	addEdge("d", "e", 6)
	addEdge("e", "f", 9)
	var res map[string]any = dijkstra("a")
	_ = res
	var dist map[string]int = func(v any) map[string]int {
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
	}(res["dist"])
	_ = dist
	var prev map[string]string = func(v any) map[string]string {
		if v == nil {
			return nil
		}
		if vv, ok := v.(map[string]string); ok {
			return vv
		}
		return nil
	}(res["prev"])
	_ = prev
	fmt.Println(((("Distance to e: " + fmt.Sprint(dist["e"])) + ", Path: ") + path(prev, "e")))
	fmt.Println(((("Distance to f: " + fmt.Sprint(dist["f"])) + ", Path: ") + path(prev, "f")))
}

func main() {
	func() {
		var ms runtime.MemStats
		runtime.ReadMemStats(&ms)
		startMem := ms.Alloc
		benchStart := time.Now().UnixNano()
		INF = 1000000000
		graph = map[string]map[string]int{}
		mochiMain()
		runtime.ReadMemStats(&ms)
		endMem := ms.Alloc
		benchEnd := time.Now().UnixNano()
		data := map[string]any{"name": "main", "duration_us": (benchEnd - benchStart) / 1000, "memory_bytes": endMem - startMem}
		out, _ := json.MarshalIndent(data, "", "  ")
		fmt.Println(string(out))
	}()
}
