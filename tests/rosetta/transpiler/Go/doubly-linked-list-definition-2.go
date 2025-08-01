//go:build ignore

// Generated by Mochi v0.10.42 on 2025-07-27 22:40:53 GMT+7
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

func _toMapIntMapStringAny(v any) map[int]map[string]any {
	if v == nil {
		return nil
	}
	if m, ok := v.(map[int]map[string]any); ok {
		return m
	}
	out := make(map[int]map[string]any)
	if m, ok := v.(map[string]any); ok {
		for k, vv := range m {
			if id, err := strconv.Atoi(k); err == nil {
				if sub, ok := vv.(map[string]any); ok {
					out[id] = sub
				}
			}
		}
	}
	return out
}

func newList() map[string]any {
	return map[string]any{"nodes": map[string]any{}, "head": 0, "tail": 0, "nextID": 1}
}

func newNode(l map[string]any, v any) map[string]any {
	var id int = l["nextID"].(int)
	_ = id
	l["nextID"] = (id + 1)
	var nodes map[int]map[string]any = _toMapIntMapStringAny(l["nodes"])
	_ = nodes
	n := map[string]any{"id": id, "value": v, "next": 0, "prev": 0}
	_ = n
	nodes[id] = n
	l["nodes"] = nodes
	return n
}

func pushFront(l map[string]any, v any) map[string]any {
	var n map[string]any = newNode(l, v)
	_ = n
	n["next"] = l["head"].(int)
	if l["head"].(int) != 0 {
		var nodes map[int]map[string]any = _toMapIntMapStringAny(l["nodes"])
		_ = nodes
		var h map[string]any = nodes[l["head"].(int)]
		_ = h
		h["prev"] = n["id"].(int)
		nodes[h["id"].(int)] = h
		l["nodes"] = nodes
	} else {
		l["tail"] = n["id"].(int)
	}
	l["head"] = n["id"].(int)
	var nodes2 map[int]map[string]any = _toMapIntMapStringAny(l["nodes"])
	_ = nodes2
	nodes2[n["id"].(int)] = n
	l["nodes"] = nodes2
	return n
}

func pushBack(l map[string]any, v any) map[string]any {
	var n map[string]any = newNode(l, v)
	_ = n
	n["prev"] = l["tail"].(int)
	if l["tail"].(int) != 0 {
		var nodes map[int]map[string]any = _toMapIntMapStringAny(l["nodes"])
		_ = nodes
		var t map[string]any = nodes[l["tail"].(int)]
		_ = t
		t["next"] = n["id"].(int)
		nodes[t["id"].(int)] = t
		l["nodes"] = nodes
	} else {
		l["head"] = n["id"].(int)
	}
	l["tail"] = n["id"].(int)
	var nodes2 map[int]map[string]any = _toMapIntMapStringAny(l["nodes"])
	_ = nodes2
	nodes2[n["id"].(int)] = n
	l["nodes"] = nodes2
	return n
}

func insertBefore(l map[string]any, refID int, v any) map[string]any {
	if refID == 0 {
		return pushFront(l, v)
	}
	var nodes map[int]map[string]any = _toMapIntMapStringAny(l["nodes"])
	_ = nodes
	var ref map[string]any = nodes[refID]
	_ = ref
	var n map[string]any = newNode(l, v)
	_ = n
	n["prev"] = ref["prev"].(int)
	n["next"] = ref["id"].(int)
	if ref["prev"].(int) != 0 {
		var p map[string]any = nodes[ref["prev"].(int)]
		_ = p
		p["next"] = n["id"].(int)
		nodes[p["id"].(int)] = p
	} else {
		l["head"] = n["id"].(int)
	}
	ref["prev"] = n["id"].(int)
	nodes[refID] = ref
	nodes[n["id"].(int)] = n
	l["nodes"] = nodes
	return n
}

func insertAfter(l map[string]any, refID int, v any) map[string]any {
	if refID == 0 {
		return pushBack(l, v)
	}
	var nodes map[int]map[string]any = _toMapIntMapStringAny(l["nodes"])
	_ = nodes
	var ref map[string]any = nodes[refID]
	_ = ref
	var n map[string]any = newNode(l, v)
	_ = n
	n["next"] = ref["next"].(int)
	n["prev"] = ref["id"].(int)
	if ref["next"].(int) != 0 {
		var nx map[string]any = nodes[ref["next"].(int)]
		_ = nx
		nx["prev"] = n["id"].(int)
		nodes[nx["id"].(int)] = nx
	} else {
		l["tail"] = n["id"].(int)
	}
	ref["next"] = n["id"].(int)
	nodes[refID] = ref
	nodes[n["id"].(int)] = n
	l["nodes"] = nodes
	return n
}

func mochiMain() {
	var l map[string]any = newList()
	_ = l
	var e4 map[string]any = pushBack(l, 4)
	_ = e4
	var e1 map[string]any = pushFront(l, 1)
	_ = e1
	insertBefore(l, e4["id"].(int), 3)
	insertAfter(l, e1["id"].(int), "two")
	var id int = l["head"].(int)
	_ = id
	var nodes map[int]map[string]any = _toMapIntMapStringAny(l["nodes"])
	_ = nodes
	for id != 0 {
		var node map[string]any = nodes[id]
		_ = node
		fmt.Println(fmt.Sprint(node["value"]))
		id = node["next"].(int)
	}
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
