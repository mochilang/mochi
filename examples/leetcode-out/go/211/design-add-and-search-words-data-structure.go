package main

import (
	"encoding/json"
)

func expect(cond bool) {
	if !cond { panic("expect failed") }
}

func Node() map[string]any {
	return map[string]any{"end": false, "next": _cast[map[string]any](map[any]any{})}
}

func addWord(root map[string]any, word string) {
	var node map[string]any = root
	for i := 0; i < len(word); i++ {
		var ch string = _indexString(word, i)
		var nextMap map[string]any = _cast[map[string]any](node["next"])
		var child map[string]any = nil
		_tmp0 := ch
		_tmp1 := nextMap
		_, _tmp2 := _tmp1[_tmp0]
		if _tmp2 {
			child = _cast[map[string]any](nextMap[ch])
		} else {
			child = Node()
		}
		if (i == (len(word) - 1)) {
			child["end"] = true
		}
		nextMap[ch] = child
		node["next"] = nextMap
		node = child
	}
}

func searchHelper(node map[string]any, word string, index int) bool {
	if (index == len(word)) {
		return _cast[bool](node["end"])
	}
	var ch string = _indexString(word, index)
	var children map[string]any = _cast[map[string]any](node["next"])
	if (ch == ".") {
		for key := range children {
			var child map[string]any = _cast[map[string]any](children[key])
			if searchHelper(child, word, (index + 1)) {
				return true
			}
		}
		return false
	}
	_tmp3 := ch
	_tmp4 := children
	_, _tmp5 := _tmp4[_tmp3]
	if _tmp5 {
		return searchHelper(_cast[map[string]any](children[ch]), word, (index + 1))
	}
	return false
}

func search(root map[string]any, word string) bool {
	return searchHelper(root, word, 0)
}

func example_1() {
	var wd map[string]any = Node()
	addWord(wd, "bad")
	addWord(wd, "dad")
	addWord(wd, "mad")
	expect((search(wd, "pad") == false))
	expect((search(wd, "bad") == true))
	expect((search(wd, ".ad") == true))
	expect((search(wd, "b..") == true))
}

func main() {
	example_1()
}

func _cast[T any](v any) T {
    data, err := json.Marshal(v)
    if err != nil { panic(err) }
    var out T
    if err := json.Unmarshal(data, &out); err != nil { panic(err) }
    return out
}

func _indexString(s string, i int) string {
    runes := []rune(s)
    if i < 0 {
        i += len(runes)
    }
    if i < 0 || i >= len(runes) {
        panic("index out of range")
    }
    return string(runes[i])
}

