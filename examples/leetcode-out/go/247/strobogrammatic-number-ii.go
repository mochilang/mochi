package main

import (
	"reflect"
)

func expect(cond bool) {
	if !cond { panic("expect failed") }
}

func findStrobogrammatic(n int) []string {
	var build func(int, int) []string
	build = func(len int, total int) []string {
		if (len == 0) {
			return []string{""}
		}
		if (len == 1) {
			return []string{"0", "1", "8"}
		}
		var mids []string = build((len - 2), total)
		var result []string = []string{}
		for _, s := range mids {
			if (len != total) {
				result = append(append([]string{}, result...), []string{"0" + s + "0"}...)
			}
			result = append(append([]string{}, result...), []string{"1" + s + "1"}...)
			result = append(append([]string{}, result...), []string{"6" + s + "9"}...)
			result = append(append([]string{}, result...), []string{"8" + s + "8"}...)
			result = append(append([]string{}, result...), []string{"9" + s + "6"}...)
		}
		return result
}
	return build(n, n)
}

func n___1() {
	expect(_equal(findStrobogrammatic(1), []string{"0", "1", "8"}))
}

func n___2() {
	expect(_equal(findStrobogrammatic(2), []string{"11", "69", "88", "96"}))
}

func n___3() {
	expect(_equal(findStrobogrammatic(3), []string{"101", "609", "808", "906", "111", "619", "818", "916", "181", "689", "888", "986"}))
}

func main() {
	n___1()
	n___2()
	n___3()
}

func _equal(a, b any) bool {
    av := reflect.ValueOf(a)
    bv := reflect.ValueOf(b)
    if av.Kind() == reflect.Slice && bv.Kind() == reflect.Slice {
        if av.Len() != bv.Len() { return false }
        for i := 0; i < av.Len(); i++ {
            if !_equal(av.Index(i).Interface(), bv.Index(i).Interface()) { return false }
        }
        return true
    }
    return reflect.DeepEqual(a, b)
}

