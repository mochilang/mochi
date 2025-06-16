package main

import (
	"fmt"
	"reflect"
	"sort"
)

func expect(cond bool) {
	if !cond { panic("expect failed") }
}

type Shuffler struct {
	Original []int `json:"original"`
	Nums []int `json:"nums"`
	Seed int `json:"seed"`
}

type RandResult struct {
	Sh Shuffler `json:"sh"`
	Value int `json:"value"`
}

func copyList(xs []int) []int {
	var out []int = []int{}
	for _, x := range xs {
		out = append(append([]int{}, out...), []int{x}...)
	}
	return out
}

func nextRand(s Shuffler) RandResult {
	var newSeed int = ((((s.Seed * 1103515245) + 12345)) % 2147483648)
	var newSh Shuffler = Shuffler{Original: s.Original, Nums: s.Nums, Seed: newSeed}
	return RandResult{Sh: newSh, Value: newSeed}
}

func newShuffler(nums []int) Shuffler {
	return Shuffler{Original: nums, Nums: copyList(nums), Seed: 1}
}

func reset(sh Shuffler) Shuffler {
	return Shuffler{Original: sh.Original, Nums: copyList(sh.Original), Seed: sh.Seed}
}

func shuffle(sh Shuffler) Shuffler {
	var arr []int = copyList(sh.Nums)
	var state Shuffler = sh
	var i int = (len(arr) - 1)
	for (i > 0) {
		var r RandResult = nextRand(state)
		_ = r
		state = r.Sh
		var j int = (r.Value % ((i + 1)))
		var tmp int = arr[i]
		arr[i] = arr[j]
		arr[j] = tmp
		i = (i - 1)
	}
	return Shuffler{Original: state.Original, Nums: arr, Seed: state.Seed}
}

func example() {
	var s Shuffler = newShuffler([]int{1, 2, 3})
	s = shuffle(s)
	var _first []int = s.Nums
	_ = _first
	s = reset(s)
	expect(_equal(s.Nums, []int{1, 2, 3}))
	s = shuffle(s)
	var sorted []int = func() []int {
	items := []int{}
	for _, x := range s.Nums {
		items = append(items, x)
	}
	type pair struct { item int; key any }
	pairs := make([]pair, len(items))
	for idx, it := range items {
		x := it
		pairs[idx] = pair{item: it, key: x}
	}
	sort.Slice(pairs, func(i, j int) bool {
		a, b := pairs[i].key, pairs[j].key
		switch av := a.(type) {
		case int:
			switch bv := b.(type) {
			case int:
				return av < bv
			case float64:
				return float64(av) < bv
			}
		case float64:
			switch bv := b.(type) {
			case int:
				return av < float64(bv)
			case float64:
				return av < bv
			}
		case string:
			bs, _ := b.(string)
			return av < bs
		}
		return fmt.Sprint(a) < fmt.Sprint(b)
	})
	for idx, p := range pairs {
		items[idx] = p.item
	}
	_res := []int{}
	for _, x := range items {
		_res = append(_res, x)
	}
	return _res
}()
	_ = sorted
	expect(_equal(sorted, []int{1, 2, 3}))
}

func empty() {
	var s Shuffler = newShuffler([]int{})
	s = shuffle(s)
	expect(_equal(s.Nums, []any{}))
	s = reset(s)
	expect(_equal(s.Nums, []any{}))
}

func main() {
	example()
	empty()
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

