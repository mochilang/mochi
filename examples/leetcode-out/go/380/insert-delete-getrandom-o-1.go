package main

import (
	"encoding/json"
	"time"
)

func expect(cond bool) {
	if !cond { panic("expect failed") }
}

type RandomizedSet struct {
	Nums []int `json:"nums"`
	Idx map[int]int `json:"idx"`
}

type InsertResult struct {
	Rs RandomizedSet `json:"rs"`
	Ok bool `json:"ok"`
}

type RemoveResult struct {
	Rs RandomizedSet `json:"rs"`
	Ok bool `json:"ok"`
}

func newSet() RandomizedSet {
	return RandomizedSet{Nums: _cast[[]int]([]any{}), Idx: _cast[map[int]int](map[any]any{})}
}

func copyMap(src map[int]int) map[int]int {
	var out map[int]int = map[int]int{}
	for k := range src {
		out[k] = src[k]
	}
	return out
}

func insert(rs RandomizedSet, val int) InsertResult {
	_tmp0 := val
	_tmp1 := rs.Idx
	_, _tmp2 := _tmp1[_tmp0]
	if _tmp2 {
		return InsertResult{Rs: rs, Ok: false}
	}
	var nums []int = append(append([]int{}, rs.Nums...), []int{val}...)
	var idxMap map[int]int = copyMap(rs.Idx)
	idxMap[val] = (len(nums) - 1)
	return InsertResult{Rs: RandomizedSet{Nums: nums, Idx: idxMap}, Ok: true}
}

func remove(rs RandomizedSet, val int) RemoveResult {
	_tmp3 := val
	_tmp4 := rs.Idx
	_, _tmp5 := _tmp4[_tmp3]
	if !(_tmp5) {
		return RemoveResult{Rs: rs, Ok: false}
	}
	var idx int = rs.Idx[val]
	var nums []int = rs.Nums
	var lastVal int = nums[(len(nums) - 1)]
	nums[idx] = lastVal
	nums = nums[0:(len(nums) - 1)]
	var idxMap map[int]int = copyMap(rs.Idx)
	idxMap[lastVal] = idx
	var newMap map[int]int = map[int]int{}
	for k := range idxMap {
		if (k != val) {
			newMap[k] = idxMap[k]
		}
	}
	return RemoveResult{Rs: RandomizedSet{Nums: nums, Idx: newMap}, Ok: true}
}

func getRandom(rs RandomizedSet) int {
	var i int64 = (int64(time.Now().UnixNano()) % int64(len(rs.Nums)))
	return rs.Nums[i]
}

func example() {
	var rs RandomizedSet = newSet()
	var a InsertResult = insert(rs, 1)
	_ = a
	rs = a.Rs
	expect((a.Ok == true))
	var b RemoveResult = remove(rs, 2)
	_ = b
	rs = b.Rs
	expect((b.Ok == false))
	var c InsertResult = insert(rs, 2)
	_ = c
	rs = c.Rs
	expect((c.Ok == true))
	var r int = getRandom(rs)
	_ = r
	expect(((((r == 1) || (r == 2))) == true))
	var d RemoveResult = remove(rs, 1)
	_ = d
	rs = d.Rs
	expect((d.Ok == true))
	var e InsertResult = insert(rs, 2)
	_ = e
	rs = e.Rs
	expect((e.Ok == false))
	expect((getRandom(rs) == 2))
}

func main() {
	example()
}

func _cast[T any](v any) T {
    data, err := json.Marshal(v)
    if err != nil { panic(err) }
    var out T
    if err := json.Unmarshal(data, &out); err != nil { panic(err) }
    return out
}

