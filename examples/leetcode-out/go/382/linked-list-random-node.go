package main

func expect(cond bool) {
	if !cond { panic("expect failed") }
}

type RandomPicker struct {
	Values []int `json:"values"`
	Seed int `json:"seed"`
}

type PickResult struct {
	Picker RandomPicker `json:"picker"`
	Value int `json:"value"`
}

func newRandomPicker(values []int, seed int) RandomPicker {
	return RandomPicker{Values: values, Seed: seed}
}

func nextSeed(seed int) int {
	return ((((seed * 1103515245) + 12345)) % 2147483648)
}

func getRandom(p RandomPicker) PickResult {
	var seed2 int = nextSeed(p.Seed)
	var idx int = (seed2 % len(p.Values))
	var np RandomPicker = RandomPicker{Values: p.Values, Seed: seed2}
	return PickResult{Picker: np, Value: p.Values[idx]}
}

func single_element() {
	var p RandomPicker = newRandomPicker([]int{5}, 1)
	var r1 PickResult = getRandom(p)
	_ = r1
	expect((r1.Value == 5))
	var r2 PickResult = getRandom(r1.Picker)
	_ = r2
	expect((r2.Value == 5))
}

func deterministic_sequence() {
	var p RandomPicker = newRandomPicker([]int{10, 20, 30}, 42)
	var r1 PickResult = getRandom(p)
	_ = r1
	expect((r1.Value == 10))
	p = r1.Picker
	var r2 PickResult = getRandom(p)
	_ = r2
	expect((r2.Value == 30))
	p = r2.Picker
	var r3 PickResult = getRandom(p)
	_ = r3
	expect((r3.Value == 30))
}

func main() {
	single_element()
	deterministic_sequence()
}

