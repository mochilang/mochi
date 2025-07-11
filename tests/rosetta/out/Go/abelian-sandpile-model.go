//go:build ignore

package main

import (
	"encoding/json"
	"fmt"
)

// line 16
func handlePile(p *[][]int, x int, y int) {
	var dim int = len(p)
	if p[y][x] >= 4 {
		p[y][x] = (p[y][x] - 4)
		if y > 0 {
			p[(y - 1)][x] = (p[(y - 1)][x] + 1)
			if p[(y - 1)][x] >= 4 {
				handlePile(p, x, (y - 1))
			}
		}
		if x > 0 {
			p[y][(x - 1)] = (p[y][(x-1)] + 1)
			if p[y][(x-1)] >= 4 {
				handlePile(p, (x - 1), y)
			}
		}
		if y < (dim - 1) {
			p[(y + 1)][x] = (p[(y + 1)][x] + 1)
			if p[(y + 1)][x] >= 4 {
				handlePile(p, x, (y + 1))
			}
		}
		if x < (dim - 1) {
			p[y][(x + 1)] = (p[y][(x+1)] + 1)
			if p[y][(x+1)] >= 4 {
				handlePile(p, (x + 1), y)
			}
		}
		handlePile(p, x, y)
	}
}

// line 40
func drawPile(p [][]int) {
	var chars []string = []string{
		" ",
		"░",
		"▓",
		"█",
	}
	var dim int = len(p)
	var y int = 0
	for {
		if !(y < dim) {
			break
		}
		var line string = ""
		var x int = 0
		for {
			if !(x < dim) {
				break
			}
			var elem int = p[y][x]
			if elem > 3 {
				elem = 3
			}
			line = line + chars[elem]
			x = (x + 1)
		}
		fmt.Println(line)
		y = (y + 1)
	}
}

var y int

func main() {
	y = 0
	var DIM int = 16
	var pile [][]int = [][]int{}
	for {
		if !(y < DIM) {
			break
		}
		var row []int = []int{}
		var x int = 0
		for {
			if !(x < DIM) {
				break
			}
			row = append(_convSlice[int, any](row), 0)
			x = (x + 1)
		}
		pile = append(_convSlice[[]int, any](pile), row)
		y = (y + 1)
	}
	var hdim int = (_cast[int]((float64(DIM) / float64(2))) - 1)
	pile[hdim][hdim] = 16
	handlePile(pile, hdim, hdim)
	drawPile(pile)
}

func _cast[T any](v any) T {
	if tv, ok := v.(T); ok {
		return tv
	}
	var out T
	switch any(out).(type) {
	case int:
		switch vv := v.(type) {
		case int:
			return any(vv).(T)
		case float64:
			return any(int(vv)).(T)
		case float32:
			return any(int(vv)).(T)
		}
	case float64:
		switch vv := v.(type) {
		case int:
			return any(float64(vv)).(T)
		case float64:
			return any(vv).(T)
		case float32:
			return any(float64(vv)).(T)
		}
	case float32:
		switch vv := v.(type) {
		case int:
			return any(float32(vv)).(T)
		case float64:
			return any(float32(vv)).(T)
		case float32:
			return any(vv).(T)
		}
	}
	if m, ok := v.(map[any]any); ok {
		v = _convertMapAny(m)
	}
	data, err := json.Marshal(v)
	if err != nil {
		panic(err)
	}
	if err := json.Unmarshal(data, &out); err != nil {
		panic(err)
	}
	return out
}

func _convSlice[T any, U any](s []T) []U {
	out := []U{}
	for _, v := range s {
		out = append(out, any(v).(U))
	}
	return out
}

func _convertMapAny(m map[any]any) map[string]any {
	out := make(map[string]any, len(m))
	for k, v := range m {
		key := fmt.Sprint(k)
		if sub, ok := v.(map[any]any); ok {
			out[key] = _convertMapAny(sub)
		} else {
			out[key] = v
		}
	}
	return out
}
