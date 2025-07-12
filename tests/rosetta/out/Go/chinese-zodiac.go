//go:build ignore

package main

import (
	"fmt"
	"reflect"
)

type Info struct {
	Animal     string `json:"animal"`
	YinYang    string `json:"yinYang"`
	Element    string `json:"element"`
	StemBranch string `json:"stemBranch"`
	Cycle      int    `json:"cycle"`
}

// line 17
func cz(yr int, animal []string, yinYang []string, element []string, sc []string, bc []string) Info {
	var y int = (yr - 4)
	var stem int = (y % 10)
	var branch int = (y % 12)
	var sb string = sc[stem] + bc[branch]
	return Info{animal[branch], yinYang[(stem % 2)], element[int((float64(stem) / float64(2)))], sb, ((y % 60) + 1)}
}

var animal []string
var yinYang []string
var element []string

func main() {
	animal = []string{
		"Rat",
		"Ox",
		"Tiger",
		"Rabbit",
		"Dragon",
		"Snake",
		"Horse",
		"Goat",
		"Monkey",
		"Rooster",
		"Dog",
		"Pig",
	}
	yinYang = []string{"Yang", "Yin"}
	element = []string{
		"Wood",
		"Fire",
		"Earth",
		"Metal",
		"Water",
	}
	var stemChArr []string = []string{
		"甲",
		"乙",
		"丙",
		"丁",
		"戊",
		"己",
		"庚",
		"辛",
		"壬",
		"癸",
	}
	var branchChArr []string = []string{
		"子",
		"丑",
		"寅",
		"卯",
		"辰",
		"巳",
		"午",
		"未",
		"申",
		"酉",
		"戌",
		"亥",
	}
	for _, yr := range []int{
		1935,
		1938,
		1968,
		1972,
		1976,
	} {
		var r Info = cz(yr, animal, yinYang, element, stemChArr, branchChArr)
		_ = r
		_print(fmt.Sprint(yr) + ": " + r.Element + " " + r.Animal + ", " + r.YinYang + ", Cycle year " + fmt.Sprint(r.Cycle) + " " + r.StemBranch)
	}
}

func _print(args ...any) {
	first := true
	for _, a := range args {
		if !first {
			fmt.Print(" ")
		}
		first = false
		rv := reflect.ValueOf(a)
		if a == nil || ((rv.Kind() == reflect.Map || rv.Kind() == reflect.Slice) && rv.IsNil()) {
			fmt.Print("<nil>")
			continue
		}
		if rv.Kind() == reflect.Slice && rv.Type().Elem().Kind() != reflect.Uint8 {
			for i := 0; i < rv.Len(); i++ {
				if i > 0 {
					fmt.Print(" ")
				}
				fmt.Print(_sprint(rv.Index(i).Interface()))
			}
			continue
		}
		fmt.Print(_sprint(a))
	}
	fmt.Println()
}

func _sprint(v any) string {
	if v == nil {
		return "<nil>"
	}
	rv := reflect.ValueOf(v)
	if (rv.Kind() == reflect.Map || rv.Kind() == reflect.Slice) && rv.IsNil() {
		return "<nil>"
	}
	return fmt.Sprint(v)
}
