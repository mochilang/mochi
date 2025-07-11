//go:build ignore

package main

import (
	"fmt"
	goffi "mochi/runtime/ffi/go"
	"reflect"
	"strings"
)

func main() {
	for _, pair := range [][]string{
		[]string{"d41d8cd98f00b204e9800998ecf8427e", ""},
		[]string{"0cc175b9c0f1b6a831c399e269772661", "a"},
		[]string{"900150983cd24fb0d6963f7d28e17f72", "abc"},
		[]string{"f96b697d7cb7938d525a2f31aaf161d0", "message digest"},
		[]string{"c3fcd3d76192e4007dfb496cca67e13b", "abcdefghijklmnopqrstuvwxyz"},
		[]string{"d174ab98d277d9f5a5611c2c9f419d9f", "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"},
		[]string{"57edf4a22be3c955ac49da2e2107b67a", "12345678901234567890" + "123456789012345678901234567890123456789012345678901234567890"},
		[]string{"e38ca1d920c4b8b8d3946b2c72f01680", "The quick brown fox jumped over the lazy dog's back"},
	} {
		var sum any = func() any { v, _ := goffi.Call("mochi/runtime/ffi/go/testpkg.MD5Hex", pair[1]); return v }()
		if !_equal(sum, pair[0]) {
			fmt.Println("MD5 fail")
			fmt.Println(strings.TrimRight(strings.Join([]string{fmt.Sprint("  for string,"), fmt.Sprint(pair[1])}, " "), " "))
			fmt.Println(strings.TrimRight(strings.Join([]string{fmt.Sprint("  expected:  "), fmt.Sprint(pair[0])}, " "), " "))
			fmt.Println(strings.TrimRight(strings.Join([]string{fmt.Sprint("  got:       "), fmt.Sprint(sum)}, " "), " "))
		}
	}
}

func _equal(a, b any) bool {
	av := reflect.ValueOf(a)
	bv := reflect.ValueOf(b)
	if av.Kind() == reflect.Slice && bv.Kind() == reflect.Slice {
		if av.Len() != bv.Len() {
			return false
		}
		for i := 0; i < av.Len(); i++ {
			if !_equal(av.Index(i).Interface(), bv.Index(i).Interface()) {
				return false
			}
		}
		return true
	}
	if av.Kind() == reflect.Map && bv.Kind() == reflect.Map {
		if av.Len() != bv.Len() {
			return false
		}
		for _, k := range av.MapKeys() {
			bvVal := bv.MapIndex(k)
			if !bvVal.IsValid() {
				return false
			}
			if !_equal(av.MapIndex(k).Interface(), bvVal.Interface()) {
				return false
			}
		}
		return true
	}
	if (av.Kind() == reflect.Int || av.Kind() == reflect.Int64 || av.Kind() == reflect.Float64) &&
		(bv.Kind() == reflect.Int || bv.Kind() == reflect.Int64 || bv.Kind() == reflect.Float64) {
		return av.Convert(reflect.TypeOf(float64(0))).Float() == bv.Convert(reflect.TypeOf(float64(0))).Float()
	}
	return reflect.DeepEqual(a, b)
}
