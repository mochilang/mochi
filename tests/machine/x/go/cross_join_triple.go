//go:build ignore

package main

import (
    "fmt"
    "strings"
    "reflect"
)

func getField(v interface{}, name string) interface{} {
    if m, ok := v.(map[interface{}]interface{}); ok {
        return m[name]
    }
    val := reflect.ValueOf(v)
    name = strings.Title(name)
    if val.Kind() == reflect.Pointer {
        val = val.Elem()
    }
    f := val.FieldByName(name)
    if f.IsValid() {
        return f.Interface()
    }
    return nil
}

func main() {
    nums := []int{1, 2}
    letters := []interface{}{"A", "B"}
    bools := []interface{}{true, false}
    combos := func() []struct{ N interface{}; L interface{}; B interface{} } {
        var _res []struct{ N interface{}; L interface{}; B interface{} }
        for _, n := range nums {
            for _, l := range letters {
                for _, b := range bools {
                    _res = append(_res, struct{ N interface{}; L interface{}; B interface{} }{N: n, L: l, B: b})
                }
            }
        }
        return _res
    }()
    fmt.Println("--- Cross Join of three lists ---")
    for _, c := range combos {
        fmt.Println(getField(c, "n"), getField(c, "l"), getField(c, "b"))
    }
}
