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
    nums := []int{1, 2, 3}
    letters := []interface{}{"A", "B"}
    pairs := func() []struct{ N interface{}; L interface{} } {
        var _res []struct{ N interface{}; L interface{} }
        for _, n := range nums {
            for _, l := range letters {
                if n % 2 == 0 {
                    _res = append(_res, struct{ N interface{}; L interface{} }{N: n, L: l})
                }
            }
        }
        return _res
    }()
    fmt.Println("--- Even pairs ---")
    for _, p := range pairs {
        fmt.Println(getField(p, "n"), getField(p, "l"))
    }
}
