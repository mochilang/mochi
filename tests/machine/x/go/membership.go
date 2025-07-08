//go:build ignore

package main

import (
    "fmt"
    "reflect"
)

func contains(coll interface{}, v interface{}) bool {
    val := reflect.ValueOf(coll)
    switch val.Kind() {
        case reflect.Slice, reflect.Array:
            for i := 0; i < val.Len(); i++ {
                if reflect.DeepEqual(val.Index(i).Interface(), v) {
                    return true
                }
            }
            return false
        case reflect.Map:
            if val.MapIndex(reflect.ValueOf(v)).IsValid() {
                return true
            }
            return false
        default:
            return false
        }
    }
    
    func main() {
    nums := []int{1, 2, 3}
    fmt.Println(contains(nums, 2))
    fmt.Println(contains(nums, 4))
    }
