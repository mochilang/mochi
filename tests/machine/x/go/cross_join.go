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
    customers := []interface{}{map[interface{}]interface{}{"id": 1, "name": "Alice"}, map[interface{}]interface{}{"id": 2, "name": "Bob"}, map[interface{}]interface{}{"id": 3, "name": "Charlie"}}
    orders := []interface{}{map[interface{}]interface{}{"id": 100, "customerId": 1, "total": 250}, map[interface{}]interface{}{"id": 101, "customerId": 2, "total": 125}, map[interface{}]interface{}{"id": 102, "customerId": 1, "total": 300}}
    result := func() []struct{ OrderId interface{}; OrderCustomerId interface{}; PairedCustomerName interface{}; OrderTotal interface{} } {
        var _res []struct{ OrderId interface{}; OrderCustomerId interface{}; PairedCustomerName interface{}; OrderTotal interface{} }
        for _, o := range orders {
            for _, c := range customers {
                _res = append(_res, struct{ OrderId interface{}; OrderCustomerId interface{}; PairedCustomerName interface{}; OrderTotal interface{} }{OrderId: getField(o, "id"), OrderCustomerId: getField(o, "customerId"), PairedCustomerName: getField(c, "name"), OrderTotal: getField(o, "total")})
            }
        }
        return _res
    }()
    fmt.Println("--- Cross Join: All order-customer pairs ---")
    for _, entry := range result {
        fmt.Println("Order", getField(entry, "orderId"), "(customerId:", getField(entry, "orderCustomerId"), ", total: $", getField(entry, "orderTotal"), ") paired with", getField(entry, "pairedCustomerName"))
    }
}
