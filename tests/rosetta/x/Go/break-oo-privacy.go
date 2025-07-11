//go:build ignore

package main

import (
	"bufio"
	"errors"
	"fmt"
	"os"
	"reflect"
	"unsafe"
)

type foobar struct {
	Exported   int
	unexported int
}

func main() {
	obj := foobar{12, 42}
	fmt.Println("obj:", obj)

	examineAndModify(&obj)
	fmt.Println("obj:", obj)

	anotherExample()
}

func examineAndModify(any interface{}) {
	v := reflect.ValueOf(any).Elem()
	fmt.Println(" v:", v, "=", v.Interface())
	t := v.Type()
	fmt.Printf("    %3s %-10s %-4s %s\n", "Idx", "Name", "Type", "CanSet")
	for i := 0; i < v.NumField(); i++ {
		f := v.Field(i)
		fmt.Printf("    %2d: %-10s %-4s %t\n", i, t.Field(i).Name, f.Type(), f.CanSet())
	}
	v.Field(0).SetInt(16)
	p := (*int)(unsafe.Pointer(v.Field(1).Addr().Pointer()))
	*p = 44
	fmt.Println("  modified unexported field via unsafe")
}

func anotherExample() {
	r := bufio.NewReader(os.Stdin)
	errp := (*error)(unsafe.Pointer(reflect.ValueOf(r).Elem().FieldByName("err").Addr().Pointer()))
	*errp = errors.New("unsafely injected error value into bufio inner workings")
	_, err := r.ReadByte()
	fmt.Println("bufio.ReadByte returned error:", err)
}
