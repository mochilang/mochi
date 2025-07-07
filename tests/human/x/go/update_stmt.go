//go:build ignore

package main

import "fmt"

type Person struct {
    Name   string
    Age    int
    Status string
}

func main() {
    people := []Person{
        {"Alice", 17, "minor"},
        {"Bob", 25, "unknown"},
        {"Charlie", 18, "unknown"},
        {"Diana", 16, "minor"},
    }

    for i, p := range people {
        if p.Age >= 18 {
            people[i].Status = "adult"
            people[i].Age = p.Age + 1
        }
    }

    expected := []Person{
        {"Alice", 17, "minor"},
        {"Bob", 26, "adult"},
        {"Charlie", 19, "adult"},
        {"Diana", 16, "minor"},
    }

    match := true
    if len(people) != len(expected) {
        match = false
    } else {
        for i := range people {
            if people[i] != expected[i] {
                match = false
                break
            }
        }
    }

    if !match {
        fmt.Println("mismatch")
        fmt.Println("got:", people)
        fmt.Println("expected:", expected)
        return
    }

    fmt.Println("ok")
}
