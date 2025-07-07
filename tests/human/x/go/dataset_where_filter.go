package main

import "fmt"

type Person struct {
    Name string
    Age  int
}

func main() {
    people := []Person{
        {"Alice", 30},
        {"Bob", 15},
        {"Charlie", 65},
        {"Diana", 45},
    }

    var adults []struct {
        Name     string
        Age      int
        IsSenior bool
    }

    for _, p := range people {
        if p.Age >= 18 {
            adults = append(adults, struct {
                Name     string
                Age      int
                IsSenior bool
            }{p.Name, p.Age, p.Age >= 60})
        }
    }

    fmt.Println("--- Adults ---")
    for _, person := range adults {
        note := ""
        if person.IsSenior {
            note = " (senior)"
        }
        fmt.Printf("%s is %d%s\n", person.Name, person.Age, note)
    }
}
