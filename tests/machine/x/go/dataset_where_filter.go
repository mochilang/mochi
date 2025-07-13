//go:build ignore

package main

import "fmt"

type PeopleItem struct {
    Name string `json:"name"`
    Age  int    `json:"age"`
}

type Adults struct {
    Name      string `json:"name"`
    Age       int    `json:"age"`
    IsSenior  bool   `json:"is_senior"`
}

func main() {
    people := []PeopleItem{{"Alice", 30}, {"Bob", 15}, {"Charlie", 65}, {"Diana", 45}}
    adults := func() []Adults {
        results := []Adults{}
        for _, person := range people {
            if person.Age >= 18 {
                results = append(results, Adults{person.Name, person.Age, person.Age >= 60})
            }
        }
        return results
    }()
    fmt.Println("--- Adults ---")
    for _, person := range adults {
        note := ""
        if person.IsSenior {
            note = " (senior)"
        }
        fmt.Println(person.Name, "is", person.Age, note)
    }
}
