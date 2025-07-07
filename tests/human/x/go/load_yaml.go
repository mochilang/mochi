//go:build ignore

package main

import (
    "fmt"
    "os"

    "gopkg.in/yaml.v2"
)

type Person struct {
    Name  string `yaml:"name"`
    Age   int    `yaml:"age"`
    Email string `yaml:"email"`
}

func main() {
    data, err := os.ReadFile("../../../interpreter/valid/people.yaml")
    if err != nil {
        panic(err)
    }

    var people []Person
    if err := yaml.Unmarshal(data, &people); err != nil {
        panic(err)
    }

    for _, p := range people {
        if p.Age >= 18 {
            fmt.Println(p.Name, p.Email)
        }
    }
}
