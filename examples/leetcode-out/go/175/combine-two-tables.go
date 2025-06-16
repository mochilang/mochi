package main

import (
	"fmt"
)

func expect(cond bool) {
	if !cond { panic("expect failed") }
}

type Person struct {
	PersonId int `json:"personId"`
	FirstName string `json:"firstName"`
	LastName string `json:"lastName"`
}

type Address struct {
	AddressId int `json:"addressId"`
	PersonId int `json:"personId"`
	City string `json:"city"`
	State string `json:"state"`
}

type Result struct {
	FirstName string `json:"firstName"`
	LastName string `json:"lastName"`
	City string `json:"city"`
	State string `json:"state"`
}

func combineTables(persons []Person, addresses []Address) []Result {
	var results []Result = []Result{}
	for _, p := range persons {
		var found bool = false
		for _, a := range addresses {
			if (a.PersonId == p.PersonId) {
				results = append(append([]Result{}, results...), []Result{Result{FirstName: p.FirstName, LastName: p.LastName, City: a.City, State: a.State}}...)
				found = true
				break
			}
		}
		if !found {
			results = append(append([]Result{}, results...), []Result{Result{FirstName: p.FirstName, LastName: p.LastName, City: "", State: ""}}...)
		}
	}
	return results
}

func combine_tables() {
	var expected []Result = []Result{Result{FirstName: "Wang", LastName: "Allen", City: "", State: ""}, Result{FirstName: "Alice", LastName: "Bob", City: "New York City", State: "New York"}, Result{FirstName: "Bob", LastName: "Brown", City: "Leetcode", State: "California"}}
	_ = expected
	expect((fmt.Sprint(combineTables(person, address)) == fmt.Sprint(expected)))
}

var person []Person = []Person{Person{PersonId: 1, FirstName: "Wang", LastName: "Allen"}, Person{PersonId: 2, FirstName: "Alice", LastName: "Bob"}, Person{PersonId: 3, FirstName: "Bob", LastName: "Brown"}}
var address []Address = []Address{Address{AddressId: 1, PersonId: 2, City: "New York City", State: "New York"}, Address{AddressId: 2, PersonId: 3, City: "Leetcode", State: "California"}}
func main() {
	combine_tables()
}

