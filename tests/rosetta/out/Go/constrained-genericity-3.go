//go:build ignore

// Generated by Mochi compiler v0.10.28 on 2006-01-02T15:04:05Z

package main

import (
	"fmt"
	"strings"
)

type v = PeelFirst

type PeelFirst struct {
	Value string `json:"value"`
}

func (s *PeelFirst) Eat() {
	fmt.Println(strings.TrimSuffix(fmt.Sprintln(any("mm, that "+s.Value+" was good!")), "\n"))
}

func main() {
}
