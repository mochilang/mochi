package testpkg

import "errors"

const Pi = 3.14

var Answer = 42

// Point is a simple struct used for testing.
type Point struct {
	X int
	Y int
}

func Add(a, b int) int { return a + b }

func Fail() error { return errors.New("boom") }
