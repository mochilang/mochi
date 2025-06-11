package testpkg

import "errors"

// Pi is the mathematical constant pi.
const Pi = 3.14

// Answer is the answer to life, the universe, and everything.
var Answer = 42

// Point is a simple struct used for testing.
type Point struct {
	X int // X coordinate
	Y int // Y coordinate
}

// Add sums a and b.
func Add(a, b int) int { return a + b }

// Fail always returns an error.
func Fail() error { return errors.New("boom") }
