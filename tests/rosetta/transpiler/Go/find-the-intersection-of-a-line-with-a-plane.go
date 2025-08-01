//go:build ignore

// Generated by Mochi v0.10.52 on 2025-08-01 19:02:53 GMT+7
package main

import (
	"fmt"
)

type Vector struct {
	X float64 `json:"x"`
	Y float64 `json:"y"`
	Z float64 `json:"z"`
}

func add(a Vector, b Vector) Vector {
	return Vector{
		X: (a.X + b.X),
		Y: (a.Y + b.Y),
		Z: (a.Z + b.Z),
	}
}

func sub(a Vector, b Vector) Vector {
	return Vector{
		X: (a.X - b.X),
		Y: (a.Y - b.Y),
		Z: (a.Z - b.Z),
	}
}

func mul(v Vector, s float64) Vector {
	return Vector{
		X: (v.X * s),
		Y: (v.Y * s),
		Z: (v.Z * s),
	}
}

func dot(a Vector, b Vector) float64 {
	return (((a.X * b.X) + (a.Y * b.Y)) + (a.Z * b.Z))
}

func intersectPoint(rv Vector, rp Vector, pn Vector, pp Vector) Vector {
	var diff Vector = sub(rp, pp)
	_ = diff
	var prod1 float64 = dot(diff, pn)
	_ = prod1
	var prod2 float64 = dot(rv, pn)
	_ = prod2
	var prod3 float64 = (prod1 / prod2)
	_ = prod3
	_ = prod3
	return sub(rp, mul(rv, prod3))
}

func mochiMain() {
	var rv Vector = Vector{
		X: 0.0,
		Y: (0 - 1.0),
		Z: (0 - 1.0),
	}
	_ = rv
	var rp Vector = Vector{
		X: 0.0,
		Y: 0.0,
		Z: 10.0,
	}
	_ = rp
	var pn Vector = Vector{
		X: 0.0,
		Y: 0.0,
		Z: 1.0,
	}
	_ = pn
	var pp Vector = Vector{
		X: 0.0,
		Y: 0.0,
		Z: 5.0,
	}
	_ = pp
	var ip Vector = intersectPoint(rv, rp, pn, pp)
	_ = ip
	fmt.Println((((((("The ray intersects the plane at (" + fmt.Sprint(ip.X)) + ", ") + fmt.Sprint(ip.Y)) + ", ") + fmt.Sprint(ip.Z)) + ")"))
}

func main() {
	mochiMain()
}
