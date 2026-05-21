// Package decimal is the Mochi-semantics decimal runtime for the Go
// target. Mochi exposes a `decimal` type for money/precision-sensitive
// arithmetic (the existing VM stores it as a *big.Rat under the hood);
// we preserve that contract here so the emitter does not need to
// thread a third-party library through every arithmetic call.
//
// The API is intentionally narrow: New, Add, Sub, Mul, Div, Cmp,
// String. The four basic ops are total (Div panics on division by
// zero, matching the VM's behaviour). The String form is the
// canonical exact representation: integer / fraction with a leading
// minus, no trailing zeros, no scientific notation. Round adds the
// half-away-from-zero rounding the VM uses for the `round` builtin.
package decimal

import (
	"fmt"
	"math/big"
	"strings"
)

// Decimal is a precise rational number. The zero value is 0/1.
type Decimal struct {
	r big.Rat
}

// New constructs a Decimal from an integer.
func New(n int64) Decimal { return Decimal{r: *big.NewRat(n, 1)} }

// FromString parses a Decimal from a base-10 string. Accepts the
// forms `123`, `-1.5`, `0.001`, `1e3`, `2.5e-1`. Returns an error on
// malformed input.
func FromString(s string) (Decimal, error) {
	r, ok := new(big.Rat).SetString(strings.TrimSpace(s))
	if !ok {
		return Decimal{}, fmt.Errorf("mochi/decimal: invalid decimal %q", s)
	}
	return Decimal{r: *r}, nil
}

// MustFromString is FromString that panics on error. Provided for
// constant literals the emitter knows are well-formed at compile time.
func MustFromString(s string) Decimal {
	d, err := FromString(s)
	if err != nil {
		panic(err)
	}
	return d
}

// Add returns a + b.
func Add(a, b Decimal) Decimal {
	var out Decimal
	out.r.Add(&a.r, &b.r)
	return out
}

// Sub returns a - b.
func Sub(a, b Decimal) Decimal {
	var out Decimal
	out.r.Sub(&a.r, &b.r)
	return out
}

// Mul returns a * b.
func Mul(a, b Decimal) Decimal {
	var out Decimal
	out.r.Mul(&a.r, &b.r)
	return out
}

// Div returns a / b. Panics if b is zero, matching the VM's
// DivByZero runtime error.
func Div(a, b Decimal) Decimal {
	if b.r.Sign() == 0 {
		panic("mochi/decimal: divide by zero")
	}
	var out Decimal
	out.r.Quo(&a.r, &b.r)
	return out
}

// Neg returns -a.
func Neg(a Decimal) Decimal {
	var out Decimal
	out.r.Neg(&a.r)
	return out
}

// Cmp returns -1, 0, or 1 as a < b, a == b, a > b.
func Cmp(a, b Decimal) int { return a.r.Cmp(&b.r) }

// IsZero reports whether d is exactly zero.
func (d Decimal) IsZero() bool { return d.r.Sign() == 0 }

// String returns the canonical decimal text. For exact rationals
// (terminating denominators) the result has no fractional digits past
// the shortest representation. For non-terminating rationals the
// result is `num/den` (matching big.Rat.String); the emitter uses
// Round to choose a fixed precision in those cases.
func (d Decimal) String() string { return d.r.RatString() }

// Float64 returns d as a float64; the returned `exact` bit reports
// whether the conversion is lossless.
func (d Decimal) Float64() (float64, bool) {
	f, exact := d.r.Float64()
	return f, exact
}

// Round returns d rounded to the given number of decimal places using
// half-away-from-zero rounding. Matches the VM's `round` builtin
// (runtime/vm/vm.go OpRound).
func Round(d Decimal, places int) Decimal {
	if places < 0 {
		places = 0
	}
	scale := big.NewInt(1)
	for i := 0; i < places; i++ {
		scale.Mul(scale, big.NewInt(10))
	}
	// Multiply by scale, round to nearest int away from zero, divide back.
	scaled := new(big.Rat).Mul(&d.r, new(big.Rat).SetInt(scale))
	num := new(big.Int).Set(scaled.Num())
	den := new(big.Int).Set(scaled.Denom())
	q, r := new(big.Int).QuoRem(num, den, new(big.Int))
	twoR := new(big.Int).Mul(r, big.NewInt(2))
	twoR.Abs(twoR)
	cmp := twoR.Cmp(den)
	if cmp > 0 || (cmp == 0 && num.Sign() != 0) {
		if num.Sign() >= 0 {
			q.Add(q, big.NewInt(1))
		} else {
			q.Sub(q, big.NewInt(1))
		}
	}
	out := new(big.Rat).SetFrac(q, scale)
	return Decimal{r: *out}
}
