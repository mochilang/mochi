package testpkg

import (
	"fmt"
	"math/big"
	"math/bits"
	"strings"
)

var one = big.NewInt(1)
var two = big.NewInt(2)
var three = big.NewInt(3)
var eight = big.NewInt(8)

// ackermannBig computes the Ackermann function using big.Int.
func ackermannBig(m, n *big.Int) *big.Int {
	if m.Cmp(three) <= 0 {
		switch m.Int64() {
		case 0:
			return new(big.Int).Add(n, one)
		case 1:
			return new(big.Int).Add(n, two)
		case 2:
			r := new(big.Int).Lsh(n, 1)
			return r.Add(r, three)
		case 3:
			if nb := n.BitLen(); nb > bits.UintSize {
				panic(TooBigError(nb))
			}
			r := new(big.Int).Lsh(eight, uint(n.Int64()))
			return r.Sub(r, three)
		}
	}
	if n.BitLen() == 0 {
		return ackermannBig(new(big.Int).Sub(m, one), one)
	}
	return ackermannBig(new(big.Int).Sub(m, one), ackermannBig(m, new(big.Int).Sub(n, one)))
}

// TooBigError is returned when n is too large to compute.
type TooBigError int

func (e TooBigError) Error() string {
	return fmt.Sprintf("A(m,n) had n of %d bits; too large", int(e))
}

// AckermannExample returns the example output from the Rosetta Code task.
func AckermannExample() string {
	var b strings.Builder
	show := func(m, n int64) {
		defer func() {
			if e := recover(); e != nil {
				if err, ok := e.(TooBigError); ok {
					fmt.Fprintf(&b, "A(%d, %d) = Error: %v\n", m, n, err)
				} else {
					panic(e)
				}
			}
		}()
		a := ackermannBig(big.NewInt(m), big.NewInt(n))
		fmt.Fprintf(&b, "A(%d, %d) = ", m, n)
		if a.BitLen() <= 256 {
			fmt.Fprintln(&b, a)
		} else {
			s := a.String()
			fmt.Fprintf(&b, "%d digits starting/ending with: %s...%s\n", len(s), s[:20], s[len(s)-20:])
		}
	}

	show(0, 0)
	show(1, 2)
	show(2, 4)
	show(3, 100)
	show(3, 1e6)
	show(4, 1)
	show(4, 2)
	show(4, 3)
	return b.String()
}
