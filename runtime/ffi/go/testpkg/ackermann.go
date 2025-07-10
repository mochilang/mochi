package testpkg

import (
	"fmt"
	"math/big"
	"math/bits"
	"strings"
)

var ackOne = big.NewInt(1)
var ackTwo = big.NewInt(2)
var ackThree = big.NewInt(3)
var ackEight = big.NewInt(8)

type ackTooBig int

func (e ackTooBig) Error() string {
	return fmt.Sprintf("A(m,n) had n of %d bits; too large", int(e))
}

func ackermannBig(m, n *big.Int) *big.Int {
	if m.Cmp(ackThree) <= 0 {
		switch m.Int64() {
		case 0:
			return new(big.Int).Add(n, ackOne)
		case 1:
			return new(big.Int).Add(n, ackTwo)
		case 2:
			r := new(big.Int).Lsh(n, 1)
			return r.Add(r, ackThree)
		case 3:
			if nb := n.BitLen(); nb > bits.UintSize {
				panic(ackTooBig(nb))
			}
			r := new(big.Int).Lsh(ackEight, uint(n.Int64()))
			return r.Sub(r, ackThree)
		}
	}
	if n.BitLen() == 0 {
		return ackermannBig(new(big.Int).Sub(m, ackOne), ackOne)
	}
	return ackermannBig(new(big.Int).Sub(m, ackOne), ackermannBig(m, new(big.Int).Sub(n, ackOne)))
}

// AckermannExample returns output for the Rosetta Code Ackermann function task.
func AckermannExample() string {
	var b strings.Builder
	show := func(m, n int64) {
		defer func() {
			if e := recover(); e != nil {
				if err, ok := e.(ackTooBig); ok {
					fmt.Fprintf(&b, "A(%d, %d) = Error: %v\n", m, n, err)
				} else {
					panic(e)
				}
			}
		}()
		a := ackermannBig(big.NewInt(m), big.NewInt(n))
		fmt.Fprintf(&b, "A(%d, %d) = ", m, n)
		if a.BitLen() <= 256 {
			fmt.Fprintf(&b, "%s\n", a.String())
		} else {
			s := a.String()
			fmt.Fprintf(&b, "%d digits starting/ending with: %s...%s\n", len(s), s[:20], s[len(s)-20:])
		}
	}
	show(0, 0)
	show(1, 2)
	show(2, 4)
	show(3, 100)
	show(3, 1000000)
	show(4, 1)
	show(4, 2)
	show(4, 3)
	return b.String()
}
