package testpkg

import (
	"fmt"
	"math/big"
	"strings"
)

var nbCache = [][]*big.Int{{big.NewInt(1)}}

func nbMin(a, b int) int {
	if a < b {
		return a
	}
	return b
}

func nbCumu(n int) []*big.Int {
	for y := len(nbCache); y <= n; y++ {
		row := []*big.Int{big.NewInt(0)}
		for x := 1; x <= y; x++ {
			cacheValue := nbCache[y-x][nbMin(x, y-x)]
			row = append(row, new(big.Int).Add(row[len(row)-1], cacheValue))
		}
		nbCache = append(nbCache, row)
	}
	return nbCache[n]
}

// NineBillionRow returns a row of values as decimal strings.
func NineBillionRow(n int) []string {
	e := nbCumu(n)
	out := make([]string, n)
	for i := 0; i < n; i++ {
		diff := new(big.Int).Sub(e[i+1], e[i])
		out[i] = diff.Text(10)
	}
	return out
}

// NineBillionSum returns the cumulative sum for n as a decimal string.
func NineBillionSum(n int) string {
	r := nbCumu(n)
	return r[len(r)-1].Text(10)
}

// NineBillionExample returns the example output used by the Rosetta code task.
func NineBillionExample() string {
	var b strings.Builder
	b.WriteString("rows:\n")
	for x := 1; x < 11; x++ {
		row := NineBillionRow(x)
		for i, v := range row {
			if i > 0 || len(v) > 0 {
				b.WriteByte(' ')
			}
			b.WriteString(v)
			b.WriteByte(' ')
		}
		b.WriteByte('\n')
	}
	b.WriteByte('\n')
	b.WriteString("sums:\n")
	for _, num := range []int{23, 123, 1234} {
		fmt.Fprintf(&b, "%d %s\n", num, NineBillionSum(num))
	}
	return b.String()
}
