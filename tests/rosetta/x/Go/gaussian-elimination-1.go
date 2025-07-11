//go:build ignore
// +build ignore

package main

import (
    "errors"
    "fmt"
    "log"
    "math"
)

type testCase struct {
    a [][]float64
    b []float64
    x []float64
}

var tc = testCase{
    // common RC example.  Result x computed with rational arithmetic then
    // converted to float64, and so should be about as close to correct as
    // float64 represention allows.
    a: [][]float64{
        {1.00, 0.00, 0.00, 0.00, 0.00, 0.00},
        {1.00, 0.63, 0.39, 0.25, 0.16, 0.10},
        {1.00, 1.26, 1.58, 1.98, 2.49, 3.13},
        {1.00, 1.88, 3.55, 6.70, 12.62, 23.80},
        {1.00, 2.51, 6.32, 15.88, 39.90, 100.28},
        {1.00, 3.14, 9.87, 31.01, 97.41, 306.02}},
    b: []float64{-0.01, 0.61, 0.91, 0.99, 0.60, 0.02},
    x: []float64{-0.01, 1.602790394502114, -1.6132030599055613,
        1.2454941213714368, -0.4909897195846576, 0.065760696175232},
}

// result from above test case turns out to be correct to this tolerance.
const ε = 1e-13

func main() {
    x, err := GaussPartial(tc.a, tc.b)
    if err != nil {
        log.Fatal(err)
    }
    fmt.Println(x)
    for i, xi := range x {
        if math.Abs(tc.x[i]-xi) > ε {
            log.Println("out of tolerance")
            log.Fatal("expected", tc.x)
        }
    }
}

func GaussPartial(a0 [][]float64, b0 []float64) ([]float64, error) {
    // make augmented matrix
    m := len(b0)
    a := make([][]float64, m)
    for i, ai := range a0 {
        row := make([]float64, m+1)
        copy(row, ai)
        row[m] = b0[i]
        a[i] = row
    }
    // WP algorithm from Gaussian elimination page
    // produces row-eschelon form
    for k := range a {
        // Find pivot for column k:
        iMax := k
        max := math.Abs(a[k][k])
        for i := k + 1; i < m; i++ {
            if abs := math.Abs(a[i][k]); abs > max {
                iMax = i
                max = abs
            }
        }
        if a[iMax][k] == 0 {
            return nil, errors.New("singular")
        }
        // swap rows(k, i_max)
        a[k], a[iMax] = a[iMax], a[k]
        // Do for all rows below pivot:
        for i := k + 1; i < m; i++ {
            // Do for all remaining elements in current row:
            for j := k + 1; j <= m; j++ {
                a[i][j] -= a[k][j] * (a[i][k] / a[k][k])
            }
            // Fill lower triangular matrix with zeros:
            a[i][k] = 0
        }
    }
    // end of WP algorithm.
    // now back substitute to get result.
    x := make([]float64, m)
    for i := m - 1; i >= 0; i-- {
        x[i] = a[i][m]
        for j := i + 1; j < m; j++ {
            x[i] -= a[i][j] * x[j]
        }
        x[i] /= a[i][i]
    }
    return x, nil
}
