//go:build slow

package testpkg

import (
	"errors"
	"net"
	"net/http"
	"net/rpc"
	"time"
)

type TaxComputer float64

func (t TaxComputer) Tax(x float64, r *float64) error {
	if x < 0 {
		return errors.New("negative value")
	}
	*r = x * float64(t)
	return nil
}

// StartTaxServer starts an RPC server on localhost:1234 in a goroutine.
func StartTaxServer() error {
	rpc.Register(TaxComputer(0.05))
	rpc.HandleHTTP()
	l, err := net.Listen("tcp", ":1234")
	if err != nil {
		return err
	}
	go http.Serve(l, nil)
	time.Sleep(100 * time.Millisecond)
	return nil
}

// CallTax calls the Tax RPC on localhost:1234 with the given amount.
func CallTax(amount float64) (float64, error) {
	client, err := rpc.DialHTTP("tcp", "localhost:1234")
	if err != nil {
		return 0, err
	}
	var tax float64
	if err := client.Call("TaxComputer.Tax", amount, &tax); err != nil {
		return 0, err
	}
	return tax, nil
}
