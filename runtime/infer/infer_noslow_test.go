//go:build !slow

package infer_test

import "testing"

func TestInferSkipped(t *testing.T) {
	t.Skip("slow tests disabled; build with -tags=slow to enable")
}
