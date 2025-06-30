package fuzz

import (
	"fmt"
	"mochi/types"
)

// newDatasetEnv creates an environment with several datasets bound to
// sequential identifier names (v1, v2, ...). Each dataset is a slice of
// maps so queries have something to operate on.
func newDatasetEnv() *types.Env {
	env := types.NewEnv(nil)
	sample := []any{
		map[string]any{"a": 1, "b": 2},
		map[string]any{"a": 2, "b": 3},
	}
	for i := 1; i <= 10; i++ {
		env.SetValue(fmt.Sprintf("v%d", i), sample, true)
	}
	return env
}
