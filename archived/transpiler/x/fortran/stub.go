//go:build !slow

package fortran

var benchMain bool

// SetBenchMain is a no-op in the stub build.
func SetBenchMain(v bool) { benchMain = v }
