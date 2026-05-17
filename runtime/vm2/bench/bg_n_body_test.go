package bench

import (
	"math"
	"testing"

	"mochi/compiler2/corpus"
	"mochi/compiler2/emit"
	"mochi/compiler2/opt"
	vm2 "mochi/runtime/vm2"
)

// goNBodyKernel mirrors corpus.BuildNBodyKernel exactly: 5 bodies with
// position (i, 2i, 3i), velocity (i/10, i/5, 3i/10), mass i+1; one
// advance step at dt=0.01; return system energy.
func goNBodyKernel() float64 {
	const N = 5
	const dt = 0.01
	x := make([]float64, N)
	y := make([]float64, N)
	z := make([]float64, N)
	vx := make([]float64, N)
	vy := make([]float64, N)
	vz := make([]float64, N)
	m := make([]float64, N)
	for i := 0; i < N; i++ {
		fi := float64(i)
		x[i] = fi
		y[i] = fi * 2
		z[i] = fi * 3
		vx[i] = fi * 0.1
		vy[i] = fi * 0.2
		vz[i] = fi * 0.3
		m[i] = fi + 1
	}
	// advance: pairwise velocity update
	for i := 0; i < N; i++ {
		for j := i + 1; j < N; j++ {
			dx := x[i] - x[j]
			dy := y[i] - y[j]
			dz := z[i] - z[j]
			d2 := dx*dx + dy*dy + dz*dz
			mag := dt / (d2 * math.Sqrt(d2))
			miMag := m[i] * mag
			mjMag := m[j] * mag
			vx[i] -= dx * mjMag
			vy[i] -= dy * mjMag
			vz[i] -= dz * mjMag
			vx[j] += dx * miMag
			vy[j] += dy * miMag
			vz[j] += dz * miMag
		}
	}
	// posUpdate
	for i := 0; i < N; i++ {
		x[i] += vx[i] * dt
		y[i] += vy[i] * dt
		z[i] += vz[i] * dt
	}
	// energy
	var e float64
	for i := 0; i < N; i++ {
		kin := 0.5 * m[i] * (vx[i]*vx[i] + vy[i]*vy[i] + vz[i]*vz[i])
		var pot float64
		for j := i + 1; j < N; j++ {
			dx := x[i] - x[j]
			dy := y[i] - y[j]
			dz := z[i] - z[j]
			r := math.Sqrt(dx*dx + dy*dy + dz*dz)
			pot += m[i] * m[j] / r
		}
		e += kin - pot
	}
	return e
}

func TestBGNBodyKernel(t *testing.T) {
	m := corpus.BuildNBodyKernel()
	for _, f := range m.Funcs {
		opt.ConstFold(f)
		opt.DCE(f)
		opt.TailCall(f)
	}
	prog, err := emit.Compile(m)
	if err != nil {
		t.Fatalf("compile: %v", err)
	}
	got, err := vm2.New(prog).Run()
	if err != nil {
		t.Fatalf("run: %v", err)
	}
	if !got.IsFloat() {
		t.Fatalf("expected float, got %#v", got)
	}
	want := goNBodyKernel()
	if d := math.Abs(got.Float() - want); d > 1e-10 {
		t.Fatalf("n_body kernel = %v, want %v (delta %v)", got.Float(), want, d)
	}
}

func BenchmarkVM2_BG_NBodyKernel(b *testing.B) {
	m := corpus.BuildNBodyKernel()
	for _, f := range m.Funcs {
		opt.ConstFold(f)
		opt.DCE(f)
		opt.TailCall(f)
	}
	prog, err := emit.Compile(m)
	if err != nil {
		b.Fatalf("compile: %v", err)
	}
	b.ReportAllocs()
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		if _, err := vm2.New(prog).Run(); err != nil {
			b.Fatalf("run: %v", err)
		}
	}
}

func BenchmarkGo_BG_NBodyKernel(b *testing.B) {
	b.ReportAllocs()
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		_ = goNBodyKernel()
	}
}
