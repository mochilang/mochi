package vm

// Profile holds execution counts for each instruction in a program.
type Profile struct {
	Funcs [][]int
}

// NewProfile allocates a profile for prog with zero counts.
func NewProfile(prog *Program) *Profile {
	p := &Profile{Funcs: make([][]int, len(prog.Funcs))}
	for i, fn := range prog.Funcs {
		p.Funcs[i] = make([]int, len(fn.Code))
	}
	return p
}
