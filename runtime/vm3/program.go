package vm3

// Program is one compilation unit. Functions are indexed by Funcs[i],
// matching OpCall*.C. Entry is the index of the function the
// interpreter starts on when Run is called with no explicit fn.
type Program struct {
	Funcs []*Function
	Entry uint32
}
