package c

type function struct {
	name      string
	ret       string
	params    []param
	body      []string
	startLine int
	endLine   int
	source    string
	signature string
}

type param struct {
	name string
	typ  string
}
