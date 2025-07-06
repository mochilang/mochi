package c

type function struct {
	name   string
	ret    string
	params []param
	body   []string
	line   int
}

type param struct {
	name string
	typ  string
}
