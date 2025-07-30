package fs

type Var struct {
	Name    string `json:"name"`
	Expr    string `json:"expr"`
	Mutable bool   `json:"mutable"`
	Type    string `json:"type"`
	Line    int    `json:"line"`
	Raw     string `json:"raw"`
}

type Assign struct {
	Name  string `json:"name"`
	Index string `json:"index"`
	Expr  string `json:"expr"`
	Line  int    `json:"line"`
	Raw   string `json:"raw"`
}

type ForIn struct {
	Var  string `json:"var"`
	Expr string `json:"expr"`
	Body []Stmt `json:"body"`
	Line int    `json:"line"`
	Raw  string `json:"raw"`
}

type PrintStmt struct {
	Expr string `json:"expr"`
	Line int    `json:"line"`
	Raw  string `json:"raw"`
}

type Stmt interface{}
