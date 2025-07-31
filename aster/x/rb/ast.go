package rb

// Typed aliases for common Ruby AST node kinds used in tests.
// These simply embed Node so the JSON representation retains the
// same structure while providing a more descriptive API.
type (
	ProgramNode             struct{ Node }
	Comment                 struct{ Node }
	Assignment              struct{ Node }
	Call                    struct{ Node }
	ArgumentList            struct{ Node }
	Array                   struct{ Node }
	Pair                    struct{ Node }
	Identifier              struct{ Node }
	Constant                struct{ Node }
	Integer                 struct{ Node }
	String                  struct{ Node }
	StringContent           struct{ Node }
	SimpleSymbol            struct{ Node }
	HashKeySymbol           struct{ Node }
	True                    struct{ Node }
	False                   struct{ Node }
	Binary                  struct{ Node }
	Begin                   struct{ Node }
	DoBlock                 struct{ Node }
	BlockParameters         struct{ Node }
	BodyStatement           struct{ Node }
	ElementReference        struct{ Node }
	ParenthesizedStatements struct{ Node }
	Method                  struct{ Node }
	MethodParameters        struct{ Node }
	Return                  struct{ Node }
	Range                   struct{ Node }
	If                      struct{ Node }
	Then                    struct{ Node }
	Unary                   struct{ Node }
)
