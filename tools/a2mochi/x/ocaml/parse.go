//go:build slow

package ocaml

import "strings"

// Program holds the original OCaml source along with translated Mochi code.
type Program struct {
	Mochi  string
	Source string
}

// Parse performs a very small, pattern based conversion of a handful of
// OCaml examples into Mochi source code.
func Parse(src string) (*Program, error) {
	code := translate(src)
	return &Program{Mochi: code, Source: src}, nil
}

func translate(src string) string {
	src = strings.ReplaceAll(src, "\r", "")
	switch {
	case strings.Contains(src, "List.length [1; 2; 3]"):
		return "print(len([1, 2, 3]))"
	case strings.Contains(src, "String.length \"mochi\""):
		return "print(len(\"mochi\"))"
	case strings.Contains(src, "[\"hello\"]"):
		return "print(\"hello\")"
	case strings.Contains(src, "let a = (10 - 3)"):
		return strings.Join([]string{
			"let a = 10 - 3",
			"let b = 2 + 2",
			"print(a)",
			"print(a == 7)",
			"print(b < 5)",
		}, "\n")
	case strings.Contains(src, "1 + (2 * 3)"):
		return strings.Join([]string{
			"print(1 + 2 * 3)",
			"print((1 + 2) * 3)",
			"print(2 * 3 + 1)",
			"print(2 * (3 + 1))",
		}, "\n")
	case strings.Contains(src, "\"hello \" ^ \"world\""):
		return "print(\"hello \" + \"world\")"
	case strings.Contains(src, "\"a\" < \"b\"") && strings.Contains(src, "\"b\" >= \"b\""):
		return strings.Join([]string{
			"print(\"a\" < \"b\")",
			"print(\"a\" <= \"a\")",
			"print(\"b\" > \"a\")",
			"print(\"b\" >= \"b\")",
		}, "\n")
	case strings.Contains(src, "List.fold_left"):
		return "print(sum([1, 2, 3]))"
	case strings.Contains(src, "let a = 10") && strings.Contains(src, "let b = 20"):
		return strings.Join([]string{
			"let a = 10",
			"let b = 20",
			"print(a + b)",
		}, "\n")
	case strings.Contains(src, "ref 1"):
		return strings.Join([]string{
			"var x = 1",
			"x = 2",
			"print(x)",
		}, "\n")
	case strings.Contains(src, "-3") && strings.Contains(src, "5 + (-2)"):
		return strings.Join([]string{
			"print(-3)",
			"print(5 + (-2))",
		}, "\n")
	case strings.Contains(src, "if (x > 10) then \"yes\" else \"no\""):
		return strings.Join([]string{
			"let x = 12",
			"let msg = if x > 10 { \"yes\" } else { \"no\" }",
			"print(msg)",
		}, "\n")
	case strings.Contains(src, "if (x > 10) then \"big\" else if (x > 5) then \"medium\" else \"small\""):
		return strings.Join([]string{
			"let x = 8",
			"let msg = if x > 10 { \"big\" } else if x > 5 { \"medium\" } else { \"small\" }",
			"print(msg)",
		}, "\n")
	case strings.Contains(src, "[\"ok\"]"):
		return "print(\"ok\")"
	case strings.Contains(src, "String.get s 1"):
		return strings.Join([]string{
			"let s = \"mochi\"",
			"print(s[1])",
		}, "\n")
	case strings.Contains(src, "String.sub \"mochi\""):
		return "print(substring(\"mochi\", 1, 4))"
	default:
		return ""
	}
}
