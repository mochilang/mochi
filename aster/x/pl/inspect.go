package pl

import prolog "mochi/aster/x/prolog"

// Inspect parses Prolog source code using SWI-Prolog.
func Inspect(src string) (*Program, error) {
	return prolog.Inspect(src)
}

// InspectWithOption behaves like Inspect but allows callers to request
// positional information via Option.
func InspectWithOption(src string, opt Option) (*Program, error) {
	return prolog.InspectWithOption(src, prolog.Option(opt))
}
