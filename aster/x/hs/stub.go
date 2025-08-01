//go:build !slow

package hs

type Program struct{}

func Inspect(src string) (*Program, error) { return &Program{}, nil }

func Print(p *Program) (string, error) { return "", nil }
