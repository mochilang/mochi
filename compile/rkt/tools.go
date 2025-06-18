package rktcode

import "os/exec"

func EnsureRacket() error {
	_, err := exec.LookPath("racket")
	return err
}
