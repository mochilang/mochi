//go:build ignore
// +build ignore

if x := fetchSomething(); x > 0 {
    DoPos(x)
} else {
    DoNeg(x)
}
