package golden

import "testing"

func TestNormalizeOutput(t *testing.T) {
	root := "/root/project/mochi"
	input := []byte(root + "/tests/example\n" + root + "\n" + "github.com/mochi-lang/mochi/cmd\n" + "mochi/tests/foo\n" + "took (1.2ms)\n")
	got := normalizeOutput(root, input)
	want := []byte("tests/example\n\ncmd\ntests/foo\ntook (X)\n")
	if string(got) != string(want) {
		t.Fatalf("unexpected normalize result:\nGot: %q\nWant: %q", got, want)
	}
}
