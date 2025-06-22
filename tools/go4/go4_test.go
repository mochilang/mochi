package main

import (
	"io/ioutil"
	"os"
	"os/exec"
	"testing"
)

func TestRunHelloGo(t *testing.T) {
	src := `package main
import "fmt"
func main() { fmt.Println("hello, world") }`
	tmp, err := ioutil.TempFile(".", "hello-*.go")
	if err != nil {
		t.Fatal(err)
	}
	defer os.Remove(tmp.Name())
	if _, err := tmp.WriteString(src); err != nil {
		t.Fatal(err)
	}
	tmp.Close()
	cmd := exec.Command("go", "run", "./go4.go", "--", tmp.Name())
	out, err := cmd.CombinedOutput()
	if err != nil {
		t.Fatalf("go4 failed: %v\n%s", err, out)
	}
	if string(out) != "hello, world\n" {
		t.Fatalf("unexpected output: %q", out)
	}
}

func TestRunSelf(t *testing.T) {
	cmd := exec.Command("go", "run", "./go4.go", "--", "./go4.go")
	out, err := cmd.CombinedOutput()
	if err != nil {
		t.Fatalf("go4 failed: %v\n%s", err, out)
	}
	if len(out) != 0 {
		t.Fatalf("expected no output, got %q", out)
	}
}

func TestIfReturn(t *testing.T) {
	src := `package main
import "fmt"

func foo() int {
    if 1 == 1 {
        return 42
    }
    return 0
}

func main() {
    fmt.Println(foo())
}`
	tmp, err := ioutil.TempFile(".", "ifret-*.go")
	if err != nil {
		t.Fatal(err)
	}
	defer os.Remove(tmp.Name())
	if _, err := tmp.WriteString(src); err != nil {
		t.Fatal(err)
	}
	tmp.Close()
	cmd := exec.Command("go", "run", "./go4.go", "--", tmp.Name())
	out, err := cmd.CombinedOutput()
	if err != nil {
		t.Fatalf("go4 failed: %v\n%s", err, out)
	}
	if string(out) != "42\n" {
		t.Fatalf("unexpected output: %q", out)
	}
}
