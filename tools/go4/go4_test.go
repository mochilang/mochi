package main

import (
	"io/ioutil"
	"os"
	"os/exec"
	"strings"
	"testing"
)

const helloC = `#include <stdio.h>
int main(){printf("hello, world\n");return 0;}`

const helloGo = `package main
import "fmt"
func main(){fmt.Println("hello, go")}`

func TestHelloC(t *testing.T) {
	tmp, err := ioutil.TempFile(".", "hello-*.c")
	if err != nil {
		t.Fatal(err)
	}
	defer os.Remove(tmp.Name())
	if _, err := tmp.WriteString(helloC); err != nil {
		t.Fatal(err)
	}
	tmp.Close()
	// build go4 binary
	if err := exec.Command("go", "build", "-o", "go4", "./main.go").Run(); err != nil {
		t.Fatal(err)
	}
	defer os.Remove("go4")

	cmd := exec.Command("./go4", tmp.Name())
	out, err := cmd.CombinedOutput()
	if err != nil {
		t.Fatalf("go4 failed: %v\n%s", err, out)
	}
	if !strings.HasPrefix(string(out), "hello, world") {
		t.Fatalf("unexpected output: %q", out)
	}
}

func TestHelloGo(t *testing.T) {
	tmp, err := ioutil.TempFile(".", "hello-*.go")
	if err != nil {
		t.Fatal(err)
	}
	defer os.Remove(tmp.Name())
	if _, err := tmp.WriteString(helloGo); err != nil {
		t.Fatal(err)
	}
	tmp.Close()

	if err := exec.Command("go", "build", "-o", "go4", "./main.go").Run(); err != nil {
		t.Fatal(err)
	}
	defer os.Remove("go4")

	cmd := exec.Command("./go4", tmp.Name())
	out, err := cmd.CombinedOutput()
	if err != nil {
		t.Fatalf("go4 failed: %v\n%s", err, out)
	}
	if !strings.Contains(string(out), "hello, go") {
		t.Fatalf("unexpected output: %q", out)
	}
}

func TestChainGoToC(t *testing.T) {
	tmpC, err := ioutil.TempFile(".", "hello-*.c")
	if err != nil {
		t.Fatal(err)
	}
	defer os.Remove(tmpC.Name())
	if _, err := tmpC.WriteString(helloC); err != nil {
		t.Fatal(err)
	}
	tmpC.Close()

	if err := exec.Command("go", "build", "-o", "go4", "./main.go").Run(); err != nil {
		t.Fatal(err)
	}
	defer os.Remove("go4")

	cmd := exec.Command("./go4", "go4.go", tmpC.Name())
	out, err := cmd.CombinedOutput()
	if err != nil {
		t.Fatalf("go4 chain failed: %v\n%s", err, out)
	}
	if !strings.HasPrefix(string(out), "hello, world") {
		t.Fatalf("unexpected output: %q", out)
	}
}
