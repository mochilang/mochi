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
	cmd := exec.Command("go", "run", "./main.go", tmp.Name())
	out, err := cmd.CombinedOutput()
	if err != nil {
		t.Fatalf("go4 failed: %v\n%s", err, out)
	}
	if !strings.HasPrefix(string(out), "hello, world") {
		t.Fatalf("unexpected output: %q", out)
	}
}

func TestSelfCompile(t *testing.T) {
	c4src, err := exec.Command("curl", "-sL", "https://raw.githubusercontent.com/rswier/c4/master/c4.c").Output()
	if err != nil {
		t.Skip("network unavailable")
	}
	tmpC4, err := ioutil.TempFile(".", "c4-*.c")
	if err != nil {
		t.Fatal(err)
	}
	defer os.Remove(tmpC4.Name())
	if _, err := tmpC4.Write(c4src); err != nil {
		t.Fatal(err)
	}
	tmpC4.Close()

	tmpHello, err := ioutil.TempFile(".", "hello-*.c")
	if err != nil {
		t.Fatal(err)
	}
	defer os.Remove(tmpHello.Name())
	if _, err := tmpHello.WriteString(helloC); err != nil {
		t.Fatal(err)
	}
	tmpHello.Close()

	cmd := exec.Command("go", "run", "./main.go", tmpC4.Name(), tmpHello.Name())
	out, err := cmd.CombinedOutput()
	if err != nil {
		t.Fatalf("go4 failed: %v\n%s", err, out)
	}
	if !strings.HasPrefix(string(out), "hello, world") {
		t.Fatalf("unexpected output: %q", out)
	}
}
