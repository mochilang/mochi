package sandbox

import (
	"fmt"
	"os"
	"path/filepath"
	"sort"
)

// Images maps language keys to Docker base images providing the compiler.
var Images = map[string]string{
	"go":      "golang:1.22",
	"py":      "python:3.11",
	"ts":      "node:lts",
	"c":       "gcc:latest",
	"cpp":     "gcc:latest",
	"cobol":   "gnu-cobol:latest",
	"cs":      "mcr.microsoft.com/dotnet/sdk:8.0",
	"dart":    "dart:stable",
	"erlang":  "erlang:latest",
	"ex":      "elixir:latest",
	"fortran": "gcc:latest",
	"fs":      "mcr.microsoft.com/dotnet/sdk:8.0",
	"hs":      "haskell:latest",
	"java":    "openjdk:latest",
	"jvm":     "openjdk:latest",
	"kt":      "openjdk:latest",
	"lua":     "lua:latest",
	"mlir":    "llvm:latest",
	"ocaml":   "ocaml/opam:debian-12",
	"pas":     "debian:bookworm",
	"php":     "php:latest",
	"pl":      "perl:latest",
	"rb":      "ruby:latest",
	"rkt":     "racket/racket:latest",
	"rust":    "rust:latest",
	"scala":   "hseeberger/scala-sbt:latest",
	"scheme":  "mitchellh/scheme:latest",
	"st":      "squeak/squeak:latest",
	"swift":   "swift:latest",
	"wasm":    "wasmerio/wasmer:latest",
	"zig":     "ziglang/zig:latest",
}

// ListLanguages returns all compiler directories under compile and compile/x.
func ListLanguages() ([]string, error) {
	var langs []string
	entries, err := os.ReadDir("compile")
	if err != nil {
		return nil, err
	}
	for _, e := range entries {
		if !e.IsDir() {
			continue
		}
		name := e.Name()
		if name == "x" {
			continue
		}
		langs = append(langs, name)
	}
	entries, err = os.ReadDir(filepath.Join("compile", "x"))
	if err == nil {
		for _, e := range entries {
			if !e.IsDir() {
				continue
			}
			name := e.Name()
			if name == "testutil" {
				continue
			}
			langs = append(langs, name)
		}
	}
	sort.Strings(langs)
	return langs, nil
}

// DockerfileFor returns a basic Dockerfile for the given language.
func DockerfileFor(lang string) string {
	img, ok := Images[lang]
	if !ok {
		img = "debian:bookworm"
	}
	return fmt.Sprintf(`FROM golang:1.22 AS builder
WORKDIR /src
COPY . .
RUN go build -o /runner ./tools/sandbox/runner/%[1]s

FROM %[2]s
WORKDIR /workspace
COPY --from=builder /runner /usr/local/bin/runner
CMD ["/usr/local/bin/runner"]
`, lang, img)
}

// RunnerMainFor returns Go source implementing the language runner.
func RunnerMainFor(lang string) string {
	return fmt.Sprintf(`package main

import (
        "mochi/tools/sandbox"
        "mochi/tools/sandbox/runner"
)

func main() {
        runner.Run(sandbox.RunCmds[%q])
}
`, lang)
}
