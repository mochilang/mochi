# Mochi LeetCode Solutions

This directory contains LeetCode problems implemented in the [Mochi programming language](https://github.com/mochilang/mochi).

## Getting Started

You need the `mochi` CLI to run or test the solutions. Download the latest binary or build from source.

### Download prebuilt binary

```bash
# from this directory
make mochi
```

### Build from source

```bash
git clone https://github.com/mochilang/mochi
cd mochi
make install
make build
```

The binary will be installed to `~/bin/mochi`. Ensure that directory is on your `PATH`.

## Running a Solution

Run any problem by its folder number:

```bash
make run ID=1
```

or directly with the CLI:

```bash
mochi run 1/two-sum.mochi
```

## Running Tests

Some solutions include `test` blocks. Execute all tests with:

```bash
make test
```

You can also test a single file:

```bash
mochi test 30/substring-with-concatenation-of-all-words.mochi
```

## Downloading Problems

`download.mochi` shows how to fetch a problem statement from LeetCode. Run it with:

```bash
mochi run download.mochi
```

## Makefile Targets

- `make mochi` – download the latest Mochi binary into `./bin`
- `make run ID=<n>` – run problem `n`
- `make run-ex ID=<n>` – compile and run the Elixir solution for problem `n`
- `make compile` – generate Go, Python, TypeScript and C++ files into `../leetcode-out`
- `make test` – run all tests
- `make clean` – remove the downloaded binary

