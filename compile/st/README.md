# Smalltalk Backend

This backend converts Mochi programs to GNU Smalltalk. The tests under `compile/st` use the `gst` interpreter.

## Installation

The helper `EnsureSmalltalk` installs GNU Smalltalk when needed. On Linux it first tries `apt-get install gnu-smalltalk`. If the package is missing the installer temporarily enables the Ubuntu 22.04 "jammy" repository to fetch it. Should apt still fail, the code downloads version 3.2.5 from `ftpmirror.gnu.org` and builds it. Set the `SMALLTALK_TARBALL` environment variable to override the source URL. On macOS Homebrew is used.

## Usage

Compile a program with `mochi build --target st` and run it using `gst`. To run the slow Smalltalk tests:

```bash
go test ./compile/st -tags slow
```

## Notes

The Smalltalk backend currently supports only a subset of Mochi. Features such
as `break` and `continue` in loops, agents, dataset queries and generative AI
blocks are not implemented yet. When the `count()` builtin is used, a helper
method is emitted automatically.
