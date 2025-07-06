# Python Converter

This package converts a small subset of Python source code into Mochi. The
implementation is now entirely in Go. When a Python language server is
available it will be used for richer information, otherwise a built in parser
driven via the standard `ast` module is executed.

