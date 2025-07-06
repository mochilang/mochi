# C Converter

This subpackage contains the C to Mochi converter used by `any2mochi`.
It relies on `clang` to extract function definitions when a language
server is not available and provides helpers for parsing hover
information. The exported `Convert` and `ConvertFile` functions return
Mochi source code for the supplied C input.
