# a2mochi Ruby Converter

This directory contains a minimal converter that translates simple Ruby
programs back into Mochi form. It uses Ruby's built in `ripper` library to
obtain an s-expression AST which is then converted to Mochi code. The
implementation mirrors the Python and TypeScript converters and is only
powerful enough for the examples under `tests/transpiler/x/rb`.
