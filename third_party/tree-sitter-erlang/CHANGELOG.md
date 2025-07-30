0.14.0 (2025-04-08)
* OTP 28 features support
  - based floating point literals [EEP-75](https://www.erlang.org/eeps/eep-0075)
  - nominal types [EEP-69](https://github.com/erlang/eep/blob/master/eeps/eep-0069.md)
  - zip generators [EEP-73](https://github.com/erlang/eep/blob/master/eeps/eep-0073.md)
0.13.0 (2025-03-11)
* Bump tree-sitter version to 0.22.4
* Align operator precedence parsing with what the Erlang compiler does
0.12.0 (2025-02-07)
* Parse escript shebangs as a top-level form
* Allow a bare expression as a top-level form. This means the grammar
  can now handle rebar.config and similar files. By @williamthome

0.11.0 (2025-02-04)
* Support EEP70 strict generators, coming in OTP28

0.10.0 (2025-01-21)
* Add a new top-level form to parse Structural Search and Replace templates.

0.9.0 (2024-10-21)
* Bump tree-sitter-cli to 0.23.2, and bring in tree-sitter-language

0.8.0 (Oct 4, 2024)
* Fix corner-case for parsing an expression with the pipe operator

0.7.0 (Jul 12, 2024)
* Add support for OTP 27

0.6.0 (2024-05-15)
* Bump tree-sitter-cli to 0.22.5

0.5.0 (2024-03-18)
* Bump tree-sitter-cli to 0.22.1

0.4.0 (2023-12-15)
* Enable parsing an expression with guards in a macro replacement

0.3.0 (2023-11-27)
* Make trailing separators optional for fun_decl.  This improves error recovery.

0.2.0 (Oct 05, 2023)
* Add support for OTP 26 feature attributes

0.1.0 (June 16, 2023)
* Add support for OTP 26 constructs
  * maybe
  * map comprehension

0.0.1 (September 26, 2022)

* Initial Release
