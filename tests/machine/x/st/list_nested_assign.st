" Generated by Mochi compiler v0.10.26 on 2025-07-16T17:40:19Z
"
| matrix |
matrix := {{1. 2}. {3. 4}}.
matrix at: 1 at: 0 put: 5.
Transcript show: (matrix at: 1 at: 0) printString; cr.
