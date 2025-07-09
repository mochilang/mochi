| add5 add |
add := [:a :b | ^(a + b). ].
add5 := add value: 5.
Transcript show: (add5 value: 3) printString; cr.
