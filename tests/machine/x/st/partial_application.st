| add add5 |
add := [:a :b | a + b].
add5 := [:x | add value: 5 value: x].
Transcript show: (add5 value: 3) printString; cr.
