| sum3 |
sum3 := [:a :b :c | ^((a + b) + c). ].
Transcript show: (sum3 value: 1 value: 2 value: 3) printString; cr.
