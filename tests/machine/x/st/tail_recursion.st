| sum_rec |
sum_rec := [:n :acc | ((n = 0)) ifTrue: [
^acc.
].
^sum_rec value: (n - 1) value: (acc + n). ].
Transcript show: (sum_rec value: 10 value: 0) printString; cr.
