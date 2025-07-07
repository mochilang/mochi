| sumRec |
sumRec := [:n :acc |
  n = 0 ifTrue: [^acc].
  ^sumRec value: n - 1 value: acc + n].
Transcript show: (sumRec value: 10 value: 0) printString; cr.
