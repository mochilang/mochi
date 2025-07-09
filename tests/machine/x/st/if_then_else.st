| x msg |
x := 12.
msg := ((x > 10)) ifTrue: ['yes'] ifFalse: ['no'].
Transcript show: (msg) printString; cr.
