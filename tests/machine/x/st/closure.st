| add10 |
makeAdder := [:n | ^[:x | (x + n) ]. ].
add10 := makeAdder value: 10.
Transcript show: (add10 value: 7) printString; cr.
