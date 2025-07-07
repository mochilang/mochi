| m |
m := Dictionary newFrom: {'a'->1. 'b'->2}.
Transcript show: (m includesKey: 'a') printString; cr.
Transcript show: (m includesKey: 'c') printString; cr.
