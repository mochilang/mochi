| m |
m := Dictionary newFrom: {1->'a'. 2->'b'}.
Transcript show: (m includesKey: 1) printString; cr.
Transcript show: (m includesKey: 3) printString; cr.
