| data |
data := Dictionary newFrom:{'outer'->Dictionary newFrom:{'inner'->1}}.
data at: 'outer' at: 'inner' put: 2.
Transcript show: (data at: 'outer' at: 'inner') printString; cr.
