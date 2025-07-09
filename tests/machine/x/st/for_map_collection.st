| m |
m := Dictionary newFrom: {'a' -> 1. 'b' -> 2}.
m do: [:k |
  Transcript show: (k) printString; cr.
]
.
