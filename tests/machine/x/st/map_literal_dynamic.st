| x y m |
x := 3.
y := 4.
m := Dictionary newFrom: {'a'->x. 'b'->y}.
Transcript show: (m at: 'a') printString; space; show: (m at: 'b') printString; cr.
