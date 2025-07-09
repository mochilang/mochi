| m x y |
x := 3.
y := 4.
m := Dictionary newFrom: {'a' -> x. 'b' -> y}.
Transcript show: (m at: 'a') printString; show: ' '; show: (m at: 'b') printString; cr.
