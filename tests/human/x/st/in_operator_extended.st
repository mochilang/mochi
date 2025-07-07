| xs ys m s |
xs := {1. 2. 3}.
ys := xs select: [:x | x odd].
Transcript show: (ys includes: 1) printString; cr.
Transcript show: (ys includes: 2) printString; cr.

m := Dictionary newFrom: {'a'->1}.
Transcript show: (m includesKey: 'a') printString; cr.
Transcript show: (m includesKey: 'b') printString; cr.

s := 'hello'.
Transcript show: (s includesSubstring: 'ell') printString; cr.
Transcript show: (s includesSubstring: 'foo') printString; cr.
