| prefix s1 s2 |
prefix := 'fore'.
s1 := 'forest'.
Transcript show: ((s1 copyFrom: 1 to: prefix size) = prefix) printString; cr.
s2 := 'desert'.
Transcript show: ((s2 copyFrom: 1 to: prefix size) = prefix) printString; cr.
