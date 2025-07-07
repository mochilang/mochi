| list sum |
list := #(1 2 3).
sum := 0.
list do: [:each | sum := sum + each].
Transcript show: (sum / list size) printString; cr.
