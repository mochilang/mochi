| arr sum |
arr := #(1 2 3).
sum := 0.
arr do: [:n | sum := sum + n].
Transcript show: sum printString; cr.
