| nums result |
nums := #(1 2 3).
result := (nums select: [:n | n > 1]) inject: 0 into: [:s :n | s + n].
Transcript show: result printString; cr.
