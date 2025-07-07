| data flag |
data := {1. 2}.
flag := data anySatisfy: [:x | x = 1].
Transcript show: flag printString; cr.
