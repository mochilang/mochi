| result |
twoSum := [:nums :target | n := (nums size).
0 to: n do: [:i |.
(i + 1) to: n do: [:j |.
(((nums at: i + nums at: j) = target)) ifTrue: [
^{i. j}.
].
].
.
].
.
^{-1. -1}. ].
result := twoSum value: {2. 7. 11. 15} value: 9.
Transcript show: (result at: 0) printString; cr.
Transcript show: (result at: 1) printString; cr.
