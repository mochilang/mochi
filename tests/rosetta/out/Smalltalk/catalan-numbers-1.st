" Generated by Mochi compiler v0.10.30 on 2006-01-02T15:04:05Z
"
| binom kk res i catalan main |
binom := [:n :k | ((((k < 0)) or: [((k > n))])) ifTrue: [
0.
] .
kk := k.
((kk > ((n - kk)))) ifTrue: [
kk := (n - kk).
] .
res := 1.
i := 0.
[(i < kk)] whileTrue: [.
res := (res * ((n - i))).
i := (i + 1).
res := (res / i) asInteger.
].
.
res. ].
catalan := [:n | ((binom value: (2 * n) value: n) / ((n + 1))) asInteger. ].
main := [ 0 to: 15 do: [:i |.
Transcript show: ((catalan value: i asString)) printString; cr.
].
. ].
main value.
