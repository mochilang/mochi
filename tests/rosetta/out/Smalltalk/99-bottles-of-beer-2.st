" Generated by Mochi compiler v0.10.30 on 2006-01-02T15:04:05Z
"
| fields words cur i ch join res numberName small tens t s pluralizeFirst w randInt next slur a idx seed j tmp k main |
fields := [:s | words := {}.
cur := ''.
i := 0.
[(i < ((s size)))] whileTrue: [.
ch := (s copyFrom: i to: (i + 1)).
((((((ch = (' '))) or: [((ch = '
'))])) or: [((ch = '	'))])) ifTrue: [
((0 > 0)) ifTrue: [
words := words copyWith: cur.
cur := ''.
] .
] ifFalse: [
cur := (cur + ch).
].
i := (i + 1).
].
.
((((cur size)) > 0)) ifTrue: [
words := words copyWith: cur.
] .
words. ].
join := [:xs :sep | res := ''.
i := 0.
[(i < ((xs size)))] whileTrue: [.
((i > 0)) ifTrue: [
res := (res + sep).
] .
res := (res + (xs at: i)).
i := (i + 1).
].
.
res. ].
numberName := [:n | small := {'no'. 'one'. 'two'. 'three'. 'four'. 'five'. 'six'. 'seven'. 'eight'. 'nine'. 'ten'. 'eleven'. 'twelve'. 'thirteen'. 'fourteen'. 'fifteen'. 'sixteen'. 'seventeen'. 'eighteen'. 'nineteen'}.
tens := {'ones'. 'ten'. 'twenty'. 'thirty'. 'forty'. 'fifty'. 'sixty'. 'seventy'. 'eighty'. 'ninety'}.
((n < 0)) ifTrue: [
''.
] .
((n < 20)) ifTrue: [
small at: n.
] .
((n < 100)) ifTrue: [
t := tens at: (n / 10) asInteger.
s := (n % 10).
((s > 0)) ifTrue: [
t := (((t + (' '))) + (small at: s)).
] .
t.
] .
''. ].
pluralizeFirst := [:s :n | ((n = 1)) ifTrue: [
s.
] .
w := fields value: s.
((((w size)) > 0)) ifTrue: [
w at: 0 put: ((w at: 0) + 's').
] .
join value: w value: ' '. ].
randInt := [:seed :n | next := (((((seed * 1664525)) + 1013904223)) % 2147483647).
(next % n). ].
slur := [:p :d | ((((p size)) <= 2)) ifTrue: [
p.
] .
a := {}.
i := 1.
[(i < ((((p size)) - 1)))] whileTrue: [.
a := a copyWith: (p copyFrom: i to: (i + 1)).
i := (i + 1).
].
.
idx := (((a size)) - 1).
seed := d.
[(idx >= 1)] whileTrue: [.
seed := (((((seed * 1664525)) + 1013904223)) % 2147483647).
((((seed % 100)) >= d)) ifTrue: [
j := (seed % ((idx + 1))).
tmp := a at: idx.
a at: idx put: a at: j.
a at: j put: tmp.
] .
idx := (idx - 1).
].
.
s := (p copyFrom: 0 to: 1).
k := 0.
[(k < ((a size)))] whileTrue: [.
s := (s + (a at: k)).
k := (k + 1).
].
.
s := (s + ((p copyFrom: (((p size)) - 1) to: (p size)))).
w := fields value: s.
join value: w value: ' '. ].
main := [ i := 99.
[(i > 0)] whileTrue: [.
Transcript show: (((((((((slur value: numberName value: i value: i) + (' '))) + (pluralizeFirst value: slur value: 'bottle of' value: i value: i))) + (' '))) + (slur value: 'beer on the wall' value: i))) printString; cr.
Transcript show: (((((((((slur value: numberName value: i value: i) + (' '))) + (pluralizeFirst value: slur value: 'bottle of' value: i value: i))) + (' '))) + (slur value: 'beer' value: i))) printString; cr.
Transcript show: (((((((((slur value: 'take one' value: i) + (' '))) + (slur value: 'down' value: i))) + (' '))) + (slur value: 'pass it around' value: i))) printString; cr.
Transcript show: (((((((((slur value: numberName value: (i - 1) value: i) + (' '))) + (pluralizeFirst value: slur value: 'bottle of' value: i value: (i - 1)))) + (' '))) + (slur value: 'beer on the wall' value: i))) printString; cr.
i := (i - 1).
].
. ].
main value.
