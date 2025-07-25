" Generated by Mochi compiler v0.10.30 on 2006-01-02T15:04:05Z
"
| primesUpTo sieve i p m res x sortInts tmp min idx out j commatize s primes getBrilliant brilliant count pow next k prod main r br limit r2 total climit ctotal cnext |
Object subclass: #BreakSignal instanceVariableNames: '' classVariableNames: '' poolDictionaries: '' category: nil!.
primesUpTo := [:n | sieve := {}.
i := 0.
[(i <= n)] whileTrue: [.
sieve := sieve copyWith: true.
i := (i + 1).
].
.
p := 2.
[(((p * p)) <= n)] whileTrue: [.
(sieve at: p) ifTrue: [
m := (p * p).
[(m <= n)] whileTrue: [.
sieve at: m put: false.
m := (m + p).
].
.
] .
p := (p + 1).
].
.
res := {}.
x := 2.
[(x <= n)] whileTrue: [.
(sieve at: x) ifTrue: [
res := res copyWith: x.
] .
x := (x + 1).
].
.
res. ].
sortInts := [:xs | res := {}.
tmp := xs.
[(((tmp size)) > 0)] whileTrue: [.
min := tmp at: 0.
idx := 0.
i := 1.
[(i < ((tmp size)))] whileTrue: [.
(((tmp at: i) < min)) ifTrue: [
min := tmp at: i.
idx := i.
] .
i := (i + 1).
].
.
res := res copyWith: min.
out := {}.
j := 0.
[(j < ((tmp size)))] whileTrue: [.
((j ~= idx)) ifTrue: [
out := out copyWith: tmp at: j.
] .
j := (j + 1).
].
.
tmp := out.
].
.
res. ].
commatize := [:n | s := (n asString).
i := (((s size)) - 3).
[(i >= 1)] whileTrue: [.
s := ((((s copyFrom: (0 + 1) to: i) + ',')) + (s copyFrom: (i + 1) to: (s size))).
i := (i - 3).
].
.
s. ].
primes := primesUpTo value: 3200000.
getBrilliant := [:digits :limit :countOnly | brilliant := {}.
count := 0.
pow := 1.
next := 999999999999999.
k := 1.
[.
[(k <= digits)] whileTrue: [.
s := {}.
[.
primes do: [:p |.
((p >= ((pow * 10)))) ifTrue: [
BreakSignal signal.
] .
((p > pow)) ifTrue: [
s := s copyWith: p.
] .
].
] on: BreakSignal do: [:ex | ].
i := 0.
[.
[(i < ((s size)))] whileTrue: [.
j := i.
[.
[(j < ((s size)))] whileTrue: [.
prod := ((s at: i) * (s at: j)).
((prod < limit)) ifTrue: [
(countOnly) ifTrue: [
count := (count + 1).
] ifFalse: [
brilliant := brilliant copyWith: prod.
].
] ifFalse: [
((prod < next)) ifTrue: [
next := prod.
] .
BreakSignal signal.
].
j := (j + 1).
].
] on: BreakSignal do: [:ex | ].
i := (i + 1).
].
] on: BreakSignal do: [:ex | ].
pow := (pow * 10).
k := (k + 1).
].
] on: BreakSignal do: [:ex | ].
(countOnly) ifTrue: [
Dictionary from:{'bc'->count. 'next'->next}.
] .
Dictionary from:{'bc'->brilliant. 'next'->next}. ].
main := [ Transcript show: 'First 100 brilliant numbers:'; cr.
r := getBrilliant value: 2 value: 10000 value: false.
br := sortInts value: r at: 'bc'.
br := br copyFrom: (0 + 1) to: 100.
i := 0.
[(i < ((br size)))] whileTrue: [.
Transcript show: (((((br at: i asString) at: "padStart") value: 4 value: (' ')) + (' '))) printString; show: ' '; show: (false) printString; cr.
((((((i + 1)) % 10)) = 0)) ifTrue: [
Transcript show: ''; show: ' '; show: (true) printString; cr.
] .
i := (i + 1).
].
.
Transcript show: ''; show: ' '; show: (true) printString; cr.
k := 1.
[(k <= 13)] whileTrue: [.
limit := pow value: 10 value: k.
r2 := getBrilliant value: k value: limit value: true.
total := r2 at: 'bc'.
next := r2 at: 'next'.
climit := commatize value: limit.
ctotal := commatize value: (total + 1).
cnext := commatize value: next.
Transcript show: ((((((((((('First >= ') + ((climit.padStart) value: 18 value: (' ')))) + (' is '))) + ((ctotal.padStart) value: 14 value: (' ')))) + (' in the series: '))) + ((cnext.padStart) value: 18 value: (' ')))) printString; cr.
k := (k + 1).
].
. ].
