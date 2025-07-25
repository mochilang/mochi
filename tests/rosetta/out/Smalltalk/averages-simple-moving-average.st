" Generated by Mochi compiler v0.10.30 on 2006-01-02T15:04:05Z
"
| indexOf i fmt3 y s dot decs pad out smaSeries res sum denom main xs sma3 sma5 line |
indexOf := [:s :ch | i := 0.
[(i < ((s size)))] whileTrue: [.
((((s copyFrom: i to: (i + 1))) = ch)) ifTrue: [
i.
] .
i := (i + 1).
].
.
-1. ].
fmt3 := [:x | y := (((((x * 1000)) + 0.5) asInteger asFloat) / 1000).
s := (y asString).
dot := indexOf value: s value: '.'.
((dot = ((0 - 1)))) ifTrue: [
s := (s + '.000').
] ifFalse: [
decs := (((((s size)) - dot)) - 1).
((decs > 3)) ifTrue: [
s := (s copyFrom: 0 to: (dot + 4)).
] ifFalse: [
[(decs < 3)] whileTrue: [.
s := (s + '0').
decs := (decs + 1).
].
.
].
].
s. ].
pad := [:s :width | out := s.
[(((out size)) < width)] whileTrue: [.
out := ((' ') + out).
].
.
out. ].
smaSeries := [:xs :period | res := {}.
sum := 0.
i := 0.
[(i < ((xs size)))] whileTrue: [.
sum := (sum + (xs at: i)).
((i >= period)) ifTrue: [
sum := (sum - (xs at: (i - period))).
] .
denom := (i + 1).
((denom > period)) ifTrue: [
denom := period.
] .
res := res copyWith: (sum / (denom asFloat)).
i := (i + 1).
].
.
res. ].
main := [ xs := {1. 2. 3. 4. 5. 5. 4. 3. 2. 1}.
sma3 := smaSeries value: xs value: 3.
sma5 := smaSeries value: xs value: 5.
Transcript show: 'x       sma3   sma5'; cr.
i := 0.
[(i < 10)] whileTrue: [.
line := ((((((((pad value: fmt3 value: xs at: i value: 5) + ('  '))) + (pad value: fmt3 value: sma3 at: i value: 5))) + ('  '))) + (pad value: fmt3 value: sma5 at: i value: 5)).
Transcript show: (line) printString; cr.
i := (i + 1).
].
. ].
main value.
