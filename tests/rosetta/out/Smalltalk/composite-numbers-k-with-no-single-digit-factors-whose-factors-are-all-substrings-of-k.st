" Generated by Mochi compiler v0.10.30 on 2006-01-02T15:04:05Z
"
| primeFactors factors x p commatize s out i c indexOf pad10 str trimRightStr end main res count k includesAll prev f fs line e |
Object subclass: #BreakSignal instanceVariableNames: '' classVariableNames: '' poolDictionaries: '' category: nil!.
Object subclass: #ContinueSignal instanceVariableNames: '' classVariableNames: '' poolDictionaries: '' category: nil!.
primeFactors := [:n | factors := {}.
x := n.
[(((x % 2)) = 0)] whileTrue: [.
factors := factors copyWith: 2.
x := (x / 2) asInteger.
].
.
p := 3.
[(((p * p)) <= x)] whileTrue: [.
[(((x % p)) = 0)] whileTrue: [.
factors := factors copyWith: p.
x := (x / p) asInteger.
].
.
p := (p + 2).
].
.
((x > 1)) ifTrue: [
factors := factors copyWith: x.
] .
factors. ].
commatize := [:n | s := (n asString).
out := ''.
i := (((s size)) - 1).
c := 0.
[(i >= 0)] whileTrue: [.
out := (((s copyFrom: i to: (i + 1))) + out).
c := (c + 1).
((((((c % 3)) = 0)) and: [((i > 0))])) ifTrue: [
out := (',' + out).
] .
i := (i - 1).
].
.
out. ].
indexOf := [:s :sub | i := 0.
[(((i + ((sub size)))) <= ((s size)))] whileTrue: [.
((((s copyFrom: i to: (i + ((sub size))))) = sub)) ifTrue: [
i.
] .
i := (i + 1).
].
.
-1. ].
pad10 := [:s | str := s.
[(((str size)) < 10)] whileTrue: [.
str := ((' ') + str).
].
.
str. ].
trimRightStr := [:s | end := (s size).
[(((end > 0)) and: [((((s copyFrom: (end - 1) to: end)) = (' ')))])] whileTrue: [.
end := (end - 1).
].
.
(s copyFrom: 0 to: end). ].
main := [ res := {}.
count := 0.
k := (11 * 11).
[.
[(count < 20)] whileTrue: [.
[.
((((((((k % 3)) = 0)) or: [((((k % 5)) = 0))])) or: [((((k % 7)) = 0))])) ifTrue: [
k := (k + 2).
ContinueSignal signal.
] .
factors := primeFactors value: k.
((((factors size)) > 1)) ifTrue: [
s := (k asString).
includesAll := true.
prev := -1.
[.
factors do: [:f |.
[.
((f = prev)) ifTrue: [
ContinueSignal signal.
] .
fs := (f asString).
(((indexOf value: s value: fs) = -1)) ifTrue: [
includesAll := false.
BreakSignal signal.
] .
prev := f.
] on: ContinueSignal do: [:ex | ].
].
] on: BreakSignal do: [:ex | ].
(includesAll) ifTrue: [
res := res copyWith: k.
count := (count + 1).
] .
] .
k := (k + 2).
] on: ContinueSignal do: [:ex | ].
].
] on: BreakSignal do: [:ex | ].
line := ''.
res copyFrom: (0 + 1) to: 10 do: [:e |.
line := (((line + (pad10 value: commatize value: e))) + (' ')).
].
.
Transcript show: (trimRightStr value: line) printString; cr.
line := ''.
res copyFrom: (10 + 1) to: 20 do: [:e |.
line := (((line + (pad10 value: commatize value: e))) + (' ')).
].
.
Transcript show: (trimRightStr value: line) printString; cr. ].
main value.
