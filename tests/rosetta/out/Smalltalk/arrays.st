" Generated by Mochi compiler v0.10.30 on 2006-01-02T15:04:05Z
"
| listStr s i a cap_s |
listStr := [:xs | s := '['.
i := 0.
[(i < ((xs size)))] whileTrue: [.
s := (s + ((xs at: i asString))).
((((i + 1)) < ((xs size)))) ifTrue: [
s := (s + (' ')).
] .
i := (i + 1).
].
.
s := (s + ']').
s. ].
a := {0. 0. 0. 0. 0}.
Transcript show: ((('len(a) = ') + ((5 asString)))) printString; cr.
Transcript show: ((('a = ') + (listStr value: a))) printString; cr.
a at: 0 put: 3.
Transcript show: ((('a = ') + (listStr value: a))) printString; cr.
Transcript show: ((('a[0] = ') + ((a at: 0 asString)))) printString; cr.
s := a copyFrom: (0 + 1) to: 4.
cap_s := 5.
Transcript show: ((('s = ') + (listStr value: s))) printString; cr.
Transcript show: ((((((('len(s) = ') + (((s size) asString)))) + ('  cap(s) = '))) + ((cap_s asString)))) printString; cr.
s := a copyFrom: (0 + 1) to: 5.
Transcript show: ((('s = ') + (listStr value: s))) printString; cr.
a at: 0 put: 22.
s at: 0 put: 22.
Transcript show: ((('a = ') + (listStr value: a))) printString; cr.
Transcript show: ((('s = ') + (listStr value: s))) printString; cr.
s := s copyWith: 4.
s := s copyWith: 5.
s := s copyWith: 6.
cap_s := 10.
Transcript show: ((('s = ') + (listStr value: s))) printString; cr.
Transcript show: ((((((('len(s) = ') + (((s size) asString)))) + ('  cap(s) = '))) + ((cap_s asString)))) printString; cr.
a at: 4 put: -1.
Transcript show: ((('a = ') + (listStr value: a))) printString; cr.
Transcript show: ((('s = ') + (listStr value: s))) printString; cr.
s := {}.
0 to: 8 do: [:i |.
s := s copyWith: 0.
].
.
cap_s := 8.
Transcript show: ((('s = ') + (listStr value: s))) printString; cr.
Transcript show: ((((((('len(s) = ') + (((s size) asString)))) + ('  cap(s) = '))) + ((cap_s asString)))) printString; cr.
