" Generated by Mochi compiler v0.10.30 on 2006-01-02T15:04:05Z
"
| lower up low i partList nAssemblies p cycle a |
lower := [:ch | up := 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'.
low := 'abcdefghijklmnopqrstuvwxyz'.
i := 0.
[(i < 26)] whileTrue: [.
((ch = ((up copyFrom: i to: (i + 1))))) ifTrue: [
(low copyFrom: i to: (i + 1)).
] .
i := (i + 1).
].
.
ch. ].
partList := {'A'. 'B'. 'C'. 'D'}.
nAssemblies := 3.
partList do: [:p |.
Transcript show: ((p + (' worker running'))) printString; cr.
].
.
1 to: (nAssemblies + 1) do: [:cycle |.
Transcript show: ((('begin assembly cycle ') + ((cycle asString)))) printString; cr.
a := ''.
partList do: [:p |.
Transcript show: ((p + (' worker begins part'))) printString; cr.
Transcript show: ((((p + (' worker completed '))) + (lower value: p))) printString; cr.
a := (a + (lower value: p)).
].
.
Transcript show: ((((((a + (' assembled.  cycle '))) + ((cycle asString)))) + (' complete'))) printString; cr.
].
.
partList do: [:p |.
Transcript show: ((p + (' worker stopped'))) printString; cr.
].
.
