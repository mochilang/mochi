" Generated by Mochi compiler v0.10.30 on 2006-01-02T15:04:05Z
"
| split parts cur i rstripEmpty n spaces out pad diff left right newFormatter lines fmtLines width words j wlen printFmt line text f |
Object subclass: #ContinueSignal instanceVariableNames: '' classVariableNames: '' poolDictionaries: '' category: nil!.
split := [:s :sep | parts := {}.
cur := ''.
i := 0.
[(i < ((s size)))] whileTrue: [.
((((((((sep size)) > 0)) and: [((((i + ((sep size)))) <= ((s size))))])) and: [((((s copyFrom: i to: (i + ((sep size))))) = sep))])) ifTrue: [
parts := parts copyWith: cur.
cur := ''.
i := (i + ((sep size))).
] ifFalse: [
cur := (cur + ((s copyFrom: i to: (i + 1)))).
i := (i + 1).
].
].
.
parts := parts copyWith: cur.
parts. ].
rstripEmpty := [:words | n := (words size).
[(((n > 0)) and: [(((words at: (n - 1)) = ''))])] whileTrue: [.
n := (n - 1).
].
.
words copyFrom: 1 to: n. ].
spaces := [:n | out := ''.
i := 0.
[(i < n)] whileTrue: [.
out := (out + (' ')).
i := (i + 1).
].
.
out. ].
pad := [:word :width :align | diff := (width - ((word size))).
((align = 0)) ifTrue: [
(word + (spaces value: diff)).
] .
((align = 2)) ifTrue: [
((spaces value: diff) + word).
] .
left := (diff / 2) asInteger.
right := (diff - left).
((((spaces value: left) + word)) + (spaces value: right)). ].
newFormatter := [:text | lines := split value: text value: '
'.
fmtLines := {}.
width := {}.
i := 0.
[.
[(i < ((lines size)))] whileTrue: [.
[.
((((lines at: i size)) = 0)) ifTrue: [
i := (i + 1).
ContinueSignal signal.
] .
words := rstripEmpty value: split value: lines at: i value: '$'.
fmtLines := fmtLines copyWith: words.
j := 0.
[(j < ((words size)))] whileTrue: [.
wlen := (words at: j size).
((j = 0)) ifTrue: [
width := width copyWith: wlen.
] ifFalse: [
((wlen > (width at: j))) ifTrue: [
width at: j put: wlen.
] .
].
j := (j + 1).
].
.
i := (i + 1).
] on: ContinueSignal do: [:ex | ].
].
] on: BreakSignal do: [:ex | ].
Dictionary from:{'text'->fmtLines. 'width'->width}. ].
printFmt := [:f :align | lines := f at: 'text'.
width := f at: 'width'.
i := 0.
[(i < ((lines size)))] whileTrue: [.
words := lines at: i.
line := ''.
j := 0.
[(j < ((words size)))] whileTrue: [.
line := (((line + (pad value: words at: j value: width at: j value: align))) + (' ')).
j := (j + 1).
].
.
Transcript show: (line) printString; cr.
i := (i + 1).
].
.
Transcript show: ''; cr. ].
text := ((((((((('Given$a$text$file$of$many$lines,$where$fields$within$a$line
' + 'are$delineated$by$a$single$''dollar''$character,$write$a$program
')) + 'that$aligns$each$column$of$fields$by$ensuring$that$words$in$each
')) + 'column$are$separated$by$at$least$one$space.
')) + 'Further,$allow$for$each$word$in$a$column$to$be$either$left
')) + 'justified,$right$justified,$or$center$justified$within$its$column.').
f := newFormatter value: text.
printFmt value: f value: 0.
printFmt value: f value: 1.
printFmt value: f value: 2.
