| label x |
x := 2.
label := (x = 1) ifTrue: ['one'] ifFalse: [(x = 2) ifTrue: ['two'] ifFalse: [(x = 3) ifTrue: ['three'] ifFalse: ['unknown']]].
Transcript show: (label) printString; cr.
