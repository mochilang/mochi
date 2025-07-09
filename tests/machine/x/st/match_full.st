| label day mood ok status classify x |
x := 2.
label := (x = 1) ifTrue: ['one'] ifFalse: [(x = 2) ifTrue: ['two'] ifFalse: [(x = 3) ifTrue: ['three'] ifFalse: ['unknown']]].
Transcript show: (label) printString; cr.
day := 'sun'.
mood := (day = 'mon') ifTrue: ['tired'] ifFalse: [(day = 'fri') ifTrue: ['excited'] ifFalse: [(day = 'sun') ifTrue: ['relaxed'] ifFalse: ['normal']]].
Transcript show: (mood) printString; cr.
ok := true.
status := (ok = true) ifTrue: ['confirmed'] ifFalse: [(ok = false) ifTrue: ['denied'] ifFalse: [nil]].
Transcript show: (status) printString; cr.
classify := [:n | ^(n = 0) ifTrue: ['zero'] ifFalse: [(n = 1) ifTrue: ['one'] ifFalse: ['many']]. ].
Transcript show: (classify value: 0) printString; cr.
Transcript show: (classify value: 5) printString; cr.
