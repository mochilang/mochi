| x msg |
x := 8.
msg := (x > 10)
          ifTrue: ['big']
          ifFalse: [(x > 5) ifTrue: ['medium'] ifFalse: ['small']].
Transcript show: msg; cr.
