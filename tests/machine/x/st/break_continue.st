| numbers i n |
numbers := #(1 2 3 4 5 6 7 8 9).
i := 1.
[i <= numbers size] whileTrue: [
    n := numbers at: i.
    (n even)
        ifTrue: [ i := i + 1 ]
        ifFalse: [
            (n > 7)
                ifTrue: [ i := numbers size + 1 ]
                ifFalse: [
                    Transcript show: 'odd number: ', n printString; cr.
                    i := i + 1.
                ].
        ].
].
