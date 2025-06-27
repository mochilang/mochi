Object subclass: #Main instanceVariableNames: '' classVariableNames: '' poolDictionaries: '' category: nil!

!Main class methodsFor: 'mochi'!
addTwoNumbers: l1 l2: l2 | carry digit i j result sum x y |
    i := 0.
    j := 0.
    carry := 0.
    result := Array new.
    [(((((i < (l1) size) or: [j]) < (l2) size) or: [carry]) > 0)] whileTrue: [
        x := 0.
        ((i < (l1) size)) ifTrue: [
            x := (l1 at: i + 1).
            i := (i + 1).
        ]
        .
        y := 0.
        ((j < (l2) size)) ifTrue: [
            y := (l2 at: j + 1).
            j := (j + 1).
        ]
        .
        sum := ((x + y) + carry).
        digit := (sum \\ 10).
        carry := (sum / 10).
        result := ((result) , (Array with: digit)).
    ]
    .
    ^ result
!

!Main class methodsFor: 'tests'!
test_example_1
    (((Main addTwoNumbers: (Array with: 2 with: 4 with: 3) l2: (Array with: 5 with: 6 with: 4)) = Array with: 7 with: 0 with: 8)) ifFalse: [ self error: 'expect failed' ]
!

!Main class methodsFor: 'tests'!
test_example_2
    (((Main addTwoNumbers: (Array with: 0) l2: (Array with: 0)) = Array with: 0)) ifFalse: [ self error: 'expect failed' ]
!

!Main class methodsFor: 'tests'!
test_example_3
    (((Main addTwoNumbers: (Array with: 9 with: 9 with: 9 with: 9 with: 9 with: 9 with: 9) l2: (Array with: 9 with: 9 with: 9 with: 9)) = Array with: 8 with: 9 with: 9 with: 9 with: 0 with: 0 with: 0 with: 1)) ifFalse: [ self error: 'expect failed' ]
!

!!
Main test_example_1.
Main test_example_2.
Main test_example_3.
