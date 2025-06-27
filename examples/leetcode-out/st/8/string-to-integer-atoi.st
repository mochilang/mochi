Object subclass: #Main instanceVariableNames: '' classVariableNames: '' poolDictionaries: '' category: nil!

!Main class methodsFor: 'mochi'!
digit: ch
    ((ch = '0')) ifTrue: [
        ^ 0
    ]
    .
    ((ch = '1')) ifTrue: [
        ^ 1
    ]
    .
    ((ch = '2')) ifTrue: [
        ^ 2
    ]
    .
    ((ch = '3')) ifTrue: [
        ^ 3
    ]
    .
    ((ch = '4')) ifTrue: [
        ^ 4
    ]
    .
    ((ch = '5')) ifTrue: [
        ^ 5
    ]
    .
    ((ch = '6')) ifTrue: [
        ^ 6
    ]
    .
    ((ch = '7')) ifTrue: [
        ^ 7
    ]
    .
    ((ch = '8')) ifTrue: [
        ^ 8
    ]
    .
    ((ch = '9')) ifTrue: [
        ^ 9
    ]
    .
    ^ (1 negated)
!

!Main class methodsFor: 'mochi'!
myAtoi: s | ch d i n result sign |
    i := 0.
    n := (s) size.
    [(((i < n) and: [(s at: i + 1)]) = (Main __index_string: ' ' idx: 0))] whileTrue: [
        i := (i + 1).
    ]
    .
    sign := 1.
    (((i < n) and: [(((((s at: i + 1) = (Main __index_string: '+' idx: 0)) or: [(s at: i + 1)]) = (Main __index_string: '-' idx: 0)))])) ifTrue: [
        (((s at: i + 1) = (Main __index_string: '-' idx: 0))) ifTrue: [
            sign := (1 negated).
        ]
        .
        i := (i + 1).
    ]
    .
    result := 0.
    [
        [(i < n)] whileTrue: [
            [
                ch := (s copyFrom: (i + 1) to: (i + 1)).
                d := (Main digit: (ch)).
                ((d < 0)) ifTrue: [
                    BreakSignal signal
                ]
                .
                result := ((result * 10) + d).
                i := (i + 1).
            ] on: ContinueSignal do: [:ex | ]
        ]
    ] on: BreakSignal do: [:ex | ]
    .
    result := (result * sign).
    ((result > 2147483647)) ifTrue: [
        ^ 2147483647
    ]
    .
    ((result < ((2147483648 negated)))) ifTrue: [
        ^ (2147483648 negated)
    ]
    .
    ^ result
!

!Main class methodsFor: 'tests'!
test_example_1
    (((Main myAtoi: ('42')) = 42)) ifFalse: [ self error: 'expect failed' ]
!

!Main class methodsFor: 'tests'!
test_example_2
    (((Main myAtoi: ('   -42')) = ((42 negated)))) ifFalse: [ self error: 'expect failed' ]
!

!Main class methodsFor: 'tests'!
test_example_3
    (((Main myAtoi: ('4193 with words')) = 4193)) ifFalse: [ self error: 'expect failed' ]
!

!Main class methodsFor: 'tests'!
test_example_4
    (((Main myAtoi: ('words and 987')) = 0)) ifFalse: [ self error: 'expect failed' ]
!

!Main class methodsFor: 'tests'!
test_example_5
    (((Main myAtoi: ('-91283472332')) = ((2147483648 negated)))) ifFalse: [ self error: 'expect failed' ]
!

Object subclass: #BreakSignal instanceVariableNames: '' classVariableNames: '' poolDictionaries: '' category: nil!

!Main class methodsFor: 'runtime'!
__index_string: s idx: i
    | idx n |
    idx := i.
    n := s size.
    idx < 0 ifTrue: [ idx := idx + n ].
    (idx < 0 or: [ idx >= n ]) ifTrue: [ self error: 'index out of range' ].
    ^ (s at: idx + 1) asString
!
!!
Main test_example_1.
Main test_example_2.
Main test_example_3.
Main test_example_4.
Main test_example_5.
