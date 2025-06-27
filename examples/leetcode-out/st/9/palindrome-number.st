Object subclass: #Main instanceVariableNames: '' classVariableNames: '' poolDictionaries: '' category: nil!

!Main class methodsFor: 'mochi'!
isPalindrome: x | i n s |
    ((x < 0)) ifTrue: [
        ^ false
    ]
    .
    s := (x printString).
    n := (s) size.
    0 to: (n / 2) - 1 do: [:i |
        (((s at: i + 1) ~= (s at: ((n - 1) - i) + 1))) ifTrue: [
            ^ false
        ]
        .
    ]
    .
    ^ true
!

!Main class methodsFor: 'tests'!
test_example_1
    (((Main isPalindrome: (121)) = true)) ifFalse: [ self error: 'expect failed' ]
!

!Main class methodsFor: 'tests'!
test_example_2
    (((Main isPalindrome: ((121 negated))) = false)) ifFalse: [ self error: 'expect failed' ]
!

!Main class methodsFor: 'tests'!
test_example_3
    (((Main isPalindrome: (10)) = false)) ifFalse: [ self error: 'expect failed' ]
!

!Main class methodsFor: 'tests'!
test_zero
    (((Main isPalindrome: (0)) = true)) ifFalse: [ self error: 'expect failed' ]
!

!!
Main test_example_1.
Main test_example_2.
Main test_example_3.
Main test_zero.
