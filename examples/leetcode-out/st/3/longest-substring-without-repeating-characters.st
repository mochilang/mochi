Object subclass: #Main instanceVariableNames: '' classVariableNames: '' poolDictionaries: '' category: nil!

!Main class methodsFor: 'mochi'!
lengthOfLongestSubstring: s | best i j length n start |
    n := (s) size.
    start := 0.
    best := 0.
    i := 0.
    [
        [(i < n)] whileTrue: [
            [
                j := start.
                [
                    [(j < i)] whileTrue: [
                        [
                            (((s at: j + 1) = (s at: i + 1))) ifTrue: [
                                start := (j + 1).
                                BreakSignal signal
                            ]
                            .
                            j := (j + 1).
                        ] on: ContinueSignal do: [:ex | ]
                    ]
                ] on: BreakSignal do: [:ex | ]
                .
                length := ((i - start) + 1).
                ((length > best)) ifTrue: [
                    best := length.
                ]
                .
                i := (i + 1).
            ] on: ContinueSignal do: [:ex | ]
        ]
    ] on: BreakSignal do: [:ex | ]
    .
    ^ best
!

!Main class methodsFor: 'tests'!
test_example_1
    (((Main lengthOfLongestSubstring: ('abcabcbb')) = 3)) ifFalse: [ self error: 'expect failed' ]
!

!Main class methodsFor: 'tests'!
test_example_2
    (((Main lengthOfLongestSubstring: ('bbbbb')) = 1)) ifFalse: [ self error: 'expect failed' ]
!

!Main class methodsFor: 'tests'!
test_example_3
    (((Main lengthOfLongestSubstring: ('pwwkew')) = 3)) ifFalse: [ self error: 'expect failed' ]
!

!Main class methodsFor: 'tests'!
test_empty_string
    (((Main lengthOfLongestSubstring: ('')) = 0)) ifFalse: [ self error: 'expect failed' ]
!

Object subclass: #BreakSignal instanceVariableNames: '' classVariableNames: '' poolDictionaries: '' category: nil!

!Main class methodsFor: 'runtime'!
!!
Main test_example_1.
Main test_example_2.
Main test_example_3.
Main test_empty_string.
