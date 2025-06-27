Object subclass: #Main instanceVariableNames: '' classVariableNames: '' poolDictionaries: '' category: nil!

!Main class methodsFor: 'mochi'!
expand: s left: left right: right | l n r |
    l := left.
    r := right.
    n := (s) size.
    [
        [(((l >= 0) and: [r]) < n)] whileTrue: [
            [
                (((s at: l + 1) ~= (s at: r + 1))) ifTrue: [
                    BreakSignal signal
                ]
                .
                l := (l - 1).
                r := (r + 1).
            ] on: ContinueSignal do: [:ex | ]
        ]
    ] on: BreakSignal do: [:ex | ]
    .
    ^ ((r - l) - 1)
!

!Main class methodsFor: 'mochi'!
longestPalindrome: s | end i l len1 len2 n start |
    (((s) size <= 1)) ifTrue: [
        ^ s
    ]
    .
    start := 0.
    end := 0.
    n := (s) size.
    0 to: n - 1 do: [:i |
        len1 := (Main expand: (s) left: (i) right: (i)).
        len2 := (Main expand: (s) left: (i) right: ((i + 1))).
        l := len1.
        ((len2 > len1)) ifTrue: [
            l := len2.
        ]
        .
        ((l > ((end - start)))) ifTrue: [
            start := (i - ((((l - 1)) / 2))).
            end := (i + ((l / 2))).
        ]
        .
    ]
    .
    ^ (s copyFrom: (start + 1) to: (end + 1))
!

!Main class methodsFor: 'tests'!
test_example_1
    ans := (Main longestPalindrome: ('babad')).
    ((((ans = 'bab') or: [ans]) = 'aba')) ifFalse: [ self error: 'expect failed' ]
!

!Main class methodsFor: 'tests'!
test_example_2
    (((Main longestPalindrome: ('cbbd')) = 'bb')) ifFalse: [ self error: 'expect failed' ]
!

!Main class methodsFor: 'tests'!
test_single_char
    (((Main longestPalindrome: ('a')) = 'a')) ifFalse: [ self error: 'expect failed' ]
!

!Main class methodsFor: 'tests'!
test_two_chars
    ans := (Main longestPalindrome: ('ac')).
    ((((ans = 'a') or: [ans]) = 'c')) ifFalse: [ self error: 'expect failed' ]
!

Object subclass: #BreakSignal instanceVariableNames: '' classVariableNames: '' poolDictionaries: '' category: nil!

!Main class methodsFor: 'runtime'!
!!
Main test_example_1.
Main test_example_2.
Main test_single_char.
Main test_two_chars.
