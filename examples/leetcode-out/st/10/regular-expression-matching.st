Object subclass: #Main instanceVariableNames: '' classVariableNames: '' poolDictionaries: '' category: nil!

!Main class methodsFor: 'mochi'!
isMatch: s p: p | dp first i i2 j j2 m n ok row star |
    m := (s) size.
    n := (p) size.
    dp := Array new.
    i := 0.
    [(i <= m)] whileTrue: [
        row := Array new.
        j := 0.
        [(j <= n)] whileTrue: [
            row := ((row) , (Array with: false)).
            j := (j + 1).
        ]
        .
        dp := ((dp) , (Array with: row)).
        i := (i + 1).
    ]
    .
    dp at: m + 1 put: true.
    i2 := m.
    [(i2 >= 0)] whileTrue: [
        j2 := (n - 1).
        [(j2 >= 0)] whileTrue: [
            first := false.
            ((i2 < m)) ifTrue: [
                (((((p at: j2 + 1) = (s at: i2 + 1))) or: [(((p at: j2 + 1) = '.'))])) ifTrue: [
                    first := true.
                ]
                .
            ]
            .
            star := false.
            (((j2 + 1) < n)) ifTrue: [
                (((p at: (j2 + 1) + 1) = '*')) ifTrue: [
                    star := true.
                ]
                .
            ]
            .
            (star) ifTrue: [
                ok := false.
                (((dp at: i2 + 1) at: (j2 + 2) + 1)) ifTrue: [
                    ok := true.
                ] ifFalse: [
                    (first) ifTrue: [
                        (((dp at: (i2 + 1) + 1) at: j2 + 1)) ifTrue: [
                            ok := true.
                        ]
                        .
                    ]
                    .
                ]
                .
                dp at: i2 + 1 put: ok.
            ] ifFalse: [
                ok := false.
                (first) ifTrue: [
                    (((dp at: (i2 + 1) + 1) at: (j2 + 1) + 1)) ifTrue: [
                        ok := true.
                    ]
                    .
                ]
                .
                dp at: i2 + 1 put: ok.
            ]
            .
            j2 := (j2 - 1).
        ]
        .
        i2 := (i2 - 1).
    ]
    .
    ^ ((dp at: 0 + 1) at: 0 + 1)
!

!Main class methodsFor: 'tests'!
test_example_1
    (((Main isMatch: ('aa') p: ('a')) = false)) ifFalse: [ self error: 'expect failed' ]
!

!Main class methodsFor: 'tests'!
test_example_2
    (((Main isMatch: ('aa') p: ('a*')) = true)) ifFalse: [ self error: 'expect failed' ]
!

!Main class methodsFor: 'tests'!
test_example_3
    (((Main isMatch: ('ab') p: ('.*')) = true)) ifFalse: [ self error: 'expect failed' ]
!

!Main class methodsFor: 'tests'!
test_example_4
    (((Main isMatch: ('aab') p: ('c*a*b')) = true)) ifFalse: [ self error: 'expect failed' ]
!

!Main class methodsFor: 'tests'!
test_example_5
    (((Main isMatch: ('mississippi') p: ('mis*is*p*.')) = false)) ifFalse: [ self error: 'expect failed' ]
!

!!
Main test_example_1.
Main test_example_2.
Main test_example_3.
Main test_example_4.
Main test_example_5.
