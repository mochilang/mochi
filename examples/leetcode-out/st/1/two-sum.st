Object subclass: #Main instanceVariableNames: '' classVariableNames: '' poolDictionaries: '' category: nil!

!Main class methodsFor: 'mochi'!
twoSum: nums target: target | i j n |
    n := (nums) size.
    0 to: n - 1 do: [:i |
        (i + 1) to: n - 1 do: [:j |
            ((((nums at: i + 1) + (nums at: j + 1)) = target)) ifTrue: [
                ^ Array with: i with: j
            ]
            .
        ]
        .
    ]
    .
    ^ Array with: (1 negated) with: (1 negated)
!

!!
result := (Main twoSum: (Array with: 2 with: 7 with: 11 with: 15) target: (9)).
((result at: 0 + 1)) displayOn: Transcript. Transcript cr.
((result at: 1 + 1)) displayOn: Transcript. Transcript cr.
