Object subclass: #Main instanceVariableNames: '' classVariableNames: '' poolDictionaries: '' category: nil!

!Main class methodsFor: 'mochi'!
findMedianSortedArrays: nums1 nums2: nums2 | i j merged mid1 mid2 total |
    merged := Array new.
    i := 0.
    j := 0.
    [(((i < (nums1) size) or: [j]) < (nums2) size)] whileTrue: [
        ((j >= (nums2) size)) ifTrue: [
            merged := ((merged) , (Array with: (nums1 at: i + 1))).
            i := (i + 1).
        ]
        .
    ]
    .
    total := (merged) size.
    (((total \\ 2) = 1)) ifTrue: [
        ^ (Main _cast: 'float' value: (merged at: (total / 2) + 1))
    ]
    .
    mid1 := (merged at: ((total / 2) - 1) + 1).
    mid2 := (merged at: (total / 2) + 1).
    ^ ((Main _cast: 'float' value: ((mid1 + mid2))) / 2.000000)
!

!Main class methodsFor: 'tests'!
test_example_1
    (((Main findMedianSortedArrays: (Array with: 1 with: 3) nums2: (Array with: 2)) = 2.000000)) ifFalse: [ self error: 'expect failed' ]
!

!Main class methodsFor: 'tests'!
test_example_2
    (((Main findMedianSortedArrays: (Array with: 1 with: 2) nums2: (Array with: 3 with: 4)) = 2.500000)) ifFalse: [ self error: 'expect failed' ]
!

!Main class methodsFor: 'tests'!
test_empty_first
    (((Main findMedianSortedArrays: ((Main _cast: '' value: Array new)) nums2: (Array with: 1)) = 1.000000)) ifFalse: [ self error: 'expect failed' ]
!

!Main class methodsFor: 'tests'!
test_empty_second
    (((Main findMedianSortedArrays: (Array with: 2) nums2: ((Main _cast: '' value: Array new))) = 2.000000)) ifFalse: [ self error: 'expect failed' ]
!

!!
Main test_example_1.
Main test_example_2.
Main test_empty_first.
Main test_empty_second.
