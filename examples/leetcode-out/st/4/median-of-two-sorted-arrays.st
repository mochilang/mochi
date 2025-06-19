Object subclass: #Main instanceVariableNames: '' classVariableNames: '' poolDictionaries: '' category: nil!

!Main class methodsFor: 'mochi'!
findMedianSortedArrays: nums1 nums2: nums2 | i j merged mid1 mid2 total |
	merged := Array new.
	i := 0.
	j := 0.
	[(((i < nums1 size) or: [j]) < nums2 size)] whileTrue: [
		((j >= nums2 size)) ifTrue: [
			merged := ((merged) , (Array with: (nums1 at: i + 1))).
			i := (i + 1).
		]
		.
	]
	.
	total := merged size.
	(((total \\ 2) = 1)) ifTrue: [
		^ (merged at: (total / 2) + 1)
	]
	.
	mid1 := (merged at: ((total / 2) - 1) + 1).
	mid2 := (merged at: (total / 2) + 1).
	^ (((mid1 + mid2)) / 2.000000)
!

!!
