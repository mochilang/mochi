Object subclass: #Main instanceVariableNames: '' classVariableNames: '' poolDictionaries: '' category: nil!

!Main class methodsFor: 'mochi'!
lengthOfLongestSubstring: s | best i j length n start |
	n := s size.
	start := 0.
	best := 0.
	i := 0.
	[(i < n)] whileTrue: [
		j := start.
		[(j < i)] whileTrue: [
			(((s at: j + 1) = (s at: i + 1))) ifTrue: [
				start := (j + 1).
			]
			.
			j := (j + 1).
		]
		.
		length := ((i - start) + 1).
		((length > best)) ifTrue: [
			best := length.
		]
		.
		i := (i + 1).
	]
	.
	^ best
!

!!
