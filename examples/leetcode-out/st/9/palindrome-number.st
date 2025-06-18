Object subclass: #Main instanceVariableNames: '' classVariableNames: '' poolDictionaries: '' category: nil!

!Main class methodsFor: 'mochi'!
isPalindrome: x | i n s |
	((x < 0)) ifTrue: [
		^ false
	]
	.
	s := (x printString).
	n := s size.
	0 to: (n / 2) - 1 do: [:i |
		(((s at: i + 1) ~= (s at: ((n - 1) - i) + 1))) ifTrue: [
			^ false
		]
		.
	]
	.
	^ true
!

!!
