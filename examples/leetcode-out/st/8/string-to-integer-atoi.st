Object subclass: #Main instanceVariableNames: '' classVariableNames: '' poolDictionaries: '' category: nil!

!Main class methodsFor: 'mochi'!
myAtoi: s | ch d digits i n result sign |
	i := 0.
	n := s size.
	[((i < n) & ((s at: i + 1) = ' '))] whileTrue: [
		i := (i + 1).
	]
	.
	sign := 1.
	(((i < n) & ((((s at: i + 1) = '+') | ((s at: i + 1) = '-'))))) ifTrue: [
		(((s at: i + 1) = '-')) ifTrue: [
			sign := (0 - 1).
		]
		.
		i := (i + 1).
	]
	.
	digits := Dictionary newFrom: { '0' -> 0 . '1' -> 1 . '2' -> 2 . '3' -> 3 . '4' -> 4 . '5' -> 5 . '6' -> 6 . '7' -> 7 . '8' -> 8 . '9' -> 9 }.
	result := 0.
	[(i < n)] whileTrue: [
		ch := (s at: i + 1).
		((((digits includes: ch))) not) ifTrue: [
		]
		.
		d := (digits at: ch + 1).
		result := ((result * 10) + d).
		i := (i + 1).
	]
	.
	result := (result * sign).
	((result > 2147483647)) ifTrue: [
		^ 2147483647
	]
	.
	((result < ((0 - 2147483648)))) ifTrue: [
		^ (0 - 2147483648)
	]
	.
	^ result
!

!!
