Object subclass: #Main instanceVariableNames: '' classVariableNames: '' poolDictionaries: '' category: nil!

!Main class methodsFor: 'mochi'!
digit: ch
	((ch = '0')) ifTrue: [
		^ 0
	]
	.
	((ch = '1')) ifTrue: [
		^ 1
	]
	.
	((ch = '2')) ifTrue: [
		^ 2
	]
	.
	((ch = '3')) ifTrue: [
		^ 3
	]
	.
	((ch = '4')) ifTrue: [
		^ 4
	]
	.
	((ch = '5')) ifTrue: [
		^ 5
	]
	.
	((ch = '6')) ifTrue: [
		^ 6
	]
	.
	((ch = '7')) ifTrue: [
		^ 7
	]
	.
	((ch = '8')) ifTrue: [
		^ 8
	]
	.
	((ch = '9')) ifTrue: [
		^ 9
	]
	.
	^ (1 negated)
!

!Main class methodsFor: 'mochi'!
myAtoi: s | ch d i n result sign |
	i := 0.
	n := s size.
	[(((i < n) and: [(s at: i + 1)]) = (' ' at: 0 + 1))] whileTrue: [
		i := (i + 1).
	]
	.
	sign := 1.
	(((i < n) and: [(((((s at: i + 1) = ('+' at: 0 + 1)) or: [(s at: i + 1)]) = ('-' at: 0 + 1)))])) ifTrue: [
		(((s at: i + 1) = ('-' at: 0 + 1))) ifTrue: [
			sign := (1 negated).
		]
		.
		i := (i + 1).
	]
	.
	result := 0.
	[(i < n)] whileTrue: [
		ch := (s copyFrom: (i + 1) to: (i + 1)).
		d := Main digit: (ch).
		((d < 0)) ifTrue: [
		]
		.
		result := ((result * 10) + d).
		i := (i + 1).
	]
	.
	result := (result * sign).
	((result > 2147483647)) ifTrue: [
		^ 2147483647
	]
	.
	((result < ((2147483648 negated)))) ifTrue: [
		^ (2147483648 negated)
	]
	.
	^ result
!

!!
