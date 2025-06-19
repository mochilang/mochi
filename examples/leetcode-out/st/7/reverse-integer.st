Object subclass: #Main instanceVariableNames: '' classVariableNames: '' poolDictionaries: '' category: nil!

!Main class methodsFor: 'mochi'!
reverse: x | digit n rev sign |
	sign := 1.
	n := x.
	((n < 0)) ifTrue: [
		sign := (1 negated).
		n := (n negated).
	]
	.
	rev := 0.
	[(n ~= 0)] whileTrue: [
		digit := (n \ 10).
		rev := ((rev * 10) + digit).
		n := (n / 10).
	]
	.
	rev := (rev * sign).
	((((rev < (((2147483647 negated) - 1))) or: [rev]) > 2147483647)) ifTrue: [
		^ 0
	]
	.
	^ rev
!

!!
