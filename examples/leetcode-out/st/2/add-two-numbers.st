Object subclass: #Main instanceVariableNames: '' classVariableNames: '' poolDictionaries: '' category: nil!

!Main class methodsFor: 'mochi'!
addTwoNumbers: l1 l2: l2 | carry digit i j result sum x y |
	i := 0.
	j := 0.
	carry := 0.
	result := Array new.
	[(((i < l1 size) | (j < l2 size)) | (carry > 0))] whileTrue: [
		x := 0.
		((i < l1 size)) ifTrue: [
			x := (l1 at: i + 1).
			i := (i + 1).
		]
		.
		y := 0.
		((j < l2 size)) ifTrue: [
			y := (l2 at: j + 1).
			j := (j + 1).
		]
		.
		sum := ((x + y) + carry).
		digit := (sum \ 10).
		carry := (sum / 10).
		result := (result , Array with: digit).
	]
	.
	^ result
!

!!
