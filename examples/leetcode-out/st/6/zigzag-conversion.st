Object subclass: #Main instanceVariableNames: '' classVariableNames: '' poolDictionaries: '' category: nil!

!Main class methodsFor: 'mochi'!
convert: s numRows: numRows | ch curr i result row rows step |
	((((numRows <= 1) or: [numRows]) >= s size)) ifTrue: [
		^ s
	]
	.
	rows := Array new.
	i := 0.
	[(i < numRows)] whileTrue: [
		rows := (rows , Array with: '').
		i := (i + 1).
	]
	.
	curr := 0.
	step := 1.
	s do: [:ch |
		rows at: curr + 1 put: ((rows at: curr + 1) + ch).
		((curr = 0)) ifTrue: [
			step := 1.
		]
		.
		curr := (curr + step).
	]
	.
	result := ''.
	rows do: [:row |
		result := (result + row).
	]
	.
	^ result
!

!!
