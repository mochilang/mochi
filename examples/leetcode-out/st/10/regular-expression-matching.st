Object subclass: #Main instanceVariableNames: '' classVariableNames: '' poolDictionaries: '' category: nil!

!Main class methodsFor: 'mochi'!
isMatch: s p: p | dp first i i2 j j2 m n row |
	m := s size.
	n := p size.
	dp := Array new.
	i := 0.
	[(i <= m)] whileTrue: [
		row := Array new.
		j := 0.
		[(j <= n)] whileTrue: [
			row := (row , Array with: false).
			j := (j + 1).
		]
		.
		dp := (dp , Array with: row).
		i := (i + 1).
	]
	.
	dp at: m + 1 put: true.
	i2 := m.
	[(i2 >= 0)] whileTrue: [
		j2 := (n - 1).
		[(j2 >= 0)] whileTrue: [
			first := false.
			((i2 < m)) ifTrue: [
				(((((p at: j2 + 1) = (s at: i2 + 1))) | (((p at: j2 + 1) = '.')))) ifTrue: [
					first := true.
				]
				.
			]
			.
			(((((j2 + 1) < n) & (p at: (j2 + 1) + 1)) = '*')) ifTrue: [
				((((dp at: i2 + 1) at: (j2 + 2) + 1) | ((first & ((dp at: (i2 + 1) + 1) at: j2 + 1))))) ifTrue: [
					dp at: i2 + 1 put: true.
				] ifFalse: [
					dp at: i2 + 1 put: false.
				]
				.
			] ifFalse: [
				((first & ((dp at: (i2 + 1) + 1) at: (j2 + 1) + 1))) ifTrue: [
					dp at: i2 + 1 put: true.
				] ifFalse: [
					dp at: i2 + 1 put: false.
				]
				.
			]
			.
			j2 := (j2 - 1).
		]
		.
		i2 := (i2 - 1).
	]
	.
	^ ((dp at: 0 + 1) at: 0 + 1)
!

!!
