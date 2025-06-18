Object subclass: #Main instanceVariableNames: '' classVariableNames: '' poolDictionaries: '' category: nil!

!Main class methodsFor: 'mochi'!
isMatch: s p: p | ans dfs first key m memo n |
	m := s size.
	n := p size.
	memo := Dictionary new.
	dfs := [:i :j | | ans first key dfs |
		key := ((i * ((n + 1))) + j).
		((memo includes: key)) ifTrue: [
			^ (memo at: key + 1)
		]
		.
		((j = n)) ifTrue: [
			^ (i = m)
		]
		.
		first := false.
		((i < m)) ifTrue: [
			(((((p at: j + 1) = (s at: i + 1))) | (((p at: j + 1) = '.')))) ifTrue: [
				first := true.
			]
			.
		]
		.
		ans := false.
		(((j + 1) < n)) ifTrue: [
			(((p at: (j + 1) + 1) = '*')) ifTrue: [
				(Main dfs: (i) j: ((j + 2))) ifTrue: [
					ans := true.
				]
				.
			] ifFalse: [
				((first & Main dfs: ((i + 1)) j: ((j + 1)))) ifTrue: [
					ans := true.
				]
				.
			]
			.
		] ifFalse: [
			((first & Main dfs: ((i + 1)) j: ((j + 1)))) ifTrue: [
				ans := true.
			]
			.
		]
		.
		memo at: key + 1 put: ans.
		^ ans
	].
	^ Main dfs: (0) j: (0)
!

!!
