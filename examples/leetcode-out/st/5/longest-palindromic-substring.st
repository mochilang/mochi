Object subclass: #Main instanceVariableNames: '' classVariableNames: '' poolDictionaries: '' category: nil!

!Main class methodsFor: 'mochi'!
expand: s left: left right: right | l n r |
	l := left.
	r := right.
	n := s size.
	[(((l >= 0) & r) < n)] whileTrue: [
		(((s at: l + 1) ~= (s at: r + 1))) ifTrue: [
		]
		.
		l := (l - 1).
		r := (r + 1).
	]
	.
	^ ((r - l) - 1)
!

!Main class methodsFor: 'mochi'!
longestPalindrome: s | end i k l len1 len2 n res start |
	((s size <= 1)) ifTrue: [
		^ s
	]
	.
	start := 0.
	end := 0.
	n := s size.
	0 to: n - 1 do: [:i |
		len1 := Main expand: (s) left: (i) right: (i).
		len2 := Main expand: (s) left: (i) right: ((i + 1)).
		l := len1.
		((len2 > len1)) ifTrue: [
			l := len2.
		]
		.
		((l > ((end - start)))) ifTrue: [
			start := (i - ((((l - 1)) / 2))).
			end := (i + ((l / 2))).
		]
		.
	]
	.
	res := ''.
	k := start.
	[(k <= end)] whileTrue: [
		res := (res + (s at: k + 1)).
		k := (k + 1).
	]
	.
	^ res
!

!!
