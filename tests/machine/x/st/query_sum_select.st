| nums result |
nums := {1. 2. 3}.
result := [ | tmp |
  tmp := OrderedCollection new.
  nums do: [:n |
    ((n > 1)) ifTrue: [
      tmp add: (n inject: 0 into: [:s :x | s + x]).
    ].
  ].
  tmp
] value.
Transcript show: (result) printString; cr.
