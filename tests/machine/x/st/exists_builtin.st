| flag data |
data := {1. 2}.
flag := exists value: [ | tmp |
  tmp := OrderedCollection new.
  data do: [:x |
    ((x = 1)) ifTrue: [
      tmp add: x.
    ].
  ].
  tmp
] value.
Transcript show: (flag) printString; cr.
