| people adults a |
people := load value: "../interpreter/valid/people.yaml" value: Dictionary newFrom:{'format'->'yaml'}.
adults := [ | tmp |
  tmp := OrderedCollection new.
  people do: [:p |
    ((p at: 'age' >= 18)) ifTrue: [
      tmp add: Dictionary newFrom:{'name'->p at: 'name'. 'email'->p at: 'email'}.
    ].
  ].
  tmp
] value.
adults do: [:a |.
Transcript show: (a at: 'name') printString; show: ' '; show: (a at: 'email') printString; cr.
].
.
