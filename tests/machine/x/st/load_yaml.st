| adults people |
people := ((Main _load: '../interpreter/valid/people.yaml' opts: Dictionary newFrom: {format -> 'yaml'}) collect: [:it | Person newFrom: it]).
adults := [ | tmp |
  tmp := OrderedCollection new.
  people do: [:p |
    ((p.age >= 18)) ifTrue: [
      tmp add: Dictionary newFrom: {name -> p.name. email -> p.email}.
    ].
  ].
  tmp
] value.
adults do: [:a |
  Transcript show: (a.name) printString; show: ' '; show: (a.email) printString; cr.
]
.
