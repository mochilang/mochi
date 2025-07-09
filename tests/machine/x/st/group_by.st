| people stats |
people := {Dictionary newFrom: {name -> 'Alice'. age -> 30. city -> 'Paris'}. Dictionary newFrom: {name -> 'Bob'. age -> 15. city -> 'Hanoi'}. Dictionary newFrom: {name -> 'Charlie'. age -> 65. city -> 'Paris'}. Dictionary newFrom: {name -> 'Diana'. age -> 45. city -> 'Hanoi'}. Dictionary newFrom: {name -> 'Eve'. age -> 70. city -> 'Paris'}. Dictionary newFrom: {name -> 'Frank'. age -> 22. city -> 'Hanoi'}}.
stats := [ | groups res |
  groups := Dictionary new.
  people do: [:person |
    | g k |
    k := person.city.
    g := groups at: k ifAbsentPut: [OrderedCollection new].
    g add: person.
  ]
  res := OrderedCollection new.
  groups keysAndValuesDo: [:k :items |
    | g |
    g := Dictionary newFrom: {#key->k. #items->items}.
    res add: Dictionary newFrom: {city -> g.key. count -> (g size). avg_age -> avg value: [ | tmp |
  tmp := OrderedCollection new.
  g do: [:p |
    tmp add: p.age.
  ].
  tmp
] value}.
  ].
  res
] value.
Transcript show: '--- People grouped by city ---'; cr.
stats do: [:s |
  Transcript show: (s.city) printString; show: ' '; show: ': count ='; show: ' '; show: (s.count) printString; show: ' '; show: ', avg_age ='; show: ' '; show: (s.avg_age) printString; cr.
]
.
