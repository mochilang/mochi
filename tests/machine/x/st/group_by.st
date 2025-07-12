| people stats s |
people := {Dictionary newFrom:{'name'->'Alice'. 'age'->30. 'city'->'Paris'}. Dictionary newFrom:{'name'->'Bob'. 'age'->15. 'city'->'Hanoi'}. Dictionary newFrom:{'name'->'Charlie'. 'age'->65. 'city'->'Paris'}. Dictionary newFrom:{'name'->'Diana'. 'age'->45. 'city'->'Hanoi'}. Dictionary newFrom:{'name'->'Eve'. 'age'->70. 'city'->'Paris'}. Dictionary newFrom:{'name'->'Frank'. 'age'->22. 'city'->'Hanoi'}}.
stats := [ | groups tmp |
  groups := Dictionary new.
  tmp := OrderedCollection new.
  people do: [:person |
    | g |
    g := groups at: person at: 'city' ifAbsentPut:[OrderedCollection new].
    g add: Dictionary newFrom:{#person->person}.
  ].
  groups keysAndValuesDo: [:k :grp |
    | g |
    g := Dictionary newFrom:{'key'->k. 'items'->grp}.
    tmp add: Dictionary newFrom:{'city'->g.key. 'count'->(g size). 'avg_age'->avg value: [ | tmp |
  tmp := OrderedCollection new.
  g do: [:p |
    tmp add: p at: 'age'.
  ].
  tmp
] value}.
  ].
  tmp
] value.
Transcript show: '--- People grouped by city ---'; cr.
stats do: [:s |.
Transcript show: (s at: 'city') printString; show: ' '; show: ': count ='; show: ' '; show: (s at: 'count') printString; show: ' '; show: ', avg_age ='; show: ' '; show: (s at: 'avg_age') printString; cr.
].
.
