| people groups big |
people := {
  Dictionary newFrom: {#name->'Alice'. #city->'Paris'}.
  Dictionary newFrom: {#name->'Bob'. #city->'Hanoi'}.
  Dictionary newFrom: {#name->'Charlie'. #city->'Paris'}.
  Dictionary newFrom: {#name->'Diana'. #city->'Hanoi'}.
  Dictionary newFrom: {#name->'Eve'. #city->'Paris'}.
  Dictionary newFrom: {#name->'Frank'. #city->'Hanoi'}.
  Dictionary newFrom: {#name->'George'. #city->'Paris'}
} asOrderedCollection.

groups := Dictionary new.
people do: [:p |
  | g |
  g := groups at: (p at: #city) ifAbsentPut: [OrderedCollection new].
  g add: p
].

big := OrderedCollection new.
groups keysAndValuesDo: [:city :grp |
  (grp size >= 4) ifTrue:[
    big add: (Dictionary newFrom:{#city->city. #num->grp size})
  ]
].
Transcript show: big printString; cr.
