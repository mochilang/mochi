| big people |
people := {Dictionary newFrom: {name -> 'Alice'. city -> 'Paris'}. Dictionary newFrom: {name -> 'Bob'. city -> 'Hanoi'}. Dictionary newFrom: {name -> 'Charlie'. city -> 'Paris'}. Dictionary newFrom: {name -> 'Diana'. city -> 'Hanoi'}. Dictionary newFrom: {name -> 'Eve'. city -> 'Paris'}. Dictionary newFrom: {name -> 'Frank'. city -> 'Hanoi'}. Dictionary newFrom: {name -> 'George'. city -> 'Paris'}}.
big := [ | groups res |
  groups := Dictionary new.
  people do: [:p |
    | g k |
    k := p.city.
    g := groups at: k ifAbsentPut: [OrderedCollection new].
    g add: p.
  ]
  res := OrderedCollection new.
  groups keysAndValuesDo: [:k :items |
    | g |
    g := Dictionary newFrom: {#key->k. #items->items}.
    (((g size) >= 4)) ifTrue: [
      res add: Dictionary newFrom: {city -> g.key. num -> (g size)}.
    ].
  ].
  res
] value.
json value: big.
