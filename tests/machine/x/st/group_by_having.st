| people big |
people := {Dictionary newFrom:{'name'->'Alice'. 'city'->'Paris'}. Dictionary newFrom:{'name'->'Bob'. 'city'->'Hanoi'}. Dictionary newFrom:{'name'->'Charlie'. 'city'->'Paris'}. Dictionary newFrom:{'name'->'Diana'. 'city'->'Hanoi'}. Dictionary newFrom:{'name'->'Eve'. 'city'->'Paris'}. Dictionary newFrom:{'name'->'Frank'. 'city'->'Hanoi'}. Dictionary newFrom:{'name'->'George'. 'city'->'Paris'}}.
big := [ | groups tmp |
  groups := Dictionary new.
  tmp := OrderedCollection new.
  people do: [:p |
    | g |
    g := groups at: p at: 'city' ifAbsentPut:[OrderedCollection new].
    g add: Dictionary newFrom:{#p->p}.
  ].
  groups keysAndValuesDo: [:k :grp |
    | g |
    g := Dictionary newFrom:{'key'->k. 'items'->grp}.
    tmp add: Dictionary newFrom:{'city'->g.key. 'num'->(g size)}.
  ].
  tmp
] value.
json value: big.
