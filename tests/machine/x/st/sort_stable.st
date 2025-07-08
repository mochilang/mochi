| items result |
items := {Dictionary newFrom: {n -> 1. v -> 'a'}. Dictionary newFrom: {n -> 1. v -> 'b'}. Dictionary newFrom: {n -> 2. v -> 'c'}}.
result := [ | tmp |
  tmp := OrderedCollection new.
  items do: [:i |
    tmp add: i.v.
  ].
  tmp := tmp asSortedCollection: [:a :b | a.n < b.n].
  tmp
] value.
Transcript show: (result) printString; cr.
