| items result |
items := {Dictionary newFrom: {cat -> 'a'. val -> 10. flag -> true}. Dictionary newFrom: {cat -> 'a'. val -> 5. flag -> false}. Dictionary newFrom: {cat -> 'b'. val -> 20. flag -> true}}.
result := [ | groups res |
  groups := Dictionary new.
  items do: [:i |
    | g k |
    k := i.cat.
    g := groups at: k ifAbsentPut: [OrderedCollection new].
    g add: i.
  ]
  res := OrderedCollection new.
  groups keysAndValuesDo: [:k :items |
    | g |
    g := Dictionary newFrom: {#key->k. #items->items}.
    res add: Dictionary newFrom: {cat -> g.key. share -> (([ | tmp |
  tmp := OrderedCollection new.
  g do: [:x |
    tmp add: (x.flag) ifTrue: [x.val] ifFalse: [0].
  ].
  tmp
] value inject: 0 into: [:s :x | s + x]) / ([ | tmp |
  tmp := OrderedCollection new.
  g do: [:x |
    tmp add: x.val.
  ].
  tmp
] value inject: 0 into: [:s :x | s + x]))}.
  ].
  res := res asSortedCollection: [:a :b | a.key < b.key].
  res
] value.
Transcript show: (result) printString; cr.
