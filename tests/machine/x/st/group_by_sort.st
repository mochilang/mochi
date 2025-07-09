| grouped items |
items := {Dictionary newFrom: {cat -> 'a'. val -> 3}. Dictionary newFrom: {cat -> 'a'. val -> 1}. Dictionary newFrom: {cat -> 'b'. val -> 5}. Dictionary newFrom: {cat -> 'b'. val -> 2}}.
grouped := [ | groups res |
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
    res add: Dictionary newFrom: {cat -> g.key. total -> ([ | tmp |
  tmp := OrderedCollection new.
  g do: [:x |
    tmp add: x.val.
  ].
  tmp
] value inject: 0 into: [:s :x | s + x])}.
  ].
  res := res asSortedCollection: [:a :b | -([ | tmp |
  tmp := OrderedCollection new.
  a do: [:x |
    tmp add: x.val.
  ].
  tmp
] value inject: 0 into: [:s :x | s + x]) < -([ | tmp |
  tmp := OrderedCollection new.
  b do: [:x |
    tmp add: x.val.
  ].
  tmp
] value inject: 0 into: [:s :x | s + x])].
  res
] value.
Transcript show: (grouped) printString; cr.
