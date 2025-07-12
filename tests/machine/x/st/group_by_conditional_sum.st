| items result |
items := {Dictionary newFrom:{'cat'->'a'. 'val'->10. 'flag'->true}. Dictionary newFrom:{'cat'->'a'. 'val'->5. 'flag'->false}. Dictionary newFrom:{'cat'->'b'. 'val'->20. 'flag'->true}}.
result := [ | groups tmp |
  groups := Dictionary new.
  tmp := OrderedCollection new.
  items do: [:i |
    | g |
    g := groups at: i at: 'cat' ifAbsentPut:[OrderedCollection new].
    g add: Dictionary newFrom:{#i->i}.
  ].
  groups keysAndValuesDo: [:k :grp |
    | g |
    g := Dictionary newFrom:{'key'->k. 'items'->grp}.
    tmp add: Dictionary newFrom:{'cat'->g.key. 'share'->(([ | tmp |
  tmp := OrderedCollection new.
  g do: [:x |
    tmp add: (x at: 'flag') ifTrue: [x at: 'val'] ifFalse: [0].
  ].
  tmp
] value inject: 0 into: [:s :x | s + x]) / ([ | tmp |
  tmp := OrderedCollection new.
  g do: [:x |
    tmp add: x at: 'val'.
  ].
  tmp
] value inject: 0 into: [:s :x | s + x]))}.
  ].
  tmp := tmp asSortedCollection: [:a :b | g.key < g.key].
  tmp
] value.
Transcript show: (result) printString; cr.
