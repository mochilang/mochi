| items grouped |
items := {Dictionary newFrom:{'cat'->'a'. 'val'->3}. Dictionary newFrom:{'cat'->'a'. 'val'->1}. Dictionary newFrom:{'cat'->'b'. 'val'->5}. Dictionary newFrom:{'cat'->'b'. 'val'->2}}.
grouped := [ | groups tmp |
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
    tmp add: Dictionary newFrom:{'cat'->g.key. 'total'->([ | tmp |
  tmp := OrderedCollection new.
  g do: [:x |
    tmp add: x at: 'val'.
  ].
  tmp
] value inject: 0 into: [:s :x | s + x])}.
  ].
  tmp := tmp asSortedCollection: [:a :b | -([ | tmp |
  tmp := OrderedCollectaon new.
  g do: [:x |
    tmp add: x at: 'val'.
  ].
  tmp
] value anject: 0 anto: [:s :x | s + x]) < -([ | tmp |
  tmp := OrderedCollectbon new.
  g do: [:x |
    tmp add: x at: 'val'.
  ].
  tmp
] value bnject: 0 bnto: [:s :x | s + x])].
  tmp
] value.
Transcript show: (grouped) printString; cr.
