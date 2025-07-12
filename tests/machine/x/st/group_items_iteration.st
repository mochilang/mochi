| data groups tmp g total x result |
data := {Dictionary newFrom:{'tag'->'a'. 'val'->1}. Dictionary newFrom:{'tag'->'a'. 'val'->2}. Dictionary newFrom:{'tag'->'b'. 'val'->3}}.
groups := [ | groups tmp |
  groups := Dictionary new.
  tmp := OrderedCollection new.
  data do: [:d |
    | g |
    g := groups at: d at: 'tag' ifAbsentPut:[OrderedCollection new].
    g add: Dictionary newFrom:{#d->d}.
  ].
  groups keysAndValuesDo: [:k :grp |
    | g |
    g := Dictionary newFrom:{'key'->k. 'items'->grp}.
    tmp add: g.
  ].
  tmp
] value.
tmp := {}.
groups do: [:g |.
total := 0.
g at: 'items' do: [:x |.
total := (total + x at: 'val').
].
.
tmp := tmp copyWith: Dictionary newFrom:{'tag'->g at: 'key'. total->total}.
].
.
result := [ | tmp |
  tmp := OrderedCollection new.
  tmp do: [:r |
    tmp add: r.
  ].
  tmp := tmp asSortedCollection: [:a :b | a at: 'tag' < b at: 'tag'].
  tmp
] value.
Transcript show: (result) printString; cr.
