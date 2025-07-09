| data groups result tmp total |
data := {Dictionary newFrom: {tag -> 'a'. val -> 1}. Dictionary newFrom: {tag -> 'a'. val -> 2}. Dictionary newFrom: {tag -> 'b'. val -> 3}}.
groups := [ | groups res |
  groups := Dictionary new.
  data do: [:d |
    | g k |
    k := d.tag.
    g := groups at: k ifAbsentPut: [OrderedCollection new].
    g add: d.
  ]
  res := OrderedCollection new.
  groups keysAndValuesDo: [:k :items |
    | g |
    g := Dictionary newFrom: {#key->k. #items->items}.
    res add: g.
  ].
  res
] value.
tmp := {}.
groups do: [:g |
  total := 0.
  g.items do: [:x |
    total := (total + x.val).
  ]
  .
  tmp := tmp copyWith: Dictionary newFrom: {tag -> g.key. total -> total}.
]
.
result := [ | tmp |
  tmp := OrderedCollection new.
  tmp do: [:r |
    tmp add: r.
  ].
  tmp := tmp asSortedCollection: [:a :b | a.tag < b.tag].
  tmp
] value.
Transcript show: (result) printString; cr.
