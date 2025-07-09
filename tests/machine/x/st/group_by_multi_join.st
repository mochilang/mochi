| partsupp filtered grouped nations suppliers |
nations := {Dictionary newFrom:{#id->1. #name->'A'}. Dictionary newFrom:{#id->2. #name->'B'}}.
suppliers := {Dictionary newFrom:{#id->1. #nation->1}. Dictionary newFrom:{#id->2. #nation->2}}.
partsupp := {Dictionary newFrom:{#part->100. #supplier->1. #cost->10. #qty->2}. Dictionary newFrom:{#part->100. #supplier->2. #cost->20. #qty->1}. Dictionary newFrom:{#part->200. #supplier->1. #cost->5. #qty->3}}.
filtered := [ | tmp |
  tmp := OrderedCollection new.
  partsupp do: [:ps |
    suppliers do: [:s |
      nations do: [:n |
        ((((n.name = 'A') and: [(s.id = ps.supplier)]) and: [(n.id = s.nation)])) ifTrue: [
          tmp add: Dictionary newFrom:{#part->ps.part. #value->(ps.cost * ps.qty)}.
        ].
      ].
    ].
  ].
  tmp
] value.
grouped := [ | groups tmp |
  groups := Dictionary new.
  tmp := OrderedCollection new.
  filtered do: [:x |
    | g |
    g := groups at: x.part ifAbsentPut:[OrderedCollection new].
    g add: Dictionary newFrom: {#x->x}.
  ].
  groups keysAndValuesDo: [:k :grp |
    | g |
    g := Dictionary newFrom:{#key->k. #items->grp}.
    tmp add: Dictionary newFrom:{#part->g.key. #total->([ | tmp |
  tmp := OrderedCollection new.
  g do: [:r |
    tmp add: r.value.
  ].
  tmp
] value inject: 0 into: [:s :x | s + x])}.
  ].
  tmp
] value.
Transcript show: (grouped) printString; cr.
