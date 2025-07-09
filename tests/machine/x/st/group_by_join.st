| customers orders stats s |
customers := {Dictionary newFrom:{#id->1. #name->'Alice'}. Dictionary newFrom:{#id->2. #name->'Bob'}}.
orders := {Dictionary newFrom:{#id->100. #customerId->1}. Dictionary newFrom:{#id->101. #customerId->1}. Dictionary newFrom:{#id->102. #customerId->2}}.
stats := [ | groups tmp |
  groups := Dictionary new.
  tmp := OrderedCollection new.
  orders do: [:o |
    customers do: [:c |
      ((o.customerId = c.id)) ifTrue: [
        | g |
        g := groups at: c.name ifAbsentPut:[OrderedCollection new].
        g add: Dictionary newFrom: {#o->o. #c->c}.
      ].
    ].
  ].
  groups keysAndValuesDo: [:k :grp |
    | g |
    g := Dictionary newFrom:{#key->k. #items->grp}.
    tmp add: Dictionary newFrom:{#name->g.key. #count->(g size)}.
  ].
  tmp
] value.
Transcript show: '--- Orders per customer ---'; cr.
stats do: [:s |.
Transcript show: (s at: 'name') printString; show: ' '; show: 'orders:'; show: ' '; show: (s at: 'count') printString; cr.
].
.
