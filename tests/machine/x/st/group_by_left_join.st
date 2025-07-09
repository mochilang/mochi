| customers orders stats s |
customers := {Dictionary newFrom: {id -> 1. name -> 'Alice'}. Dictionary newFrom: {id -> 2. name -> 'Bob'}. Dictionary newFrom: {id -> 3. name -> 'Charlie'}}.
orders := {Dictionary newFrom: {id -> 100. customerId -> 1}. Dictionary newFrom: {id -> 101. customerId -> 1}. Dictionary newFrom: {id -> 102. customerId -> 2}}.
stats := [ | groups tmp |
  groups := Dictionary new.
  tmp := OrderedCollection new.
  customers do: [:c |
    orders do: [:o |
      ((o.customerId = c.id)) ifTrue: [
        | g |
        g := groups at: c.name ifAbsentPut:[OrderedCollection new].
        g add: Dictionary newFrom: {#c->c. #o->o}.
      ].
    ].
  ].
  groups keysAndValuesDo: [:k :grp |
    | g |
    g := Dictionary newFrom:{#key->k. #items->grp}.
    tmp add: Dictionary newFrom: {name -> g.key. count -> ([ | tmp |
  tmp := OrderedCollection new.
  g do: [:r |
    (r.o) ifTrue: [
      tmp add: r.
    ].
  ].
  tmp
] value size)}.
  ].
  tmp
] value.
Transcript show: ('--- Group Left Join ---') printString; cr.
stats do: [:s |.
Transcript show: (s at: 'name') printString; show: ' '; show: 'orders:'; show: ' '; show: (s at: 'count') printString; cr.
].
.
