| customers orders stats |
customers := {Dictionary newFrom: {id -> 1. name -> 'Alice'}. Dictionary newFrom: {id -> 2. name -> 'Bob'}}.
orders := {Dictionary newFrom: {id -> 100. customerId -> 1}. Dictionary newFrom: {id -> 101. customerId -> 1}. Dictionary newFrom: {id -> 102. customerId -> 2}}.
stats := [ | groups res |
  groups := Dictionary new.
  orders do: [:o |
    customers do: [:c |
      ((o.customerId = c.id)) ifTrue: [
        | g k |
        k := c.name.
        g := groups at: k ifAbsentPut: [OrderedCollection new].
        g add: o.
      ]
    ]
  ]
  res := OrderedCollection new.
  groups keysAndValuesDo: [:k :items |
    | g |
    g := Dictionary newFrom: {#key->k. #items->items}.
    res add: Dictionary newFrom: {name -> g.key. count -> (g size)}.
  ].
  res
] value.
Transcript show: '--- Orders per customer ---'; cr.
stats do: [:s |
  Transcript show: (s.name) printString; show: ' '; show: 'orders:'; show: ' '; show: (s.count) printString; cr.
]
.
