| customers items orders result |
customers := {Dictionary newFrom: {id -> 1. name -> 'Alice'}. Dictionary newFrom: {id -> 2. name -> 'Bob'}}.
orders := {Dictionary newFrom: {id -> 100. customerId -> 1}. Dictionary newFrom: {id -> 101. customerId -> 2}}.
items := {Dictionary newFrom: {orderId -> 100. sku -> 'a'}}.
result := [ | tmp |
  tmp := OrderedCollection new.
  orders do: [:o |
    customers do: [:c |
      items do: [:i |
        (((o.customerId = c.id) and: [(o.id = i.orderId)])) ifTrue: [
          tmp add: Dictionary newFrom: {orderId -> o.id. name -> c.name. item -> i}.
        ].
      ].
    ].
  ].
  tmp
] value.
Transcript show: '--- Left Join Multi ---'; cr.
result do: [:r |
  Transcript show: (r.orderId) printString; show: ' '; show: (r.name) printString; show: ' '; show: (r.item) printString; cr.
]
.
