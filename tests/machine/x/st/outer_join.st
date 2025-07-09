| customers orders result |
customers := {Dictionary newFrom: {id -> 1. name -> 'Alice'}. Dictionary newFrom: {id -> 2. name -> 'Bob'}. Dictionary newFrom: {id -> 3. name -> 'Charlie'}. Dictionary newFrom: {id -> 4. name -> 'Diana'}}.
orders := {Dictionary newFrom: {id -> 100. customerId -> 1. total -> 250}. Dictionary newFrom: {id -> 101. customerId -> 2. total -> 125}. Dictionary newFrom: {id -> 102. customerId -> 1. total -> 300}. Dictionary newFrom: {id -> 103. customerId -> 5. total -> 80}}.
result := [ | tmp |
  tmp := OrderedCollection new.
  orders do: [:o |
    customers do: [:c |
      ((o.customerId = c.id)) ifTrue: [
        tmp add: Dictionary newFrom: {order -> o. customer -> c}.
      ].
    ].
  ].
  tmp
] value.
Transcript show: '--- Outer Join using syntax ---'; cr.
result do: [:row |
  (row.order) ifTrue: [
    (row.customer) ifTrue: [
      Transcript show: 'Order'; show: ' '; show: (row.order.id) printString; show: ' '; show: 'by'; show: ' '; show: (row.customer.name) printString; show: ' '; show: '- $'; show: ' '; show: (row.order.total) printString; cr.
    ] ifFalse: [
      Transcript show: 'Order'; show: ' '; show: (row.order.id) printString; show: ' '; show: 'by'; show: ' '; show: 'Unknown'; show: ' '; show: '- $'; show: ' '; show: (row.order.total) printString; cr.
    ].
  ] ifFalse: [
    Transcript show: 'Customer'; show: ' '; show: (row.customer.name) printString; show: ' '; show: 'has no orders'; cr.
  ].
]
.
