| result entry customers orders |
customers := {Dictionary newFrom: {id -> 1. name -> 'Alice'}. Dictionary newFrom: {id -> 2. name -> 'Bob'}}.
orders := {Dictionary newFrom: {id -> 100. customerId -> 1. total -> 250}. Dictionary newFrom: {id -> 101. customerId -> 3. total -> 80}}.
result := [ | tmp |
  tmp := OrderedCollection new.
  orders do: [:o |
    customers do: [:c |
      ((o.customerId = c.id)) ifTrue: [
        tmp add: Dictionary newFrom: {orderId -> o.id. customer -> c. total -> o.total}.
      ].
    ].
  ].
  tmp
] value.
Transcript show: '--- Left Join ---'; cr.
result do: [:entry |.
Transcript show: 'Order'; show: ' '; show: (entry at: 'orderId') printString; show: ' '; show: 'customer'; show: ' '; show: (entry at: 'customer') printString; show: ' '; show: 'total'; show: ' '; show: (entry at: 'total') printString; cr.
].
.
