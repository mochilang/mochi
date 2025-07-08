| customers orders result |
customers := {Dictionary newFrom: {id -> 1. name -> 'Alice'}. Dictionary newFrom: {id -> 2. name -> 'Bob'}. Dictionary newFrom: {id -> 3. name -> 'Charlie'}}.
orders := {Dictionary newFrom: {id -> 100. customerId -> 1. total -> 250}. Dictionary newFrom: {id -> 101. customerId -> 2. total -> 125}. Dictionary newFrom: {id -> 102. customerId -> 1. total -> 300}}.
result := [ | tmp |
  tmp := OrderedCollection new.
  orders do: [:o |
    customers do: [:c |
      tmp add: Dictionary newFrom: {orderId -> o.id. orderCustomerId -> o.customerId. pairedCustomerName -> c.name. orderTotal -> o.total}.
    ].
  ].
  tmp
] value.
Transcript show: '--- Cross Join: All order-customer pairs ---'; cr.
result do: [:entry |
  Transcript show: 'Order'; show: ' '; show: (entry.orderId) printString; show: ' '; show: '(customerId:'; show: ' '; show: (entry.orderCustomerId) printString; show: ' '; show: ', total: $'; show: ' '; show: (entry.orderTotal) printString; show: ' '; show: ') paired with'; show: ' '; show: (entry.pairedCustomerName) printString; cr.
].
