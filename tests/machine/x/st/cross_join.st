" Generated by Mochi compiler v0.10.26 on 2025-07-16T17:40:19Z
"
| customers orders result |
customers := {Dictionary from: {'id' -> 1. 'name' -> 'Alice'}. Dictionary from: {'id' -> 2. 'name' -> 'Bob'}. Dictionary from: {'id' -> 3. 'name' -> 'Charlie'}}.
orders := {Dictionary from: {'id' -> 100. 'customerId' -> 1. 'total' -> 250}. Dictionary from: {'id' -> 101. 'customerId' -> 2. 'total' -> 125}. Dictionary from: {'id' -> 102. 'customerId' -> 1. 'total' -> 300}}.
result := [ | tmp |
  tmp := OrderedCollection new.
  orders do: [:o |
    customers do: [:c |
      tmp add: Dictionary from: {'orderId' -> o.id. 'orderCustomerId' -> o.customerId. 'pairedCustomerName' -> c.name. 'orderTotal' -> o.total}.
    ].
  ].
  tmp
] value.
Transcript show: '--- Cross Join: All order-customer pairs ---'; cr.
result do: [:entry |
  Transcript show: 'Order'; show: ' '; show: (entry.orderId) printString; show: ' '; show: '(customerId:'; show: ' '; show: (entry.orderCustomerId) printString; show: ' '; show: ', total: $'; show: ' '; show: (entry.orderTotal) printString; show: ' '; show: ') paired with'; show: ' '; show: (entry.pairedCustomerName) printString; cr.
]
.
