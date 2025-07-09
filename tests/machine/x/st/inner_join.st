| result entry customers orders |
customers := {Dictionary newFrom:{#id->1. #name->'Alice'}. Dictionary newFrom:{#id->2. #name->'Bob'}. Dictionary newFrom:{#id->3. #name->'Charlie'}}.
orders := {Dictionary newFrom:{#id->100. #customerId->1. #total->250}. Dictionary newFrom:{#id->101. #customerId->2. #total->125}. Dictionary newFrom:{#id->102. #customerId->1. #total->300}. Dictionary newFrom:{#id->103. #customerId->4. #total->80}}.
result := [ | tmp |
  tmp := OrderedCollection new.
  orders do: [:o |
    customers do: [:c |
      ((o.customerId = c.id)) ifTrue: [
        tmp add: Dictionary newFrom:{#orderId->o.id. #customerName->c.name. #total->o.total}.
      ].
    ].
  ].
  tmp
] value.
Transcript show: '--- Orders with customer info ---'; cr.
result do: [:entry |.
Transcript show: 'Order'; show: ' '; show: (entry at: 'orderId') printString; show: ' '; show: 'by'; show: ' '; show: (entry at: 'customerName') printString; show: ' '; show: '- $'; show: ' '; show: (entry at: 'total') printString; cr.
].
.
