| customers orders items result r |
customers := {Dictionary newFrom:{#id->1. #name->'Alice'}. Dictionary newFrom:{#id->2. #name->'Bob'}}.
orders := {Dictionary newFrom:{#id->100. #customerId->1}. Dictionary newFrom:{#id->101. #customerId->2}}.
items := {Dictionary newFrom:{#orderId->100. #sku->'a'}. Dictionary newFrom:{#orderId->101. #sku->'b'}}.
result := [ | tmp |
  tmp := OrderedCollection new.
  orders do: [:o |
    customers do: [:c |
      items do: [:i |
        (((o.customerId = c.id) and: [(o.id = i.orderId)])) ifTrue: [
          tmp add: Dictionary newFrom:{#name->c.name. #sku->i.sku}.
        ].
      ].
    ].
  ].
  tmp
] value.
Transcript show: '--- Multi Join ---'; cr.
result do: [:r |.
Transcript show: (r at: 'name') printString; show: ' '; show: 'bought item'; show: ' '; show: (r at: 'sku') printString; cr.
].
.
