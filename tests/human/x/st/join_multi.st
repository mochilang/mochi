| customers orders items result |
customers := {
  Dictionary newFrom:{#id->1. #name->'Alice'}.
  Dictionary newFrom:{#id->2. #name->'Bob'}
} asOrderedCollection.
orders := {
  Dictionary newFrom:{#id->100. #customerId->1}.
  Dictionary newFrom:{#id->101. #customerId->2}
} asOrderedCollection.
items := {
  Dictionary newFrom:{#orderId->100. #sku->'a'}.
  Dictionary newFrom:{#orderId->101. #sku->'b'}
} asOrderedCollection.

result := OrderedCollection new.
orders do: [:o |
  c := customers detect: [:cust | (cust at:#id) = (o at:#customerId)].
  items select: [:it | (it at:#orderId) = (o at:#id)] do: [:it |
    result add:(Dictionary newFrom:{#name->(c at:#name). #sku->(it at:#sku)})
  ]
].
Transcript show:'--- Multi Join ---'; cr.
result do: [:r | Transcript show:(r at:#name); show:' bought item '; show:(r at:#sku); cr].
